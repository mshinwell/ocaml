(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2015, Jane Street Group, LLC                             *)
(*                                                                     *)
(*  Licensed under the Apache License, Version 2.0 (the "License");    *)
(*  you may not use this file except in compliance with the License.   *)
(*  You may obtain a copy of the License at                            *)
(*                                                                     *)
(*      http://www.apache.org/licenses/LICENSE-2.0                     *)
(*                                                                     *)
(*  Unless required by applicable law or agreed to in writing,         *)
(*  software distributed under the License is distributed on an        *)
(*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,       *)
(*  either express or implied.  See the License for the specific       *)
(*  language governing permissions and limitations under the License.  *)
(*                                                                     *)
(***********************************************************************)

(* Instrumentation for allocation profiling falls into two halves.
   Code for instrumenting allocation points and adding prologue code
   is expressed in Cmm, to decrease complexity.
   The remainder is expressed in Mach, since we need information
   about any tail call optimizations that have been performed, in order
   to keep the backtrace stack in sync with the system stack.  (This also
   enables us to prevent unnecessary recomputation of the hash bucket
   upon a tail call to the current function.) *)

open Cmm

let use_override_profinfo =
  Cconst_symbol "caml_allocation_profiling_use_override_profinfo"

let override_profinfo =
  Cconst_symbol "caml_allocation_profiling_override_profinfo"

let profinfo_counter =
  Cconst_symbol "caml_allocation_profiling_profinfo"

let add_prologue ~body ~num_allocation_points ~backtrace_bucket =
  (* Upon entry to an OCaml function, the backtrace top of stack pointer
     points at the word holding the hash of all previous frames.  There is a
     fixed register assignment for this.  It is the responsibility of the
     callee to push their return address and to update the hash.  We make
     callees do this rather than callers to keep code size down; there should
     be fewer callees than call points.  The callee must then determine which
     backtrace hash bucket is to be used for the current function; this
     pointer is held in the [backtrace_bucket] variable.  There is no fixed
     register assignment for this.

     To reduce complexity, part of this logic is implemented in C as a
     "noalloc" function; the overhead should be small since it's reached via
     a static branch and the C compiler should be able to apply high
     optimization to the code.
  *)
  Clet (backtrace_bucket,
    Cop (Cextcall ("caml_allocation_profiling_prologue",
        [| Int |],   (* Actually a naked pointer, just don't tell anyone. *)
        false, (* = noalloc *)
        Debuginfo.none),
      [Cconst_int num_allocation_points;
       (* [Cbacktrace_stack] decrements the contents of the fixed backtrace
          top of stack register by two words, and returns that new value. *)
       Cop (Cbacktrace_stack, []);
       Cop (Creturn_address, []);  (* return address for the current frame *)
      ]),
    (* [Ctailrec_entry_point] in this position ensures that we do not run
       the bucket calculation code when re-entering the same function from
       a tail call. *)
    Csequence (Ctailrec_entry_point, body))

let code_for_allocation_point ~value's_header ~alloc_point_number
      ~backtrace_bucket =
  let pc = Ident.create "pc" in
  let existing_profinfo = Ident.create "existing_profinfo" in
  let new_profinfo = Ident.create "new_profinfo" in
  let new_profinfo' = Ident.create "new_profinfo'" in
  let new_profinfo'' = Ident.create "new_profinfo''" in
  let profinfo = Ident.create "profinfo" in
  let offset_into_backtrace_bucket =
    ((2 * Arch.size_addr) * alloc_point_number)
  in
  let address_of_profinfo =
    Cop (Cadda, [
      Cvar backtrace_bucket;
      Cconst_int offset_into_backtrace_bucket;
    ])
  in
  let address_of_pc =
    Cop (Cadda, [
      Cvar backtrace_bucket;
      Cconst_int (offset_into_backtrace_bucket + Arch.size_addr);
    ])
  in
  let do_not_use_override_profinfo =
    (* Determine whether values should be annotated with a user-specified
       profinfo. *)
    Cop (Ccmpi Ceq, [
      Cop (Cload Word, [use_override_profinfo]);
      Cconst_int 0;
    ])
  in
  (* CR mshinwell: ensure these match the C code *)
  let profinfo_shift = Cconst_int 42 in
  let max_profinfo = Cconst_int 0x3f_ffff in
  let generate_new_profinfo =
    (* When a new profinfo value is required, we obtain the current
       program counter, and store it together with a fresh profinfo value
       into the current backtrace hash table bucket. *)
    Clet (pc, Cop (Cprogram_counter, []),
      Clet (new_profinfo,
        Clet (new_profinfo', Cop (Cload Word, [profinfo_counter]),
          Cifthenelse (
            Cop (Ccmpi Cgt, [Cvar new_profinfo'; max_profinfo]),
            Cconst_int 0,  (* profiling counter overflow *)
            Clet (new_profinfo'',
              Cop (Caddi, [Cvar new_profinfo'; Cconst_int 1]),
              Csequence (
                Cop (Cstore Word,
                  [profinfo_counter; Cvar new_profinfo'']),
                Cvar new_profinfo'
              )))),
        Csequence (
          Csequence (
            Cop (Cstore Word, [address_of_pc; Cvar pc]),
            Cop (Cstore Word, [address_of_profinfo; Cvar new_profinfo])),
          Cop (Clsl, [Cvar new_profinfo; profinfo_shift]))))
  in
  (* Check if we have already allocated a profinfo value for this allocation
     point with the current backtrace.  If so, use that value; if not,
     allocate a new one. *)
  Clet (existing_profinfo, Cop (Cload Word, [address_of_profinfo]),
    Clet (profinfo,
      Cifthenelse (
        Cop (Ccmpa Ceq, [Cvar existing_profinfo; Cconst_pointer 0]),
        Cvar existing_profinfo,
        Cifthenelse (do_not_use_override_profinfo,
          generate_new_profinfo,
          Cop (Cload Word, [override_profinfo]))),
      (* [profinfo] is already shifted by [PROFINFO_SHIFT]. *)
      Cop (Cor, [Cvar profinfo; Cconst_natint value's_header])))

let instrument_function_body expr ~backtrace_bucket =
  let next_alloc_point_number = ref 0 in
  (* CR mshinwell: misleading variable name *)
  let contains_calls = ref false in
  let rec instrument_headers expr =
    (* Instrument the calculation of block headers so as to insert profiling
       information. *)
    match expr with
    | Cblockheader value's_header ->
      let alloc_point_number = !next_alloc_point_number in
      incr next_alloc_point_number;
      code_for_allocation_point ~value's_header ~alloc_point_number
        ~backtrace_bucket
    | (Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_symbol _
      | Cconst_pointer _ | Cconst_natpointer _ | Cvar _) -> expr
    | Clet (id, e1, e2) ->
      Clet (id, instrument_headers e1, instrument_headers e2)
    | Cassign (id, e) ->
      Cassign (id, instrument_headers e)
    | Ctuple es ->
      Ctuple (List.map instrument_headers es)
    | Cop (op, es) ->
      begin match op with
      | Calloc | Capply _
      | Cextcall (_, _, true, _) -> contains_calls := true
      | _ -> ()
      end;
      Cop (op, List.map instrument_headers es)
    | Csequence (e1, e2) ->
      Csequence (instrument_headers e1, instrument_headers e2)
    | Cifthenelse (e1, e2, e3) ->
      Cifthenelse (instrument_headers e1, instrument_headers e2,
        instrument_headers e3)
    | Cswitch (e, is, es) ->
      Cswitch (instrument_headers e, is, Array.map instrument_headers es)
    | Cloop e ->
      Cloop (instrument_headers e)
    | Ccatch (n, ids, e1, e2) ->
      Ccatch (n, ids, instrument_headers e1, instrument_headers e2)
    | Cexit (n, es) ->
      Cexit (n, List.map instrument_headers es)
    | Ctrywith (e1, id, e2) ->
      Ctrywith (instrument_headers e1, id, instrument_headers e2)
    | Ctailrec_entry_point -> Ctailrec_entry_point
  in
  let expr = instrument_headers expr in
  !next_alloc_point_number, !contains_calls, expr

let fundecl decl =
  if not !Clflags.allocation_profiling then
    { decl with
      fun_body = Csequence (Ctailrec_entry_point, decl.fun_body);
    }
  else
    let backtrace_bucket = Ident.create "backtrace_bucket" in
    let num_allocation_points, contains_calls, body =
      instrument_function_body decl.fun_body ~backtrace_bucket
    in
    (* Even if there are no allocation points, we must record a backtrace
       stack frame unless the function is a leaf function. *)
    if num_allocation_points < 1 && (not contains_calls) then decl
    else
      let body =
        add_prologue ~body ~num_allocation_points ~backtrace_bucket
      in
      { decl with
        fun_body = body;
        fun_num_instrumented_alloc_points = num_allocation_points;
      }
