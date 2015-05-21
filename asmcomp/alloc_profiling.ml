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

open Cmm

let top_of_stack =
  Ident.create_persistent "caml_allocation_profiling_backtrace_top_of_stack"

let code_for_prologue ~backtrace_bucket ~function_body
      ~num_allocation_points ~add_epilogue =
  (* Upon entry to an OCaml function, the backtrace top of stack pointer
     points at the word holding the hash of all previous frames.  It is
     the responsibility of the callee to push their return address and to
     update the hash.  We make callees do this rather than callers to keep
     code size down; there should be fewer callees than call points.
     To reduce complexity, this is implemented in C as a "noalloc" function;
     the overhead should be small since it's a static branch and the
     C compiler should be able to apply high optimization to the code. *)
  let saved_backtrace_stack_ptr = Ident.create "saved_backtrace_stack_ptr" in
  let with_prologue =
    Clet (saved_backtrace_stack_ptr,
      Cop (Cload Word, [Cvar top_of_stack]),
      Clet (backtrace_bucket,
        Cop (Cextcall ("caml_allocation_profiling_prologue",
            Int,   (* Actually a naked pointer, just don't tell anyone. *)
            false, (* = noalloc *)
            Debuginfo.none),
          [Cconst_int num_allocation_points;
           (* We pass the return address since it's easy to get hold of and
              avoids invoking libunwind. *)
           Creturn_address;
          ]),
        add_epilogue ~function_body ~saved_backtrace_stack_ptr))
  in
  saved_backtrace_stack_ptr, with_prologue

let code_for_exit_point expr ~saved_backtrace_stack_ptr =
  (* Upon exit from an OCaml function, the backtrace stack pointer must
     be restored.  To reduce the chance of error, we restore it to an
     absolute position, instead of using a relative offset. *)
  Csequence (
    Cop (Cstore Word, [saved_backtrace_stack_ptr; Cvar top_of_stack]),
    expr)

let add_epilogue ~function_body ~saved_backtrace_stack_ptr =
  let code_for_exit_point = code_for_exit_point ~saved_backtrace_stack_ptr in
  let rec instrument_exit_points expr =
    match expr with
    | (Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_symbol _
      | Cconst_pointer _ | Cconst_natpointer _ | Cvar _) ->
      code_for_exit_point expr
    | Clet (id, e1, e2) ->
      Clet (id, e1, instrument_exit_points e2)
    | Cassign (id, e) -> code_for_exit_point (Cassign (id, e))
    | Ctuple es -> 
      begin match List.rev es with
      | [] -> code_for_exit_point expr
      | e_last::e_rest ->
        let e_last =
          let id = Ident.create "tuple_result" in
          Clet (id, e_last, code_for_exit_point (Cvar id))
        in
        Ctuple (List.rev (e_last::e_rest))
      end
    (* XXX: note: we could trap extcalls that are not "noalloc" and record
       things, if it helps the veneers *)
    | Cop _ -> expr
    | Csequence (e1, e2) -> Csequence (e1, instrument_exit_points e2)
    | Cifthenelse (e1, e2, e3) ->
      Cifthenelse (e1, instrument_exit_points e2, instrument_exit_points e3)
    | Cswitch (e, is, es) ->
      Cswitch (e, is, Array.map instrument_exit_points es)
    | Cloop _ -> expr
    | Ccatch (n, ids, e1, e2) ->
      Ccatch (n, ids, instrument_exit_points e1, instrument_exit_points e2)
    | Cexit _ -> expr
    | Ctrywith (e1, id, e2) ->
      Ctrywith (instrument_exit_points e1, id, instrument_exit_points e2)
  in
  instrument_exit_points function_body

(* XXX need to think about tail calls *)

let profinfo_counter =
  Ident.create_persistent "caml_allocation_profiling_profinfo"

let code_for_allocation_point ~value's_header ~alloc_point_number
      ~backtrace_bucket =
  let pc = Ident.create "pc" in
  let existing_profinfo = Ident.create "existing_profinfo" in
  let new_profinfo = Ident.create "new_profinfo" in
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
  let generate_new_profinfo =
    Clet (pc, Cop (Cprogram_counter, []),
      Clet (new_profinfo, Cop (Cload Word, [profinfo_counter]),
        Csequence (
          Cop (Cstore Word, [
            Cop (Cadda, [new_profinfo; Cconst_int 1]);
            profinfo_counter;
          ]),
          Csequence (
            Csequence (
              Cop (Cstore Word, [pc, address_of_pc]);
              Cop (Cstore Word, [new_profinfo, address_of_profinfo])),
            new_profinfo))))
  in
  Clet (existing_profinfo, Cop (Cload Word, [address_of_profinfo]),
    Clet (profinfo,
      Cifthenelse (
        (* Check if we have already allocated a profinfo value for this
           allocation point with the current backtrace.  If so, use that
           value; if not, allocate a new one. *)
        Cop (Ccmpa Ceq, [Cvar existing_profinfo; Cconst_pointer 0]),
        generate_new_profinfo,
        existing_profinfo),
      Cop (Cor, [profinfo; value's_header])))

let instrument_function_body expr =
  let next_alloc_point_number = ref 0 in
  let backtrace_bucket = Ident.create "backtrace_bucket" in
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
  in
  let function_body = instrument_headers expr in
  let might_allocate = !next_alloc_point_number > 0 in
  if not might_allocate then
    expr
  else
    let with_prologue =
      (* Add code to push the return address onto the backtrace stack, adjust
         the backtrace stack pointer, and locate the backtrace bucket for the
         current function.
         This binds the [backtrace_bucket] variable used above. *)
      code_for_prologue ~backtrace_bucket ~function_body
        ~num_allocation_points:!next_alloc_point_number
    in
    (* Add code to restore the backtrace stack pointer at the exit points
       of the function. *)
    instrument_exit_points with_prologue

let fundecl decl =
  (* CR mshinwell: prevent [allocation_profiling] being set on 32 bit *)
  if not !Clflags.allocation_profiling then decl
  else
    { decl with
      expression = instrument_function_body decl.expression;
    }
