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

let size_in_words_of_trie_node ~num_allocation_points ~call_types =
  let module C = Cmm.Call_type in
  let direct =
    C.Table.find_default call_types C.Direct_call ~default:0
  in
  let external_direct =
    C.Table.find_default call_types C.External_direct_call ~default:0
  in
  let indirect =
    C.Table.find_default call_types C.Indirect_call ~default:0
  in
  let non_self_direct_tail =
    C.Table.find_default call_types C.Non_self_direct_tailcall ~default:0
  in
  let indirect_tail =
    C.Table.find_default call_types C.Indirect_tailcall ~default:0
  in
  let direct_calls = direct + external_direct + non_self_direct_tail in
  let indirect_calls = indirect + indirect_tail in
  2*num_allocation_points + direct_calls + 2*indirect_calls

let code_to_allocate_trie_node ~num_allocation_points ~call_types =
  let size = size_in_words_of_trie_node ~num_allocation_points ~call_types in
  let node = Ident.create "node" in
  let open Cmm in
  Cop (Cextcall ("calloc", [| Int |], false, Debuginfo.none),
    [Cconst_int size;
     Cconst_int Sys.word_size;
    ])

let function_prologue ~num_allocation_points ~call_types =
  let node_hole = Ident.create "node_hole" in
  let node = Ident.create "node" in
  let new_node = Ident.create "new_node" in
  let open Cmm in
  Clet (node_hole, Cop (Calloc_profiling_node_hole, []),
    Clet (node, Cop (Cload Word, [Cvar node_hole]),
      Cifthenelse (Cop (Ccmpi Cne, [Cvar node, Cconst_int 0]),
        Cvar node,
        Clet (new_node,
          code_to_allocate_trie_node ~num_allocation_points ~call_types,
          Csequence (
            Cop (Cstore Word, [Cvar node_hole; Cvar new_node]),
            Cvar new_node)))))

let use_override_profinfo =
  Cmm.Cconst_symbol "caml_allocation_profiling_use_override_profinfo"

let override_profinfo =
  Cmm.Cconst_symbol "caml_allocation_profiling_override_profinfo"

let profinfo_counter =
  Cmm.Cconst_symbol "caml_allocation_profiling_profinfo"

let code_for_allocation_point ~value's_header ~alloc_point_number ~node =
  let pc = Ident.create "pc" in
  let existing_profinfo = Ident.create "existing_profinfo" in
  let new_profinfo = Ident.create "new_profinfo" in
  let new_profinfo' = Ident.create "new_profinfo'" in
  let new_profinfo'' = Ident.create "new_profinfo''" in
  let profinfo = Ident.create "profinfo" in
  let offset_into_node =
    ((2 * Arch.size_addr) * alloc_point_number)
  in
  let open Cmm in
  let address_of_profinfo =
    Cop (Cadda, [
      Cvar node;
      Cconst_int offset_into_node;
    ])
  in
  let address_of_pc =
    Cop (Cadda, [
      Cvar node;
      Cconst_int (offset_into_node + Arch.size_addr);
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
       into the current trie node. *)
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

let code_for_direct_call ~node ~num_instrumented_alloc_points ~callee
      ~direct_call_point_index =
  let offset_in_trie_node_in_words =
    1 + num_instrumented_alloc_points*2 + direct_call_point_index*2
  in
  let within_node =
    Cop (Caddi, [
      node;
      Cconst_int (offset_in_trie_node * Sys.word_size);
    ]
  in
  Csequence (
    Cop (Cstore Word, [within_node; Cprogram_counter]),
    Cop (Calloc_profiling_load_node_hole_ptr,
      [Cop (Caddi, [within_node; Sys.word_size])]))

let instrument_function_body expr ~backtrace_bucket ~fun_name =
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
      (* CR mshinwell: what about bounds checks? *)
      | Calloc -> contains_calls := true
      | Capply _ ->
        begin match es with
        | (Cconst_symbol s)::_ when s = fun_name -> ()
        | _ -> contains_calls := true
        end
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
        ~fun_name:decl.fun_name
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
