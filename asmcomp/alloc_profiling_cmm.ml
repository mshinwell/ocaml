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

let index_within_node = ref 2 (* Cf. [Node_num_header_words] in the runtime. *)
let alloc_profiling_node = ref (Cvar (Ident.create "dummy"))
let alloc_profiling_node_ident = ref (Ident.create "dummy")
let direct_tail_call_point_indexes = ref []

let something_was_instrumented () =
  !index_within_node > 2

let next_index_within_node ~words_needed =
  let index = !index_within_node in
  index_within_node := !index_within_node + words_needed in
  index

let reset ~alloc_profiling_node_ident:ident =
  index_within_node := 2;
  alloc_profiling_node := Cvar ident;
  alloc_profiling_node_ident := ident;
  direct_tail_call_point_indexes := []

let code_for_function_prologue () =
  let node_hole = Ident.create "node_hole" in
  let new_node = Ident.create "new_node" in
  let must_allocate_node = Ident.create "must_allocate_node" in
  let open Cmm in
  let initialize_direct_call_points_and_return_node =
    let new_node_encoded = Ident.create "new_node_encoded" in
    (* The callee node pointers within direct call points must initially
       point back at the start of the current node. *)
    let indexes = !direct_tail_call_point_indexes in
    let body =
      List.fold_left (fun init_code index ->
          (* Cf. [Direct_callee_node] in the runtime. *)
          let offset_in_bytes = (index + 2) * Arch.size_addr;
          let place = Ident.create "tail_init" in
          Clet (place,
            Cop (Cadda, [Cvar new_node; Cconst_int offset_in_bytes]),
            Cop (Cstore Word, [Cvar place; Cvar new_node_encoded])))
        (Cvar new_node)
        indexes
    in
    match indexes with
    | [] -> body
    | _ ->
      Clet (new_node_encoded,
        (* Cf. [Encode_tail_caller_node] in the runtime. *)
        Cop (Cor, [Cvar new_node; Cconst_int 1]),
        body)
  in
  Clet (node_hole, Cop (Calloc_profiling_node_hole, []),
    Clet (node, Cop (Cload Word, [Cvar node_hole]),
      Clet (must_allocate_node, Cop (Cand, [Cvar node; Cconst_int 1]),
        Cifthenelse (Cvar must_allocate_node,
          Clet (new_node,
            Cop (Cextcall ("caml_allocation_profiling_allocate_node",
              [| Int |], false, Debuginfo.none),
              [Cconst_int (1 + !index_within_node);
               Cop (Cprogram_counter, []);
               Cvar node_hole;
              ]),
            initialize_direct_call_points_and_return_node),
          Cvar node))))

let code_for_allocation_point ~value's_header ~node =
  let existing_profinfo = Ident.create "existing_profinfo" in
  let profinfo = Ident.create "profinfo" in
  let pc = Ident.create "pc" in
  let address_of_profinfo = Ident.create "address_of_profinfo" in
  let index_within_node = next_index_within_node ~words_needed:2 in
  let offset_into_node = Arch.size_addr * index_within_node in
  let open Cmm in
  let generate_new_profinfo =
    (* This will generate a static branch to a function that should usually
       be in the cache, which hopefully gives a good code size/performance
       balance. *)
    Clet (pc, Cop (Cor, [
        (* Cf. [Encode_alloc_point_pc] in the runtime. *)
        Cop (Clsl, [Cop (Cprogram_counter, []); Cconst_int 2]);
          Cconst_int 1]),
      Cop (Cextcall ("caml_alloc_profiling_generate_profinfo", [| Int |],
          false, Debuginfo.none),
        [Cvar pc; Cvar address_of_profinfo]))
  in
  (* Check if we have already allocated a profinfo value for this allocation
     point with the current backtrace.  If so, use that value; if not,
     allocate a new one. *)
  Clet (address_of_profinfo,
    Cop (Cadda, [
      Cvar node;
      Cconst_int offset_into_node;
    ]),
    Clet (existing_profinfo, Cop (Cload Word, [Cvar address_of_profinfo]),
      Clet (profinfo,
        Cifthenelse (
          Cop (Ccmpa Cne, [Cvar existing_profinfo; Cconst_pointer 1]),
          Cvar existing_profinfo,
          generate_new_profinfo),
        (* [profinfo] is already shifted by [PROFINFO_SHIFT]. *)
        Cop (Cor, [Cvar profinfo; Cconst_natint value's_header]))))

type callee =
  | Direct of string
  | Indirect of Cmm.expression

let code_for_call ~node ~index_within_node ~callee ~is_tail =
  let words_needed =
    match callee with
    | Direct -> 3  (* Cf. [Direct_num_fields in the runtime]. *)
    | Indirect -> 2  (* Cf. [Indirect_num_fields in the runtime]. *)
  in
  let index_within_node = next_index_within_node ~words_needed in
  begin match callee with
    (* If this is a direct tail call point, we need to note down its index,
       so the correct initialization code can be emitted in the prologue. *)
    | Direct when is_tail ->
      direct_tail_call_point_indexes :=
        index_within_node::!direct_tail_call_point_indexes
    | Direct _ | Indirect _ -> ()
  end
  let place_within_node = Ident.create "place_within_node" in
  let open Cmm in
  let encode_pc pc =
    (* Cf. [Encode_call_point_pc] in the runtime. *)
    Cop (Cor, [Cop (Clsl, [pc; Cconst_int 2]); Cconst_int 3])
  in
  let within_node ~index =
    Cop (Caddi, [node; Cconst_int (index * Arch.size_addr)])
  in
  Clet (place_within_node,
    within_node ~index:index_within_node,
    Csequence (
      (* This point in the generated code is deemed to be the "call site". *)
      Cop (Cstore Word, [Cvar place_within_node; encode_pc Cprogram_counter]),
      match callee with
      | Direct callee ->
        let callee_slot = Ident.create "callee" in
        Clet (callee_slot,
          within_node ~index:(index_within_node + 1),
          Csequence (
            Cop (Cstore Word, [Cvar callee_slot; encode_pc callee]),
            Cop (Calloc_profiling_load_node_hole_ptr,
              within_node ~index:(index_within_node + 2))))
      | Indirect callee ->
        let node_hole_ptr = Ident.create "node_hole_ptr" in
        let is_tail = if is_tail then Cconst_int 1 else Cconst_int 0 in
        Clet (node_hole_ptr,
          Cop (Cextcall ("caml_allocation_profiling_indirect_node_hole_ptr",
            [callee; Cvar place_within_node; is_tail])),
          Cop (Calloc_profiling_load_node_hole_ptr, [Cvar node_hole_ptr])))

class instruction_selection = object (self)
  inherit Selectgen.selector_generic as super

  method private instrument_direct_call ~lbl ~is_tail =
    let instrumentation =
      code_for_call
        ~node:!alloc_profiling_node
        ~callee:(Direct lbl)
        ~is_tail
    in
    ignore (self#emit_expr env instrumentation)

  method private instrument_indirect_call ~callee ~is_tail =
    (* [callee] is a pseudoregister, so we have to bind it in the environment
       and reference the variable to which it is bound. *)
    let callee_ident = Ident.create "callee" in
    let callee_expr = Cmm.Cvar callee_ident in
    let env = Tbl.add callee_ident callee env in
    let instrumentation =
      code_for_call
        ~node:!alloc_profiling_node
        ~callee:(Indirect (Cmm.Cvar callee_ident))
        ~is_tail
    in
    ignore (self#emit_expr env instrumentation)

  method private maybe_instrument desc ~arg ~res =
    match desc with
    | Iop (Icall_imm lbl) ->
      self#instrument_direct_call ~callee:(Direct lbl) ~is_tail:false
    | Iop Icall_ind ->
      self#instrument_indirect_call ~callee:(Indirect arg.(0)) ~is_tail:false
    | Iop (Itailcall_imm lbl) ->
      self#instrument_direct_call ~callee:(Direct lbl) ~is_tail:true
    | Iop Itailcall_ind ->
      self#instrument_indirect_call ~callee:(Indirect arg.(0)) ~is_tail:true
    | Iop (Iextcall lbl) ->
      self#instrument_direct_call ~callee:(Direct lbl) ~is_tail:false
    | _ -> ()

  method private instrument_allocation_point ~value's_header =


  method private emit_prologue f ~env_after_main_prologue
        ~last_insn_of_main_prologue =
    (* We don't need the prologue unless we inserted some instrumentation.
       This corresponds to adding the prologue if the function contains one
       or more call or allocation points. *)
    if something_was_instrumented () then begin
      let prologue_cmm = code_for_function_prologue () in
      (* Splice the allocation prologue after the main prologue but before the
         function body.  Remember that [instr_seq] points at the last
         instruction (the list is in reverse order). *)
      let last_insn_of_body = instr_seq in
      let first_insn_of_body = ref dummy_instr in
      while not (instr_seq == last_insn_of_main_prologue) do
        first_insn_of_body := instr_seq;
        instr_seq <- instr_seq.next
      done;
      instr_seq <- last_insn_of_main_prologue;
      let node_temp_reg =
        match self#emit_expr env_after_main_prologue prologue_cmm with
        | None ->
          Misc.fatal_error "Alloc_profiling prologue instruction \
              selection did not yield a destination register"
        | Some node_temp_reg -> node_temp_reg
      in
      let node_reg = Tbl.find node env_after_main_prologue in
      self#insert_moves node_temp_reg node_reg;
      if not (!first_insn_of_body == dummy_instr) then begin
        (!first_insn_of_body).next <- instr_seq;
        instr_seq <- last_insn_of_body
      end
    end

  method! insert_debug desc dbg arg res =
    if !Clflags.allocation_profiling then maybe_instrument desc ~arg ~res;
    super#insert_debug desc dbg arg res

  method! insert desc arg res =
    if !Clflags.allocation_profiling then maybe_instrument desc ~arg ~res;
    super#insert desc dbg arg res

  method! emit_blockheader n =
    if !Clflags.allocation_profiling then
      self#instrument_allocation_point ~value's_header:n
    else
      super#emit_blockheader n

  method! initial_env () =
    let env = super#initial_env () in
    if !Clflags.allocation_profiling then
      Tbl.add !alloc_profiling_node_ident (self#regs_for typ_int) env
    else
      env

  method! emit_fundecl f =
    if !Clflags.allocation_profiling then begin
      reset ~alloc_profiling_node_ident:f.Cmm.fun_alloc_profiling_node
    end;
    super#emit_fundecl f

  method! after_body f ~env_after_prologue ~last_insn_of_prologue =
    if !Clflags.allocation_profiling then begin
      self#emit_prologue f ~env_after_main_prologue:env_after_prologue
        ~last_insn_of_main_prologue:last_insn_of_prologue
    end;
    super#after_body f ~env_after_prologue ~last_insn_of_prologue
end
