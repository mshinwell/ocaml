(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module L = Linearize

include Calculate_ranges.Make (struct
  module RD = Reg_with_debug_info

  (* By the time this pass has run, register stamps are irrelevant; indeed,
     there may be multiple registers with different stamps assigned to the
     same location.  As such, we quotient register sets by the equivalence
     relation that identifies two registers iff they have the same name and
     location. *)
  module Key = struct
    include RD

    module Set = RD.Set_distinguishing_names_and_locations
    module Map = RD.Map_distinguishing_names_and_locations
  end

  module Index = Backend_var

  module Subrange_state : sig
    include Compute_ranges_intf.S_subrange_state

    val stack_offset : t -> int
  end = struct
    type t = {
      stack_offset : int;
    }

    let create () =
      { stack_offset = Proc.initial_stack_offset;
      }

    let advance_over_instruction t (insn : L.instruction) =
      let stack_offset =
        match insn.desc with
        | Lop (Istackoffset delta) -> t.stack_offset + delta
        | Lpushtrap -> t.stack_offset + Proc.trap_frame_size_in_bytes
        | Lpoptrap -> t.stack_offset - Proc.trap_frame_size_in_bytes
        | Lend | Lprologue | Lop _ | Lreloadretaddr | Lreturn
        | Llabel _ | Lbranch _ | Lcondbranch _ | Lcondbranch3 _
        | Lswitch _ | Lsetuptrap _ | Lraise _ -> t.stack_offset
      in
      { stack_offset;
      }

    let stack_offset t = t.stack_offset
  end

  module Subrange_info : sig
    include Compute_ranges_intf.S_subrange_info
      with type key := Key.t
      with type subrange_state := Subrange_state.t

    val offset_from_stack_ptr_in_bytes : t -> int
  struct
    type t = {
      reg : Reg.t;
      start_insn : L.instruction;
      offset_from_stack_ptr_in_bytes : int option;
    }

    let create reg ~start_insn ~subrange_state =
      let reg = RD.reg reg in
      let stack_offset = Subrange_state.stack_offset subrange_state in
      let offset_from_stack_ptr_in_bytes =
        match reg.loc with
        | Stack loc ->
          let frame_size = Proc.frame_size ~stack_offset in
          let slot_offset =
            Proc.slot_offset loc ~reg_class:(Proc.register_class reg)
              ~stack_offset
          in
          Some (frame_size - slot_offset)
        | Reg _ | Unknown -> None
      in
      { reg;
        start_insn;
        offset_from_stack_ptr_in_bytes;
      }

    let offset_from_stack_ptr_in_bytes t =
      match t.offset_from_stack_ptr_in_bytes with
      | Some offset -> offset
      | None ->
        Misc.fatal_error "No offset from stack pointer available (register \
          not assigned to the stack)"
  end

  module Range_info : Compute_ranges_intf.S_range_info
    with type key := Key.t
    with type index := Index.t
  = struct
    type is_parameter =
      | Local
      | Parameter of { index : int; }

    type t = {
      provenance : Backend_var.Provenance.t option;
      is_parameter : is_parameter;
    }

    let create _fundecl reg ~start_insn:_ =
      match RD.debug_info reg with
      | None -> None
      | Some debug_info ->
        let var = RD.Debug_info.holds_value_of debug_info in
        let provenance = RD.Debug_info.provenance debug_info in
        let is_parameter =
          match RD.Debug_info.which_parameter debug_info with
          | None -> Local
          | Some index -> Parameter { index; }
        in
        let range_info =
          { provenance;
            is_parameter;
          }
        in
        Some (var, range_info)

    let provenance t = t.provenance
    let is_parameter t = t.is_parameter
  end

  let available_before (insn : L.instruction) =
    match insn.available_before with
    | Unreachable -> Key.Set.empty
    | Ok available_before ->
      RD.Set.fold (fun reg result -> Key.Set.add reg result)
        available_before
        Key.Set.empty

  let end_pos_offset ~prev_insn reg =
    (* If the range is for a register destroyed by a call and which
       ends immediately after a call instruction, move the end of the
       range back very slightly.  The effect is that the register is seen
       in the debugger as available when standing on the call instruction
       but unavailable when we are in the callee (and move to the previous
       frame). *)
    (* CR-someday mshinwell: I wonder if this should be more
       conservative for Iextcall.  If the C callee is compiled with
       debug info then it should describe where any callee-save
       registers have been saved, so when we step back to the OCaml frame
       in the debugger, the unwinding procedure should get register
       values correct.  (I think.)  However if it weren't compiled with
       debug info, this wouldn't happen, and when looking back up into
       the OCaml frame I suspect registers would be wrong.  This may
       not be a great problem once libmonda is hardened, although it
       is possible for this to be subtle and misleading (e.g. an integer
       value being 1 instead of 2 or something.) *)
    match prev_insn with
    | None -> None
    | Some prev_insn ->
      match prev_insn.L.desc with
      | Lop ((Icall_ind _ | Icall_imm _ | Iextcall _) as op) ->
        let destroyed_locations =
          Array.map (fun (reg : Reg.t) -> reg.loc)
            (Proc.destroyed_at_oper (Iop op))
        in
        let holds_immediate = RD.holds_non_pointer reg in
        let on_stack = RD.assigned_to_stack reg in
        let live_across = Reg.Set.mem (RD.reg reg) prev_insn.L.live in
        let remains_available =
          live_across
            || (holds_immediate && on_stack)
        in
        if Array.mem (RD.location reg) destroyed_locations
            || not remains_available
        then
          Some (-1)
        else
          None
      | _ -> None

  let maybe_restart_ranges ~proto_births ~proto_deaths =
    (* Available subranges are allowed to cross points at which the stack
       pointer changes, since we reference the stack slots as an offset from the
       CFA, not from the stack pointer.

       We avoid generating ranges that overlap, since this confuses lldb.  This
       pass may generate ranges that are the same as other ranges, but those
       are deduped in [Dwarf].
    *)
    match !Clflags.debug_full with
    | Some Gdb -> false
    | Some Lldb ->
      (* Work at OCamlPro suggested that lldb requires ranges to be
         completely restarted in the event of any change. *)
      RD.Set.cardinal proto_births <> 0 || RD.Set.cardinal proto_deaths <> 0
    | None -> Misc.fatal_error "Shouldn't be here without [debug_full]"
end)
