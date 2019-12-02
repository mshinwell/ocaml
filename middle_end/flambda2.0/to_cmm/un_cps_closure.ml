(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Flambda2
open! Flambda.Import

(* Mappings from elements of a closure to offsets. *)

type env = {
  closure_offsets : int Closure_id.Map.t;
  env_var_offsets : int Var_within_closure.Map.t;
}
(** Public state to store the mapping from elements of a closure to offset. *)

let print_env fmt env =
  Format.fprintf fmt "{@[<v>closures: @[<v>%a@]@,env_vars: @[<v>%a@]@]}"
    (Closure_id.Map.print Numbers.Int.print) env.closure_offsets
    (Var_within_closure.Map.print Numbers.Int.print) env.env_var_offsets

let empty_env = {
  closure_offsets = Closure_id.Map.empty;
  env_var_offsets = Var_within_closure.Map.empty;
}

let add_closure_offset env closure offset =
  match Closure_id.Map.find closure env.closure_offsets with
  | o -> assert (o = offset); env
  | exception Not_found ->
      let closure_offsets =
        Closure_id.Map.add closure offset env.closure_offsets
      in
      { env with closure_offsets; }

let add_env_var_offset env env_var offset =
  match Var_within_closure.Map.find env_var env.env_var_offsets with
  | o -> assert (o = offset); env
  | exception Not_found ->
      let env_var_offsets =
        Var_within_closure.Map.add env_var offset env.env_var_offsets
      in
      { env with env_var_offsets; }

let closure_offset env closure =
  match Closure_id.Map.find closure env.closure_offsets with
  | exception Not_found ->
      Misc.fatal_errorf
        "Closure id %a has no associated offset" Closure_id.print closure
  | res -> res

let env_var_offset env env_var =
  match Var_within_closure.Map.find env_var env.env_var_offsets with
  | exception Not_found ->
      Misc.fatal_errorf "Var within closure id %a has no associated offset"
        Var_within_closure.print env_var
  | res -> res


(* Compute offsets for elements within a closure block

   Closure_ids and environment values within a closure block can occur
   in more than one closure blocks, but need to have the same offset
   in all blocks where they appear.

   We assume an ideal (i.e. compact) layout for a block containing
   a set of closures is the following:

   |----------------------|
   | header               |
   |----------------------| offset 0
   | fn_block_0           |
   | (pos 0, size 2 or 3) |
   |----------------------|
   | Infix_header         |
   |----------------------|
   | fn_block_1           |
   | (pos x, size 2 or 3) |  (x=3 if fn_block_0 is of size 2 for instance)
   |----------------------|
   | Infix_header         |
   |----------------------|
   .                      .
   .                      .
   .                      .
   |----------------------|
   | Infix_header         |
   |----------------------|
   | fn_block             |
   | (last closure slot)  |
   |----------------------|
   | Env_value (slot 0)   |
   |----------------------|
   | Env_value (slot 1)   |
   |----------------------|
   .                      .
   .                      .
   |----------------------|
   | Env_value (last slot)|
   |----------------------|

   However, that ideal layout may not be possible in certain circumstances.
   There are two ways the actual layout of a block might differ from this ideal:
   - there may be arbitrary holes between slots (i.e. unused words in the block)
   - it is possible for some env slots to occur before some closure slots

   Notably, since symbols imported from other compilation units have their offset fixed.
   For instance, it is perfectly possible to have a situation where an env_value has
   been fixed at offset 3 (because it is in a simple closure with one function of
   arity > 1 in another cmx), however it is in a closure set with more than one closure
   in the current compilation unit. In this case, it is impossible to make all the closures
   fit before the env_value in the closure block.
*)

type layout_slot =
  | Env_var of Var_within_closure.t
  | Infix_header
  | Closure of Closure_id.t

type layout = (int * layout_slot) list

let order_closures env l acc =
  List.fold_left (fun acc closure ->
      let o = closure_offset env closure in
      Numbers.Int.Map.add o (Closure closure) acc
    ) acc l

let order_env_vars env l acc =
  List.fold_left (fun acc env_var ->
      let o = env_var_offset env env_var in
      Numbers.Int.Map.add o (Env_var env_var) acc
    ) acc l

let order env closures env_vars =
  order_env_vars env env_vars
    (order_closures env closures Numbers.Int.Map.empty)

let rec layout_aux map acc i =
  match Numbers.Int.Map.find_first (fun j -> j >= i) map with
  | exception Not_found -> List.rev acc
  | (j, (Closure _ as slot)) when j = 0 ->
      assert (i = j);
      assert (acc = []);
      layout_aux map [(0, slot)] (i + 1)
  | (j, (Closure _ as slot)) ->
      layout_aux map ((j, slot) :: (j - 1, Infix_header) :: acc) (j + 1)
  | (j, (Env_var _ as slot)) ->
      layout_aux map ((j, slot) :: acc) (j + 1)
  | (_, Infix_header) ->
      (* Internal invariant: such layout slots are not generated by
         the {order} function, so they should not appear. *)
      assert false

let layout env closures env_vars =
  let map = order env closures env_vars in
  layout_aux map [] 0

let print_layout_slot fmt = function
  | Env_var v -> Format.fprintf fmt "var %a" Var_within_closure.print v
  | Infix_header -> Format.fprintf fmt "infix_header"
  | Closure cid -> Format.fprintf fmt "closure %a" Closure_id.print cid

let print_layout fmt l =
  Format.fprintf fmt "@[<v>";
  List.iter (fun (i, slot) ->
      Format.fprintf fmt "@[<h>%d %a@]@," i print_layout_slot slot
    ) l;
  Format.fprintf fmt "@]"


(* Greedy algorithm *)

module Greedy = struct
  (** Greedy algorithm for assigning slots to closures and environment variables.
      Slots are assigned using a "first comes, first served" basis, filling
      upwards from 0.
      As much as is possible, the algorithm tries and put first all the closure
      slots, and then all the env_var slots, however, that may be impossible
      because of constraints read from a cmx.

      This strategy should be able to correctly compute offsets for all legitimate
      situations, with no expected blowup of computation time. However the generated
      offsets can be far from optimal (i.e. leave more holes than necessary).

      CR Gbury: when do we import info from a cmx ? When creating new slots (check for
      info in external compunits ?).
  *)

  (* Internal types *)

  type slot_desc =
    | Closure of Closure_id.t
    | Env_var of Var_within_closure.t

  type slot_pos =
    | Assigned of int
    | Unassigned

  type set_of_closures = {
    id : int;
    mutable unallocated_closure_slots : slot list;
    mutable unallocated_env_var_slots : slot list;
    mutable allocated_slots : slot Numbers.Int.Map.t;
  }

  and slot = {
    desc : slot_desc;
    mutable pos : slot_pos;
    mutable size : int;
    mutable sets : set_of_closures list;
  }

  type state = {
    closures : slot Closure_id.Map.t;
    env_vars : slot Var_within_closure.Map.t;
    sets_of_closures : set_of_closures list;
  }
  (** Intermediate state to store slots for closures and environment variables
      before computing the actual offsets of these elements within a block. *)


  (* Create structures *)

  (* create a fresh slot (with no position allocated yet) *)
  let create_slot size desc = {
    desc; size;
    pos = Unassigned;
    sets = []; }

  let make_set =
    let c = ref 0 in
    (fun _ ->
       incr c;
       {
         id = !c;
         unallocated_closure_slots = [];
         unallocated_env_var_slots = [];
         allocated_slots = Numbers.Int.Map.empty;
       }
    )

  let empty_state = {
    closures = Closure_id.Map.empty;
    env_vars = Var_within_closure.Map.empty;
    sets_of_closures = [];
  }

  (* debug printing *)
  let print_set_id fmt s = Format.fprintf fmt "%d" s.id

  let print_set_ids fmt l =
    List.iter (function s ->
        Format.fprintf fmt "%a,@ " print_set_id s
      ) l

  let print_desc fmt = function
    | Closure c -> Format.fprintf fmt "%a" Closure_id.print c
    | Env_var v -> Format.fprintf fmt "%a" Var_within_closure.print v

  let print_slot_desc fmt s = print_desc fmt s.desc

  let print_slot_descs fmt l =
    List.iter (function s ->
        Format.fprintf fmt "%a,@ " print_slot_desc s
      ) l

  let print_slot_pos fmt = function
    | Assigned i -> Format.fprintf fmt "%d" i
    | Unassigned -> Format.fprintf fmt "?"

  let print_slot fmt s =
    Format.fprintf fmt
      "@[<hov>[pos: %a;@ size: %d;@ desc: %a;@ sets: %a]@]"
      print_slot_pos s.pos s.size print_desc s.desc print_set_ids s.sets

  let print_set fmt s =
    Format.fprintf fmt
      "%d:{ @[<v>unallocated_closures: @[<hov>%a@];@ unallocated_env_vars: @[<hov>%a@];@ allocated: @[<hov>%a@]@]}@]"
      s.id
      print_slot_descs s.unallocated_closure_slots
      print_slot_descs s.unallocated_env_var_slots
      (Numbers.Int.Map.print print_slot) s.allocated_slots

  let print_sets fmt l =
    List.iter (function s ->
        Format.fprintf fmt "%a,@ " print_set s
      ) l

  let print fmt state =
    Format.fprintf fmt
      "@[<v 2>{ closures: @[<hov>%a@];@ env_vars: @[<hov>%a@];@ sets: @[<hov>%a@]@ }@]"
      (Closure_id.Map.print print_slot) state.closures
      (Var_within_closure.Map.print print_slot) state.env_vars
      print_sets state.sets_of_closures
  [@@warning "-32"]

  (* Slots *)

  let is_closure_slot slot =
    match slot.desc with
    | Closure _ ->
        assert (slot.size = 2 || slot.size = 3);
        true
    | Env_var _ ->
        assert (slot.size = 1);
        false

  let add_slot_offset_to_set offset slot set =
    let map = set.allocated_slots in
    assert (not (Numbers.Int.Map.mem offset map));
    let map = Numbers.Int.Map.add offset slot map in
    set.allocated_slots <- map

  let add_slot_offset env slot offset =
    assert (slot.pos = Unassigned);
    slot.pos <- Assigned offset;
    List.iter (add_slot_offset_to_set offset slot) slot.sets;
    match slot.desc with
    | Closure c -> add_closure_offset env c offset
    | Env_var v -> add_env_var_offset env v offset

  (* Sets of Closures *)

  let add_set_to_state state set =
    { state with sets_of_closures = set :: state.sets_of_closures; }

  let add_unallocated_slot_to_set slot set =
    slot.sets <- set :: slot.sets;
    match slot.desc with
    | Closure _ ->
        set.unallocated_closure_slots <- slot :: set.unallocated_closure_slots
    | Env_var _ ->
        set.unallocated_env_var_slots <- slot :: set.unallocated_env_var_slots

  (* Accumulator state *)

  let add_closure_slot state closure slot =
    let closures = Closure_id.Map.add closure slot state.closures in
    { state with closures; }

  let add_env_var_slot state var slot =
    { state with env_vars = Var_within_closure.Map.add var slot state.env_vars; }

  let find_closure_slot state closure =
    Closure_id.Map.find_opt closure state.closures

  let find_env_var_slot state var =
    Var_within_closure.Map.find_opt var state.env_vars


  (* Create slots (and create the cross-referencing). *)

  let rec create_closure_slots set state = function
    | [] -> state
    | (c, def) :: r ->
        let s, state =
          match find_closure_slot state c with
          | Some s -> s, state
          | None ->
              let parity = Function_declaration.params_arity def in
              let arity = List.length parity in
              let size = if arity = 1 then 2 else 3 in
              let s = create_slot size (Closure c) in
              s, add_closure_slot state c s
        in
        let () = add_unallocated_slot_to_set s set in
        create_closure_slots set state r

  let rec create_env_var_slots set state = function
    | [] -> state
    | v :: r ->
        let s, state =
          match find_env_var_slot state v with
          | Some s -> s, state
          | None ->
              let s = create_slot 1 (Env_var v) in
              s, add_env_var_slot state v s
        in
        let () = add_unallocated_slot_to_set s set in
        create_env_var_slots set state r

  let create_slots_for_set state used_closure_vars set_id =
    let set = make_set set_id in
    let state = add_set_to_state state set in
    (* Fill closure slots *)
    let function_decls = Set_of_closures.function_decls set_id in
    let closure_map = Function_declarations.funs function_decls in
    let closures = Closure_id.Map.bindings closure_map in
    let state = create_closure_slots set state closures in
    (* Fill env var slots *)
    let env_map =
      Var_within_closure.Map.filter (fun clos_var _bound_to ->
          Var_within_closure.Set.mem clos_var used_closure_vars)
        (Set_of_closures.closure_elements set_id)
    in
    let env_vars = List.map fst (Var_within_closure.Map.bindings env_map) in
    let state = create_env_var_slots set state env_vars in
    state


  (* Folding functions.
     To avoid pathological cases in allocating slots to offsets,
     folding on slots is done by consuming the first unallocated
     slot of each set of closures, and then repeating until all
     slots have been consumed. *)

  let rec fold_on_unallocated_closure_slots f acc state =
    let has_work_been_done = ref false in
    let aux acc set =
      match set.unallocated_closure_slots with
      | [] -> acc
      | slot :: r ->
          has_work_been_done := true;
          set.unallocated_closure_slots <- r;
          f acc slot
    in
    let res = List.fold_left aux acc state.sets_of_closures in
    if not !has_work_been_done then res
    else fold_on_unallocated_closure_slots f res state

  let rec fold_on_unallocated_env_var_slots f acc state =
    let has_work_been_done = ref false in
    let aux acc set =
      match set.unallocated_env_var_slots with
      | [] -> acc
      | slot :: r ->
          has_work_been_done := true;
          set.unallocated_env_var_slots <- r;
          f acc slot
    in
    let res = List.fold_left aux acc state.sets_of_closures in
    if not !has_work_been_done then res
    else fold_on_unallocated_env_var_slots f res state


  (* Find the first space available to fit a given slot.

     This function returns the first free offset with enough space to
     fit the slot (potential header included), but points at the start
     of the free space (so the header word for closure which need it).
     Function {assign_offset} is here to compute the actual offset/position
     from this free space start position.

     This function is abit more compicated than necessary because each
     slot's size does not include the header for closures. There are two
     reasons for that choice:
     - the closure slot at offset 0 does not need a header since it uses
       the header of the whole block, so the necessity of a header is
       actually dependant on the position of the closure slot.
     - that way, the offset/position of a slot corresponds to the actual
       ocaml pointer (which points at the first field of a block rather
       than the header).

  *)

  let first_free_offset slot map start =
    (* space needed to fit a slot at the current offset. *)
    let needed_space curr =
      if is_closure_slot slot && curr <> 0 then slot.size + 1 else slot.size
    in
    (* first offset used by a slot *)
    let first_used_by s =
      match s.pos with
      | Unassigned -> assert false
      | Assigned pos ->
          if is_closure_slot s && pos <> 0 then pos - 1 else pos
    in
    (* first potentially free offset after a slot *)
    let first_free_after slot =
      match slot.pos with
      | Unassigned -> assert false
      | Assigned i -> i + slot.size
    in
    (* Adjust a starting position to not point in the middle of a block. *)
    let adjust curr =
      match Numbers.Int.Map.find_last (fun i -> i <= curr) map with
      | exception Not_found -> curr
      | (j, s) ->
          assert (Assigned j = s.pos);
          let first_free_after = j + s.size in
          max curr first_free_after
    in
    (* find the first available space for the slot. *)
    let rec loop curr =
      match Numbers.Int.Map.find_first (fun i -> i >= curr) map with
      | exception Not_found -> curr
      | (_, next_slot) ->
          let available_space = (first_used_by next_slot) - curr in
          assert (available_space >= 0);
          if available_space >= needed_space curr then
            curr
          else
            loop (first_free_after next_slot)
    in
    loop (adjust start)


  (** Assign an offset using the current offset,
      assuming there is enough space *)
  let assign_offset slot offset =
    if not (is_closure_slot slot) then offset
    (* closure need a header (infix_tag) before them, except for
       the first one (which uses the closure block header). *)
    else if offset = 0 then offset else offset + 1

  (* Loop to find the first free offset available for a slot
     given the set of sets in which it appears. *)

  let rec first_available_offset slot start first_set other_sets =
    let aux ((_, offset) as acc) s =
      let new_offset = first_free_offset slot s.allocated_slots offset in
      assert (new_offset >= offset);
      if new_offset = offset then acc
      else (true, new_offset)
    in
    let start = first_free_offset slot first_set.allocated_slots start in
    let changed, offset = List.fold_left aux (false, start) other_sets in
    if not changed then
      assign_offset slot offset
    else
      first_available_offset slot offset first_set other_sets

  let first_available_offset slot start = function
    | s :: r -> first_available_offset slot start s r
    | [] ->
        (* Internal invariant: a slot cannot have an empty list of
           sets it belongs to (at least not slots for which we need to
           assign an offset), thus this case cannot happen. *)
        assert false

  (* Assign offsets to closure slots *)

  let assign_slot_offset env slot =
    match slot.pos with
    | Unassigned ->
        let offset = first_available_offset slot 0 slot.sets in
        add_slot_offset env slot offset
    | Assigned _pos ->
        env

  let assign_closure_offsets state env =
    fold_on_unallocated_closure_slots assign_slot_offset env state

  let assign_env_var_offsets state env =
    fold_on_unallocated_env_var_slots assign_slot_offset env state

  (* Tansform an internal accumulator state for slots into
     an actual mapping that assigns offsets.*)
  let finalize state =
    let env = empty_env in
    let env = assign_closure_offsets state env in
    let env = assign_env_var_offsets state env in
    env

end

(* Iter on all sets of closures of a given program. *)

module Iter_on_sets_of_closures = struct

  let rec expr f e =
    match (Expr.descr e : Expr.descr) with
    | Let e' -> let_expr f e'
    | Let_cont e' -> let_cont f e'
    | Apply e' -> apply_expr f e'
    | Apply_cont e' -> apply_cont f e'
    | Switch e' -> switch f e'
    | Invalid e' -> invalid f e'

  and named f n =
    match (n : Named.t) with
    | Simple _ | Prim _ -> ()
    | Set_of_closures s ->
        let () = f None s in
        set_of_closures f s

  and let_expr f t =
    Let.pattern_match t ~f:(fun ~bound_vars:_ ~body ->
        let e = Let.defining_expr t in
        named f e;
        expr f body
      )

  and let_cont f = function
    | Let_cont.Non_recursive { handler; _ } ->
        Non_recursive_let_cont_handler.pattern_match handler ~f:(fun k ~body ->
            let h = Non_recursive_let_cont_handler.handler handler in
            let_cont_aux f k h body
          )
    | Let_cont.Recursive handlers ->
        Recursive_let_cont_handlers.pattern_match handlers ~f:(fun ~body conts ->
            assert (not (Continuation_handlers.contains_exn_handler conts));
            let_cont_rec f conts body
          )

  and let_cont_aux f k h body =
    continuation_handler f k h;
    expr f body

  and let_cont_rec f conts body =
    let map = Continuation_handlers.to_map conts in
    Continuation.Map.iter (continuation_handler f) map;
    expr f body

  and continuation_handler f _ h =
    let h = Continuation_handler.params_and_handler h in
    Continuation_params_and_handler.pattern_match h ~f:(fun _ ~handler ->
        expr f handler
      )

  (* Expression application, continuation application and Switches
     only use single expressions and continuations, so no sets_of_closures
     can syntatically appear inside. *)
  and apply_expr _ _ = ()

  and apply_cont _ _ = ()

  and switch _ _ = ()

  and invalid _ _ = ()

  and set_of_closures f s =
    let decls = Set_of_closures.function_decls s in
    let map = Function_declarations.funs decls in
    Closure_id.Map.iter (fun_decl f) map

  and fun_decl f _ decl =
    let t = Function_declaration.params_and_body decl in
    Function_params_and_body.pattern_match t
      ~f:(fun ~return_continuation:_ _exn_k _args ~body ~my_closure:_ ->
          expr f body
        )

  let computation f c =
    Flambda_static.Program_body.Computation.iter_expr c ~f:(expr f)

  let static_structure_aux f
      ((S (symbs, st)) : Flambda_static.Program_body.Static_structure.t0) =
    match symbs, st with
    | Set_of_closures r, Set_of_closures s ->
        f (Some r.closure_symbols) s;
        set_of_closures f s
    | _ -> ()

  let static_structure f s =
    List.iter (static_structure_aux f) s

  let definition f (d : Flambda_static.Program_body.Definition.t) =
    Flambda_static.Program_body.Definition.iter_computation d ~f:(computation f);
    static_structure f d.static_structure

  let body f b =
    Flambda_static.Program_body.iter_definitions b ~f:(definition f)

  let program f t =
    Flambda_static.Program.iter_body t ~f:(body f)

end

let compute_offsets program =
  let state = ref Greedy.empty_state in
  let used_closure_vars =
    Name_occurrences.closure_vars (Flambda_static.Program.free_names program)
  in
  let aux _ s =
    state := Greedy.create_slots_for_set !state used_closure_vars s
  in
  Iter_on_sets_of_closures.program aux program;
  Greedy.finalize !state


(* Map on each function body once.
   This is useful to avoid a global state of translated function bodies *)


(* CR Gbury: currently closure_name and closure_id_name **must** be in sync
             (which is manual task somewhat complex). It would be better to
             have a unique mapping from closure_id to linkage name somewhere. *)
let closure_name id =
  let compunit = Closure_id.get_compilation_unit id in
  let name = Compilation_unit.get_linkage_name compunit in
  Format.asprintf "%a__%s" Linkage_name.print name (Closure_id.to_string id)

let closure_id_name o id =
  match o with
  | None -> closure_name id
  | Some _map -> closure_name id
  (*
      (* CR Gbury: is this part really necessary ? why not always
                   return closure_name id ? *)
      let s = Closure_id.Map.find id map in
      let name = Symbol.linkage_name s in
      Format.asprintf "%a" Linkage_name.print name
  *)

let closure_code s = Format.asprintf "%s_code" s

let map_on_function_decl f program =
  (* CR vlaviron: Why was this Code_id ? *)
  let map = ref Closure_id.Map.empty in
  let aux o s =
    let decls = Set_of_closures.function_decls s in
    let funs = Function_declarations.funs decls in
    Closure_id.Map.iter (fun closure_id decl ->
        let name = closure_code (closure_id_name o closure_id) in
        if not (Closure_id.Map.mem closure_id !map) then
          map := Closure_id.Map.add closure_id (f name closure_id decl) !map
      ) funs
  in
  Iter_on_sets_of_closures.program aux program;
  !map

