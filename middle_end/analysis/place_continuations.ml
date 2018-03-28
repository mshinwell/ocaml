(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017 OCamlPro SAS                                          *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Placement = struct
  type t =
    | After_let of Variable.t
    | After_let_cont of Continuation.Set.t
    | Just_inside_continuation of Continuation.t

  include Hashtbl.Make_with_map (struct
    type nonrec t = t

    let compare t1 t2 =
      match t1, t2 with
      | After_let v1, After_let v2 -> Variable.compare v1 v2
      | After_let _, _ -> -1
      | _, After_let _ -> 1
      | After_let_cont conts1, After_let_cont conts2 ->
        Continuation.Set.compare conts1 conts2
      | After_let_cont _, _ -> -1
      | _, After_let_cont _ -> 1
      | Just_inside_continuation cont1, Just_inside_continuation cont2 ->
        Continuation.compare cont1 cont2

    let equal t1 t2 = (compare t1 t2 = 0)

    let hash t =
      match t with
      | After_let v -> Hashtbl.hash (0, Variable.hash v)
      | After_let_cont conts ->
        let conts_hash =
          Continuation.Set.fold (fun cont hash ->
              Hashtbl.hash (hash, Continuation.hash cont))
            conts
            0
        in
        Hashtbl.hash (1, conts_hash)
      | Just_inside_continuation cont ->
        Hashtbl.hash (2, Continuation.hash cont)

    let print ppf t =
      match t with
      | After_let var ->
        Format.fprintf ppf "after let-binding of %a" Variable.print var
      | After_let_cont conts ->
        Format.fprintf ppf "after Let_cont binding {%a}"
          Continuation.Set.print conts
      | Just_inside_continuation cont ->
        Format.fprintf ppf "just inside handler of %a" Continuation.print cont
  end)
end

type being_placed = {
  handlers : Flambda.Let_cont_handlers.t;
  handlers_as_map : Flambda.Continuation_handlers.t;
  needed_fvs : Variable.Set.t;
}

type insertion_state = {
  vars_in_scope : Variable.Set.t;
  (* All variables currently in scope. *)
  pending : Flambda.Let_cont_handlers.t list Continuation.Map.t;
  (* From pre-existing continuations to specialised versions thereof.
     (For each pre-existing, possibly mutually-recursive, set of continuations
     there may be multiple specialised versions; hence the "list".) *)
  placing : being_placed list;
  (* Specialised continuations whose corresponding pre-existing continuation
     is in scope and that we are looking to place as soon as all required
     variables are in scope. *)
  placed : Flambda.Let_cont_handlers.t list Placement.Map.t;
  (* Specialised continuations whose placement has been identified.
     There may of course be more than one set of handlers placed at any one
     particular spot. *)
}

let find_insertion_points expr ~vars_in_scope ~new_conts =
  if !Clflags.flambda_invariant_checks then begin
    let all_conts = Flambda.Expr.all_defined_continuations_toplevel expr in
    let add_after = Continuation.Map.keys new_conts in
    let not_defined = Continuation.Set.diff add_after all_conts in
    if not (Continuation.Set.is_empty not_defined) then begin
      Misc.fatal_errorf "Request to place continuation(s) after \
          continuation(s) %a that are not defined in the provided \
          expression:@ \n%a"
        Continuation.Set.print not_defined
        Flambda.Expr.print expr
    end
  end;
  let rec find_insertion_points (expr : Flambda.Expr.t) ~state =
    let passing_var_binder var ~make_placement ~state =
      let vars_in_scope = Variable.Set.add var state.vars_in_scope in
      let placed = ref state.placed in
      let placing =
        Misc.Stdlib.List.filter_map (fun (being_placed : being_placed) ->
            let needed_fvs = Variable.Set.remove var being_placed.needed_fvs in
            if Variable.Set.is_empty needed_fvs then begin
              let placement = make_placement var in
              let already_placed =
                match Placement.Map.find placement !placed with
                | exception Not_found -> []
                | already_placed -> already_placed
              in
              placed :=
                Placement.Map.add placement
                  (being_placed.handlers :: already_placed) !placed;
              None
            end else begin
              Some { being_placed with needed_fvs; }
            end)
          state.placing
      in
      { state with
        placed = !placed;
        placing;
        vars_in_scope;
      }
    in
    let passing_continuation_binding ~name ~state =
      match Continuation.Map.find name state.pending with
      | exception Not_found -> state
      | new_handlers_list ->
        List.fold_left (fun state
                (new_handlers : Flambda.Let_cont_handlers.t) ->
            let pending = Continuation.Map.remove name state.pending in
            let needed_fvs =
              Variable.Set.diff
                (Flambda.Let_cont_handlers.free_variables new_handlers)
                state.vars_in_scope
            in
            let being_placed =
              let handlers_as_map =
                match new_handlers with
                | Non_recursive { name; handler; } ->
                  Continuation.Map.add name handler Continuation.Map.empty
                | Recursive handlers -> handlers
              in
              { handlers = new_handlers;
                handlers_as_map;
                needed_fvs;
              }
            in
            let placing = being_placed :: state.placing in
            { state with
              pending;
              placing;
            })
          state
          new_handlers_list
    in
    let enter_continuation_handlers ~handlers ~state =
      Continuation.Map.fold (fun name
              (handler : Flambda.Continuation_handler.t) state ->
          let params =
            Variable.Set.of_list
              (Flambda.Typed_parameter.List.vars handler.params)
          in
          let vars_in_scope =
            Variable.Set.union state.vars_in_scope params
          in
          let state = { state with vars_in_scope; } in
          let state =
            Variable.Set.fold (fun var state ->
                passing_var_binder var ~make_placement:(fun _var ->
                    Just_inside_continuation name)
                  ~state)
              params
              state
          in
          find_insertion_points handler.handler ~state)
        handlers
        state
    in
    let passing_continuation_bindings ~body ~handlers ~state =
      let state =
        Continuation.Map.fold (fun name _handler state ->
            passing_continuation_binding ~name ~state)
          handlers
          state
      in
      let state =
        Variable.Set.fold (fun var state ->
            passing_var_binder var ~make_placement:(fun _var ->
                After_let_cont (Continuation.Map.keys handlers))
              ~state)
          state.vars_in_scope
          state
      in
      let state = enter_continuation_handlers ~handlers ~state in
      find_insertion_points body ~state
    in
    match expr with
    | Let { var; body; _ } ->
      let state =
        passing_var_binder var ~make_placement:(fun var -> After_let var)
          ~state
      in
      find_insertion_points body ~state
    | Let_cont { body; handlers; } ->
      let handlers = Flambda.Let_cont_handlers.to_continuation_map handlers in
      passing_continuation_bindings ~body ~handlers ~state
    | Let_mutable { body; _ } -> find_insertion_points body ~state
    | Apply _ | Apply_cont _ | Switch _ | Invalid _ -> state
  in
  let state =
    let state =
      { vars_in_scope;
        pending = new_conts;
        placing = [];
        placed = Placement.Map.empty;
      }
    in
    find_insertion_points expr ~state
  in
  if not (Continuation.Map.is_empty state.pending) then begin
    Misc.fatal_errorf "Failed to find pre-existing continuations after \
        which to find placements for the following: %a"
      (Continuation.Map.print
        (Format.pp_print_list Flambda.Let_cont_handlers.print))
      state.pending
  end;
  assert (state.placing = []);
  assert (
    let num_new_conts =
      Continuation.Map.fold (fun _cont new_conts num_new_conts ->
          num_new_conts + List.length new_conts)
        new_conts
        0
    in
    let num_placed =
      Placement.Map.fold (fun _placement handlers_list num_placed ->
          num_placed + List.length handlers_list)
        state.placed
        0
    in
    num_new_conts = num_placed);
  state.placed
