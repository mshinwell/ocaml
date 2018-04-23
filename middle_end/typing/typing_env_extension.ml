(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Make (T : sig
  include Flambda_type0_internal_intf.S
end) (Typing_env0 : sig
  include Typing_env0_intf.S
    with type typing_environment := T.typing_environment
    with type env_extension := T.env_extension
    with type flambda_type := T.flambda_type
    with type t_in_context := T.t_in_context
    with type 'a ty = 'a T.ty
    with type 'a unknown_or_join = 'a T.unknown_or_join
end) (Meet_and_join : sig
  include Meet_and_join_intf.S_for_types
    with type t_in_context := T.t_in_context
    with type env_extension := T.env_extension
    with type flambda_type := T.flambda_type
end) (Type_equality : sig
  include Type_equality_intf.S
    with type flambda_type := T.flambda_type
end) = struct
  type t = T.env_extension

  open T

  module TE = Typing_env0

  let print ppf t = print_typing_env_extension ppf t

  let fast_equal t1 t2 = (t1 == t2)

  let invariant _t =
    (* CR mshinwell: Work out what to do here.  Probably just a check that
       the ordering is reasonable. *)
    ()

  let empty =
    { first_definitions = [];
      at_or_after_cut_point = Scope_level.Map.empty;
      last_definitions_rev = [];
    }

  let is_empty t = Scope_level.Map.is_empty t.at_or_after_cut_point

  let defined_names t =
    let from_first_definitions =
      Name.Set.of_list (
        List.map (fun (name, _ty) -> name) t.first_definitions)
    in
    Scope_level.Map.fold (fun _level by_sublevel defined_names ->
        Scope_level.Sublevel.Map.fold
          (fun _sublevel (name, (entry : typing_environment_entry0))
               defined_names ->
            match entry with
            | Definition _ -> Name.Set.add name defined_names
            | Equation _ -> defined_names)
          by_sublevel
          defined_names)
      t.at_or_after_cut_point
      from_first_definitions

  let add_definition_at_beginning t name ty =
    let first_definitions = (name, ty) :: t.first_definitions in
    { t with first_definitions; }

  let add_equation t name ty =
    let last_equations_rev = (name, ty) :: t.last_equations_rev in
    { t with last_equations_rev; }

  let meet (env : typing_environment) (t1 : t) (t2 : t) : t =
    if fast_equal t1 t2 then t1
    else if is_empty t1 then t2
    else if is_empty t2 then t1
    else
      let scope_level = Scope_level.next (TE.max_level env) in
      let env = TE.add_or_meet_env_extension env t1 scope_level in
      let env = TE.add_or_meet_env_extension env t2 scope_level in
      TE.cut env_with_t1_and_t2
        ~existential_if_defined_at_or_later_than:scope_level

  let join (env : typing_environment) (t1' : t) (t2' : t) (t1 : t) (t2 : t) =
    if fast_equal t1 t2 then t1
    else if is_empty t1 then empty
    else if is_empty t2 then empty
    else
      let t =
        ...
      in
      let defined_names_t1 = defined_names t1 in
      let defined_names_t2 = defined_names t2 in
      let defined_names_join =
        Name.Set.inter defined_names_t1 defined_names_t2
      in
      let first_definitions =
        List.filter (fun (name, _ty) ->
            Name.Set.mem name defined_names_join)
          t.first_definitions
      in
      let at_or_after_cut_point =
        Scope_level.Map.filter_map (fun _cont_level by_sublevel ->
            let by_sublevel =
              Scope_level.Sublevel.Map.filter_map
                (fun _sublevel ((name, _) as entry) ->
                  if Name.Set.mem name defined_names_join then Some entry
                  else None)
              by_sublevel
            in
            if Scope_level.Sublevel.Map.is_empty by_sublevel then None
            else Some by_sublevel)
          t.at_or_after_cut_point
      in
      (* We don't need to filter [Equation]s in [t] because any [Equation]
         containing a reference to a name defined in exactly one of [t1] or
         [t2] should have had such reference removed by the join operation on
         the type inside the [Equation]. *)
      let t =
        { t with
          first_definitions;
          at_or_after_cut_point;
        }
      in
      invariant t;
      t
end
