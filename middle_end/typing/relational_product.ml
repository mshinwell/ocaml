
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

(** A "relational product" represents the indexed product of a number of
    components:

       ------
        |  |
        |  |     (component_i : Component)
      i : Index

    together with relational information between the components expressed
    as a typing environment extension.
*)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module type Name_like = sig
  type t

  include Map.With_set with type t := t
  include Contains_names.S with type t := t

  val kind : t -> Flambda_kind.t
end

module Make
  (Index : Name_like)
  (Component : Name_like)
  (T : Flambda_type0_internal_intf.S)
  (Typing_env : Typing_env_intf.S with module T := T)
  (Typing_env_extension : Typing_env_extension_intf.S with module T := T)
  (Meet_and_join : Meet_and_join_intf.S_both with module T := T)
  (Join_env : Join_env_intf.S with module T := T)
= struct
  open T

  module JE = Join_env
  module TE = Typing_env
  module TEE = Typing_env_extension

  type t = {
    components_by_index : Component.t Index.Map.t;
    env_extension : TEE.t;
  }

  let invariant _t =
    (* CR mshinwell: This should check that the [env_extension] never contains
       [Definition]s for the indexes. *)
    ()

  let create_with_env_extension external_structure env_extension : t =
    let components_by_index =
      let indexs = EVS.to_set external_structure in
      EV.Set.fold (fun index components_by_index ->
          let kind = EV.kind index in
          let fresh_logical_var = LV.create kind in
          let components_by_index =
            EV.Map.add index fresh_logical_var components_by_index_in_result
          in
          components_by_index)
        indexs
        EV.Map.empty
    in
    let t =
      { external_structure;
        components_by_index;
        env_extension;
      }
    in
    invariant t;
    t

  let create external_structure =
    create_with_env_extension external_structure TEE.empty

  let print ppf { external_structure; components_by_index; env_extension; } =
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(components_by_index@ %a)@]@ \
        @[<hov 1>(env_extension@ %a)@])@]"
      (Index.Map.print Component.print) components_by_index
      TEE.print env_extension

  let indexes t = Index.Map.keys t.components_by_index

  type meet_or_join =
    | Meet
    | Join

  let environment_for_meet_or_join (t1 : t) (t2 : t) ~indexes ~all_indexes =
    let components_by_index_in_result, env =
      Index.Set.fold (fun index (components_by_index_in_result, env) ->
          let kind = Index.kind index in
          let env =
            JE.add_definition_central_environment env fresh_name (T.bottom kind)
          in
          if not (Index.Set.mem index indexes) then
            components_by_index_in_result
          else
            let fresh_component = Component.create kind in
            let components_by_index_in_result =
              Index.Map.add index fresh_component components_by_index_in_result
            in
            components_by_index_in_result, env)
        all_indexes
        (Index.Map.empty, env)
    in
    let add_definitions_and_equalities_to_extension (t : t) =
      Index.Map.fold (fun (index, fresh_component) env_extension ->
          let stale_name = Component.name stale_component in
          let kind = Component.kind stale_component in
          let env_extension =
            TEE.add_definition_at_beginning env_extension stale_name
              (T.bottom kind)
          in
          match Component.find index t.components_by_index with
          | exception Not_found -> env_extension
          | stale_component ->
            let fresh_name = Component.name fresh_component in
            let fresh_name_ty =
              T.alias_type_of kind (Simple.name stale_name)
            in
            TEE.add_equation env_extension fresh_name fresh_name_ty)
        components_by_index_in_result
        t.env_extension
    in
    let env_extension1 = add_definitions_and_equalities_to_extension t1 in
    let env_extension2 = add_definitions_and_equalities_to_extension t2 in
    env, env_extension1, env_extension2, external_structure,
      components_by_index_in_result

  let meet env t1 t2 : _ Or_bottom.t =
    if t1 == t2 then Ok t1
    else
      let env = JE.create env in
      let indexes = Index.Set.inter (indexes t1) (indexes t2) in
      if Index.Set.is_empty indexes then Bottom
      else
        let all_indexes = Index.Set.union (indexes t1) (indexes t2) in
        let env, env_extension1, env_extension2, components_by_index =
          environment_for_meet_or_join env t1 t2 ~indexes ~all_indexes
        in
        let env_extension =
          TEE.meet (JE.central_environment env) env_extension1 env_extension2
        in
        Ok {
          components_by_index;
          env_extension;
        }

  let join env t1 t2 : _ Or_unknown.t =
    if t1 == t2 then Ok t1
    else
      let env = JE.create env in
      let indexes = Index.Set.union (indexes t1) (indexes t2) in
      let env, env_extension1, env_extension2, components_by_index =
        environment_for_meet_or_join env t1 t2 ~indexes ~all_indexes:indexes
      in
      let env_extension =
        TEE.join (JE.central_environment env) env_extension1 env_extension2
      in
      Ok {
        components_by_index;
        env_extension;
      }

  let add_or_meet_equations t env env_extension =
    { t with
      env_extension = TEE.meet env t.env_extension env_extension;
    }

  let standalone_extension t =
    EV.Map.fold (fun _index logical_var env_extension ->
        let stale_name = LV.name logical_var in
        let kind = LV.kind logical_var in
        TEE.add_definition_at_beginning env_extension stale_name
          (T.bottom kind))
      t.components_by_index
      t.env_extension

  let introduce t env =
    let scope_level = Typing_env.max_level env in
    TE.add_or_meet_env_extension env (standalone_extension t) scope_level

  let free_names_in_components_by_index t =
    LV.Set.fold (fun logical_var free_names ->
        Name_occurrences.union (LV.free_names logical_var) free_names)
      (EV.Map.data t.components_by_index)
      free_names

  let free_names { external_structure = _; components_by_index; env_extension; } =
    let free_names = TEE.free_names env_extension in
    let bound_names = free_names_in_components_by_index t in
    Name_occurrences.diff free_names bound_names

  (* CR mshinwell: Do we really need [bound_names]?  It seems like this
     might be problematic with the two notions of freshness (the usual one
     for terms; and for existentials, which are binding but not freshened
     until opened). *)
  let bound_names { external_structure; components_by_index; env_extension = _; } =
    let bound_names =
      Name_occurrences.create ()
      (* XXX Have removed until CR above is resolved.  The logical var
         names aren't fresh.
         free_names_in_components_by_index t *)
    in
    EV.Set.fold (fun index bound_names ->
        Name_occurrences.union (EV.free_names index) bound_names)
      (EVS.to_set external_structure)
      bound_names

  let apply_name_permutation
        { external_structure; components_by_index; env_extension; } perm =
    let components_by_index =
      EV.Map.fold (fun index logical_var components_by_index ->
          let index = EV.apply_name_permutation index perm in
          let logical_var = LV.apply_name_permutation logical_var perm in
          EV.Map.add index logical_var components_by_index)
        components_by_index
        EV.Map.empty
    in
    let env_extension = TEE.apply_name_permutation env_extension perm in
    { external_structure;
      components_by_index;
      env_extension;
    }

  let freshen t freshening =
    apply_name_permutation t (Freshening.name_permutation freshening)
end
