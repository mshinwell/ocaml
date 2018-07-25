
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
  val name : t -> Name.t
end

module Make
  (Index : Name_like)
  (Component : sig
    include Name_like

    val create : unit -> t
  end)
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
       [Definition]s for [Name]s occurring in the indexes. *)
    ()

  let create_with_env_extension indexes env_extension : t =
    let components_by_index =
      Index.Set.fold (fun index components_by_index ->
          let kind = Index.kind index in
          let component = Component.create kind in
          let components_by_index =
            Index.Map.add index component components_by_index_in_result
          in
          components_by_index)
        indexes
        Index.Map.empty
    in
    let t =
      { components_by_index;
        env_extension;
      }
    in
    invariant t;
    t

  let create indexes =
    create_with_env_extension indexes TEE.empty

  let print ppf { components_by_index; env_extension; } =
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(components_by_index@ %a)@]@ \
        @[<hov 1>(env_extension@ %a)@])@]"
      (Index.Map.print Component.print) components_by_index
      TEE.print env_extension

  let indexes t = Index.Map.keys t.components_by_index

  type fresh_component_semantics =
    | Fresh
    | Left
    | Right

  type meet_or_join =
    | Meet
    | Join

  let environment_for_meet_or_join env (t1 : t) (t2 : t)
        ~fresh_component_semantics ~indexes =
    let components_by_index_in_result, env =
      Index.Set.fold (fun index (components_by_index_in_result, env) ->
          let kind = Index.kind index in
          let component =
            match fresh_component_semantics with
            | Fresh -> Component.create kind
            | Left ->
              begin match Index.Map.find index t1.components_by_index with
              | exception Not_found -> Component.create kind
              | component_on_left -> component_on_left
              end
            | Right ->
              begin match Index.Map.find index t2.components_by_index with
              | exception Not_found -> Component.create kind
              | component_on_right -> component_on_right
              end
          in
          let components_by_index_in_result =
            Index.Map.add index component components_by_index_in_result
          in
          let env =
            JE.add_definition_central_environment env
              (Component.name component) (T.bottom kind)
          in
          components_by_index_in_result, env)
        indexes
        (Index.Map.empty, env)
    in
    let result_components = Index.Map.data components_by_index_in_result in
    let add_definitions_to_extension t =
      Index.Map.fold (fun _index component env_extension ->
          if Component.Set.mem component result_components then
            env_extension
          else
            let name = Component.name component in
            let kind = Component.kind component in
            TEE.add_definition_at_beginning env_extension name (T.bottom kind))
        t.components_by_index
        t.env_extension
    in
    let env_extension1 = add_definitions_to_extension t1 in
    let env_extension2 = add_definitions_to_extension t2 in
    let add_equalities_to_extension (t : t) =
      Index.Map.fold (fun (index, component) env_extension ->
          let name = Component.name component in
          let kind = Component.kind component in
          match Index.find index t.components_by_index with
          | exception Not_found -> env_extension
          | stale_component ->
            if Component.equal component stale_component then env_extension
            else
              let stale_name = Component.name stale_component in
              let name_ty = T.alias_type_of kind (Simple.name stale_name) in
              TEE.add_equation env_extension name name_ty)
        components_by_index_in_result
        t.env_extension
    in
    let env_extension1 = add_equalities_to_extension t1 in
    let env_extension2 = add_equalities_to_extension t2 in
    env, env_extension1, env_extension2, components_by_index_in_result

  let meet env t1 t2 ~fresh_component_semantics : _ Or_bottom.t =
    if t1 == t2 then Ok t1
    else
      let env = JE.create env in
      let indexes = Index.Set.inter (indexes t1) (indexes t2) in
      if Index.Set.is_empty indexes then Bottom
      else
        let env, env_extension1, env_extension2, components_by_index =
          environment_for_meet_or_join env t1 t2 ~fresh_component_semantics
            ~indexes
        in
        let env_extension =
          TEE.meet (JE.central_environment env) env_extension1 env_extension2
        in
        Ok {
          components_by_index;
          env_extension;
        }

  let join env t1 t2 ~fresh_component_semantics : _ Or_unknown.t =
    if t1 == t2 then Ok t1
    else
      let env = JE.create env in
      let indexes = Index.Set.union (indexes t1) (indexes t2) in
      let env, env_extension1, env_extension2, components_by_index =
        environment_for_meet_or_join env t1 t2 ~fresh_component_semantics
          ~indexes
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
    Index.Map.fold (fun _index component env_extension ->
        let name = Component.name component in
        let kind = Component.kind component in
        TEE.add_definition_at_beginning env_extension name (T.bottom kind))
      t.components_by_index
      t.env_extension

  let introduce t env =
    let scope_level = Typing_env.max_level env in
    TE.add_or_meet_env_extension env (standalone_extension t) scope_level

  let free_names_in_indexes t =
    Index.Set.fold (fun index free_names ->
        Name_occurrences.union (Index.free_names index) free_names)
      (Index.Map.keys t.components_by_index)
      (Name_occurrences.create ())

  let free_names_in_components t =
    Component.Set.fold (fun component free_names ->
        Name_occurrences.union (Component.free_names component) free_names)
      (Index.Map.data t.components_by_index)
      (Name_occurrences.create ())

  (* CR mshinwell: Do we really need [bound_names]?  It seems like this
     might be problematic with the two notions of freshness (the usual one
     for terms; and for existentials, which are binding but not freshened
     until opened). *)
  let bound_names t = free_names_in_components t

  let free_names { components_by_index = _; env_extension; } =
    Name_occurrences.union (free_names_in_indexes t)
      (Name_occurrences.diff (TEE.free_names env_extension) (bound_names t))

  let apply_name_permutation { components_by_index; env_extension; } perm =
    let components_by_index =
      Index.Map.fold (fun index component components_by_index ->
          let index = Index.apply_name_permutation index perm in
          let component = Component.apply_name_permutation component perm in
          Index.Map.add index component components_by_index)
        components_by_index
        Index.Map.empty
    in
    let env_extension = TEE.apply_name_permutation env_extension perm in
    { components_by_index;
      env_extension;
    }

  let freshen t freshening =
    apply_name_permutation t (Freshening.name_permutation freshening)
end
