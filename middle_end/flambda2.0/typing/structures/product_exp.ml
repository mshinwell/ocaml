(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module K = Flambda_kind

module Make
  (Index : Name_like_intf.S)
  (Component : sig
    include Type_structure_intf.S
    val create : Flambda_kind.t -> t
    val name : t -> Name.t
    val kind : t -> Flambda_kind.t
  end) =
struct
  type t = {
    components_by_index : Component.t Index.Map.t;
  }

  let invariant _t =
    ()

  let print ppf { components_by_index; } =
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(components_by_index@ %a)@]\
        )@]"
      (Index.Map.print Component.print) components_by_index

  let print_with_cache ~cache:_ ppf t = print ppf t

  let equal env result
        { components_by_index = components_by_index1; }
        { components_by_index = components_by_index2; } =
    let equal =
      Index.Map.fold2_stop_on_key_mismatch
        (fun _index component1 component2 equal ->
          if not equal then false
          else Component.equal env component1 component2)
        components_by_index1 components_by_index2
        true
    in
    match equal with
    | None | Some false -> Type_equality_result.types_known_unequal ()
    | Some true -> result

  let indexes t = Index.Map.keys t.components_by_index

  (* CR mshinwell: The [kind] may not be needed in [Component] but it
     isn't clear yet. We can sort this out later. At present all
     products map to components of kind [Value]. *)
  (* XXX Isn't this wrong?  Float arrays... *)
  let kind = Flambda_kind.value ()

  let kind_of_index _t _index = kind (* XXX *)

  let widen t ~to_match =
    let missing_indexes =
      Index.Set.diff (indexes to_match) (indexes t)
    in
    let components_by_index =
      Index.Set.fold (fun index components_by_index ->
          assert (not (Index.Map.mem index components_by_index));
          let kind = kind_of_index to_match index in
          let component = Component.create kind in
          Index.Map.add index component components_by_index)
        missing_indexes
        t.components_by_index
    in
    { t with components_by_index; }

  let environment_for_meet_or_join env (t1 : t) (t2 : t)
        ~indexes =
    let components_by_index_in_result, env =
      Index.Set.fold (fun index (components_by_index_in_result, env) ->
          let component = Component.create kind in
          let components_by_index_in_result =
            Index.Map.add index component components_by_index_in_result
          in
          let env =
            Typing_env.add_definition env
              (Component.name component) (Flambda_type0_core.unknown kind)
          in
          components_by_index_in_result, env)
        indexes
        (Index.Map.empty, env)
    in
    let result_components =
      Index.Map.data components_by_index_in_result
    in
    env, components_by_index_in_result, result_components

  (* XXX need actual meet/join ops on components *)

  let meet env t1 t2 : _ Or_bottom.t =
    let indexes = Index.Set.inter (indexes t1) (indexes t2) in
    if Index.Set.is_empty indexes then Bottom
    else
      let env = Join_env.create env in
      let env, env_extension1, env_extension2, components_by_index,
          result_components =
        environment_for_meet_or_join env t1 t2 ~indexes
      in
      { components_by_index;
      }

  let join env t1 t2 =
    let indexes = Index.Set.union (indexes t1) (indexes t2) in
    let env, components_by_index, result_components =
      environment_for_meet_or_join env t1 t2 ~indexes
    in
    { components_by_index;
    }
end
