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

module LV = Logical_variable

module Make
  (T : Flambda_type0_internal_intf.S)
  (Typing_env : Typing_env_intf.S with module T := T)
  (Typing_env_extension : Typing_env_extension_intf.S with module T := T)
  (Meet_and_join : Meet_and_join_intf.S_both with module T := T)
  (Join_env : Join_env_intf.S with module T := T)
  (External_var : sig
    type t
    include Map.With_set with type t := t

    val kind : t -> Flambda_kind.t

    val apply_name_permutation_map
       : 'a Map.t
      -> f:('a -> 'a)
      -> Name_permutation.t
      -> 'a Map.t
  end)
  (Make_structure : functor
    Set.OrderedType
    ->
    sig
      type t

      val print : Format.formatter -> t -> unit
      val fold : ('a -> External_type.t -> 'a) -> 'a -> t -> 'a

      val meet : t -> t -> t
      val join : t -> t -> t

      val to_set : t -> External_type.Set.t
    end)
= struct
  open T

  module JE = Join_env
  module TE = Typing_env
  module TEE = Typing_env_extension

  module EV = External_var
  module EVS = Make_structure (EV)

  type t = {
    external_structure : EVS.t;
    logical_vars : LV.t EV.Map.t;
    env_extension : TEE.t;
  }

  let invariant _t =
    (* CR mshinwell: This should check that the [env_extension] never contains
       [Definition]s for the [logical_vars]. *)
    ()

  let create logical_vars : t =
    let t =
      { logical_vars;
        env_extension = TEE.empty;
      }
    in
    invariant t;
    t

  let create_with_env_extension logical_vars env_extension : t =
    let t =
      { logical_vars;
        env_extension;
      }
    in
    invariant t;
    t

  let print ppf { logical_vars; env_extension; } =


    T.print_parameters ~cache:(Printing_cache.create ()) ppf t

  let external_structure t = t.external_structure

  let add_or_meet_equations t env env_extension =
    { t with
      env_extension = TEE.meet env t.env_extension env_extension;
    }

  type meet_or_join =
    | Meet
    | Join

  let environment_for_meet_or_join (op : meet_or_join) (t1 : t) (t2 : t) =
    let external_structure =
      match op with
      | Meet -> EVS.meet t1.external_structure t2.external_structure
      | Join -> EVS.join t1.external_structure t2.external_structure
    in
    let external_vars_in_result = EVS.to_set external_structure in
    let logical_vars_in_result, env =
      EV.Set.fold (fun external_var (logical_vars_in_result, env) ->
          let kind = EV.kind external_var in
          let fresh_logical_var = LV.create kind in
          let logical_vars_in_result =
            EV.Map.add external_var fresh_logical_var logical_vars_in_result
          in
          let env =
            JE.add_definition_central_environment env fresh_name (T.bottom kind)
          in
          logical_vars_in_result, env)
        external_vars_in_result
        (EV.Map.empty, env)
    in
    let add_definitions_and_equalities_to_extension (t : t) =
      EV.Map.fold (fun (external_var, fresh_logical_var) env_extension ->
          let stale_name = LV.name stale_logical_var in
          let kind = LV.kind stale_logical_var in
          let env_extension =
            TEE.add_definition_at_beginning env_extension stale_name
              (T.bottom kind)
          in
          match LV.find external_var t.logical_vars with
          | exception Not_found -> env_extension
          | stale_logical_var ->
            let fresh_name = LV.name fresh_logical_var in
            let fresh_name_ty =
              T.alias_type_of kind (Simple.name stale_name)
            in
            TEE.add_equation env_extension fresh_name fresh_name_ty)
        with_fresh_logical_vars
        t.env_extension
    in
    let env_extension1 = add_definitions_and_equalities_to_extension t1 in
    let env_extension2 = add_definitions_and_equalities_to_extension t2 in
    env, env_extension1, env_extension2, external_structure,
      logical_vars_in_result

  let meet_or_join env t1 t2 ~op =
    if t1 == t2 then t1
    else
      let env, env_extension1, env_extension2, logical_vars,
          external_structure =
        environment_for_meet_or_join env t1 t2
      in
      let env_extension = op env env_extension1 env_extension2 in
      { external_structure;
        logical_vars;
        env_extension;
      }

  let meet env t1 t2 =
    let env = JE.create env in
    meet_or_join env t1 t2
      ~op:(fun join_env env_extension1 env_extension2 ->
        TEE.meet (JE.central_environment join_env)
          env_extension1 env_extension2)

  let join env t1 t2 =
    meet_or_join env t1 t2 ~op:TEE.join

  let standalone_extension t =
    EV.Map.fold (fun _external_var logical_var env_extension ->
        let stale_name = LV.name logical_var in
        let kind = LV.kind logical_var in
        TEE.add_definition_at_beginning env_extension stale_name
          (T.bottom kind))
      t.logical_vars
      t.env_extension

  let introduce (t : t) env =
    let scope_level = Typing_env.max_level env in
    TE.add_or_meet_env_extension env (standalone_extension t) scope_level

  let apply_name_permutation
        { external_structure; logical_vars; env_extension; } perm =
    let external_structure =
      EVS.apply_name_permutation external_structure perm
    in
    let logical_vars =
      EV.apply_name_permutation_map logical_vars
        ~f:(fun logical_var -> LV.apply_name_permutation logical_var perm)
    in
    let env_extension = TEE.apply_name_permutation env_extension perm in
    { external_structure;
      logical_vars;
      env_extension;
    }
end
