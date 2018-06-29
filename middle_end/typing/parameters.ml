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

module KP = Kinded_parameter

module Make0
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

  (* CR mshinwell: Rename [KP] to mention "logical variable" *)
  type t = {
    params : KP.Set.t;
    env_extension : TEE.t;
  }

  let invariant _t =
    (* CR mshinwell: This should check that the [env_extension] never contains
       [Definition]s for the [params]. *)
    ()

  let create params : t =
    { params;
      env_extension = TEE.empty;
    }

  let create_with_env_extension params env_extension : t =
    let t =
      { params;
        env_extension;
      }
    in
    invariant t;
    t

  let create_same_params_no_extension (t : t) : t =
    { params = t.params;
      env_extension = TEE.empty;
    }

  let create_from_types = T.create_parameters_from_types

  let print ppf t =
    T.print_parameters ~cache:(Printing_cache.create ()) ppf t

  let introduce (t : t) env =
    let scope_level = Typing_env.max_level env in
    let env =
      KP.Set.fold (fun param env ->
          let name = Index.name param in
          let kind = Index.kind param in
          let ty = T.bottom kind in
          TE.add env name scope_level (Definition ty))
        t.params
        env
    in
    TE.add_or_meet_env_extension env t.env_extension scope_level

  let kinded_params t = t.params

  let add_or_meet_equations t env env_extension =
    { t with
      env_extension = TEE.meet env t.env_extension env_extension;
    }

  type meet_or_join =
    | Meet
    | Join

  let environment_for_meet_or_join ?(fresh_name_semantics = Fresh)
        (op : meet_or_join) (t1 : t) (t2 : t) =
    let all_params = KP.Set.union t1.params t2.params in
    let params_to_bind =
      match op with
      | Meet -> KP.Set.inter t1.params t2.params
      | Join -> all_params
    in
    (* XXX This isn't right.  There may be bound names shared between
       t1 and t2.  There needs to be one map per side *)
    let with_fresh_params =
      KP.Set.fold (fun param with_fresh_params ->
          let fresh_param = KP.rename param in
          KP.Map.add param fresh_param with_fresh_params)
        t.params
        KP.Map.empty
    in
    let env =
      KP.Map.fold (fun (_our_param, fresh_param) env ->
          let fresh_name = KP.name fresh_param in
          let kind = KP.kind fresh_param in
          JE.add_definition_central_environment env fresh_name (T.bottom kind))
        with_fresh_params
        env
    in
    let add_definitions_and_equalities_to_extension (t : t) =
      KP.Map.fold (fun env_extension (our_param, fresh_param) ->
          assert (Index.equal_kinds our_param fresh_param);
          let our_name = Index.name our_param in
          let our_param_kind = Index.kind our_param in
          let env_extension =
            TEE.add_definition_at_beginning env_extension our_name
              (T.bottom our_param_kind)
          in
          let fresh_name = Index.name fresh_param in
          let fresh_name_ty =
            T.alias_type_of our_param_kind (Simple.name our_name)
          in
          TEE.add_equation env_extension fresh_name fresh_name_ty)
        with_fresh_params
        t.env_extension
    in
    let env_extension1 = add_definitions_and_equalities_to_extension t1 in
    let env_extension2 = add_definitions_and_equalities_to_extension t2 in
    let fresh_params_to_bind =
      KP.Map.filter (fun our_param _fresh_param ->
          KP.Set.mem params_to_bind our_param)
        with_fresh_params
    in
    env, env_extension1, env_extension2, fresh_params_to_bind

  let meet_or_join env t1 t2 ~op =
    if t1 == t2 then t1
    else
      let env, env_extension1, env_extension2, params =
        environment_for_meet_or_join env t1 t2
      in
      let env_extension = op env env_extension1 env_extension2 in
      { params;
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
    SC.fold t.params
      ~init:t.env_extension
      ~f:(fun env_extension param ->
        let name = Index.name param in
        let param_kind = Index.kind param in
        TEE.add_definition_at_beginning env_extension name
          (T.bottom param_kind))

  let apply_name_permutation { params; env_extension; } perm =
    let params =
      KP.Set.fold (fun param params ->
          let param = Name_permutation.apply_kinded_parameter param perm in
          KP.Set.add param params)
        t.params
        KP.Set.empty
    in
    { params;
      env_extension = TEE.apply_name_permutation env_extension perm;
    }
end

(* So the name policy for the non existentially bound ones should maybe
be external to here *)

module Make
  (Index : sig
    type t
    include Hashtbl.With_map with type t := t
    val apply_name_permutation : t -> Name_permutation.t -> t
  end)
  (T : Flambda_type0_internal_intf.S)
  (Typing_env : Typing_env_intf.S with module T := T)
  (Typing_env_extension : Typing_env_extension_intf.S with module T := T)
  (Meet_and_join : Meet_and_join_intf.S_both with module T := T)
  (Join_env : Join_env_intf.S with module T := T) =
struct
  module Params = Make0 (T) (Typing_env) (Typing_env_extension)
    (Meet_and_join) (Join_env)

  type t = {
    params : Params.t;
    indexes_to_params : KP.t Index.Map.t;
  }

  let meet env t1 t2 =
    let params, perm1, perm2 =
      Params.meet env t1.params t2.params
    in
    let permute_indexes_to_params indexes_to_params perm =
      Index.Map.fold (fun index param indexes_to_params ->
          let param = Name_permutation.apply_kinded_parameter param perm in
          Index.Map.add index param indexes_to_params)
        indexes_to_params
        Index.Map.empty
    in
    let indexes_to_params1 =
      permute_indexes_to_params t.indexes_to_params perm1
    in
    let indexes_to_params2 =
      permute_indexes_to_params t.indexes_to_params perm2
    in
    let 

  let apply_name_permutation t perm =
    let indexes_to_params =
      Index.Map.fold (fun index param indexes_to_params ->
(* not correct *)
          let index = Index.apply_name_permutation index perm in
          let param = Name_permutation.apply_kinded_parameter param perm in
          Index.Map.add index param indexes_to_params)
        t.indexes_to_params
        Index.Map.empty
    in
    { params = Params.apply_name_permutation t.params perm;
      indexes_to_params;
    }
end
