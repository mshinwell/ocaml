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
end) (Typing_env : sig
  include Typing_env_intf.S
    with type typing_environment := T.typing_environment
    with type typing_environment_entry := T.typing_environment_entry
    with type env_extension := T.env_extension
    with type flambda_type := T.flambda_type
    with type t_in_context := T.t_in_context
    with type 'a ty = 'a T.ty
    with type 'a unknown_or_join = 'a T.unknown_or_join
end) (Typing_env_extension : sig
  include Typing_env_extension_intf.S
    with type env_extension := T.env_extension
    with type typing_environment := T.typing_environment
    with type join_env := T.join_env
    with type flambda_type := T.flambda_type
end) (Meet_and_join : sig
  include Meet_and_join_intf.S_for_types
    with type typing_environment := T.typing_environment
    with type env_extension := T.env_extension
    with type join_env := T.join_env
    with type flambda_type := T.flambda_type
end) (Join_env : sig
  include Join_env_intf.S
    with type env_extension := T.env_extension
    with type typing_environment := T.typing_environment
    with type join_env := T.join_env
    with type flambda_type := T.flambda_type
end) = struct
  open T

  module JE = Join_env
  module TE = Typing_env
  module TEE = Typing_env_extension

  type t = parameters

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

  let print ppf { params; env_extension; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(params@ %a)@]@ \
        @[<hov 1>(env_extension@ %a)@])@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
        Kinded_parameter.print) params
      TEE.print env_extension

  let introduce (t : t) env =
    let scope_level = Typing_env.max_level env in
    let env =
      List.fold_left (fun env param ->
          let name = Kinded_parameter.name param in
          let kind = Kinded_parameter.kind param in
          let ty = T.bottom kind in
          TE.add env name scope_level (Definition ty))
        env
        t.params
    in
    TE.add_or_meet_env_extension env t.env_extension scope_level

  let arity (t : t) =
    List.map (fun kinded_param -> Kinded_parameter.kind kinded_param) t.params

  let check_arities_match (t1 : t) (t2 : t) =
    let fail () =
      Misc.fatal_errorf "Cannot meet or join [Parameters.t] values with \
          different arities:@ %a@ and@ %a"
        print t1
        print t2
    in
    if List.compare_lengths t1.params t2.params <> 0 then begin
      fail ()
    end;
    List.map (fun (kinded_param1, kinded_param2) ->
        let kind1 = Kinded_parameter.kind kinded_param1 in
        let kind2 = Kinded_parameter.kind kinded_param2 in
        if not (Flambda_kind.equal kind1 kind2) then begin
          fail ()
        end;
        kind1)
      (List.combine t1.params t2.params)

  type fresh_name_semantics =
    | Fresh
    | Left
    | Right

  let environment_for_meet_or_join ?(fresh_name_semantics = Fresh) env
        (t1 : t) (t2 : t) =
    let arity = check_arities_match t1 t2 in
    let fresh_params =
      match fresh_name_semantics with
      | Fresh ->
        List.map (fun kind ->
            let fresh_name = Name.var (Variable.create "join") in
            Kinded_parameter.create (Parameter.create fresh_name) kind)
          arity
      | Left -> t1.params
      | Right -> t2.params
    in
    let env =
      List.fold_left (fun env fresh_param ->
          let fresh_name = Kinded_parameter.name fresh_param in
          let kind = Kinded_parameter.kind fresh_param in
          JE.add_definition_central_environment env fresh_name (T.bottom kind))
        env
        fresh_params
    in
    let add_definitions_and_equalities_to_extension (t : t) =
      List.fold_left (fun env_extension (our_param, fresh_param) ->
          assert (Kinded_parameter.equal_kinds our_param fresh_param);
          let our_name = Kinded_parameter.name our_param in
          let our_param_kind = Kinded_parameter.kind our_param in
          let env_extension =
            TEE.add_definition_at_beginning env_extension our_name
              (T.bottom our_param_kind)
          in
          let fresh_name = Kinded_parameter.name fresh_param in
          let fresh_name_ty = T.alias_type_of our_param_kind our_name in
          TEE.add_equation env_extension fresh_name fresh_name_ty)
        t.env_extension
        (List.combine t.params fresh_params)
    in
    let env_extension1 =
      match fresh_name_semantics with
      | Fresh | Right -> add_definitions_and_equalities_to_extension t1
      | Left -> t1.env_extension
    in
    let env_extension2 =
      match fresh_name_semantics with
      | Fresh | Left -> add_definitions_and_equalities_to_extension t2
      | Right -> t2.env_extension
    in
    env, env_extension1, env_extension2, fresh_params

  let meet_or_join ?fresh_name_semantics env t1 t2 ~op =
    if t1 == t2 then t1
    else
      let env, env_extension1, env_extension2, params =
        environment_for_meet_or_join ?fresh_name_semantics env t1 t2
      in
      let env_extension = op env env_extension1 env_extension2 in
      { params;
        env_extension;
      }

  let meet ?fresh_name_semantics env t1 t2 =
    meet_or_join ?fresh_name_semantics env t1 t2 ~op:TEE.meet

  let join ?fresh_name_semantics env t1 t2 =
    meet_or_join ?fresh_name_semantics env t1 t2 ~op:TEE.join
end
