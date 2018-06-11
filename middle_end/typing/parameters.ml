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
    with type env_extension := T.env_extension
    with type flambda_type := T.flambda_type
    with type t_in_context := T.t_in_context
    with type 'a ty = 'a T.ty
    with type 'a unknown_or_join = 'a T.unknown_or_join
end) (Typing_env_extension : sig
  include Typing_env_extension_intf.S
    with type env_extension := T.env_extension
    with type typing_environment := T.typing_environment
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

  let create params : t =
    { params;
      env_extension = empty_env_extension;
    }

  let print ppf { params; env_extension; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(params@ %a)@]@ \
        @[<hov 1>(env_extension@ %a)@])@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
        Kinded_parameter.print) params
      print_typing_env_extension env_extension

  let introduce t freshening env =
    let scope_level = Typing_env.max_level env in
    let env =
      List.fold_left (fun env param ->
          let name =
            Freshening.apply_name freshening (Kinded_parameter.name param)
          in
          let kind = Kinded_parameter.kind param in
          let ty = bottom kind in
          Typing_env.add env name scope_level (Definition ty))
        env
        t.params
    in
    Typing_env.add_or_meet_env_extension ?freshening env
      t.env_extension scope_level

  let env_extension t = t.env_extension

  let check_arities_match t1 t2 =
    let fail () =
      Misc.fatal_errorf "Cannot meet or join [Parameters.t] values with
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

  let meet env t1 t2 =
    let arity = check_arities_match t1 t2 in
    let env =
      List.fold_left (fun env (kinded_param1, kinded_param2) ->
          let name1 = Parameter.name (Kinded_parameter.param kinded_param1) in
          let name2 = Parameter.name (Kinded_parameter.param kinded_param2) in
          let name2_ty = T.alias_type_of name1 in
          JE.add env name1 name2_ty)
        env
        (List.combine t1.params t2.params)
    in


  let join env t1 t2 =
    let arity = check_arities_match t1 t2 in
    let fresh_names =
      List.map (fun _kind -> Name.var (Variable.create "join")) arity
    in
    let fresh_params =
      List.map (fun (fresh_name, kind) ->
          Kinded_parameter.create (Parameter.create fresh_name) kind)
        (List.combine fresh_names arity)
    in
    let env =
      List.fold_left (fun env (fresh_name, kind) ->
          let ty = T.bottom kind in
          JE.add_definition env fresh_name ty)
        env
        fresh_names_with_kinds
    in
    let add_equalities_to_extension t =
      List.fold_left (fun env_extension (our_param, fresh_param) ->
          assert (Kinded_parameter.equal_kinds our_param fresh_param);
          let our_name = Kinded_parameter.name our_param in
          let fresh_name = Kinded_parameter.name fresh_param in
          let fresh_name_ty = T.alias_type_of our_name in
          TEE.add_equation env_extension fresh_name fresh_name_ty)
        t.env_extension
        (List.combine t.params fresh_names_with_kinds)
    in
    let env_extension1 = add_equalities_to_extension t1 in
    let env_extension2 = add_equalities_to_extension t2 in
    let env_extension = TEE.join env_extension1 env_extension2 in
    { params = fresh_params;
      env_extension;
    }
end
