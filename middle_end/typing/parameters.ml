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
    with type flambda_type := T.flambda_type
end) (Join_env : sig
  include Join_env_intf.S
    with type ...
end) = struct
  open T

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

  let join join_env t1 t2 =
    match use_the_same_fresh_names t1 t2 with
    | None -> None
    | Some (kinded_params, t1, t2) ->
      let add_parameter_definitions join_env kinded_params ~add =
        List.fold_left (fun join_env kinded_param ->
            let name = Kinded_parameter.name kinded_param in
            let kind = Kinded_parameter.kind kinded_param in
            let ty = T.bottom kind in
            add join_env name ty)
          join_env
          kinded_params
      in
      let join_env =
        add_parameter_definitions join_env
          kinded_params
          ~add:Join_env.add_definition_all_environments
      in
      let join_env =
        add_parameter_definitions join_env
          (kinded_params t1)
          ~add:Join_env.add_definition_left_environment
      in
      let join_env =
        add_parameter_definitions join_env
          (kinded_params t2)
          ~add:Join_env.add_definition_right_environment
      in
      let joined_params_env_extension =
        Typing_env_extension.join join_env
          (env_extension t1)
          (env_extension t2)
      in
      let t =
        create_with_env_extension kinded_params
          joined_params_env_extension
      in
      let join_env =
        Join_env.add_extensions join_env
          ~holds_in_join:(env_extension params)
          ~holds_on_left:(env_extension t1)
          ~holds_on_right:(env_extension t2)
      in
      t, join_env
end
