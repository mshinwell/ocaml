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
end) = struct
  open T

  module TE = Typing_env
  module TEE = Typing_env_extension

  type typing_environment = TE.t
  type env_extension = TEE.t
  type join_env = T.join_env

  type t = join_env

  let create env =
    { env;
      env_plus_extension1 = env;
      env_plus_extension2 = env;
      extension1 = empty_env_extension;
      extension2 = empty_env_extension;
    }

  let invariant _t =
    ()

  let add_extensions t ~meet_or_join_env_extension ~holds_on_left
        ~holds_on_right =
    let holds_in_meet_or_join =
      meet_or_join_env_extension t holds_on_left holds_on_right
    in
    let env =
      TE.add_or_meet_env_extension t.env holds_in_meet_or_join
        (TE.max_level t.env)
    in
    let env_plus_extension1 =
      TE.add_or_meet_env_extension t.env_plus_extension1 holds_on_left
        (TE.max_level t.env)
    in
    let extension1 = TEE.meet t.env t.extension1 holds_on_left in
    let env_plus_extension2 =
      TE.add_or_meet_env_extension t.env_plus_extension2 holds_on_right
        (TE.max_level t.env)
    in
    let extension2 = TEE.meet t.env t.extension2 holds_on_right in
    let t = {
      env;
      env_plus_extension1;
      env_plus_extension2;
      extension1;
      extension2;
    }
    in
    invariant t;
    t, holds_in_meet_or_join

  let add_extensions_and_return_meet t ~holds_on_left ~holds_on_right =
    add_extensions t
      ~meet_or_join_env_extension:Typing_env_extension.meet
      ~holds_on_left ~holds_on_right

  let add_extensions_and_return_meet t ~holds_on_left ~holds_on_right =
    add_extensions t
      ~meet_or_join_env_extension:Typing_env_extension.join
      ~holds_on_left ~holds_on_right

  let environment t = t.env

  let environment_on_left t = t.env_plus_extension1

  let environment_on_right t = t.env_plus_extension2

  let holds_on_left t = t.extension1

  let holds_on_right t = t.extension2

  let fast_check_extensions_same_both_sides t =
    TEE.fast_equal t.extension1 t.extension2
end
