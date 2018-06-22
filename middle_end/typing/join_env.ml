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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make
  (T : Flambda_type0_internal_intf.S)
  (Typing_env : Typing_env_intf.S with module T := T)
  (Typing_env_extension : Typing_env_extension_intf.S with module T := T) =
struct
  open T

  module TE = Typing_env
  module TEE = Typing_env_extension

  type t = {
    env : TE.t;
    env_plus_extension1 : TE.t;
    env_plus_extension2 : TE.t;
    extension1 : TEE.t;
    extension2 : TEE.t;
  }

  let create env =
    { env;
      env_plus_extension1 = env;
      env_plus_extension2 = env;
      extension1 = TEE.empty;
      extension2 = TEE.empty;
    }

  let invariant _t =
    ()

  let add_extensions t ~holds_on_left ~holds_on_right =
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
    t

  let add_extensions_and_extend_central_environment t
        ~holds_on_left ~holds_on_right ~central_extension =
    let env =
      TE.add_or_meet_env_extension t.env central_extension (T.max_level t.env)
    in
    let t = { t with env; } in
    invariant t;
    add_extensions t ~holds_on_left ~holds_on_right

  let environment t = t.env

  let environment_on_left t = t.env_plus_extension1

  let environment_on_right t = t.env_plus_extension2

  let holds_on_left t = t.extension1

  let holds_on_right t = t.extension2

  let fast_check_extensions_same_both_sides t =
    TEE.fast_equal t.extension1 t.extension2
end
