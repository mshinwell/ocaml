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

(* CR mshinwell: Delete >= 4.08 *)
[@@@ocaml.warning "-60"]
module Flambda_type0_core = struct end
module Meet_env = struct end
module Typing_env = struct end
module Typing_env_extension = struct end

module Make (W : Typing_world.S) = struct
  open! W

  type t = {
    env : Meet_env.t;
    env_plus_extension1 : Typing_env.t;
    env_plus_extension2 : Typing_env.t;
    extension1 : Typing_env_extension.t;
    extension2 : Typing_env_extension.t;
  }

  let create env =
    { env;
      env_plus_extension1 = env;
      env_plus_extension2 = env;
      extension1 = Typing_env_extension.empty;
      extension2 = Typing_env_extension.empty;
    }

  let invariant _t =
    ()

  let add_extensions t ~holds_on_left ~holds_on_right =
    let env_plus_extension1 =
      Typing_env.add_or_meet_env_extension t.env_plus_extension1
        holds_on_left (Typing_env.max_level (Meet_env.env t.env))
    in
    let extension1 =
      Typing_env_extension.meet (Meet_env.env t.env) t.extension1 holds_on_left
    in
    let env_plus_extension2 =
      Typing_env.add_or_meet_env_extension t.env_plus_extension2 holds_on_right
        (Typing_env.max_level (Meet_env.env t.env))
    in
    let extension2 =
      Typing_env_extension.meet (Meet_env.env t.env)
        t.extension2 holds_on_right
    in
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
      Typing_env.add_or_meet_env_extension (Meet_env.env t.env)
        central_extension
        (Typing_env.max_level (Meet_env.env t.env))
    in
    let t = { t with env; } in
    invariant t;
    add_extensions t ~holds_on_left ~holds_on_right

  let central_environment t = t.env

  let environment_on_left t = t.env_plus_extension1

  let environment_on_right t = t.env_plus_extension2

  let holds_on_left t = t.extension1

  let holds_on_right t = t.extension2

  let fast_check_extensions_same_both_sides t =
    Typing_env_extension.fast_equal t.extension1 t.extension2

  let shortcut_precondition t =
    fast_check_extensions_same_both_sides t
      && Meet_env.shortcut_precondition t.env

  module Flambda_type0_core = W.Flambda_type0_core
  module Meet_env = W.Meet_env
  module Typing_env = W.Typing_env
  module Typing_env_extension = W.Typing_env_extension
end
