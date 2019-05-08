(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = {
  env : Meet_env.t;
  env_plus_extension1 : Typing_env.t;
  env_plus_extension2 : Typing_env.t;
  extension1 : Typing_env_extension.t;
  extension2 : Typing_env_extension.t;
}

let create env =
  { env;
    env_plus_extension1 = Meet_env.env env;
    env_plus_extension2 = Meet_env.env env;
    extension1 = Typing_env_extension.empty ();
    extension2 = Typing_env_extension.empty ();
  }

let invariant _t =
  ()

let print ppf
      { env; env_plus_extension1; env_plus_extension2;
        extension1; extension2; } =
  Format.fprintf ppf
    "@[<v 1>(\
        @[<hov 1>(env@ @[<v 1>%a@])@]@;\
        @[<hov 1>(env_plus_extension1@ @[<v 1>%a@])@]@;\
        @[<hov 1>(env_plus_extension2@ @[<v 1>%a@])@]@;\
        @[<hov 1>(extension1@ @[<v 1>%a@])@]@;\
        @[<hov 1>(extension2@ @[<v 1>%a@])@])@]"
      Meet_env.print env
      Typing_env.print env_plus_extension1
      Typing_env.print env_plus_extension2
      Typing_env_extension.print extension1
      Typing_env_extension.print extension2

let add_extensions t ~holds_on_left ~holds_on_right =
  let env_plus_extension1 =
    Typing_env.add_env_extension t.env_plus_extension1
      holds_on_left
  in
  let extension1 =
    Typing_env_extension.meet t.env t.extension1 holds_on_left
  in
  let env_plus_extension2 =
    Typing_env.add_env_extension t.env_plus_extension2
      holds_on_right
  in
  let extension2 =
    Typing_env_extension.meet t.env t.extension2 holds_on_right
  in
  let t = {
    env = t.env;
    env_plus_extension1;
    env_plus_extension2;
    extension1;
    extension2;
  }
  in
  invariant t;
  t

let add_opened_extensions t ~holds_on_left ~holds_on_right =
  let env_plus_extension1 =
    Typing_env.add_opened_env_extension t.env_plus_extension1
      holds_on_left
  in
  let extension1 =
    Typing_env_extension.meet t.env t.extension1
      (Typing_env_extension.create holds_on_left)
  in
  let env_plus_extension2 =
    Typing_env.add_opened_env_extension t.env_plus_extension2
      holds_on_right
  in
  let extension2 =
    Typing_env_extension.meet t.env t.extension2
      (Typing_env_extension.create holds_on_right)
  in
  let t = {
    env = t.env;
    env_plus_extension1;
    env_plus_extension2;
    extension1;
    extension2;
  }
  in
  invariant t;
  t

(* CR mshinwell: Misleading name? *)
let add_definition_central_environment t name ty =
  let kind = Flambda_type0_core.kind ty in
  let add env =
    let env = Typing_env.add_definition env name kind in
    Typing_env.add_equation env name ty
  in
  let env =
    Meet_env.with_env t.env (fun env -> add env)
  in
  let env_plus_extension1 = add t.env_plus_extension1 in
  let env_plus_extension2 = add t.env_plus_extension2 in
  let t =
    { t with
      env;
      env_plus_extension1;
      env_plus_extension2;
    }
  in
  invariant t;
  t

let _add_extensions_and_extend_central_environment t
      ~holds_on_left ~holds_on_right ~central_extension =
  let env =
    Meet_env.with_env t.env (fun env ->
      Typing_env.add_env_extension env central_extension)
  in
  let t = { t with env; } in
  invariant t;
  add_extensions t ~holds_on_left ~holds_on_right

let central_environment t = t.env

let central_typing_environment t = Meet_env.env t.env

let environment_on_left t = t.env_plus_extension1

let environment_on_right t = t.env_plus_extension2

let holds_on_left t = t.extension1

let holds_on_right t = t.extension2

let fast_check_extensions_same_both_sides t =
  Typing_env_extension.fast_equal t.extension1 t.extension2

let shortcut_precondition t =
  fast_check_extensions_same_both_sides t
    && Meet_env.shortcut_precondition t.env

let perm_left t = Meet_env.perm_left t.env
let perm_right t = Meet_env.perm_right t.env

let clear_name_permutations t =
  { t with
    env = Meet_env.clear_name_permutations t.env;
  }

let compose_name_permutations t ~perm_left ~perm_right =
  { t with
    env = Meet_env.compose_name_permutations t.env ~perm_left ~perm_right;
  }
