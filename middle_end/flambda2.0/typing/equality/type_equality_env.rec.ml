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
  typing_env_left : Typing_env.t;
  typing_env_right : Typing_env.t;
}

let _print ppf { typing_env_left; typing_env_right; } =
  Format.fprintf ppf
    "@[<hov 1>(\
      @[<hov 1>(typing_env_left@ %a)@]@ \
      @[<hov 1>(typing_env_right@ %a)@]\
      @]"
    Typing_env.print typing_env_left
    Typing_env.print typing_env_right

let create ~typing_env_left ~typing_env_right =
  { typing_env_left;
    typing_env_right;
  }

let typing_env_left t = t.typing_env_left
let typing_env_right t = t.typing_env_right
