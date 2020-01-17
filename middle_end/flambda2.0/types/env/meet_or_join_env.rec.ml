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

[@@@ocaml.warning "+a-30-40-41-42"]

type t = {
  central_env : Meet_env.t;
  left_join_env : Typing_env.t option;
  right_join_env : Typing_env.t option;
}

let create_for_meet meet_env =
  { central_env = meet_env;
    left_join_env = None;
    right_join_env = None;
  }

let create_for_join central_env ~left_env ~right_env =
  { central_env = Meet_env.create central_env;
    left_join_env = Some left_env;
    right_join_env = Some right_env;
  }

let meet_env t = t.central_env

let target_join_env t = Meet_env.env t.central_env

let left_join_env t =
  match t.left_join_env with
  | Some env -> env
  | None -> target_join_env t

let right_join_env t =
  match t.right_join_env with
  | Some env -> env
  | None -> target_join_env t

let flip_join_envs t =
  { central_env = t.central_env;
    left_join_env = t.right_join_env;
    right_join_env = t.left_join_env;
  }

(* CR mshinwell: fix naming, it's odd at the moment to be using
   [already_meeting]... *)
let now_joining t simple1 simple2 =
  let central_env = Meet_env.now_meeting t.central_env simple1 simple2 in
  { t with
    central_env;
  }

let already_joining t simple1 simple2 =
  Meet_env.already_meeting t.central_env simple1 simple2
