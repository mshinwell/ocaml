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

type t = Neither | Left | Right | Both

let join (t1 : t) (t2 : t) =
  match t1, t2 with
  | Neither, Neither -> Neither
  | Neither, Left -> Left
  | Neither, Right -> Right
  | Neither, Both -> Both
  | Left, Neither -> Left
  | Left, Left -> Left
  | Left, Right -> Both
  | Left, Both -> Both
  | Right, Neither -> Right
  | Right, Left -> Both
  | Right, Right -> Right
  | Right, Both -> Both
  | Both, Neither -> Both
  | Both, Left -> Both
  | Both, Right -> Both
  | Both, Both -> Both
