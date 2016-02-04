(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file ../LICENSE.       *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Project_var of Flambda.project_var
  | Project_closure of Flambda.project_closure
  | Move_within_set_of_closures of Flambda.move_within_set_of_closures
  | Field of int * Variable.t

let to_named t : Flambda.named =
  match t with
  | Project_var project_var -> Project_var project_var
  | Project_closure project_closure -> Project_closure project_closure
  | Move_within_set_of_closures move -> Move_within_set_of_closures move
  | Field (field_index, var) ->
    Prim (Pfield field_index, [var], Debuginfo.none)
