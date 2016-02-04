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

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    match t1, t2 with
    | Project_var project_var1, Project_var project_var2 ->
      Flambda.compare_project_var project_var1 project_var2
    | Project_closure project_closure1, Project_closure project_closure2 ->
      Flambda.compare_project_closure project_closure1 project_closure2
    | Move_within_set_of_closures move1, Move_within_set_of_closures move2 ->
      Flambda.compare_move_within_set_of_closures move1 move2
    | Field (index1, var1), Field (index2, var2) ->
      let c = compare index1 index2 in
      if c <> 0 then c
      else Variable.compare var1 var2
    | Project_var _, _ -> -1
    | _, Project_var _ -> 1
    | Project_closure _, -1
    | _, Project_closure _ -> 1
    | Move_within_set_of_closures _, _ -> -1
    | _, Move_within_set_of_closures _ -> -1

  let equal t1 t2 =
    (compare t1 t2) = 0

  let hash = Hashtbl.hash

  let print _ _ = failwith "Projection.print: not yet implemented"

  let output _ _ = failwith "Projection.output: not yet implemented"
end)

let to_named t : Flambda.named =
  match t with
  | Project_var project_var -> Project_var project_var
  | Project_closure project_closure -> Project_closure project_closure
  | Move_within_set_of_closures move -> Move_within_set_of_closures move
  | Field (field_index, var) ->
    Prim (Pfield field_index, [var], Debuginfo.none)
