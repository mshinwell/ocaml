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

type project_closure = {
  set_of_closures : Variable.t;
  closure_id : Closure_id.t;
}

type move_within_set_of_closures = {
  closure : Variable.t;
  start_from : Closure_id.t;
  move_to : Closure_id.t;
}

type project_var = {
  closure : Variable.t;
  closure_id : Closure_id.t;
  var : Var_within_closure.t;
}

let print_project_closure ppf (project_closure : project_closure) =
  fprintf ppf "@[<2>(project_closure@ %a@ from@ %a)@]"
    Closure_id.print project_closure.closure_id
    Variable.print project_closure.set_of_closures

let print_move_within_set_of_closures ppf
      (move_within_set_of_closures : move_within_set_of_closures) =
  fprintf ppf "@[<2>(move_within_set_of_closures@ %a <-- %a@ (closure = %a))@]"
    Closure_id.print move_within_set_of_closures.move_to
    Closure_id.print move_within_set_of_closures.start_from
    Variable.print move_within_set_of_closures.closure

let print_project_var ppf (project_var : project_var) =
  fprintf ppf "@[<2>(project_var@ %a@ from %a=%a)@]"
    Var_within_closure.print project_var.var
    Closure_id.print project_var.closure_id
    Variable.print project_var.closure

type t =
  | Project_var of project_var
  | Project_closure of project_closure
  | Move_within_set_of_closures of move_within_set_of_closures
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

  let print ppf t =
    match t with
    | Project_closure (project_closure) ->
      print_project_closure ppf project_closure
    | Project_var (project_var) -> print_project_var ppf project_var
    | Move_within_set_of_closures (move_within_set_of_closures) ->
      print_move_within_set_of_closures ppf move_within_set_of_closures
    | Field (field_index, var) ->
      fprintf ppf "Field %d of %a" field_index Variable.print var

  let output _ _ = failwith "Projection.output: not yet implemented"
end)

let to_named t ~map_projected_from : Flambda.named =
  match t with
  | Project_var project_var -> Project_var project_var
  | Project_closure project_closure -> Project_closure project_closure
  | Move_within_set_of_closures move -> Move_within_set_of_closures move
  | Field (field_index, var) ->
    Prim (Pfield field_index, [var], Debuginfo.none)

let projecting_from t =
  match t with
  | Project_var { var; _ } -> var
  | Project_closure { set_of_closures; _ } -> set_of_closures
  | Move_within_set_of_closures { closure; _ } -> closure
  | Field (_, var) -> var

let map_projecting_from t ~f : t =
  match t with
  | Project_var project_var ->
    let project_var : Flambda.project_var =
      { project_var with
        closure = f project_var.closure;
      }
    in
    Project_var project_var
  | Project_closure project_closure ->
    let project_closure : Flambda.project_closure =
      { project_closure with
        set_of_closures = f project_closure.set_of_closures;
      }
    in
    Project_closure project_closure
  | Move_within_set_of_closures move ->
    let move : Flambda.move_within_set_of_closures =
      { move with
        closure = f move.closure;
      }
    in
    Move_within_set_of_closures move
  | Field (field_index, var) -> Field (field_index, f var)

let freshen t ~freshening ~closure_freshening =

