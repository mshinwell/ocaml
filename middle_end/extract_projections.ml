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

(* CR-soon pchambart: should we restrict only to cases
  when the field is aliased to a variable outside
  of the closure (i.e. when we can certainly remove
  the allocation of the block) ?
  Note that this may prevent cases with imbricated
  closures from benefiting from this transformations.
  mshinwell: What word was "imbricated" supposed to be?
  (The code this referred to has been deleted, but the same thing is
  probably still happening).
*)

let from_function_decl ~which_variables
      ~(function_decl : Flambda.function_declaration) =
  let projections = ref Projection.Set.empty in
  let used_which_variables = ref Variable.Set.empty in
  let for_expr (expr : Flambda.expr) =
    match expr with
    | Var var ->
      if Variable.Set.mem var which_variables then begin
        used_which_variables := Variable.Set.add var !used_which_variables
      end
    | _ -> ()
  in
  let for_named (named : Flambda.named) =
    match named with
    | Project_var project_var ->
      projections :=
        Projection.Set.add (Project_var project_var) !projections
    | Project_closure project_closure ->
      projections :=
        Projection.Set.add (Project_closure project_closure) !projections
    | Move_within_set_of_closures move ->
      projections :=
        Projection.Set.add (Move_within_set_of_closures move) !projections
    | Prim (Pfield field_index, [var], _dbg) ->
      projections :=
        Projection.Set.add (Field (field_index, var)) !projections
    | _ -> ()
  in
  Flambda_iterators.iter for_expr for_named function_decl.body;
  let projections = !projections in
  let used_which_variables = !used_which_variables in
  (* Don't extract projections whose [projecting_from] variable is also
     used boxed.  We could in the future consider being more sophisticated
     about this based on the uses in the body, but given we are not doing
     that yet, it seems safest in performance terms not to (e.g.) unbox a
     specialised argument whose boxed version is used. *)
  Projection.Set.filter (fun projection ->
      let projecting_from = Projection.projecting_from projection in
      not (Variable.Set.mem projecting_from used_which_variables))
    projections
