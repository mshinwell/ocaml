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

(** Identify variables used in function bodies (free variables or
    specialised args, for example) whose approximation says they are
    closures or blocks.  Replace uses of projections from such variables
    with new variables and build a mapping from the new variables to the
    projection expressions.  The benefit of removing the projections is
    also returned.

    The returned definitions of extracted projection expressions are
    collated together in a list.  Each member of the list corresponds to
    all discovered projections from one particular variable.
    Note that the [Flambda.expr] values here are not rewritten in any
    way: they still reference the variables they did when they were in
    the function body.  Users of this pass may need to perform a
    substitution (see [Unbox_specialised_args] for example).
*)
type projection_defns = Flambda.expr Variable.Map.t list

type result = {
  projection_defns : projection_defns;
  new_function_body : Flambda.expr;
  additional_free_vars : Variable.t Variable.Map.t;
  benefit : Inlining_cost.Benefit.t;
}

(** [which_variables] maps inner variables to outer variables in the
    manner of [free_vars] and [specialised_args] in
    [Flambda.set_of_closures]. *)
val from_function_decl
   : which_variables:Variable.t Variable.Map.t
  -> env:Inline_and_simplify_aux.Env.t
  -> function_decl:Flambda.function_declaration
  -> result
