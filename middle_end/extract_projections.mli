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

(** Identify projections from variables used in function bodies (free
    variables or specialised args, for example, according to [which_variables]
    below) whose approximation says they are closures or blocks. *)

type result = Projection.t list

(** [which_variables] maps inner variables to outer variables in the
    manner of [free_vars] and [specialised_args] in
    [Flambda.set_of_closures]. *)
val from_function_decl
   : which_variables:Flambda.specialised_to Variable.Map.t
  -> env:Inline_and_simplify_aux.Env.t
  -> function_decl:Flambda.function_declaration
  -> result option

val print_result : Format.formatter -> result -> unit
