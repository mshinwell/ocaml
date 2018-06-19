(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Miscellaneous utility functions on Flambda terms that don't really
    belong in any of the submodules of [Flambda]. *)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** Like [Flambda_static.make_closure_map], but takes a mapping from set
    of closures IDs to function declarations, instead of a [Program]. *)
val make_closure_map'
   : Flambda.Function_declarations.t Set_of_closures_id.Map.t
  -> Flambda.Function_declarations.t Closure_id.Map.t

val make_variable_symbol : Variable.t -> Symbol.t

val make_variables_symbol
   : Variable.t list
  -> Symbol.t

val create_wrapper_params
   : params:Flambda_type.Parameters.t
  -> freshening_already_assigned:Freshening.t
  -> Freshening.t * Flambda_type.Parameters.t

val make_let_cont_alias
   : name:Continuation.t
  -> alias_of:Continuation.t
  -> params:Parameters.t
  -> Flambda.Let_cont_handlers.t
