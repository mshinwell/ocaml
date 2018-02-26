(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
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

(*
(** For the compilation of switch statements. *)
module Switch_storer : sig
  val mk_store : unit -> Continuation.t Switch.t_store
end
*)

(*
type specialised_to_same_as =
  | Not_specialised
  | Specialised_and_aliased_to of Variable.Set.t

(** For each parameter in a given set of function declarations and the usual
    specialised-args mapping, determine which other parameters are specialised
    to the same variable as that parameter.
    The result is presented as a map from [fun_vars] to lists, corresponding
    componentwise to the usual [params] list in the corresponding function
    declaration. *)
val parameters_specialised_to_the_same_variable
   : function_decls:Flambda.Function_declarations.t
  -> specialised_args:Flambda.specialised_to Variable.Map.t
  -> specialised_to_same_as list Variable.Map.t
*)

(*
val create_wrapper_params
   : params:Flambda.Typed_parameter.t list
  -> freshening_already_assigned:
    Flambda.Typed_parameter.t Flambda.Typed_parameter.Map.t
  -> Flambda.Typed_parameter.t Flambda.Typed_parameter.Map.t
    * Flambda.Typed_parameter.t list
*)

val make_let_cont_alias
   : name:Continuation.t
  -> alias_of:Continuation.t
  -> parameter_types:Flambda_type.t list
  -> Flambda.Let_cont_handlers.t
