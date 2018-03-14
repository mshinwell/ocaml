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

(** Utility functions used throughout the simplifier. *)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type bounds_check_result = private
  | In_range
  | Out_of_range

val bounds_check
   : width:Flambda_primitive.string_accessor_width
  -> string_length_in_bytes:Targetint.OCaml.t
  -> index_in_bytes:Immediate.t
  -> bounds_check_result

val all_indexes_out_of_range
   : width:Flambda_primitive.string_accessor_width
  -> Immediate.Set.t
  -> max_string_length:Targetint.OCaml.t
  -> bool

val prepare_to_simplify_set_of_closures
   : env:Simplify_env_and_result.Env.t
  -> set_of_closures:Flambda.Set_of_closures.t
  -> function_decls:Flambda.Function_declarations.t
  -> Flambda.Function_declarations.t
       * Flambda_type.t
       * Simplify_env_and_result.Env.t

val prepare_to_simplify_closure
   : function_decl:Flambda.Function_declaration.t
  -> set_of_closures_env:Simplify_env_and_result.Env.t
  -> Simplify_env_and_result.Env.t

(** Command line argument "-inline". *)
val initial_inlining_threshold : round:int -> Inlining_cost.Threshold.t

(** Command line argument "-inline-toplevel". *)
val initial_inlining_toplevel_threshold
  : round:int -> Inlining_cost.Threshold.t

val params_for_exception_handler : unit -> Flambda.Typed_parameter.t list
