(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017--2018 OCamlPro SAS                                    *)
(*   Copyright 2017--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Given a variable (expected to be a parameter of a function or
    continuation) and its type, devise a strategy to unbox it. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module How_to_unbox : sig
  type t = private {
    being_unboxed_to_wrapper_params_being_unboxed : Variable.t Variable.Map.t;
    add_bindings_in_wrapper : Flambda.Expr.t -> Flambda.Expr.t;
    new_arguments_for_call_in_wrapper : Variable.t list;
    new_type_for_unboxee : Flambda_type.t;
    build_boxed_value_from_new_params :
      (Flambda.Typed_parameter.t * (Flambda.Expr.t -> Flambda.Expr.t)) list;
  }

  val merge : t -> t -> t
  val merge_variable_map : t Variable.Map.t -> t
end

val how_to_unbox
   : (env:Simplify_env_and_result.t
  -> unboxee:Variable.t
  -> unboxee_ty:Flambda_type.t
  -> is_unbox_returns:bool
  -> How_to_unbox.t option) Flambda_type.type_accessor
