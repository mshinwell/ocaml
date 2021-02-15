(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Simplification of primitives taking three arguments. *)

val simplify_ternary_primitive
   : Downwards_env.t
  -> Flambda_primitive.ternary_primitive
  -> Simple.t
  -> Simple.t
  -> Simple.t
  -> Debuginfo.t
  -> result_var:Var_in_binding_pos.t
  -> Simplified_named.t * Flambda_type.Typing_env_extension.t
       * Simple.t list * Downwards_env.t
