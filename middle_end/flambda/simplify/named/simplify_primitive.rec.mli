(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

(** Simplification of primitive applications. *)

val simplify_primitive
   : Downwards_acc.t
  -> original_named:Flambda.Named.t
  -> Flambda_primitive.t
  -> Debuginfo.t
  -> result_var:Var_in_binding_pos.t
  -> Simplified_named.t * Flambda_type.Typing_env_extension.t
       * Simple.t list * Downwards_acc.t
