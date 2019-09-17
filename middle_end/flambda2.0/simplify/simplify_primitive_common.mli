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

val simplify_projection
   : Downwards_acc.t
  -> original_term:Flambda.Named.t
  -> deconstructing:Flambda_type.t
  -> shape:Flambda_type.t
  -> result_var:Var_in_binding_pos.t
  -> result_kind:Flambda_kind.t
  -> Reachable.t * Flambda_type.Typing_env_extension.t * Downwards_acc.t

type cse =
  | Invalid of Flambda_type.t
  | Applied of
      (Reachable.t * Flambda_type.Typing_env_extension.t * Downwards_acc.t)
  | Not_applied of Downwards_acc.t

val try_cse
   : Downwards_acc.t
  -> original_prim:Flambda_primitive.t
  -> result_kind:Flambda_kind.t
  -> min_occurrence_kind:Name_occurrence_kind.t
  -> result_var:Variable.t
  -> cse
