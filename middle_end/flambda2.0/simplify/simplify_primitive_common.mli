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

open! Simplify_common

val meet_shape
   : TE.t
  -> T.t
  -> shape:T.t
  -> result_var:Var_in_binding_pos.t
  -> result_kind:K.t
  -> TEE.t Or_bottom.t

val simplify_projection
   : DA.t
  -> original_term:Flambda.Named.t
  -> deconstructing:T.t
  -> shape:T.t
  -> result_var:Var_in_binding_pos.t
  -> result_kind:K.t
  -> Reachable.t * TEE.t * DA.t

type cse =
  | Invalid of T.t
  | Applied of (Reachable.t * TEE.t * DA.t)
  | Not_applied of DA.t

val try_cse
   : DA.t
  -> original_prim:P.t
  -> result_kind:K.t
  -> min_occurrence_kind:Name_occurrence_kind.t
  -> result_var:Variable.t
  -> cse
