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

[@@@ocaml.warning "+a-30-40-41-42"]

val simplify_projection
   : Downwards_env.t
  -> original_term:Flambda.Named.t
  -> deconstructing:Flambda_type.t
  -> shape:Flambda_type.t
  -> result_var:Var_in_binding_pos.t
  -> result_kind:Flambda_kind.t
  -> Simplified_named.t
     * Flambda_type.Typing_env_extension.t
     * Downwards_env.t

type cse =
  | Invalid of Flambda_type.t
  (* CR mshinwell: Use a record type for the following and all of the
     simplify_*primitive.mli files *)
  | Applied of
      (Simplified_named.t * Flambda_type.Typing_env_extension.t
        * Simple.t list * Downwards_env.t)
  | Not_applied of Downwards_env.t

val try_cse
   : Downwards_env.t
  -> original_prim:Flambda_primitive.t
  -> result_kind:Flambda_kind.t
  -> min_name_mode:Name_mode.t
  -> args:Simple.t list
  -> result_var:Variable.t
  -> cse
