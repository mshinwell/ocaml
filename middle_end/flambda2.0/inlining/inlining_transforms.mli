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

open! Flambda.Import

val inline
   : Simplify_env_and_result.Env.t
  -> callee:Simple.t
  -> args:Simple.t list
  -> Flambda_type.function_declaration
  -> apply_return_continuation:Continuation.t
  -> apply_exn_continuation:Exn_continuation.t
  -> Debuginfo.t
  -> Inline_attribute.t
  -> (Simplify_env_and_result.Env.t * Expr.t) option
