(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016--2018 OCamlPro SAS                                    *)
(*   Copyright 2016--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Inlining of continuations used only once.

    This runs on toplevel expressions (i.e. function bodies,
    [Initialize_symbol] bodies and [Effect] bodies) after simplification
    and collection of continuation use information has happened.
*)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

val for_toplevel_expression
   : Flambda.Expr.t
  -> Simplify_env_and_result.Result.t
  -> Flambda.Expr.t * Simplify_env_and_result.Result.t
