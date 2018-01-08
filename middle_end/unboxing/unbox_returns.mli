(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017 OCamlPro SAS                                          *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Unbox the return values of functions, such that they can return multiple
    results at once without allocation. *)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

val run
   : continuation_uses:
       Simplify_env_and_result.Result.Continuation_uses.t Continuation.Map.t
  -> function_decls:Flambda.Function_declarations.t
  -> backend:(module Backend_intf.S)
  -> Flambda.Function_declarations.t
