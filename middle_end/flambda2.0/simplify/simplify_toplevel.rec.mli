(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

val simplify_toplevel
   : DA.t
  -> Expr.t
  -> return_continuation:Continuation.t
  -> return_arity:Flambda_arity.t
  -> Exn_continuation.t
  -> return_cont_scope:Scope.t
  -> exn_cont_scope:Scope.t
  -> after_traversal:(
       DA.t
    -> rebuild:(UA.t -> after_rebuild:(Expr.t -> UA.t -> 'a) -> 'a)
    -> 'b)
  -> 'b
