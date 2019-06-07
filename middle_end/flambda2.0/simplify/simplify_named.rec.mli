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

val simplify_named
   : Downwards_acc.t
  -> Flambda.Named.t
  -> result_var:Variable.t
  -> Downwards_acc.t * Flambda_type.t * Reachable.t

val simplify_function
   : Downwards_acc.t
  -> Closure_id.t
  -> Flambda.Function_declaration.t
  -> type_of_my_closure:(
       Closure_id.t
    -> param_arity:Flambda_arity.t
    -> result_arity:Flambda_arity.t
    -> Flambda_type.t)
  -> Flambda.Function_declaration.t * Flambda_type.function_declaration
       * Downwards_acc.t
