(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

type reify_primitive_at_toplevel_result =
  | Lift of {
    symbol : Symbol.t;
    static_const : Flambda.Static_const.t;
  }
  | Shared of Symbol.t
  | Cannot_reify

val reify_primitive_at_toplevel
   : Downwards_acc.t
  -> Var_in_binding_pos.t
  -> Flambda_type.t
  -> reify_primitive_at_toplevel_result
