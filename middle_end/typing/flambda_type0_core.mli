(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make (W : Typing_world.S) :
  Flambda_type0_core_intf.S
    with module Blocks = W.Blocks
    with module Both_meet_and_join = W.Both_meet_and_join
    with module Closure_elements = W.Closure_elements
    with module Closure_ids = W.Closure_ids
    with module Closures_entry_by_closure_id = W.Closures_entry_by_closure_id
    with module Discriminants = W.Discriminants
    with module Expr = W.Expr
    with module Function_type = W.Function_type
    with module Immediates = W.Immediates
    with module Join_env = W.Join_env
    with module Meet_env = W.Meet_env
    with module Type_printers = W.Type_printers
    with module Types_by_closure_id = W.Types_by_closure_id
