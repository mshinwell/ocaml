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

(** Simplification of the right-hand sides of [Let] bindings. *)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

val simplify_named
   : DA.t
  -> bound_vars:Bindable_let_bound.t
  -> Named.t
  -> after_traversal:(
       DA.t
    -> rebuild:(
         UA.t
      -> after_rebuild:(
           bindings_outermost_first:(Bindable_let_bound.t * Reachable.t) list
        -> UA.t
        -> 'a)
      -> 'a)
    -> 'b)
  -> 'b
