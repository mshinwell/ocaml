(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Simplify_import

type t

val have_simplified_to_zero_terms : DA.t -> t

(** Note that even though there is one term, the binding might contain
    multiple bound variables, in the case of a set of closures. *)
val have_simplified_to_single_term
   : DA.t
  -> Bindable_let_bound.t
  -> Simplified_named.t
  -> t

val have_lifted_set_of_closures
   : DA.t
  -> Symbol.t Var_in_binding_pos.Map.t
  -> t

type descr = private
  | Zero_terms
  | Single_term of Bindable_let_bound.t * Simplified_named.t
  | Multiple_bindings_to_symbols of Symbol.t Var_in_binding_pos.Map.t

val descr : t -> descr

val dacc : t -> DA.t

val bindings_to_place_in_any_order
   : t
  -> (Bindable_let_bound.t * Simplified_named.t) list
