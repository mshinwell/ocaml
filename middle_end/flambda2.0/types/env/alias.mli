(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = private Simple.t
type elt = t

val initialise : unit -> unit

val create
   : Simple.t
  -> Binding_time.t
  -> Name_mode.t
  -> t

val create_symbol : Symbol.t -> t

val defined_earlier : t -> than:t -> bool

val simple : t -> Simple.t

val name_mode : t -> Name_mode.t

module Order_within_equiv_class
  : module type of struct include Name_mode end

val order_within_equiv_class : t -> Order_within_equiv_class.t

module Set_ordered_by_binding_time : Set.S with type elt := t

include Identifiable.S with type t := t

val set_to_simple_set : Set.t -> Simple.Set.t
