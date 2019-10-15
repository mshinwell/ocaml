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

type t
type elt = t

val create
   : Flambda_kind.t
  -> Simple.t
  -> Binding_time.t
  -> Name_occurrence_kind.t
  -> t

val create_name
   : Flambda_kind.t
  -> Name.t
  -> Binding_time.t
  -> Name_occurrence_kind.t
  -> t

include Identifiable.S with type t := t

val defined_earlier : t -> than:t -> bool

val simple : t -> Simple.t

val kind : t -> Flambda_kind.t

val implicitly_bound_and_canonical : t -> bool

val name_occurrence_kind : t -> Name_occurrence_kind.t

module Order_within_equiv_class
  : module type of struct include Name_occurrence_kind end

val order_within_equiv_class : t -> Order_within_equiv_class.t
