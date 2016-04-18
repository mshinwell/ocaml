(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t

include Emittable.S with type t := t

(** Constraints on [reg] are the same as for [Proc.dwarf_register_number]. *)
val in_register : reg:Reg.t -> t
val at_offset_from_register: reg:Reg.t -> offset_in_bytes:Target_addr.t -> t

val at_offset_from_stack_pointer : offset_in_bytes:Target_addr.t -> t
val at_offset_from_frame_pointer : offset_in_bytes:Target_addr.t -> t

val at_computed_offset_from_stack_pointer
   : offset_in_bytes:(unit -> Target_addr.t)
  -> t

val at_symbol : Symbol.t -> t
