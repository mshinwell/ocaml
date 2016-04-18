(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Single operators only for the moment.  There are other cases (p23-24) *)
type t = Operator.t

let at_offset_from_symbol ~base ~symbol ~offset_in_bytes =
  Operator.at_offset_from_symbol ~base ~symbol ~offset_in_bytes

let at_symbol symbol =
  Operator.at_symbol symbol

let in_register ~reg_number =
  Operator.register ~reg_number

let register_based_addressing ~reg_number ~offset_in_bytes =
  Operator.register_based_addressing ~reg_number ~offset_in_bytes

let frame_base_register ~offset_in_bytes =
  Operator.frame_base_register ~offset_in_bytes

let size t =
  Operator.size t

let emit t asm =
  Operator.emit t asm
