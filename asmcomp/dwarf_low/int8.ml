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

type t = int

let zero = 0
let one = 1

let of_int_exn i =
  if i < -(1 lsl 8) || i > ((1 lsl 8) - 1) then
    Misc.fatal_errorf "Int8.of_int_exn: %d is out of range" i
  else
    i

let to_int i = i
