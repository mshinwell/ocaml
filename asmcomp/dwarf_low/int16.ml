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

let of_int_exn i =
  if i < -(1 lsl 16) || i > ((1 lsl 16) - 1) then
    Misc.fatal_errorf "Int16.of_int_exn: %d is out of range" i
  else
    i

let of_int64_exn i =
  if Int64.compare i 0xFFFFL >= 0 then
    Misc.fatal_errorf "Int16.of_int64_exn: %Ld is out of range" i
  else
    Int64.to_int i

let to_int t = t
