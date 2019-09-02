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

include Numbers.Int

let strictly_earlier t ~than =
  t < than

let consts_and_discriminants = 0
let symbols = 1
let earliest_var = 2

let succ t =
  if t < earliest_var then
    Misc.fatal_error "Cannot increment binding time for symbols"
  else
    t + 1
