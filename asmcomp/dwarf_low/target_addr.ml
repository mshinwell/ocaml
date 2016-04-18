(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* CR-someday mshinwell: if proper cross compilation support is
   implemented, change this as appropriate. *)
include Nativeint

let size _ = Int64.of_int Arch.size_addr

let emit t asm =
  let module A = (val asm : Asm_directives.S) in
  A.nativeint t

let to_int64 t = Int64.of_nativeint t

let zero = 0n
let highest = -1n
