(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*          Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Address = struct
  type t =
    | Int32 of Int32.t
    | Int64 of Int64.t

  type word_size = Four | Eight

  let word_size () =
    match Sys.word_size with
    | 32 -> Four
    | 64 -> Eight
    | bits -> Misc.fatal_errorf "Unknown word size %d" bits

  let zero () =
    match word_size () with
    | Four -> Int32 Int32.zero
    | Eight -> Int64 Int64.zero

  let all_ones () =
    match word_size () with
    | Four -> Int32 Int32.minus_one
    | Eight -> Int64 Int64.minus_one

  let thirty_two_bit_bounds =
    Int64.of_int32 Int32.min_int,
      Int64.of_int32 Int32.max_int

  let of_int_exn i =
    let min, max =
      match word_size () with
      | Four -> thirty_two_bit_bounds
      | Eight -> Int64.min_int, Int64.max_int
    in
    let i' = Int64.of_int i in
    if Int64.compare i' min < 0 || Int64.compare i' max > 0 then
      Misc.fatal_errorf "Target_system.Address.of_int_exn: 0x%Ld out of range"
        i'
    else
      match word_size () with
      | Four -> Int32 (Int32.of_int i)
      | Eight -> Int64 i'

  let to_int64 = function
    | Int32 i -> Int64.of_int32 i
    | Int64 i -> i
end

type system =
  (* 32 and 64 bit *)
  | S_macosx
  | S_gnu
  | S_cygwin
  (* 32 bit only *)
  | S_solaris
  | S_win32
  | S_linux_elf
  | S_bsd_elf
  | S_beos
  | S_mingw
  (* 64 bit only *)
  | S_win64
  | S_linux
  | S_mingw64
  | S_unknown

let system = match Config.system with
  | "macosx" -> S_macosx
  | "solaris" -> S_solaris
  | "win32" -> S_win32
  | "linux_elf" -> S_linux_elf
  | "bsd_elf" -> S_bsd_elf
  | "beos" -> S_beos
  | "gnu" -> S_gnu
  | "cygwin" -> S_cygwin
  | "mingw" -> S_mingw
  | "mingw64" -> S_mingw64
  | "win64" -> S_win64
  | "linux" -> S_linux
  | _ -> S_unknown

let windows =
  match system with
  | S_mingw64 | S_cygwin | S_win64 -> true
  | _ -> false

let masm =
  match system with
  | S_win32 | S_win64 -> true
  | _ -> false
