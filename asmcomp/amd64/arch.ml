(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2000 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* CR mshinwell: How do we determine the defaults for these values?
   It seems like maybe a configure script test is needed. *)

(* CR mshinwell: This potentially major caveat should probably go in the help
   text of the relevant option below. *)

(* LZCNT instruction is not available on Intel Architectures prior to Haswell.

   Important: lzcnt assembles to bsr on architectures prior to Haswell.  Code
   that uses lzcnt will run on older Intels and silently produce wrong
   results. *)
let lzcnt_support = ref true

(* CR mshinwell: Likewise, I would put something to this effect in the help
   text below. *)
(* POPCNT instruction is not available prior to Nehalem. *)
let popcnt_support = ref true

(* PREFETCHW instruction is not available on processors
   based on Haswell or earlier microarchitectures.
*)
let prefetchw_support = ref true

(* Machine-specific command-line options *)

let command_line_options =
  [ "-fPIC", Arg.Set Clflags.pic_code,
      " Generate position-independent machine code (default)";
    "-fno-PIC", Arg.Clear Clflags.pic_code,
      " Generate position-dependent machine code";
    "-flzcnt", Arg.Set lzcnt_support,
      " Use lzcnt instruction to count leading zeros";
    "-fno-lzcnt", Arg.Clear lzcnt_support,
      " Do not use lzcnt instruction to count leading zeros";
    "-fpopcnt", Arg.Set popcnt_support,
      " Use popcnt instruction to count the number of bits set";
    "-fno-popcnt", Arg.Clear popcnt_support,
      " Do not use popcnt instruction to count the number of bits set";
    "-fprefetchw", Arg.Set prefetchw_support,
      " Use prefetchw and prefetchwt1 instructions";
    "-fno-prefetchw", Arg.Clear prefetchw_support,
      " Do not use prefetchw and prefetchwt1 instructions";
  ]

(* Specific operations for the AMD64 processor *)

open Format

type temporal_locality = Not_at_all | Low | Moderate | High

let temporal_locality = function
  | Not_at_all -> "none" (* CR mshinwell: same comment as in the Cmm part *)
  | Low -> "low"
  | Moderate -> "moderate"
  | High -> "high"

type addressing_mode =
    Ibased of string * int              (* symbol + displ *)
  | Iindexed of int                     (* reg + displ *)
  | Iindexed2 of int                    (* reg + reg + displ *)
  | Iscaled of int * int                (* reg * scale + displ *)
  | Iindexed2scaled of int * int        (* reg + reg * scale + displ *)

(* CR mshinwell: rename to prefetch_locality_hint or something?  (I left a
   similar CR elsewhere; it would be worth ensuring the names match.) *)
type hint = {
  is_write: bool;
  locality: temporal_locality;
  addr: addressing_mode;
}

type specific_operation =
    Ilea of addressing_mode             (* "lea" gives scaled adds *)
  | Istore_int of nativeint * addressing_mode * bool
                                        (* Store an integer constant *)
  | Ioffset_loc of int * addressing_mode (* Add a constant to a location *)
  | Ifloatarithmem of float_operation * addressing_mode
                                       (* Float arith operation with memory *)
  | Ibswap of int                      (* endianness conversion *)
  | Isqrtf                             (* Float square root *)
  | Ifloatsqrtf of addressing_mode     (* Float square root from memory *)
  | Isextend32                         (* 32 to 64 bit conversion with sign
                                          extension *)
  | Izextend32                         (* 32 to 64 bit conversion with zero
                                          extension *)
  | Ilzcnt                             (* count leading zeros instruction *)
  (* CR mshinwell: [non_zero] isn't very descriptive.  Maybe
     "arg_is_definitely_non_zero" or something (assuming that's what it
     means)? *)
  | Ibsr of { non_zero : bool }        (* bit scan reverse instruction *)
  | Ibsf of { non_zero : bool }        (* bit scan forward instruction *)
  | Irdtsc                             (* read timestamp *)
  | Irdpmc                             (* read performance counter *)
  | Icrc32q                            (* compute crc *)
  | Iprefetch of hint                  (* memory prefetching hint *)

and float_operation =
    Ifloatadd | Ifloatsub | Ifloatmul | Ifloatdiv

let spacetime_node_hole_pointer_is_live_before _specific_op = false

(* Sizes, endianness *)

let big_endian = false

let size_addr = 8
let size_int = 8
let size_float = 8

let allow_unaligned_access = true

(* Behavior of division *)

let division_crashes_on_overflow = true

(* Operations on addressing modes *)

let identity_addressing = Iindexed 0

let offset_addressing addr delta =
  match addr with
    Ibased(s, n) -> Ibased(s, n + delta)
  | Iindexed n -> Iindexed(n + delta)
  | Iindexed2 n -> Iindexed2(n + delta)
  | Iscaled(scale, n) -> Iscaled(scale, n + delta)
  | Iindexed2scaled(scale, n) -> Iindexed2scaled(scale, n + delta)

let num_args_addressing = function
    Ibased _ -> 0
  | Iindexed _ -> 1
  | Iindexed2 _ -> 2
  | Iscaled _ -> 1
  | Iindexed2scaled _ -> 2

(* Printing operations and addressing modes *)

let print_addressing printreg addr ppf arg =
  match addr with
  | Ibased(s, 0) ->
      fprintf ppf "\"%s\"" s
  | Ibased(s, n) ->
      fprintf ppf "\"%s\" + %i" s n
  | Iindexed n ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a%s" printreg arg.(0) idx
  | Iindexed2 n ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a + %a%s" printreg arg.(0) printreg arg.(1) idx
  | Iscaled(scale, n) ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a  * %i%s" printreg arg.(0) scale idx
  | Iindexed2scaled(scale, n) ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a + %a * %i%s" printreg arg.(0) printreg arg.(1) scale idx

let print_specific_operation printreg op ppf arg =
  match op with
  | Ilea addr -> print_addressing printreg addr ppf arg
  | Istore_int(n, addr, is_assign) ->
      fprintf ppf "[%a] := %nd %s"
         (print_addressing printreg addr) arg n
         (if is_assign then "(assign)" else "(init)")
  | Ioffset_loc(n, addr) ->
      fprintf ppf "[%a] +:= %i" (print_addressing printreg addr) arg n
  | Isqrtf ->
      fprintf ppf "sqrtf %a" printreg arg.(0)
  | Ifloatsqrtf addr ->
     fprintf ppf "sqrtf float64[%a]"
             (print_addressing printreg addr) [|arg.(0)|]
  | Ifloatarithmem(op, addr) ->
      let op_name = function
      | Ifloatadd -> "+f"
      | Ifloatsub -> "-f"
      | Ifloatmul -> "*f"
      | Ifloatdiv -> "/f" in
      fprintf ppf "%a %s float64[%a]" printreg arg.(0) (op_name op)
                   (print_addressing printreg addr)
                   (Array.sub arg 1 (Array.length arg - 1))
  | Ibswap i ->
      fprintf ppf "bswap_%i %a" i printreg arg.(0)
  | Isextend32 ->
      fprintf ppf "sextend32 %a" printreg arg.(0)
  | Izextend32 ->
      fprintf ppf "zextend32 %a" printreg arg.(0)
  | Ilzcnt ->
      fprintf ppf "lzcnt %a" printreg arg.(0)
  | Ibsr { non_zero; } ->
      fprintf ppf "bsr non_zero=%b %a" non_zero printreg arg.(0)
  | Ibsf { non_zero; } ->
      fprintf ppf "bsf non_zero=%b %a" non_zero printreg arg.(0)
  | Irdtsc ->
      fprintf ppf "rdtsc"
  | Irdpmc ->
      fprintf ppf "rdpmc %a" printreg arg.(0)
  | Icrc32q ->
      fprintf ppf "crc32 %a %a" printreg arg.(0) printreg arg.(1)
  | Iprefetch { is_write; locality; } ->
      fprintf ppf "prefetch is_write=%b temporal_locality=%s %a" is_write
        (temporal_locality locality) printreg arg.(0)

let win64 =
  match Config.system with
  | "win64" | "mingw64" | "cygwin" -> true
  | _                   -> false
