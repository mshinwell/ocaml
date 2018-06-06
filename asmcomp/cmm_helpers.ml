(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module V = Backend_var
module VP = Backend_var.With_provenance
open Cmm
open Arch

(* Local binding of complex expressions *)

let bind name arg fn =
  match arg with
    Cvar _ | Cconst_int _ | Cconst_natint _ | Cconst_symbol _
  | Cconst_pointer _ | Cconst_natpointer _
  | Cblockheader _ -> fn arg
  | _ -> let id = V.create_local name in Clet(VP.create id, arg, fn (Cvar id))

let bind_load name arg fn =
  match arg with
  | Cop(Cload _, [Cvar _], _) -> fn arg
  | _ -> bind name arg fn

let bind_nonvar name arg fn =
  match arg with
    Cconst_int _ | Cconst_natint _ | Cconst_symbol _
  | Cconst_pointer _ | Cconst_natpointer _
  | Cblockheader _ -> fn arg
  | _ -> let id = V.create_local name in Clet(VP.create id, arg, fn (Cvar id))

let caml_black = Nativeint.shift_left (Nativeint.of_int 3) 8
    (* cf. runtime/caml/gc.h *)

(* Block headers. Meaning of the tag field: see stdlib/obj.ml *)

let floatarray_tag dbg = Cconst_int (Obj.double_array_tag, dbg)

let block_header tag sz =
  Nativeint.add (Nativeint.shift_left (Nativeint.of_int sz) 10)
                (Nativeint.of_int tag)
(* Static data corresponding to "value"s must be marked black in case we are
   in no-naked-pointers mode.  See [caml_darken] and the code below that emits
   structured constants and static module definitions. *)
let black_block_header tag sz = Nativeint.logor (block_header tag sz) caml_black
let white_closure_header sz = block_header Obj.closure_tag sz
let black_closure_header sz = black_block_header Obj.closure_tag sz
let infix_header ofs = block_header Obj.infix_tag ofs
let float_header = block_header Obj.double_tag (size_float / size_addr)
let floatarray_header len =
  (* Zero-sized float arrays have tag zero for consistency with
     [caml_alloc_float_array]. *)
  assert (len >= 0);
  if len = 0 then block_header 0 0
  else block_header Obj.double_array_tag (len * size_float / size_addr)
let string_header len =
      block_header Obj.string_tag ((len + size_addr) / size_addr)
let boxedint32_header = block_header Obj.custom_tag 2
let boxedint64_header = block_header Obj.custom_tag (1 + 8 / size_addr)
let boxedintnat_header = block_header Obj.custom_tag 2

let alloc_float_header dbg = Cblockheader (float_header, dbg)
let alloc_floatarray_header len dbg = Cblockheader (floatarray_header len, dbg)
let alloc_closure_header sz dbg = Cblockheader (white_closure_header sz, dbg)
let alloc_infix_header ofs dbg = Cblockheader (infix_header ofs, dbg)
let alloc_boxedint32_header dbg = Cblockheader (boxedint32_header, dbg)
let alloc_boxedint64_header dbg = Cblockheader (boxedint64_header, dbg)
let alloc_boxedintnat_header dbg = Cblockheader (boxedintnat_header, dbg)

(* Integers *)

let max_repr_int = max_int asr 1
let min_repr_int = min_int asr 1

let int_const dbg n =
  if n <= max_repr_int && n >= min_repr_int
  then Cconst_int((n lsl 1) + 1, dbg)
  else Cconst_natint
          (Nativeint.add (Nativeint.shift_left (Nativeint.of_int n) 1) 1n, dbg)

let natint_const_untagged dbg n =
  if n > Nativeint.of_int max_int
  || n < Nativeint.of_int min_int
  then Cconst_natint (n,dbg)
  else Cconst_int (Nativeint.to_int n, dbg)

let cint_const n =
  Cint(Nativeint.add (Nativeint.shift_left (Nativeint.of_int n) 1) 1n)

let targetint_const n =
  Targetint.add (Targetint.shift_left (Targetint.of_int n) 1)
    Targetint.one

let add_no_overflow n x c dbg =
  let d = n + x in
  if d = 0 then c else Cop(Caddi, [c; Cconst_int (d, dbg)], dbg)

let rec add_const c n dbg =
  if n = 0 then c
  else match c with
  | Cconst_int (x, _) when Misc.no_overflow_add x n -> Cconst_int (x + n, dbg)
  | Cop(Caddi, [Cconst_int (x, _); c], _)
    when Misc.no_overflow_add n x ->
      add_no_overflow n x c dbg
  | Cop(Caddi, [c; Cconst_int (x, _)], _)
    when Misc.no_overflow_add n x ->
      add_no_overflow n x c dbg
  | Cop(Csubi, [Cconst_int (x, _); c], _) when Misc.no_overflow_add n x ->
      Cop(Csubi, [Cconst_int (n + x, dbg); c], dbg)
  | Cop(Csubi, [c; Cconst_int (x, _)], _) when Misc.no_overflow_sub n x ->
      add_const c (n - x) dbg
  | c -> Cop(Caddi, [c; Cconst_int (n, dbg)], dbg)

let incr_int c dbg = add_const c 1 dbg
let decr_int c dbg = add_const c (-1) dbg

let rec add_int c1 c2 dbg =
  match (c1, c2) with
  | (Cconst_int (n, _), c) | (c, Cconst_int (n, _)) ->
      add_const c n dbg
  | (Cop(Caddi, [c1; Cconst_int (n1, _)], _), c2) ->
      add_const (add_int c1 c2 dbg) n1 dbg
  | (c1, Cop(Caddi, [c2; Cconst_int (n2, _)], _)) ->
      add_const (add_int c1 c2 dbg) n2 dbg
  | (_, _) ->
      Cop(Caddi, [c1; c2], dbg)

let rec sub_int c1 c2 dbg =
  match (c1, c2) with
  | (c1, Cconst_int (n2, _)) when n2 <> min_int ->
      add_const c1 (-n2) dbg
  | (c1, Cop(Caddi, [c2; Cconst_int (n2, _)], _)) when n2 <> min_int ->
      add_const (sub_int c1 c2 dbg) (-n2) dbg
  | (Cop(Caddi, [c1; Cconst_int (n1, _)], _), c2) ->
      add_const (sub_int c1 c2 dbg) n1 dbg
  | (c1, c2) ->
      Cop(Csubi, [c1; c2], dbg)

let rec lsl_int c1 c2 dbg =
  match (c1, c2) with
  | (Cop(Clsl, [c; Cconst_int (n1, _)], _), Cconst_int (n2, _))
    when n1 > 0 && n2 > 0 && n1 + n2 < size_int * 8 ->
      Cop(Clsl, [c; Cconst_int (n1 + n2, dbg)], dbg)
  | (Cop(Caddi, [c1; Cconst_int (n1, _)], _), Cconst_int (n2, _))
    when Misc.no_overflow_lsl n1 n2 ->
      add_const (lsl_int c1 c2 dbg) (n1 lsl n2) dbg
  | (_, _) ->
      Cop(Clsl, [c1; c2], dbg)

let is_power2 n = n = 1 lsl Misc.log2 n

and mult_power2 c n dbg = lsl_int c (Cconst_int (Misc.log2 n, dbg)) dbg

let rec mul_int c1 c2 dbg =
  match (c1, c2) with
  | (c, Cconst_int (0, _)) | (Cconst_int (0, _), c) ->
      Csequence (c, Cconst_int (0, dbg))
  | (c, Cconst_int (1, _)) | (Cconst_int (1, _), c) ->
      c
  | (c, Cconst_int(-1, _)) | (Cconst_int(-1, _), c) ->
      sub_int (Cconst_int (0, dbg)) c dbg
  | (c, Cconst_int (n, _)) when is_power2 n -> mult_power2 c n dbg
  | (Cconst_int (n, _), c) when is_power2 n -> mult_power2 c n dbg
  | (Cop(Caddi, [c; Cconst_int (n, _)], _), Cconst_int (k, _)) |
    (Cconst_int (k, _), Cop(Caddi, [c; Cconst_int (n, _)], _))
    when Misc.no_overflow_mul n k ->
      add_const (mul_int c (Cconst_int (k, dbg)) dbg) (n * k) dbg
  | (c1, c2) ->
      Cop(Cmuli, [c1; c2], dbg)


let ignore_low_bit_int = function
    Cop(Caddi,
        [(Cop(Clsl, [_; Cconst_int (n, _)], _) as c); Cconst_int (1, _)], _)
      when n > 0
      -> c
  | Cop(Cor, [c; Cconst_int (1, _)], _) -> c
  | c -> c

let lsr_int c1 c2 dbg =
  match c2 with
    Cconst_int (0, _) ->
      c1
  | Cconst_int (n, _) when n > 0 ->
      Cop(Clsr, [ignore_low_bit_int c1; c2], dbg)
  | _ ->
      Cop(Clsr, [c1; c2], dbg)

let asr_int c1 c2 dbg =
  match c2 with
    Cconst_int (0, _) ->
      c1
  | Cconst_int (n, _) when n > 0 ->
      Cop(Casr, [ignore_low_bit_int c1; c2], dbg)
  | _ ->
      Cop(Casr, [c1; c2], dbg)

let tag_int i dbg =
  match i with
    Cconst_int (n, _) ->
      int_const dbg n
  | Cop(Casr, [c; Cconst_int (n, _)], _) when n > 0 ->
      Cop(Cor,
        [asr_int c (Cconst_int (n - 1, dbg)) dbg; Cconst_int (1, dbg)],
        dbg)
  | c ->
      incr_int (lsl_int c (Cconst_int (1, dbg)) dbg) dbg

let force_tag_int i dbg =
  match i with
    Cconst_int (n, _) ->
      int_const dbg n
  | Cop(Casr, [c; Cconst_int (n, _)], dbg') when n > 0 ->
      Cop(Cor, [asr_int c (Cconst_int (n - 1, dbg)) dbg'; Cconst_int (1, dbg)],
        dbg)
  | c ->
      Cop(Cor, [lsl_int c (Cconst_int (1, dbg)) dbg; Cconst_int (1, dbg)], dbg)

let untag_int i dbg =
  match i with
    Cconst_int (n, _) -> Cconst_int(n asr 1, dbg)
  | Cop(Caddi, [Cop(Clsl, [c; Cconst_int (1, _)], _); Cconst_int (1, _)], _) ->
      c
  | Cop(Cor, [Cop(Casr, [c; Cconst_int (n, _)], _); Cconst_int (1, _)], _)
    when n > 0 && n < size_int * 8 ->
      Cop(Casr, [c; Cconst_int (n+1, dbg)], dbg)
  | Cop(Cor, [Cop(Clsr, [c; Cconst_int (n, _)], _); Cconst_int (1, _)], _)
    when n > 0 && n < size_int * 8 ->
      Cop(Clsr, [c; Cconst_int (n+1, dbg)], dbg)
  | Cop(Cor, [c; Cconst_int (1, _)], _) ->
      Cop(Casr, [c; Cconst_int (1, dbg)], dbg)
  | c -> Cop(Casr, [c; Cconst_int (1, dbg)], dbg)

let mk_if_then_else dbg cond ifso_dbg ifso ifnot_dbg ifnot =
  match cond with
  | Cconst_int (0, _) -> ifnot
  | Cconst_int (1, _) -> ifso
  | _ ->
    Cifthenelse(cond, ifso_dbg, ifso, ifnot_dbg, ifnot, dbg)

let mk_not dbg cmm =
  match cmm with
  | Cop(Caddi,
        [Cop(Clsl, [c; Cconst_int (1, _)], _); Cconst_int (1, _)], dbg') ->
    begin
      match c with
      | Cop(Ccmpi cmp, [c1; c2], dbg'') ->
          tag_int
            (Cop(Ccmpi (negate_integer_comparison cmp), [c1; c2], dbg'')) dbg'
      | Cop(Ccmpa cmp, [c1; c2], dbg'') ->
          tag_int
            (Cop(Ccmpa (negate_integer_comparison cmp), [c1; c2], dbg'')) dbg'
      | Cop(Ccmpf cmp, [c1; c2], dbg'') ->
          tag_int
            (Cop(Ccmpf (negate_float_comparison cmp), [c1; c2], dbg'')) dbg'
      | _ ->
        (* 0 -> 3, 1 -> 1 *)
        Cop(Csubi,
            [Cconst_int (3, dbg); Cop(Clsl, [c; Cconst_int (1, dbg)], dbg)], dbg)
    end
  | Cconst_int (3, _) -> Cconst_int (1, dbg)
  | Cconst_int (1, _) -> Cconst_int (3, dbg)
  | c ->
      (* 1 -> 3, 3 -> 1 *)
      Cop(Csubi, [Cconst_int (4, dbg); c], dbg)


(* Turning integer divisions into multiply-high then shift.
   The [division_parameters] function is used in module Emit for
   those target platforms that support this optimization. *)

(* Unsigned comparison between native integers. *)

let ucompare x y = Nativeint.(compare (add x min_int) (add y min_int))

(* Unsigned division and modulus at type nativeint.
   Algorithm: Hacker's Delight section 9.3 *)

let udivmod n d = Nativeint.(
  if d < 0n then
    if ucompare n d < 0 then (0n, n) else (1n, sub n d)
  else begin
    let q = shift_left (div (shift_right_logical n 1) d) 1 in
    let r = sub n (mul q d) in
    if ucompare r d >= 0 then (succ q, sub r d) else (q, r)
  end)

(* Compute division parameters.
   Algorithm: Hacker's Delight chapter 10, fig 10-1. *)

let divimm_parameters d = Nativeint.(
  assert (d > 0n);
  let twopsm1 = min_int in (* 2^31 for 32-bit archs, 2^63 for 64-bit archs *)
  let nc = sub (pred twopsm1) (snd (udivmod twopsm1 d)) in
  let rec loop p (q1, r1) (q2, r2) =
    let p = p + 1 in
    let q1 = shift_left q1 1 and r1 = shift_left r1 1 in
    let (q1, r1) =
      if ucompare r1 nc >= 0 then (succ q1, sub r1 nc) else (q1, r1) in
    let q2 = shift_left q2 1 and r2 = shift_left r2 1 in
    let (q2, r2) =
      if ucompare r2 d >= 0 then (succ q2, sub r2 d) else (q2, r2) in
    let delta = sub d r2 in
    if ucompare q1 delta < 0 || (q1 = delta && r1 = 0n)
    then loop p (q1, r1) (q2, r2)
    else (succ q2, p - size)
  in loop (size - 1) (udivmod twopsm1 nc) (udivmod twopsm1 d))

(* The result [(m, p)] of [divimm_parameters d] satisfies the following
   inequality:

      2^(wordsize + p) < m * d <= 2^(wordsize + p) + 2^(p + 1)    (i)

   from which it follows that

      floor(n / d) = floor(n * m / 2^(wordsize+p))
                              if 0 <= n < 2^(wordsize-1)
      ceil(n / d) = floor(n * m / 2^(wordsize+p)) + 1
                              if -2^(wordsize-1) <= n < 0

   The correctness condition (i) above can be checked by the code below.
   It was exhaustively tested for values of d from 2 to 10^9 in the
   wordsize = 64 case.

let add2 (xh, xl) (yh, yl) =
  let zl = add xl yl and zh = add xh yh in
  ((if ucompare zl xl < 0 then succ zh else zh), zl)

let shl2 (xh, xl) n =
  assert (0 < n && n < size + size);
  if n < size
  then (logor (shift_left xh n) (shift_right_logical xl (size - n)),
        shift_left xl n)
  else (shift_left xl (n - size), 0n)

let mul2 x y =
  let halfsize = size / 2 in
  let halfmask = pred (shift_left 1n halfsize) in
  let xl = logand x halfmask and xh = shift_right_logical x halfsize in
  let yl = logand y halfmask and yh = shift_right_logical y halfsize in
  add2 (mul xh yh, 0n)
    (add2 (shl2 (0n, mul xl yh) halfsize)
       (add2 (shl2 (0n, mul xh yl) halfsize)
          (0n, mul xl yl)))

let ucompare2 (xh, xl) (yh, yl) =
  let c = ucompare xh yh in if c = 0 then ucompare xl yl else c

let validate d m p =
  let md = mul2 m d in
  let one2 = (0n, 1n) in
  let twoszp = shl2 one2 (size + p) in
  let twop1 = shl2 one2 (p + 1) in
  ucompare2 twoszp md < 0 && ucompare2 md (add2 twoszp twop1) <= 0
*)

let raise_regular dbg exc =
  Csequence(
    Cop(Cstore (Thirtytwo_signed, Lambda.Assignment),
        [(Cconst_symbol ("caml_backtrace_pos", dbg));
         Cconst_int (0, dbg)], dbg),
      Cop(Craise Raise_withtrace,[exc], dbg))

let raise_symbol dbg symb =
  raise_regular dbg (Cconst_symbol (symb, dbg))

let rec div_int c1 c2 is_safe dbg =
  match (c1, c2) with
    (c1, Cconst_int (0, _)) ->
      Csequence(c1, raise_symbol dbg "caml_exn_Division_by_zero")
  | (c1, Cconst_int (1, _)) ->
      c1
  | (Cconst_int (n1, _), Cconst_int (n2, _)) ->
      Cconst_int (n1 / n2, dbg)
  | (c1, Cconst_int (n, _)) when n <> min_int ->
      let l = Misc.log2 n in
      if n = 1 lsl l then
        (* Algorithm:
              t = shift-right-signed(c1, l - 1)
              t = shift-right(t, W - l)
              t = c1 + t
              res = shift-right-signed(c1 + t, l)
        *)
        Cop(Casr, [bind "dividend" c1 (fun c1 ->
                     let t = asr_int c1 (Cconst_int (l - 1, dbg)) dbg in
                     let t =
                       lsr_int t (Cconst_int (Nativeint.size - l, dbg)) dbg
                     in
                     add_int c1 t dbg);
                   Cconst_int (l, dbg)], dbg)
      else if n < 0 then
        sub_int (Cconst_int (0, dbg))
          (div_int c1 (Cconst_int (-n, dbg)) is_safe dbg)
          dbg
      else begin
        let (m, p) = divimm_parameters (Nativeint.of_int n) in
        (* Algorithm:
              t = multiply-high-signed(c1, m)
              if m < 0, t = t + c1
              if p > 0, t = shift-right-signed(t, p)
              res = t + sign-bit(c1)
        *)
        bind "dividend" c1 (fun c1 ->
          let t = Cop(Cmulhi, [c1; Cconst_natint (m, dbg)], dbg) in
          let t = if m < 0n then Cop(Caddi, [t; c1], dbg) else t in
          let t =
            if p > 0 then Cop(Casr, [t; Cconst_int (p, dbg)], dbg) else t
          in
          add_int t (lsr_int c1 (Cconst_int (Nativeint.size - 1, dbg)) dbg) dbg)
      end
  | (c1, c2) when !Clflags.unsafe || is_safe = Lambda.Unsafe ->
      Cop(Cdivi, [c1; c2], dbg)
  | (c1, c2) ->
      bind "divisor" c2 (fun c2 ->
        bind "dividend" c1 (fun c1 ->
          Cifthenelse(c2,
                      dbg,
                      Cop(Cdivi, [c1; c2], dbg),
                      dbg,
                      raise_symbol dbg "caml_exn_Division_by_zero",
                      dbg)))

let mod_int c1 c2 is_safe dbg =
  match (c1, c2) with
    (c1, Cconst_int (0, _)) ->
      Csequence(c1, raise_symbol dbg "caml_exn_Division_by_zero")
  | (c1, Cconst_int ((1 | (-1)), _)) ->
      Csequence(c1, Cconst_int (0, dbg))
  | (Cconst_int (n1, _), Cconst_int (n2, _)) ->
      Cconst_int (n1 mod n2, dbg)
  | (c1, (Cconst_int (n, _) as c2)) when n <> min_int ->
      let l = Misc.log2 n in
      if n = 1 lsl l then
        (* Algorithm:
              t = shift-right-signed(c1, l - 1)
              t = shift-right(t, W - l)
              t = c1 + t
              t = bit-and(t, -n)
              res = c1 - t
         *)
        bind "dividend" c1 (fun c1 ->
          let t = asr_int c1 (Cconst_int (l - 1, dbg)) dbg in
          let t = lsr_int t (Cconst_int (Nativeint.size - l, dbg)) dbg in
          let t = add_int c1 t dbg in
          let t = Cop(Cand, [t; Cconst_int (-n, dbg)], dbg) in
          sub_int c1 t dbg)
      else
        bind "dividend" c1 (fun c1 ->
          sub_int c1 (mul_int (div_int c1 c2 is_safe dbg) c2 dbg) dbg)
  | (c1, c2) when !Clflags.unsafe || is_safe = Lambda.Unsafe ->
      (* Flambda already generates that test *)
      Cop(Cmodi, [c1; c2], dbg)
  | (c1, c2) ->
      bind "divisor" c2 (fun c2 ->
        bind "dividend" c1 (fun c1 ->
          Cifthenelse(c2,
                      dbg,
                      Cop(Cmodi, [c1; c2], dbg),
                      dbg,
                      raise_symbol dbg "caml_exn_Division_by_zero",
                      dbg)))

(* Division or modulo on boxed integers.  The overflow case min_int / -1
   can occur, in which case we force x / -1 = -x and x mod -1 = 0. (PR#5513). *)

let is_different_from x = function
    Cconst_int (n, _) -> n <> x
  | Cconst_natint (n, _) -> n <> Nativeint.of_int x
  | _ -> false

let safe_divmod_bi mkop is_safe mkm1 c1 c2 bi dbg =
  bind "dividend" c1 (fun c1 ->
  bind "divisor" c2 (fun c2 ->
    let c = mkop c1 c2 is_safe dbg in
    if Arch.division_crashes_on_overflow
    && (size_int = 4 || bi <> Primitive.Pint32)
    && not (is_different_from (-1) c2)
    then
      Cifthenelse(Cop(Ccmpi Cne, [c2; Cconst_int (-1, dbg)], dbg),
        dbg, c,
        dbg, mkm1 c1 dbg,
        dbg)
    else
      c))

let safe_div_bi is_safe =
  safe_divmod_bi div_int is_safe
    (fun c1 dbg -> Cop(Csubi, [Cconst_int (0, dbg); c1], dbg))

let safe_mod_bi is_safe =
  safe_divmod_bi mod_int is_safe (fun _ dbg -> Cconst_int (0, dbg))

(* Bool *)

let test_bool dbg cmm =
  match cmm with
  | Cop(Caddi, [Cop(Clsl, [c; Cconst_int (1, _)], _); Cconst_int (1, _)], _) ->
      c
  | Cconst_int (n, dbg) ->
      if n = 1 then
        Cconst_int (0, dbg)
      else
        Cconst_int (1, dbg)
  | c -> Cop(Ccmpi Cne, [c; Cconst_int (1, dbg)], dbg)

(* Float *)

let box_float dbg c = Cop(Calloc, [alloc_float_header dbg; c], dbg)

let map_ccatch f rec_flag handlers body =
  let handlers = List.map
      (fun (n, ids, handler, dbg) -> (n, ids, f handler, dbg))
      handlers in
  Ccatch(rec_flag, handlers, f body)

let rec unbox_float dbg cmm =
  match cmm with
  | Cop(Calloc, [Cblockheader (header, _); c], _) when header = float_header ->
      c
  | Clet(id, exp, body) -> Clet(id, exp, unbox_float dbg body)
  | Cifthenelse(cond, ifso_dbg, e1, ifnot_dbg, e2, dbg) ->
      Cifthenelse(cond,
        ifso_dbg, unbox_float dbg e1,
        ifnot_dbg, unbox_float dbg e2,
        dbg)
  | Csequence(e1, e2) -> Csequence(e1, unbox_float dbg e2)
  | Cswitch(e, tbl, el, dbg') ->
    Cswitch(e, tbl,
      Array.map (fun (expr, dbg) -> unbox_float dbg expr, dbg) el, dbg')
  | Ccatch(rec_flag, handlers, body) ->
    map_ccatch (unbox_float dbg) rec_flag handlers body
  | Ctrywith(e1, id, e2, dbg) ->
      Ctrywith(unbox_float dbg e1, id, unbox_float dbg e2, dbg)
  | c -> Cop(Cload (Double_u, Asttypes.Immutable), [c], dbg)

