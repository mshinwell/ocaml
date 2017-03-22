(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Env : sig
  type t

  val empty : t
  val add : t -> Ident.t -> Cmm.expression -> t
  val lookup_expr : t -> Cmm.expression -> Cmm.expression
end = struct
  type t = Cmm.expression Ident.Map.t

  let empty = Ident.Map.empty

  let rec eligible (expr : Cmm.expression) =
    match expr with
    | Cvar _ -> true
    | Cop (op, args, _dbg) ->
        begin match op with
        | Caddi
        | Caddv
        | Cadda
        | Csubi
        | Cmuli
        | Cmulhi
        | Cmodi _
        | Cand
        | Cor
        | Cxor
        | Clsl
        | Clsr
        | Casr -> List.for_all (fun arg -> eligible arg) args
        | Capply _
        | Cextcall _
        | Cload _
        | Calloc
        | Cstore _
        | Cdivi _
        | Ccmpi _
        | Ccmpa _
        | Cnegf | Cabsf
        | Caddf | Csubf | Cmulf | Cdivf
        | Cfloatofint | Cintoffloat
        | Ccmpf _
        | Craise _
        | Ccheckbound -> false
        end
    | Cconst_int _
    | Cconst_natint _
    | Cconst_float _
    | Cconst_symbol _
    | Cconst_pointer _
    | Cconst_natpointer _
    | Cblockheader _ -> true
    | Clet _
    | Cassign _
    | Ctuple _
    | Csequence _
    | Cifthenelse _
    | Cswitch _
    | Cloop _
    | Ccatch _
    | Cexit _
    | Ctrywith _ -> false

  let add t id (expr : Cmm.expression) =
    if eligible expr then Ident.Map.add id expr t
    else t

  let lookup_expr t (expr : Cmm.expression) =
    match expr with
    | Cvar id ->
      begin match Ident.Map.find id t with
      | exception Not_found -> expr
      | expr -> expr
      end
    | _ -> expr
end

let add_no_overflow op n x c dbg : Cmm.expression =
  let d = n + x in
  if d = 0 then c else Cop(op, [c; Cmm.Cconst_int d], dbg)

(* CR mshinwell: Check that none of these assume the presence of a tag bit *)

let rec add_const_generic env op c n dbg : Cmm.expression =
  if n = 0 then c
  else
    let c = Env.lookup_expr env c in
    match c with
    | Cconst_int x when Misc.no_overflow_add x n -> Cconst_int (x + n)
    | Cop(op, [Cconst_int x; c], _)
      when Misc.no_overflow_add n x ->
        add_no_overflow op n x c dbg
    | Cop(op, [c; Cconst_int x], _)
      when Misc.no_overflow_add n x ->
        add_no_overflow op n x c dbg
    | Cop(Csubi, [Cconst_int x; c], _)
          when op = Cmm.Caddi && Misc.no_overflow_add n x ->
        Cop(Csubi, [Cmm.Cconst_int (n + x); c], dbg)
    | Cop(Csubi, [c; Cconst_int x], _)
          when op = Cmm.Caddi && Misc.no_overflow_sub n x ->
        add_const_generic env op c (n - x) dbg
    | c -> Cop(op, [c; Cmm.Cconst_int n], dbg)

let add_const env c n dbg = add_const_generic env Cmm.Caddi c n dbg

let rec add_int (op : Cmm.operation) env c1 c2 dbg : Cmm.expression =
  assert (op = Cmm.Caddi || op = Cmm.Cadda || op = Cmm.Caddv);
  let c1 = Env.lookup_expr env c1 in
  let c2 = Env.lookup_expr env c2 in
  match (c1, c2) with
  | (Cconst_int n, c) | (c, Cconst_int n) ->
      add_const_generic env op c n dbg
  | (Cop(op', [c1; Cconst_int n1], _), c2) when op = op' ->
      add_const_generic env op (add_int op env c1 c2 dbg) n1 dbg
  | (c1, Cop(op', [c2; Cconst_int n2], _)) when op = op' ->
      add_const_generic env op (add_int op env c1 c2 dbg) n2 dbg
  | (_, _) ->
      Cop(op, [c1; c2], dbg)

let rec sub_int env c1 c2 dbg : Cmm.expression =
  let c1 = Env.lookup_expr env c1 in
  let c2 = Env.lookup_expr env c2 in
  match (c1, c2) with
  | (c1, Cconst_int n2) when n2 <> min_int ->
      add_const env c1 (-n2) dbg
  | (c1, Cop(Caddi, [c2; Cconst_int n2], _)) when n2 <> min_int ->
      add_const env (sub_int env c1 c2 dbg) (-n2) dbg
  | (Cop(Caddi, [c1; Cconst_int n1], _), c2) ->
      add_const env (sub_int env c1 c2 dbg) n1 dbg
  | (c1, c2) ->
      Cop(Csubi, [c1; c2], dbg)

let rec lsl_int env c1 c2 dbg : Cmm.expression =
  let c1 = Env.lookup_expr env c1 in
  let c2 = Env.lookup_expr env c2 in
  match (c1, c2) with
  | (Cop(Clsl, [c; Cconst_int n1], _), Cconst_int n2)
    when n1 > 0 && n2 > 0 && n1 + n2 < Arch.size_int * 8 ->
      Cop(Clsl, [c; Cmm.Cconst_int (n1 + n2)], dbg)
  | (Cop((Caddi | Cadda | Caddv) as op, [c1; Cconst_int n1], _), Cconst_int n2)
    when Misc.no_overflow_lsl n1 n2 ->
      add_const_generic env op (lsl_int env c1 c2 dbg) (n1 lsl n2) dbg
  | (_, _) ->
      Cop(Clsl, [c1; c2], dbg)

let is_power2 n =
  n = 1 lsl Misc.log2 n

let mult_power2 env c n dbg =
  lsl_int env c (Cconst_int (Misc.log2 n)) dbg

let rec mul_int env c1 c2 dbg : Cmm.expression =
  let c1 = Env.lookup_expr env c1 in
  let c2 = Env.lookup_expr env c2 in
  match (c1, c2) with
  | (c, Cconst_int 0) | (Cconst_int 0, c) -> Csequence (c, Cconst_int 0)
  | (c, Cconst_int 1) | (Cconst_int 1, c) ->
      c
  | (c, Cconst_int(-1)) | (Cconst_int(-1), c) ->
      sub_int env (Cconst_int 0) c dbg
  | (c, Cconst_int n) when is_power2 n -> mult_power2 env c n dbg
  | (Cconst_int n, c) when is_power2 n -> mult_power2 env c n dbg
  | (Cop((Caddi | Cadda | Caddv) as op, [c; Cconst_int n], _), Cconst_int k)
  | (Cconst_int k, Cop((Caddi | Cadda | Caddv) as op, [c; Cconst_int n], _))
    when Misc.no_overflow_mul n k ->
      add_const_generic env op
        (mul_int env c (Cmm.Cconst_int k) dbg) (n * k) dbg
  | (c1, c2) ->
      Cop(Cmuli, [c1; c2], dbg)

let lsr_int env c1 c2 dbg : Cmm.expression =
  let c1 = Env.lookup_expr env c1 in
  let c2 = Env.lookup_expr env c2 in
  match c2 with
  | Cconst_int 0 ->
      c1
  | Cconst_int n when n > 0 ->
      Cop(Clsr, [Cmmgen.ignore_low_bit_int c1; c2], dbg)
  | _ ->
      Cop(Clsr, [c1; c2], dbg)

let asr_int env c1 c2 dbg : Cmm.expression =
  let c1 = Env.lookup_expr env c1 in
  let c2 = Env.lookup_expr env c2 in
  match c2 with
  | Cconst_int 0 ->
      c1
  | Cconst_int n when n > 0 ->
      Cop(Casr, [Cmmgen.ignore_low_bit_int c1; c2], dbg)
  | _ ->
      Cop(Casr, [c1; c2], dbg)

(* Turning integer divisions into multiply-high then shift.
   The [division_parameters] function is used in module Emit for
   those target platforms that support this optimization. *)

(* Unsigned comparison between native integers. *)

let ucompare x y =
  let open! Nativeint in
  compare (add x min_int) (add y min_int)

(* Unsigned division and modulus at type nativeint.
   Algorithm: Hacker's Delight section 9.3 *)

let udivmod n d =
  let open! Nativeint in
  if d < 0n then
    if ucompare n d < 0 then (0n, n) else (1n, sub n d)
  else begin
    let q = shift_left (div (shift_right_logical n 1) d) 1 in
    let r = sub n (mul q d) in
    if ucompare r d >= 0 then (succ q, sub r d) else (q, r)
  end

(* Compute division parameters.
   Algorithm: Hacker's Delight chapter 10, fig 10-1. *)

let divimm_parameters d =
  let open! Nativeint in
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
  in loop (size - 1) (udivmod twopsm1 nc) (udivmod twopsm1 d)

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

let rec div_int env c1 c2 is_safe dbg : Cmm.expression =
  let open Cmm in
  let bind = Cmmgen.bind in
  let c1 = Env.lookup_expr env c1 in
  let c2 = Env.lookup_expr env c2 in
  match (c1, c2) with
  | (c1, Cconst_int 0) ->
      Csequence(c1, Cmmgen.raise_symbol dbg "caml_exn_Division_by_zero")
  | (c1, Cconst_int 1) ->
      c1
  | (Cconst_int n1, Cconst_int n2) ->
      Cconst_int (n1 / n2)
  | (c1, Cconst_int n) when n <> min_int ->
      let l = Misc.log2 n in
      if n = 1 lsl l then
        (* Algorithm:
              t = shift-right-signed(c1, l - 1)
              t = shift-right(t, W - l)
              t = c1 + t
              res = shift-right-signed(c1 + t, l)
        *)
        Cop(Casr, [bind "dividend" c1 (fun c1 ->
                     let t = asr_int env c1 (Cconst_int (l - 1)) dbg in
                     let t =
                      lsr_int env t (Cconst_int (Nativeint.size - l)) dbg
                     in
                     add_int Caddi env c1 t dbg);
                   Cconst_int l], dbg)
      else if n < 0 then
        sub_int env (Cconst_int 0)
          (div_int env c1 (Cconst_int (-n)) is_safe dbg) dbg
      else begin
        let (m, p) = divimm_parameters (Nativeint.of_int n) in
        (* Algorithm:
              t = multiply-high-signed(c1, m)
              if m < 0, t = t + c1
              if p > 0, t = shift-right-signed(t, p)
              res = t + sign-bit(c1)
        *)
        (* CR mshinwell: This should use [add_int], etc. *)
        bind "dividend" c1 (fun c1 ->
          let t = Cop(Cmulhi, [c1; Cconst_natint m], dbg) in
          let t = if m < 0n then Cop(Caddi, [t; c1], dbg) else t in
          let t = if p > 0 then Cop(Casr, [t; Cconst_int p], dbg) else t in
          add_int Caddi env
            t (lsr_int env c1 (Cconst_int (Nativeint.size - 1)) dbg) dbg)
      end
  | (c1, c2) when !Clflags.fast || is_safe = Lambda.Unsafe ->
      Cop(Cdivi is_safe, [c1; c2], dbg)
  | (c1, c2) ->
      bind "divisor" c2 (fun c2 ->
        Cifthenelse(c2,
                    Cop(Cdivi is_safe, [c1; c2], dbg),
                    Cmmgen.raise_symbol dbg "caml_exn_Division_by_zero"))

let mod_int env c1 c2 is_safe dbg : Cmm.expression =
  let open Cmm in
  let bind = Cmmgen.bind in
  let c1 = Env.lookup_expr env c1 in
  let c2 = Env.lookup_expr env c2 in
  match (c1, c2) with
  | (c1, Cconst_int 0) ->
      Csequence(c1, Cmmgen.raise_symbol dbg "caml_exn_Division_by_zero")
  | (c1, Cconst_int (1 | (-1))) ->
      Csequence(c1, Cconst_int 0)
  | (Cconst_int n1, Cconst_int n2) ->
      Cconst_int (n1 mod n2)
  | (c1, (Cconst_int n as c2)) when n <> min_int ->
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
          let t = asr_int env c1 (Cconst_int (l - 1)) dbg in
          let t = lsr_int env t (Cconst_int (Nativeint.size - l)) dbg in
          let t = add_int Caddi env c1 t dbg in
          let t = Cop(Cand, [t; Cconst_int (-n)], dbg) in
          sub_int env c1 t dbg)
      else
        bind "dividend" c1 (fun c1 ->
          sub_int env
            c1 (mul_int env (div_int env c1 c2 is_safe dbg) c2 dbg) dbg)
  | (c1, c2) when !Clflags.fast || is_safe = Lambda.Unsafe ->
      (* Flambda already generates this test *)
      Cop(Cmodi is_safe, [c1; c2], dbg)
  | (c1, c2) ->
      bind "divisor" c2 (fun c2 ->
        Cifthenelse(c2,
                    Cop(Cmodi is_safe, [c1; c2], dbg),
                    Cmmgen.raise_symbol dbg "caml_exn_Division_by_zero"))

let simplify_op env (op : Cmm.operation) args dbg expr =
  match op with
  | Capply _
  | Cextcall _
  | Cload _
  | Calloc
  | Cstore _ -> expr
  | Caddi -> add_int Caddi env args.(0) args.(1) dbg
  | Caddv -> add_int Caddv env args.(0) args.(1) dbg
  | Cadda -> add_int Cadda env args.(0) args.(1) dbg
  | Csubi -> sub_int env args.(0) args.(1) dbg
  | Cmuli -> mul_int env args.(0) args.(1) dbg
  | Cmulhi -> expr
  | Cdivi is_safe -> div_int env args.(0) args.(1) is_safe dbg
  | Cmodi is_safe -> mod_int env args.(0) args.(1) is_safe dbg
  | Cand
  | Cor
  | Cxor -> expr
  | Clsl -> lsl_int env args.(0) args.(1) dbg
  | Clsr -> lsr_int env args.(0) args.(1) dbg
  | Casr -> asr_int env args.(0) args.(1) dbg
  | Ccmpi _
  | Ccmpa _
  | Cnegf | Cabsf
  | Caddf | Csubf | Cmulf | Cdivf
  | Cfloatofint | Cintoffloat
  | Ccmpf _
  | Craise _
  | Ccheckbound -> expr

let rec simplify_expr env (expr : Cmm.expression) : Cmm.expression =
  match expr with
  | Cconst_int _
  | Cconst_natint _
  | Cconst_float _
  | Cconst_symbol _
  | Cconst_pointer _
  | Cconst_natpointer _
  | Cblockheader _
  | Cvar _ -> expr
  | Clet (id, defining_expr, body) ->
      let defining_expr = simplify_expr env defining_expr in
      let env = Env.add env id defining_expr in
      let body = simplify_expr env body in
      Clet (id, defining_expr, body)
  | Cassign (id, expr) -> Cassign (id, simplify_expr env expr)
  | Ctuple exprs -> Ctuple (simplify_exprs env exprs)
  | Cop (op, args, dbg) ->
      let args = Array.of_list (simplify_exprs env args) in
      simplify_op env op args dbg expr
  | Csequence (expr1, expr2) ->
      Csequence (simplify_expr env expr1, simplify_expr env expr2)
  | Cifthenelse (cond, ifso, ifnot) ->
      Cifthenelse (simplify_expr env cond, simplify_expr env ifso,
        simplify_expr env ifnot)
  | Cswitch (scrutinee, consts, arms, dbg) ->
      let scrutinee = simplify_expr env scrutinee in
      let arms = Array.map (fun arm -> simplify_expr env arm) arms in
      Cswitch (scrutinee, consts, arms, dbg)
  | Cloop expr -> Cloop (simplify_expr env expr)
  | Ccatch (rec_flag, handlers, body) ->
    let handlers =
      List.map (fun (cont, params, handler) ->
          cont, params, simplify_expr env handler)
        handlers
    in
    let body = simplify_expr env body in
    Ccatch (rec_flag, handlers, body)
  | Cexit (cont, exprs) -> Cexit (cont, simplify_exprs env exprs)
  | Ctrywith (body, id, handler) ->
      Ctrywith (simplify_expr env body, id, simplify_expr env handler)

and simplify_exprs env exprs =
  List.map (fun expr -> simplify_expr env expr) exprs

let fundecl (fundecl : Cmm.fundecl) =
  let fun_body = simplify_expr Env.empty fundecl.fun_body in
  { fundecl with fun_body; }
