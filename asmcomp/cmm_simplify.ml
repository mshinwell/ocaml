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

  val add : t -> Ident.t -> Cmm.expression -> t

  val lookup_expr : t -> Cmm.expression -> Cmm.expression
end = struct

end

let rec add_int env c1 c2 dbg =
  let c1 = Env.lookup_expr env c1 in
  let c2 = Env.lookup_expr env c2 in
  match (c1, c2) with
  | (Cconst_int n, c) | (c, Cconst_int n) ->
      add_const c n dbg
  | (Cop(Caddi, [c1; Cconst_int n1], _), c2) ->
      add_const (add_int c1 c2 dbg) n1 dbg
  | (c1, Cop(Caddi, [c2; Cconst_int n2], _)) ->
      add_const (add_int c1 c2 dbg) n2 dbg
  | (_, _) ->
      Cop(Caddi, [c1; c2], dbg)

let rec sub_int c1 c2 dbg =
  let c1 = Env.lookup_expr env c1 in
  let c2 = Env.lookup_expr env c2 in
  match (c1, c2) with
  | (c1, Cconst_int n2) when n2 <> min_int ->
      add_const c1 (-n2) dbg
  | (c1, Cop(Caddi, [c2; Cconst_int n2], _)) when n2 <> min_int ->
      add_const (sub_int c1 c2 dbg) (-n2) dbg
  | (Cop(Caddi, [c1; Cconst_int n1], _), c2) ->
      add_const (sub_int c1 c2 dbg) n1 dbg
  | (c1, c2) ->
      Cop(Csubi, [c1; c2], dbg)

let rec lsl_int c1 c2 dbg =
  let c1 = Env.lookup_expr env c1 in
  let c2 = Env.lookup_expr env c2 in
  match (c1, c2) with
  | (Cop(Clsl, [c; Cconst_int n1], _), Cconst_int n2)
    when n1 > 0 && n2 > 0 && n1 + n2 < size_int * 8 ->
      Cop(Clsl, [c; Cconst_int (n1 + n2)], dbg)
  | (Cop(Caddi, [c1; Cconst_int n1], _), Cconst_int n2)
    when no_overflow_lsl n1 n2 ->
      add_const (lsl_int c1 c2 dbg) (n1 lsl n2) dbg
  | (_, _) ->
      Cop(Clsl, [c1; c2], dbg)

let is_power2 n =
  n = 1 lsl Misc.log2 n
and mult_power2 c n dbg =
  lsl_int c (Cconst_int (Misc.log2 n)) dbg

let rec mul_int c1 c2 dbg =
  let c1 = Env.lookup_expr env c1 in
  let c2 = Env.lookup_expr env c2 in
  match (c1, c2) with
  | (c, Cconst_int 0) | (Cconst_int 0, c) -> Csequence (c, Cconst_int 0)
  | (c, Cconst_int 1) | (Cconst_int 1, c) ->
      c
  | (c, Cconst_int(-1)) | (Cconst_int(-1), c) ->
      sub_int (Cconst_int 0) c dbg
  | (c, Cconst_int n) when is_power2 n -> mult_power2 c n dbg
  | (Cconst_int n, c) when is_power2 n -> mult_power2 c n dbg
  | (Cop(Caddi, [c; Cconst_int n], _), Cconst_int k) |
    (Cconst_int k, Cop(Caddi, [c; Cconst_int n], _))
    when no_overflow_mul n k ->
      add_const (mul_int c (Cconst_int k) dbg) (n * k) dbg
  | (c1, c2) ->
      Cop(Cmuli, [c1; c2], dbg)

let ignore_low_bit_int = function
  | Cop(Caddi, [(Cop(Clsl, [_; Cconst_int n], _) as c); Cconst_int 1], _)
      when n > 0
      -> c
  | Cop(Cor, [c; Cconst_int 1], _) -> c
  | c -> c

let lsr_int c1 c2 dbg =
  let c1 = Env.lookup_expr env c1 in
  let c2 = Env.lookup_expr env c2 in
  match c2 with
  | Cconst_int 0 ->
      c1
  | Cconst_int n when n > 0 ->
      Cop(Clsr, [ignore_low_bit_int c1; c2], dbg)
  | _ ->
      Cop(Clsr, [c1; c2], dbg)

let asr_int c1 c2 dbg =
  let c1 = Env.lookup_expr env c1 in
  let c2 = Env.lookup_expr env c2 in
  match c2 with
  | Cconst_int 0 ->
      c1
  | Cconst_int n when n > 0 ->
      Cop(Casr, [ignore_low_bit_int c1; c2], dbg)
  | _ ->
      Cop(Casr, [c1; c2], dbg)

let rec simplify_op env (op : Cmm.operation) args dbg expr =
  match op with
  | Capply _
  | Cextcall _
  | Cload _
  | Calloc
  | Cstore _ -> expr
  | Caddi -> add_int env args.(0) args.(1) dbg
  | Csubi -> sub_int env args.(0) args.(1) dbg
  | Cmuli -> mul_int env args.(0) args.(1) dbg
  | Cmulhi
  | Cdivi
  | Cmodi
  | Cand
  | Cor
  | Cxor -> expr
  | Clsl -> lsl_int env args.(0) args.(1) dbg
  | Clsr -> lsr_int env args.(0) args.(1) dbg
  | Casr -> asr_int env args.(0) args.(1) dbg
  | Ccmpi _
  | Caddv
  | Cadda
  | Ccmpa _
  | Cnegf | Cabsf
  | Caddf | Csubf | Cmulf | Cdivf
  | Cfloatofint | Cintoffloat
  | Ccmpf _
  | Craise _
  | Ccheckbound -> expr

let rec simplify_expr env (expr : Cmm.expression) =
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
      let body = simplify env body in
      Clet (id, defining_expr, body)
  | Cassign (id, expr) -> Cassign (id, simplify env expr)
  | Ctuple exprs -> Ctuple (simplify_exprs env exprs)
  | Cop (op, args, dbg) -> simplify_op env op args dbg expr
  | Csequence (expr1, expr2) ->
      Csequence (simplify env expr1, simplify env expr2)
  | Cifthenelse (cond, ifso, ifnot) ->
      Cifthenelse (simplify env cond, simplify env ifso, simplify env ifnot)
  | Cswitch of expression * int array * expression array * Debuginfo.t
  | Cloop expr -> Cloop (simplify env expr)
  | Ccatch of rec_flag * (int * Ident.t list * expression) list * expression
  | Cexit (cont, exprs) -> Cexit (cont, simplify_list env exprs)
  | Ctrywith (body, id, handler) ->
      Ctrywith (simplify env body, id, simplify env handler)

let run expr =
  let expr, _env = simplify_expr Env.empty expr in
  expr
