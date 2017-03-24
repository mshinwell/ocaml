(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

let rec propagate env (expr : Cmm.expression) : Cmm.expression =
  match expr with
  | Cconst_int _
  | Cconst_natint _
  | Cconst_float _
  | Cconst_symbol _
  | Cconst_pointer _
  | Cconst_natpointer _
  | Cblockheader _ -> expr
  | Cvar var ->
    begin match Ident.Map.find var env with
    | exception Not_found -> expr
    | expr -> expr
    end
  | Clet (Immutable, id, defining_expr, body) ->
    let defining_expr = propagate env defining_expr in
    let env =
      match defining_expr with
      | Cconst_int _
      | Cconst_natint _
      | Cconst_float _
      | Cconst_symbol _
      | Cconst_pointer _
      | Cconst_natpointer _ -> Ident.Map.add id defining_expr env
      | Cblockheader _
      | Cvar _
      | Clet _
      | Cassign _
      | Ctuple _
      | Cop _
      | Csequence _
      | Cifthenelse _
      | Cswitch _
      | Cloop _
      | Ccatch _
      | Cexit _
      | Ctrywith _ -> env
    in
    let body = propagate env body in
    Clet (Immutable, id, defining_expr, body)
  | Clet (Mutable, id, defining_expr, body) ->
    Clet (Mutable, id, propagate env defining_expr, propagate env body)
  | Cassign (id, expr) -> Cassign (id, propagate env expr)
  | Ctuple exprs -> Ctuple (propagate_list env exprs)
  | Cop (op, args, dbg) -> Cop (op, propagate_list env args, dbg)
  | Csequence (expr1, expr2) ->
    Csequence (propagate env expr1, propagate env expr2)
  | Cifthenelse (cond, ifso, ifnot) ->
    Cifthenelse (propagate env cond, propagate env ifso,
      propagate env ifnot)
  | Cswitch (scrutinee, consts, arms, dbg) ->
    let scrutinee = propagate env scrutinee in
    let arms = Array.map (fun arm -> propagate env arm) arms in
    Cswitch (scrutinee, consts, arms, dbg)
  | Cloop expr -> Cloop (propagate env expr)
  | Ccatch (rec_flag, handlers, body) ->
    let handlers =
      List.map (fun (cont, params, handler) ->
          cont, params, propagate env handler)
        handlers
    in
    let body = propagate env body in
    Ccatch (rec_flag, handlers, body)
  | Cexit (cont, exprs) -> Cexit (cont, propagate_list env exprs)
  | Ctrywith (body, id, handler) ->
      Ctrywith (propagate env body, id, propagate env handler)

and propagate_list env exprs =
  List.map (fun expr -> propagate env expr) exprs

let fundecl (fundecl : Cmm.fundecl) =
  let fun_body = propagate Ident.Map.empty fundecl.fun_body in
  { fundecl with fun_body; }
