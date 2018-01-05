
[@@@ocaml.warning "+a-4-30-40-41-42"]

let rec expand_switch ~scrutinee ~cases dbg : Cmm.expression =
  let compare_cases ((num1 : int), _expr1) (num2, _expr2) =
    Pervasives.compare num1 num2
  in
  Array.sort compare_cases cases;
  match cases with
  | [| |] -> Ctuple []
  | [| _num, expr |] -> expr
  | [| _num1, Cmm.Cexit (k1, []); _num2, Cmm.Cexit (k2, []) |] when k1 = k2 ->
    Cexit (k1, [])
  | [| num1, expr1; _num2, expr2 |] ->
    let cond : Cmm.expression =
      Cop (Ccmpi Ceq, [Cmm.Cvar scrutinee; Cmm.Cconst_int num1], dbg)
    in
    Cifthenelse (cond, expr1, expr2)
  | _ ->
    let size = Array.length cases in
    assert (size >= 3);
    let pivot_pos = (size / 2) - (1 - (size mod 2)) in
    assert (pivot_pos >= 0 && pivot_pos < size - 1);
    let pivot_elt, _pivot_expr = cases.(pivot_pos) in
    let left_cases_and_pivot = Array.sub cases 0 (pivot_pos + 1) in
    let right_cases =
      Array.sub cases (pivot_pos + 1) (size - (pivot_pos + 1))
    in
    assert (
      Array.length left_cases_and_pivot + Array.length right_cases = size);
    let cond : Cmm.expression =
      Cop (Ccmpi Cle, [Cmm.Cvar scrutinee; Cmm.Cconst_int pivot_elt], dbg)
    in
    let left_expr = expand_switch ~scrutinee ~cases:left_cases_and_pivot dbg in
    let right_expr = expand_switch ~scrutinee ~cases:right_cases dbg in
    match left_expr, right_expr with
    | Cexit (k1, []), Cexit (k2, []) when k1 = k2 -> Cexit (k1, [])
    | _, _ -> Cifthenelse (cond, left_expr, right_expr)

let rec rewrite_expression (expr : Cmm.expression) : Cmm.expression =
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
    let defining_expr = rewrite_expression defining_expr in
    let body = rewrite_expression body in
    Clet (id, defining_expr, body)
  | Cassign (id, new_value) -> Cassign (id, rewrite_expression new_value)
  | Ctuple exprs -> Ctuple (List.map rewrite_expression exprs)
  | Cop (op, exprs, dbg) -> Cop (op, List.map rewrite_expression exprs, dbg)
  | Csequence (expr1, expr2) ->
    Csequence (rewrite_expression expr1, rewrite_expression expr2)
  | Cifthenelse (cond, ifso, ifnot) ->
    Cifthenelse (rewrite_expression cond, rewrite_expression ifso,
      rewrite_expression ifnot)
  | Cswitch (scrutinee, indexes, exprs, dbg) ->
    let scrutinee_id = Ident.create "scrutinee" in
    let exprs = Array.map rewrite_expression exprs in
    let expr_use_count = Array.make (Array.length exprs) 0 in
    Array.iter (fun index ->
        expr_use_count.(index) <- expr_use_count.(index) + 1)
      indexes;
    let continuations =
      Array.mapi (fun index use_count ->
          if use_count <= 1 then None
          else Some (Lambda.next_raise_count (), exprs.(index)))
        expr_use_count
    in
    let exprs' =
      Array.map2 (fun expr continuation : Cmm.expression ->
          match continuation with
          | None -> expr
          | Some (continuation, _handler) -> Cexit (continuation, []))
        exprs continuations
    in
    let cases = Array.mapi (fun num index -> num, exprs'.(index)) indexes in 
    let expanded = expand_switch ~scrutinee:scrutinee_id ~cases dbg in
    let initial_expr : Cmm.expression =
      Clet (scrutinee_id, scrutinee, expanded)
    in
    let new_expr =
      Array.fold_left (fun expr maybe_continuation : Cmm.expression ->
          match maybe_continuation with
          | None -> expr
          | Some (continuation, handler) ->
            Ccatch (Nonrecursive, [continuation, [], handler], expr))
        initial_expr
        continuations
    in
(*
    Format.fprintf Format.err_formatter "Rewrote:\n%a\nto:\n%a"
      Printcmm.expression expr
      Printcmm.expression new_expr;
*)
    new_expr
  | Cloop expr -> Cloop (rewrite_expression expr)
  | Ccatch (rec_flag, handlers, body) ->
    let handlers =
      List.map (fun (cont, params, handler) ->
          cont, params, rewrite_expression handler)
        handlers
    in
    Ccatch (rec_flag, handlers, rewrite_expression body)
  | Cexit (cont, args) ->
    Cexit (cont, List.map rewrite_expression args)
  | Ctrywith (body, arg, handler) ->
    Ctrywith (rewrite_expression body, arg, rewrite_expression handler)

let fundecl (fundecl : Cmm.fundecl) =
  if not !Clflags.mretpoline then fundecl
  else
    { fundecl with Cmm.
      fun_body = rewrite_expression fundecl.fun_body;
    }
