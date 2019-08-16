module Program_body = Flambda_static.Program_body
module Named = Flambda.Named
module E = Flambda.Expr

module C = struct
  type t = string
  let print ppf c = Format.pp_print_string ppf c
  let compare = String.compare
end
module CM = Map.Make(C)

module V = struct
  type t = string
  let print ppf c = Format.pp_print_string ppf c
  let compare = String.compare
end
module VM = Map.Make(V)

type env = {
  continuations : (Continuation.t * int) CM.t;
  variables : Variable.t VM.t;
}

let init_env = {
  continuations = CM.empty;
  variables = VM.empty;
}

let fresh_cont env ?sort (c, _loc) arity =
  let c' = Continuation.create ?sort () in
  c',
  { env with
    continuations = CM.add c (c', arity) env.continuations }

let fresh_var env (name, _loc) =
  let v = Variable.create name in
  v,
  { env with
    variables = VM.add name v env.variables }

let const (c:Fexpr.const) : Simple.Const.t =
  match c with
  | Tagged_immediate i ->
    let i = Targetint.of_string i in
    Tagged_immediate (Immediate.int (Targetint.OCaml.of_targetint i))
  | Naked_immediate i ->
    let i = Targetint.of_string i in
    Naked_immediate (Immediate.int (Targetint.OCaml.of_targetint i))

  (* | Naked_immediate of Immediate.t
   * | Naked_float of Numbers.Float_by_bit_pattern.t
   * | Naked_int32 of Int32.t
   * | Naked_int64 of Int64.t
   * | Naked_nativeint of Targetint.t *)
  | _ ->
    failwith "TODO const"

let simple env (s:Fexpr.simple) : Simple.t =
  match s with
  | Var (v, loc) -> begin
      match VM.find_opt v env.variables with
      | None ->
        Misc.fatal_errorf "Unbound variable %s : %a" v
          Location.print_loc loc
      | Some var -> Simple.var var
    end
  | Const c ->
    Simple.const (const c)
  | s ->
    Misc.fatal_errorf "TODO simple %a"
      Print_fexpr.simple s

let unop (unop:Fexpr.unop) : Flambda_primitive.unary_primitive =
  match unop with
  | Opaque_identity -> Opaque_identity

let defining_expr env (named:Fexpr.named) : Named.t =
  match named with
  | Simple s ->
    Named.create_simple (simple env s)
  | Prim (Unop (u, arg)) ->
    let prim : Flambda_primitive.t =
      Unary (unop u, simple env arg)
    in
    Named.create_prim prim Debuginfo.none
  | _ -> assert false

let rec expr env (e : Fexpr.expr) : E.t =
  match e with
  | Let { var = Some var; kind = _; defining_expr = d; body } ->
    let id, env = fresh_var env var in
    let named = defining_expr env d in
    let body = expr env body in
    let var =
      Var_in_binding_pos.create id Name_occurrence_kind.normal
    in
    E.create_let var named body

  | Apply_cont ((cont, _loc), None, args) ->
    let c, arity = CM.find cont env.continuations in
    if List.length args <> arity then
      Misc.fatal_errorf "wrong continuation arity %a" C.print cont;
    let args = List.map (simple env) args in
    let apply_cont = Flambda.Apply_cont.create c ~args in
    E.create_apply_cont apply_cont
  | _ ->
    failwith "TODO"

let rec conv_top ~backend func_env (prog : Fexpr.program) : Program_body.t =
  match prog with
  | [] -> assert false
  | Root (_, _loc) :: _ :: _ ->
    Misc.fatal_errorf "Root must be the last construction of the file"
  | [ Root (s, _loc) ] ->
    let module Backend = (val backend : Flambda2_backend_intf.S) in
    let symbol = Backend.symbol_for_global' (Ident.create_persistent s) in
    Program_body.root symbol
  | Define_symbol
      (Nonrecursive,
       { computation = Some c;
         static_structure = [ ] }) :: tail ->
    let cont_arity = List.length c.computed_values in
    let return_continuation, env = fresh_cont init_env ~sort:Return c.return_cont cont_arity in
    let exn_handler, env = fresh_cont ~sort:Exn env c.exception_cont 1 in
    let exn_continuation = Exn_continuation.create ~exn_handler ~extra_args:[] in
    let computation_expr = expr env c.expr in
    let computation : Program_body.Computation.t = {
      expr = computation_expr;
      return_continuation;
      exn_continuation;
      computed_values = [];
    } in
    let body = conv_top ~backend func_env tail in
    Program_body.define_symbol ~body {
      computation = Some computation;
      static_structure = S [];
    }
  | _ ->
    assert false

let conv ~backend fexpr : Flambda_static.Program.t =
  let body = conv_top ~backend () fexpr in
  { imported_symbols = Symbol.Map.empty;
    body; }
