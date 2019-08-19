module Program_body = Flambda_static.Program_body
module Named = Flambda.Named
module E = Flambda.Expr
module I = Flambda_kind.Standard_int
module P = Flambda_primitive

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

let find_cont env (c, loc) =
  match CM.find_opt c env.continuations with
  | None ->
    Misc.fatal_errorf "Unbound continuation %s: %a"
      c Location.print_loc loc
  | Some c ->
    c

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

let binop (unop:Fexpr.binop) : Flambda_primitive.binary_primitive =
  match unop with
  | Plus -> Int_arith (I.Tagged_immediate, Add)
  | Minus -> Int_arith (I.Tagged_immediate, Sub)
  | Plusdot
  | Minusdot -> failwith "TODO binop"

let convert_mutable_flag (flag : Fexpr.mutable_or_immutable)
      : P.mutable_or_immutable =
  match flag with
  | Mutable -> Mutable
  | Immutable -> Immutable

let convert_block_shape ~num_fields =
  List.init num_fields (fun _field : P.Value_kind.t -> Anything)

let defining_expr env (named:Fexpr.named) : Named.t =
  match named with
  | Simple s ->
    Named.create_simple (simple env s)
  | Prim (Unop (u, arg)) ->
    let prim : Flambda_primitive.t =
      Unary (unop u, simple env arg)
    in
    Named.create_prim prim Debuginfo.none
  | Prim (Binop (b, a1, a2)) ->
    let prim : Flambda_primitive.t =
      Binary (binop b, simple env a1, simple env a2)
    in
    Named.create_prim prim Debuginfo.none
  | Prim (Block (tag, mutability, args)) ->
    let mutability = convert_mutable_flag mutability in
    let shape = convert_block_shape ~num_fields:(List.length args) in
    let kind : P.make_block_kind =
      Full_of_values (Tag.Scannable.create_exn tag, shape)
    in
    let prim : P.t =
      P.Variadic (
        Make_block (kind, mutability),
        List.map (simple env) args
      )
    in
    Named.create_prim prim Debuginfo.none
  | _ -> assert false

let value_kind _ = Flambda_kind.value

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

  | Let_cont
      { recursive; body;
        handlers = [handler] } -> begin
      let is_exn_handler = false in
      let name, body_env =
        fresh_cont env handler.name (List.length handler.params)
      in
      let body = expr body_env body in
      let env =
        match recursive with
        | Nonrecursive -> env
        | Recursive -> body_env
      in
      let handler_env, params =
        List.fold_right
          (fun ({ param; ty }:Fexpr.typed_parameter)
            (env, args) ->
            let var, env = fresh_var env param in
            let user_visible = true in
            let param = Variable.with_user_visible var ~user_visible in
            let param = Kinded_parameter.create (Parameter.wrap param) (value_kind ty) in
            env, param :: args)
          handler.params (env, [])
      in
      let handler =
        expr handler_env handler.handler
      in
      let params_and_handler =
        Flambda.Continuation_params_and_handler.create params ~handler
      in
      let handler =
        Flambda.Continuation_handler.create ~params_and_handler
          ~stub:false
          ~is_exn_handler:is_exn_handler
      in
      match recursive with
      | Nonrecursive ->
        Flambda.Let_cont.create_non_recursive name handler ~body
      | Recursive ->
        let handlers = Continuation.Map.singleton name handler in
        Flambda.Let_cont.create_recursive handlers ~body
    end

  | Apply_cont ((cont, _loc) as cont', None, args) ->
    let c, arity = find_cont env cont' in
    if List.length args <> arity then
      Misc.fatal_errorf "wrong continuation arity %a" C.print cont;
    let args = List.map (simple env) args in
    let apply_cont = Flambda.Apply_cont.create c ~args in
    E.create_apply_cont apply_cont

  | Switch { scrutinee; sort; cases } ->
    let sort : Flambda.Switch.Sort.t =
      match sort with
      | Int -> Int
      | Is_int -> Is_int
      | Tag { tags_to_sizes } ->
        Tag {
          tags_to_sizes =
            List.fold_left (fun acc (tag, size) ->
              Tag.Scannable.Map.add
                (Tag.Scannable.create_exn tag)
                (Targetint.OCaml.of_int size) acc)
              Tag.Scannable.Map.empty tags_to_sizes;
        }
    in
    let arms =
      let module D = Discriminant in
      let sort : D.Sort.t =
        match sort with
        | Int -> Int
        | Tag _ -> Tag
        | Is_int -> Is_int
      in
      D.Map.of_list
        (List.map (fun (case, (arm, _loc)) ->
           let c, arity = CM.find arm env.continuations in
           assert(arity = 0);
           D.of_int_exn sort case, c)
           cases)
    in
    Flambda.Expr.create_switch sort
      ~scrutinee:(simple env scrutinee)
      ~arms
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
