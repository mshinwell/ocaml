(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open Int_replace_polymorphic_compare

module Env = Closure_conversion_aux.Env
module Expr = Flambda.Expr
module Function_decls = Closure_conversion_aux.Function_decls
module Function_decl = Function_decls.Function_decl
module Named = Flambda.Named
module Program_body = Flambda_static.Program_body
module Static_part = Flambda_static.Static_part

module K = Flambda_kind
module LC = Lambda_conversions
module P = Flambda_primitive
module T = Flambda_type

type t = {
  current_unit_id : Ident.t;
  symbol_for_global' : (Ident.t -> Symbol.t);
  filename : string;
  mutable imported_symbols : Symbol.Set.t;
  (* All symbols in [imported_symbols] are to be of kind [Value]. *)
  mutable declared_symbols : (Symbol.t * Static_part.t) list;
}

let symbol_for_ident t id =
  let symbol = t.symbol_for_global' id in
  t.imported_symbols <- Symbol.Set.add symbol t.imported_symbols;
  Simple.symbol symbol

(* Generate a wrapper ("stub") function that accepts a tuple argument and calls
   another function with arguments extracted in the obvious manner from the
   tuple. *)
let tupled_function_call_stub
      (original_params : (Variable.t * Lambda.value_kind) list)
      (unboxed_version : Closure_id.t)
      ~(closure_bound_var : Closure_id.t) =
  let dbg = Debuginfo.none in
  let continuation_param = Continuation.create () in
  let exn_continuation =
    Exn_continuation.create ~exn_handler:(Continuation.create ())
      ~extra_args:[]
  in
  let tuple_param_var =
    Variable.rename ~append:"tupled_stub_param"
      (Closure_id.unwrap unboxed_version)
  in
  let my_closure =
    Variable.rename ~append:"tupled_stub"
      (Closure_id.unwrap unboxed_version)
  in
  let params = List.map (fun (var, _) -> Variable.rename var) original_params in
  let unboxed_version_var = Variable.create "unboxed_version" in
  let call =
    let call_kind =
      Call_kind.direct_function_call unboxed_version ~return_arity:[K.value ()]
    in
    let apply =
      Flambda.Apply.create ~callee:(Name.var unboxed_version_var)
        ~continuation:continuation_param
        ~exn_continuation
        ~args:(Simple.vars params)
        ~call_kind
        ~dbg:Debuginfo.none
        ~inline:Default_inline
        ~specialise:Default_specialise
    in
    Expr.create_apply apply
  in
  let body_with_closure_bound =
    let move : Flambda_primitive.unary_primitive =
      Move_within_set_of_closures {
        move_from = closure_bound_var;
        move_to = unboxed_version;
      }
    in
    Expr.create_let unboxed_version_var
      (K.value ())
      (Named.create_prim (Unary (move, Simple.var my_closure)) dbg)
      call
  in
  let _, body =
    List.fold_left (fun (pos, body) param ->
        let defining_expr =
          let pos = Immediate.int (Targetint.OCaml.of_int pos) in
          Named.create_prim
            (Binary (
              Block_load (Block (Value Unknown), Immutable),
              Simple.var tuple_param_var,
              Simple.const (Tagged_immediate pos)))
            dbg
        in
        let expr = Expr.create_let param (K.value ()) defining_expr body in
        pos + 1, expr)
      (0, body_with_closure_bound)
      params
  in
  let tuple_param =
    Kinded_parameter.create (Parameter.wrap tuple_param_var) (K.value ())
  in
  let params_and_body =
    Flambda.Function_params_and_body.create [tuple_param]
      ~param_relations:T.Typing_env_extension.empty
      ~body
      ~my_closure
  in
  Flambda.Function_declaration.create
    ~closure_origin:(Closure_origin.create closure_bound_var)
    ~continuation_param
    ~exn_continuation
    ~params_and_body
    ~result_arity:[K.value ()]
    ~stub:true
    ~dbg
    ~inline:Default_inline
    ~specialise:Default_specialise
    ~is_a_functor:false

let register_const t (constant : Static_part.t) name
      : Flambda_static.Of_kind_value.t * string =
  let current_compilation_unit = Compilation_unit.get_current_exn () in
  (* Create a variable to ensure uniqueness of the symbol. *)
  let var = Variable.create ~current_compilation_unit name in
  let symbol = Flambda_utils.make_variable_symbol var in
  t.declared_symbols <- (symbol, constant) :: t.declared_symbols;
  Symbol symbol, name

let rec declare_const t (const : Lambda.structured_constant)
      : Flambda_static.Of_kind_value.t * string =
  match const with
  | Const_base (Const_int c) ->
    Tagged_immediate (Immediate.int (Targetint.OCaml.of_int c)), "int"
  | Const_pointer p ->
    (* CR mshinwell: This needs to be removed. *)
    Tagged_immediate (Immediate.int (Targetint.OCaml.of_int p)), "const_ptr"
  | Const_base (Const_char c) -> Tagged_immediate (Immediate.char c), "char"
  | Const_base (Const_string (s, _)) ->
    let const, name =
      (* CR mshinwell: Double-check this is the correct condition for
         everything in the application being compiled with safe-string *)
      if Config.safe_string then
        Static_part.Immutable_string (Const s), "immstring"
      else
        Static_part.Mutable_string { initial_value = Const s; }, "string"
    in
    register_const t const name
  | Const_base (Const_float c) ->
    let c = Numbers.Float_by_bit_pattern.create (float_of_string c) in
    register_const t (Boxed_float (Const c)) "float"
  | Const_base (Const_int32 c) ->
    register_const t (Boxed_int32 (Const c)) "int32"
  | Const_base (Const_int64 c) ->
    register_const t (Boxed_int64 (Const c)) "int64"
  | Const_base (Const_nativeint c) ->
    (* CR pchambart: this should be pushed further to lambda *)
    let c = Targetint.of_int64 (Int64.of_nativeint c) in
    register_const t (Boxed_nativeint (Const c)) "nativeint"
  | Const_immstring c ->
    register_const t (Immutable_string (Const c)) "immstring"
  | Const_float_array c ->
    (* CR mshinwell: check that Const_float_array is always immutable *)
    register_const t
      (Immutable_float_array
         (List.map (fun s ->
           let f = Numbers.Float_by_bit_pattern.create (float_of_string s) in
           Static_part.Const f) c))
      "float_array"
  | Const_block (tag, consts) ->
    let const : Static_part.t =
      Block
        (Tag.Scannable.create_exn tag, Immutable,
         List.map (fun c -> fst (declare_const t c)) consts)
    in
    register_const t const "const_block"

let close_const t (const : Lambda.structured_constant) =
  match declare_const t const with
  | Tagged_immediate c, name ->
    Named.create_simple (Simple.const (Tagged_immediate c)), name
  | Symbol s, name -> Named.create_simple (Simple.symbol s), name
  | Dynamically_computed _, name ->
    Misc.fatal_errorf "Declaring a computed constant %s" name

let close_c_call env ~let_bound_id (prim : Primitive.description) ~args
      exn_continuation dbg (k : Named.t -> Expr.t) : Expr.t =
  (* CR pchambart: there should be a special case if body is a
     apply_cont *)
  let return_continuation = Continuation.create () in
  let exn_continuation =
    Exn_continuation.create ~exn_handler:exn_continuation ~extra_args:[]
  in
  let param_arity =
    List.map LC.kind_of_primitive_native_repr prim.prim_native_repr_args
  in
  let return_arity =
    [LC.kind_of_primitive_native_repr prim.prim_native_repr_res]
  in
  let call_kind =
    Call_kind.c_call ~alloc:prim.prim_alloc ~param_arity ~return_arity
  in
  let call_symbol =
    Symbol.create (Compilation_unit.external_symbols ())
      (Linkage_name.create prim.prim_name)
  in
  let call args =
    let apply =
      Flambda.Apply.create ~callee:(Name.symbol call_symbol)
        ~continuation:return_continuation
        ~exn_continuation
        ~args
        ~call_kind
        ~dbg
        ~inline:Default_inline
        ~specialise:Default_specialise
    in
    Flambda.Expr.create_apply apply
  in
  let call =
    List.fold_right2 (fun arg (arg_repr : Primitive.native_repr)
            (call : Simple.t list -> Expr.t) ->
        let unbox_arg : P.unary_primitive option =
          match arg_repr with
          | Same_as_ocaml_repr -> None
          | Unboxed_float -> Some (P.Unbox_number Naked_float)
          | Unboxed_integer Pnativeint -> Some (P.Unbox_number Naked_nativeint)
          | Unboxed_integer Pint32 -> Some (P.Unbox_number Naked_int32)
          | Unboxed_integer Pint64 -> Some (P.Unbox_number Naked_int64)
          | Untagged_int ->
            Some (P.Num_conv { src = Tagged_immediate; dst = Naked_nativeint; })
        in
        match unbox_arg with
        | None -> (fun args -> call (arg :: args))
        | Some named ->
          (fun args ->
             let unboxed_arg = Variable.create "unboxed" in
             Expr.create_let unboxed_arg
               (LC.kind_of_primitive_native_repr arg_repr)
               (Named.create_prim (Unary (named, arg)) dbg)
               (call ((Simple.var unboxed_arg) :: args))))
      (Env.find_simples env args)
      prim.prim_native_repr_args
      call []
  in
  let code_after_call, handler_param =
    let box_return_value =
      match prim.prim_native_repr_res with
      | Same_as_ocaml_repr -> None
      | Unboxed_float -> Some (P.Box_number Naked_float)
      | Unboxed_integer Pnativeint -> Some (P.Box_number Naked_nativeint)
      | Unboxed_integer Pint32 -> Some (P.Box_number Naked_int32)
      | Unboxed_integer Pint64 -> Some (P.Box_number Naked_int64)
      | Untagged_int ->
        Some (P.Num_conv { src = Naked_nativeint; dst = Tagged_immediate; })
    in
    match box_return_value with
    | None ->
      let body_env, handler_param = Env.add_var_like env let_bound_id in
      let body = close t body_env body in
      body, handler_param
    | Some box_return_value ->
      let handler_param = Variable.create (prim.prim_name ^ "_return") in
      let body_env, boxed_var = Env.add_var_like env let_bound_id in
      let body = close t body_env body in
      Expr.create_let boxed_var
        (K.value ())
        (Prim (Unary (box_return_value, Simple.var handler_param), dbg)) body,
        handler_param
  in
  let after_call =
    let params =
      [Kinded_parameter.create (Parameter.wrap handler_param) return_kind]
    in
    let params_and_handler =
      Flambda.Continuation_params_and_handler.create params
        ~param_relations:T.Typing_env_extension.empty
        ~handler:code_after_call
    in
    Flambda.Continuation_handler.create ~params_and_handler
      ~inferred_typing:T.Parameters.empty
      ~stub:false
      ~is_exn_handler:false
  in
  let let_cont =
    Flambda.Let_cont.create_non_recursive return_continuation after_call
      ~body:call
  in
  Expr.create_let_cont let_cont

let close_primitive env ~let_bound_id prim ~args loc exn_continuation
      (k : Named.t -> Expr.t) : Expr.t =
  let exn_continuation =
    match exn_continuation with
    | None -> None
    | Some { exn_continuation = exn_handler; extra_args; } ->
      let extra_args = List.map (fun id -> Simple.var (Env.find_var env id)) in
      Some (Exn_continuation.create ~exn_handler ~extra_args)
  in
  let args = Env.find_simples env args in
  let dbg = Debuginfo.from_location loc in
  match prim, args with
  | Pccall prim, args ->
    close_c_call env ~let_bound_id prim ~args exn_continuation dbg k
  | Pgetglobal id, [] ->
    let is_predef_exn = Ident.is_predef_exn id in
    if not (is_predef_exn || not (Ident.same id t.current_unit_id))
    then begin
      Misc.fatal_errorf "Non-predef Pgetglobal %a in the same unit"
        Ident.print id
    end;
    k (Simple (symbol_for_ident t id))
  | Praise raise_kind, [_] ->
    let exn_continuation =
      match exn_continuation with
      | None ->
        Misc.fatal_errorf "Praise is missing exception continuation: %a"
          Ilambda.print_named named
      | Some exn_continuation -> exn_continuation
    in
    let apply_cont =
      Flambda.Apply_cont.create ?trap_action:None exn_continuation ~args
    in
    k (Apply_cont apply_cont)
  | prim, args ->
    Lambda_to_flambda_primitives.convert_and_bind prim ~args
      exn_continuation dbg k

let rec close t env (ilam : Ilambda.t) : Expr.t =
  match ilam with
  | Let (id, kind, defining_expr, body) ->
    let body_env, var = Env.add_var_like env id in
    let cont (defining_expr : Named.t) =
      (* CR pchambart: Not tail ! *)
      let body = close t body_env body in
      Expr.create_let var kind defining_expr body
    in
    close_named t env ~let_bound_id:id defining_expr cont
  | Let_rec (defs, body) -> close_let_rec env ~defs ~body
  | Let_cont { name; administrative; is_exn_handler; params; recursive; body;
      handler; } ->
    if is_exn_handler then begin
      if administrative then begin
        Misc.fatal_errorf "[Let_cont]s marked as exception handlers cannot \
            also be marked as administrative redexes: %a"
          Ilambda.print ilam
      end;
      if List.length params <> 1 then begin
        Misc.fatal_errorf "[Let_cont]s marked as exception handlers must \
            have exactly one parameter: %a"
          Ilambda.print ilam
      end;
      match recursive with
      | Nonrecursive -> ()
      | Recursive ->
        Misc.fatal_errorf "[Let_cont]s marked as exception handlers must \
            be [Nonrecursive]: %a"
          Ilambda.print ilam
    end;
    if administrative then begin
      (* Inline out administrative redexes. *)
      assert (recursive = Asttypes.Nonrecursive);
      let body_env =
        Env.add_administrative_redex env name ~params:params
          ~handler:handler
      in
      close t body_env body
    end else begin
      let handler_env, params = Env.add_vars_like env params in
      let params =
        List.map (fun (param, kind) ->
            Kinded_parameter.create (Parameter.wrap param)
              (flambda_type_of_lambda_value_kind kind))
          params
      in
      let handler = close t handler_env handler in
      let params_and_handler =
        Flambda.Continuation_params_and_handler.create params
          ~param_relations:T.Typing_env_extension.empty
          ~handler
      in
      let handler =
        Flambda.Continuation_handler.create ~params_and_handler
          ~inferred_typing:T.Parameters.empty
          ~stub:false
          ~is_exn_handler:is_exn_handler
      in
      let body = close t env body in
      let let_cont =
        match recursive with
        | Nonrecursive ->
          Flambda.create_non_recursive name handler ~body
        | Recursive ->
          let handlers = Continuation.Map.singleton name handler in
          Flambda.create_recursive handlers ~body
      in
      Expr.create_let_cont let_cont
    end
  | Apply { kind; func; args; continuation; exn_continuation;
      loc; should_be_tailcall = _; inlined; specialised; } ->
    let call_kind =
      match kind with
      | Function -> Call_kind.indirect_function_call_unknown_arity ()
      | Method { kind; obj; } ->
        Call_kind.method_call (LC.method_kind kind) ~obj:(Env.find_name env obj)
    in
    let apply =
      Flambda.Apply.create ~callee:(Env.find_name env func)
        ~continuation
        ~exn_continuation
        ~args:(Env.find_simples env args)
        ~call_kind
        ~dbg:(Debuginfo.from_location loc)
        ~inline:(LC.inline_attribute inlined)
        ~specialise:(LC.specialise_attribute specialised)
    in
    Expr.create_apply apply
  | Apply_cont (cont, trap_action, args) ->
    let args = Env.find_vars env args in
    begin match Env.find_administrative_redex env cont with
    | Some (params, handler) when Option.is_none trap_action ->
      let handler_env = Env.add_vars env params args in
      close t handler_env handler
    | _ ->
      let trap_action =
        Option.map (fun (trap_action : Ilambda.trap_action) : Trap_action.t ->
            match trap_action with
            | Push { exn_handler; } -> Push { exn_handler; }
            | Pop { exn_handler; } ->
              Pop { exn_handler; take_backtrace = false; })
          trap_action
      in
      let args = Simple.vars args in
      Apply_cont (cont, trap_action, args)
    end
  | Switch (scrutinee, sw) ->
    let module D = Discriminant in
    let arms = List.map (fun (case, arm) -> D.of_int_exn case, arm) sw.consts in
    let arms =
      match sw.failaction with
      | None -> D.Map.of_list arms
      | Some default ->
        Numbers.Int.Set.fold (fun case cases ->
            let case = D.of_int_exn case in
            if D.Map.mem case cases then cases
            else D.Map.add case default cases)
          (Numbers.Int.zero_to_n (sw.numconsts - 1))
          (D.Map.of_list arms)
    in
    let scrutinee = Env.find_name env scrutinee in
    Expr.create_switch ~scrutinee ~arms

and close_named t env ~let_bound_id (named : Ilambda.named)
      (k : Named.t -> Expr.t) : Expr.t =
  match named with
  | Var id ->
    let simple =
      if not (Ident.is_predef_exn id) then Simple.var (Env.find_var env id)
      else symbol_for_ident t id
    in
    k (Simple simple)
  | Const cst ->
    let named, _name = close_const t cst in
    k named
  | Prim { prim; args; loc; exn_continuation; } ->
    close_primitive env ~let_bound_id prim ~args loc exn_continuation k

and close_let_rec env ~defs ~body =
  let env =
    List.fold_right (fun (id, _) env ->
        let env, _var = Env.add_var_like env id in
        env)
      defs env
  in
  let function_declarations =
    List.map (function (let_rec_ident,
            ({ kind; continuation_param; exn_continuation_param;
               params; body; attr; loc; stub;
             } : Ilambda.function_declaration)) ->
        let closure_bound_var =
          Closure_id.wrap
            (Variable.create_with_same_name_as_ident let_rec_ident)
        in
        let free_idents_of_body = Lambda.free_variables body in
        let function_declaration =
          Function_decl.create ~let_rec_ident:(Some let_rec_ident)
            ~closure_bound_var ~kind ~params ~continuation_param
            ~exn_continuation_param ~body
            ~attr ~loc ~free_idents_of_body ~stub
        in
        function_declaration)
      defs
  in
  let name =
    (* The Microsoft assembler has a 247-character limit on symbol
       names, so we keep them shorter to try not to hit this. *)
    (* CR-soon mshinwell: We should work out how to shorten symbol names
       anyway, to help avoid enormous ELF string tables. *)
    if Sys.win32 then begin
      match defs with
      | (id, _)::_ -> (Ident.unique_name id) ^ "_let_rec"
      | _ -> "let_rec"
    end else begin
      String.concat "_and_"
        (List.map (fun (id, _) -> Ident.unique_name id) defs)
    end
  in
  let set_of_closures_var = Variable.create name in
  let set_of_closures =
    close_functions t env (Function_decls.create function_declarations)
  in
  let body =
    let set_of_closures_var = Simple.var set_of_closures_var in
    List.fold_left (fun body decl ->
        let let_rec_ident = Function_decl.let_rec_ident decl in
        let closure_bound_var = Function_decl.closure_bound_var decl in
        let let_bound_var = Env.find_var env let_rec_ident in
        let project_closure : Flambda_primitive.t =
          Unary (Project_closure closure_bound_var, set_of_closures_var)
        in
        Expr.create_let let_bound_var
          (K.value ()) (Prim (project_closure, Debuginfo.none))
          body)
      (close t env body)
      function_declarations
  in
  Expr.create_let set_of_closures_var (K.fabricated ())
    set_of_closures body

and close_functions t external_env function_declarations : Named.t =
  let all_free_idents =
    (* Filter out predefined exception identifiers, since they will be
       turned into symbols when we closure-convert the body. *)
    Ident.Set.filter (fun ident ->
        not (Ident.is_predef_exn ident))
      (Function_decls.all_free_idents function_declarations)
  in
  let vars_within_closure_from_ident =
    Ident.Set.fold (fun id map ->
        let var = Variable.create_with_same_name_as_ident id in
        Ident.Map.add id (Var_within_closure.wrap var) map)
      all_free_idents
      Ident.Map.empty
  in
  let closure_ids_from_ident =
    List.fold_left (fun map decl ->
        let id = Function_decl.let_rec_ident decl in
        let closure_id = Function_decl.closure_bound_var decl in
        Ident.Map.add id closure_id map)
      Ident.Map.empty
      (Function_decls.to_list function_declarations)
  in
  let funs =
    List.fold_left close_one_function
      Closure_id.Map.empty
      (Function_decls.to_list function_declarations)
  in
  let function_decls = Flambda.Function_declarations.create ~funs in
  let closure_elements =
    Ident.Map.fold (fun id var_within_closure map ->
        let external_var = Simple.var (Env.find_var external_env id) in
        Var_within_closure.Map.add var_within_closure external_var map)
      vars_within_closure_from_ident
      Var_within_closure.Map.empty
  in
  let set_of_closures =
    Flambda.Set_of_closures.create ~function_decls
      ~set_of_closures_ty:(T.any_value ())
      ~closure_elements
      ~direct_call_surrogates:Closure_id.Map.empty
  in
  Set_of_closures set_of_closures

and close_one_function map decl =
  let body = Function_decl.body decl in
  let loc = Function_decl.loc decl in
  let dbg = Debuginfo.from_location loc in
  let params = Function_decl.params decl in
  let my_closure = Variable.create "my_closure" in
  let closure_bound_var = Function_decl.closure_bound_var decl in
  let unboxed_version =
    Closure_id.wrap (Variable.create (
      Ident.name (Function_decl.let_rec_ident decl)))
  in
  let my_closure_id =
    match Function_decl.kind decl with
    | Curried -> closure_bound_var
    | Tupled -> unboxed_version
  in
  (* The free variables are:
     - The parameters: direct substitution by [Variable]s
     - The function being defined: accessible through [my_closure]
     - Other functions in the set being defined: accessible from [my_closure]
       then a [Move_within_set_of_closures]
     - Other free variables: accessible using [Project_var] from
       [my_closure].
     Note that free variables corresponding to predefined exception
     identifiers have been filtered out by [close_functions], above.
  *)
  let var_within_closure_to_bind, var_for_ident_within_closure =
    Ident.Map.fold (fun id var_within_closure (to_bind, var_for_ident) ->
        let var = Variable.create_with_same_name_as_ident id in
        Variable.Map.add var var_within_closure to_bind,
        Ident.Map.add id var var_for_ident)
      vars_within_closure_from_ident
      (Variable.Map.empty, Ident.Map.empty)
  in
  let project_closure_to_bind, var_for_project_closure =
    List.fold_left (fun (to_bind, var_for_ident) function_decl ->
        let let_rec_ident = Function_decl.let_rec_ident function_decl in
        let to_bind, var =
          if Ident.same var_for_ident let_rec_ident then
            to_bind, my_closure  (* my_closure is already bound *)
          else
            let variable =
              Variable.create_with_same_name_as_ident let_rec_ident
            in
            let closure_id =
              Ident.Map.find let_rec_ident closure_ids_from_ident
            in
            Variable.Map.add variable closure_id to_bind, variable
        in
        to_bind,
        Ident.Map.add let_rec_ident var var_for_ident)
      (Variable.Map.empty, Ident.Map.empty)
      (Function_decls.to_list function_declarations)
  in
  let closure_env_without_parameters =
    let empty_env = Env.clear_local_bindings external_env in
    Env.add_var_map (Env.add_var_map empty_env var_for_ident_within_closure)
      var_for_project_closure
  in
  let closure_env =
    List.fold_right (fun (id, _) env ->
        let env, _var = Env.add_var_like env id in
        env)
      params
      closure_env_without_parameters
  in
  (* CR-someday pchambart: eta-expansion wrappers for primitives are
     not marked as stubs but certainly should be. *)
  let stub = Function_decl.stub decl in
  let param_vars =
    List.map (fun (id, kind) -> Env.find_var closure_env id, kind) params
  in
  let params =
    List.map (fun (var, kind) ->
        Flambda.Typed_parameter.create (Parameter.wrap var)
          (LC.flambda_type_of_lambda_value_kind t))
      param_vars
  in
  let body = close t closure_env body in
  let free_vars_of_body = Name_occurrences.variables (Expr.free_names body) in
  let my_closure' = Simple.var my_closure in
  let body =
    Variable.Map.fold (fun var closure_id body ->
        if not (Variable.Set.mem var free_vars_of_body) then body
        else
          let move : Flambda.unary_primitive =
            Move_within_set_of_closures {
              move_from = my_closure_id;
              move_to = closure_id;
            }
          in
          Expr.create_let var (K.value ())
            (Named.create_prim (Unary (move, my_closure')) Debuginfo.none)
            body)
      project_closure_to_bind
      body
  in
  let body =
    Variable.Map.fold (fun var var_within_closure body ->
        if not (Variable.Set.mem var free_vars_of_body) then body
        else
          Expr.create_let var
            (K.value ())
            (Named.create_prim
              (Unary (Project_var var_within_closure, my_closure'))
              Debuginfo.none)
            body)
      var_within_closure_to_bind
      body
  in
  let fun_decl =
    let closure_origin = Closure_origin.create my_closure_id in
    let inline = LC.inline_attribute (Function_decl.inline decl) in
    let specialise = LC.specialise_attribute (Function_decl.specialise decl) in
    let params_and_body =
      Flambda.Function_params_and_body.create params
        ~param_relations:T.Typing_env_extension.empty
        ~body
        ~my_closure
    in
    Flambda.Function_declaration.create
      ~closure_origin:(Closure_origin.create closure_bound_var)
      ~continuation_param:(Function_decl.continuation_param decl)
      ~exn_continuation_param:(Function_decl.exn_continuation_param decl)
      ~params_and_body
      ~result_arity:[K.value ()]
      ~stub
      ~dbg
      ~inline
      ~specialise
      ~is_a_functor:(Function_decl.is_a_functor decl)
  in
  match Function_decl.kind decl with
  | Curried -> Closure_id.Map.add my_closure_id fun_decl map
  | Tupled ->
    let generic_function_stub =
      tupled_function_call_stub param_vars unboxed_version ~closure_bound_var
    in
    Closure_id.Map.add unboxed_version fun_decl
      (Closure_id.Map.add closure_bound_var generic_function_stub map)

let ilambda_to_flambda ~backend ~module_ident ~size ~filename
      (ilam : Ilambda.program): Flambda_static.Program.t =
  let module Backend = (val backend : Backend_intf.S) in
  let compilation_unit = Compilation_unit.get_current_exn () in
  let t =
    { current_unit_id = Compilation_unit.get_persistent_ident compilation_unit;
      symbol_for_global' = Backend.symbol_for_global';
      filename;
      imported_symbols = Symbol.Set.empty;
      declared_symbols = [];
    }
  in
  let module_symbol = Backend.symbol_for_global' module_ident in
  let module_block_tag = Tag.Scannable.zero in
  let module_block_var = Variable.create "module_block" in
  let return_cont = Continuation.create () in
  let field_vars =
    List.init size (fun pos ->
      let pos_str = string_of_int pos in
      Variable.create ("block_field_" ^ pos_str), K.value ())
  in
  (* For review, skip down to "let expr =" below, read the comment then
     come back here. *)
  let load_fields_body =
    let field_vars =
      List.init size (fun pos ->
        let pos_str = string_of_int pos in
        pos, Variable.create ("block_field_" ^ pos_str))
    in
    let body : Expr.t =
      let fields = List.map (fun (_, var) -> Simple.var var) field_vars in
      let apply_cont = Flambda.Apply_cont.create return_cont fields in
      Flambda.Expr.create_apply_cont apply_cont
    in
    List.fold_left (fun body (pos, var) ->
        let pos = Immediate.int (Targetint.OCaml.of_int pos) in
        Expr.create_let var (K.value ())
          (Named.create_prim
            (Binary (
              Block_load (Block (Value Unknown), Immutable),
              Simple.var module_block_var,
              Simple.const (Tagged_immediate pos)))
            Debuginfo.none)
          body)
      body field_vars
  in
  let load_fields_cont_handler =
    let param =
      Kinded_parameter.create (Parameter.wrap module_block_var) (K.value ())
    in
    let params_and_handler =
      Flambda.Continuation_params_and_handler.create [param]
        ~param_relations:T.Typing_env_extension.empty
        ~handler:load_fields_body;
    in
    Flambda.Continuation_handler.create ~params_and_handler
      ~inferred_typing:T.Parameters.empty
      ~stub:true
      ~is_exn_handler:false
  in
  let expr =
    (* This binds the return continuation that is free (or, at least, not bound)
       in the incoming Ilambda code. The handler for the continuation receives a
       tuple with fields indexed from zero to [size]. The handler extracts the
       fields; the variables bound to such fields are returned as the
       [computed_values], below. The compilation of the [Define_symbol]
       constructions below then causes the actual module block to be created. *)
    let body = close t Env.empty ilam.expr in
    Flambda.Let_cont.create_non_recursive ilam.return_continuation
      load_fields_cont_handler
      ~body
  in
  let computation : Program_body.computation =
    { expr;
      return_cont;
      exception_cont = ilam.exception_continuation;
      computed_values = field_vars;
    }
  in
  let static_part : Static_part.t =
    let field_vars =
      List.map (fun (var, _) : Flambda_static.Of_kind_value.t ->
          Dynamically_computed var)
        field_vars
    in
    Block (module_block_tag, Immutable, field_vars)
  in
  let program_body : Program_body.t =
    let bound_symbols : Program_body.bound_symbols =
      Singleton (module_symbol, K.value ())
    in
    let definition : Program_body.definition =
      { computation = Some computation;
        static_structure = [bound_symbols, static_part];
      }
    in
    Define_symbol (definition, Root module_symbol)
  in
  let program_body =
    (* CR mshinwell: Share with [Simplify_program] *)
    List.fold_left (fun program_body (symbol, static_part) : Program_body.t ->
        let bound_symbols : Program_body.bound_symbols =
          Singleton (symbol, K.value ())
        in
        let static_structure = [bound_symbols, static_part] in
        let definition : Program_body.definition =
          { computation = None;
            static_structure;
          }
        in
        Define_symbol (definition, program_body))
      program_body
      t.declared_symbols
  in
  let imported_symbols =
    Symbol.Set.fold (fun symbol imported_symbols ->
        Symbol.Map.add symbol (K.value ()) imported_symbols)
      t.imported_symbols
      Symbol.Map.empty
  in
  { imported_symbols;
    body = program_body;
  }
