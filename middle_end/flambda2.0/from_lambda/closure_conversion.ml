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
module Function_decls = Closure_conversion_aux.Function_decls
module Function_decl = Function_decls.Function_decl
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
  mutable declared_symbols :
    (Symbol.t * Flambda_static0.Static_part.t) list;
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
      ~(closure_bound_var : Closure_id.t)
      : Flambda.Function_declaration.t =
  let continuation_param = Continuation.create () in
  let exn_continuation_param = Continuation.create () in
  let tuple_param_var =
    Variable.rename ~append:"tupled_stub_param"
      (Closure_id.unwrap unboxed_version)
  in
  let my_closure =
    Variable.rename ~append:"tupled_stub"
      (Closure_id.unwrap unboxed_version)
  in
  let params = List.map (fun (p, _) -> Variable.rename p) original_params in
  let unboxed_version_var =
    Variable.create "unboxed_version"
  in
  let call : Flambda.Expr.t =
    Apply ({
      continuation = continuation_param;
      exn_continuation = exn_continuation_param;
      func = Name.var unboxed_version_var;
      args = Simple.vars params;
      (* CR-someday mshinwell for mshinwell: investigate if there is some
         redundancy here (func is also unboxed_version) *)
      call_kind = Function (Direct {
        closure_id = unboxed_version;
        return_arity = [K.value ()];
      });
      dbg = Debuginfo.none;
      inline = Default_inline;
      specialise = Default_specialise;
    })
  in
  let body_with_closure_bound =
    let move =
      P.Move_within_set_of_closures {
        move_from = closure_bound_var;
        move_to = unboxed_version;
      }
    in
    Flambda.Expr.create_let unboxed_version_var
      (K.value ())
      (Prim (Unary (move, Simple.var my_closure), Debuginfo.none))
      call
  in
  let _, body =
    List.fold_left (fun (pos, body) param ->
        let lam : Flambda.Named.t =
          let pos = Immediate.int (Targetint.OCaml.of_int pos) in
          Prim (Binary (Block_load (Block (Value Unknown), Immutable),
                        Simple.var tuple_param_var,
                        Simple.const (Tagged_immediate pos)),
                Debuginfo.none)
        in
        pos + 1,
        Flambda.Expr.create_let param (K.value ()) lam body)
      (0, body_with_closure_bound) params
  in
  let tuple_param =
    (* We do not have an accessor here *)

    (* Flambda.Typed_parameter.create (Parameter.wrap tuple_param_var) *)
    (*   (Flambda_type.block Tag.Scannable.zero *)
    (*     (Array.of_list *)
    (*       (List.map (fun _ -> Flambda_type.any_value Must_scan Other) params))) *)

    Flambda.Typed_parameter.create (Parameter.wrap tuple_param_var)
      (T.any_value ())
  in
  Flambda.Function_declaration.create
    ~my_closure
    ~params:[tuple_param] ~continuation_param
    ~exn_continuation_param
    ~return_arity:[K.value ()]
    ~body ~stub:true ~dbg:Debuginfo.none ~inline:Default_inline
    ~specialise:Default_specialise ~is_a_functor:false
    ~closure_origin:(Closure_origin.create closure_bound_var)

let register_const t (constant : Static_part.t) name
      : Flambda_static0.Of_kind_value.t * string =
  let current_compilation_unit = Compilation_unit.get_current_exn () in
  (* Create a variable to ensure uniqueness of the symbol. *)
  let var = Variable.create ~current_compilation_unit name in
  let symbol = Flambda_utils.make_variable_symbol var in
  t.declared_symbols <- (symbol, constant) :: t.declared_symbols;
  Symbol symbol, name

let rec declare_const t (const : Lambda.structured_constant)
      : Flambda_static0.Of_kind_value.t * string =
  match const with
  | Const_base (Const_int c) ->
    Tagged_immediate (Immediate.int (Targetint.OCaml.of_int c)), "int"
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
    register_const t (Static_part.Boxed_float (Const c)) "float"
  | Const_base (Const_int32 c) ->
    register_const t (Static_part.Boxed_int32 (Const c)) "int32"
  | Const_base (Const_int64 c) ->
    register_const t (Static_part.Boxed_int64 (Const c)) "int64"
  | Const_base (Const_nativeint c) ->
    (* CR pchambart: this should be pushed further to lambda *)
    let c = Targetint.of_int64 (Int64.of_nativeint c) in
    register_const t (Static_part.Boxed_nativeint (Const c)) "nativeint"
  | Const_immstring c ->
    register_const t (Static_part.Immutable_string (Const c)) "immstring"
  | Const_float_array c ->
    (* CR mshinwell: check that Const_float_array is always immutable *)
    register_const t
      (Static_part.Immutable_float_array
         (List.map (fun s ->
           let f = float_of_string s in
           let f = Numbers.Float_by_bit_pattern.create f in
           Static_part.Const f) c))
      "float_array"
  | Const_block (tag, consts) ->
    let const : Static_part.t =
      Block
        (Tag.Scannable.create_exn tag, Immutable,
         List.map (fun c -> fst (declare_const t c)) consts)
    in
    register_const t const "const_block"

let close_const t (const : Lambda.structured_constant)
      : Flambda.Named.t * string =
  match declare_const t const with
  | Tagged_immediate c, name -> Simple (Simple.const (Tagged_immediate c)), name
  | Symbol s, name -> Simple (Simple.symbol s), name
  | Dynamically_computed _, name ->
    Misc.fatal_errorf "Declaring a computed constant %s" name

let close_c_call env prim ~args exn_continuation dbg
      (k : Flambda.Named.t -> Flambda.Expr.t) : Flambda.Expr.t =
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
  let dbg = Debuginfo.from_location loc in
  let call args =
    let apply =
      Flambda.Apply.create ~callee:(Name.symbol call_symbol)
        ~continuation
        ~exn_continuation
        ~args
        ~call_kind
        ~dbg
        ~inline:Default_inline
        ~specialise:Default_specialise
    in
    Flambda.create_apply apply
  in
  let call =
    List.fold_right2 (fun arg (arg_repr : Primitive.native_repr)
            (call : Simple.t list -> Flambda.Expr.t) ->
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
             let unboxed_arg = Simple.var (Variable.create "unboxed") in
             Flambda.Expr.create_let unboxed_arg
               (kind_of_repr arg_repr) (Prim (Unary (named, arg), dbg))
               (call (unboxed_arg :: args))))
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
      let body_env, handler_param = Env.add_var_like env id in
      let body = close t body_env body in
      body, handler_param
    | Some box_return_value ->
      let handler_param = Variable.create (prim.prim_name ^ "_return") in
      let body_env, boxed_var = Env.add_var_like env id in
      let body = close t body_env body in
      Flambda.Expr.create_let boxed_var
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
        ~param_relations:Flambda_type.Typing_env_extension.empty
        ~handler:code_after_call
    in
    Flambda.Continuation_handler.create ~params_and_handler
      ~inferred_typing:Flambda_type.Parameters.empty
      ~stub:false
      ~is_exn_handler:false
  in
  let let_cont =
    Flambda.Let_cont.create_non_recursive return_continuation after_call
      ~body:call
  in
  Flambda.Expr.create_let_cont let_cont

let close_primitive env prim ~args loc exn_continuation
      (k : Flambda.Named.t -> Flambda.Expr.t) : Flambda.Expr.t =
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
  | Pccall prim, args -> close_c_call env prim ~args exn_continuation dbg k
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

let rec close t env (ilam : Ilambda.t) : Flambda.Expr.t =
  match ilam with
  | Let (id, kind, defining_expr, body) ->
    let body_env, var = Env.add_var_like env id in
    let cont (defining_expr : Flambda.Named.t) =
      (* CR pchambart: Not tail ! *)
      let body = close t body_env body in
      Flambda.Expr.create_let var kind defining_expr body
    in
    close_named t env defining_expr cont
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
          ~param_relations:Flambda_type.Typing_env_extension.empty
          ~handler
      in
      let handler =
        Flambda.Continuation_handler.create ~params_and_handler
          ~inferred_typing:Flambda_type.Parameters.empty
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
      Flambda.Expr.create_let_cont let_cont
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
    Flambda.Expr.create_apply apply
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
    Flambda.Expr.create_switch ~scrutinee ~arms

and close_named t env (named : Ilambda.named)
      (k : Flambda.Named.t -> Flambda.Expr.t) : Flambda.Expr.t =
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
    close_prim env prim ~args loc exn_continuation k

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
               free_idents_of_body; } : Ilambda.function_declaration)) ->
        let closure_bound_var =
          Closure_id.wrap
            (Variable.create_with_same_name_as_ident let_rec_ident)
        in
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
        Flambda.Expr.create_let let_bound_var
          (K.value ()) (Prim (project_closure, Debuginfo.none))
          body)
      (close t env body)
      function_declarations
  in
  Flambda.Expr.create_let set_of_closures_var (K.fabricated ())
    set_of_closures body

and close_functions t external_env function_declarations : Flambda.Named.t =
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
      ~set_of_closures_ty:(Flambda_type.any_value ())
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
            (* my_closure is already bound *)
            to_bind, my_closure
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
  (* If the function is the wrapper for a function with an optional
     argument with a default value, make sure it always gets inlined.
     CR-someday pchambart: eta-expansion wrappers for primitives are
     not marked as stubs but certainly should be. *)
  let stub = Function_decl.stub decl in
  let param_vars =
    List.map (fun (p, t) -> Env.find_var closure_env p, t) params
  in
  let params =
    List.map (fun (p, t) ->
        Flambda.Typed_parameter.create (Parameter.wrap p)
          (flambda_type_of_lambda_value_kind t))
      param_vars
  in
  let body = close t closure_env body in
  let free_vars_of_body =
    Name.set_to_var_set (Name_occurrences.in_terms (
      Flambda.Expr.free_names body))
  in
  let body =
    let my_closure = Simple.var my_closure in
    Variable.Map.fold (fun var closure_id body ->
        if not (Variable.Set.mem var free_vars_of_body) then body
        else
          let move : Flambda.unary_primitive =
            Move_within_set_of_closures {
              move_from = my_closure_id;
              move_to = closure_id;
            }
          in
          Flambda.Expr.create_let var (K.value ())
            (Prim (Unary (move, my_closure), Debuginfo.none))
            body)
      project_closure_to_bind
      body
  in
  let body =
    let my_closure = Simple.var my_closure in
    Variable.Map.fold (fun var var_within_closure body ->
        if not (Variable.Set.mem var free_vars_of_body) then body
        else
          Flambda.Expr.create_let var
            (K.value ())
            (Prim (Unary (Project_var var_within_closure, my_closure),
              Debuginfo.none))
            body)
      var_within_closure_to_bind
      body
  in
  let fun_decl =
    let closure_origin = Closure_origin.create my_closure_id in
    let inline = LC.inline_attribute (Function_decl.inline decl) in
    let specialise = LC.specialise_attribute (Function_decl.specialise decl) in
    Flambda.Function_declaration.create
      ~my_closure
      ~params
      ~continuation_param:(Function_decl.continuation_param decl)
      ~exn_continuation_param:(Function_decl.exn_continuation_param decl)
      ~return_arity:[K.value ()]
      ~body ~stub ~dbg ~inline
      ~specialise
      ~is_a_functor:(Function_decl.is_a_functor decl)
      ~closure_origin
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
  let block_var = Variable.create "module_block" in
  let assign_continuation = Continuation.create () in
  let field_vars =
    List.init size
      (fun pos ->
         let pos_str = string_of_int pos in
         Variable.create ("block_field_" ^ pos_str),
         K.value ())
  in
  let assign_continuation_body =
    let field_vars =
      List.init size
        (fun pos ->
           let pos_str = string_of_int pos in
           pos, Variable.create ("block_field_" ^ pos_str))
    in
    let body : Flambda.Expr.t =
      Apply_cont
        (assign_continuation, None,
         List.map (fun (_, var) -> Simple.var var) field_vars)
    in
    List.fold_left (fun body (pos, var) ->
      let pos = Immediate.int (Targetint.OCaml.of_int pos) in
      Flambda.Expr.create_let var (K.value ())
        (Prim (Binary (Block_load (Block (Value Unknown), Immutable),
                       Simple.var block_var,
                       Simple.const (Tagged_immediate pos)),
               Debuginfo.none))
        body)
      body field_vars
  in
  let assign_cont_def : Flambda.Continuation_handler.t =
    { params =
        [Flambda.Typed_parameter.create
           (Parameter.wrap block_var)
           (T.unknown (K.value ()))];
      stub = false; (* XXX for debugging only, change back to true *)
      is_exn_handler = false;
      handler = assign_continuation_body;
    }
  in
  let expr : Flambda.Expr.t =
    Let_cont
      { handlers =
          Non_recursive { name = ilam.return_continuation;
                          handler = assign_cont_def };
        body = close t Env.empty ilam.expr; }
  in

  let computation : Program_body.computation =
    { expr;
      return_cont = assign_continuation;
      exception_cont = ilam.exception_continuation;
      computed_values = field_vars;
    }
  in
  let static_part : Static_part.t =
    Block (Tag.Scannable.zero, Immutable,
           List.map (fun (var, _) : Flambda_static0.Of_kind_value.t ->
             Dynamically_computed var)
             field_vars)
  in
  let program_body : Program_body.t =
    Define_symbol
      ({ computation = Some computation;
         static_structure =
           [module_symbol, K.value (), static_part]; },
       (Root module_symbol))
  in
  let program_body =
    (* CR mshinwell: Share with [Simplify_program] *)
    List.fold_left (fun program_body (symbol, static_part) : Program_body.t ->
        let static_structure =
          [symbol, K.value (), static_part]
        in
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
