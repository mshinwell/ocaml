(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Env = Closure_conversion_aux.Env
module Function_decls = Closure_conversion_aux.Function_decls
module Function_decl = Function_decls.Function_decl
module IdentSet = Lambda.IdentSet
module P = Flambda_primitive
module K = Flambda_kind
module Program_body = Flambda_static.Program_body
module Typed_parameter = Flambda.Typed_parameter

type t = {
  current_unit_id : Ident.t;
  symbol_for_global' : (Ident.t -> Symbol.t);
  filename : string;
  mutable imported_symbols : Symbol.Set.t;
  (* All symbols in [imported_symbols] are to be of kind [Value]. *)
  mutable declared_symbols :
    (Symbol.t * Flambda_static0.Static_part.t) list;
}

(** Generate a wrapper ("stub") function that accepts a tuple argument and
    calls another function with arguments extracted in the obvious
    manner from the tuple. *)
let tupled_function_call_stub
      ( original_params : ( Variable.t * Lambda.value_kind ) list )
      (unboxed_version : Closure_id.t) ~(closure_bound_var : Closure_id.t)
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
        args = List.map Simple.var params;
        (* CR-someday mshinwell for mshinwell: investigate if there is some
           redundancy here (func is also unboxed_version) *)
        call_kind = Function (Direct {
          closure_id = unboxed_version;
          return_arity = [K.value Unknown];
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
      (K.value Definitely_pointer)
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
        Flambda.Expr.create_let param (K.value Unknown) lam body)
      (0, body_with_closure_bound) params
  in
  let tuple_param =
    (* We do not have an accessor here *)

    (* Flambda.Typed_parameter.create (Parameter.wrap tuple_param_var) *)
    (*   (Flambda_type.block Tag.Scannable.zero *)
    (*     (Array.of_list *)
    (*       (List.map (fun _ -> Flambda_type.any_value Must_scan Other) params))) *)

    Flambda.Typed_parameter.create_from_kind (Parameter.wrap tuple_param_var)
      (K.value Unknown)
  in
  Flambda.Function_declaration.create
    ~my_closure
    ~params:[tuple_param] ~continuation_param
    ~exn_continuation_param
    ~return_arity:[K.value Unknown]
    ~body ~stub:true ~dbg:Debuginfo.none ~inline:Default_inline
    ~specialise:Default_specialise ~is_a_functor:false
    ~closure_origin:(Closure_origin.create closure_bound_var)

module Static_part = Flambda_static0.Static_part

let register_const t (constant : Static_part.t) name
      : Flambda_static0.Of_kind_value.t * string =
  let current_compilation_unit = Compilation_unit.get_current_exn () in
  (* Create a variable to ensure uniqueness of the symbol *)
  let var = Variable.create ~current_compilation_unit name in
  let symbol =
    Flambda_utils.make_variable_symbol var
  in
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
        Static_part.Mutable_string { initial_value = Const s; },
          "string"
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
    register_const t (Static_part.Boxed_nativeint (Const c))
      "nativeint"
  | Const_pointer c ->
    (* XCR pchambart: the kind needs to be propagated somewhere to
       say that this value must be scanned
       mshinwell: I don't think it does need to be scanned?
    *)
    Tagged_immediate (Immediate.int (Targetint.OCaml.of_int c)), "pointer"
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
  | Tagged_immediate c, name ->
    Simple (Simple.const (Tagged_immediate c)), name
  | Symbol s, name ->
    Simple (Simple.symbol s), name
  | Dynamically_computed _, name ->
    Misc.fatal_errorf "Declaring a computed constant %s" name

(* CR pchambart: move to flambda_type ? *)
let flambda_type_of_lambda_value_kind (k : Lambda.value_kind) : Flambda_type.t =
  match k with
  | Pgenval ->
    Flambda_type.any_value Unknown
  | Pfloatval ->
    Flambda_type.any_boxed_float ()
  | Pboxedintval Pint32 ->
    Flambda_type.any_boxed_int32 ()
  | Pboxedintval Pint64 ->
    Flambda_type.any_boxed_int64 ()
  | Pboxedintval Pnativeint ->
    Flambda_type.any_boxed_nativeint ()
  | Pintval ->
    Flambda_type.any_tagged_immediate ()
  | Pnaked_intval ->
    Misc.fatal_error "[Pnaked_intval] shouldn't exist before Flambda"

let convert_inline_attribute_from_lambda
      (attr : Lambda.inline_attribute)
      : Flambda.inline_attribute =
  match attr with
  | Always_inline -> Always_inline
  | Never_inline -> Never_inline
  | Unroll i -> Unroll i
  | Default_inline -> Default_inline

let convert_specialise_attribute_from_lambda
      (attr : Lambda.specialise_attribute)
      : Flambda.specialise_attribute =
  match attr with
  | Always_specialise -> Always_specialise
  | Never_specialise -> Never_specialise
  | Default_specialise -> Default_specialise

let kind_of_repr (repr : Primitive.native_repr) : K.t =
  match repr with
  | Same_as_ocaml_repr -> K.value Unknown
  | Unboxed_float -> K.naked_float ()
  | Unboxed_integer Pnativeint -> K.naked_nativeint ()
  | Unboxed_integer Pint32 -> K.naked_int32 ()
  | Unboxed_integer Pint64 -> K.naked_int64 ()
  | Untagged_int -> K.naked_immediate ()

let rec close t env (lam : Ilambda.t) : Flambda.Expr.t =
  match lam with
  | Let (id,
         Prim { prim = Pccall prim; args; loc; exception_continuation },
         body) ->
    (* CR pchambart: there should be a special case if body is a
       apply_cont *)
    let continuation = Continuation.create () in
    let return_kind = kind_of_repr prim.prim_native_repr_res in
    let call_kind : Flambda.Call_kind.t =
      C_call {
        alloc = prim.prim_alloc;
        param_arity = List.map kind_of_repr prim.prim_native_repr_args;
        return_arity = [ return_kind ];
      }
    in
    let call_symbol =
      Symbol.create
        (Compilation_unit.external_symbols ())
        (Linkage_name.create prim.prim_name)
    in
    let dbg = Debuginfo.from_location loc in
    (* TODO:
       unbox arguments
       box return *)
    let call args : Flambda.Expr.t =
      Apply ({
        call_kind;
        func = Name.symbol call_symbol;
        args;
        continuation;
        exn_continuation = exception_continuation;
        dbg;
        inline = Default_inline;
        specialise = Default_specialise;
      })
    in
    let call =
      List.fold_right2
        (fun arg (arg_repr:Primitive.native_repr)
          (call : Simple.t list -> Flambda.Expr.t) ->
        let boxing : P.unary_primitive option =
          match arg_repr with
          | Same_as_ocaml_repr -> None
          | Unboxed_float ->
            Some (P.Unbox_number Naked_float)
          | Unboxed_integer Pnativeint ->
            Some (P.Unbox_number Naked_nativeint)
          | Unboxed_integer Pint32 ->
            Some (P.Unbox_number Naked_int32)
          | Unboxed_integer Pint64 ->
            Some (P.Unbox_number Naked_int64)
          | Untagged_int ->
            Some (P.Num_conv { src = Tagged_immediate; dst = Naked_nativeint; })
        in
        match boxing with
        | None ->
          (fun args -> call (arg :: args))
        | Some named ->
          (fun args ->
             let unboxed_arg = Variable.create "unboxed" in
             Flambda.Expr.create_let unboxed_arg
               (kind_of_repr arg_repr) (Prim (Unary (named, arg), dbg))
               (call (Simple.var unboxed_arg :: args))))
        (Env.find_simples env args)
        prim.prim_native_repr_args
        call []
    in
    let cont, handler_param =
      let unboxing : P.unary_primitive option =
        match prim.prim_native_repr_res with
        | Same_as_ocaml_repr -> None
        | Unboxed_float ->
          Some (P.Box_number Naked_float)
        | Unboxed_integer Pnativeint ->
          Some (P.Box_number Naked_nativeint)
        | Unboxed_integer Pint32 ->
          Some (P.Box_number Naked_int32)
        | Unboxed_integer Pint64 ->
          Some (P.Box_number Naked_int64)
        | Untagged_int ->
          Some (P.Num_conv { src = Naked_nativeint; dst = Tagged_immediate })
      in
      match unboxing with
      | None ->
        let body_env, handler_param = Env.add_var_like env id in
        let body = close t body_env body in
        body, handler_param
      | Some unboxing ->
        let handler_param = Variable.create (prim.prim_name ^ "_return") in
        let body_env, boxed_var = Env.add_var_like env id in
        let body = close t body_env body in
        Flambda.Expr.create_let boxed_var
          (K.value Definitely_pointer)
          (Prim (Unary (unboxing, Simple.var handler_param), dbg)) body,
          handler_param
    in
    let handler : Flambda.Continuation_handler.t = {
      params =
        [ Typed_parameter.create_from_kind
            (Parameter.wrap handler_param)
            return_kind ];
      stub = false;
      is_exn_handler = false;
      handler = cont;
    } in
    Let_cont {
      body = call;
      handlers = Non_recursive { name = continuation; handler };
    };

  | Let (id, defining_expr, body) ->
    let body_env, var = Env.add_var_like env id in
    let cont (defining_expr : Flambda.Named.t) =
      (* CR pchambart: Not tail ! *)
      let body = close t body_env body in
      (* CR pchambart: Kind annotation on let should to go through Ilambda
         mshinwell: I added the following basic inference *)
      let kind =
        match defining_expr with
        | Simple (Name (Symbol _)) ->
          K.value Definitely_pointer
        | Simple (Const (Untagged_immediate _)) ->
          K.naked_immediate ()
        | Simple (Const (Tagged_immediate _)) ->
          K.value Definitely_immediate
        | Simple (Const (Naked_float _)) ->
          K.naked_float ()
        | Simple (Const (Naked_int32 _)) ->
          K.naked_int32 ()
        | Simple (Const (Naked_int64 _)) ->
          K.naked_int64 ()
        | Simple (Const (Naked_nativeint _)) ->
          K.naked_nativeint ()
        | Set_of_closures _ ->
          K.fabricated Definitely_pointer
        | Assign _ ->
          K.unit ()
        | Prim (prim, _dbg) ->
          begin match Flambda_primitive.result_kind prim with
          | Singleton kind -> kind
          | Unit -> K.unit ()
          | Never_returns -> K.value Unknown
          end
        | Simple (Name (Var _))
        | Read_mutable _ -> K.value Unknown
      in
      Flambda.Expr.create_let var kind defining_expr body
    in
    close_named t env defining_expr cont
  | Let_mutable { id; initial_value; contents_kind; body; } ->
    (* See comment on [Pread_mutable] below. *)
    let var = Mutable_variable.of_ident id in
    let initial_value = Env.find_simple env initial_value in
    let body = close t (Env.add_mutable_var env id var) body in
    Let_mutable {
      var;
      initial_value;
      body;
      contents_type = flambda_type_of_lambda_value_kind contents_kind;
    }
  | Let_rec (defs, body) ->
    let env =
      List.fold_right (fun (id,  _) env ->
          let env, _var = Env.add_var_like env id in
          env)
        defs env
    in
    let function_declarations =
      (* Functions will be named after the corresponding identifier in the
         [let rec]. *)
      List.map (function
          | (let_rec_ident,
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
    (* We eliminate the [let rec] construction, instead producing a normal
       [Let] that binds a set of closures containing all of the functions.
       ([let rec] on non-functions was removed in [Prepare_lambda].)
    *)
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
      List.fold_left (fun body decl ->
          let let_rec_ident = Function_decl.let_rec_ident decl in
          let closure_bound_var = Function_decl.closure_bound_var decl in
          let let_bound_var = Env.find_var env let_rec_ident in
          (* Inside the body of the [let], each function is referred to by
             a [Project_closure] expression, which projects from the set of
             closures. *)
          (Flambda.Expr.create_let let_bound_var
             (K.value Definitely_pointer)
             (Prim (Unary (Project_closure closure_bound_var,
                           Simple.var set_of_closures_var),
                    Debuginfo.none))
            body))
        (close t env body) function_declarations
    in
    Flambda.Expr.create_let set_of_closures_var
      (K.fabricated Definitely_pointer)
      set_of_closures body
  | Let_cont let_cont ->
    if let_cont.is_exn_handler then begin
      assert (not let_cont.administrative);
      assert (List.length let_cont.params = 1);
      assert (let_cont.recursive = Asttypes.Nonrecursive);
    end;
    (* Inline out administrative redexes. *)
    if let_cont.administrative then begin
      assert (let_cont.recursive = Asttypes.Nonrecursive);
      let body_env =
        Env.add_administrative_redex env let_cont.name ~params:let_cont.params
          ~handler:let_cont.handler
      in
      close t body_env let_cont.body
    end else begin
      let handler_env, params = Env.add_vars_like env let_cont.params in
      let params =
        List.map (fun param ->
          Flambda.Typed_parameter.create_from_kind
            (Parameter.wrap param)
            (K.value Unknown))
          params
      in
      let handler : Flambda.Continuation_handler.t =
        { params;
          stub = false;
          is_exn_handler = let_cont.is_exn_handler;
          handler = close t handler_env let_cont.handler;
        };
      in
      let handlers : Flambda.Let_cont_handlers.t =
        match let_cont.recursive with
        | Nonrecursive -> Non_recursive { name = let_cont.name; handler; }
        | Recursive ->
          Recursive (Continuation.Map.add let_cont.name handler
            Continuation.Map.empty)
      in
      Let_cont {
        body = close t env let_cont.body;
        handlers;
      };
    end
  | Apply { kind; func; args; continuation; exn_continuation;
      loc; should_be_tailcall = _; inlined; specialised; } ->
    let call_kind : Flambda.Call_kind.t =
      match kind with
      | Function -> Function Indirect_unknown_arity
      | Method { kind; obj; } ->
        let kind : Flambda.Call_kind.method_kind =
          match kind with
          | Self -> Self
          | Public -> Public
          | Cached -> Cached
        in
        Method {
          kind;
          obj = Env.find_name env obj;
        }
    in
    Apply ({
      call_kind;
      func = Env.find_name env func;
      args = Env.find_simples env args;
      continuation;
      exn_continuation;
      dbg = Debuginfo.from_location loc;
      inline = convert_inline_attribute_from_lambda inlined;
      specialise = convert_specialise_attribute_from_lambda specialised;
    })
  | Apply_cont (cont, trap_action, args) ->
    let args = Env.find_vars env args in
    begin match Env.find_administrative_redex env cont with
    | Some (params, handler) when trap_action = None ->
      let handler_env = Env.add_vars env params args in
      close t handler_env handler
    | _ ->
      let trap_action =
        Misc.Stdlib.Option.map (fun (trap_action : Ilambda.trap_action)
                  : Flambda.Trap_action.t ->
            match trap_action with
            | Push { id; exn_handler; } -> Push { id; exn_handler; }
            | Pop { id; exn_handler; } ->
              Pop { id; exn_handler; take_backtrace = false; })
          trap_action
      in
      Apply_cont (cont, trap_action, List.map Simple.var args)
    end
  | Switch (scrutinee, sw) ->
    begin match sw.kind with
    | Int ->
      let arms =
        List.map (fun (case, arm) -> Targetint.OCaml.of_int case, arm)
          sw.consts
      in
      let arms =
        match sw.failaction with
        | None ->
          Targetint.OCaml.Map.of_list arms
        | Some default ->
          Numbers.Int.Set.fold (fun case cases ->
            let case = Targetint.OCaml.of_int case in
            if Targetint.OCaml.Map.mem case cases then
              cases
            else
              Targetint.OCaml.Map.add case default cases)
            (Numbers.Int.zero_to_n (sw.numconsts - 1))
            (Targetint.OCaml.Map.of_list arms)
      in
      Flambda.Expr.create_int_switch ~scrutinee:(Env.find_name env scrutinee)
        ~arms
    | Tag ->
      (* CR mshinwell: work out how to share code with case above *)
      let arms =
        List.map (fun (case, arm) -> Tag.create_exn case, arm)
          sw.consts
      in
      let arms =
        match sw.failaction with
        | None ->
          Tag.Map.of_list arms
        | Some default ->
          Numbers.Int.Set.fold (fun case cases ->
            let case = Tag.create_exn case in
            if Tag.Map.mem case cases then
              cases
            else
              Tag.Map.add case default cases)
            (Numbers.Int.zero_to_n (sw.numconsts - 1))
            (Tag.Map.of_list arms)
      in
      Flambda.Expr.create_tag_switch ~scrutinee:(Env.find_name env scrutinee)
        ~arms
    end
  | Event (ilam, _) -> close t env ilam

and close_named t env (named : Ilambda.named)
      (cont : Flambda.Named.t -> Flambda.Expr.t) : Flambda.Expr.t =
  match named with
  | Var id ->
    let simple =
      if Ident.is_predef_exn id then begin
        let symbol = t.symbol_for_global' id in
        t.imported_symbols <- Symbol.Set.add symbol t.imported_symbols;
        Simple.symbol symbol
      end else begin
        Simple.var (Env.find_var env id)
      end
    in
    cont (Simple simple)
  | Const cst ->
    cont (fst (close_const t cst))
  | Prim { prim = Pread_mutable id; args } ->
    (* All occurrences of mutable variables bound by [Let_mutable] are
       identified by [Prim (Pread_mutable, ...)] in Ilambda. *)
    assert (args = []);
    cont (Read_mutable (Env.find_mutable_var env id))
  | Prim { prim = Pgetglobal id; args = [] } when Ident.is_predef_exn id ->
    let symbol = t.symbol_for_global' id in
    t.imported_symbols <- Symbol.Set.add symbol t.imported_symbols;
    cont (Simple (Simple.symbol symbol))
  | Prim { prim = Pgetglobal id; args = [] } ->
    assert (not (Ident.same id t.current_unit_id));
    let symbol = t.symbol_for_global' id in
    t.imported_symbols <- Symbol.Set.add symbol t.imported_symbols;
    cont (Simple (Simple.symbol symbol))
  | Prim { prim; args; loc; exception_continuation } ->
    Lambda_to_flambda_primitives.convert_and_bind prim
      ~args:(Env.find_simples env args)
      ~exception_continuation
      (Debuginfo.from_location loc) cont
  | Assign { being_assigned; new_value; } ->
    cont (Assign {
      being_assigned = Env.find_mutable_var env being_assigned;
      new_value = Env.find_simple env new_value;
    })

(** Perform closure conversion on a set of function declarations, returning a
    set of closures.  (The set will often only contain a single function;
    the only case where it cannot is for "let rec".) *)
and close_functions t external_env function_declarations : Flambda.Named.t =
  let all_free_idents =
    (* Filter out predefined exception identifiers, since they will be
       turned into symbols when we closure-convert the body. *)
    IdentSet.filter (fun ident ->
        not (Ident.is_predef_exn ident))
      (Function_decls.all_free_idents function_declarations)
  in
  let var_within_closure_from_ident =
    Ident.Set.fold (fun id map ->
      let v = Variable.create_with_same_name_as_ident id in
      Ident.Map.add id (Var_within_closure.wrap v) map)
      all_free_idents Ident.Map.empty
  in
  let closure_id_from_ident =
    List.fold_left (fun map decl ->
      let id = Function_decl.let_rec_ident decl in
      let closure_id = Function_decl.closure_bound_var decl in
      Ident.Map.add id closure_id map)
      Ident.Map.empty
      (Function_decls.to_list function_declarations)
  in

  let close_one_function map decl =
    let body = Function_decl.body decl in
    let loc = Function_decl.loc decl in
    let dbg = Debuginfo.from_location loc in
    let params = Function_decl.params decl in
    let my_closure = Variable.create "my_closure" in

    let closure_bound_var =
      Function_decl.closure_bound_var decl
    in
    let unboxed_version =
      (* Better variable name *)
      Closure_id.wrap (Variable.create "unboxed_version")
    in
    let my_closure_id =
      match Function_decl.kind decl with
      | Curried -> closure_bound_var
      | Tupled -> unboxed_version
    in

    (* les variables libres sont:
       les paramètres: substitution directe en variables
       la fonction définie: accessible avec 'my_closure'
       les autres fonctions: accessibles avec un move
       let autres variables libres: accessibles avec une projection *)

    let var_within_closure_to_bind,
        var_for_ident_within_closure =
      Ident.Map.fold (fun id var_within_closure (to_bind, var_for_ident) ->
        let var = Variable.create_with_same_name_as_ident id in
        Variable.Map.add var var_within_closure to_bind,
        Ident.Map.add id var var_for_ident)
        var_within_closure_from_ident
        (Variable.Map.empty, Ident.Map.empty)
    in

    let project_closure_to_bind,
        var_for_project_closure =
      List.fold_left (fun (to_bind, var_for_ident) function_decl ->
        let let_rec_ident = Function_decl.let_rec_ident function_decl in
        let to_bind, var =
          if Ident.same (Function_decl.let_rec_ident function_decl)
               let_rec_ident then
            (* my_closure is already bound *)
            to_bind, my_closure
          else
            let variable =
              Variable.create_with_same_name_as_ident let_rec_ident
            in
            let closure_id =
              Ident.Map.find let_rec_ident closure_id_from_ident
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
      Env.add_var_map
        (Env.add_var_map empty_env var_for_ident_within_closure)
        var_for_project_closure
    in

    (* Create fresh variables for the elements of the closure (cf.
       the comment on [Function_decl.closure_env_without_parameters], above).
       This induces a renaming on [Function_decl.free_idents]; the results of
       that renaming are stored in [free_variables]. *)
    let closure_env =
      List.fold_right (fun (id, _) env ->
          let env, _var = Env.add_var_like env id in
          env)
        params closure_env_without_parameters
    in
    (* If the function is the wrapper for a function with an optional
       argument with a default value, make sure it always gets inlined.
       CR-someday pchambart: eta-expansion wrapper for a primitive are
       not marked as stub but certainly should *)
    let stub = Function_decl.stub decl in
    let param_vars =
      List.map (fun (p, t) -> Env.find_var closure_env p, t) params
    in
    let params =
      List.map (fun (p, t) ->
        Flambda.Typed_parameter.create (Parameter.wrap p)
          ~type_of_name:(fun ?local_env:_ _ -> None)
          (flambda_type_of_lambda_value_kind t))
        param_vars
    in
    let body = close t closure_env body in
    let free_var_of_body =
      Flambda.Expr.free_variables body
    in

    let body =
      Variable.Map.fold (fun var closure_id body ->
        if Variable.Set.mem var free_var_of_body then
          let move =
            Flambda_primitive.Move_within_set_of_closures {
              move_from = my_closure_id;
              move_to = closure_id;
            }
          in
          Flambda.Expr.create_let var (K.value Definitely_pointer)
            (Prim (Unary (move, Simple.var my_closure), Debuginfo.none))
            body
        else
          body
      ) project_closure_to_bind body
    in

    let body =
      Variable.Map.fold (fun var var_within_closure body ->
        if Variable.Set.mem var free_var_of_body then
          let projection =
            Flambda_primitive.Project_var
              (my_closure_id, var_within_closure)
          in
          Flambda.Expr.create_let var
            (K.value Definitely_pointer)
            (Prim
               (Unary (projection, Simple.var my_closure),
                Debuginfo.none))
            body
        else
          body
      ) var_within_closure_to_bind body
    in

    let fun_decl =
      let closure_origin =
        Closure_origin.create my_closure_id
        (* Closure_origin.create (Closure_id.wrap unboxed_version) *)
      in
      let inline =
        convert_inline_attribute_from_lambda (Function_decl.inline decl)
      in
      let specialise =
        convert_specialise_attribute_from_lambda
          (Function_decl.specialise decl)
      in
      Flambda.Function_declaration.create
        ~my_closure
        ~params
        ~continuation_param:(Function_decl.continuation_param decl)
        ~exn_continuation_param:(Function_decl.exn_continuation_param decl)
        ~return_arity:[K.value Unknown]
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
  in
  let function_decls =
    Flambda.Function_declarations.create
      ~funs:
        (List.fold_left close_one_function Closure_id.Map.empty
          (Function_decls.to_list function_declarations))
  in
  (* The closed representation of a set of functions is a "set of closures".
     (For avoidance of doubt, the runtime representation of the *whole set* is
     a single block with tag [Closure_tag].) *)
  let set_of_closures =
    let in_closure =
      Ident.Map.fold (fun id var_within_closure map ->
        let external_var : Flambda.Free_var.t =
          { var = Env.find_var external_env id;
            (* CR pchambart: Should we populate that with a projection primitive ? *)
            equalities = Flambda_primitive.With_fixed_value.Set.empty;
          }
        in
        Var_within_closure.Map.add var_within_closure external_var map)
        var_within_closure_from_ident
        Var_within_closure.Map.empty
    in
    Flambda.Set_of_closures.create ~function_decls ~in_closure
      ~direct_call_surrogates:Closure_id.Map.empty
  in
  Set_of_closures set_of_closures

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
  (* let block_symbol = *)
  (*   let linkage_name = Linkage_name.create "module_as_block" in *)
  (*   Symbol.create compilation_unit linkage_name *)
  (* in *)
  (* The global module block is built by accessing the fields of all the
     introduced symbols. *)
  (* CR-soon mshinwell for mshinwell: Add a comment describing how modules are
     compiled. *)
  (* let continuation = Continuation.create () in *)
  (* let main_module_block_expr = *)
  (*   let field_vars = *)
  (*     List.init size *)
  (*       (fun pos -> *)
  (*          let pos_str = string_of_int pos in *)
  (*          pos, Variable.create ("block_symbol_" ^ pos_str)) *)
  (*   in *)
  (*   let call_continuation : Flambda.Expr.t = *)
  (*     Apply_cont (continuation, None, List.map snd field_vars) *)
  (*   in *)
  (*   let block_symbol_var = Variable.create "block_symbol" in *)
  (*   let body = *)
  (*     List.fold_left (fun body (pos, var) -> *)
  (*       Flambda.Expr.create_let var *)
  (*         (K.value Must_scan) *)
  (*         (Prim (Pfield pos, [block_symbol_var], Debuginfo.none)) *)
  (*         body) *)
  (*       call_continuation field_vars *)
  (*   in *)
  (*   Flambda.Expr.create_let block_symbol_var *)
  (*     (K.value Must_scan) *)
  (*     (Read_symbol_field { symbol = block_symbol; logical_field = 0 }) *)
  (*     body *)
  (* in *)

  let block_var = Variable.create "module_block" in
  let assign_continuation = Continuation.create () in
  let field_vars =
    List.init size
      (fun pos ->
         let pos_str = string_of_int pos in
         Variable.create ("block_field_" ^ pos_str),
         K.value Unknown)
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
      Flambda.Expr.create_let var (K.value Unknown)
        (Prim (Binary (Block_load (Block (Value Unknown), Immutable),
                       Simple.var block_var,
                       Simple.const (Tagged_immediate pos)),
               Debuginfo.none))
        body)
      body field_vars
  in
  let assign_cont_def : Flambda.Continuation_handler.t =
    { params =
        [Flambda.Typed_parameter.create_from_kind
           (Parameter.wrap block_var)
           (K.value Definitely_pointer)];
      stub = true;
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
           [module_symbol, K.value Definitely_pointer, static_part]; },
       (Root module_symbol))
  in
  let program_body =
    (* CR mshinwell: Share with [Simplify_program] *)
    List.fold_left (fun program_body (symbol, static_part) : Program_body.t ->
        let static_structure =
          [symbol, K.value Definitely_pointer, static_part]
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
        Symbol.Map.add symbol (K.value Definitely_pointer) imported_symbols)
      t.imported_symbols
      Symbol.Map.empty
  in
(* let module_initialize : Program_body.Initialize_symbol.t = *)
  (*   { expr = main_module_block_expr; *)
  (*     return_cont = continuation; *)
  (*     return_arity = List.init size (fun _ -> K.value Must_scan); *)
  (*   } *)
  (* in *)
  (* let module_initializer : Program_body.t = *)
  (*   Initialize_symbol ( *)
  (*     block_symbol, *)
  (*     block_initialize, *)
  (*     Initialize_symbol ( *)
  (*       module_symbol, *)
  (*       module_initialize, *)
  (*       End module_symbol)) *)
  (* in *)
  (* let program_body = *)
  (*   List.fold_left *)
  (*     (fun program_body (symbol, constant) : Program_body.t -> *)
  (*        Let_symbol (symbol, constant, program_body)) *)
  (*     module_initializer *)
  (*     t.declared_symbols *)
  (* in *)
  { imported_symbols;
    body = program_body;
  }


(* CR mshinwell: read carefully.  Moved here from Flambda_type

  let refine_using_value_kind t (kind : Lambda.value_kind) =
    match kind with
    | Pgenval -> t
    | Pfloatval ->
      begin match t.descr with
      | Boxed_or_encoded_number (Boxed Float,
          { descr = Naked_number (Float _); _ }) ->
        t
      | Unknown ((Unboxed_float | Bottom), reason) ->
        { t with
          descr = Boxed_or_encoded_number (Boxed Float,
            just_descr (Unknown (K.unboxed_float (), reason)));
        }
      | Unknown (
          (Value | Tagged_int | Naked_int | Naked_int32 | Naked_int64
            | Unboxed_nativeint), _) ->
        Misc.fatal_errorf "Wrong type for Pfloatval kind: %a"
          print t
      | Union _
      | Naked_number _
      | Boxed_or_encoded_number _
      | Set_of_closures _
      | Closure _
      | Immutable_string _
      | Mutable_string _
      | Float_array _
      | Bottom ->
        (* Invalid _ *)
        { t with descr = Bottom }
      | Load_lazily _ ->
        (* We don't know yet *)
        t
      end
    (* CR mshinwell: Do we need more cases here?  We could add Pintval *)
    | _ -> t
*)
