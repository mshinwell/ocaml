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
(*   special exception on linking described in the file LICENSDE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Flambda.Import

module CUE = Continuation_uses_env
module DA = Downwards_acc
module DE = Simplify_env_and_result.Downwards_env
module K = Flambda_kind
module KP = Kinded_parameter
module R = Simplify_env_and_result.Result
module S = Simplify_simple
module T = Flambda_type
module UA = Upwards_acc
module UE = Simplify_env_and_result.Upwards_env

module Of_kind_value = Flambda_static.Of_kind_value
module Program = Flambda_static.Program
module Program_body = Flambda_static.Program_body
module Static_part = Flambda_static.Static_part
module Static_structure = Flambda_static.Program_body.Static_structure

(* CR-someday mshinwell: Add improved simplification using types (we have
   prototype code to do this). *)

let simplify_of_kind_value dacc (of_kind_value : Of_kind_value.t) =
  let denv = DA.denv dacc in
  match of_kind_value with
  | Symbol sym ->
    DE.check_symbol_is_bound denv sym;
    of_kind_value
  | Tagged_immediate _ -> of_kind_value
  | Dynamically_computed var ->
    let min_occurrence_kind = Name_occurrence_kind.normal in
    match S.simplify_simple dacc (Simple.var var) ~min_occurrence_kind with
    | Bottom, ty ->
      assert (K.equal (T.kind ty) K.value);
      (* CR mshinwell: Work out what should happen here *)
      of_kind_value
    | Ok simple, _ty ->
      match Simple.descr simple with
      | Name (Symbol sym) -> Of_kind_value.Symbol sym
      | Name (Var _) | Const _ | Discriminant _ -> of_kind_value

let simplify_or_variable dacc (or_variable : _ Static_part.or_variable) =
  let denv = DA.denv dacc in
  match or_variable with
  | Const _ -> or_variable
  | Var var ->
    DE.check_variable_is_bound denv var;
    or_variable

let simplify_set_of_closures dacc ~result_dacc set_of_closures
      ~set_of_closures_symbol
      ~closure_symbols ~closure_elements_and_types =
  let closure_elements, closure_element_types =
    match closure_elements_and_types with
    | Some (closure_elements, closure_element_types) ->
      closure_elements, closure_element_types
    | None ->
      Var_within_closure.Map.fold
        (fun var_within_closure simple
             (closure_elements, closure_element_types) ->
          let min_occurrence_kind = Name_occurrence_kind.normal in
          match
            Simplify_simple.simplify_simple dacc simple ~min_occurrence_kind
          with
          | Bottom, ty ->
            assert (K.equal (T.kind ty) K.value);
            (* CR mshinwell: Work out what should happen here *)
            closure_elements, closure_element_types
          | Ok simple, ty ->
            let closure_elements =
              Var_within_closure.Map.add var_within_closure simple
                closure_elements
            in
            let ty_value = T.force_to_kind_value ty in
            let closure_element_types =
              Var_within_closure.Map.add var_within_closure ty_value
                closure_element_types
            in
            closure_elements, closure_element_types)
        (Set_of_closures.closure_elements set_of_closures)
        (Var_within_closure.Map.empty, Var_within_closure.Map.empty)
  in
  let function_decls = Set_of_closures.function_decls set_of_closures in
  let funs = Function_declarations.funs function_decls in
  let denv = DA.denv dacc in
  let denv = DE.define_symbol denv set_of_closures_symbol K.fabricated in
  let denv =
    Closure_id.Map.fold (fun _closure_id closure_symbol denv ->
        DE.define_symbol denv closure_symbol K.value)
      closure_symbols
      denv
  in
  let set_of_closures_ty_fabricated =
    T.alias_type_of_as_ty_fabricated (Simple.symbol set_of_closures_symbol)
  in
  (* CR mshinwell: Some of this code could maybe be shared with the
     post-body-simplification code that builds types, below. *)
  let set_of_closures_type =
    (* The set-of-closures type describes the closures it contains via
       aliases to the closure symbols.  This means that when an appropriate
       [Project_closure] primitive, for example, is encountered then it will
       simplify directly to a symbol.

       The detail of the closures themselves is described using closure
       types assigned to the individual closure symbols.  These types in
       turn tie back recursively to the set-of-closures symbol. *)
    let closure_types_via_symbols =
      Closure_id.Map.map (fun closure_sym ->
          T.alias_type_of K.value (Simple.symbol closure_sym))
        closure_symbols
    in
    T.set_of_closures ~closures:closure_types_via_symbols
  in
  let denv =
    DE.add_equation_on_symbol denv set_of_closures_symbol set_of_closures_type
  in
  let closure_symbols_and_types =
    Closure_id.Map.mapi (fun closure_id func_decl ->
        let closure_symbol = Closure_id.Map.find closure_id closure_symbols in
        let function_decl_type =
          T.create_inlinable_function_declaration func_decl
            (Rec_info.create ~depth:1 ~unroll_to:None)
        in
        let closure_type =
          T.closure closure_id function_decl_type closure_element_types
            ~set_of_closures:set_of_closures_ty_fabricated
        in
        closure_symbol, closure_type)
      funs
  in
  let denv =
    Closure_id.Map.fold (fun _closure_id (closure_symbol, closure_type) denv ->
        DE.add_equation_on_symbol denv closure_symbol closure_type)
      closure_symbols_and_types
      denv
  in
  let dacc = DA.with_denv dacc denv in
  let pre_simplification_types_of_my_closures
        : Simplify_named.pre_simplification_types_of_my_closures =
    let closure_types =
      Closure_id.Map.map (fun (_closure_symbol, closure_type) -> closure_type)
        closure_symbols_and_types
    in
    { set_of_closures = None;
      closure_types;
    }
  in
  let r = DA.r dacc in
  let funs, fun_types, r =
    Closure_id.Map.fold (fun closure_id function_decl (funs, fun_types, r) ->
        let dacc = DA.with_r dacc r in
        let function_decl, ty, r =
          Simplify_named.simplify_function dacc closure_id function_decl
            pre_simplification_types_of_my_closures
        in
        let funs = Closure_id.Map.add closure_id function_decl funs in
        let fun_types = Closure_id.Map.add closure_id ty fun_types in
        funs, fun_types, r)
      funs
      (Closure_id.Map.empty, Closure_id.Map.empty, r)
  in
  let result_dacc = DA.with_r result_dacc r in
  let function_decls = Function_declarations.create funs in
  let set_of_closures =
    Set_of_closures.create ~function_decls ~closure_elements
  in
  let closure_types =
    Closure_id.Map.mapi (fun closure_id function_decl_type ->
        T.closure closure_id function_decl_type closure_element_types
          ~set_of_closures:set_of_closures_ty_fabricated)
      fun_types
  in
  let closure_symbols_and_types =
    Closure_id.Map.mapi (fun closure_id typ ->
        (* CR mshinwell: clean this up *)
        let closure_symbol = Closure_id.Map.find closure_id closure_symbols in
        closure_symbol, typ)
      closure_types
  in
  let closure_symbols =
    Closure_id.Map.map (fun (sym, _typ) -> sym) closure_symbols_and_types
  in
  let static_part : K.fabricated Static_part.t =
    Set_of_closures set_of_closures
  in
  let bound_symbols : K.fabricated Program_body.Bound_symbols.t =
    Set_of_closures {
      set_of_closures_symbol;
      closure_symbols;
    }
  in
  let static_structure : Program_body.Static_structure.t =
    S [bound_symbols, static_part]
  in
  let set_of_closures_type =
    let closure_types_via_symbols =
      Closure_id.Map.map (fun closure_sym ->
          T.alias_type_of K.value (Simple.symbol closure_sym))
        closure_symbols
    in
    T.set_of_closures ~closures:closure_types_via_symbols
  in
  (* The returned bindings are put into [result_env], rather than [env], so
     [simplify_static_structure] below can correctly handle simultaneous
     definitions of symbols. *)
  let denv =
    DE.define_symbol (DA.denv result_dacc) set_of_closures_symbol K.fabricated
  in
  let denv =
    Closure_id.Map.fold (fun _ (symbol, _typ) denv ->
        DE.define_symbol denv symbol K.value)
      closure_symbols_and_types
      denv
  in
  let denv =
    DE.add_equation_on_symbol denv set_of_closures_symbol set_of_closures_type
  in
  let denv =
    Closure_id.Map.fold (fun _ (symbol, typ) denv ->
        DE.add_equation_on_symbol denv symbol typ)
      closure_symbols_and_types
      denv
  in
  let static_structure_types =
    let static_structure_types =
      Closure_id.Map.fold (fun _ (symbol, typ) static_structure_types ->
          Symbol.Map.add symbol typ static_structure_types)
        closure_symbols_and_types
        Symbol.Map.empty
    in
    Symbol.Map.add set_of_closures_symbol set_of_closures_type
      static_structure_types
  in
  let result_dacc = DA.with_denv result_dacc denv in
  set_of_closures, result_dacc, set_of_closures_type, static_structure_types,
    static_structure

let simplify_static_part_of_kind_value dacc
      (static_part : K.value Static_part.t) ~result_sym
      : K.value Static_part.t * DA.t =
  let bind_result_sym ty =
    DA.map_denv dacc ~f:(fun denv -> DE.add_symbol denv result_sym ty)
  in
  match static_part with
  | Block (tag, is_mutable, fields) ->
    let fields =
      List.map (fun of_kind_value ->
          simplify_of_kind_value dacc of_kind_value)
        fields
    in
    let dacc = bind_result_sym (T.any_value ()) in
    Block (tag, is_mutable, fields), dacc
  | Fabricated_block var ->
    DE.check_variable_is_bound (DA.denv dacc) var;
    let dacc = bind_result_sym (T.any_fabricated ()) in
    static_part, dacc
  | Boxed_float or_var ->
    let dacc = bind_result_sym (T.any_boxed_float ()) in
    Boxed_float (simplify_or_variable dacc or_var), dacc
  | Boxed_int32 (Const i) ->
    let ty = T.this_boxed_int32 i in
    let dacc = bind_result_sym ty in
    static_part, dacc
  | Boxed_int32 or_var ->
    let dacc = bind_result_sym (T.any_boxed_int32 ()) in
    Boxed_int32 (simplify_or_variable dacc or_var), dacc
  | Boxed_int64 (Const i) ->
    let ty = T.this_boxed_int64 i in
    let dacc = bind_result_sym ty in
    static_part, dacc
  | Boxed_int64 or_var ->
    let dacc = bind_result_sym (T.any_boxed_int64 ()) in
    Boxed_int64 (simplify_or_variable dacc or_var), dacc
  | Boxed_nativeint or_var ->
    let dacc = bind_result_sym (T.any_boxed_nativeint ()) in
    Boxed_nativeint (simplify_or_variable dacc or_var), dacc
  | Immutable_float_array fields ->
    let fields =
      List.map (fun field -> simplify_or_variable dacc field) fields
    in
    let dacc = bind_result_sym (T.any_value ()) in
    Immutable_float_array fields, dacc
  | Mutable_string { initial_value; } ->
    let static_part : K.value Static_part.t =
      Mutable_string {
        initial_value = simplify_or_variable dacc initial_value;
      }
    in
    let dacc = bind_result_sym (T.any_value ()) in
    static_part, dacc
  | Immutable_string or_var ->
    let dacc = bind_result_sym (T.any_value ()) in
    Immutable_string (simplify_or_variable dacc or_var), dacc

let simplify_static_part_of_kind_fabricated dacc ~result_dacc
      (static_part : K.fabricated Static_part.t)
      ~set_of_closures_symbol ~closure_symbols
    : K.fabricated Static_part.t * DA.t =
  match static_part with
  | Set_of_closures set_of_closures ->
     let set_of_closures, dacc, _ty, _static_structure_types,
         _static_structure =
       simplify_set_of_closures dacc ~result_dacc set_of_closures
         ~set_of_closures_symbol ~closure_symbols
         ~closure_elements_and_types:None
     in
     Set_of_closures set_of_closures, dacc

let simplify_piece_of_static_structure (type k) dacc ~result_dacc
      (bound_syms : k Program_body.Bound_symbols.t)
      (static_part : k Static_part.t)
      : k Static_part.t * DA.t =
  match bound_syms with
  | Singleton result_sym ->
    simplify_static_part_of_kind_value dacc static_part ~result_sym
  | Set_of_closures { set_of_closures_symbol; closure_symbols; } ->
    simplify_static_part_of_kind_fabricated dacc ~result_dacc static_part
      ~set_of_closures_symbol ~closure_symbols

let simplify_static_structure dacc
      ((S pieces) : Program_body.Static_structure.t)
      : DA.t * Program_body.Static_structure.t =
  let str_rev, next_dacc =
    (* The bindings in the individual pieces of the [Static_structure] are
       simultaneous, so we keep a [result_dacc] accumulating the final
       environment, but always use [dacc] for the simplification of the
       pieces. *)
    List.fold_left (fun (str_rev, result_dacc) (bound_syms, static_part) ->
        let static_part, result_dacc =
          simplify_piece_of_static_structure dacc ~result_dacc
            bound_syms static_part
        in
        let str_rev = (bound_syms, static_part) :: str_rev in
        str_rev, result_dacc)
      ([], dacc)
      pieces
  in
  next_dacc, S (List.rev str_rev)

let simplify_definition dacc (defn : Program_body.Definition.t) =
  let dacc, computation, static_structure =
    match defn.computation with
    | None ->
      let dacc, static_structure =
        simplify_static_structure dacc defn.static_structure
      in
      dacc, None, static_structure
    | Some computation ->
      let return_cont_arity = KP.List.arity computation.computed_values in
      let denv = DA.denv dacc in
      let definition_denv = denv in
      assert (Scope.equal (DE.get_continuation_scope_level denv)
        Scope.initial);
      let wrapper_cont_scope = Scope.initial in
      let dummy_cont_scope = Scope.next wrapper_cont_scope in
      let exn_cont_scope = Scope.next dummy_cont_scope in
      let dacc =
        DA.add_exn_continuation dacc computation.exn_continuation
          ~definition_scope_level:exn_cont_scope
      in
      let dummy_cont = Continuation.create () in
      let dacc =
        DA.add_continuation dacc dummy_cont
          ~definition_scope_level:dummy_cont_scope
          return_cont_arity
      in
      let denv =
        DE.increment_continuation_scope_level
          (DE.increment_continuation_scope_level
            (DE.increment_continuation_scope_level denv))
      in
      let dacc = DA.with_denv dacc denv in
      let dummy_cont_handler =
        (* CR mshinwell: Try to rework this so the dummy stuff isn't
           necessary. *)
        (* The handler will never be executed.  It just needs to be an
           expression that cannot be simplified away and must have the
           [computed_values] as free variables.  This will then enable the
           usual unused continuation parameter removal algorithm to work
           automatically on the return continuation. *)
        let handler =
          let apply_cont =
            Apply_cont.create dummy_cont
              ~args:(KP.List.simples computation.computed_values)
          in
          Expr.create_apply_cont apply_cont
        in
        let params_and_handler =
          Continuation_params_and_handler.create computation.computed_values
            ~handler
        in
        Continuation_handler.create ~params_and_handler
          ~stub:false
          ~is_exn_handler:true
      in
      let expr, _dummy_cont_handler, additional_cont_handler,
          (used_computed_values, static_structure, dacc), uacc =
        Simplify_expr.simplify_body_of_non_recursive_let_cont dacc
          computation.return_continuation
          dummy_cont_handler
          ~body:computation.expr
          (fun cont_uses_env r ->
            let definition_denv =
              DE.increment_continuation_scope_level definition_denv
            in
            let definition_denv =
              DE.add_lifted_constants definition_denv
                (R.get_lifted_constants r)
            in
            let typing_env, arg_types =
              CUE.continuation_env_and_arg_types cont_uses_env
                ~definition_typing_env:(DE.typing_env definition_denv)
                computation.return_continuation
            in
            assert (List.compare_lengths arg_types
              computation.computed_values = 0);
            let definition_denv =
              DE.with_typing_environment definition_denv typing_env
            in
            let dacc = DA.create definition_denv cont_uses_env r in
            let dacc =
              DA.map_denv dacc ~f:(fun denv ->
                List.fold_left2 (fun denv ty param ->
                    let var =
                      Var_in_binding_pos.create (KP.var param)
                        Name_occurrence_kind.normal
                    in
                    let kind = KP.kind param in
                    assert (Flambda_kind.equal (T.kind ty) kind);
                    DE.add_variable denv var ty)
                  (DE.with_typing_environment denv typing_env)
                  arg_types computation.computed_values)
            in
            let dacc, static_structure =
              simplify_static_structure dacc defn.static_structure
            in
            let free_variables =
              Static_structure.free_variables static_structure
            in
            let used_computed_values =
              List.map (fun param ->
                  Variable.Set.mem (KP.var param) free_variables)
                computation.computed_values
            in
            let uenv =
              UE.add_continuation UE.empty dummy_cont dummy_cont_scope
                return_cont_arity
            in
            let uenv =
              UE.add_exn_continuation uenv computation.exn_continuation
                exn_cont_scope
            in
            let uacc = UA.create uenv r in
            (used_computed_values, static_structure, dacc), uacc)
      in
      let dacc = DA.with_r dacc (UA.r uacc) in
      let computed_values =
        List.filter_map (fun (param, is_used) ->
            if is_used then Some param else None)
          (List.combine computation.computed_values used_computed_values)
      in
      let return_continuation =
        match additional_cont_handler with
        | Some (cont, _) ->
          assert (not (Continuation.equal cont
            computation.return_continuation));
          cont
        | None -> computation.return_continuation
      in
      let computation_can_be_deleted =
        match Expr.descr expr with
        | Apply_cont apply_cont ->
          begin match Apply_cont.to_goto apply_cont with
          | Some cont when Continuation.equal cont return_continuation ->
            true
          | _ -> false
          end
        | _ -> false
      in
      let computation : Program_body.Computation.t option =
        if computation_can_be_deleted then None
        else
          Some ({
            expr;
            return_continuation;
            exn_continuation = computation.exn_continuation;
            computed_values;
          })
      in
      dacc, computation, static_structure
  in
  let definition : Program_body.Definition.t =
    { static_structure;
      computation;
    }
  in
  definition, dacc

let define_lifted_constants lifted_constants (body : Program_body.t) =
  List.fold_left (fun body lifted_constant : Program_body.t ->
      let static_structure =
        (* CR mshinwell: We should have deletion of unused symbols
           automatically -- needs to be done for non-lifted constants too *)
        Static_structure.delete_bindings
          (Lifted_constant.static_structure lifted_constant)
          ~allowed:(Program_body.free_symbols body)
      in
      if Static_structure.is_empty static_structure then body
      else
        let definition : Program_body.Definition.t =
          { computation = None;
            static_structure;
          }
        in
        Program_body.define_symbol definition ~body)
    body
    lifted_constants

let rec simplify_program_body dacc (body : Program_body.t) =
  match Program_body.descr body with
  | Define_symbol (defn, body) ->
    let dacc = DA.map_r dacc ~f:(fun r -> R.clear_lifted_constants r) in
    let defn, dacc = simplify_definition dacc defn in
    let r = DA.r dacc in
    let body, dacc = simplify_program_body dacc body in
    let body = Program_body.define_symbol defn ~body in
    let body = define_lifted_constants (R.get_lifted_constants r) body in
    body, dacc
  | Root _ -> body, dacc

let check_imported_symbols_don't_overlap_predef_exns
      ~imported_symbols ~predef_exn_symbols ~descr =
  let wrong_symbols =
    Symbol.Set.inter (Symbol.Map.keys imported_symbols)
      (Symbol.Map.keys predef_exn_symbols)
  in
  if not (Symbol.Set.is_empty wrong_symbols) then begin
    Misc.fatal_errorf "Program's [imported_symbols] (%s) must not contain \
        predefined exception symbols"
      descr
  end

let simplify_program denv (program : Program.t) : Program.t =
  let backend = DE.backend denv in
  let module Backend = (val backend : Flambda2_backend_intf.S) in
  let predef_exn_symbols =
    Symbol.Set.fold (fun symbol predef_exn_symbols ->
        Symbol.Map.add symbol K.value predef_exn_symbols)
      Backend.all_predefined_exception_symbols
      Symbol.Map.empty
  in
  let denv =
    Symbol.Map.fold (fun symbol kind denv ->
        DE.add_symbol denv symbol (T.unknown kind))
      (Symbol.Map.disjoint_union program.imported_symbols predef_exn_symbols)
      denv
  in
  check_imported_symbols_don't_overlap_predef_exns
    ~imported_symbols:program.imported_symbols ~predef_exn_symbols
    ~descr:"before simplification";
  let r = R.create ~resolver:(DE.resolver denv) in
  let dacc = DA.create denv Continuation_uses_env.empty r in
  let body, dacc = simplify_program_body dacc program.body in
  let r = DA.r dacc in
  let imported_symbols = R.imported_symbols r in
  check_imported_symbols_don't_overlap_predef_exns
    ~imported_symbols:imported_symbols ~predef_exn_symbols
    ~descr:"after simplification";
  { imported_symbols;
    body;
  }
