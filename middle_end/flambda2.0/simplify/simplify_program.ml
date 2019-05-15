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

module E = Simplify_env_and_result.Env
module K = Flambda_kind
module R = Simplify_env_and_result.Result
module T = Flambda_type

module Of_kind_value = Flambda_static.Of_kind_value
module Program = Flambda_static.Program
module Program_body = Flambda_static.Program_body
module Static_part = Flambda_static.Static_part

module Make
  (Simplify_named : Simplify_named_intf.S)
  (Simplify_toplevel : Simplify_toplevel_intf.S) =
struct
  (* CR-someday mshinwell: Add improved simplification using types (we have
     prototype code to do this). *)

  let simplify_of_kind_value env (of_kind_value : Of_kind_value.t) =
    begin match of_kind_value with
    | Symbol sym -> E.check_symbol_is_bound env sym
    | Tagged_immediate _ -> ()
    | Dynamically_computed var -> E.check_variable_is_bound env var
    end;
    of_kind_value

  let simplify_or_variable env (or_variable : _ Static_part.or_variable) =
    match or_variable with
    | Const _ -> or_variable
    | Var var ->
      E.check_variable_is_bound env var;
      or_variable

  let simplify_static_part env r (static_part : Static_part.t)
        : Static_part.t * R.t =
    match static_part with
    | Block (tag, is_mutable, fields) ->
      let fields =
        List.map (fun of_kind_value -> simplify_of_kind_value env of_kind_value)
          fields
      in
      Block (tag, is_mutable, fields), r
    | Fabricated_block var ->
      E.check_variable_is_bound env var;
      static_part, r
    | Set_of_closures set_of_closures ->
      let set_of_closures, r =
        Simplify_named.simplify_set_of_closures_and_drop_type env r
          set_of_closures
      in
      Set_of_closures set_of_closures, r
    | Closure (sym, closure) ->
      E.check_symbol_is_bound env sym;
      static_part, r
    | Boxed_float or_var -> Boxed_float (simplify_or_variable env or_var), r
    | Boxed_int32 or_var -> Boxed_int32 (simplify_or_variable env or_var), r
    | Boxed_int64 or_var -> Boxed_int64 (simplify_or_variable env or_var), r
    | Boxed_nativeint or_var ->
      Boxed_nativeint (simplify_or_variable env or_var), r
    | Immutable_float_array fields ->
      let fields =
        List.map (fun field -> simplify_or_variable env field) fields
      in
      Immutable_float_array fields, r
    | Mutable_string { initial_value; } ->
      Mutable_string {
        initial_value = simplify_or_variable env initial_value;
      }, r
    | Immutable_string or_var ->
      Immutable_string (simplify_or_variable env or_var), r

  let simplify_computation env r
        (computation : Program_body.Computation.t option) =
    match computation with
    | None -> env, r, None
    | Some computation ->
      let scope_level_for_lifted_constants = E.continuation_scope_level env in
      let return_cont_arity =
        List.map (fun (_var, kind) -> kind) computation.computed_values
      in
      let env =
        E.add_continuation env computation.return_continuation return_cont_arity
      in
      let env = E.add_exn_continuation env computation.exn_continuation in
      let env = E.increment_continuation_scope_level env in
      let previous_r = r in
      let expr, r =
        Simplify_toplevel.simplify_toplevel env r computation.expr
          ~return_continuation:computation.return_continuation
          computation.exn_continuation
          ~scope_level_for_lifted_constants
      in
      let env = E.add_lifted_constants_from_r env r ~previous_r in
      let computation : Program_body.Computation.t option =
        Some ({
          expr;
          return_continuation = computation.return_continuation;
          exn_continuation = computation.exn_continuation;
          computed_values = computation.computed_values;
        })
      in
      env, r, computation

  let simplify_static_structure env r (str : Program_body.Static_structure.t) =
    let str_rev, next_env, result =
      List.fold_left (fun (str_rev, next_env, r)
                ((bound_syms : Program_body.Bound_symbols.t), static_part) ->
          let static_part, r = simplify_static_part env r static_part in
          let str_rev = (bound_syms, static_part) :: str_rev in
          let ty = T.any_value () in
          let next_env =
            match bound_syms with
            | Singleton (sym, kind) -> E.add_symbol next_env sym ty
            | Set_of_closures { set_of_closures_symbol; closure_symbols; } ->
              begin match static_part with
              | Set_of_closures _ | Fabricated_block _ -> ()
              | _ ->
                Misc.fatal_errorf "Illegal static part binding to \
                    set-of-closures symbol %a"
                  Symbol.print set_of_closures_symbol
              end;
              let next_env = E.add_symbol next_env set_of_closures_symbol ty in
              Closure_id.Map.fold (fun _closure_id closure_sym next_env ->
                  E.add_symbol next_env closure_sym (T.any_value ()))
                closure_symbols
                next_env
          in
          str_rev, next_env, r)
        ([], env, r)
        str
    in
    next_env, result, List.rev str_rev

  let simplify_definition env r (defn : Program_body.Definition.t) =
    let env, r, computation = simplify_computation env r defn.computation in
    let env, r, static_structure =
      simplify_static_structure env r defn.static_structure
    in
    let definition : Program_body.Definition.t =
      { static_structure;
        computation;
      }
    in
    definition, env, r

  let rec simplify_program_body env r (body : Program_body.t)
        : Program_body.t * R.t =
    match body with
    | Define_symbol (defn, body) ->
      let defn, env, r = simplify_definition env r defn in
      let body, r = simplify_program_body env r body in
      let body : Program_body.t = Define_symbol (defn, body) in
      body, r
    | Root _ -> body, r

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

  let define_lifted_constants lifted_constants (body : Program_body.t) =
    (* CR mshinwell: Dependencies between lifted constants?  Need to get the
       ordering correct. *)
    Symbol.Map.fold (fun symbol (_ty, kind, static_part) body 
              : Program_body.t ->
        let bound_symbols : Program_body.Bound_symbols.t =
          Singleton (symbol, kind)
        in
        let definition : Program_body.Definition.t =
          { computation = None;
            static_structure = [bound_symbols, static_part];
          }
        in
        Define_symbol (definition, body))
      lifted_constants
      body

  let simplify_program env (program : Program.t) : Program.t =
    let backend = E.backend env in
    let module Backend = (val backend : Flambda2_backend_intf.S) in
    let predef_exn_symbols =
      Symbol.Set.fold (fun symbol predef_exn_symbols ->
          Symbol.Map.add symbol K.value predef_exn_symbols)
        Backend.all_predefined_exception_symbols
        Symbol.Map.empty
    in
    let env =
      Symbol.Map.fold (fun symbol kind env ->
          E.add_symbol env symbol (T.unknown kind))
        (Symbol.Map.disjoint_union program.imported_symbols predef_exn_symbols)
        env
    in
    check_imported_symbols_don't_overlap_predef_exns
      ~imported_symbols:program.imported_symbols ~predef_exn_symbols
      ~descr:"before simplification";
    let r = R.create ~resolver:(E.resolver env) in
    let body, r = simplify_program_body env r program.body in
    let body = define_lifted_constants (R.get_lifted_constants r) body in
    let imported_symbols = R.imported_symbols r in
    check_imported_symbols_don't_overlap_predef_exns
      ~imported_symbols:imported_symbols ~predef_exn_symbols
      ~descr:"after simplification";
    { imported_symbols;
      body;
    }
end
