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

type 'a or_invalid =
  | Ok of 'a
  | Invalid

module Make
  (Simplify_named : Simplify_named_intf.S)
  (Simplify_toplevel : Simplify_toplevel_intf.S) =
struct
  let simplify_static_structure initial_env (recursive : Recursive.t) str =
    let unreachable, env, str =
      List.fold_left
        (fun ((now_unreachable, env, str) as acc) (sym, kind, static_part) ->
          if now_unreachable then
            acc
          else (* CR mshinwell: At least check the static part *)
            let ty = T.any_value () in
            let env =
              match recursive with
              | Non_recursive -> E.add_symbol env sym ty
              | Recursive -> E.redefine_symbol env sym ty
            in
            false, env, ((sym, kind, static_part) :: str))
        (false, initial_env, [])
        str
    in
    unreachable, env, List.rev str

  let initial_environment_for_recursive_symbols env
        (defn : Program_body.definition) =
    let env =
      List.fold_left (fun env (symbol, kind, _static_part) ->
          E.add_symbol env symbol (T.unknown kind))
        env defn.static_structure
    in
    let _unreachable, env, _str =
      simplify_static_structure env Recursive defn.static_structure
    in
    env

  let simplify_define_symbol env (recursive : Recursive.t)
        (defn : Program_body.definition) =
    let env, computation, newly_imported_symbols, lifted_constants =
      match defn.computation with
      | None -> env, defn.computation, Symbol.Map.empty, Symbol.Map.empty
      | Some computation ->
        let name = computation.return_cont in
        let return_cont_params =
          List.map (fun (var, kind) ->
              let param = Parameter.wrap var in
              let ty = T.unknown kind in
              Flambda.Typed_parameter.create param ty)
            computation.computed_values
        in
        let return_cont_approx =
          Continuation_approx.create_unknown ~name ~params:return_cont_params
        in
        let exn_cont_approx =
          Continuation_approx.create_unknown ~name:computation.exception_cont
            ~params:(Simplify_aux.params_for_exception_handler ())
        in
        let expr, r, continuation_uses, lifted_constants =
          let scope_level_for_lifted_constants = E.continuation_scope_level env in
          let env = E.add_continuation env name return_cont_approx in
          let env =
            E.add_continuation env computation.exception_cont exn_cont_approx
          in
          let env = E.increment_continuation_scope_level env in
          let r = R.create ~resolver:(E.resolver env) in
          let descr =
            let symbol_names =
              List.map (fun (sym, _, _) ->
                  Format.asprintf "%a" Symbol.print sym)
                defn.static_structure
            in
            Printf.sprintf "Toplevel binding(s) of: %s"
              (String.concat "+" symbol_names)
          in
          Simplify_toplevel.simplify_toplevel env r computation.expr
            ~continuation:name
            ~continuation_params:return_cont_params
            ~exn_continuation:computation.exception_cont
            ~descr
            ~scope_level_for_lifted_constants
        in
        (* CR mshinwell: Add unboxing of the continuation here.  This will look
           like half of Unbox_returns (same analysis and the same thing to
           happen to [expr]; but instead of generating a function wrapper, we
           need to do something else here).  Note that the linearity check
           for Unbox_returns will enable us to handle mutable returned values
           too. *)
        let env =
          Symbol.Map.fold (fun symbol (ty, _kind, _static_part) env ->
              E.add_symbol env symbol ty)
            lifted_constants
            env
        in
        let _args_types, new_env, _env_extension =
          (* CR mshinwell: move to auxiliary function; share with
             [Simplify_named]? *)
          let default_env =
            List.fold_left (fun env param ->
                let var = Flambda.Typed_parameter.var param in
                let scope_level = Scope_level.initial in
                let ty = Flambda.Typed_parameter.ty param in
                T.Typing_env.add env (Name.var var) scope_level (Definition ty))
              (E.get_typing_environment env)
              return_cont_params
          in
          try
            Join_point.param_types_and_body_env continuation_uses
              ~arity:(Flambda.Typed_parameter.List.arity return_cont_params)
              (E.freshening env)
              ~default_env
          with Misc.Fatal_error as exn -> begin
            Format.eprintf "\n%sContext: Term resulting from \
                [simplify_toplevel]:%s@ %a@ \
                Default environment:@ %a\n%!"
              (Misc_color.bold_red ())
              (Misc_color.reset ())
              Flambda.Expr.print expr
              E.print env;
            raise exn
          end
        in
        let env = E.replace_typing_environment env new_env in
        let computation =
          match expr with
          | Apply_cont (cont, None, []) ->
            assert (Continuation.equal cont computation.return_cont);
            None
          | _ ->
            Some ({
              expr;
              return_cont = computation.return_cont;
              exception_cont = computation.exception_cont;
              computed_values = computation.computed_values;
            } : Program_body.computation)
        in
        env, computation, R.newly_imported_symbols r, lifted_constants
    in
    let env =
      match recursive with
      | Non_recursive -> env
      | Recursive -> initial_environment_for_recursive_symbols env defn
    in
    let unreachable, env, static_structure =
      simplify_static_structure env recursive defn.static_structure
    in
    (* CR mshinwell: [unreachable] should also be set to [true] if
       [computation] is [Some (Invalid _)]. *)
    let computation, static_structure =
      (* CR-someday mshinwell: We could imagine propagating an "unreachable"
         (if that's what [invalid ()] turns into, rather than a trap) back to
         previous [Define_symbol]s. *)
      if not unreachable then
        computation, static_structure
      else
        match computation with
        | None ->
          let computation : Program_body.computation =
            { expr = Flambda.Expr.invalid ();
              return_cont = Continuation.create ();
              exception_cont = Continuation.create ();
              computed_values = [];
            }
          in
          Some computation, []
        | Some computation ->
          let params =
            List.map (fun (var, kind) ->
                let param = Parameter.wrap var in
                Flambda.Typed_parameter.create param (T.unknown kind))
              computation.computed_values
          in
          let expr : Flambda.Expr.t =
            Let_cont {
              body = computation.expr;
              handlers = Non_recursive {
                name = computation.return_cont;
                handler = {
                  params;
                  stub = false;
                  is_exn_handler = false;
                  handler = Flambda.Expr.invalid ();
                };
              };
            }
          in
          let new_return_cont =
            (* This continuation will never be called. *)
            Continuation.create ()
          in
          let computation : Program_body.computation =
            { expr;
              return_cont = new_return_cont;
              (* CR mshinwell: Think more about exception continuations here *)
              exception_cont = computation.exception_cont;
              computed_values = [];
            }
          in
          Some computation, []
    in
    let definition : Program_body.definition =
      { static_structure;
        computation;
      }
    in
    definition, env, newly_imported_symbols, lifted_constants

  let add_lifted_constants lifted_constants (body : Program_body.t) =
    (* CR mshinwell: Dependencies between lifted constants?  Need to get the
       ordering correct. *)
    List.fold_left (fun body (symbol, (_ty, kind, constant)) : Program_body.t ->
        let definition : Program_body.definition =
          { computation = None;
            static_structure = [symbol, kind, constant];
          }
        in
        Define_symbol (definition, body))
      body
      (Symbol.Map.bindings lifted_constants)

  let rec simplify_program_body env (body : Program_body.t)
        : Program_body.t * (K.t Symbol.Map.t) =
    match body with
    | Define_symbol (defn, body) ->
      let defn, env, newly_imported_symbols1, lifted_constants =
        simplify_define_symbol env Non_recursive defn
      in
      let body, newly_imported_symbols2 =
        simplify_program_body env body
      in
      let newly_imported_symbols =
        Symbol.Map.disjoint_union newly_imported_symbols1 newly_imported_symbols2
      in
      let body : Program_body.t =
        Define_symbol (defn, body)
      in
      add_lifted_constants lifted_constants body, newly_imported_symbols
    | Root _ -> body, Symbol.Map.empty

  let simplify_program env (program : Program.t) =
    let backend = E.backend env in
    let module Backend = (val backend : Backend_intf.S) in
    let predef_exn_symbols =
      Symbol.Set.fold (fun symbol predef_exn_symbols ->
          Symbol.Map.add symbol (K.value ()) predef_exn_symbols)
        (Backend.all_predefined_exception_symbols ())
        Symbol.Map.empty
    in
    let env =
      Symbol.Map.fold (fun symbol kind env ->
          E.add_symbol env symbol (T.unknown kind))
        (Symbol.Map.disjoint_union program.imported_symbols predef_exn_symbols)
        env
    in
    let body, newly_imported_symbols = simplify_program_body env program.body in
    { program with body; }, newly_imported_symbols
end
