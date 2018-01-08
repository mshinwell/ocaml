(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Of_kind_value = struct
  include Flambda_static0.Of_kind_value

  let invariant env t =
    let module E = Invariant_env in
    match t with
    | Symbol sym -> E.check_symbol_is_bound env sym
    | Tagged_immediate _ -> ()
    | Dynamically_computed var ->
      E.check_variable_is_bound_and_of_kind env var
        (Flambda_kind.value Unknown)
end

module Static_part = struct
  include Flambda_static0.Static_part

  let invariant env t =
    let module E = Invariant_env in
    match t with
    | Block (_tag, _mut, fields) ->
      List.iter (fun field -> Of_kind_value.invariant env field) fields
    | Set_of_closures set ->
      Flambda.Set_of_closures.invariant env set
    | Closure (sym, _closure_id) ->
      E.check_symbol_is_bound env sym
    | Boxed_float (Var v) ->
      E.check_variable_is_bound_and_of_kind env v
        (Flambda_kind.naked_float ())
    | Boxed_int32 (Var v) ->
      E.check_variable_is_bound_and_of_kind env v
        (Flambda_kind.naked_int32 ())
    | Boxed_int64 (Var v) ->
      E.check_variable_is_bound_and_of_kind env v
        (Flambda_kind.naked_int64 ())
    | Boxed_nativeint (Var v) ->
      E.check_variable_is_bound_and_of_kind env v
        (Flambda_kind.naked_nativeint ())
    | Mutable_string { initial_value = Var v; }
    | Immutable_string (Var v) ->
      E.check_variable_is_bound_and_of_kind env v
        (Flambda_kind.value Definitely_pointer)
    | Boxed_float (Const _)
    | Boxed_int32 (Const _)
    | Boxed_int64 (Const _)
    | Boxed_nativeint (Const _)
    | Mutable_string { initial_value = Const _; }
    | Immutable_string (Const _) -> ()
    | Mutable_float_array { initial_value = fields; }
    | Immutable_float_array fields ->
      List.iter (fun (field : float or_variable) ->
          match field with
          | Var v ->
            E.check_variable_is_bound_and_of_kind env v
              (Flambda_kind.naked_float ())
          | Const _ -> ())
        fields

  module Iterators = struct
    let iter_toplevel_exprs t ~f =
      match t with
      | Set_of_closures set ->
        Flambda.Set_of_closures.Iterators.iter_function_bodies set ~f
      | Block _
      | Closure _
      | Boxed_float _
      | Boxed_int32 _
      | Boxed_int64 _
      | Boxed_nativeint _
      | Mutable_float_array _
      | Immutable_float_array _
      | Mutable_string _
      | Immutable_string _ -> ()

    let iter_sets_of_closures t ~f =
      match t with
      | Set_of_closures set -> f set
      | Block _
      | Closure _
      | Boxed_float _
      | Boxed_int32 _
      | Boxed_int64 _
      | Boxed_nativeint _
      | Mutable_float_array _
      | Immutable_float_array _
      | Mutable_string _
      | Immutable_string _ -> ()
  end

  module Mappers = struct
    let map_toplevel_exprs t ~f : t =
      match t with
      | Set_of_closures set ->
        let set = Flambda.Set_of_closures.Mappers.map_function_bodies set ~f in
        Set_of_closures set
      | Block _
      | Closure _
      | Boxed_float _
      | Boxed_int32 _
      | Boxed_int64 _
      | Boxed_nativeint _
      | Mutable_float_array _
      | Immutable_float_array _
      | Mutable_string _
      | Immutable_string _ -> t

(*
    let map_set_of_closures t ~f =
      (* XXX this doesn't descend recursively.  Change name or semantics *)
      match t with
      | Set_of_closures set -> Set_of_closures (f set)
      | Block _
      | Closure _
      | Boxed_float _
      | Boxed_int32 _
      | Boxed_int64 _
      | Boxed_nativeint _
      | Mutable_float_array _
      | Immutable_float_array _
      | Mutable_string _
      | Immutable_string _ -> t
*)
  end
end

module Program_body = struct
  include Flambda_static0.Program_body

  module Iterators = struct
    let iter_toplevel_exprs_in_definition defn ~f =
      begin match defn.computation with
      | None -> ()
      | Some computation ->
        let continuation_arity =
          List.map (fun (_var, kind) -> kind) computation.computed_values
        in
        f ~continuation_arity computation.return_cont computation.expr;
        Flambda.Expr.Iterators.iter_function_bodies computation.expr ~f
      end;
      List.iter (fun (_sym, static_part) ->
          Static_part.Iterators.iter_toplevel_exprs static_part ~f)
        defn.static_structure

    let rec iter_toplevel_exprs (t : t) ~f =
      match t with
      | Define_symbol (defn, t)
      | Define_symbol_rec (defn, t) ->
        iter_toplevel_exprs_in_definition defn ~f;
        iter_toplevel_exprs t ~f
      | Root _ -> ()

    let iter_sets_of_closures_in_definition defn ~f =
      begin match defn.computation with
      | None -> ()
      | Some computation ->
        Flambda.Expr.Iterators.iter_sets_of_closures f computation.expr
      end;
      List.iter (fun (_sym, static_part) ->
          Static_part.Iterators.iter_sets_of_closures static_part ~f)
        defn.static_structure

    let rec iter_sets_of_closures t ~f =
      match t with
      | Define_symbol (defn, t)
      | Define_symbol_rec (defn, t) ->
        iter_sets_of_closures_in_definition defn ~f;
        iter_sets_of_closures t ~f
      | Root _ -> ()
  end

  module Mappers = struct
    let map_toplevel_exprs_in_definition defn ~f =
      let computation =
        match defn.computation with
        | None -> None
        | Some computation ->
          let continuation_arity =
            List.map (fun (_var, kind) -> kind) computation.computed_values
          in
          let expr =
            Flambda.Expr.Mappers.map_function_bodies ~f computation.expr
          in
          let expr =
            f ~continuation_arity computation.return_cont expr
          in
          Some { computation with expr; }
      in
      let static_structure =
        List.map (fun (sym, static_part) ->
            let static_part =
              Static_part.Mappers.map_toplevel_exprs static_part ~f
            in
            sym, static_part)
          defn.static_structure
      in
      { computation;
        static_structure;
      }

    let rec map_toplevel_exprs t ~f : t =
      match t with
      | Define_symbol (defn, t) ->
        let defn = map_toplevel_exprs_in_definition defn ~f in
        Define_symbol (defn, map_toplevel_exprs t ~f)
      | Define_symbol_rec (defn, t) ->
        let defn = map_toplevel_exprs_in_definition defn ~f in
        Define_symbol_rec (defn, map_toplevel_exprs t ~f)
      | Root _ -> t
  end

  let invariant_define_symbol env defn (recursive : Flambda.recursive) =
    let module E = Invariant_env in
    begin match defn.computation with
    | None -> ()
    | Some computation ->
      (* [Flambda.Expr.invariant] will also catch unbound variables and
         continuations, but we can give a better error message by having
         these two specific checks.  It also gives some amount of testing
         of [free_variables] and [free_continuations] in [Expr]. *)
      let free_variables = Flambda.Expr.free_variables computation.expr in
      if not (Variable.Set.is_empty free_variables) then begin
        Misc.fatal_errorf "Toplevel computation is not closed: %a"
          Flambda.Expr.print computation.expr
      end;
      let free_conts = Flambda.Expr.free_continuations computation.expr in
      if not (Continuation.Set.is_empty free_conts) then begin
        begin match Continuation.Set.get_singleton free_conts with
        | Some cont when Continuation.equal cont computation.return_cont -> ()
        | _ ->
          Misc.fatal_errorf "Toplevel computation has illegal free \
              continuations (%a); the only permitted free continuation is the \
              return continuation, %a"
            Continuation.Set.print free_conts
            Continuation.print computation.return_cont
        end
      end;
      let computation_env =
        let return_arity =
          List.map (fun (_var, kind) -> kind) computation.computed_values
        in
        E.add_continuation env
          computation.return_cont
          return_arity
          Normal
          (E.Continuation_stack.var ())
      in
      Flambda.Expr.invariant computation_env computation.expr;
      List.iter (fun (var, _kind) ->
          if Invariant_env.variable_is_bound env var then begin
            Misc.fatal_errorf "[computed_values] of a toplevel computation \
                must contain fresh variables.  %a is not fresh.  \
                Computation: %a"
              Variable.print var
              Flambda.Expr.print computation.expr;
          end)
        computation.computed_values
    end;
    (* CR mshinwell: lwhite/xclerc have some fixes on the old Flambda in this
       area which we should port forward *)
    let env =
      match recursive with
      | Non_recursive -> env
      | Recursive ->
        List.fold_left (fun env (sym, _static_part) ->
            let ty =
              Flambda_type.unknown (Flambda_kind.value Definitely_immediate)
            in
            E.add_symbol env sym ty)
          env
          defn.static_structure
    in
    let allowed_fns =
      match defn.computation with
      | None -> Name.Set.empty
      | Some computation ->
        Name.Set.of_list (
          List.map (fun (var, _kind) -> Name.var var)
            computation.computed_values)
    in
    List.iter (fun (sym, static_part) ->
        let free_names = Static_part.free_names static_part in
        (* This will also be caught by [invariant_static_part], but will
           give a better message; and allows some testing of
           [Static_part.free_variables]. *)
        if not (Name.Set.subset free_names allowed_fns) then begin
          Misc.fatal_errorf "Static part is only allowed to reference \
              the following free names: { %a }, whereas it references \
              { %a }.  Static part: %a = %a"
            Name.Set.print free_names
            Name.Set.print allowed_fns
            Symbol.print sym
            Static_part.print static_part
        end;
        Static_part.invariant env static_part)
      defn.static_structure;
    List.fold_left (fun env (sym, _static_part) ->
        let ty =
          Flambda_type.unknown (Flambda_kind.value Definitely_immediate)
        in
        match recursive with
        | Non_recursive -> E.add_symbol env sym ty
        | Recursive ->
          (* If we ever store data about symbols, this place needs updating
             to do a "redefine_symbol" operation on [env]. *)
          env)
      env
      defn.static_structure

  let rec invariant env t =
    let module E = Invariant_env in
    match t with
    | Define_symbol (defn, t) ->
      let env = invariant_define_symbol env defn Non_recursive in
      invariant env t
    | Define_symbol_rec (defn, t) ->
      let env = invariant_define_symbol env defn Recursive in
      invariant env t
    | Root sym -> E.check_symbol_is_bound env sym
end

module Program = struct
  include Flambda_static0.Program

  let imported_symbols t = t.imported_symbols

  let root_symbol t =
    let rec loop (body : Program_body.t) =
      match body with
      | Define_symbol (_, body)
      | Define_symbol_rec (_, body) -> loop body
      | Root root -> root
    in
    loop t.body

  module Iterators = struct
    let iter_sets_of_closures t ~f =
      Program_body.Iterators.iter_sets_of_closures t.body ~f

(*
    let iter_constant_defining_values (program : t) ~f =
      let rec loop (program : Program_body.t) =
        match program with
        | Let_symbol (_, const, program) ->
          f const;
          loop program
        | Let_rec_symbol (defs, program) ->
          List.iter (fun (_, const) -> f const) defs;
          loop program
        | Initialize_symbol (_, _, program) ->
          loop program
        | Effect (_, _, program) ->
          loop program
        | End _ -> ()
      in
      loop program.program_body

    module Toplevel_only = struct
      let iter_exprs (program : t) ~f =
        let rec loop (program : Program_body.t) =
          match program with
          | Let_symbol (_, Set_of_closures set_of_closures, program) ->
            Closure_id.Map.iter
              (fun _ (function_decl : F.Function_declaration.t) ->
                f ~continuation_arity:function_decl.return_arity
                  function_decl.continuation_param function_decl.body)
              set_of_closures.function_decls.funs;
            loop program
          | Let_rec_symbol (defs, program) ->
            List.iter (function
                | (_, CDV.Set_of_closures set_of_closures) ->
                  Closure_id.Map.iter
                    (fun _ (function_decl : F.Function_declaration.t) ->
                      f ~continuation_arity:function_decl.return_arity
                        function_decl.continuation_param function_decl.body)
                    set_of_closures.function_decls.funs
                | _ -> ()) defs;
            loop program
          | Let_symbol (_, _, program) ->
            loop program
          | Initialize_symbol (_, descr, program) ->
            f ~continuation_arity:descr.return_arity descr.return_cont
              descr.expr;
            loop program
          | Effect (expr, cont, program) ->
            f ~continuation_arity:[] cont expr;
            loop program
          | End _ -> ()
        in
        loop program.program_body
    end 
*)

    let iter_toplevel_exprs (t : t) ~f =
      Program_body.Iterators.iter_toplevel_exprs t.body ~f

    let iter_apply t ~f =
      iter_toplevel_exprs t
        ~f:(fun ~continuation_arity:_ _cont expr ->
          Flambda.Expr.Iterators.iter (function
              | Apply apply -> f apply
              | _ -> ())
            (fun _ -> ())
            expr)

    let iter_named t ~f =
      iter_toplevel_exprs t ~f:(fun ~continuation_arity:_ _ e ->
        Flambda.Expr.Iterators.iter_named f e)
  end

  module Mappers = struct
    let map_toplevel_exprs t ~f =
      { t with body = Program_body.Mappers.map_toplevel_exprs t.body ~f; }
  end

(*
    let map_sets_of_closures (program : t)
          ~(f : F.Set_of_closures.t -> F.Set_of_closures.t) =
      let rec loop (program : Program_body.t)
            : Program_body.t =
        let map_constant_set_of_closures (set_of_closures:F.Set_of_closures.t) =
          let done_something = ref false in
          let function_decls =
            let funs =
              Closure_id.Map.map
                (fun (function_decl : F.Function_declaration.t) ->
                  let body =
                    Flambda.Expr.Mappers.map_sets_of_closures
                      function_decl.body ~f
                  in
                  if body == function_decl.body then
                    function_decl
                  else begin
                    done_something := true;
                    F.Function_declaration.update_body function_decl ~body
                  end)
                set_of_closures.function_decls.funs
            in
            if not !done_something then
              set_of_closures.function_decls
            else
              F.Function_declarations.update set_of_closures.function_decls
                ~funs
          in
          let new_set_of_closures = f set_of_closures in
          if new_set_of_closures == set_of_closures then
            set_of_closures
          else
            F.Set_of_closures.create ~function_decls
              ~free_vars:set_of_closures.free_vars
              ~direct_call_surrogates:set_of_closures.direct_call_surrogates
        in
        match program with
        | Let_symbol (symbol, Set_of_closures set_of_closures, program') ->
          let new_set_of_closures =
            map_constant_set_of_closures set_of_closures
          in
          let new_program' = loop program' in
          if new_set_of_closures == set_of_closures
              && new_program' == program' then
            program
          else
            let const = CDV.create_set_of_closures new_set_of_closures in
            Let_symbol (symbol, const, new_program')
        | Let_symbol (symbol, const, program') ->
          let new_program' = loop program' in
          if new_program' == program' then
            program
          else
            Let_symbol (symbol, const, new_program')
        | Let_rec_symbol (defs, program') ->
          let done_something = ref false in
          let defs =
            List.map (function
                | (var, CDV.Set_of_closures set_of_closures) ->
                  let new_set_of_closures =
                    map_constant_set_of_closures set_of_closures
                  in
                  if not (new_set_of_closures == set_of_closures) then begin
                    done_something := true
                  end;
                  let const = CDV.create_set_of_closures new_set_of_closures in
                  var, const
                | def -> def)
              defs
          in
          let new_program' = loop program' in
          if new_program' == program' && not !done_something then
            program
          else
            Let_rec_symbol (defs, loop program')
        | Initialize_symbol (symbol, descr, program') ->
          let done_something = ref false in
          let descr : Program_body.Initialize_symbol.t =
            let new_expr =
              Flambda.Expr.Mappers.map_sets_of_closures descr.expr ~f
            in
            if not (new_expr == descr.expr) then begin
              done_something := true
            end;
            { descr with
              expr = new_expr;
            }
          in
          let new_program' = loop program' in
          if new_program' == program' && not !done_something then
            program
          else
            Initialize_symbol (symbol, descr, new_program')
        | Effect (expr, cont, program') ->
          let new_expr =
            Flambda.Expr.Mappers.map_sets_of_closures expr ~f
          in
          let new_program' = loop program' in
          if new_expr == expr && new_program' == program' then
            program
          else
            Effect (new_expr, cont, new_program')
        | End _ -> program
      in
      { program with
        program_body = loop program.program_body;
      }

    let map_toplevel_exprs (program : t) ~(f : F.Expr.t -> F.Expr.t) =
      let rec loop (program : Program_body.t) : Program_body.t =
        let map_constant_set_of_closures
              (set_of_closures : F.Set_of_closures.t) =
          let done_something = ref false in
          let funs =
            Closure_id.Map.map
              (fun (function_decl : F.Function_declaration.t) ->
                let body = f function_decl.body in
                if body == function_decl.body then
                  function_decl
                else begin
                  done_something := true;
                  F.Function_declaration.update_body function_decl ~body
                end)
              set_of_closures.function_decls.funs
          in
          if not !done_something then
            set_of_closures
          else
            let function_decls =
              F.Function_declarations.update set_of_closures.function_decls
                ~funs
            in
            F.Set_of_closures.create ~function_decls
              ~free_vars:set_of_closures.free_vars
              ~direct_call_surrogates:set_of_closures.direct_call_surrogates
        in
        (* CR-soon mshinwell: code very similar to the above function *)
        match program with
        | Let_symbol (symbol, Set_of_closures set_of_closures, program') ->
          let new_set_of_closures =
            map_constant_set_of_closures set_of_closures
          in
          let new_program' = loop program' in
          if new_set_of_closures == set_of_closures
              && new_program' == program' then
            program
          else
            let const = CDV.create_set_of_closures new_set_of_closures in
            Let_symbol (symbol, const, new_program')
        | Let_symbol (symbol, const, program') ->
          let new_program' = loop program' in
          if new_program' == program' then
            program
          else
            Let_symbol (symbol, const, new_program')
        | Let_rec_symbol (defs, program') ->
          let done_something = ref false in
          let defs =
            List.map (function
                | (var, CDV.Set_of_closures set_of_closures) ->
                  let new_set_of_closures =
                    map_constant_set_of_closures set_of_closures
                  in
                  if not (new_set_of_closures == set_of_closures) then begin
                    done_something := true
                  end;
                  let const = CDV.create_set_of_closures new_set_of_closures in
                  var, const
                | def -> def)
              defs
          in
          let new_program' = loop program' in
          if new_program' == program' && not !done_something then
            program
          else
            Let_rec_symbol (defs, new_program')
        | Initialize_symbol (symbol, descr, program') ->
          let done_something = ref false in
          let descr : Program_body.Initialize_symbol.t =
            let new_expr = f descr.expr in
            if not (new_expr == descr.expr) then begin
              done_something := true
            end;
            { descr with
              expr = new_expr;
            }
          in
          let new_program' = loop program' in
          if new_program' == program' && not !done_something then
            program
          else
            Initialize_symbol (symbol, descr, new_program')
        | Effect (expr, cont, program') ->
          let new_expr = f expr in
          let new_program' = loop program' in
          if new_expr == expr && new_program' == program' then
            program
          else
            Effect (new_expr, cont, new_program')
        | End _ -> program
      in
      { program with
        program_body = loop program.program_body;
      }

    let map_named t ~(f : Variable.t -> F.Named.t -> F.Named.t) =
      map_toplevel_exprs t ~f:(fun expr ->
        Flambda.Expr.Mappers.map_named_with_id f expr)
  end

  let all_sets_of_closures program =
    let list = ref [] in
    Iterators.iter_sets_of_closures program
      ~f:(fun set_of_closures ->
          list := set_of_closures :: !list);
    !list

  let all_sets_of_closures_map program =
    let r = ref Set_of_closures_id.Map.empty in
    Iterators.iter_sets_of_closures program
      ~f:(fun set_of_closures ->
        r := Set_of_closures_id.Map.add
            set_of_closures.function_decls.set_of_closures_id
            set_of_closures !r);
    !r

  let all_function_decls_indexed_by_set_of_closures_id program =
    Set_of_closures_id.Map.map
      (fun { Flambda.Set_of_closures. function_decls; _ } -> function_decls)
      (all_sets_of_closures_map program)

  let all_lifted_constants (program : t) =
    let rec loop (program : Program_body.t) =
      match program with
      | Let_symbol (symbol, decl, program) -> (symbol, decl) :: (loop program)
      | Let_rec_symbol (decls, program) ->
        List.fold_left (fun l (symbol, decl) -> (symbol, decl) :: l)
          (loop program)
          decls
      | Initialize_symbol (_, _, program)
      | Effect (_, _, program) -> loop program
      | End _ -> []
    in
    loop program.program_body

  let all_lifted_constant_sets_of_closures program =
    let set = ref Set_of_closures_id.Set.empty in
    List.iter (function
        | (_, CDV.Set_of_closures {
            function_decls = { set_of_closures_id } }) ->
          set := Set_of_closures_id.Set.add set_of_closures_id !set
        | _ -> ())
      (all_lifted_constants program);
    !set

  let all_lifted_constants_as_map program =
    Symbol.Map.of_list (all_lifted_constants program)

  let introduce_needed_import_symbols program : t =
    { program with
      imported_symbols = needed_import_symbols program;
    }
*)

  let invariant t =
    let module E = Invariant_env in
    let every_used_function_from_current_unit_is_declared env t =
      let current_compilation_unit = Compilation_unit.get_current_exn () in
      let not_declared = E.closure_ids_not_declared env in
      let not_declared_from_current_unit =
        Closure_id.Set.filter (fun cu ->
            Closure_id.in_compilation_unit cu current_compilation_unit)
          not_declared
      in
      if not (Closure_id.Set.is_empty not_declared_from_current_unit) then begin
        Misc.fatal_errorf "Closure ID(s) { %a } from the current compilation \
            unit is/are referenced but not declared: %a"
          Closure_id.Set.print not_declared
          print t
      end
    in
    let every_used_var_within_closure_from_current_unit_is_declared env t =
      let current_compilation_unit = Compilation_unit.get_current_exn () in
      let not_declared = E.var_within_closures_not_declared env in
      let not_declared_from_current_unit =
        Var_within_closure.Set.filter (fun cu ->
            Var_within_closure.in_compilation_unit cu current_compilation_unit)
          not_declared
      in
      if not (Var_within_closure.Set.is_empty not_declared_from_current_unit)
      then begin
        Misc.fatal_errorf "Closure ID(s) { %a } from the current compilation \
            unit is/are referenced but not declared: %a"
          Var_within_closure.Set.print not_declared
          print t
      end
    in
    let env =
      Symbol.Set.fold (fun symbol env ->
          let ty =
            Flambda_type.unknown (Flambda_kind.value Definitely_immediate)
          in
          E.add_symbol env symbol ty)
        t.imported_symbols
        (E.create ())
    in
    Program_body.invariant env t.body;
    every_used_function_from_current_unit_is_declared env t;
    every_used_var_within_closure_from_current_unit_is_declared env t
end


(*

(*
  let declare_simple t static_part =
    let symbol = Symbol.create "boxed_float" in
    let definition =
      { static_structure = [symbol, static_part];
        computation = None;
      }
    in
    let definition_group =
      { recursive = Non_recursive;
        definitions = [definition];
      }
    in
    { t with
      definitions = definition_group :: t.definitions;
    }

  let declare_boxed_float t f = declare_simple t (Boxed_float (Const f))
  let declare_boxed_int32 t n = declare_simple t (Boxed_int32 (Const n))
  let declare_boxed_int64 t n = declare_simple t (Boxed_int64 (Const n))
  let declare_boxed_nativeint t n = declare_simple t (Boxed_nativeint (Const n))

  let declare_immutable_string t s =
    declare_simple t (Immutable_string (Const s))

  let declare_mutable_string t ~initial_value =
    declare_simple t (Immutable_string (Const { initial_value; }))

  let declare_float_array t fs =
    let fs = List.map (fun f : _ or_variable -> Const f) fs in
    declare_simple t (Immutable_float_array fs)

  let declare_block t tag fields =
    let fields = List.map (fun s : Field_of_kind_value.t -> Symbol s) fields in
    declare_simple t (Block (tag, fields))

  let declare_single_field_block_pointing_at t thing kind =
    let field : Field_of_kind_value.t = Dynamically_computed thing in
    declare_simple t (Block (Tag.Scannable.zero, [field]))
*)
*)
