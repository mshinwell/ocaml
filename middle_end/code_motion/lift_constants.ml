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

(* XXX vlaviron is going to try a new implementation based on Reachability *)

(*

(* XXX we need to make sure that e.g.
   Ptag_immediate (Const (Untagged_immediate 42)) is treated as a constant *)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module CDV = Flambda_static.Constant_defining_value

(*
(* CR-someday mshinwell: move to Flambda_utils *)
let rec tail_variable : Flambda.Expr.t -> Variable.t option = function
  | Let_mutable { body = e }
  | Let { body = e; _ } -> tail_variable e
  | _ -> None
*)

let closure_symbol ~(backend : (module Backend_intf.S)) closure_id =
  let module Backend = (val backend) in
  Backend.closure_symbol closure_id

let make_variable_symbol prefix var =
  Symbol.create (Compilation_unit.get_current_exn ())
    (Linkage_name.create
       (prefix ^ Variable.unique_name (Variable.rename var)))

let closure_id_set_singleton closure_id =
  match Closure_id.Set.get_singleton closure_id with
  | None ->
    Misc.fatal_errorf
      "A closure_id singleton set was expected, but this set is %a"
      Closure_id.Set.print closure_id
  | Some closure_id ->
    closure_id

let closure_id_map_singleton print map =
  match Closure_id.Map.get_singleton map with
  | None ->
    Misc.fatal_errorf
      "A closure_id singleton map was expected, but this map is %a"
      (Closure_id.Map.print print) map
  | Some (k, v) ->
    k, v

(** Traverse the given expression assigning symbols to [let]- and [let rec]-
    bound constant variables.  At the same time collect the definitions of
    such variables. *)
let assign_symbols_and_collect_constant_definitions
    ~(backend : (module Backend_intf.S))
    ~(program : Flambda_static.Program.t)
    ~(inconstants : Inconstant_idents.result) =
  let var_to_symbol_tbl = Variable.Tbl.create 42 in
  let var_to_definition_tbl = Variable.Tbl.create 42 in
  let module AA = Alias_analysis in
  let assign_symbol var (named : Flambda.Named.t) =
    if not (Inconstant_idents.variable var inconstants) then begin
      let assign_symbol () =
        let symbol = make_variable_symbol "" var in
        Variable.Tbl.add var_to_symbol_tbl var symbol
      in
      let assign_existing_symbol = Variable.Tbl.add var_to_symbol_tbl var in
      let record_definition = Variable.Tbl.add var_to_definition_tbl var in
      match named with
      | Var var -> record_definition (AA.Variable var)
      | Symbol symbol ->
        assign_existing_symbol symbol;
        record_definition (AA.Symbol symbol)
      | Const (Tagged_immediate imm) ->
        record_definition (AA.Tagged_immediate imm)
      | Const (Untagged_immediate _ | Naked_float _ | Naked_int32 _
          | Naked_int64 _ | Naked_nativeint _) ->
        (* [Inconstant_idents] always marks these expressions as
           inconstant, so we should never get here. *)
        assert false
      | Allocated_const const ->
        assign_symbol ();
        record_definition (AA.Allocated_const (Normal const))
      | Read_mutable _ | Assign _ ->
        (* [Inconstant_idents] always marks these expressions as
           inconstant, so we should never get here. *)
        assert false
      | Prim (Pmakeblock (tag, _, _value_kind), fields, _) ->
        assign_symbol ();
        record_definition (AA.Block (Tag.Scannable.create_exn tag, fields))
      | Read_symbol_field (symbol, field) ->
        record_definition (AA.Symbol_field (symbol, field))
      | Set_of_closures (
          { function_decls = { funs; set_of_closures_id; _ };
            _ } as set) ->
        assert (not (Inconstant_idents.closure set_of_closures_id
                       inconstants));
        assign_symbol ();
        record_definition (AA.Set_of_closures set);
        Variable.Map.iter (fun fun_var _ ->
            let closure_id = Closure_id.wrap fun_var in
            let closure_symbol = closure_symbol ~backend closure_id in
            Variable.Tbl.add var_to_symbol_tbl fun_var closure_symbol;
            let project_closure =
              Alias_analysis.Project_closure
                { set_of_closures = var;
                  closure_id = Closure_id.Set.singleton closure_id }
            in
            Variable.Tbl.add var_to_definition_tbl fun_var
              project_closure)
          funs
      | Move_within_set_of_closures ({ closure = _; move; }
          as projection) ->
        let (_, move_to) = closure_id_map_singleton Closure_id.print move in
        assign_existing_symbol (closure_symbol ~backend move_to);
        record_definition (AA.Move_within_set_of_closures projection)
      | Project_closure ({ closure_id } as project_closure) ->
        let closure_id = closure_id_set_singleton closure_id in
        assign_existing_symbol (closure_symbol ~backend  closure_id);
        record_definition (AA.Project_closure project_closure)
      | Prim (Pfield index, [block], _) ->
        record_definition (AA.Field (block, index))
      | Prim (Pfield _, _, _) ->
        Misc.fatal_errorf "[Pfield] with the wrong number of arguments"
          Flambda.Named.print named
      | Prim (Pmakearray (Pfloatarray as kind, mutability), args, _) ->
        assign_symbol ();
        record_definition (AA.Allocated_const (Array (kind, mutability, args)))
      | Prim (Pduparray (kind, mutability), [arg], _) ->
        assign_symbol ();
        record_definition (AA.Allocated_const (
          Duplicate_array (kind, mutability, arg)))
      | Prim _ ->
        Misc.fatal_errorf "Primitive not expected to be constant: @.%a@."
          Flambda.Named.print named
      | Project_var project_var ->
        let (closure_id, var) =
          closure_id_map_singleton Var_within_closure.print project_var.var
        in
        record_definition (AA.Project_var {
          closure = project_var.closure;
          closure_id;
          var; })
    end
  in
  let assign_symbol_program ~continuation_arity:_ _cont expr =
    Flambda.Expr.Iterators.iter_all_immutable_let_and_let_rec_bindings expr
      ~f:assign_symbol
  in
  Flambda_static.Program.Iterators.Toplevel_only.iter_exprs program
    ~f:assign_symbol_program;
  let let_symbol_to_definition_tbl = Symbol.Tbl.create 42 in
  let initialize_symbol_to_definition_tbl = Symbol.Tbl.create 42 in
  let rec collect_let_and_initialize_symbols
          (program : Flambda_static.Program_body.t) =
    match program with
    | Let_symbol (symbol, decl, program) ->
      Symbol.Tbl.add let_symbol_to_definition_tbl symbol decl;
      collect_let_and_initialize_symbols program
    | Let_rec_symbol (decls, program) ->
      List.iter (fun (symbol, decl) ->
          Symbol.Tbl.add let_symbol_to_definition_tbl symbol decl)
        decls;
      collect_let_and_initialize_symbols program
    | Effect (_, _, program) -> collect_let_and_initialize_symbols program
    | Initialize_symbol (symbol, descr, program) ->
      collect_let_and_initialize_symbols program;
      begin match descr with
      | Values { tag = _; fields; } ->
        let fields =
          (* CR mshinwell: This "None" used to be "tail_variable field".
             We need to think about this. *)
          List.map (fun (_field, _scanning, _cont) -> None) fields
        in
        Symbol.Tbl.add initialize_symbol_to_definition_tbl symbol fields
      | Float _ ->
        (* CR mshinwell for pchambart: Need to decide what to do. (Possibly
           nothing, since [Symbol]s are currently always [Value].) *)
        assert false
      | Int32 (_expr, _cont)
      | Int64 (_expr, _cont)
      | Nativeint (_expr, _cont) ->
        (* CR mshinwell for pchambart: Need to decide what to do. (Possibly
           nothing, since [Symbol]s are currently always [Value].) *)
        assert false
      end
    | End _ -> ()
  in
  collect_let_and_initialize_symbols program.program_body;
  let record_set_of_closure_equalities
        (set_of_closures : Flambda.Set_of_closures.t) =
    Variable.Map.iter (fun arg (free_var : Flambda.Free_var.t) ->
        if not (Inconstant_idents.variable arg inconstants) then
          Variable.Tbl.add var_to_definition_tbl arg
            (AA.Variable free_var.var))
      set_of_closures.free_vars
    (* XXX need to traverse parameters *)
(*
    Variable.Map.iter (fun arg (spec_to : Flambda.specialised_to) ->
        match spec_to.var with
        | None -> ()
        | Some var ->
          if not (Inconstant_idents.variable arg inconstants) then
            Variable.Tbl.add var_to_definition_tbl arg (AA.Variable var))
      set_of_closures.specialised_args
*)
  in
  Flambda_static.Program.Iterators.iter_set_of_closures program
    ~f:(fun ~constant set_of_closures ->
      record_set_of_closure_equalities set_of_closures;
      if constant then begin
        Variable.Map.iter (fun fun_var _ ->
            let closure_id = Closure_id.wrap fun_var in
            let closure_symbol = closure_symbol ~backend closure_id in
            Variable.Tbl.add var_to_definition_tbl fun_var
              (AA.Symbol closure_symbol);
            Variable.Tbl.add var_to_symbol_tbl fun_var closure_symbol)
          set_of_closures.function_decls.funs
      end);
  var_to_symbol_tbl, var_to_definition_tbl,
    let_symbol_to_definition_tbl, initialize_symbol_to_definition_tbl

let variable_field_definition
    (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl :
      Alias_analysis.constant_defining_value Variable.Tbl.t)
    (var : Variable.t)
    : Flambda_static.Constant_defining_value_block_field.t =
  try
    Symbol (Variable.Tbl.find var_to_symbol_tbl var)
  with Not_found ->
    match Variable.Tbl.find var_to_definition_tbl var with
    | Tagged_immediate c -> Tagged_immediate c
    | const_defining_value ->
      Misc.fatal_errorf "Unexpected pattern for a constant: %a: %a"
        Variable.print var
        Alias_analysis.print_constant_defining_value const_defining_value
    | exception Not_found ->
      Misc.fatal_errorf "No associated symbol for the constant %a"
        Variable.print var

let resolve_variable
    (aliases : Alias_analysis.allocation_point Variable.Map.t)
    (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl :
      Alias_analysis.constant_defining_value Variable.Tbl.t)
    (var : Variable.t)
    : Flambda_static.Constant_defining_value_block_field.t =
  match Variable.Map.find var aliases with
  | exception Not_found ->
    variable_field_definition var_to_symbol_tbl var_to_definition_tbl var
  | Symbol s -> Symbol s
  | Variable aliased_variable ->
    variable_field_definition var_to_symbol_tbl var_to_definition_tbl
      aliased_variable

let translate_set_of_closures
    (inconstants : Inconstant_idents.result)
    (aliases : Alias_analysis.allocation_point Variable.Map.t)
    (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl:
      Alias_analysis.constant_defining_value Variable.Tbl.t)
    (set_of_closures : Flambda.Set_of_closures.t) =
  let f var (named : Flambda.Named.t) : Flambda.Named.t =
    if Inconstant_idents.variable var inconstants then
      named
    else
      let resolved =
        resolve_variable
          aliases
          var_to_symbol_tbl
          var_to_definition_tbl
          var
      in
      match resolved with
      | Symbol s -> Symbol s
      | Tagged_immediate i -> Const (Tagged_immediate i)
  in
  Flambda.Set_of_closures.Mappers.map_function_bodies set_of_closures
    ~f:(Flambda.Expr.Mappers.map_all_immutable_let_and_let_rec_bindings ~f)

let translate_constant_set_of_closures
    (inconstants : Inconstant_idents.result)
    (aliases : Alias_analysis.allocation_point Variable.Map.t)
    (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl:
      Alias_analysis.constant_defining_value Variable.Tbl.t)
    (constant_defining_values :
      CDV.t Symbol.Map.t) =
  Symbol.Map.map (fun const ->
      CDV.Mappers.map_set_of_closures const
        ~f:(fun set_of_closures ->
          translate_set_of_closures
            (inconstants : Inconstant_idents.result)
            (aliases : Alias_analysis.allocation_point Variable.Map.t)
            (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
            (var_to_definition_tbl:
              Alias_analysis.constant_defining_value Variable.Tbl.t)
            (set_of_closures : Flambda.Set_of_closures.t)))
    constant_defining_values

let find_original_set_of_closure
    (aliases : Alias_analysis.allocation_point Variable.Map.t)
    (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl:
      Alias_analysis.constant_defining_value Variable.Tbl.t)
    project_closure_map
    var =
  let rec loop var =
    match Variable.Map.find var aliases with
    | Variable var ->
      begin match Variable.Tbl.find var_to_definition_tbl var with
        | Project_closure { set_of_closures = var }
        | Move_within_set_of_closures { closure = var } ->
          loop var
        | Set_of_closures _ -> begin
            match Variable.Tbl.find var_to_symbol_tbl var with
            | s ->
              s
            | exception Not_found ->
              Format.eprintf "var: %a@." Variable.print var;
              assert false
          end
        | _ -> assert false
      end
    | Symbol s ->
      match Symbol.Map.find s project_closure_map with
      | exception Not_found ->
        Misc.fatal_errorf "find_original_set_of_closure: cannot find \
            symbol %a in the project-closure map"
          Symbol.print s
      | s -> s
  in
  loop var

let translate_definition_and_resolve_alias inconstants
    (aliases : Alias_analysis.allocation_point Variable.Map.t)
    (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl :
      Alias_analysis.constant_defining_value Variable.Tbl.t)
    (symbol_definition_map : CDV.t Symbol.Map.t)
    (project_closure_map : Symbol.t Symbol.Map.t)
    (definition : Alias_analysis.constant_defining_value)
    ~(backend : (module Backend_intf.S))
    : CDV.t option =
  ignore backend;  (* XXX *)
  let resolve_float_array_involving_variables
        ~(mutability : Flambda.mutable_or_immutable) ~vars
        : CDV.t option =
    (* Resolve an [Allocated_const] of the form:
        [Array (Pfloatarray, _, _)]
       (which references its contents via variables; it does not contain
        manifest floats). *)
    let find_float_var_definition var =
      match Variable.Tbl.find var_to_definition_tbl var with
      | Allocated_const (Normal (Float f)) -> f
      | const_defining_value ->
          Misc.fatal_errorf "Bad definition for float array member %a: %a"
            Variable.print var
            Alias_analysis.print_constant_defining_value
            const_defining_value
    in
    let find_float_symbol_definition sym =
      match Symbol.Map.find sym symbol_definition_map with
      | Allocated_const (Float f) -> f
      | const_defining_value ->
          Misc.fatal_errorf "Bad definition for float array member %a: %a"
            Symbol.print sym
            CDV.print
            const_defining_value
    in
    let floats =
      List.map (fun var ->
          match Variable.Map.find var aliases with
          | exception Not_found -> find_float_var_definition var
          | Variable var -> find_float_var_definition var
          | Symbol sym -> find_float_symbol_definition sym)
        vars
    in
    let const : Allocated_const.t =
      match mutability with
      | Immutable -> Immutable_float_array floats
      | Mutable -> Float_array floats
    in
    Some (CDV.create_allocated_const const)
  in
  match definition with
  | Block (tag, fields) ->
    let fields =
      List.map (resolve_variable aliases var_to_symbol_tbl
          var_to_definition_tbl)
        fields
    in
    Some (CDV.create_block tag fields)
  | Allocated_const (Normal const) ->
    Some (CDV.create_allocated_const const)
  | Allocated_const (Duplicate_array (Pfloatarray, mutability, var)) ->
    (* CR-someday mshinwell: This next section could do with cleanup.
       What happens is:
        - Duplicate contains a variable, which is resolved to
        a float array thing full of variables;
        - We send that value back through this function again so the
        individual members of that array are resolved from variables to
        floats.
        - Then we can build the Flambda.name term containing the
        Allocated_const (full of floats).
       We should maybe factor out the code from the
       Allocated_const (Array (...)) case below so this function doesn't have
       to be recursive. *)
    let (constant_defining_value : Alias_analysis.constant_defining_value) =
      match Variable.Map.find var aliases with
      | exception Not_found ->
        Variable.Tbl.find var_to_definition_tbl var
      | Variable var ->
        Variable.Tbl.find var_to_definition_tbl var
      | Symbol sym ->
        match Symbol.Map.find sym symbol_definition_map with
        | Allocated_const ((Immutable_float_array _) as const) ->
          Alias_analysis.Allocated_const (Normal const)
        | (Allocated_const _ | Block _ | Set_of_closures _
            | Project_closure _) as wrong ->
          Misc.fatal_errorf
            "Lift_constants.translate_definition_and_resolve_alias: \
              Duplicate Pfloatarray %a with symbol %a mapping to \
              wrong constant defining value %a"
            Variable.print var
            Alias_analysis.print_constant_defining_value definition
            CDV.print wrong
        | exception Not_found ->
          assert false
(* XXX this needs to use the new proving functions in Flambda_type
          let module Backend = (val backend) in
          match (Backend.import_symbol sym).descr with
          | Unresolved _ ->
            Misc.fatal_errorf
              "Lift_constants.translate_definition_and_resolve_alias: \
               Duplicate Pfloatarray %a with unknown symbol: %a"
              Variable.print var
              Alias_analysis.print_constant_defining_value definition
          | Float_array float_array ->
            let contents =
              Flambda_type.reify_as_constant_float_array float_array
            in
            begin match contents with
            | None ->
              Misc.fatal_errorf
                "Lift_constants.translate_definition_and_resolve_alias: \
                 Duplicate Pfloatarray %a with not completely known float \
                 array from symbol: %a"
                Variable.print var
                Alias_analysis.print_constant_defining_value definition
            | Some l ->
              Alias_analysis.Allocated_const (Normal (Immutable_float_array l))
            end
          | wrong ->
            (* CR-someday mshinwell: we might hit this if we ever duplicate
               a mutable array across compilation units (e.g. "snapshotting"
               an array).  We do not currently generate such code. *)
            Misc.fatal_errorf
              "Lift_constants.translate_definition_and_resolve_alias: \
               Duplicate Pfloatarray %a with symbol %a that does not \
               have an export description of an immutable array"
              Variable.print var
              Alias_analysis.print_constant_defining_value definition
              Flambda_type.print_descr wrong
*)
    in
    begin match constant_defining_value with
    | Allocated_const (Normal (Float_array _)) ->
      (* This example from pchambart illustrates why we do not allow
         the duplication of mutable arrays:

         {|
         let_symbol a = Allocated_const (Immutable_float_array [|0.|])
         initialize_symbol b = Duparray(Mutable, a)
         effect b.(0) <- 1.
         initialize_symbol c = Duparray(Mutable, b)
         |}

         This will be converted to:
         {|
         let_symbol a = Allocated_const (Immutable_float_array [|0.|])
         let_symbol b = Allocated_const (Float_array [|0.|])
         effect b.(0) <- 1.
         let_symbol c = Allocated_const (Float_array [|0.|])
         |}

         We can't encounter that currently, but it's scary.
      *)
      Misc.fatal_error "Pduparray is not allowed on mutable arrays"
    | Allocated_const (Normal (Immutable_float_array floats)) ->
      let const : Allocated_const.t =
        match mutability with
        | Immutable -> Immutable_float_array floats
        | Mutable -> Float_array floats
      in
      Some (CDV.create_allocated_const const)
    | Allocated_const (Array (Pfloatarray, _, vars)) ->
      (* Important: [mutability] is from the [Duplicate_array]
         construction above. *)
      resolve_float_array_involving_variables ~mutability ~vars
    | const ->
      Misc.fatal_errorf
        "Lift_constants.translate_definition_and_resolve_alias: \
          Duplicate Pfloatarray %a with wrong argument: %a"
        Variable.print var
        Alias_analysis.print_constant_defining_value const
    end
  | Allocated_const (Duplicate_array (_, _, _)) ->
    Misc.fatal_errorf "Lift_constants.translate_definition_and_resolve_alias: \
        Duplicate_array with non-Pfloatarray kind: %a"
      Alias_analysis.print_constant_defining_value definition
  | Allocated_const (Array (Pfloatarray, mutability, vars)) ->
    resolve_float_array_involving_variables ~mutability ~vars
  | Allocated_const (Array (_, _, _)) ->
    Misc.fatal_errorf "Lift_constants.translate_definition_and_resolve_alias: \
        Array with non-Pfloatarray kind: %a"
      Alias_analysis.print_constant_defining_value definition
  | Project_closure { set_of_closures; closure_id } ->
    let closure_id = closure_id_set_singleton closure_id in
    begin match Variable.Map.find set_of_closures aliases with
    | Symbol s ->
      Some (CDV.create_project_closure s closure_id)
    (* If a closure projection is a constant, the set of closures must
       be assigned to a symbol. *)
    | exception Not_found ->
      assert false
    | Variable v ->
      match Variable.Tbl.find var_to_symbol_tbl v with
      | s ->
        Some (CDV.create_project_closure s closure_id)
      | exception Not_found ->
        Format.eprintf "var: %a@." Variable.print v;
        assert false
    end
  | Move_within_set_of_closures { closure; move } ->
    let (_, move_to) = closure_id_map_singleton Closure_id.print move in
    let set_of_closure_symbol =
      find_original_set_of_closure
        aliases
        var_to_symbol_tbl
        var_to_definition_tbl
        project_closure_map
        closure
    in
    Some (CDV.create_project_closure set_of_closure_symbol move_to)
  | Set_of_closures set_of_closures ->
    let set_of_closures =
      translate_set_of_closures
        inconstants
        aliases
        var_to_symbol_tbl
        var_to_definition_tbl
        set_of_closures
    in
    Some (CDV.create_set_of_closures set_of_closures)
  | Project_var _ -> None
  | Field (_,_) | Symbol_field _ -> None
  | Tagged_immediate _ -> None
  | Symbol _ -> None
  | Variable _ -> None

let translate_definitions_and_resolve_alias
    inconstants
    (aliases : Alias_analysis.allocation_point Variable.Map.t)
    (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl:
      Alias_analysis.constant_defining_value Variable.Tbl.t)
    symbol_definition_map
    project_closure_map
    ~backend =
  Variable.Tbl.fold (fun var def map ->
      match
        translate_definition_and_resolve_alias inconstants aliases ~backend
          var_to_symbol_tbl var_to_definition_tbl symbol_definition_map
          project_closure_map def
      with
      | None -> map
      | Some def ->
        let symbol = Variable.Tbl.find var_to_symbol_tbl var in
        Symbol.Map.add symbol def map)
    var_to_definition_tbl Symbol.Map.empty

(* Resorting of graph including Initialize_symbol *)
let constant_dependencies ~backend:_
        (const : CDV.t) =
  match const with
  | Allocated_const _ -> Symbol.Set.empty
  | Block (_, fields) ->
    let symbol_fields =
      Misc.Stdlib.List.filter_map
        (fun (field : Flambda_static.Constant_defining_value_block_field.t) ->
          match field with
          | Symbol s -> Some s
          | Tagged_immediate _ -> None)
        fields
    in
    Symbol.Set.of_list symbol_fields
  | Set_of_closures set_of_closures ->
    Flambda.Named.free_symbols (Set_of_closures set_of_closures)
  | Project_closure (s, _) ->
    Symbol.Set.singleton s

module Symbol_SCC = Strongly_connected_components.Make (Symbol)

let program_graph ~backend imported_symbols symbol_to_constant
    (initialize_symbol_tbl :
      (Tag.Scannable.t
          * (Flambda.Expr.t * Flambda_kind.scanning * Continuation.t) list
          * Symbol.t option)
        Symbol.Tbl.t)
    (effect_tbl : (Flambda.Expr.t * Continuation.t * Symbol.t option)
      Symbol.Tbl.t) =
  let expression_symbol_dependencies expr = Flambda.Expr.free_symbols expr in
  let graph_with_only_constant_parts =
    Symbol.Map.map (fun const ->
        Symbol.Set.diff (constant_dependencies ~backend const)
          imported_symbols)
      symbol_to_constant
  in
  let graph_with_initialisation =
    Symbol.Tbl.fold (fun sym (_tag, fields, previous) ->
        let order_dep =
          match previous with
          | None -> Symbol.Set.empty
          | Some previous -> Symbol.Set.singleton previous
        in
        let deps = List.fold_left (fun set (field, _scanning, _cont) ->
            Symbol.Set.union (expression_symbol_dependencies field) set)
            order_dep fields
        in
        let deps = Symbol.Set.diff deps imported_symbols in
        Symbol.Map.add sym deps)
      initialize_symbol_tbl graph_with_only_constant_parts
  in
  let graph =
    Symbol.Tbl.fold (fun sym (expr, _cont, previous) ->
        let order_dep =
          match previous with
          | None -> Symbol.Set.empty
          | Some previous -> Symbol.Set.singleton previous
        in
        let deps =
          Symbol.Set.union (expression_symbol_dependencies expr) order_dep
        in
        let deps = Symbol.Set.diff deps imported_symbols in
        Symbol.Map.add sym deps
      )
      effect_tbl graph_with_initialisation
  in
  let components =
    Symbol_SCC.connected_components_sorted_from_roots_to_leaf
      graph
  in
  components

(* rebuilding the program *)
let add_definition_of_symbol constant_definitions
    (initialize_symbol_tbl :
      (Tag.Scannable.t
          * (Flambda.Expr.t * Flambda_kind.scanning * Continuation.t) list
          * Symbol.t option)
        Symbol.Tbl.t)
    (effect_tbl :
      (Flambda.Expr.t * Continuation.t * Symbol.t option) Symbol.Tbl.t)
    (program : Flambda_static.Program_body.t)
    component : Flambda_static.Program_body.t =
  let symbol_declaration sym =
    (* A symbol declared through an Initialize_symbol construct
       cannot be recursive, this is not allowed in the construction.
       This also couldn't have been introduced by this pass, so we can
       safely assert that this is not possible here *)
    assert (not (Symbol.Tbl.mem initialize_symbol_tbl sym));
    sym, Symbol.Map.find sym constant_definitions
  in
  match component with
  | Symbol_SCC.Has_loop l ->
    let l = List.map symbol_declaration l in
    Let_rec_symbol (l, program)
  | Symbol_SCC.No_loop sym ->
    match Symbol.Tbl.find initialize_symbol_tbl sym with
    | (tag, fields, _previous) ->
      let descr : Flambda_static.Program_body.Initialize_symbol.t =
        Values {
          tag;
          fields;
        }
      in
      Initialize_symbol (sym, descr, program)
    | exception Not_found ->
      match Symbol.Tbl.find effect_tbl sym with
      | (expr, cont, _previous) ->
        Effect (expr, cont, program)
      | exception Not_found ->
        let decl = Symbol.Map.find sym constant_definitions in
        Let_symbol (sym, decl, program)

let add_definitions_of_symbols constant_definitions initialize_symbol_tbl
    effect_tbl program components =
  Array.fold_left
    (add_definition_of_symbol constant_definitions initialize_symbol_tbl
      effect_tbl)
    program components

let introduce_free_variables_in_set_of_closures ~importer
    (var_to_block_field_tbl :
      Flambda_static.Constant_defining_value_block_field.t Variable.Tbl.t)
    (({ function_decls; free_vars; direct_call_surrogates; _ }
       : Flambda.Set_of_closures.t)
      as set_of_closures) =
  let add_definition_and_make_substitution var (expr, subst) =
    let searched_var = var
(*
      match Variable.Map.find var specialised_args with
      | exception Not_found -> var
      | external_var ->
        (* specialised arguments bound to constant can be rewritten *)
        match external_var.var with
        | Some var -> var
        | None ->
          Misc.fatal_errorf "No equality to variable for specialised arg %a"
            Variable.print var
*)
    in
    (* XXX [var_to_block_field_tbl] should keep track of [scanning] *)
    match Variable.Tbl.find var_to_block_field_tbl searched_var with
    | def ->
      let fresh = Variable.rename var in
      let named : Flambda.Named.t =
        match def with
        | Symbol sym -> Symbol sym
        | Tagged_immediate c -> Const (Tagged_immediate c)
      in
      let kind = Flambda_kind.value Must_scan in
      (Flambda.Expr.create_let fresh kind named expr),
        Variable.Map.add var fresh subst
    | exception Not_found ->
      (* The variable is bound by the closure or the arguments or not
         constant. In either case it does not need to be bound *)
      expr, subst
  in
  let done_something = ref false in
  let function_decls : Flambda.Function_declarations.t =
    Flambda.Function_declarations.update function_decls
      ~funs:(Variable.Map.map
          (fun (func_decl : Flambda.Function_declaration.t) ->
             let variables_to_bind =
               (* Closures from the same set must not be bound. *)
               Variable.Set.diff func_decl.free_variables
                 (Variable.Map.keys function_decls.funs)
             in
             let body, subst =
               Variable.Set.fold add_definition_and_make_substitution
                 variables_to_bind
                 (func_decl.body, Variable.Map.empty)
             in
             if Variable.Map.is_empty subst then begin
               func_decl
             end else begin
               done_something := true;
               let body =
                 Flambda.Expr.toplevel_substitution ~importer subst body
               in
               Flambda.Function_declaration.update_body func_decl ~body
             end)
          function_decls.funs)
  in
  let free_vars =
    (* Keep only those that are not rewritten to constants. *)
    Variable.Map.filter (fun v _ ->
        let keep = not (Variable.Tbl.mem var_to_block_field_tbl v) in
        if not keep then done_something := true;
        keep)
      free_vars
  in
  let free_vars = Flambda.Free_vars.clean_projections free_vars in
(* XXX presumably we need to do something with argument types here *)
(*
  let specialised_args =
    (* Keep only those that are not rewritten to constants. *)
    Variable.Map.filter (fun param (spec_to : Flambda.specialised_to) ->
        let keep =
          match spec_to.var with
          | Some var -> not (Variable.Tbl.mem var_to_block_field_tbl var)
          | None ->
            Misc.fatal_errorf "No equality to variable for specialised arg %a"
              Variable.print param
        in
        if not keep then begin
          done_something := true
        end;
        keep)
      specialised_args
  in
  let specialised_args =
    Flambda_utils.clean_specialised_args_projections specialised_args
  in
*)
  if not !done_something then
    set_of_closures
  else
    Flambda.Set_of_closures.create ~function_decls ~free_vars
      ~direct_call_surrogates

let rewrite_project_var
      (var_to_block_field_tbl :
        Flambda_static.Constant_defining_value_block_field.t Variable.Tbl.t)
      (project_var : Projection.Project_var.t) ~original : Flambda.Named.t =
  match Closure_id.Map.get_singleton project_var.var with
  | None -> (* it cannot be a constant, no rewriting *)
    original
  | Some (_closure_id, var) ->
    let var = Var_within_closure.unwrap var in
    match Variable.Tbl.find var_to_block_field_tbl var with
    | exception Not_found -> original
    | Symbol sym -> Symbol sym
    | Tagged_immediate imm -> Const (Tagged_immediate imm)

let introduce_free_variables_in_sets_of_closures ~importer
    (var_to_block_field_tbl:
      Flambda_static.Constant_defining_value_block_field.t Variable.Tbl.t)
    (translate_definition : CDV.t Symbol.Map.t) =
  Symbol.Map.map (fun (def : CDV.t) ->
      CDV.Mappers.map_set_of_closures def ~f:(fun set_of_closures ->
        introduce_free_variables_in_set_of_closures ~importer
          var_to_block_field_tbl
          set_of_closures))
    translate_definition

let var_to_block_field
    (aliases : Alias_analysis.allocation_point Variable.Map.t)
    (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl :
      Alias_analysis.constant_defining_value Variable.Tbl.t) =
  let var_to_block_field_tbl = Variable.Tbl.create 42 in
  Variable.Tbl.iter (fun var _ ->
      let def =
        resolve_variable aliases var_to_symbol_tbl var_to_definition_tbl var
      in
      Variable.Tbl.add var_to_block_field_tbl var def)
    var_to_definition_tbl;
  var_to_block_field_tbl

let program_symbols ~backend (program : Flambda_static.Program.t) =
  let new_fake_symbol =
    let r = ref 0 in
    fun () ->
      incr r;
      Symbol.create (Compilation_unit.get_current_exn ())
        (Linkage_name.create ("fake_effect_symbol_" ^ string_of_int !r))
  in
  let initialize_symbol_tbl = Symbol.Tbl.create 42 in
  let effect_tbl = Symbol.Tbl.create 42 in
  let symbol_definition_tbl = Symbol.Tbl.create 42 in
  let add_project_closure_definitions def_symbol
        (const : CDV.t) =
    match const with
    | Set_of_closures { function_decls = { funs } } ->
        Variable.Map.iter (fun fun_var _ ->
            let closure_id = Closure_id.wrap fun_var in
            let closure_symbol = closure_symbol ~backend closure_id in
            let project_closure =
              CDV.create_project_closure def_symbol closure_id
            in
            Symbol.Tbl.add symbol_definition_tbl closure_symbol
              project_closure)
          funs
    | Project_closure _
    | Allocated_const _
    | Block _ -> ()
  in
  let rec loop (program : Flambda_static.Program_body.t) previous_effect =
    match program with
    | Let_symbol (symbol, def, program) ->
      add_project_closure_definitions symbol def;
      Symbol.Tbl.add symbol_definition_tbl symbol def;
      loop program previous_effect
    | Let_rec_symbol (defs, program) ->
      List.iter (fun (symbol, def) ->
          add_project_closure_definitions symbol def;
          Symbol.Tbl.add symbol_definition_tbl symbol def)
        defs;
      loop program previous_effect
    | Initialize_symbol (symbol, descr, program) ->
      (* previous_effect is used to keep the order of initialize and effect
         values. Their effects order must be kept ordered.
         it is used as an extra dependency when sorting the symbols. *)
      (* CR-someday pchambart: if the fields expressions are pure, we could
         drop this dependency
         mshinwell: deferred CR *)
      begin match descr with
      | Values { tag; fields; } ->
        Symbol.Tbl.add initialize_symbol_tbl symbol
          (tag, fields, previous_effect)
      | Float _ | Int32 _ | Int64 _ | Nativeint _ ->
        (* XXX *)
        ()
      end;
      loop program (Some symbol)
    | Effect (expr, cont, program) ->
      (* Used to ensure that effects are correctly ordered *)
      let fake_effect_symbol = new_fake_symbol () in
      Symbol.Tbl.add effect_tbl fake_effect_symbol
        (expr, cont, previous_effect);
      loop program (Some fake_effect_symbol)
    | End _ -> ()
  in
  loop program.program_body None;
  initialize_symbol_tbl, symbol_definition_tbl, effect_tbl

let replace_definitions_in_initialize_symbol_and_effects
    (inconstants : Inconstant_idents.result)
    (aliases : Alias_analysis.allocation_point Variable.Map.t)
    (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl :
      Alias_analysis.constant_defining_value Variable.Tbl.t)
    (initialize_symbol_tbl :
      (Tag.Scannable.t
          * (Flambda.Expr.t * Flambda_kind.scanning * Continuation.t) list
          * Symbol.t option)
        Symbol.Tbl.t)
    (effect_tbl :
      (Flambda.Expr.t * Continuation.t * Symbol.t option) Symbol.Tbl.t) =
  let rewrite_expr expr =
    Flambda.Expr.Mappers.map_all_immutable_let_and_let_rec_bindings expr
      ~f:(fun var (named : Flambda.Named.t) : Flambda.Named.t ->
        if Inconstant_idents.variable var inconstants then
          named
        else
          let resolved =
            resolve_variable
              aliases
              var_to_symbol_tbl
              var_to_definition_tbl
              var
          in
          match named, resolved with
          | Symbol s1, Symbol s2 ->
            assert (s1 == s2);  (* physical equality for speed *)
            named;
          | Const (Tagged_immediate c1), Tagged_immediate c2 ->
            assert (c1 == c2);
            named
          | _, Symbol s -> Symbol s
          | _, Tagged_immediate c -> Const (Tagged_immediate c))
  in
  (* This is safe because we only [replace] the current key during
     iteration (cf. https://github.com/ocaml/ocaml/pull/337) *)
  Symbol.Tbl.iter
    (fun symbol (tag, fields, previous) ->
      let fields =
        List.map (fun (field, scanning, cont) ->
            rewrite_expr field, scanning, cont)
          fields
      in
      Symbol.Tbl.replace initialize_symbol_tbl symbol (tag, fields, previous))
    initialize_symbol_tbl;
  Symbol.Tbl.iter
    (fun symbol (expr, cont, previous) ->
      Symbol.Tbl.replace effect_tbl symbol
        (rewrite_expr expr, cont, previous))
    effect_tbl

(* CR-soon mshinwell: Update the name of [project_closure_map]. *)
let project_closure_map symbol_definition_map =
  Symbol.Map.fold (fun sym (const : CDV.t) acc ->
      match const with
      | Project_closure (set_of_closures, _) ->
        Symbol.Map.add sym set_of_closures acc
      | Set_of_closures _ ->
        Symbol.Map.add sym sym acc
      | Allocated_const _
      | Block _ -> acc)
    symbol_definition_map
    Symbol.Map.empty

let the_dead_constant_index = ref 0

let lift_constants ~backend (program : Flambda_static.Program.t) =
  let module B = (val backend : Backend_intf.S) in
  let importer = (module B : Flambda_type.Importer) in
  let the_dead_constant =
    let index = !the_dead_constant_index in
    incr the_dead_constant_index;
    let name = Printf.sprintf "the_dead_constant_%d" index in
    Symbol.create (Compilation_unit.get_current_exn ())
      (Linkage_name.create name)
  in
  let program_body : Flambda_static.Program_body.t =
    Let_symbol (the_dead_constant,
      CDV.create_allocated_const (Nativeint Targetint.zero),
      program.program_body)
  in
  let program : Flambda_static.Program.t =
    { program with program_body; }
  in
  let inconstants =
    Inconstant_idents.inconstants_on_program program ~importer
      ~compilation_unit:(Compilation_unit.get_current_exn ())
  in
  let initialize_symbol_tbl, symbol_definition_tbl, effect_tbl =
    program_symbols ~backend program
  in
  let var_to_symbol_tbl, var_to_definition_tbl, let_symbol_to_definition_tbl,
      initialize_symbol_to_definition_tbl =
    assign_symbols_and_collect_constant_definitions ~backend ~program
      ~inconstants
  in
  let aliases =
    Alias_analysis.run var_to_definition_tbl
      initialize_symbol_to_definition_tbl
      let_symbol_to_definition_tbl
      ~the_dead_constant
  in
  replace_definitions_in_initialize_symbol_and_effects
      (inconstants : Inconstant_idents.result)
      (aliases : Alias_analysis.allocation_point Variable.Map.t)
      (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
      (var_to_definition_tbl
        : Alias_analysis.constant_defining_value Variable.Tbl.t)
      initialize_symbol_tbl
      effect_tbl;
  let symbol_definition_map =
    translate_constant_set_of_closures
      (inconstants : Inconstant_idents.result)
      (aliases : Alias_analysis.allocation_point Variable.Map.t)
      (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
      (var_to_definition_tbl
        : Alias_analysis.constant_defining_value Variable.Tbl.t)
      (Symbol.Tbl.to_map symbol_definition_tbl)
  in
  let project_closure_map = project_closure_map symbol_definition_map in
  let translated_definitions =
    translate_definitions_and_resolve_alias
      inconstants
      (aliases : Alias_analysis.allocation_point Variable.Map.t)
      (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
      (var_to_definition_tbl
        : Alias_analysis.constant_defining_value Variable.Tbl.t)
      symbol_definition_map
      project_closure_map
      ~backend
  in
  let var_to_block_field_tbl =
    var_to_block_field
      (aliases : Alias_analysis.allocation_point Variable.Map.t)
      (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
      (var_to_definition_tbl
        : Alias_analysis.constant_defining_value Variable.Tbl.t)
  in
  let translated_definitions =
    introduce_free_variables_in_sets_of_closures ~importer
      var_to_block_field_tbl
      translated_definitions
  in
  let constant_definitions =
    (* Add previous Let_symbol to the newly discovered ones *)
    Symbol.Map.union
      (fun _sym
        (c1:CDV.t)
        (c2:CDV.t) ->
        match c1, c2 with
        | Project_closure (s1, closure_id1),
          Project_closure (s2, closure_id2) when
            Symbol.equal s1 s2 &&
            Closure_id.equal closure_id1 closure_id2 ->
          Some c1
        | Project_closure (s1, closure_id1),
          Project_closure (s2, closure_id2) ->
          Format.eprintf "not equal project closure@. s %a %a@. cid %a %a@."
            Symbol.print s1 Symbol.print s2
            Closure_id.print closure_id1 Closure_id.print closure_id2;
          assert false
        | _ ->
          assert false
      )
      symbol_definition_map
      translated_definitions
  in
  (* Upon the [Initialize_symbol]s, the [Effect]s and the constant definitions,
     do the following:
     1. Introduce [Let]s to bind variables that are going to be replaced
     by constants.
     2. If a variable bound by a closure gets replaced by a symbol and
     thus eliminated from the [free_vars] set of the closure, we need to
     rewrite any subsequent [Project_var] expressions that project that
     variable. *)
  let rewrite_expr expr =
    Flambda.Expr.Mappers.map_named (function
        | (Set_of_closures set_of_closures) as named ->
          let new_set_of_closures =
            introduce_free_variables_in_set_of_closures ~importer
              var_to_block_field_tbl set_of_closures
          in
          if new_set_of_closures == set_of_closures then
            named
          else
            Set_of_closures new_set_of_closures
        | (Project_var project_var) as original ->
          rewrite_project_var var_to_block_field_tbl project_var ~original
        | (Var _ | Symbol _ | Const _ | Allocated_const _ | Project_closure _
        | Move_within_set_of_closures _ | Prim _ | Assign _
        | Read_mutable _ | Read_symbol_field _) as named -> named)
      expr
  in
  let constant_definitions =
    Symbol.Map.map (fun (const : CDV.t) ->
        match const with
        | Allocated_const _ | Block _ | Project_closure _ -> const
        | Set_of_closures set_of_closures ->
          let set_of_closures =
            Flambda.Set_of_closures.Mappers.map_function_bodies set_of_closures
              ~f:rewrite_expr
          in
          CDV.create_set_of_closures
            (introduce_free_variables_in_set_of_closures ~importer
              var_to_block_field_tbl set_of_closures))
    constant_definitions
  in
  let effect_tbl =
    Symbol.Tbl.map effect_tbl (fun (effect, cont, dep) ->
      rewrite_expr effect, cont, dep)
  in
  let initialize_symbol_tbl =
    Symbol.Tbl.map initialize_symbol_tbl (fun (tag, fields, dep) ->
      let fields =
        List.map (fun (field, scanning, cont) ->
            rewrite_expr field, scanning, cont)
          fields
      in
      tag, fields, dep)
  in
  let imported_symbols = Flambda_static.Program.imported_symbols program in
  let components =
    program_graph ~backend imported_symbols constant_definitions
      initialize_symbol_tbl effect_tbl
  in
  let program_body =
    add_definitions_of_symbols constant_definitions
      initialize_symbol_tbl
      effect_tbl
      (End (Flambda_static.Program.root_symbol program))
      components
  in
  Flambda_static.Program.introduce_needed_import_symbols
    { program with program_body; }

*)

let lift_constants ~backend:_ (program : Flambda_static.Program.t) =
  program
