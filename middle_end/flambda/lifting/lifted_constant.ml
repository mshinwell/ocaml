(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Flambda.Import

module Definition = struct
  type descr =
    | Code of Code_id.t
    | Set_of_closures of {
        denv : Downwards_env.t;
        closure_symbols_with_types
          : (Symbol.t * Flambda_type.t) Closure_id.Lmap.t;
        symbol_projections : Symbol_projection.t Variable.Map.t;
      }
    | Block_like of {
        symbol : Symbol.t;
        denv : Downwards_env.t;
        ty : Flambda_type.t;
        symbol_projections : Symbol_projection.t Variable.Map.t;
      }

  type t = {
    descr : descr;
    defining_expr : Static_const_with_free_names.t;
  }

  let binds_symbol t sym =
    match t.descr with
    | Code _ -> false
    | Set_of_closures { closure_symbols_with_types; _ } ->
      Closure_id.Lmap.exists (fun _ (sym', _) -> Symbol.equal sym sym')
        closure_symbols_with_types
    | Block_like { symbol; _ } -> Symbol.equal sym symbol

  let free_names t =
    match t.descr with
    | Code _ -> Static_const_with_free_names.free_names t.defining_expr
    | Set_of_closures { symbol_projections; _ }
    | Block_like { symbol_projections; _ } ->
      (* The symbols mentioned in any symbol projections must be counted
         as free names, so that the definition doesn't get placed too high
         in the code. *)
      Variable.Map.fold (fun _var proj free_names ->
          Name_occurrences.add_symbol free_names
            (Symbol_projection.symbol proj) Name_mode.normal)
        symbol_projections
        (Static_const_with_free_names.free_names t.defining_expr)

  let print_descr ppf descr =
    match descr with
    | Code code_id -> Code_id.print ppf code_id
    | Set_of_closures { closure_symbols_with_types; _ } ->
      let symbols =
        Closure_id.Lmap.data closure_symbols_with_types
        |> List.map fst
      in
      Format.fprintf ppf "@[<hov 1>(%a)@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Symbol.print)
        symbols
    | Block_like { symbol; _ } -> Symbol.print ppf symbol

  let print ppf { descr; defining_expr; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(descr@ %a)@]@ \
        @[<hov 1>(defining_expr@ %a)@]\
        @]"
      print_descr descr
      Static_const_with_free_names.print defining_expr

  let descr t = t.descr
  let defining_expr t = t.defining_expr

  let symbol_projections t =
    match t.descr with
    | Code _ -> Variable.Map.empty
    | Set_of_closures { symbol_projections; _ }
    | Block_like { symbol_projections; _ } -> symbol_projections

  let code code_id defining_expr =
    match Static_const_with_free_names.const defining_expr with
    | Code code ->
      if Code_id.equal code_id (Code.code_id code) then
        { descr = Code code_id;
          defining_expr;
        }
      else
        Misc.fatal_errorf "Mismatched code ids: %a vs.@ %a"
          Code_id.print code_id
          Code_id.print (Code.code_id code)
    | _ ->
      Misc.fatal_errorf "Not a code definition: %a"
        Static_const_with_free_names.print defining_expr

  let set_of_closures denv ~closure_symbols_with_types
        ~symbol_projections defining_expr =
    { descr = Set_of_closures {
        denv;
        closure_symbols_with_types;
        symbol_projections;
      };
      defining_expr;
    }

  let block_like denv symbol ty ~symbol_projections defining_expr =
    { descr = Block_like {
        symbol;
        denv;
        ty;
        symbol_projections;
      };
      defining_expr;
    }

  let denv t =
    match t.descr with
    | Code _ -> None
    | Set_of_closures { denv; _ } | Block_like { denv; _ } -> Some denv

  let bound_symbols_pattern t =
    let module P = Bound_symbols.Pattern in
    match t.descr with
    | Code code_id -> P.code code_id
    | Set_of_closures { closure_symbols_with_types; _; } ->
      P.set_of_closures (Closure_id.Lmap.map fst closure_symbols_with_types)
    | Block_like { symbol; _ } -> P.block_like symbol

  let bound_symbols t =
    Bound_symbols.create [bound_symbols_pattern t]

  let types_of_symbols t =
    match t.descr with
    | Code _ -> Symbol.Map.empty
    | Set_of_closures { denv; closure_symbols_with_types; _ } ->
      Closure_id.Lmap.fold (fun _closure_id (symbol, ty) types_of_symbols ->
          Symbol.Map.add symbol (denv, ty) types_of_symbols)
        closure_symbols_with_types
        Symbol.Map.empty
    | Block_like { symbol; denv; ty; _ } ->
      Symbol.Map.singleton symbol (denv, ty)
end

type t = {
  definitions : Definition.t list;
  bound_symbols : Bound_symbols.t;
  defining_exprs : Static_const_with_free_names.Group.t;
  symbol_projections : Symbol_projection.t Variable.Map.t;
  is_fully_static : bool;
}

let definitions t = t.definitions
let symbol_projections t = t.symbol_projections

let free_names_of_defining_exprs t =
  Static_const_with_free_names.Group.free_names t.defining_exprs

let is_fully_static t = t.is_fully_static

let print ppf
      { definitions; bound_symbols = _; defining_exprs = _;
        is_fully_static = _; symbol_projections = _; } =
  Format.fprintf ppf "@[<hov 1>(%a)@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space Definition.print)
    definitions

let compute_bound_symbols definitions =
  ListLabels.map definitions ~f:Definition.bound_symbols_pattern
  |> Bound_symbols.create

let compute_defining_exprs definitions =
  ListLabels.map definitions ~f:Definition.defining_expr
  |> Static_const_with_free_names.Group.create

let create_block_like symbol ~symbol_projections defining_expr denv ty =
  (* CR mshinwell: check that [defining_expr] is not a set of closures
     or code *)
  let definition =
    Definition.block_like denv symbol ty ~symbol_projections defining_expr
  in
  let definitions = [definition] in
  { definitions;
    bound_symbols = compute_bound_symbols definitions;
    defining_exprs = compute_defining_exprs definitions;
    is_fully_static =
      Static_const_with_free_names.is_fully_static defining_expr;
    symbol_projections = Definition.symbol_projections definition;
  }

let create_set_of_closures denv ~closure_symbols_with_types
      ~symbol_projections defining_expr =
  let definition =
    Definition.set_of_closures denv ~closure_symbols_with_types
      ~symbol_projections defining_expr
  in
  let definitions = [definition] in
  { definitions;
    bound_symbols = compute_bound_symbols definitions;
    defining_exprs = compute_defining_exprs definitions;
    is_fully_static =
      Static_const_with_free_names.is_fully_static defining_expr;
    symbol_projections = Definition.symbol_projections definition;
  }

let create_code code_id defining_expr =
  let definition = Definition.code code_id defining_expr in
  let definitions = [definition] in
  { definitions;
    bound_symbols = compute_bound_symbols definitions;
    defining_exprs = compute_defining_exprs definitions;
    is_fully_static =
      Static_const_with_free_names.is_fully_static defining_expr;
    symbol_projections = Definition.symbol_projections definition;
  }

let concat ts =
  let definitions =
    List.fold_left (fun definitions t ->
        t.definitions @ definitions)
      []
      ts
  in
  let bound_symbols =
    List.fold_left (fun bound_symbols t ->
        Bound_symbols.concat t.bound_symbols bound_symbols)
      Bound_symbols.empty
      ts
  in
  let defining_exprs =
    List.fold_left (fun defining_exprs t ->
        Static_const_with_free_names.Group.concat t.defining_exprs
          defining_exprs)
      Static_const_with_free_names.Group.empty
      ts
  in
  let is_fully_static =
    List.fold_left (fun is_fully_static t ->
        t.is_fully_static && is_fully_static)
      true
      ts
  in
  let symbol_projections =
    List.fold_left (fun symbol_projections t ->
        Variable.Map.disjoint_union ~eq:Symbol_projection.equal
          t.symbol_projections symbol_projections)
      Variable.Map.empty
      ts
  in
  { definitions;
    bound_symbols;
    defining_exprs;
    is_fully_static;
    symbol_projections;
  }

let defining_exprs t =
  Static_const_with_free_names.Group.create
    (List.map Definition.defining_expr t.definitions)

let bound_symbols t =
  Bound_symbols.create
    (List.map Definition.bound_symbols_pattern t.definitions)

let types_of_symbols t =
  ListLabels.fold_left t.definitions
    ~init:Symbol.Map.empty
    ~f:(fun types_of_symbols definition ->
      Symbol.Map.disjoint_union (Definition.types_of_symbols definition)
        types_of_symbols)

let all_defined_symbols t =
  Symbol.Map.keys (types_of_symbols t)

let apply_projection t proj =
  let symbol = Symbol_projection.symbol proj in
  let matching_defining_exprs =
    ListLabels.filter_map t.definitions ~f:(fun definition ->
      if Definition.binds_symbol definition symbol then
        Some (Definition.defining_expr definition)
      else
        None)
  in
  match matching_defining_exprs with
  | [defining_expr] ->
    let simple =
      match
        Symbol_projection.projection proj,
        Static_const_with_free_names.const defining_expr
      with
      | Block_load { index; }, Block (tag, mut, fields) ->
        if not (Tag.Scannable.equal tag Tag.Scannable.zero) then begin
          Misc.fatal_errorf "Symbol projection@ %a@ on block which doesn't \
              have tag zero:@ %a"
            Symbol_projection.print proj
            Static_const_with_free_names.print defining_expr
        end;
        if Mutability.is_mutable mut then begin
          Misc.fatal_errorf "Symbol projection@ %a@ on mutable block:@ %a"
            Symbol_projection.print proj
            Static_const_with_free_names.print defining_expr
        end;
        let index = Targetint.OCaml.to_int_exn index in
        begin match List.nth_opt fields index with
        | Some field ->
          begin match field with
          | Symbol symbol -> Simple.symbol symbol
          | Tagged_immediate imm ->
            Simple.const_int (Target_imm.to_targetint imm)
          | Dynamically_computed var -> Simple.var var
          end
        | None ->
          Misc.fatal_errorf "Symbol projection@ %a@ has out-of-range \
              index:@ %a"
            Symbol_projection.print proj
            Static_const_with_free_names.print defining_expr
        end;
      | Project_var { project_from; var; }, Set_of_closures set ->
        let decls = Set_of_closures.function_decls set in
        if not (Function_declarations.binds_closure_id decls project_from)
        then begin
          Misc.fatal_errorf "Symbol projection@ %a@ has closure ID not \
              bound by this set of closures:@ %a"
            Symbol_projection.print proj
            Static_const_with_free_names.print defining_expr
        end;
        let closure_env = Set_of_closures.closure_elements set in
        begin match Var_within_closure.Map.find var closure_env with
        | exception Not_found ->
          Misc.fatal_errorf "Symbol projection@ %a@ has closure var not \
              defined in the environment of this set of closures:@ %a"
            Symbol_projection.print proj
            Static_const_with_free_names.print defining_expr
        | closure_entry -> closure_entry
        end
      | Block_load _,
        (Code _ | Set_of_closures _ | Boxed_float _ | Boxed_int32 _
        | Boxed_int64 _ | Boxed_nativeint _ | Immutable_float_block _
        | Immutable_float_array _ | Mutable_string _ | Immutable_string _)
      | Project_var _,
        (Code _ | Block _ | Boxed_float _ | Boxed_int32 _
        | Boxed_int64 _ | Boxed_nativeint _ | Immutable_float_block _
        | Immutable_float_array _ | Mutable_string _ | Immutable_string _) ->
        Misc.fatal_errorf "Symbol projection@ %a@ cannot be applied to:@ %a"
          Symbol_projection.print proj
          Static_const_with_free_names.print defining_expr
    in
    Some simple
  | [] -> None
  | _::_::_ ->
    Misc.fatal_errorf "Symbol projection@ %a@ matches more than one \
        constant in:@ %a"
      Symbol_projection.print proj
      print t
