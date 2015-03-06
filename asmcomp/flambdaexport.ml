(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2013 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Ext_types
open Symbol
open Abstract_identifiers
open Flambda

module Innerid = Id (struct end)
module Export_id = UnitId (Innerid) (Compilation_unit)

type tag = int

type _ boxed_int =
  | Int32 : int32 boxed_int
  | Int64 : int64 boxed_int
  | Nativeint : nativeint boxed_int

type descr =
  | Value_block of tag * approx array
  | Value_int of int
  | Value_constptr of int
  | Value_float of float
  | Value_boxed_int : 'a boxed_int * 'a -> descr
  | Value_string
  | Value_closure of value_offset
  | Value_set_of_closures of value_closure

and value_offset = {
  fun_id : Closure_id.t;
  closure : value_closure;
}

and value_closure = {
  closure_id : Set_of_closures_id.t;
  bound_var : approx Var_within_closure.Map.t;
  results : approx Closure_id.Map.t;
}

and approx =
  | Value_unknown
  | Value_id of Export_id.t
  | Value_symbol of Symbol.t

module Exported = struct
  type t = {
    functions : unit function_declarations Set_of_closures_id.Map.t;
    functions_off : unit function_declarations Closure_id.Map.t;
    ex_values : descr Export_id.Map.t Compilation_unit.Map.t;
    globals : approx Ident.Map.t;
    id_symbol : Symbol.t Export_id.Map.t Compilation_unit.Map.t;
    symbol_id : Export_id.t Symbol.Map.t;
    offset_fun : int Closure_id.Map.t;
    offset_fv : int Var_within_closure.Map.t;
    constants : Symbol.Set.t;
    constant_closures : Set_of_closures_id.Set.t;
    kept_arguments : Variable.Set.t Set_of_closures_id.Map.t;
  }

  let find_declaration t closure_id =
    Closure_id.Map.find closure_id t.functions_off

  let get_fun_offset_exn t closure_id =
    Closure_id.Map.find closure_id t.fun_offset_table

  let get_fv_offset_exn t var =
    Var_within_closure.Map.find var t.fv_offset_table

  let closure_declaration_position_exn t closure_id =
    Closure_id.Map.find closure_id t.functions_off

  let set_of_closures_declaration_position_exn t set_of_closures_id =
    Set_of_closures_id.Map.find set_of_closures_id t.functions

  (* CR mshinwell: consider renaming to [is_set_of_closures_constant] *)
  let is_function_constant t set_of_closures_id =
    Set_of_closures_id.Set.mem set_of_closures_id t.constant_closures
end

type exported = Exported.t

let empty_export = {
  functions = Set_of_closures_id.Map.empty;
  functions_off = Closure_id.Map.empty;
  ex_values =  Compilation_unit.Map.empty;
  globals = Ident.Map.empty;
  id_symbol =  Compilation_unit.Map.empty;
  symbol_id = Symbol.Map.empty;
  offset_fun = Closure_id.Map.empty;
  offset_fv = Var_within_closure.Map.empty;
  constants = Symbol.Set.empty;
  constant_closures = Set_of_closures_id.Set.empty;
  kept_arguments = Set_of_closures_id.Map.empty;
}

let find_ex_value eid map =
  let unit = Export_id.unit eid in
  let unit_map = Compilation_unit.Map.find unit map in
  Export_id.Map.find eid unit_map

let find_description eid ex = find_ex_value eid ex.ex_values

let eidmap_disjoint_union m1 m2 =
  Compilation_unit.Map.merge
    (fun id x y -> match x, y with
       | None, None -> None
       | None, Some v
       | Some v, None -> Some v
       | Some v1, Some v2 ->
           Some (Export_id.Map.disjoint_union v1 v2))
    m1 m2

let nest_eid_map map =
  let add_map eid v map =
    let unit = Export_id.unit eid in
    let m = try Compilation_unit.Map.find unit map
      with Not_found -> Export_id.Map.empty in
    Compilation_unit.Map.add unit (Export_id.Map.add eid v m) map
  in
  Export_id.Map.fold add_map map Compilation_unit.Map.empty

let print_approx ppf export =
  let values = export.ex_values in
  let open Format in
  let printed = ref Export_id.Set.empty in
  let printed_closure = ref Set_of_closures_id.Set.empty in
  let rec print_approx ppf = function
    | Value_unknown -> fprintf ppf "?"
    | Value_id id ->
      if Export_id.Set.mem id !printed
      then fprintf ppf "(%a: _)" Export_id.print id
      else
        (try
           let descr = find_ex_value id values in
           printed := Export_id.Set.add id !printed;
           fprintf ppf "(%a: %a)"
             Export_id.print id
             print_descr descr
         with Not_found ->
           fprintf ppf "(%a: Not available)"
             Export_id.print id)
    | Value_symbol sym -> Symbol.print ppf sym
  and print_descr ppf = function
    | Value_int i -> pp_print_int ppf i
    | Value_constptr i -> fprintf ppf "%ip" i
    | Value_block (tag, fields) ->
      fprintf ppf "[%i:%a]" tag print_fields fields
    | Value_closure {fun_id; closure} ->
      fprintf ppf "(function %a, %a)" Closure_id.print fun_id print_closure closure
    | Value_set_of_closures closure ->
      fprintf ppf "(ufunction %a)" print_closure closure
    | Value_string -> Format.pp_print_string ppf "string"
    | Value_float f -> Format.pp_print_float ppf f
    | Value_boxed_int (t, i) ->
      match t with
      | Int32 -> Format.fprintf ppf "%li" i
      | Int64 -> Format.fprintf ppf "%Li" i
      | Nativeint -> Format.fprintf ppf "%ni" i
  and print_fields ppf fields =
    Array.iter (fun approx -> fprintf ppf "%a@ " print_approx approx) fields
  and print_closure ppf { closure_id; bound_var } =
    if Set_of_closures_id.Set.mem closure_id !printed_closure
    then fprintf ppf "%a" Set_of_closures_id.print closure_id
    else begin
      printed_closure :=
        Set_of_closures_id.Set.add closure_id !printed_closure;
      fprintf ppf "{%a: %a}"
        Set_of_closures_id.print closure_id
        print_binding bound_var
    end
  and print_binding ppf bound_var =
    Var_within_closure.Map.iter (fun clos_id approx ->
        fprintf ppf "%a -> %a,@ "
          Var_within_closure.print clos_id
          print_approx approx) bound_var
  in
  let print_approxs id approx =
    fprintf ppf "%a -> %a;@ " Ident.print id print_approx approx
  in
  Ident.Map.iter print_approxs export.globals

let print_symbols ppf export =
  let open Format in
  let print_symbol eid sym =
    fprintf ppf "%a -> %a@." Symbol.print sym Export_id.print eid
  in
    Compilation_unit.Map.iter (fun _ -> Export_id.Map.iter print_symbol)
      export.id_symbol

let print_all ppf export =
  let open Format in
  fprintf ppf "approxs@ %a@.@."
    print_approx export;
  fprintf ppf "id_symbol@ %a@.@."
    (Compilation_unit.Map.print (Export_id.Map.print Symbol.print))
      export.id_symbol;
  fprintf ppf "symbol_id@ %a@.@."
    (Symbol.Map.print Export_id.print) export.symbol_id;
  fprintf ppf "constants@ %a@.@."
    Symbol.Set.print export.constants;
  fprintf ppf "functions@ %a@.@."
    (Set_of_closures_id.Map.print Printflambda.function_declarations)
      export.functions

let merge e1 e2 =
  let int_eq (i:int) j = i = j in
  { ex_values = eidmap_disjoint_union e1.ex_values e2.ex_values;
    globals = Ident.Map.disjoint_union e1.globals e2.globals;
    functions = Set_of_closures_id.Map.disjoint_union e1.functions
      e2.functions;
    functions_off =
      Closure_id.Map.disjoint_union e1.functions_off e2.functions_off;
    id_symbol = eidmap_disjoint_union  e1.id_symbol e2.id_symbol;
    symbol_id = Symbol.Map.disjoint_union e1.symbol_id e2.symbol_id;
    offset_fun = Closure_id.Map.disjoint_union
        ~eq:int_eq e1.offset_fun e2.offset_fun;
    offset_fv = Var_within_closure.Map.disjoint_union
        ~eq:int_eq e1.offset_fv e2.offset_fv;
    constants = Symbol.Set.union e1.constants e2.constants;
    constant_closures =
      Set_of_closures_id.Set.union e1.constant_closures e2.constant_closures;
    kept_arguments =
      Set_of_closures_id.Map.disjoint_union e1.kept_arguments
        e2.kept_arguments;
  }

(* importing informations to build a pack: the global identifying the
   compilation unit of symbols is changed to be the pack one *)

let rename_id_state = Export_id.Tbl.create 100

let import_eid_for_pack units pack id =
  try Export_id.Tbl.find rename_id_state id
  with Not_found ->
    let unit_id = Export_id.unit id in
    let id' =
      if Compilation_unit.Set.mem unit_id units
      then
        Export_id.create ?name:(Export_id.name id) pack
      else id in
    Export_id.Tbl.add rename_id_state id id';
    id'

let import_symbol_for_pack units pack symbol =
  let unit = symbol.sym_unit in
  if Compilation_unit.Set.mem unit units
  then { symbol with sym_unit = pack }
  else symbol

let import_approx_for_pack units pack = function
  | Value_symbol sym -> Value_symbol (import_symbol_for_pack units pack sym)
  | Value_id eid -> Value_id (import_eid_for_pack units pack eid)
  | Value_unknown -> Value_unknown

let import_closure units pack closure =
  { closure_id = closure.closure_id;
    bound_var =
      Var_within_closure.Map.map (import_approx_for_pack units pack)
        closure.bound_var;
    results =
      Closure_id.Map.map (import_approx_for_pack units pack) closure.results;
  }

let import_descr_for_pack units pack = function
  | Value_int _
  | Value_constptr _
  | Value_string
  | Value_float _
  | Value_boxed_int _ as desc -> desc
  | Value_block (tag, fields) ->
    Value_block (tag, Array.map (import_approx_for_pack units pack) fields)
  | Value_closure { fun_id; closure } ->
    Value_closure { fun_id; closure = import_closure units pack closure }
  | Value_set_of_closures closure ->
    Value_set_of_closures (import_closure units pack closure)

let import_code_for_pack units pack expr =
  Flambdaiter.map (function
      | Fsymbol (sym, ()) ->
        Fsymbol (import_symbol_for_pack units pack sym, ())
      | e -> e)
    expr

let import_ffunctions_for_pack units pack ffuns =
  { ffuns with
    funs = Variable.Map.map (fun ffun ->
        { ffun with body = import_code_for_pack units pack ffun.body })
      ffuns.funs;
  }

let functions_off functions =
  let aux_fun ffunctions function_id _ map =
    Closure_id.Map.add
      (Closure_id.wrap function_id) ffunctions map
  in
  let aux _ f map = Variable.Map.fold (aux_fun f) f.funs map in
  Set_of_closures_id.Map.fold aux functions Closure_id.Map.empty

let import_eidmap_for_pack units pack f map =
  nest_eid_map
    (Compilation_unit.Map.fold (fun _ map acc ->
        Export_id.Map.disjoint_union map acc)
    (Compilation_unit.Map.map (fun map ->
        Export_id.Map.map_keys (import_eid_for_pack units pack)
          (Export_id.Map.map f map))
      map)
  Export_id.Map.empty)

let import_for_pack ~pack_units ~pack exp =
  let import_sym = import_symbol_for_pack pack_units pack in
  let import_desr = import_descr_for_pack pack_units pack in
  let import_approx = import_approx_for_pack pack_units pack in
  let import_eid = import_eid_for_pack pack_units pack in
  let import_eidmap f map = import_eidmap_for_pack pack_units pack f map in
  let functions =
    Set_of_closures_id.Map.map (import_ffunctions_for_pack pack_units pack)
      exp.functions
  in
  (* The only reachable global identifier of a pack is the pack itself *)
  let globals = Ident.Map.filter (fun unit _ ->
      Ident.same (Compilation_unit.get_persistent_ident pack) unit)
    exp.globals
  in
  let res =
    { functions;
      functions_off = functions_off functions;
      globals = Ident.Map.map import_approx globals;
      offset_fun = exp.offset_fun;
      offset_fv = exp.offset_fv;
      ex_values = import_eidmap import_desr exp.ex_values;
      id_symbol = import_eidmap import_sym exp.id_symbol;
      symbol_id = Symbol.Map.map_keys import_sym
          (Symbol.Map.map import_eid exp.symbol_id);
      constants = Symbol.Set.map import_sym exp.constants;
      constant_closures = exp.constant_closures;
      kept_arguments = exp.kept_arguments;
    }
  in
  res

let clear_import_state () = Export_id.Tbl.clear rename_id_state

let canonical_approx (approx : Flambdaexport.approx)
      ~canonical_symbol =
  match approx with
  | Value_unknown
  | Value_id _ as v -> v
  | Value_symbol sym -> Value_symbol (canonical_symbol sym)

let rec canonical_descr (descr : Flambdaexport.descr) ~canonical_symbol =
  match descr with
  | Value_block (tag, fields) ->
    Value_block (tag, Array.map (canonical_approx ~canonical_symbol) fields)
  | Value_int _
  | Value_constptr _
  | Value_string
  | Value_float _
  | Value_boxed_int _ as v -> v
  | Value_closure offset ->
    Value_closure { offset with closure =
      (aux_closure offset.closure ~canonical_symbol ) }
  | Value_set_of_closures clos ->
    Value_set_of_closures (aux_closure clos ~canonical_symbol)

and aux_closure clos ~canonical_symbol =
  let canonical_approx = canonical_approx ~canonical_symbol in
  { closure_id = clos.closure_id;
    bound_var = Var_within_closure.Map.map canonical_approx clos.bound_var;
    results = Closure_id.Map.map canonical_approx clos.results;
  }
