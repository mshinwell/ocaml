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

open Abstract_identifiers

let get_fun_offset t (id : Closure_id.t) =
  match Compilenv.find_in_any_unit t id E.get_fun_offset with
  | Some offset -> offset
  | None ->
    Misc.fatal_errorf "offset for function not found: %a"
      Closure_id.print id

let get_fun_offset_from_var t id =
  get_fun_offset t (Closure_id.wrap id)

let get_fv_offset t (var : Var_within_closure.t) =
  match Compilenv.find_in_any_unit t id E.get_fv_offset with
  | Some offset -> offset
  | None ->
    Misc.fatal_errorf "offset for free variable not found: %a"
      Var_within_closure.print id

let get_fv_offset_from_var t var =
  get_fv_offset t (Var_within_closure.wrap var)

let get_local_fv_offset_from_var t (var : Variable.t) =
  match Compilenv.find_in_any_unit' t id E.get_fv_offset with
  | Some (Current_unit (_, offset)) -> offset
  | Some (Imported_unit _) ->
    Misc.fatal_errorf "offset for free variable was expected to be in the \
      current compilation unit, but was found in an imported unit: %a"
      Var_within_closure.print id
  | None ->
    Misc.fatal_errorf "(local) offset for free variable not found: %a"
      Var_within_closure.print id

let fundecls_for_closure_id t (id : Closure_id.t) =
  match Compilenv.find_in_any_unit t id E.fundecls_for_closure_id with
  | Some fundecls -> fundecls
  | None ->
    Misc.fatal_errorf "could not find function declarations corresponding \
        to closure ID: %a"
      Closure_id.print id

let fundecls_for_set_of_closures_id t (id : Set_of_closures_id.t) =
  match Compilenv.find_in_any_unit t id E.fundecls_for_set_of_closures_id with
  | Some fundecls -> fundecls
  | None ->
    Misc.fatal_errorf "could not find function declarations corresponding \
        to set-of-closures ID: %a"
      Set_of_closures_id.print id

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

let import_set_of_closures units pack set_of_closures =
  { set_of_closures_id = set_of_closures.set_of_closures_id;
    bound_var =
      Var_within_closure.Map.map (import_approx_for_pack units pack)
        set_of_closures.bound_var;
    results =
      Closure_id.Map.map (import_approx_for_pack units pack)
       set_of_closures.results;
  }

let import_descr_for_pack units pack = function
  | Value_int _
  | Value_constptr _
  | Value_string
  | Value_float _
  | Value_boxed_int _ as desc -> desc
  | Value_block (tag, fields) ->
    Value_block (tag, Array.map (import_approx_for_pack units pack) fields)
  | Value_closure { closure_id; set_of_closures } ->
    Value_closure {
      closure_id;
      set_of_closures = import_set_of_closures units pack set_of_closures;
    }
  | Value_set_of_closures set_of_closures ->
    Value_set_of_closures (import_set_of_closures units pack set_of_closures)

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

let canonical_approx (approx : approx)
      ~canonical_symbol =
  match approx with
  | Value_unknown
  | Value_id _ as v -> v
  | Value_symbol sym -> Value_symbol (canonical_symbol sym)

let rec canonical_descr (descr : descr) ~canonical_symbol =
  match descr with
  | Value_block (tag, fields) ->
    Value_block (tag, Array.map (canonical_approx ~canonical_symbol) fields)
  | Value_int _
  | Value_constptr _
  | Value_string
  | Value_float _
  | Value_boxed_int _ as v -> v
  | Value_closure offset ->
    Value_closure { offset with set_of_closures =
      (aux_set_of_closures offset.set_of_closures ~canonical_symbol ) }
  | Value_set_of_closures set_of_closures ->
    Value_set_of_closures
      (aux_set_of_closures set_of_closures ~canonical_symbol)

and aux_set_of_closures set_of_closures ~canonical_symbol =
  let canonical_approx = canonical_approx ~canonical_symbol in
  { set_of_closures_id = set_of_closures.set_of_closures_id;
    bound_var = Var_within_closure.Map.map canonical_approx
        set_of_closures.bound_var;
    results = Closure_id.Map.map canonical_approx set_of_closures.results;
  }
