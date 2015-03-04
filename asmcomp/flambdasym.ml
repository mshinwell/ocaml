(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Misc
open Symbol
open Abstract_identifiers
open Flambda
open Flambdaexport
open Flambdautils

let constant_closures constant_result expr =
  Set_of_closures_id.Set.diff
    (all_closures expr)
    (constant_result.Flambdaconstants.not_constant_closure)

let functions not_constants expr =
  let cf_map = ref Closure_id.Map.empty in
  let fun_id_map = ref Set_of_closures_id.Map.empty in
  let argument_kept = ref Set_of_closures_id.Map.empty in
  let aux ({ cl_fun } as cl) _ =
    let add var _ map =
      Closure_id.Map.add (Closure_id.wrap var) cl_fun map in
    cf_map := Variable.Map.fold add cl_fun.funs !cf_map;
    fun_id_map := Set_of_closures_id.Map.add cl.cl_fun.ident cl.cl_fun !fun_id_map;
    argument_kept :=
      Set_of_closures_id.Map.add cl.cl_fun.ident
          (Flambdautils.unchanging_params_in_recursion cl_fun) !argument_kept
  in
  Flambdaiter.iter_on_closures aux expr;
  !fun_id_map, !cf_map, !argument_kept

module type Param1 = sig
  type t
  val expr : t Flambda.flambda
  val not_constants : Flambdaconstants.constant_result
  val constant_closures : Set_of_closures_id.Set.t
end

type const_sym =
  | Lbl of Symbol.t
  | No_lbl
  | Not_const
  | Const_closure

type infos = {
  global : (int, approx) Hashtbl.t;
  ex_table : descr Export_id.Map.t ref;
  symbol_id : Export_id.t Symbol.Map.t ref;
  constants : unit flambda Symbol.Tbl.t;
  symbol_alias : Symbol.t Symbol.Tbl.t;
}

let init_infos () = {
  global = Hashtbl.create 10;
  ex_table = ref Export_id.Map.empty;
  symbol_id = ref Symbol.Map.empty;
  constants = Symbol.Tbl.create 10;
  symbol_alias = Symbol.Tbl.create 10;
}

let rec canonical_symbol s infos =
  try
    let s' = Symbol.Tbl.find infos.symbol_alias s in
    let s'' = canonical_symbol s' infos in
    if s' != s''
    then Symbol.Tbl.replace infos.symbol_alias s s'';
    s''
  with Not_found -> s

let new_descr descr infos =
  let id = Export_id.create (Compilenv.current_unit ()) in
  infos.ex_table := Export_id.Map.add id descr !(infos.ex_table);
  id

module Conv(P:Param1) = struct
  open Flambdaexport

  let functions, closures, kept_arguments = functions P.not_constants P.expr

  let used_variable_withing_closure = list_used_variable_withing_closure P.expr


  let not_constants = P.not_constants
  let is_constant id =
    not (Variable.Set.mem id not_constants.Flambdaconstants.not_constant_id)

  type env = {
    sb : unit flambda Variable.Map.t; (* substitution *)
    cm : Symbol.t Variable.Map.t; (* variables associated to constants *)
    approx : approx Variable.Map.t;
  }

  let infos = init_infos ()

  let empty_env =
    { sb = Variable.Map.empty;
      cm = Variable.Map.empty;
      approx = Variable.Map.empty }

  let canonical_symbol s = canonical_symbol s infos
  let set_symbol_alias s1 s2 =
    let s1' = canonical_symbol s1 in
    let s2' = canonical_symbol s2 in
    if s1' <> s2'
    then Symbol.Tbl.add infos.symbol_alias s1' s2'

  let add_sb id subst env =
    { env with sb = Variable.Map.add id subst env.sb }

  let add_cm id const env =
    { env with cm = Variable.Map.add id const env.cm }

  let copy_env id' id env =
    try
      let const = Variable.Map.find id env.cm in
      add_cm id' const env
    with Not_found -> env

  let add_global i approx =
    Hashtbl.add infos.global i approx
  let get_global i =
    try Hashtbl.find infos.global i
    with Not_found ->
      (* Value_unknown *)
      fatal_error (Format.asprintf "no global %i" i)

  let add_approx id approx env =
    { env with approx = Variable.Map.add id approx env.approx }
  let get_approx id env =
    try Variable.Map.find id env.approx with Not_found -> Value_unknown

  let extern_symbol_descr sym =
    if Compilenv.is_predefined_exception sym
    then None
    else
      let export = Compilenv.approx_for_global sym.sym_unit in
      try
        let id = Symbol.Map.find sym export.symbol_id in
        let descr = find_description id export in
        Some descr
      with
      | Not_found -> None

  let extern_id_descr ex =
    let export = Compilenv.approx_env () in
    try Some (find_description ex export)
    with Not_found -> None

  let get_descr approx =
    match approx with
    | Value_unknown -> None
    | Value_id ex ->
        (try Some (Export_id.Map.find ex !(infos.ex_table)) with
         | Not_found ->
             extern_id_descr ex)
    | Value_symbol sym ->
        try
          let ex = Symbol.Map.find sym !(infos.symbol_id) in
          Some (Export_id.Map.find ex !(infos.ex_table))
        with Not_found ->
          extern_symbol_descr sym

  let add_symbol sym id =
    infos.symbol_id := Symbol.Map.add sym id !(infos.symbol_id)

  let symbol_id sym =
    try Some (Symbol.Map.find sym !(infos.symbol_id)) with Not_found -> None

  let add_constant lam ex_id =
    let sym = Compilenv.new_const_symbol' () in
    Symbol.Tbl.add infos.constants sym lam;
    add_symbol sym ex_id;
    sym

  let new_descr descr = new_descr descr infos
  let unit_approx () = Value_id (new_descr (Value_constptr 0))

  and is_simple_constant = function
    | Fconst _
    | Fsymbol _ -> true
    | _ -> false

  and constant_symbol : unit flambda -> const_sym = function
    | Fsymbol(sym, ()) ->
        Lbl sym
    | Fconst(_, ()) ->
        No_lbl
    | Fset_of_closures ({ cl_fun }, _) ->
        if Set_of_closures_id.Set.mem cl_fun.ident P.constant_closures
        then Const_closure
        else Not_const
    | _ -> Not_const
end

module type Param2 = sig
  include Param1
  val infos : infos
  val expr : unit flambda
end

module Prepare(P:Param2) = struct
  open P

  (*** Preparing export informations: Replacing every symbol by its
       canonical representant ***)

  let canonical_symbol s = canonical_symbol s infos

  (* Replace all symbols occurences by their representative *)
  let expr, constants =
    let use_canonical_symbols = function
      | Fsymbol(sym, ()) as expr ->
          let sym' = canonical_symbol sym in
          if sym == sym' then expr
          else Fsymbol(sym', ())
      | expr -> expr in
    let aux sym lam map =
      let sym' = canonical_symbol sym in
      Symbol.Map.add sym' (Flambdaiter.map use_canonical_symbols lam) map
    in
    Flambdaiter.map use_canonical_symbols expr,
    Symbol.Tbl.fold aux infos.constants Symbol.Map.empty

  let functions =
    let functions = ref Set_of_closures_id.Map.empty in
    let aux { cl_fun } _ =
      functions := Set_of_closures_id.Map.add cl_fun.ident cl_fun !functions
    in
    Flambdaiter.iter_on_closures aux expr;
    Symbol.Map.iter (fun _ -> Flambdaiter.iter_on_closures aux) constants;
    !functions

  let new_descr descr = new_descr descr infos

  (* build the approximation of the root module *)
  let root_id =
    let size_global =
      1 + (Hashtbl.fold (fun k _ acc -> max k acc) infos.global (-1)) in
    let fields = Array.init size_global (fun i ->
        try canonical_approx (Hashtbl.find infos.global i) with
        | Not_found -> Value_unknown) in
    new_descr (Value_block (0,fields))

  let root_approx =
    Value_id root_id

  (* replace symbol by their representative in value approximations *)
  let ex_values =
    Export_id.Map.map canonical_descr !(infos.ex_table)

  (* build the symbol to id and id to symbol maps *)
  let module_symbol =
    Compilenv.current_unit_symbol ()

  let symbol_id =
    let aux sym ex map =
      let sym' = canonical_symbol sym in
      Symbol.Map.add sym' ex map
    in
    Symbol.Map.fold aux !(infos.symbol_id) Symbol.Map.empty

  let symbol_id =
    Symbol.Map.add module_symbol root_id
      symbol_id
  let id_symbol =
    Symbol.Map.fold (fun sym id map -> Export_id.Map.add id sym map)
      symbol_id Export_id.Map.empty

  let functions_off =
    let aux_fun ffunctions off_id _ map =
      let fun_id = Closure_id.wrap off_id in
      Closure_id.Map.add fun_id ffunctions map in
    let aux _ f map = Variable.Map.fold (aux_fun f) f.funs map in
    Set_of_closures_id.Map.fold aux functions Closure_id.Map.empty
end

let convert (type a) ~compilation_unit (expr:a Flambda.flambda) =
  let not_constants =
    Flambdaconstants.not_constants ~compilation_unit ~for_clambda:true expr
  in
  let constant_closures = constant_closures not_constants expr in
  let module P1 = struct
    type t = a
    let expr = expr
    let not_constants = not_constants
    let constant_closures = constant_closures
  end in
  let module C = Conv(P1) in
  let module P2 = struct
    include P1
    let expr = C.expr
    let infos = C.infos
  end in
  let module C2 = Prepare(P2) in
  let export = let open Flambdaexport in
    { empty_export with
      ex_values = Flambdaexport.nest_eid_map C2.ex_values;
      globals = Ident.Map.singleton
          (Compilenv.current_unit_id ()) C2.root_approx;
      symbol_id = C2.symbol_id;
      id_symbol = Flambdaexport.nest_eid_map C2.id_symbol;
      functions = C2.functions;
      functions_off = C2.functions_off;
      constant_closures = constant_closures;
      kept_arguments = C.kept_arguments;
    }
  in
  C2.expr, C2.constants, export
