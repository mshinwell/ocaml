(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Pierre Chambart and Mark Shinwell                   *)
(*                                                                     *)
(*  Copyright 2014--2015, OCamlPro                                     *)
(*  Copyright 2015, Jane Street Group                                  *)
(*                                                                     *)
(*  Licensed under the Apache License, Version 2.0 (the "License");    *)
(*  you may not use this file except in compliance with the License.   *)
(*  You may obtain a copy of the License at                            *)
(*                                                                     *)
(*      http://www.apache.org/licenses/LICENSE-2.0                     *)
(*                                                                     *)
(*  Unless required by applicable law or agreed to in writing,         *)
(*  software distributed under the License is distributed on an        *)
(*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,       *)
(*  either express or implied.  See the License for the specific       *)
(*  language governing permissions and limitations under the License.  *)
(*                                                                     *)
(***********************************************************************)

open Abstract_identifiers

module Compilation_unit = Symbol.Compilation_unit
module E = Flambda_exports_by_unit

type t = {
  exported : E.t;
  symbol_alias : Symbol.t Symbol.Tbl.t;
  mutable ex_table : Flambdaexport.descr Export_id.Map.t;
}

let create () =
  { exported = E.create ();
    symbol_alias = Symbol.Tbl.create 42;
    ex_table = Export_id.Map.empty;
  }

let rec canonical_symbol t s =
  try
    let s' = Symbol.Tbl.find t.symbol_alias s in
    let s'' = canonical_symbol t s' in
    if s' != s'' then Symbol.Tbl.replace t.symbol_alias s s'';
    s''
  with Not_found -> s

let set_symbol_alias t s1 s2 =
  let s1' = canonical_symbol t s1 in
  let s2' = canonical_symbol t s2 in
  if s1' <> s2' then Symbol.Tbl.add t.symbol_alias s1' s2'

let structured_constant_for_symbol (sym : Symbol.t)
      (ulambda : Clambda.ulambda) =
  match ulambda with
  | Uconst (Uconst_ref (lbl', Some cst)) ->
    let lbl =
      Compilenv.canonical_symbol
        (Symbol.string_of_linkage_name sym.sym_label)
    in
    assert (lbl = Compilenv.canonical_symbol lbl');
    cst
  (* CR mshinwell for pchambart: We need to work out whether the following
     line is needed, or not. *)
  (* | Uconst (Uconst_ref (None, Some cst)) -> cst *)
  | _ -> assert false

(* Given a description of a value to be exported from the current
   compilation unit, assign it a new export ID, and record the mapping from
   ID to description. *)
let add_new_export t (descr : Flambdaexport.descr) =
  let id = Export_id.create (Compilenv.current_unit ()) in
  t.ex_table <- Export_id.Map.add id descr t.ex_table;
  id

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

let add_new_export descr = add_new_export descr infos

let unit_approx () = Value_id (add_new_export (Value_constptr 0))

let empty_env =
  { subst = Variable.Map.empty;
    var = Variable.Map.empty;
  }

let add_sb id subst env =
  { env with subst = Variable.Map.add id subst env.subst }

let find_sb id env = Variable.Map.find id env.subst
let find_var id env = Variable.Map.find id env.var

let add_unique_ident var env =
  let id = Variable.unique_ident var in
  id, { env with var = Variable.Map.add var id env.var }

let add_unique_idents vars env =
  let env_handler, ids =
    List.fold_right (fun var (env, ids) ->
        let id, env = add_unique_ident var env in
        env, id :: ids)
      vars (env, [])
  in
  ids, env_handler

let rewritten_flambda_and_approx_for_constant (cst : Flambda.const)
      : (_ Flambda.t option * Flambdaexport.descr) =
  match cst with
  | Fconst_base (Const_int i) ->
    None, Value_id (add_new_export (Value_int i))
  | Fconst_base (Const_char c) ->
    None, Value_id (add_new_export (Value_int (Char.code c)))
  | Fconst_base (Const_float s) ->
    None, Value_id (add_new_export (Value_float (float_of_string s)))
  | Fconst_base (Const_int32 i) ->
    None, Value_id (add_new_export (Value_boxed_int (Int32, i)))
  | Fconst_base (Const_int64 i) ->
    None, Value_id (add_new_export (Value_boxed_int (Int64, i)))
  | Fconst_base (Const_nativeint i) ->
    None, Value_id (add_new_export (Value_boxed_int (Nativeint, i)))
  | Fconst_float f ->
    None, Value_id (add_new_export (Value_float f))
  | Fconst_pointer c ->
    None, Value_id (add_new_export (Value_constptr c))
  | Fconst_base (Const_string _) | Fconst_immstring _
  | Fconst_float_array _ ->
    let id = add_new_export Value_string in
    Some (Fsymbol (add_constant (Fconst (cst, ())) id, ())), Value_id id

let clambda_for_constant expected_symbol cst : Clambda.uconstant =
  let add_new_exported_structured_const ?not_shared cst : Clambda.uconstant =
    (* CR mshinwell: try to hoist this function *)
    let name =
      let shared =
        match not_shared with
        | None -> true
        | Some () -> false
      in
      Compilenv.structured_constant_label expected_symbol ~shared cst
    in
    Uconst_ref (name, Some cst)
  in
  match cst with
  | Fconst_base (Const_int c) -> Uconst_int c
  | Fconst_base (Const_char c) -> Uconst_int (Char.code c)
  | Fconst_pointer c -> Uconst_ptr c
  | Fconst_float f -> add_new_exported_structured_const (Uconst_float f)
  | Fconst_float_array c ->  (* constant float arrays are really immutable *)
    add_new_exported_structured_const
      (Uconst_float_array (List.map float_of_string c))
  | Fconst_immstring c ->
    add_new_exported_structured_const (Uconst_string c)
  | Fconst_base (Const_float x) ->
    add_new_exported_structured_const (Uconst_float (float_of_string x))
  | Fconst_base (Const_int32 x) ->
    add_new_exported_structured_const (Uconst_int32 x)
  | Fconst_base (Const_int64 x) ->
    add_new_exported_structured_const (Uconst_int64 x)
  | Fconst_base (Const_nativeint x) ->
    add_new_exported_structured_const (Uconst_nativeint x)
  | Fconst_base (Const_string (s, o)) ->
    add_new_exported_structured_const ~not_shared:() (Uconst_string s)

let fclambda_and_approx_for_constant cst =
  let flambda, approx =
    match rewritten_flambda_and_approx_for_constant cst with
    | None, approx -> flambda, approx
    | Some flambda, approx -> flambda, approx
  in
  let clambda = clambda_for_constant cst in
  flambda, clambda, approx

(* Note: there is at most one closure (likewise, one set of closures) in
   scope during any one call to [conv].  Accesses to outer closures are
   performed via this distinguished one.  (This was arranged during closure
   conversion---see [Flambdagen].) *)
(* CR mshinwell: put [expected_symbol] in [env] *)
let rec fclambda_and_approx_for_expr t (env : env) (expr : _ Flambda.t)
      : unit Flambda.t * Clambda.ulambda * Flambdaexport.approx =
  match expr with
  | Fvar (var, _) -> flambda_and_approx_for_var t env ~var
  | Fsymbol (sym, _) ->
    let sym = canonical_symbol t sym in
    let linkage_name = Symbol.string_of_linkage_name sym.sym_label in
    let label = Compilenv.canonical_symbol linkage_name in
    (* CR pchambart for pchambart: Should delay the conversion a bit more
       mshinwell: I turned this comment into a CR.  What does it mean? *)
    Fsymbol sym, Uconst (Uconst_ref (label, None)), Value_symbol sym
  | Fconst (cst, _) -> Uconst (conv_const expected_symbol cst)
  | Flet (str, var, lam, body, _) ->
    fclambda_and_approx_for_let t ~env ~str ~var ~lam ~body
  | Fletrec (defs, body, _) ->
    let env, defs =
      List.fold_right (fun (var, def) (env, defs) ->
          let id, env = add_unique_ident var env in
          env, (id, def) :: defs)
        defs (env, [])
    in
    let udefs = List.map (fun (id, def) -> id, conv env def) defs in
    Uletrec (udefs, conv env body)
  | Fset_of_closures set_of_closures ->
    fclambda_and_approx_for_set_of_closures t env set_of_closures
  | Fclosure closure -> fclambda_and_approx_for_closure env closure
  | Fvariable_in_closure var_in_closure ->
    fclambda_and_approx_for_variable_in_closure env var_in_closure
  | Fapply apply -> fclambda_and_approx_for_application env apply
  | Fswitch (arg, sw, d) -> fclambda_and_approx_for_switch env ~arg ~sw ~d
  | Fstringswitch (arg, sw, def, d) ->
    let arg, uarg = fclambda_for_expr env arg in
    let sw, usw =
      List.fold_left (fun (sw, usw) (s, e) ->
          let e, ue = fclambda_for_expr env e in
          e::sw, ue::usw)
        [] sw
    in
    let def, udef =
      match def with
      | None -> None, None
      | Some def ->
        let def, udef = fclambda_for_expr env def in
        Some def, Some udef
    in
    Fstringswitch (arg, sw, def, d), Ustringswitch (uarg, usw, udef),
      Value_unknown
  | Fprim (primitive, args, dbg, _) ->
    let args, uargs = fclambda_for_expr_list env args in
    conv_primitive ~env ~primitive ~args ~uargs ~dbg
  | Fstaticraise (i, args, _) ->
    let args, uargs = fclambda_for_expr_list env args in
    Fstaticraise (i, args, ()),
      Ustaticfail (Static_exception.to_int i, uargs), Value_unknown
  | Fstaticcatch (i, vars, body, handler, _) ->
    let body, ubody = fclambda_for_expr env body in
    let ids, env_handler = add_unique_idents vars env in
    let handler = fclambda_for_expr env_handler body in
    Fstaticcatch (i, vars, body, handler),
      Ucatch (Static_exception.to_int i, ids, ubody, uhandler),
      Value_unknown
  | Ftrywith (body, var, handler, _) ->
    let body, ubody = fclambda_for_expr env body in
    let id, env_handler = add_unique_ident var env in
    let handler, uhandler = fclambda_for_expr env_handler body in
    Ftrywith (body, var, handler), Utrywith (ubody, id, uhandler),
      Value_unknown
  | Fifthenelse (arg, ifso, ifnot, _) ->
    let arg, uarg = fclambda_for_expr env arg in
    let ifso, uifso, ifnot, uifnot = fclambda_for_expr_pair env ifso ifnot in
    Fifthenelse (arg, ifso, ifnot, ()), Uifthenelse (uarg, uifso, uifnot),
      Value_unknown
  | Fsequence (lam1, lam2, _) ->
    let lam1, ulam1 = fclambda_for_expr env lam1 in
    let lam2, ulam2, approx = fclambda_and_approx_for_expr env lam2 in
    Fsequence (lam1, lam2, ()), Usequence (ulam1, ulam2), approx
  | Fwhile (cond, body, _) ->
    let cond, ucond, body, ubody = fclambda_for_expr_pair env cond body in
    Fwhile (cond, body, ()), Uwhile (cond, body), unit_approx ()
  | Ffor (var, lo, hi, dir, body, _) ->
    let lo, ulo, hi, uhi = fclambda_for_expr_pair env lo hi in
    let id, env_body = add_unique_ident var env in
    let body, ubody = fclambda_for_expr env_body body in
    Ffor (var, lo, hi, dir, body, ()), Ufor (id, lo, hi, dir, body),
      unit_approx ()
  | Fassign (var, lam, _) ->
    let lam, ulam = fclambda_for_expr env lam in
    let id = try find_var var env with Not_found -> assert false in
    Fassign (var, lam, ()), Uassign (id, ulam), unit_approx ()
  | Fsend (kind, met, obj, args, dbg, _) ->
    let met, umet = fclambda_for_expr env met in
    let obj, uobj = fclambda_for_expr env obj in
    let args, uargs = fclambda_for_expr_list env args in
    Fsend (kind, met, obj, args, dbg, ()),
      Usend (kind, umet, uobj, uargs, dbg), Value_unknown
  (* CR pchambart for pchambart: shouldn't be executable, maybe build
     something else
     mshinwell: I turned this into a CR. *)
  | Funreachable _ -> Funreachable (), Uunreachable, Value_unknown
    (* Uprim (Praise, [Uconst (Uconst_pointer 0, None)], Debuginfo.none) *)
  | Fevent _ -> assert false

and fclambda_for_expr env expr =
  let expr, _approx = fclambda_and_approx_for_expr env expr in
  expr

and fclambda_for_expr_pair env expr1 expr2 =
  let expr1, uexpr1 = fclambda_for_expr env expr1 in
  let expr2, uexpr2 = fclambda_for_expr env expr2 in
  expr1, uexpr1, expr2, uexpr2

and fclambda_and_approx_for_primitive t env ~(primitive : Lambda.primitive)
      ~args ~dbg : unit Flambda.t * Clambda.ulambda * Flambdaexport.descr =
  match primitive, args, dbg with
  | Pgetglobal id, l, _ ->
    (* Accesses to globals are transformed into symbol accesses. *)
    assert (l = []);
    let sym = Compilenv.symbol_for_global' id in
    fclambda_and_approx_for_expr t env (Fsymbol (sym, ()))
  | Pgetglobalfield (id, i), l, dbg ->
    assert (l = []);
    let lam : _ Flambda.t =
      Fprim (Pfield i, [Fprim (Pgetglobal id, l, dbg, v)], dbg, v)
    in
    if id <> Compilenv.current_unit_id () then
      fclambda_and_approx_for_expr env lam
    else
      let approx = get_global i in
      begin match approx with
      | Value_symbol sym -> fclambda_for_expr env (Fsymbol (sym, ())), approx
      | Value_unknown _ | Value_id _ -> fclambda_for_expr env lam, approx
      end
  | Psetglobalfield i, [arg], dbg ->
    (* CR mshinwell: review this case and the one above once we figure out
       what is going on *)
    let arg, uarg, approx =
      fclambda_and_approx_for_expr ?expected_symbol env arg
    in
    add_global i approx;
    Fprim (Psetglobalfield i, [arg], dbg, ()),
      Uprim (Psetfield (i, false),
          [Uprim (Pgetglobal (Ident.create_persistent
              (Compilenv.make_symbol None)), [], dbg);
           conv env arg],
          dbg),
      Value_unknown
  | (Pmakeblock (tag, Asttypes.Immutable)) as p, args, dbg ->
    (* If we know the values of all of the fields of the new block, then
       emit it as data, with a symbol to identify it.  The original
       [Pmakeblock] turns into a reference to the symbol. *)
    let args, approxs =
      clambda_and_approx_for_list ?expected_symbol env args
    in
    let ex = add_new_export (Value_block (tag, Array.of_list approxs)) in
    begin match constant_list args with
    | None -> Uprim (p, args, dbg), Value_id ex
    | Some arg_values ->
      (* CR mshinwell: probably shouldn't be Uprim.  Just store [args] *)
      let sym = add_constant (Uprim (p, args, dbg)) ex in
      let cst : Clambda.ustructured_constant =
        Uconst_block (tag, arg_values)
      in
      let lbl = Compilenv.structured_constant_label sym ~shared:true cst in
      Uconst (Uconst_ref (lbl, Some cst)), Value_symbol sym
    end
  | Pfield i, [arg], dbg ->
    let block, block_approx =
      clambda_and_approx_for_expr ?unknown_symbol env arg
    in
    let approx =
      match get_descr block_approx with
      | Some (Value_block (_, fields)) ->
        if i >= 0 && i < Array.length fields then fields.(i)
        else Value_unknown
      | _ -> Value_unknown
    in
    Uprim (Pfield i, [block], dbg, ()), approx
  | primitive, args, dbg ->
    Uprim (primitive, conv_list env args, dbg), Value_unknown

and fclambda_and_approx_for_var t env ~var =
  let ulam, approx =
    (* If the variable references a constant, it is replaced by the
       constant's label. *)
    begin match Variable.Map.find id env.cm with
    | lbl -> Fsymbol (lbl, ()), Value_symbol lbl
    | exception Not_found ->
      (* If the variable is a recursive access to the function currently
         being defined, it is replaced by an offset in the closure.  If
         the variable is bound by the closure, it is replaced by a field
         access inside the closure. *)
      try
        let ulam =
      (* unfinished *)
            clambda_for_expr ?expected_symbol env (Variable.Map.find id env.sb) in
        lam, get_approx id env
      with Not_found ->
        Fvar (id, ()), get_approx id env
  in
  (* If the variable is bound by any current closure, or is one of the
     function identifiers bound by any current set of closures, then we
     can find out how to access the variable by looking in the
     substitution inside [env].  Such substitutions are constructed below,
     in [conv_closure].

     For variables not falling into these categories, we must use
     the [var] mapping inside [env], which turns [Variable.t] values into
     [Ident.t] values as required in the [Clambda] intermediate
     language.
  *)
  begin try find_sb var env
  with Not_found ->
    try Uvar (find_var var env)
    with Not_found ->
      Misc.fatal_error
        (Format.asprintf "Clambdagen.conv: unbound variable %a@."
           Variable.print var)
  end
(* from flambdasym:
          (* If the variable reference a constant, it is replaced by the
             constant label *)
          try
            let lbl = Variable.Map.find id env.cm in
            Fsymbol(lbl, ()), Value_symbol lbl
          with Not_found ->

            (* If the variable is a recursive access to the function
               currently being defined: it is replaced by an offset in the
               closure. If the variable is bound by the closure, it is
               replace by a field access inside the closure *)
            try
              let lam = Variable.Map.find id env.sb in
              lam, get_approx id env
            with Not_found ->
              Fvar (id, ()), get_approx id env
*)

and fclambda_and_approx_for_closure env
      { fu_closure = lam; fu_fun = id; fu_relative_to = rel } =




  let ulam, approx =
    if E.is_function_local_and_constant t.exported id then
      (* Only references to functions declared in the current module should
         need rewriting to a symbol.  For external functions this should
         already have been done at the original declaration. *)
      let sym = Compilenv.closure_symbol id in
      clambda_and_approx_for_expr ?expected_symbol env (Fsymbol (sym, ()))
    else
      let ulam, fun_approx =
        clambda_and_approx_for_expr ?expected_symbol env lam
      in
      let approx =
        match get_descr fun_approx with
        | Some (Value_set_of_closures closure)
        | Some (Value_closure { closure }) ->
          Value_id (add_new_export (Value_closure { fun_id = id; closure }))
        | Some _ -> assert false
        | _ ->
          Format.printf "Bad approximation for Fclosure expression: %a@."
            Printflambda.flambda expr;
          assert false
      in
      clambda_and_approx_for_expr ?expected_symbol env
          (Fclosure ({ fu_closure = ulam; fu_fun = id;
              fu_relative_to = rel }, ()),
        approx
  in
  let relative_offset =
    let offset = get_fun_offset id in
    match rel with
    | None -> offset
    | Some rel -> offset - get_fun_offset rel
  in
  (* Compilation of [let rec] in [Cmmgen] assumes that a closure is not
     offseted ([Cmmgen.expr_size]). *)
  if relative_offset = 0 then ulam
  else Uoffset (ulam, relative_offset)

and fclambda_and_approx_for_variable_in_closure t env
      { vc_closure = closure; vc_var = var; vc_fun = fun_id } =
  let closure, uclosure, closure_approx =
    fclambda_and_approx_for_expr env closure
  in
  let approx =
    match get_descr closure_approx with
    | Some (Value_closure { closure = { bound_var = bound_vars; } }) ->
      begin match Var_within_closure.Map.find var bound_vars with
      | approx -> approx
      | exception Not_found ->
        Misc.fatal_errorf "Fvariable_in_closure expression references \
            variable not bound by the given closure: %a@.%a@."
          Printflambda.flambda expr
          Printflambda.flambda closure
      end
    | Some _ ->
      Misc.fatal_errorf "Fvariable_in_closure expression references \
          variable with a non-Value_closure approximation: %a@.%a@."
        Printflambda.flambda expr
        Printflambda.flambda closure
    | None ->
      Misc.fatal_errorf "Fvariable_in_closure expression references \
          closure that has no approximation: %a@.%a@."
        Printflambda.flambda expr
        Printflambda.flambda closure
  in
  let offset_within_closure = get_fv_offset var - get_fun_offset fun_id in
  Fvariable_in_closure ({ vc_closure = closure; vc_var = var;
      vc_fun = fun_id}, ()),
    (* CR mshinwell: [Debuginfo.none] is almost certainly wrong *)
    Uprim (Pfield offset_within_closure, [uclosure], Debuginfo.none),
    approx

and fclambda_and_approx_for_let t ~env ~str ~var ~lam ~body =
  let ulam, approx =
    clambda_and_approx_for_expr ?expected_symbol env lam
  in
  let env =
    if is_constant id || str = Not_assigned
    then add_approx id approx env
    else add_approx id Value_unknown env
  in
  begin match is_constant id, constant_symbol lam, str with
  | _, _, Assigned
  | false, (Not_const | No_lbl | Const_closure), _ ->
    let id, env_body = add_unique_ident var env in
    let ubody, body_approx =
      clambda_and_approx_for_expr ?expected_symbol env body
    in
    Ulet (id, conv env lam, conv env_body body), body_approx
  | true, No_lbl, Not_assigned ->
    (* No label => the value is an integer: substitute it. *)
    clambda_and_approx_for_expr ?expected_symbol (add_sb id lam env) body
  | _, Lbl lbl, Not_assigned ->
    (* Label => the value is a block: reference it. *)
    clambda_and_approx_for_expr ?expected_symbol (add_cm id lbl env) body
  | true, Const_closure, Not_assigned ->
    clambda_and_approx_for_expr ?expected_symbol env body
  | true, Not_const, Not_assigned ->
    Format.eprintf "%a@.%a" Variable.print id Printflambda.flambda lam;
    assert false
  end

and fclambda_and_approx_for_switch t env cases num_keys default =
  let module Switch_storer =
    Switch.Store (struct
      type t = _ Flambda.t
      type key = Flambdautils.sharing_key
      let make_key = Flambdautils.make_key
    end)
  in
    let arg = clambda_for_expr ?expected_symbol env arg in
    (*
      fs_consts = List.map (fun (i,lam) -> i, conv env lam) sw.fs_consts;
      fs_blocks = List.map (fun (i,lam) -> i, conv env lam) sw.fs_blocks;
      fs_failaction = may_map (conv env) sw.fs_failaction }, ()),
    *)
    let aux () : Clambda.ulambda =
      let const_index, const_actions =
        conv_switch env sw.fs_consts sw.fs_numconsts sw.fs_failaction
      and block_index, block_actions =
        conv_switch env sw.fs_blocks sw.fs_numblocks sw.fs_failaction
      in
      Uswitch (conv env arg, {
        us_index_consts = const_index;
        us_actions_consts = const_actions;
        us_index_blocks = block_index;
        us_actions_blocks = block_actions;
      })
    in
    let rec simple_expr (expr : _ Flambda.t) =
      match expr with
      | Fconst ( Fconst_base (Asttypes.Const_string _), _ ) -> false
      | Fvar _ | Fsymbol _ | Fconst _ -> true
      | Fstaticraise (_, args, _) -> List.for_all simple_expr args
      | _ -> false
    in
    (* Check that failaction is effectively copyable: i.e. it can't declare
       symbols.  If this is not the case, share it through a
       staticraise/staticcatch *)
    let ulam =
      match sw.fs_failaction with
      | None -> aux ()
      | Some (Fstaticraise (_, args, _))
          when List.for_all simple_expr args -> aux ()
      | Some failaction ->
        let exn = Static_exception.create () in
        let fs_failaction = Some (Flambda.Fstaticraise (exn, [], d)) in
        let sw = { sw with fs_failaction } in
        clambda_for_expr ?expected_symbol env
          (Fstaticcatch (exn, [], Fswitch (arg, sw, d), failaction, d))
    in
    ulam, Value_unknown
  let num_keys =
    if Ext_types.Int.Set.cardinal num_keys = 0
    then 0
    else Ext_types.Int.Set.max_elt num_keys + 1 in
  let index = Array.make num_keys 0 in
  let store = Switch_storer.mk_store () in
  (* First the default case. *)
  begin match default with
  | Some def when List.length cases < num_keys ->
      ignore (store.Switch.act_store def)
  | _ -> ()
  end ;
  (* Then all other cases. *)
  List.iter (fun (key, lam) -> index.(key) <- store.Switch.act_store lam) cases;
  (* Compile the actions. *)
  let actions = Array.map (conv env) (store.Switch.act_get ()) in
  match actions with
  | [| |] -> [| |], [| |] (* May happen when [default] is [None] *)
  | _ -> index, actions

and clambda_and_approx_for_let_rec env defs body =
      let consts, not_consts =
        List.partition (fun (id,_) -> is_constant id) defs in

      let env, consts = List.fold_left
          (fun (env, acc) (id,def) ->
             let open Asttypes in
             match def with
             | Fconst (( Fconst_pointer _
                       | Fconst_base
                           (Const_int _ | Const_char _
                           | Const_float _ | Const_int32 _
                           | Const_int64 _ | Const_nativeint _)), _) ->
                 (* When the value is an integer constant, we cannot affect a label
                    to it: hence we must substitute it directly.
                    For other numerical constant, a label could be attributed, but
                    unboxing doesn't handle it well *)
                 add_sb id (conv env def) env, acc
             | Fvar (var_id, _) ->
                 assert(List.for_all(fun (id,_) -> not (Variable.equal var_id id)) consts);
                 (* For variables: the variable could have been substituted to
                    a constant: avoid it by substituting it directly *)
                 add_sb id (conv env def) env, acc
             | _ ->
                 let sym = Compilenv.new_const_symbol' () in
                 let env = add_cm id sym env in
                 env, (id,sym,def)::acc) (env,[]) consts in

      List.iter (fun (id,sym,def) ->
          match constant_symbol (conv env def) with
          | Lbl sym' ->
              (match symbol_id sym' with
               | None -> ()
               | Some eid -> add_symbol sym eid);
              set_symbol_alias sym sym'
          | _ ->
              fatal_error (Format.asprintf
                             "recursive constant value without symbol %a"
                             Variable.print id))
        consts;

      let not_consts, env =
        List.fold_right (fun (id,def) (not_consts,env') ->
            let flam, approx = conv_approx env def in
            let env' = add_approx id approx env' in
            (id, flam) :: not_consts, env') not_consts ([],env) in

      let body, approx = conv_approx env body in
      (match not_consts with
       | [] -> body
       | _ -> Fletrec(not_consts, body, ())),
      approx

and fclambda_and_approx_for_one_closure_in_a_set_of_closures t env id func =
  (* inside the body of the function, we cannot access variables
     declared outside, so take a clean substitution table. *)
  let env = { env with sb = Variable.Map.empty } in

  (* add informations about currently defined functions to
     allow direct call *)
  let env =
    Variable.Map.fold (fun id _ env ->
        let fun_id = Closure_id.wrap id in
        let desc = Value_closure { fun_id; closure = value_closure' } in
        let ex = add_new_export desc in
        if closed then add_symbol (Compilenv.closure_symbol fun_id) ex;
        add_approx id (Value_id ex) env)
      functs.funs env
  in

  let env =
    (* param_approxs must be constants: part of specialised_args *)
    Variable.Map.fold (fun id approx env -> add_approx id approx env)
      param_approxs env in

  (* Add to the substitution the value of the free variables *)
  let add_env_variable id lam env =
    match constant_symbol lam with
    | Not_const ->
        assert(not closed);
        env
    | No_lbl ->
        add_sb id lam env
    | Lbl lbl ->
        add_cm id lbl env
    | Const_closure ->
        env
  in
  let env = Variable.Map.fold add_env_variable fv_ulam env in

  let env =
    if closed
    then
      (* if the function is closed, recursive call access those constants *)
      Variable.Map.fold (fun id _ env ->
          let fun_id = Closure_id.wrap id in
          add_cm id (Compilenv.closure_symbol fun_id) env) functs.funs env
    else env
  in
  let body, approx = conv_approx env func.body in
  { func with
    free_variables = Variable.Set.filter kept_fv func.free_variables;
    body }, approx
  (* there follows the Clambdagen part *)
  let conv_function (id, (func : _ Flambda.function_declaration))
        : Clambda.ufunction =
    let cf = Closure_id.wrap id in
    let fun_offset = Closure_id.Map.find cf P.fun_offset_table in
    let env =
      (* Create a substitution that shows how to access variables
         that were originally free in the function from the closure. *)
      let env =
        List.fold_left (fun env (id, (lam : Clambda.ulambda)) ->
            match
              Var_within_closure.Map.find (Var_within_closure.wrap id)
                P.fv_offset_table
            with
            | exception Not_found -> env
            | var_offset ->
              let closure_element_not_statically_known =
                (* CR mshinwell for pchambart: Didn't you say that some of
                   these should have been substituted out earlier?  That
                   appears not to be the case. *)
                match lam with
                | Uconst (Uconst_int _ | Uconst_ptr _ | Uconst_ref _)
                | Uprim (Pgetglobal _, [], _) -> false
                | _ -> true
              in
              assert (not (closure_is_constant
                && closure_element_not_statically_known));
              if closure_element_not_statically_known then
                let pos = var_offset - fun_offset in
                add_sb id (Uprim (Pfield pos, [Uvar env_var],
                    Debuginfo.none)) env
              else
                env)
          (* Inside the body of the function, we cannot access variables
             declared outside, so take a clean substitution table. *)
          empty_env
          fv_ulam
      in
      (* Augment the substitution with ways of accessing the function
         identifiers bound by the closure. *)
      (* CR mshinwell for pchambart: We need to understand if there might
         be a case where [closure_is_constant] is true and we inserted
         values into the substitution, above.  If there is no such case,
         we should consider moving this next [if] above the [fold_left].
         (Actually, there's something in the large comment above that may
         be relevant.) *)
      if closure_is_constant then env
      else
        let add_offset_subst pos env (id, _) =
          let offset =
            Closure_id.Map.find (Closure_id.wrap id) P.fun_offset_table
          in
          (* Note that the resulting offset may be negative, in the case
             where we are accessing an earlier (= with lower address)
             closure in a block holding multiple closures. *)
          let exp : Clambda.ulambda = Uoffset (Uvar env_var, offset - pos) in
          add_sb id exp env
        in
        List.fold_left (add_offset_subst fun_offset) env funct
    in
    let env_body, params =
      List.fold_right (fun var (env, params) ->
          let id, env = add_unique_ident var env in
          env, id :: params)
        func.params (env, [])
    in
    { label = Compilenv.function_label cf;
      arity = Flambdautils.function_arity func;
      params = if closure_is_constant then params else params @ [env_var];
      body = conv env_body func.body;
      dbg = func.dbg;
    }
  in

(* renamed functs -> fundecls. *)
and fclambda_and_approx_for_set_of_closures t env
      (fundecls : _ Flambda.function_declarations)
      (fv : _ Flambda.t Variable.Map.t)
      (specialised_args : Variable.t Variable.Map.t) =
  let is_constant = E.is_set_of_closures_local_and_constant fundecls.ident in
  (* Compute fclambda and approximations for the free variables of the
     set of closures. *)
  let fvs_with_approxs =
    Variable.Map.map (fclambda_and_approx_for_expr t env) fv
  in
  let fvs_to_approxs =
    Variable.Map.map (fun (_flambda, _clambda, approx) -> approx)
      fvs_with_approxs
  in
  (* Augment the environment with:
     1. approximations of the free variables of the set of closures;
     2. variable-symbol equalities, for arguments whose values are known to
        always be equal to the value associated with a given symbol. *)
  let env =
    Env.add_variable_symbol_equalities
      (Env.add_approximations env fvs_to_approxs)
      specialised_args_with_symbol_equalities
  in
  let specialised_args =
    if closed then
      (* CR mshinwell: "already have been performed" where? *)
      (* Constant closures will be moved out of their scope and assigned to
         symbols.  When this happens, we must erase any constraint that
         specializes an argument to another variable, since that variable may
         no longer be in scope.  (Specializations of variables to values that
         are now referenced by symbols, rather than variables, will already have
         been performed.  As such, the operation is equivalent to erasing all
         specialization information.)
      *)
      Variable.Map.empty
    else
      (* For a set of closures that is non-constant, remove from
         [specialised_args] any arguments that have a variable-symbol
         equality. *)
      Variable.Map.filter (fun _arg specialised_to ->
          not (Env.variable_has_symbol_equality env specialised_to))
        specialised_args
  in

  let kept_fv id =
    let cv = Var_within_closure.wrap id in
    not (is_constant id)
    || (Var_within_closure.Set.mem cv used_variable_withing_closure) in

  let used_fv_approx = Variable.Map.filter (fun id _ -> kept_fv id) fv_ulam_approx in
  let used_fv = Variable.Map.map (fun (lam,approx) -> lam) used_fv_approx in

  let varmap_to_closfun_map map =
    Variable.Map.fold (fun var v acc ->
        let cf = Closure_id.wrap var in
        Closure_id.Map.add cf v acc)
      map Closure_id.Map.empty in

  let value_closure' =
    { closure_id = functs.ident;
      bound_var =
        Variable.Map.fold (fun off_id (_,approx) map ->
            let cv = Var_within_closure.wrap off_id in
            Var_within_closure.Map.add cv approx map)
          used_fv_approx Var_within_closure.Map.empty;
      results =
        varmap_to_closfun_map
          (Variable.Map.map (fun _ -> Value_unknown) functs.funs) } in





  let funs_approx = Variable.Map.mapi conv_function functs.funs in
(* conv_function -> fclambda_and_approx_for_one_closure_in_a_set_of_closures *)

  let fundecls =
    { fundecls with
      funs = ...;
    }
  in
(*  let ufunct = { functs with funs = Variable.Map.map fst funs_approx } in*)

  let value_closure' =
    { value_closure' with
      results = varmap_to_closfun_map (Variable.Map.map snd funs_approx) } in

  (* The set of closures will always be exported; assign a new export ID
     and build the approximation. *)
  let closure_ex_id = add_new_export (Value_set_of_closures value_closure') in

  let expr =
    let expr =
      Fset_of_closures ({ cl_fun = ufunct;
                  cl_free_var = used_fv;
                  cl_specialised_arg = spec_arg }, ()) in
    if Set_of_closures_id.Set.mem ufunct.ident P.constant_closures
    then
      let sym = add_constant expr closure_ex_id in
      Fsymbol(sym, ())
    else expr in
  expr, value_closure
(* end flambdasym set of closures "conv_closure" case *)
  (* Substitute variables bound by the set of closures (function identifiers
     and free variables) for code that accesses their values from the
     closure value that will exist at runtime.  There is one closure value
     for each set of closures.  As an example, the closure value for:
       let rec fun_a x =
         if x <= 0 then 0 else fun_b (x-1) v1
       and fun_b x y =
         if x <= 0 then 0 else v1 + v2 + y + fun_a (x-1)

     will be represented in memory like this:
       [ Closure_tag header; fun_a; 1;
         Infix_tag header; fun caml_curry_2; 2; fun_b; v1; v2;
       ]

     fun_a and fun_b will take an additional parameter 'env' to
     access their closure.  It will be shifted such that in the body
     of a function the env parameter points to its code
     pointer. i.e. in fun_b it will be shifted by 3 words.

     Hence accessing to v1 in the body of fun_a is accessing to the
     6th field of 'env' and in the body of fun_b it is the 1st
     field.

     If the closure can be compiled to a constant, the env parameter
     is not always passed to the function (for direct calls). Inside
     the body of the function, we acces a constant globaly defined:
     there are label camlModule__id created to access the functions.
     fun_a can be accessed by 'camlModule__id' and fun_b by
     'camlModule__id_3' (3 is the offset of fun_b in the closure).
     This can happen even for (toplevel) mutually-recursive functions.

     Inside a constant closure, there will be no access to the
     closure for the free variables, but if the function is inlined,
     some variables can be retrieved from the closure outside of its
     body, so constant closure still contains their free
     variables. *)
(*
    | Fset_of_closures({ cl_fun = functs; cl_free_var = fv }, _) ->
(in original code, cl_fun = funct, then passed to "functs" argument)
*)
  let funct = Variable.Map.bindings functs.funs in
  (* The environment used for non constant closures. *)
  let env_var = Ident.create "env" in
  (* The label used for constant closures. *)
  let closure_lbl =
    match expected_symbol with
    | None ->
      assert (not closure_is_constant);
      Compilenv.new_const_symbol ()
    | Some sym ->
      (* CR mshinwell for pchambart: please clarify comment *)
      (* should delay conversion *)
      Symbol.string_of_linkage_name sym.sym_label
  in
  (* Build the clambda expression representing the set of closures. *)
  let clambda_for_set_of_closures =
    if is_constant then
      match constant_list (List.map snd fv_ulam) with
      | Some fv_const ->
        let cst : Clambda.ustructured_constant =
          Uconst_closure (ufunct, closure_lbl, fv_const)
        in
        let closure_lbl =
          Compilenv.add_structured_constant closure_lbl cst ~shared:true
        in
        Uconst (Uconst_ref (closure_lbl, Some cst))
      | None -> assert false
    else
      Uclosure (ufunct, List.map snd fv_ulam)
  in
  ???, clambda_for_set_of_closures, Value_id closure_ex_id

and clambda_for_direct_application ufunct args direct_func dbg env =
  let closed = is_function_constant direct_func in
  let label = Compilenv.function_label direct_func in
  let uargs =
    let uargs = conv_list env args in
    if closed then uargs else uargs @ [ufunct]
  in
  let apply : Clambda.ulambda = Udirect_apply (label, uargs, dbg) in
  let no_effect (ulambda : Clambda.ulambda) =
    (* This is usually sufficient to detect application expressions where
       the left-hand side has a side effect. *)
    let rec no_effect (ulambda : Clambda.ulambda) =
      match ulambda with
      | Uvar _ | Uconst _ | Uprim (Pgetglobalfield _, _, _)
      | Uprim (Pgetglobal _, _, _) -> true
      | Uprim (Pfield _, [arg], _) -> no_effect arg
      | _ -> false
    in
    match ulambda with
    (* if the function is closed, then it is a Uconst otherwise,
       we do not call this function *)
    | Uclosure _ -> assert false
    | e -> no_effect e
  in
  (* if the function is closed, the closure is not in the parameters,
     so we must ensure that it is executed if it does some side effects *)
  if closed && not (no_effect ufunct) then Usequence (ufunct, apply)
  else apply

and clambda_and_approx_for_application env = function
  | Fapply ({
      (* The simple case: direct call of a closure from a known set of
         closures. *)
      ap_function =
        Fclosure ({
          fu_closure =
            Fset_of_closures ({ cl_fun = ffuns; cl_free_var = fv;
                cl_specialised_arg }, _);
          fu_fun = off;
          fu_relative_to = (None as rel) }, _);
      ap_arg = args;
      ap_kind = Direct direc;
      ap_dbg = dbg }, _) ->

      assert (Closure_id.equal off direc);
      let uargs, args_approx = conv_list_approx env args in
      let func =
        try find_declaration off ffuns
        with Not_found -> assert false in
      assert(List.length uargs = List.length func.params);
      let args_approx =
        List.fold_right2 Variable.Map.add func.params args_approx Variable.Map.empty
        |> Variable.Map.filter (fun var _ -> Variable.Map.mem var cl_specialised_arg) in
      let uffuns, fun_approx = conv_closure env ffuns args_approx cl_specialised_arg fv in
      let approx = match get_descr fun_approx with
        | Some(Value_closure { fun_id; closure = { results } }) ->
            Closure_id.Map.find fun_id results
        | _ -> Value_unknown
      in

      Fapply({ap_function =
                Fclosure ({fu_closure = uffuns;
                            fu_fun = off;
                            fu_relative_to = rel}, ());
              ap_arg = uargs;
              ap_kind = Direct direc;
              ap_dbg = dbg}, ()),
      approx


  | Fapply ({ ap_function = funct; ap_arg = args; ap_kind = direct;
        ap_dbg = dbg}, _) ->

      let ufunct, fun_approx = conv_approx env funct in
      let direct = match direct with
        | Direct _ -> direct
        | Indirect -> match get_descr fun_approx with
          (* We mark some calls as direct when it is unknown:
             for instance if simplify wasn't run before. *)
          | Some (Value_closure { fun_id }) when
              (function_arity fun_id) = List.length args ->
              Direct fun_id
          | _ -> Indirect
      in
      let approx = match get_descr fun_approx with
        | Some(Value_closure { fun_id; closure = { results } }) ->
            Closure_id.Map.find fun_id results
        | _ -> Value_unknown
      in
      Fapply({ap_function = ufunct; ap_arg = conv_list env args;
              ap_kind = direct;
              ap_dbg = dbg}, ()),
      approx

  (* two cases from pass 2: *)
  | Fapply ({ ap_function = funct; ap_arg = args;
        ap_kind = Direct direct_func; ap_dbg = dbg }, _) ->
    conv_direct_apply (conv env funct) args direct_func dbg env
  | Fapply ({ ap_function = funct; ap_arg = args;
        ap_kind = Indirect; ap_dbg = dbg }, _) ->
    (* the closure parameter of the function is added by cmmgen, but
       it already appears in the list of parameters of the clambda
       function for generic calls. Notice that for direct calls it is
       added here. *)
    Ugeneric_apply (conv env funct, conv_list env args, dbg)

and conv_list env l = List.map (conv env) l

and constant_list l =
  let rec aux acc (ulambda : Clambda.ulambda list) =
    match ulambda with
    | [] -> Some (List.rev acc)
    | (Uconst v)::q -> aux (v :: acc) q
    | _ -> None
  in
  aux [] l

let constants =
  Symbol.Map.mapi
    (fun sym lam ->
       let ulam = conv empty_env ~expected_symbol:sym lam in
       structured_constant_for_symbol sym ulam)
    P.constants

let clambda_expr = conv empty_env P.expr

let convert (type a)
    ((expr : a Flambda.t),
     (constants : a Flambda.t Symbol.Map.t),
     (exported : Flambdaexport.exported)) =
  let closures =
    let closures = ref Closure_id.Map.empty in
    Flambdautils.list_closures expr ~closures;
    Symbol.Map.iter (fun _ expr -> Flambdautils.list_closures expr ~closures)
        constants;
    !closures
  in
  let fun_offset_table, fv_offset_table =
    Flambda_lay_out_closure.assign_offsets ~expr ~constants
  in
  let add_ext_offset_fun, add_ext_offset_fv =
    let extern_fun_offset_table, extern_fv_offset_table =
      (Compilenv.approx_env ()).offset_fun,
        (Compilenv.approx_env ()).offset_fv
    in
    Flambda_lay_out_closure.reexported_offsets ~extern_fun_offset_table
      ~extern_fv_offset_table ~expr
  in
  let module C = Generate_clambda (struct
    type t = a
    let expr = expr
    let constants = constants
    let constant_closures = exported.constant_closures
    let fun_offset_table = fun_offset_table
    let fv_offset_table = fv_offset_table
    let closures = closures
    let functions = exported.functions
  end) in
  let export : Flambdaexport.exported =
    { exported with
      offset_fun = add_ext_offset_fun fun_offset_table;
      offset_fv = add_ext_offset_fv fv_offset_table;
    }
  in
  Compilenv.set_export_info export;
  Symbol.Map.iter (fun sym cst ->
       let lbl = Symbol.string_of_linkage_name sym.sym_label in
       Compilenv.add_exported_constant lbl)
    C.constants;
  C.clambda_expr
