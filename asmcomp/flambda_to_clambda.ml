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

module Symbol_alias_db : sig
  type t

  val create : unit -> t
  val canonical_symbol : t -> Symbol.t -> Symbol.t
  val set_symbol_alias : t -> Symbol.t -> Symbol.t -> unit
end = struct
  type t = Symbol.t Symbol.Table.t

  let create () = Symbol.Table.create ()

  (* XXX there used to be something doing this within value descriptions,
     where does this happen now? *)
  (* CR mshinwell: check this function has the necessary property and
     document it *)
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
end

type 'a t = {
  current_unit : 'a Flambdaexport.exported_mutable;
  imported_units : Flambdaexport.exported;
  symbol_alias_db : Symbol_alias_db.t;
  unit_approx : Flambdaexport.approx;
}

type ('a, 'b) which_unit = Current_unit of 'a | Imported_unit of 'b

let get_fun_offset t (id : Closure_id.t) =
  let offset_fun =
    if Closure_id.in_current_compilation_unit id then
      t.current_unit.offset_fun
    else
      t.imported_units.offset_fun
  in
  try Closure_id.Map.find id offset_fun
  with Not_found ->
    Misc.fatal_errorf "Offset for function not found: %a"
      Closure_id.print id

let get_fv_offset t (id : Var_within_closure.t) =
  let offset_fv =
    if Var_within_closure.in_current_compilation_unit id then
      t.current_unit.offset_fv
    else
      t.imported_units.offset_fv
  in
  try Var_within_closure.Map.find id offset_fv
  with Not_found ->
    Misc.fatal_errorf "Offset for fv not found: %a"
      Var_within_closure.print id

let get_local_fv_offset_from_var_exn t (var : Variable.t) =
  let id = Var_within_closure.wrap var in
  try Var_within_closure.Map.find id t.current_unit.offset_fv
  with Not_found ->
    Misc.fatal_errorf "Offset for local fv not found: %a"
      Var_within_closure.print id

let find_closure_declaration t (id : Closure_id.t) =
  match Closure_id.Map.find id t.current_unit.closures with
  | decls -> Current_unit decls
  | exception Not_found ->
    match Closure_id.Map.find id t.imported_units.closures with
    | decls -> Imported_unit decls
    | exception Not_found ->
      Misc.fatal_errorf "Could not find declaration for closure: %a"
        Closure_id.print id

let find_set_of_closures_declaration t (id : Closure_id.t) =
  match Set_of_closures_id.Map.find id t.current_unit.sets_of_closures with
  | decls -> Current_unit decls
  | exception Not_found ->
    match Set_of_closures_id.Map.find id t.imported_units.sets_of_closures with
    | decls -> Imported_unit decls
    | exception Not_found ->
      Misc.fatal_errorf "Could not find declaration for set of closures: %a"
        Set_of_closures_id.print id

let is_set_of_closures_constant t (id : Set_of_closures_id.t) =
  match find_set_of_closures_declaration t id with
  | Current_unit decls ->
    Set_of_closures_id.Set.mem t.current_unit.constant_closures decls.ident
  | Imported_unit decls ->
    Set_of_closures_id.Set.mem t.imported_units.constant_closures decls.ident

let is_set_of_closures_local_and_constant t (id : Set_of_closures_id.t) =
  match find_set_of_closures_declaration t id with
  | Current_unit decls ->
    Set_of_closures_id.Set.mem t.current_unit.constant_closures decls.ident
  | Imported_unit decls -> false

let function_arity t (id : Closure_id.t) =
  Flambdautils.function_arity (find_closure_declaration t id)

(* [find_approx_descr t approx] obtains the approximation description
   referenced by [approx] by looking in the maps of exported approximations
   for the current and the imported compilation units.  This function may
   cause a .cmx file to be read. *)
let find_approx_descr t approx =
  match approx with
  | Value_unknown -> None
  | Value_id export_id ->
    begin match Export_id.Map.find export_id t.current_unit.values with
    | descr -> Some descr
    | exception Not_found ->
      (* XXX there's something a bit weird going on here; this doesn't try
         to load any .cmx files.  Needs a comment (or fixing) *)
      begin match
        Flambdaexport.find_description export_id t.imported_units.ex_values
      with
      | descr -> Some descr
      | exception Not_found -> None
      end
    end
  | Value_symbol sym ->
    (* Try to resolve the symbol to a value being exported by the current
       unit.  If that fails, attempt to load the .cmx file for the unit known
       to hold the symbol (if it has not been loaded already), and then try
       the lookup there. *)
    let descr =
      match Symbol.Map.find sym t.current_unit.symbol_id with
      | export_id ->
        begin match Export_id.Map.find export_id t.current_unit.values with
        | descr -> Some descr
        | exception Not_found -> None
        end
      | exception Not_found -> None
    in
    match descr with
    | Some descr -> descr
    | None ->
      if Compilenv.is_predefined_exception sym then None
      else begin
        (* [approx_for_global] is the .cmx-loading step. *)
        let exported = Compilenv.approx_for_global sym.sym_unit in
        match Symbol.Map.find sym t.imported_units.symbol_id with
        | Some export_id ->
          begin match Flambdaexport.descr_for_export_id export_id exported with
          | Some descr -> descr
          | None -> None
          end
        | None -> None
      end

(* Given a description of a value to be exported from the current
   compilation unit, assign it a new export ID, and record the mapping from
   ID to description. *)
let add_value_description' exported_mutable (descr : Flambdaexport.descr) =
  let id = Export_id.create (Compilenv.current_unit ()) in
  Export_id.Tbl.add id descr exported_mutable.values;
  id
let add_value_description t descr = add_value_description' t.current_unit descr

(*
The old add_constant from flambdasym
  let add_symbol sym id =
    infos.ex_symbol_id := SymbolMap.add sym id !(infos.ex_symbol_id)

  let add_constant lam ex_id =
    let sym = Compilenv.new_const_symbol' () in
    SymbolTbl.add infos.constants sym lam;
    add_symbol sym ex_id;
    sym

It is only used for
- the Fconst cases
- Pmakeblock
- constant closures.

It receives the export ID to be associated with the value.
It always generates the new symbol itself.

The rewritten flambda term is always Fsymbol ...
but the approximation might be something more complicated than Value_symbol.

It isn't used for binding constructs, so there's no update of the
environment.

--

The old add_cm from flambdasym
This only affects the environment.
The env info is read in two places
- Fvar
- when erasing specialized argument information.
add_cm is called in these places:
- Flet
- Fletrec
- translating a closure
i.e. all the binding places.
*)

(* Remember the defining expression of a structured constant such that it
   can be recalled later.  Also assign a fresh symbol to the constant and
   associate the given export identifier.  The symbol and the corresponding
   Clambda structured constant label (to be used in [Uconst_ref]) are
   returned. *)
let add_structured_constant t lam ulam export_id ?not_shared =
  let sym = Compilenv.new_const_symbol' () in
  let shared =
    match not_shared with
    | None -> true
    | Some () -> false
  in
  assert (not (SymbolTbl.mem t.current_unit.constants sym));
  SymbolTbl.add t.current_unit.constants sym lam;
  assert (not (SymbolTbl.mem t.current_unit.ex_symbol_id sym));
  SymbolTbl.add t.current_unit.ex_symbol_id sym export_id;
  let label = Compilenv.structured_constant_label sym ~shared ulam in
  sym, label

(* Record that the global at position [index] (within the global block for
   the current compilation unit) has approximation [approx]. *)
let add_approx_for_global t ~index ~approx =
  Ident.Tbl.add t.current_unit.globals index approx

(* Find the approximation for a global in the current compilation unit. *)
let approx_for_global t ~index =
  try Ident.Tbl.find t.current_unit.globals index
  with Not_found ->
    Misc.fatal_errorf "No global at index %d in the current compilation unit" i

(* "fclambda" means a pair of an Flambda term, rewritten to have references
   to symbols as required for exporting, together with a Clambda term.  The
   associated approximation of the Clambda term is also often present.
   Locally-scoped environments of type [Env.t] are present throughout the
   translation.  These give information about variables currently in scope.
*)

module Env : sig
  type t

  val create : unit -> t

  (* [add_substitution] assigns a fresh [Ident.t] to the given variable and
     records the mapping within the environment.

     The mapping from variables to identifiers may be retrieved using
     [identifier_assignment].

     Within the environment will also be recorded a mapping from the variable
     to the given Clambda term, export approximation and (optionally) Flambda
     term. *)
  val add_substitution
     : t
    -> Variable.t
    -> _ Flambda.t option
    -> Clambda.ulambda
    -> Flambdaexport.approx
    -> Ident.t * t

  (* [find_substitution_for_variable] retrieves a substitution previously
     added with [add_substitution_for_variable].  If [None] was provided as the
     Flambda argument to [add_unique_id_and_substitution], [default_flambda]
     is returned as the Flambda term. *)
  val find_substitution
     : t
    -> Variable.t
    -> default_flambda:unit Flambda.t
    -> (unit Flambda.t * Clambda.ulambda * Flambdaexport.approx) option
end = struct

(*
(* Sometimes we need to move an Flambda expression (bound to some variable) to
   be the defining expression of a symbol, with the original expression
   replaced by a reference to the symbol.  The typical scenario is for
   expressions defining certain kinds of structured constants.  What has to be
   done is to update the environment with a substitution to put in place the
   reference(s) to the symbol; and to record separately the defining
   expression. *)
let make_defining_expr_of_symbol ?symbol t env defining_expr =
  let symbol =
    match symbol with
    | None -> Compilenv.new_const_symbol' ()
    | Some symbol -> symbol
  in
  let reference_expr, ureference_expr, approx =
    fclambda_and_approx_for_symbol t env symbol
  in
  let id, env =
    Env.add_substitution env reference_expr ureference_expr approx
  in
  (* XXX (still need to reconcile with [add_constant]) *)
  symbol, id, env
*)

end

let fclambda_and_approx_for_symbol t env sym =
  (* Ensure all symbols are replaced with their canonical representative. *)
  let sym = Symbol_alias_db.canonical_symbol t.symbol_alias_db sym in
  let linkage_name = Symbol.string_of_linkage_name sym.sym_label in
  let label = Compilenv.canonical_symbol linkage_name in
  (* CR pchambart for pchambart: Should delay the conversion a bit more
     mshinwell: I turned this comment into a CR.  What does it mean? *)
  Fsymbol sym, Uconst (Uconst_ref (label, None)), Value_symbol sym

let fclambda_and_approx_for_constant t env ~(cst : Flambda.const) ~annot =
  (* There are two kinds of constants: those that are translated to
     values of type [Clambda.ustructured_constant], and those that are not.
     The ones in the former category have to be assigned symbols, since we
     need symbols in order to compute the "label" for the [Clambda.Uconst_ref]
     constructor associated with a [Clambda.ustructured_constant]. *)
  let structured ?not_shared (ucst : Clambda.ustructured_constant) descr =
    let export_id = add_value_description t descr in
    let symbol, label =
      add_structured_constant t (Fconst (cst, annot)) ucst export_id
        ?not_shared
    in
    Fsymbol (symbol, ()), Uconst_ref (label, Some ucst), Value_id export_id
  in
  let not_structured ucst descr =
    Fconst (cst, ()), ucst, Value_id (add_value_description t descr)
  in
  match cst with
  | Fconst_base (Const_float f) ->
    (* CR mshinwell for pchambart: Is there any chance [float_of_string]
       might fail due to user error? *)
    let f = float_of_string f in
    structured (Uconst_float f) (Value_float f)
  | Fconst_base (Const_int32 i) ->
    structured (Uconst_int32 i) (Value_boxed_int (Int32, i))
  | Fconst_base (Const_int64 i) ->
    structured (Uconst_int64 i) (Value_boxed_int (Int64, i))
  | Fconst_base (Const_nativeint i) ->
    structured (Uconst_nativeint i) (Value_boxed_int (Nativeint, i))
  | Fconst_base (Const_string s) ->
    structured (Uconst_string s) Value_string ~not_shared:()
  | Fconst_float_array fs ->
    let fs = List.map float_of_string fs in
    (* CR mshinwell for pchambart: Why is this [Value_string]? *)
    structured (Uconst_float_array fs) Value_string
  | Fconst_immstring s ->
    structured (Uconst_string s) Value_string
  | Fconst_base (Const_int i) ->
    not_structured (Uconst_int i) (Value_int i)
  | Fconst_base (Const_char c) ->
    let c = Char.code c in
    not_structured (Uconst_int c) (Value_int c)
  | Fconst_pointer ptr ->
    not_structured (Uconst_ptr ptr) (Value_constptr ptr)

let fclambda_and_approx_for_var t env ~var =
  match Env.find_substitution env var ~default_flambda:(Fvar (var, ()) with
  | Some (lam, ulam, approx) -> lam, ulam, approx
  | None ->
    Misc.fatal_errorf "Environment does not know about variable %a: %a"
      Variable.print var
      Env.print env

let rec fclambda_and_approx_for_primitive t env ~(primitive : Lambda.primitive)
      ~args ~dbg : unit Flambda.t * Clambda.ulambda * Flambdaexport.descr =
  match primitive, args, dbg with
  | Pgetglobalfield (id, index), l, dbg ->
    (* [Pgetglobalfield] may correspond to an access to a global in either
       the current compilation unit or an imported one. *)
    assert (l = []);
    let lam : _ Flambda.t =
      Fprim (Pfield index, [Fprim (Pgetglobal id, l, dbg, v)], dbg, v)
    in
    if id <> Compilenv.current_unit_id () then
      fclambda_and_approx_for_expr t env lam
    else
      let approx = get_approx_for_global ~index in
      begin match approx with
      | Value_symbol sym ->
        fclambda_for_expr t env (Fsymbol (sym, ())), approx
      | Value_unknown _ | Value_id _ -> fclambda_for_expr t env lam, approx
      end
  | Psetglobalfield index, [arg], dbg ->
    (* [Psetglobalfield] always corresponds to the current compilation unit,
       and cannot be inlined across modules. *)
    let arg, uarg, approx = fclambda_and_approx_for_expr t env arg in
    (* CR mshinwell for pchambart: Can any global fields be changed again
       once they have been set for the first time?  I assume the answer is
       no, otherwise this next line looks wrong (couldn't the approximation
       change)? *)
    add_approx_for_global t ~index ~approx;
    Fprim (Psetglobalfield index, [arg], dbg, ()),
      Uprim (Psetfield (index, false),
          [Uprim (Pgetglobal (Ident.create_persistent
              (Compilenv.make_symbol None)), [], dbg);
           uarg],
          dbg),
      Value_unknown
  | Pgetglobal id, l, _ ->
    (* Accesses to globals are transformed into symbol accesses. *)
    (* CR mshinwell for pchambart: Does this case only arise from the
       previous [Pgetglobalfield] case?  If so, we should probably make
       this case fail, and inline this code above. *)
    assert (l = []);
    let sym = Compilenv.symbol_for_global' id in
    fclambda_and_approx_for_expr t env (Fsymbol (sym, ()))
  | (Pmakeblock (tag, Asttypes.Immutable)) as p, args, dbg ->
    (* If we know the values of all of the fields of the new block, then
       emit it as data, with a symbol to identify it.  The original
       [Pmakeblock] turns into a reference to the symbol. *)
    let args, uargs, approxs = fclambda_and_approx_for_expr_list t env args in
    let ex = add_value_description (Value_block (tag, Array.of_list approxs)) in
    begin match Clambda.all_constants uargs with
    | None -> Uprim (p, args, dbg), Value_id ex
    | Some arg_values ->
      (* XXX needs fixing after changes above *)
      let sym = add_constant t (Uprim (p, args, dbg)) ex in
      let cst : Clambda.ustructured_constant =
        Uconst_block (tag, arg_values)
      in
      let lbl = Compilenv.structured_constant_label sym ~shared:true cst in
      Uconst (Uconst_ref (lbl, Some cst)), Value_symbol sym
    end
  | Pfield index, [arg], dbg ->
    (* For arbitrary field access, we try to identify the approximation
       of the field itself, if we know enough about the block. *)
    let block, ublock, block_approx = fclambda_and_approx_for_expr t env arg in
    let approx =
      match find_approx_descr t block_approx with
      | Some (Value_block (_, fields)) ->
        if index >= 0 && index < Array.length fields then fields.(index)
        else Value_unknown
      | _ -> Value_unknown
    in
    Fprim (Pfield index, [block], dbg, ()),
      Uprim (Pfield index, [block], dbg, ()), approx
  | primitive, args, dbg ->
    let args, uargs = fclambda_for_expr_list t env args in
    Fprim (primitive, args, dbg, ()), Uprim (primitive, uargs, dbg),
      Value_unknown

and fclambda_and_approx_for_let t ~env ~is_assigned ~var ~rhs ~body
      : unit Flambda.t * Clambda.ulambda * Flambdaexport.approx =
  let rhs, urhs, approx = fclambda_and_approx_for_expr t env rhs in
  (* XXX this next line needs fixing.  where is the function?
     Also, we should add a comment to explain why this let-simplification
     cannot be done in an earlier pass.  Or maybe we should do it earlier.
  *)
  let is_constant = can_be_compiled_to_clambda_constant t var in
  let approx =
    let approx_never_changes_during_execution_of_body =
      is_constant
        || match is_assigned with
           | Not_assigned -> true
           | Assigned -> false
    in
    if approx_never_changes_during_execution_of_body then approx
    else Value_unknown
  in
  let id, env = Env.add_substitution env var (Some rhs) urhs approx in
  if not is_constant then begin
    (* We still require a [let]-binding in the Clambda expression. *)
    let body, ubody, body_approx = fclambda_and_approx_for_expr t env body in
    Flet (is_assigned, var, rhs, body, ()), Ulet (id, urhs, ubody), body_approx
  end else begin
    (* The [let] is not needed; the value of the constant (which might be
       a symbol reference to obtain it) will be directly substituted
       for [var] within [body]. *)
    fclambda_and_approx_for_expr t env body
  end

and fclambda_and_approx_for_let_rec t env defs body =
  let consts, not_consts =
    List.partition (fun (id, _) -> is_constant t.exported id) defs
  in
  (* Add substitutions to the environment for those variables bound by the
     [let rec] whose values we know are constant.  Also note down which of
     these variables, if any, have bindings that we know to be constant but
     where we do not know the actual value (and thus cannot substitute). *)
  let env, constant_but_cannot_be_substituted =
    List.fold_left (fun (env, constant_but_cannot_be_substituted) (var, def) ->
        let def, udef, def_approx = fclambda_and_approx_for_expr t env def in
        (* CR mshinwell: improve comment *)
        (* Where possible, substitute a constant (which might be a reference
           via a symbol) for the variable. *)
        match def with
        | Fconst (( Fconst_pointer _
                  | Fconst_base
                      ( Const_int _ | Const_char _
                      | Const_float _ | Const_int32 _
                      | Const_int64 _ | Const_nativeint _)), _) ->
          (* When the value is an integer constant, we cannot attribute a
             label to it: hence we must substitute it directly.
             For other numerical constants, a label could be attributed, but
             unboxing doesn't handle it well. *)
          let _id, env =
            Env.add_substitution env var (Some def) udef def_approx
          in
          env, constant_but_cannot_be_substituted
        | Fvar (rhs_var, _) ->
          (* If the expression on the right-hand side is manifestly a
             variable, whose value is known to be constant, we again
             arrange for a substitution. *)
          (* CR mshinwell for pchambart: please document this assertion *)
          assert (List.for_all (fun (const_var, _, _) ->
              not (Variable.equal rhs_var const_var)) consts);
          (* CR mshinwell for pchambart: this comment needs clarifying *)
          (* For variables: the variable could have been substituted to
             a constant: avoid it by substituting it directly *)
          let _id, env =
            Env.add_substitution env var (Some def) udef def_approx
          in
          env, constant_but_cannot_be_substituted
        | _ ->
          (* If the expression on the right-hand side is something more
             complicated (but which we still know to be constant), assign a
             new symbol, which may be referenced from any of the right-hand
             sides. *)
          let id, sym, env =
            Env.add_substitution_via_symbol env var def udef def_approx
          in
          env, (var, id, sym, def)::acc)
      (env, []) consts
  in
  (* In the augmented environment, verify that all bindings deemed constant
     that we have not been able to directly substitute are assigned symbols. *)
  List.iter (fun (id, sym, def) ->
      match def with
      | Fsymbol (sym', ()) ->
      (* CR mshinwell: we should have an example as to how a chain of
         symbol aliases can happen *)
        begin match symbol_id sym' with
        | None -> ()
        | Some eid -> add_symbol sym eid
        end;
        set_symbol_alias sym sym'
      | _ ->
        Misc.fatal_errorf "Recursive constant value without symbol %a %a"
          Variable.print id
          Printflambda.flambda def)
    consts;
  (* Translate bindings whose right-hand sides are not known to be
     constant.  This again happens in the augmented environment from
     above, and indeed augments the environment further. *)
  let not_consts, unot_consts, env =
    List.fold_right (fun (var, def) (not_consts, unot_consts, env') ->
        (* N.B. [def] is evaluated w.r.t. [env] not [env']. *)
        let def, udef, approx = fclambda_and_approx_for_expr t env def in
        let id, env' =
          Env.add_substitution env var (Some def) udef def_approx
        in
        let env' = Env.add_approx env' var approx in
        (var, def)::not_consts, (id, udef)::unot_consts, env')
      not_consts ([], [], env)
  in
  (* [env] now contains all bindings, so we can translate the body of the
     [let rec] expression. *)
  let body, ubody, approx = fclambda_and_approx_for_expr t env body in
  let flambda, clambda =
    (* The "let rec" may be completely eliminated in the case where all
       of the bound variables are constant.  Otherwise, we still need the
       "let rec", but we can remove any constant bindings from it
       (since their corresponding values will have been substituted for
       the bound variables). *)
    assert (List.length not_consts = List.length unot_consts);
    match not_consts with
    | [] -> body, ubody
    | _ -> Fletrec (not_consts, body, ()), Uletrec (unot_consts, ubody)
  in
  flambda, clambda, approx

and fclambda_and_approx_for_switch t env ~arg ~sw =
  let arg, uarg = fclambda_for_expr t env arg in
  let default, udefault =
    match sw.fs_failaction with
    | None -> None, None
    | Some failaction ->
      let flambda, clambda = fclambda_for_expr t failaction in
      Some flambda, Some clambda
  in
  let fclambda_for_cases ~cases ~num_keys =
    (* Produce rewritten Flambda and Clambda for a subset of the cases within
       a switch. *)
    let module Switch_storer =
      Switch.Store (struct
        type t = _ Flambda.t
        type key = Flambdautils.sharing_key
        let make_key = Flambdautils.make_key
      end)
    in
    let num_keys =
      if Ext_types.Int.Set.cardinal num_keys = 0
      then 0
      else Ext_types.Int.Set.max_elt num_keys + 1 in
    let index = Array.make num_keys 0 in
    let store = Switch_storer.mk_store () in
    (* First the default case. *)
    begin match default with
    | Some default when List.length cases < num_keys ->
      ignore ((store.act_store default) : int)
    | _ -> ()
    end;
    (* Then all other cases. *)
    List.iter (fun (key, lam) -> index.(key) <- store.act_store lam) cases;
    (* Compile the actions. *)
    let actions_and_uactions =
      List.map (fun action ->
          (* Avoid translating [default] more than once. *)
          if action == default then default, udefault
          else fclambda_for_expr t env action)
        (Array.to_list (store.act_get ()))
    in
    match actions_and_uactions with
    | [] -> [| |], [| |], [| |]  (* May happen when [default] is [None]. *)
    | _ ->
      let actions, uactions = List.split actions, uactions in
      index, Array.of_list actions, Array.of_list uactions
  in
  let fclambda_for_all_cases () : unit Flambda.t * Clambda.ulambda =
    let const_index, const_actions, const_uactions =
      (* Cases matching on immediate values. *)
      fclambda_for_cases ~cases:sw.fs_consts ~num_keys:sw.fs_numconsts
    in
    let block_index, block_actions, block_uactions =
      (* Cases matching on boxed values ("blocks"). *)
      fclambda_for_cases ~cases:sw.fs_blocks ~num_keys:sw.fs_numblocks
    in
    let flambda =
      Fswitch (arg, {
        sw with
        fs_consts = const_actions;
        fs_blocks = block_actions;
        fs_failaction = default;
      })
    in
    let clambda =
      Uswitch (uarg, {
        us_index_consts = const_index;
        us_actions_consts = const_uactions;
        us_index_blocks = block_index;
        us_actions_blocks = block_uactions;
      })
    in
    flambda, clambda
  in
  let flambda, clambda =
    (* CR mshinwell for pchambart: "effectively copyable" needs
       clarification *)
    (* Check that failaction is effectively copyable: i.e. it can't declare
       symbols.  If this is not the case, share it through a
       staticraise/staticcatch. *)
    let rec simple_expr (expr : _ Flambda.t) =
      match expr with
      | Fconst (Fconst_base (Const_string _), _) -> false
      | Fvar _ | Fsymbol _ | Fconst _ -> true
      | Fstaticraise (_, args, _) -> List.for_all simple_expr args
      | _ -> false
    in
    match sw.fs_failaction with
    | None -> fclambda_for_all_cases ()
    | Some (Fstaticraise (_, args, _))
        when List.for_all simple_expr args -> fclambda_for_all_cases ()
    | Some failaction ->
      (* Replace the [failaction] with a static raise, and re-translate
         the whole expression, wrapped in a [Fstaticcatch]. *)
      let exn = Static_exception.create () in
      let sw = { sw with fs_failaction = Some (Fstaticraise (exn, [], d)) } in
      fclambda_for_expr t env
        (Fstaticcatch (exn, [], Fswitch (arg, sw, d), failaction, d))
  in
  flambda, clambda, Value_unknown

(* Handle an expression that retrieves the value of a variable bound by
   some closure. *)
and fclambda_and_approx_for_variable_in_closure t env
      { vc_closure = closure; vc_var = var; vc_fun = fun_id } =
  let closure, uclosure, closure_approx =
    fclambda_and_approx_for_expr env closure
  in
  let approx =
    match find_approx_descr t closure_approx with
    | Some (Value_closure { closure = { bound_var = bound_vars; } }) ->
      begin match Var_within_closure.Map.find var bound_vars with
      | approx -> approx
      | exception Not_found ->
        Misc.fatal_errorf "Fvariable_in_closure expression references \
            variable not bound by the given closure: %a@.%a@."
          Printflambda.flambda expr
          Printflambda.flambda closure
      end
    | None
        when not (Closure_id.in_compilation_unit
          (Compilenv.current_unit ()) id) ->
      (* If some .cmx files are missing, the approximation may be absent,
         in which case we can safely use [Value_unknown].  However it is a
         bug for this situation to occur in the case of a value defined in
         the current compilation unit.  The "None" case below handles this. *)
      Value_unknown
    | Some ((Value_block _ | Value_int _ | Value_constptr _ | Value_float _
        | Flambdaexport.Value_boxed_int _ | Value_string _
        | Value_set_of_closures _) as descr) ->
      Misc.fatal_errorf "Fvariable_in_closure expression references \
          variable with a non-Value_closure approximation: %a@.%a@.%a@."
        Printflambda.flambda expr
        Printflambda.flambda closure
        Flambdaexport.print_descr descr
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

(* Handle an expression that retrieves an actual closure, rather than the
   value of some variable bound by a closure. *)
and fclambda_and_approx_for_closure_reference t env
      { fu_closure = lam; fu_fun = id; fu_relative_to = rel } =
  let ulam, approx =
    if is_function_local_and_constant t.exported id then
      (* Only references to functions declared in the current module should
         need rewriting to a symbol.  For external functions this should
         already have been done when the external compilation unit was
         compiled. *)
      let sym = Compilenv.closure_symbol id in
      fclambda_and_approx_for_expr env (Fsymbol (sym, ()))
    else
      let lam, ulam, fun_approx =
        fclambda_and_approx_for_expr env lam
      in
      let approx =
        match find_approx_descr t fun_approx with
        | Some (Value_set_of_closures closure)
        | Some (Value_closure { closure }) ->
          Value_id (add_value_description (Value_closure { fun_id = id; closure }))
        | Some ((Value_block _ | Value_int _ | Value_constptr _ | Value_float _
            | Flambdaexport.Value_boxed_int _ | Value_string _
            | Value_set_of_closures _) as descr) ->
          Misc.fatal_errorf "Bad approximation for Fclosure expression: %a %a"
            Printflambda.flambda expr
            Flambdaexport.print_descr descr
        | None
            when not (Closure_id.in_compilation_unit
              (Compilenv.current_unit ()) id) ->
          (* See comment in the equivalent case in
             [fclambda_and_approx_for_variable_in_closure], above. *)
          Value_unknown
        | None ->
          Misc.fatal_errorf "Fclosure expression references \
              closure that has no approximation: %a@.%a@."
            Printflambda.flambda expr
            Printflambda.flambda closure
      in
      fclambda_for_expr env
          (Fclosure ({ fu_closure = ulam; fu_fun = id;
              fu_relative_to = rel }, ()),
        approx
  in
  let relative_offset =
    let offset = get_fun_offset t.exported id in
    match rel with
    | None -> offset
    | Some rel -> offset - get_fun_offset rel
  in
  (* CR mshinwell: clarify comment *)
  (* Compilation of [let rec] in [Cmmgen] assumes that a closure is not
     offseted ([Cmmgen.expr_size]). *)
  if relative_offset = 0 then ulam
  else Uoffset (ulam, relative_offset)

(* Handle a single function declaration (contained within a set of
   closures).  [env] already contains the relevant information about the
   following:
   - the free variables of the set of closures;
   - the function identifiers bound by the set of closures. *)
and fclambda_and_approx_for_function_declaration t env fun_id
      ~(fundecl : _ Flambda.function_declaration)
      ~(fv : (_ Flambda.t * Clambda.ulambda) Variable.Map.t)
      ~env_var ~descr_set_of_closures =
  let closure_id = Closure_id.wrap fun_id in
  (* Inside the body of the function, we cannot access variables declared
     outside, so forget any substitutions we know about. *)
  let env = Env.forget_substitutions env in
  (* Now we populate the environment with information relating to various
     kinds of variable, as per the comments below. *)
  let env =
    (* 1. The free variables of the function.  The corresponding Clambda
       terms (which have already been produced, together with the rewritten
       Flambda) describe how to extract the variables' values from the closure.
       We should never need to do this for constant closures, since none
       of their free variables should have satisfied [closure_needs_variable],
       above. *)
    assert ((not closure_is_constant) || List.length fvs = 0);
    let fun_offset = get_fun_offset t.exported closure_id in
    List.fold_left (fun env (fv, (lam, ulam)) ->
        match get_local_fv_offset_from_var_exn t.exported fv with
        | var_offset ->
          (* CR mshinwell for pchambart: [pos] can be negative, right? *)
          let pos = var_offset - fun_offset in
          (* CR mshinwell: Debuginfo.none is almost certainly wrong *)
          let ulam : Clambda.ulambda =
            Uprim (Pfield pos, [Uvar env_var], Debuginfo.none)
          in
          Env.add_substitution_for_variable env fv (Some lam) ulam
        | exception Not_found ->
          Misc.fatal_errorf "Free variable has no offset: %a %a"
            Printflambda.function_declarations fundecls
            Variable.print fv)
      env fvs
  in
  let env =
    (* 2. Function identifiers bound by the set-of-closures declaration. *)
    Variable.Map.fold (fun env closure_var ->
        let closure_id = Closure_id.wrap closure_var in
        let descr : Flambdaexport.descr =
          Value_closure {
            closure_id;
            set_of_closures = descr_set_of_closures;
          }
        in
        let export_id = add_value_description descr in
        let closure_symbol = Compilenv.closure_symbol closure_id in
        if closure_is_constant then begin
          (* Constant closures must have a symbol attributed to them. *)
          add_symbol closure_symbol export_id
        end;
        let env = Env.add_approx env closure_var (Value_id export_id) in
        (* CR mshinwell for pchambart: We need to understand if there might
           be a case where [closure_is_constant] is true and we inserted
           values into the substitution, above.  If there is no such case,
           we should consider moving this next [if] above the [fold_left].
           (Actually, there's something in the large comment above that may
           be relevant.) *)
        if closure_is_constant then
          (* If the set of closures is constant, recursive calls to a
             function bound by the closure must go via the symbol associated
             with the given function. *)
          let id, env =
            Env.add_substitution_via_known_symbol env closure_var
              closure_symbol XXX XXX XXX
          in
          XXX
        else
          (* If the set of closures is non-constant, recursive calls
             translate to accesses through a closure value. *)
          let offset = get_fun_offset t.exported closure_var in
          (* Note that the resulting offset may be negative, in the case
             where we are accessing an earlier (= with lower address)
             closure in a block holding multiple closures. *)
          let ulam : Clambda.ulambda = Uoffset (Uvar env_var, offset - pos) in
          Env.add_substitution_for_variable env closure_var None ulam)
      env funcs.funs
  in
  let env, uparams =
    (* 3. The parameters of the function. *)
    List.fold_right (fun var (env, params) ->
        let id, env = Env.add_unique_ident var env in
        env, id :: params)
      func.params (env, [])
  in
  let body, ubody, body_approx =
    fclambda_and_approx_for_expr t env fundecl.body
  in
  let flambda =
    { fundecl with
      (* Some of the free variables may have been filtered out (see
         [fclambda_and_approx_for_function_declarations], below), so we must
         update the record field here. *)
      free_variables = Variable.Map.keys fv;
      body;
    }
  in
  let uparams =
    (* If the closure is not constant, then the Clambda function needs an
       extra parameter, to receive the environment. *)
    if closure_is_constant then
      uparams
    else
      uparams @ [env_var]
  in
  let clambda : Clambda.ufunction =
    { label = Compilenv.function_label closure_id;
      arity = Flambdautils.function_arity fundecl;
      params = uparams;
      body = ubody;
      dbg = fundecl.dbg;
    }
  in
  flambda, clambda, body_approx

(* XXX update comment *)
(* [fclambda_and_approx_for_set_of_closures] assigns symbols throughout a
   given Flambda term corresponding to a set of closures (which correspond to
   a single set of simultaneous function declarations), converts the
   resulting Flambda to Clambda, and computes its approximation.

   When converting to Clambda, this also results in the substitution of
   occurrences of variables bound by the set of closures (function identifiers
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
and fclambda_and_approx_for_function_declarations t env
      (fundecls : _ Flambda.function_declarations)
      (* CR mshinwell: see comment in the application function below.  If
         this is only supposed to contain approximations for specialised
         args, we need to rename it. *)
      (args_approx : Flambdaexport.approx Variable.Map.t)
      (specialised_args : Variable.t Variable.Map.t)
      (* [fv] maps free variables of the function declaration to Flambda
         expressions computing their values. *)
      (fv : _ Flambda.t Variable.Map.t) =
  let is_constant = is_set_of_closures_local_and_constant fundecls.ident in
  (* The environment parameter used for non-constant closures. *)
  let env_var = Ident.create "env" in
  (* The label used for constant closures. *)
  let closure_lbl =
    (* XXX [expected_symbol] has been removed now
       Can we deal with this by returning the symbol we created, and then
       adding an alias? *)
    match expected_symbol with
    | None ->
      assert (not is_constant);
      Compilenv.new_const_symbol ()
    | Some sym ->
      (* CR mshinwell for pchambart: please clarify comment *)
      (* should delay conversion *)
      Symbol.string_of_linkage_name sym.sym_label
  in
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
      (Env.add_approxs env fvs_to_approxs)
      specialised_args_with_symbol_equalities
  in
  (* Eliminate specialised argument information that will become unnecessary
     or invalid. *)
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
  (* Determine the runtime layout of the set of closures. *)
  Flambda_lay_out_closure.assign_offsets ...
  (* Build the export description of the set of closures.
     We have to start with [Value_unknown] in [results], since
     [fclambda_and_approx_for_closure_declaration] needs to
     construct values pointing at the export description, yet [results] cannot
     be properly computed until we have called that function. *)
  let descr_set_of_closures : Flambdaexport.descr_set_of_closures =
    { closure_id = fundecls.ident;
      bound_var =
        let module M = Map_map (Variable) (Var_within_closure) in
        M.map used_fv_approx ~map_data:snd;
      results =
        let module M = Map_map (Variable) (Closure_id) in
        M.map functs.funs ~map_data:(fun _ -> Value_unknown);
    }
  in
  let env =
    (* XXX this was in conv_function in flambdasym originally, but I'm
       not sure why.  Should there be some filtering here? *)
    (* param_approxs must be constants: part of specialised_args *)
    Variable.Map.fold (fun id approx env -> Env.add_approx env id approx)
      param_approxs env
  in
  (* Assign symbols throughout the individual function declarations making
     up the set of closures (yielding new Flambda terms), convert them to
     Clambda, and obtain their approximations. *)
  let fclambda_for_fundecls_with_approxs =
    Variable.Map.mapi (fun fun_id fundecl ->
      (fclambda_and_approx_for_function_declaration t env ~fun_id ~fundecl
        ~descr_set_of_closures)
      fundecls.funs
  in
  let descr_set_of_closures =
    (* Update [results] now we can properly compute it. *)
    { descr_set_of_closures with
      results =
        let module M = Map_map (Variable) (Closure_id) in
        M.map fclambda_for_fundecls_with_approxs ~map_data:Misc.thd3;
    }
  in
  (* The set of closures will always be exported; assign a new export ID
     and build the approximation. *)
  let closure_ex_id = add_value_description (Value_set_of_closures value_closure') in
  (* Build the rewritten Flambda term representing the set of closures.
     If the set of closures is constant, then we will emit a constant
     definition (labelled with a symbol) and replace the set of closures
     expression by a symbol access.  Otherwise, the expression remains as
     [Fset_of_closures], and code will be emitted to dynamically construct the
     closure at runtime (this is the [Uclosure] expression in Clambda). *)
  let flambda =
    let expr : _ Flambda.t =
      let fundecls =
        { fundecls with
          funs = Variable.Map.map Misc.fst3 fclambda_for_fundecls_with_approxs;
        }
      in
      Fset_of_closures ({ cl_fun = fundecls;
          cl_free_var = used_fv;
          cl_specialised_arg = spec_arg },
        ())
    in
    if is_function_local_and_constant t.exported ufunct.ident then
      let sym = add_constant t expr closure_ex_id in
      Fsymbol (sym, ())
    else expr
  in
  (* Build the Clambda expression representing the set of closures. *)
  let clambda =
    let clambda_ufunctions =
      List.map Misc.snd3 Variable.Map.data fclambda_for_fundecls_with_approxs
    in
    if is_constant then
      match Clambda.all_constants (List.map snd fv_ulam) with
      | Some fv_const ->
        let cst : Clambda.ustructured_constant =
          Uconst_closure (clambda_ufunctions, closure_lbl, fv_const)
        in
        let closure_lbl =
          Compilenv.add_structured_constant closure_lbl cst ~shared:true
        in
        Uconst (Uconst_ref (closure_lbl, Some cst))
      | None ->
        Misc.fatal_error "Constant closure with non-constant free variable(s)"
    else
      Uclosure (clambda_ufunctions, List.map snd fv_ulam)
  in
  flambda, clambda, Value_id closure_ex_id

and clambda_for_direct_application t env ~fundecl ~args ~dbg =
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

and clambda_for_application t env ~direct ~ufundecl ~uargs ~dbg =
  match direct with
  | Direct _ -> clambda_for_direct_application ~ufundecl ~uargs ~dbg
  | Indirect ->
    (* the closure parameter of the function is added by cmmgen, but
       it already appears in the list of parameters of the clambda
       function for generic calls. Notice that for direct calls it is
       added here. *)
    (* CR mshinwell: update comment *)
    Ugeneric_apply (ufundecl, uargs, dbg)

(* Given the approximation for a closure (which always corresponds to a single
   function), find the approximation of the function's result, if known. *)
and approx_result_of_application t ~(fundecl_approx : Flambdaexport.approx) =
  match find_approx_descr t fundecl_approx with
  | None -> Value_unknown
  | Some fundecl_descr ->
    match fundecl_descr with
    | Value_closure { closure_id; set_of_closures = { results } } ->
      begin match Closure_id.Map.find closure_id results with
      | result_approx -> result_approx
      | exception Not_found ->
        Misc.fatal_error "Value_closure approximation provides no \
            approximation for the result of the given closure ID: %a %a"
          Flambdaexport.print_approx fundecl_approx
          Flambdaexport.print_descr fundecl_descr
      end
    (* CR mshinwell for pchambart: I added cases here to cause a fatal error;
       is this correct? *)
    | Value_block _ | Value_int _ | Value_constptr _ | Value_float _
    | Value_boxed_int _ | Value_string | Value_set_of_closures _ ->
      Misc.fatal_error "Inappropriate approximation for function
          being applied: %a %a"
        Flambdaexport.print_approx fundecl_approx
        Flambdaexport.print_descr fundecl_descr

and fclambda_and_approx_for_simple_application t env ~expr ~fundecls
      ~fv ~specialised_args ~closure_id ~args ~dbg =
  let args, uargs, args_approx =
    fclambda_and_approx_for_expr_list t env args
  in
  let fundecl =
    try find_declaration closure_id fundecls
    with Not_found ->
      Misc.fatal_errorf "Direct-application expression contains closure ID not
          present in corresponding set of closures: %a %a"
        Printflambda.flambda expr
        Closure_id.print closure_id
  in
  assert (List.length args = List.length fundecl.params);
  assert (List.length uargs = List.length fundecl.params);
  assert (List.length args_approx = List.length fundecl.params);
  let params_to_args_approx =
    (* CR mshinwell for pchambart: would it be harmful to avoid this
       [filter]?  (Presumably if an argument is not specialized we won't
       look at its approximation.) *)
    Variable.Map.filter (fun var _ ->
        Variable.Map.mem var specialised_args)
      (Variable.Map.of_list (List.combine fundecl.params args_approx))
  in
  let fundecl, ufundecl, fundecl_approx =
    fclambda_and_approx_for_function_declaration t env fundecl
      args_approx specialised_args fv
  in
  let flambda =
    Fapply ({
      ap_function =
        Fclosure ({
          fu_closure = fundecls; (* XXX check *)
          fu_fun = closure_id;
          fu_relative_to = None
        }, ());
      ap_arg = args;
      ap_kind = Direct closure_id;
      ap_dbg = dbg;
    }, ()),
  in
  let clambda = clambda_for_application t env ~direct ~ufundecl ~uargs ~dbg in
  flambda, clambda, approx_result_of_application t ~fundecl_approx

and fclambda_and_approx_for_non_simple_application t env ~expr ~direct
      ~fundecls ~fv ~specialised_args ~closure_id ~args ~dbg =
  let args, uargs, args_approx =
    fclambda_and_approx_for_expr_list t env args
  in
  let fundecl, ufundecl, fundecl_approx =
    fclambda_and_approx_for_function_declaration env funct
  in
  assert (List.length args = List.length fundecl.params);
  assert (List.length uargs = List.length fundecl.params);
  assert (List.length args_approx = List.length fundecl.params);
  let direct =
    match direct with
    | Direct _ -> direct
    | Indirect ->
      (* CR mshinwell for pchambart: Do we still need to do this?  It it
         definitely the case that we don't need to do this for the "simple"
         case above, too (before checking [ap_kind])?
         follow-up: we should just ensure simplify is run, perhaps.  The
         tighter the invariants are here, the better.
      *)
      match find_approx_descr t fun_approx with
      (* We mark some calls as direct when it is unknown:
         for instance if simplify wasn't run before. *)
      | Some (Value_closure { closure_id }) when
          (function_arity closure_id) = List.length args ->
        Direct closure_id
      | _ -> Indirect
  in
  let flambda =
    Fapply ({
      ap_function = fundecl;
      ap_arg = args;
      ap_kind = direct;
      ap_dbg = dbg;
    }, ())
  in
  let clambda = clambda_for_application t env ~direct ~ufundecl ~uargs ~dbg in
  fclambda, clambda, approx_for_application ~fundecl_approx

and fclambda_and_approx_for_application t env (expr : _ Flambda.t) =
  match expr with
  | Fapply ({
    (* The simple case: direct call of a closure from a known set of
       closures. *)
      ap_function =
        Fclosure ({
          fu_closure =
            Fset_of_closures ({ cl_fun = fundecls; cl_free_var = fv;
                cl_specialised_arg = specialised_args; }, _);
          fu_fun = closure_id;
          fu_relative_to = (None as rel) }, _);
      ap_arg = args;
      ap_kind = Direct closure_id';
      ap_dbg = dbg }, _) ->
    (* CR mshinwell: investigate why [Direct] needs to specify the
       closure ID; it would be nice to statically eliminate the possibility
       that it doesn't match [closure_id] *)
    assert (Closure_id.equal closure_id closure_id');
    fclambda_and_approx_for_simple_application t env ~expr ~fundecls ~fv
      ~specialised_args ~closure_id ~args ~dbg
  | Fapply ({ ap_function = funct; ap_arg = args; ap_kind = direct;
      ap_dbg = dbg }, _) ->
    (* CR mshinwell: consider adding comments illustrating what ends up
       here. *)
    fclambda_and_approx_for_non_simple_application t env ~expr ~direct
      ~fundecls ~fv ~specialised_args ~closure_id ~args ~dbg

(* Note: there is at most one closure (likewise, one set of closures) in
   scope during any one call to [conv].  Accesses to outer closures are
   performed via this distinguished one.  (This was arranged during closure
   conversion---see [Flambdagen].) *)
and fclambda_and_approx_for_expr t env (expr : _ Flambda.t)
      : unit Flambda.t * Clambda.ulambda * Flambdaexport.approx =
  match expr with
  | Fvar (var, _) -> flambda_and_approx_for_var t env ~var
  | Fsymbol (sym, _) -> fclambda_and_approx_for_symbol t env sym
  | Fconst (cst, annot) -> fclambda_and_approx_for_constant t env ~cst ~annot
  | Flet (str, var, lam, body, _) ->
    fclambda_and_approx_for_let t ~env ~str ~var ~lam ~body
  | Fletrec (defs, body, _) -> fclambda_and_approx_for_let_rec t env defs body
    Uletrec (udefs, conv env body)
  | Fset_of_closures set_of_closures ->
    fclambda_and_approx_for_set_of_closures_declaration t env set_of_closures
  | Fclosure closure -> fclambda_and_approx_for_closure_reference env closure
  | Fvariable_in_closure var_in_closure ->
    fclambda_and_approx_for_variable_in_closure env var_in_closure
  | Fapply apply -> fclambda_and_approx_for_application env apply
  | Fswitch (arg, sw, _) -> fclambda_and_approx_for_switch env ~arg ~sw
  | Fstringswitch (arg, sw, def, _) ->
    let arg, uarg = fclambda_for_expr t env arg in
    let sw, usw =
      List.fold_left (fun (sw, usw) (s, e) ->
          let e, ue = fclambda_for_expr t env e in
          e::sw, ue::usw)
        [] sw
    in
    let def, udef =
      match def with
      | None -> None, None
      | Some def ->
        let def, udef = fclambda_for_expr t env def in
        Some def, Some udef
    in
    Fstringswitch (arg, sw, def, ()), Ustringswitch (uarg, usw, udef),
      Value_unknown
  | Fprim (primitive, args, dbg, _) ->
    let args, uargs = fclambda_for_expr_list env args in
    fclambda_and_approx_for_primitive ~env ~primitive ~args ~uargs ~dbg
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
    Fwhile (cond, body, ()), Uwhile (cond, body), t.unit_approx
  | Ffor (var, lo, hi, dir, body, _) ->
    let lo, ulo, hi, uhi = fclambda_for_expr_pair env lo hi in
    let id, env_body = add_unique_ident var env in
    let body, ubody = fclambda_for_expr env_body body in
    Ffor (var, lo, hi, dir, body, ()), Ufor (id, lo, hi, dir, body),
      t.unit_approx
  | Fassign (var, lam, _) ->
    let lam, ulam = fclambda_for_expr env lam in
    let id = try find_var var env with Not_found -> assert false in
    Fassign (var, lam, ()), Uassign (id, ulam), t.unit_approx
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
  let expr, uexpr, _approx = fclambda_and_approx_for_expr env expr in
  expr, uexpr

and clambda_for_expr env expr =
  let _expr, uexpr, _approx = fclambda_and_approx_for_expr env expr in
  uexpr

and fclambda_for_expr_pair env expr1 expr2 =
  let expr1, uexpr1 = fclambda_for_expr env expr1 in
  let expr2, uexpr2 = fclambda_for_expr env expr2 in
  expr1, uexpr1, expr2, uexpr2

let starting_state_for_current_unit ~expr : Flambdaexport.exported_mutable =
  let constant_closures = Flambdaconstants.constant_closures expr in
  let sets_of_closures, closures, kept_arguments =
    Flambdautils.sets_of_closures_and_closures_and_kept_arguments expr in
  in
  { values = Compilation_unit.Tbl.create ();
    globals = Ident.Tbl.create ();
    symbol_id = Symbol.Tbl.create ();
    constants = Symbol.Tbl.create ();
    offset_fun = Closure_id.Tbl.create 42;
    offset_fv = Var_within_closure_id.Tbl.create 42;
    sets_of_closures; closures; constant_closures; kept_arguments;
  }

(* Add to [Compilenv] the definitions of those structured constants emitted
   by this pass, as opposed to those emitted by Cmmgen.  A set of all
   symbols corresponding to such constants is returned. *)
let add_exported_constants_to_compilenv t =
  let constants =
    Flambda_share_constants.compute_sharing
      ~constants:t.current_unit.constants
      ~fclambda_for_expr:(fclambda_for_expr t Env.empty)
  in
  (* XXX what happens to the constant's defining expr? *)
  Symbol.Map.fold (fun sym _cst all_syms ->
      let lbl = Symbol.string_of_linkage_name sym.sym_label in
      Compilenv.add_exported_constant lbl;
      Symbol.Set.add sym all_syms)
    constants
    Symbol.Set.empty

let export_id_of_global_module_block t =
  let max_index =
    Hashtbl.fold (fun index_in_block _ max_index ->
        assert (index_in_block >= 0);
        max index_in_block max_index)
      t.global (-1)
  in
  let fields =
    Array.init (max_index + 1) (fun index_in_block ->
        match Hashtbl.find t.global index_in_block with
        | exception Not_found -> Value_unknown
        | approx ->
          Flambdaexport.canonical_approx approx
            ~canonicalize_symbol:xxx
  in
  add_value_description (Value_block (0, fields))

(* Offsets into closures defined in external units, that are used by the
   current unit, need to be exported by the current unit.  This function
   returns such offsets together with those defined by the current unit. *)
let fun_and_fv_offsets_for_export t ~expr =
  (* CR mshinwell: reference the other comment, whereever it is *)
  let add_reexported_offsets_fun, add_reexported_offsets_fv =
    let extern_fun_offset_table, extern_fv_offset_table =
      (Compilenv.approx_env ()).offset_fun,
        (Compilenv.approx_env ()).offset_fv
    in
    Flambda_lay_out_closure.reexported_offsets
      ~extern_fun_offset_table:t.imported_units.offset_fun
      ~extern_fv_offset_table:t.imported_units.offset_fv
      ~expr
      ~constants:t.current_unit.constants
  in
  add_reexported_offsets_fun t.current_unit.offset_fun,
    add_reexported_offsets_fv t.current_unit.offset_fv

(* Translate an Flambda term to Clambda, recording necessary information
   (including export information) in Compilenv. *)
let translate_and_update_compilenv ~compilation_unit ~(expr : _ Flambda.t)
      : Clambda.ulambda =
  let current_unit = starting_state_for_current_unit ~expr in
  let imported_units = Compilenv.approx_env () in
  let unit_approx =
    Value_id (add_value_description' current_unit (Value_constptr 0))
  in
  let symbol_alias_db = Symbol_alias_db.create () in
  let t =
    { current_unit; imported_units; unit_approx; symbol_alias_db; }
  in
  let expr, uexpr = fclambda_for_expr t (Env.create ()) expr in
  Flambdacheck.check expr ~current_compilation_unit:compilation_unit
    ~flambdasym:true;
  let constants = add_exported_constants_to_compilenv t in
  let sets_of_closures =
    (* Recover the defining expressions for all function declarations.  These
       may occur directly within [expr], but may also have been moved out into
       [t.current_unit.constants] (and been replaced by a symbol access within
       [expr]). *)
    Set_of_closures_id.Map.disjoint_union
      (Flambdautils.all_sets_of_closures_defined_by expr)
      (Export_id.Tbl.fold (fun _export_id constant sets_of_closures ->
          Set_of_closures_id.Map.disjoint_union sets_of_closures
            (Flambdautils.all_sets_of_closures_defined_by constant))
        t.current_unit.constants
        Set_of_closures_id.Map.empty
  in
  let closures =
    Flambdautils.sets_of_closures_map_to_closures_map sets_of_closures
  in
  let ex_values =
    Flambdaexport.freeze_values t.current_unit ~compilation_unit
  in
  let export_id_of_global_module_block = export_id_of_global_module_block t in
  let globals =
    Ident.Map.singleton (Compilenv.current_unit_id ())
      (Value_id export_id_of_global_module_block)
  in
  let offset_fun, offset_fv = fun_and_fv_offsets_for_export t ~expr in
  (* XXX this next part needs work.  Where do we populate [symbol_id]? *)
  let symbol_id =
    let aux sym ex map =
      let sym' = canonical_symbol sym in
      Symbol.Map.add sym' ex map
    in
    Symbol.Map.fold aux !(infos.symbol_id)
      (Symbol.Map.add (Compilenv.current_unit_symbol ())
        export_id_of_global_module_block
        Symbol.Map.empty)
  in
  let id_symbol =
    Symbol.Map.fold (fun sym id map -> Export_id.Map.add id sym map)
      symbol_id Export_id.Map.empty
  in
  let exported : Flambdaexport.exported =
    { sets_of_closures; closures; ex_values; globals; id_symbol; symbol_id;
      offset_fun; offset_fv; constants;
      constant_closures = t.current_unit.constant_closures;
      kept_arguments = t.current_unit.kept_arguments;
    }
  in
  Compilenv.set_export_info exported;
  uexpr
