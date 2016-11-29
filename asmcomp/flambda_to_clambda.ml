(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type for_one_or_more_units = {
  fun_offset_table : int Closure_id.Map.t;
  fv_offset_table : int Var_within_closure.Map.t;
  closures : Flambda.function_declarations Closure_id.Map.t;
  constant_sets_of_closures : Set_of_closures_id.Set.t;
}

type t = {
  current_unit : for_one_or_more_units;
  imported_units : for_one_or_more_units;
  traps : Continuation.t Trap_id.Map.t;
  exn_handlers : Continuation.Set.t;
}

type ('a, 'b) declaration_position =
  | Current_unit of 'a
  | Imported_unit of 'b
  | Not_declared

let get_fun_offset t closure_id =
  let fun_offset_table =
    if Closure_id.in_compilation_unit closure_id (Compilenv.current_unit ())
    then t.current_unit.fun_offset_table
    else t.imported_units.fun_offset_table
  in
  try Closure_id.Map.find closure_id fun_offset_table
  with Not_found ->
    Misc.fatal_errorf "Flambda_to_clambda: missing offset for closure %a"
      Closure_id.print closure_id

let get_fv_offset t var_within_closure =
  let fv_offset_table =
    if Var_within_closure.in_compilation_unit var_within_closure
        (Compilenv.current_unit ())
    then t.current_unit.fv_offset_table
    else t.imported_units.fv_offset_table
  in
  try Var_within_closure.Map.find var_within_closure fv_offset_table
  with Not_found ->
    Misc.fatal_errorf "Flambda_to_clambda: missing offset for variable %a"
      Var_within_closure.print var_within_closure

let function_declaration_position t closure_id =
  try
    Current_unit (Closure_id.Map.find closure_id t.current_unit.closures)
  with Not_found ->
    try
      Imported_unit (Closure_id.Map.find closure_id t.imported_units.closures)
    with Not_found -> Not_declared

let is_function_constant t closure_id =
  match function_declaration_position t closure_id with
  | Current_unit { set_of_closures_id } ->
    Set_of_closures_id.Set.mem set_of_closures_id
      t.current_unit.constant_sets_of_closures
  | Imported_unit { set_of_closures_id } ->
    Set_of_closures_id.Set.mem set_of_closures_id
      t.imported_units.constant_sets_of_closures
  | Not_declared ->
    Misc.fatal_errorf "Flambda_to_clambda: missing closure %a"
      Closure_id.print closure_id

(* Instrumentation of closure and field accesses to try to catch compiler
   bugs. *)

let check_closure ulam named : Clambda.ulambda =
  if not !Clflags.clambda_checks then ulam
  else
    let desc =
      Primitive.simple ~name:"caml_check_value_is_closure"
        ~arity:2 ~alloc:false
    in
    let str = Format.asprintf "%a" Flambda.print_named named in
    let str_const =
      Compilenv.new_structured_constant (Uconst_string str) ~shared:true
    in
    Uprim (Pccall desc,
           [ulam; Clambda.Uconst (Uconst_ref (str_const, None))],
           Debuginfo.none)

let check_field ulam pos named_opt : Clambda.ulambda =
  if not !Clflags.clambda_checks then ulam
  else
    let desc =
      Primitive.simple ~name:"caml_check_field_access"
        ~arity:3 ~alloc:false
    in
    let str =
      match named_opt with
      | None -> "<none>"
      | Some named -> Format.asprintf "%a" Flambda.print_named named
    in
    let str_const =
      Compilenv.new_structured_constant (Uconst_string str) ~shared:true
    in
    Uprim (Pccall desc, [ulam; Clambda.Uconst (Uconst_int pos);
        Clambda.Uconst (Uconst_ref (str_const, None))],
      Debuginfo.none)

module Env : sig
  type t

  val empty : t

  val add_subst : t -> Variable.t -> Clambda.ulambda -> t
  val find_subst_exn : t -> Variable.t -> Clambda.ulambda

  val add_fresh_ident : t -> Variable.t -> Ident.t * t
  val ident_for_var_exn : t -> Variable.t -> Ident.t

  val add_fresh_mutable_ident : t -> Mutable_variable.t -> Ident.t * t
  val ident_for_mutable_var_exn : t -> Mutable_variable.t -> Ident.t

  val add_allocated_const : t -> Symbol.t -> Allocated_const.t -> t
  val allocated_const_for_symbol : t -> Symbol.t -> Allocated_const.t option

  val keep_only_symbols : t -> t

  val add_continuation_alias
     : t
    -> Continuation.t
    -> alias_of:Continuation.t
    -> t

  val add_return_continuation : t -> Continuation.t -> t

  type expanded_continuation =
    | Normal of Continuation.t
    | Return_continuation

  val expand_continuation_aliases : t -> Continuation.t -> expanded_continuation
end = struct
  type expanded_continuation =
    | Normal of Continuation.t
    | Return_continuation

  type t =
    { subst : Clambda.ulambda Variable.Map.t;
      var : Ident.t Variable.Map.t;
      mutable_var : Ident.t Mutable_variable.Map.t;
      toplevel : bool;
      allocated_constant_for_symbol : Allocated_const.t Symbol.Map.t;
      continuations : expanded_continuation Continuation.Map.t;
    }

  let empty =
    { subst = Variable.Map.empty;
      var = Variable.Map.empty;
      mutable_var = Mutable_variable.Map.empty;
      toplevel = false;
      allocated_constant_for_symbol = Symbol.Map.empty;
      continuations = Continuation.Map.empty;
    }

  let add_subst t id subst =
    { t with subst = Variable.Map.add id subst t.subst }

  let find_subst_exn t id = Variable.Map.find id t.subst

  let ident_for_var_exn t id = Variable.Map.find id t.var

  let add_fresh_ident t var =
    let id = Ident.create (Variable.unique_name var) in
    id, { t with var = Variable.Map.add var id t.var }

  let ident_for_mutable_var_exn t mut_var =
    Mutable_variable.Map.find mut_var t.mutable_var

  let add_fresh_mutable_ident t mut_var =
    let id = Mutable_variable.unique_ident mut_var in
    let mutable_var = Mutable_variable.Map.add mut_var id t.mutable_var in
    id, { t with mutable_var; }

  let add_allocated_const t sym cons =
    { t with
      allocated_constant_for_symbol =
        Symbol.Map.add sym cons t.allocated_constant_for_symbol;
    }

  let allocated_const_for_symbol t sym =
    try
      Some (Symbol.Map.find sym t.allocated_constant_for_symbol)
    with Not_found -> None

  let keep_only_symbols t =
    { empty with
      allocated_constant_for_symbol = t.allocated_constant_for_symbol;
    }

  let expand_continuation_aliases t cont =
    match Continuation.Map.find cont t.continuations with
    | exception Not_found -> Normal cont
    | cont -> cont

  let add_return_continuation t cont =
    { t with
      continuations =
        Continuation.Map.add cont Return_continuation t.continuations;
    }

  let add_continuation_alias t cont ~alias_of =
    let alias_of = expand_continuation_aliases t alias_of in
    { t with
      continuations =
        Continuation.Map.add cont alias_of t.continuations;
    }
end

let subst_var env var : Clambda.ulambda =
  try Env.find_subst_exn env var
  with Not_found ->
    try Uvar (Env.ident_for_var_exn env var)
    with Not_found ->
      Misc.fatal_errorf "Flambda_to_clambda: unbound variable %a@."
        Variable.print var

let subst_vars env vars = List.map (subst_var env) vars

let build_uoffset ulam offset : Clambda.ulambda =
  if offset = 0 then ulam
  else Uoffset (ulam, offset)

let to_clambda_allocated_constant (const : Allocated_const.t)
      : Clambda.ustructured_constant =
  match const with
  | Float f -> Uconst_float f
  | Int32 i -> Uconst_int32 i
  | Int64 i -> Uconst_int64 i
  | Nativeint i -> Uconst_nativeint i
  | Immutable_string s | String s -> Uconst_string s
  | Immutable_float_array a | Float_array a -> Uconst_float_array a

let to_uconst_symbol env symbol : Clambda.ustructured_constant option =
  match Env.allocated_const_for_symbol env symbol with
  | Some ((Float _ | Int32 _ | Int64 _ | Nativeint _) as const) ->
    Some (to_clambda_allocated_constant const)
  | None  (* CR-soon mshinwell: Try to make this an error. *)
  | Some _ -> None

let to_clambda_symbol' env sym : Clambda.uconstant =
  let lbl = Linkage_name.to_string (Symbol.label sym) in
  Uconst_ref (lbl, to_uconst_symbol env sym)

let to_clambda_symbol env sym : Clambda.ulambda =
  Uconst (to_clambda_symbol' env sym)

let to_clambda_const env (const : Flambda.constant_defining_value_block_field)
      : Clambda.uconstant =
  match const with
  | Symbol symbol -> to_clambda_symbol' env symbol
  | Const (Int i) -> Uconst_int i
  | Const (Char c) -> Uconst_int (Char.code c)
  | Const (Const_pointer i) -> Uconst_ptr i

let switch_numconsts_for_if = Numbers.Int.Set.of_list [0; 1]

let switch_looks_like_if ~scrutinee ~(switch : Flambda.switch) =
  if not (Numbers.Int.Set.equal switch.numconsts switch_numconsts_for_if
    && Numbers.Int.Set.equal switch.numblocks Numbers.Int.Set.empty)
  then
    None
  else
    match switch.consts, switch.failaction with
    | [0, ifnot_cont; 1, ifso_cont], None
    | [1, ifso_cont; 0, ifnot_cont], None
    | [0, ifnot_cont], Some ifso_cont
    | [1, ifso_cont], Some ifnot_cont ->
      Some (ifso_cont, ifnot_cont)
    | _ ->
      Misc.fatal_errorf "Malformed Flambda.switch: %a"
        Flambda.print (Flambda.Switch (scrutinee, switch))

type tag_switch_or_string_switch =
  | Tag of (int * Continuation.t) list
  | String of (string * Continuation.t) list

let tag_switch_or_string_switch ~scrutinee ~(switch : Flambda.switch) =
  match switch.consts, switch.blocks with
  | [], [] ->
    Misc.fatal_errorf "Flambda.switch with no cases: %a"
      Flambda.print (Flambda.Switch (scrutinee, switch))
  | _, _ ->
    (* CR mshinwell: If this code stays, add [List.partition_map] *)
    let tags, strings =
      List.partition (fun ((pat : Ilambda.switch_block_pattern), _case) ->
          match pat with
          | Tag _ -> true
          | String _ -> false)
        switch.blocks
    in
    let tags =
      List.map (fun ((pat : Ilambda.switch_block_pattern), case) ->
          match pat with
          | Tag tag -> tag, case
          | String _ -> assert false)
        tags
    in
    let strings =
      List.map (fun ((pat : Ilambda.switch_block_pattern), case) ->
          match pat with
          | Tag _ -> assert false
          | String str -> str, case)
        strings
    in
    match switch.consts, tags, strings with
    | _, _::_, _::_ ->
      Misc.fatal_errorf "Flambda.switch with both tag and string cases: %a"
        Flambda.print (Flambda.Switch (scrutinee, switch))
    | [], [], [] -> assert false  (* see above *)
    | _consts, tags, [] -> Tag tags
    | _consts, [], strings -> String strings

let apply_cont env cont arg : Clambda.ulambda =
  let cont = Env.expand_continuation_aliases env cont in
  match cont with
  | Normal cont -> Ustaticfail (Continuation.to_int cont, [arg])
  | Return_continuation -> Uprim (Preturn, [arg], Debuginfo.none)

let apply_cont_returning_unit env cont : Clambda.ulambda =
  let cont = Env.expand_continuation_aliases env cont in
  match cont with
  | Normal cont -> Ustaticfail (Continuation.to_int cont, [])
  | Return_continuation ->
    Uprim (Preturn, [Clambda.Uconst (Uconst_int 0)], Debuginfo.none)

let rec to_clambda (t : t) env (flam : Flambda.t) : Clambda.ulambda =
  match flam with
  | Let { var; defining_expr; body; _ } ->
    (* TODO: synthesize proper value_kind *)
    let id, env_body = Env.add_fresh_ident env var in
    Ulet (Immutable, Pgenval, id, to_clambda_named t env var defining_expr,
      to_clambda t env_body body)
  | Let_mutable { var = mut_var; initial_value = var; body; contents_kind } ->
    let id, env_body = Env.add_fresh_mutable_ident env mut_var in
    let def = subst_var env var in
    Ulet (Mutable, contents_kind, id, def, to_clambda t env_body body)
  | Apply { kind = Function; func; continuation; args;
      call_kind = Direct direct_func; dbg = dbg } ->
    (* The closure _parameter_ of the function is added by cmmgen.
       At the call site, for a direct call, the closure argument must be
       explicitly added (by [to_clambda_direct_apply]); there is no special
       handling of such in the direct call primitive.
       For an indirect call, we do not need to do anything here; Cmmgen will
       do the equivalent of the previous paragraph when it generates a direct
       call to [caml_apply]. *)
    let ulam =
      to_clambda_direct_apply t func args direct_func dbg env
    in
    apply_cont env continuation ulam
  | Apply { kind = Function; func; continuation; args; call_kind = Indirect;
      dbg = dbg } ->
    let callee = subst_var env func in
    let ulam : Clambda.ulambda =
      Ugeneric_apply (check_closure callee (Flambda.Var func),
        subst_vars env args, dbg)
    in
    apply_cont env continuation ulam
  | Apply { kind = Method { kind; obj; }; func; continuation; args;
      call_kind = _; dbg = dbg; } ->
    let ulam : Clambda.ulambda =
      Usend (kind, subst_var env func, subst_var env obj,
        subst_vars env args, dbg)
    in
    apply_cont env continuation ulam
  | Switch (scrutinee, sw) ->
    let arg = subst_var env scrutinee in
    (* CR mshinwell: More transformations here to satisfy Cmmgen? *)
    begin match switch_looks_like_if ~scrutinee ~switch:sw with
    | Some (ifso_cont, ifnot_cont) ->
      let ifso = apply_cont_returning_unit env ifso_cont in
      let ifnot = apply_cont_returning_unit env ifnot_cont in
      Uifthenelse (arg, ifso, ifnot)
    | None ->
      match tag_switch_or_string_switch ~scrutinee ~switch:sw with
      | Tag blocks ->
        (* Note that the [failaction] may always be duplicated, since it's
           just a jump to a continuation.  ([Uswitch] doesn't have the notion
           of a default case.) *)
        let const_index, const_actions =
          to_clambda_switch t env sw.consts sw.numconsts sw.failaction
        in
        let block_index, block_actions =
          to_clambda_switch t env blocks sw.numblocks sw.failaction
        in
        Uswitch (arg,
          { us_index_consts = const_index;
            us_actions_consts = const_actions;
            us_index_blocks = block_index;
            us_actions_blocks = block_actions;
          })
      | String cases ->
        let cases =
          List.map (fun (str, cont) ->
              str, apply_cont_returning_unit env cont)
            cases
        in
        let failaction =
          match sw.failaction with
          | None -> None
          | Some cont -> Some (apply_cont_returning_unit env cont)
        in
        Ustringswitch (arg, cases, failaction)
    end
  | Apply_cont (cont, trap_action, args) ->
    if Continuation.Set.mem cont t.exn_handlers then begin
      Misc.fatal_errorf "Continuation %a is an exception handler and cannot \
          be called using [Apply_cont]"
        Continuation.print cont
    end;
    let cont = Env.expand_continuation_aliases env cont in
    let args = List.map (subst_var env) args in
    let expr : Clambda.ulambda =
      match cont with
      | Normal cont -> Ustaticfail (Continuation.to_int cont, args)
      | Return_continuation ->
        match args with
        | [arg] -> arg
        | [] -> Clambda.Uconst (Uconst_int 0)
        | _ ->
          Misc.fatal_errorf "Apply_cont of return continuation with more than \
              one argument: %a"
            Flambda.print flam
    in
    let trap_action : Clambda.ulambda option =
      match trap_action with
      | None -> None
      | Some (Push { id = _; exn_handler; }) ->
        Some (Clambda.Upushtrap (Continuation.to_int exn_handler))
      | Some (Pop id) ->
        match Trap_id.Map.find id t.traps with
        | exception Not_found ->
          Misc.fatal_errorf "Trap %a not in traps list" Trap_id.print id
        | exn_handler ->
          Some (Clambda.Upoptrap (Continuation.to_int exn_handler))
    in
    begin match trap_action with
    | None -> expr
    | Some trap_action -> Usequence (trap_action, expr)
    end
  | Let_cont { name; body; handler = Alias alias_of; } ->
    let env = Env.add_continuation_alias env name ~alias_of in
    to_clambda t env body
  | Let_cont { name; body; handler = Handler {
      params; handler; recursive }; } ->
    let env_handler, ids =
      List.fold_right (fun var (env, ids) ->
          let id, env = Env.add_fresh_ident env var in
          env, id :: ids)
        params (env, [])
    in
    let kind : Clambda.catch_kind =
      if Continuation.Set.mem name t.exn_handlers then begin
        assert (List.length params = 1);
        Exn_handler
      end else begin
        Normal recursive
      end
    in
    Ucatch (Continuation.to_int name, kind, ids,
      to_clambda t env body, to_clambda t env_handler handler)

and to_clambda_named (t : t) env var (named : Flambda.named) : Clambda.ulambda =
  match named with
  | Var var -> subst_var env var
  | Symbol sym -> to_clambda_symbol env sym
  | Const (Const_pointer n) -> Uconst (Uconst_ptr n)
  | Const (Int n) -> Uconst (Uconst_int n)
  | Const (Char c) -> Uconst (Uconst_int (Char.code c))
  | Allocated_const _ ->
    Misc.fatal_errorf "[Allocated_const] should have been lifted to a \
        [Let_symbol] construction before [Flambda_to_clambda]: %a = %a"
      Variable.print var
      Flambda.print_named named
  | Read_mutable mut_var ->
    begin try Uvar (Env.ident_for_mutable_var_exn env mut_var)
    with Not_found ->
      Misc.fatal_errorf "Unbound mutable variable %a in [Read_mutable]: %a"
        Mutable_variable.print mut_var
        Flambda.print_named named
    end
  | Assign { being_assigned; new_value } ->
    let id =
      try Env.ident_for_mutable_var_exn env being_assigned
      with Not_found ->
        Misc.fatal_errorf "Unbound mutable variable %a in [Assign]: %a"
          Mutable_variable.print being_assigned
          Flambda.print_named named
    in
    Uassign (id, subst_var env new_value)
  | Read_symbol_field (symbol, field) ->
    Uprim (Pfield field, [to_clambda_symbol env symbol], Debuginfo.none)
  | Set_of_closures set_of_closures ->
    to_clambda_set_of_closures t env set_of_closures
  | Project_closure { set_of_closures; closure_id } ->
    (* Note that we must use [build_uoffset] to ensure that we do not generate
       a [Uoffset] construction in the event that the offset is zero, otherwise
       we might break pattern matches in Cmmgen (in particular for the
       compilation of "let rec"). *)
    check_closure (
      build_uoffset
        (check_closure (subst_var env set_of_closures)
           (Flambda.Var set_of_closures))
        (get_fun_offset t closure_id))
      named
  | Move_within_set_of_closures { closure; start_from; move_to } ->
    check_closure (build_uoffset
      (check_closure (subst_var env closure)
         (Flambda.Var closure))
      ((get_fun_offset t move_to) - (get_fun_offset t start_from)))
      named
  | Project_var { closure; var; closure_id } ->
    let ulam = subst_var env closure in
    let fun_offset = get_fun_offset t closure_id in
    let var_offset = get_fv_offset t var in
    let pos = var_offset - fun_offset in
    Uprim (Pfield pos,
      [check_field (check_closure ulam (Var closure)) pos (Some named)],
      Debuginfo.none)
  | Prim (Pfield index, [block], dbg) ->
    Uprim (Pfield index, [check_field (subst_var env block) index None], dbg)
  | Prim (Psetfield (index, maybe_ptr, init), [block; new_value], dbg) ->
    Uprim (Psetfield (index, maybe_ptr, init), [
        check_field (subst_var env block) index None;
        subst_var env new_value;
      ], dbg)
  | Prim (Popaque, args, dbg) ->
    Uprim (Pidentity, subst_vars env args, dbg)
  | Prim (p, args, dbg) ->
    Uprim (p, subst_vars env args, dbg)
  | Proved_unreachable -> Uunreachable

and to_clambda_switch _t env cases num_keys default =
  let num_keys =
    if Numbers.Int.Set.cardinal num_keys = 0 then 0
    else Numbers.Int.Set.max_elt num_keys + 1
  in
  let index = Array.make num_keys 0 in
  let store = Flambda_utils.Switch_storer.mk_store () in
  begin match default with
  | Some def when List.length cases < num_keys ->
    ignore (store.act_store def)
  | _ -> ()
  end;
  List.iter (fun (key, cont) -> index.(key) <- store.act_store cont) cases;
  let actions =
    Array.map (fun cont : Clambda.ulambda ->
        apply_cont_returning_unit env cont)
      (store.act_get ())
  in
  match actions with
  | [| |] -> [| |], [| |]  (* May happen when [default] is [None]. *)
  | _ -> index, actions

and to_clambda_direct_apply t func args direct_func dbg env : Clambda.ulambda =
  let closed = is_function_constant t direct_func in
  let label = Compilenv.function_label direct_func in
  let uargs =
    let uargs = subst_vars env args in
    (* Remove the closure argument if the closure is closed.  (Note that the
       closure argument is always a variable, so we can be sure we are not
       dropping any side effects.) *)
    if closed then uargs else uargs @ [subst_var env func]
  in
  Udirect_apply (label, uargs, dbg)

(* Describe how to build a runtime closure block that corresponds to the
   given Flambda set of closures.

   For instance the closure for the following set of closures:

     let rec fun_a x =
       if x <= 0 then 0 else fun_b (x-1) v1
     and fun_b x y =
       if x <= 0 then 0 else v1 + v2 + y + fun_a (x-1)

   will be represented in memory as:

     [ closure header; fun_a;
       1; infix header; fun caml_curry_2;
       2; fun_b; v1; v2 ]

   fun_a and fun_b will take an additional parameter 'env' to
   access their closure.  It will be arranged such that in the body
   of each function the env parameter points to its own code
   pointer.  For example, in fun_b it will be shifted by 3 words.

   Hence accessing v1 in the body of fun_a is accessing the
   6th field of 'env' and in the body of fun_b the 1st field.
*)
and to_clambda_set_of_closures t env
      (({ function_decls; free_vars } : Flambda.set_of_closures)
        as set_of_closures) : Clambda.ulambda =
  let all_functions = Variable.Map.bindings function_decls.funs in
  let env_var = Ident.create "env" in
  let to_clambda_function
        (closure_id, (function_decl : Flambda.function_declaration))
        : Clambda.ufunction =
    let closure_id = Closure_id.wrap closure_id in
    let fun_offset =
      Closure_id.Map.find closure_id t.current_unit.fun_offset_table
    in
    let env =
      (* Inside the body of the function, we cannot access variables
         declared outside, so start with a suitably clean environment.
         Note that we must not forget the information about which allocated
         constants contain which unboxed values. *)
      let env = Env.keep_only_symbols env in
      let env =
        Env.add_return_continuation env function_decl.continuation_param
      in
      (* Add the Clambda expressions for the free variables of the function
         to the environment. *)
      let add_env_free_variable id _ env =
        let var_offset =
          try
            Var_within_closure.Map.find
              (Var_within_closure.wrap id) t.current_unit.fv_offset_table
          with Not_found ->
            Misc.fatal_errorf "Clambda.to_clambda_set_of_closures: offset for \
                free variable %a is unknown.  Set of closures: %a"
              Variable.print id
              Flambda.print_set_of_closures set_of_closures
        in
        let pos = var_offset - fun_offset in
        Env.add_subst env id
          (Uprim (Pfield pos, [Clambda.Uvar env_var], Debuginfo.none))
      in
      let env = Variable.Map.fold add_env_free_variable free_vars env in
      (* Add the Clambda expressions for all functions defined in the current
         set of closures to the environment.  The various functions may be
         retrieved by moving within the runtime closure, starting from the
         current function's closure. *)
      let add_env_function pos env (id, _) =
        let offset =
          Closure_id.Map.find (Closure_id.wrap id)
            t.current_unit.fun_offset_table
        in
        let exp : Clambda.ulambda = Uoffset (Uvar env_var, offset - pos) in
        Env.add_subst env id exp
      in
      List.fold_left (add_env_function fun_offset) env all_functions
    in
    let env_body, params =
      List.fold_right (fun var (env, params) ->
          let id, env = Env.add_fresh_ident env var in
          env, id :: params)
        function_decl.params (env, [])
    in
    { label = Compilenv.function_label closure_id;
      arity = Flambda_utils.function_arity function_decl;
      params = params @ [env_var];
      body = to_clambda t env_body function_decl.body;
      dbg = function_decl.dbg;
    }
  in
  let funs = List.map to_clambda_function all_functions in
  let free_vars =
    Variable.Map.bindings (Variable.Map.map (
      fun (free_var : Flambda.specialised_to) ->
        subst_var env free_var.var) free_vars)
  in
  Uclosure (funs, List.map snd free_vars)

and to_clambda_closed_set_of_closures t env symbol
      ({ function_decls; } : Flambda.set_of_closures)
      : Clambda.ustructured_constant =
  let functions = Variable.Map.bindings function_decls.funs in
  let to_clambda_function (id, (function_decl : Flambda.function_declaration))
        : Clambda.ufunction =
    (* All that we need in the environment, for translating one closure from
       a closed set of closures, is the substitutions for variables bound to
       the various closures in the set.  Such closures will always be
       referenced via symbols. *)
    let env =
      List.fold_left (fun env (var, _) ->
          let closure_id = Closure_id.wrap var in
          let symbol = Compilenv.closure_symbol closure_id in
          Env.add_subst env var (to_clambda_symbol env symbol))
        (Env.keep_only_symbols env)
        functions
    in
    let env =
      Env.add_return_continuation env function_decl.continuation_param
    in
    let env_body, params =
      List.fold_right (fun var (env, params) ->
          let id, env = Env.add_fresh_ident env var in
          env, id :: params)
        function_decl.params (env, [])
    in
    { label = Compilenv.function_label (Closure_id.wrap id);
      arity = Flambda_utils.function_arity function_decl;
      params;
      body = to_clambda t env_body function_decl.body;
      dbg = function_decl.dbg;
    }
  in
  let ufunct = List.map to_clambda_function functions in
  let closure_lbl = Linkage_name.to_string (Symbol.label symbol) in
  Uconst_closure (ufunct, closure_lbl, [])

let to_clambda_initialize_symbol t env symbol fields
      : Clambda.ulambda * Continuation.t =
  let fields =
    List.mapi (fun index (expr, cont) ->
        index, to_clambda t env expr, cont)
      fields
  in
  let build_setfield index id : Clambda.ulambda =
    (* Note that this will never cause a write barrier hit, owing to
       the [Initialization]. *)
    Uprim (Psetfield (index, Pointer, Initialization),
      [to_clambda_symbol env symbol; Clambda.Uvar id],
      Debuginfo.none)
  in
  match fields with
  | [] -> assert false  (* See below. *)
  | (first_pos, first, first_cont)::rest ->
    let acc, prev_pos, prev_cont =
      List.fold_left (fun (acc, prev_pos, prev_cont) (pos, field, cont) ->
          let prev_cont = Continuation.to_int prev_cont in
          let id = Ident.create "prev_field" in
          Clambda.Ucatch (prev_cont, Normal Nonrecursive, [id], acc,
              Usequence (build_setfield prev_pos id, field)),
            pos, cont)
        (first, first_pos, first_cont)
        rest
    in
    let id = Ident.create "prev_field" in
    let return_cont = Continuation.create () in
    Clambda.Ucatch (Continuation.to_int prev_cont, Normal Nonrecursive,
        [id], acc,
        Usequence (build_setfield prev_pos id,
          Ustaticfail (Continuation.to_int return_cont, []))),
      return_cont

let accumulate_structured_constants t env symbol
      (c : Flambda.constant_defining_value) acc =
  match c with
  | Allocated_const c ->
    Symbol.Map.add symbol (to_clambda_allocated_constant c) acc
  | Block (tag, fields) ->
    let fields = List.map (to_clambda_const env) fields in
    Symbol.Map.add symbol (Clambda.Uconst_block (Tag.to_int tag, fields)) acc
  | Set_of_closures set_of_closures ->
    let to_clambda_set_of_closures =
      to_clambda_closed_set_of_closures t env symbol set_of_closures
    in
    Symbol.Map.add symbol to_clambda_set_of_closures acc
  | Project_closure _ -> acc

let to_clambda_program t env constants (program : Flambda.program) =
  let rec loop env constants (program : Flambda.program_body)
        : Clambda.ulambda * Clambda.ustructured_constant Symbol.Map.t =
    match program with
    | Let_symbol (symbol, alloc, program) ->
      (* Useful only for unboxing. Since floats and boxed integers will
         never be part of a Let_rec_symbol, handling only the Let_symbol
         is sufficient. *)
      let env =
        match alloc with
        | Allocated_const const -> Env.add_allocated_const env symbol const
        | _ -> env
      in
      let constants =
        accumulate_structured_constants t env symbol alloc constants
      in
      loop env constants program
    | Let_rec_symbol (defs, program) ->
      let constants =
        List.fold_left (fun constants (symbol, alloc) ->
            accumulate_structured_constants t env symbol alloc constants)
          constants defs
      in
      loop env constants program
    | Initialize_symbol (symbol, _tag, fields, program) ->
      (* The tag is ignored here: It is used separately to generate the
         preallocated block. Only the initialisation code is generated
         here. *)
      let e2, constants = loop env constants program in
      begin match fields with
      | [] -> e2, constants
      | fields ->
        let e1, cont = to_clambda_initialize_symbol t env symbol fields in
        Ucatch (Continuation.to_int cont, Normal Nonrecursive, [], e1, e2),
          constants
      end
    | Effect (expr, cont, program) ->
      let e1 = to_clambda t env expr in
      let e2, constants = loop env constants program in
      let cont = Continuation.to_int cont in
      let unused = Ident.create "unused" in
      Ucatch (cont, Normal Nonrecursive, [unused], e1, e2), constants
    | End _ ->
      Uconst (Uconst_ptr 0), constants
  in
  loop env constants program.program_body

let collect_traps program =
  let traps = ref Trap_id.Map.empty in
  let exn_handlers = ref Continuation.Set.empty in
  Flambda_iterators.iter_exprs_at_toplevel_of_program program ~f:(fun expr ->
    Flambda_iterators.iter (fun (expr : Flambda.expr) ->
        match expr with
        | Apply_cont (_, Some (Push { id; exn_handler; }), _) ->
          traps := Trap_id.Map.add id exn_handler !traps;
          exn_handlers := Continuation.Set.add exn_handler !exn_handlers
        | _ -> ())
      (fun _named -> ())
      expr);
  !traps, !exn_handlers

type result = {
  expr : Clambda.ulambda;
  preallocated_blocks : Clambda.preallocated_block list;
  structured_constants : Clambda.ustructured_constant Symbol.Map.t;
  exported : Export_info.t;
}

let convert (program, exported) : result =
  let current_unit =
    let offsets = Closure_offsets.compute program in
    { fun_offset_table = offsets.function_offsets;
      fv_offset_table = offsets.free_variable_offsets;
      closures = Flambda_utils.make_closure_map program;
      constant_sets_of_closures =
        Flambda_utils.all_lifted_constant_sets_of_closures program;
    }
  in
  let imported_units =
    let imported = Compilenv.approx_env () in
    { fun_offset_table = imported.offset_fun;
      fv_offset_table = imported.offset_fv;
      closures = imported.closures;
      constant_sets_of_closures = imported.constant_sets_of_closures;
    }
  in
  let traps, exn_handlers = collect_traps program in
  let t = { current_unit; imported_units; traps; exn_handlers; } in
  let preallocated_blocks =
    List.map (fun (symbol, tag, fields) ->
        { Clambda.
          symbol = Linkage_name.to_string (Symbol.label symbol);
          exported = true;
          tag = Tag.to_int tag;
          size = List.length fields;
        })
      (Flambda_utils.initialize_symbols program)
  in
  let expr, structured_constants =
    to_clambda_program t Env.empty Symbol.Map.empty program
  in
  let offset_fun, offset_fv =
    Closure_offsets.compute_reexported_offsets program
      ~current_unit_offset_fun:current_unit.fun_offset_table
      ~current_unit_offset_fv:current_unit.fv_offset_table
      ~imported_units_offset_fun:imported_units.fun_offset_table
      ~imported_units_offset_fv:imported_units.fv_offset_table
  in
  let exported =
    Export_info.add_clambda_info exported
      ~offset_fun
      ~offset_fv
      ~constant_sets_of_closures:current_unit.constant_sets_of_closures
  in
  { expr; preallocated_blocks; structured_constants; exported; }
