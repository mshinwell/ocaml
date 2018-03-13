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

(* CR mshinwell: Recognise patterns for CFnlt etc. *)

(*
[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type for_one_or_more_units = {
  fun_offset_table : int Closure_id.Map.t;
  fv_offset_table : int Var_within_closure.Map.t;
  closures : Flambda.Function_declarations.t Closure_id.Map.t;
  constant_sets_of_closures : Set_of_closures_id.Set.t;
  arity_table : int Closure_id.Map.t;
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

let get_fun_arity t (closure_id:Closure_id.t) =
  let arity_table =
    if Closure_id.in_compilation_unit closure_id (Compilenv.current_unit ())
    then t.current_unit.arity_table
    else t.imported_units.arity_table
  in
  try Closure_id.Map.find closure_id arity_table
  with Not_found ->
    Misc.fatal_errorf "Flambda_to_clambda: missing arity for closure %a"
      Closure_id.print closure_id

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
end

let find_var env var =
  try Env.ident_for_var_exn env var
  with Not_found ->
    Misc.fatal_errorf "Flambda_to_clambda: unbound variable %a@."
      Variable.print var

let subst_var env var : Clambda.ulambda =
  try Env.find_subst_exn env var
  with Not_found -> Uvar (find_var env var)

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

let test_closure_code_pointer t uclosure closure_id : Clambda.ulambda =
  let code_pointer_field =
    (* If the arity is zero or one, the code pointer is at offset 0
       othewise it is at offset 2.
       Offset 0 is the indirect call code pointer 'caml_curry_n', and
       offset 1 is the arity *)
    if get_fun_arity t closure_id <= 1 then 0 else 2
  in
  let code_pointer_from_the_closure_value =
    Clambda.Uprim (Pfield code_pointer_field, [uclosure], Debuginfo.none)
  in
  let code_pointer : Clambda.ulambda =
    Uconst(Uconst_ref (Compilenv.function_label closure_id, None))
  in
  Uprim (Pintcomp Ceq,
         [code_pointer;
          code_pointer_from_the_closure_value],
         Debuginfo.none)

let switch_numconsts_for_if = Numbers.Int.Set.of_list [0; 1]

let switch_looks_like_if ~scrutinee:_ ~(switch : Flambda.switch) =
  if not (Numbers.Int.Set.equal switch.numconsts switch_numconsts_for_if) then
    None
  else
    match switch.consts, switch.failaction with
    | [0, ifnot_cont; 1, ifso_cont], None
    | [1, ifso_cont; 0, ifnot_cont], None
    | [0, ifnot_cont], Some ifso_cont
    | [1, ifso_cont], Some ifnot_cont ->
      Some (ifso_cont, ifnot_cont)
    | _ -> None
      (* CR mshinwell: This breaks on e.g.
         (switch (2) ty/2606
           case int 1: goto k728
           case int 0: goto k727
           default: goto k724)
         For the moment we allow it. *)
(*
      Misc.fatal_errorf "Malformed Flambda.switch: %a"
        Flambda.print (Flambda.Switch (scrutinee, switch))
*)

(* CR mshinwell: combine with to_clambda_direct_apply? *)
let to_clambda_apply env cont ~continuation_arity arg : Clambda.ulambda =
  let dbg = Debuginfo.none in
  let cont = Env.expand_continuation_aliases env cont in
  (* Detect when a function that returns multiple results is being called. *)
  assert (continuation_arity >= 1);
  if continuation_arity < 2 then
    match cont with
    | Normal cont -> Ustaticfail (Continuation.to_int cont, [arg])
    | Return_continuation -> Uprim (Preturn, [arg], dbg)
  else
    match cont with
    | Normal cont ->
      let vars_and_indexes =
        Array.to_list (Array.init continuation_arity (fun index ->
          let var = Ident.create (Printf.sprintf "result%d" index) in
          var, index))
      in
      let call_cont : Clambda.ulambda =
        let vars =
          List.map (fun (var, _index) : Clambda.ulambda -> Uvar var)
            vars_and_indexes
        in
        Ustaticfail (Continuation.to_int cont, vars)
      in
      let apply_result = Ident.create "result" in
      Ulet (Immutable, Pgenval, apply_result, arg,
        List.fold_right (fun (var, index) expr : Clambda.ulambda ->
            Ulet (Immutable, Pgenval, var,
              Uprim (Punboxed_tuple_field index, [Clambda.Uvar apply_result],
                dbg),
              expr))
          vars_and_indexes
          call_cont)
    | Return_continuation ->
      (* If a multiple-result set is going straight to the return continuation
         of the current function, there's no point in fishing out all the
         results and then packaging them up again. *)
      Uprim (Preturn, [arg], Debuginfo.none)

let apply_cont_returning_unit env cont : Clambda.ulambda =
  let cont = Env.expand_continuation_aliases env cont in
  match cont with
  | Normal cont -> Ustaticfail (Continuation.to_int cont, [])
  | Return_continuation ->
    Uprim (Preturn, [Clambda.Uconst (Uconst_int 0)], Debuginfo.none)

let rec to_clambda (t : t) env (flam : Flambda.Expr.t) : Clambda.ulambda =
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
      call_kind = Direct { closure_id = direct_func; return_arity; };
      dbg = dbg; } ->
    (* The closure _parameter_ of the function is added by cmmgen.
       At the call site, for a direct call, the closure argument must be
       explicitly added (by [to_clambda_direct_apply]); there is no special
       handling of such in the direct call primitive.
       For an indirect call, we do not need to do anything here; Cmmgen will
       do the equivalent of the previous paragraph when it generates a direct
       call to [caml_apply]. *)
    let ulam =
      to_clambda_direct_apply t func args direct_func dbg env
        ~return_arity
    in
    to_clambda_apply env continuation ~continuation_arity:return_arity ulam
  | Apply { kind = Function; func; continuation; args; call_kind = Indirect;
      dbg = dbg } ->
    let callee = subst_var env func in
    let ulam : Clambda.ulambda =
      Ugeneric_apply (check_closure callee (Flambda.Var func),
        subst_vars env args, dbg)
    in
    to_clambda_apply env continuation ~continuation_arity:1 ulam
  | Apply { kind = Method { kind; obj; }; func; continuation;
      args; call_kind = _; dbg = dbg; } ->
    let ulam : Clambda.ulambda =
      Usend (kind, subst_var env func, subst_var env obj,
        subst_vars env args, dbg)
    in
    to_clambda_apply env continuation ~continuation_arity:1 ulam
  | Switch (scrutinee, sw) ->
    let arg = subst_var env scrutinee in
    (* CR mshinwell: More transformations here to satisfy Cmmgen? *)
    begin match switch_looks_like_if ~scrutinee ~switch:sw with
    | Some (ifso_cont, ifnot_cont) ->
      let ifso = apply_cont_returning_unit env ifso_cont in
      let ifnot = apply_cont_returning_unit env ifnot_cont in
      Uifthenelse (arg, ifso, ifnot)
    | None ->
(*
Format.eprintf "Compiling switch:@ \n%a\n%!" Flambda.print flam;
*)
      let result =
        to_clambda_switch t env arg sw.consts sw.numconsts sw.failaction
in
(*
Format.eprintf "Compiled Flambda switch:@ \n%a@ \nto Clambda switch:@ \n%a\n%!"
  Flambda.print flam
  Printclambda.clambda result;
*)
result
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
        let dbg = Debuginfo.none in
        match args with
        | [] -> Uprim (Preturn, [Clambda.Uconst (Uconst_int 0)], dbg)
        | [arg] -> Uprim (Preturn, [arg], dbg)
        | args ->
          let unboxed_tuple = Ident.create "multi_result" in
          Ulet (Immutable, Pgenval, unboxed_tuple,
            Uprim (Pmake_unboxed_tuple, args, dbg),
            Uprim (Preturn, [Clambda.Uvar unboxed_tuple], dbg))
    in
    let trap_action : Clambda.ulambda option =
      match trap_action with
      | None -> None
      | Some (Push { id = _; exn_handler; }) ->
        Some (Clambda.Upushtrap (Continuation.to_int exn_handler))
      | Some (Pop { id = _; exn_handler; }) ->
        Some (Clambda.Upoptrap (Continuation.to_int exn_handler))
    in
    begin match trap_action with
    | None -> expr
    | Some trap_action -> Usequence (trap_action, expr)
    end
  | Let_cont { body; handlers = Non_recursive { name; handler = {
      params; is_exn_handler; handler; _ }; }; } ->
    if Continuation.Set.mem name t.exn_handlers && not is_exn_handler then begin
      Misc.fatal_errorf "Continuation %a is an exception handler but is not \
          marked as such in the [Let_cont] that defines it"
        Continuation.print name
    end;
    if is_exn_handler && List.length params <> 1 then begin
      Misc.fatal_errorf "Continuation %a is an exception handler but does not \
          have exactly one parameter"
        Continuation.print name
    end;
    let env_handler, params =
      List.fold_right (fun param (env, ids) ->
          let var = Parameter.var param in
          let id, env = Env.add_fresh_ident env var in
          env, id :: ids)
        params (env, [])
    in
    let handler = to_clambda t env_handler handler in
    let name = Continuation.to_int name in
    let kind : Clambda.catch_kind =
      if is_exn_handler then Exn_handler
      else Normal Non_recursive
    in
    Ucatch (kind, [name, params, handler], to_clambda t env body)
  | Let_cont { body; handlers = Recursive handlers; } ->
    let conts =
      Continuation.Map.fold (fun name (handler : Flambda.continuation_handler)
                conts ->
          let env_handler, ids =
            List.fold_right (fun param (env, ids) ->
                let var = Parameter.var param in
                let id, env = Env.add_fresh_ident env var in
                env, id :: ids)
              handler.params (env, [])
          in
          let is_exn_handler = handler.is_exn_handler in
          let handler = to_clambda t env_handler handler.handler in
          (* We check both of these in case they are inconsistent. *)
          if is_exn_handler || Continuation.Set.mem name t.exn_handlers
          then begin
            Misc.fatal_errorf "Continuation %a is an exception handler, so \
                it cannot be involved in a [Recursive] [Let_cont]"
              Continuation.print name
          end;
          (Continuation.to_int name, ids, handler) :: conts)
        handlers
        []
    in
    Ucatch (Normal Recursive, conts, to_clambda t env body)
  | Invalid _ -> Uunreachable

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
    to_clambda_project_closure t env named set_of_closures closure_id
  | Move_within_set_of_closures { closure; move } ->
    to_clambda_move_within_set_of_closures t env named closure move
  | Project_var project_var ->
    to_clambda_project_var t env named project_var
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

and to_clambda_project_var t env named (project_var:Flambda.project_var)
  : Clambda.ulambda =
  match Closure_id.Map.bindings project_var.var with
  | [] -> Uunreachable
  | first_closure :: other_closures ->
    let uclosure =
      check_closure (subst_var env project_var.closure)
        (Flambda.Var project_var.closure)
    in
    let make_project_var (closure_id, var) : Clambda.ulambda =
      let fun_offset = get_fun_offset t closure_id in
      let var_offset = get_fv_offset t var in
      let pos = var_offset - fun_offset in
      Uprim (Pfield pos,
        [check_field uclosure
           pos (Some named)],
        Debuginfo.none)
    in
    (* CR pchambart: In most cases, This could be static.
       This is kept as a complete if tree right now to simplify testing *)
    List.fold_left (fun body ((closure_id, _) as projection) ->
      Clambda.Uifthenelse(
        test_closure_code_pointer t uclosure closure_id,
        make_project_var projection,
        body))
      (make_project_var first_closure)
      other_closures

and to_clambda_move_within_set_of_closures t env named closure move
  : Clambda.ulambda =
  match Closure_id.Map.bindings move with
  | [] -> Uunreachable
  | first_closure :: other_closures ->
    let uclosure = check_closure (subst_var env closure) (Flambda.Var closure) in
    let make_move (start_from, move_to) : Clambda.ulambda =
      check_closure
        (build_uoffset uclosure
           ((get_fun_offset t move_to) - (get_fun_offset t start_from)))
        named
    in
    (* CR pchambart: In most cases, This could be static.
       This is kept as a complete if tree right now to simplify testing *)
    List.fold_left (fun body ((closure_id, _) as projection) ->
      Clambda.Uifthenelse(
        test_closure_code_pointer t uclosure closure_id,
        make_move projection,
        body))
      (make_move first_closure)
      other_closures

and to_clambda_project_closure t env named set_of_closures closure_id
  : Clambda.ulambda =
  let uset_of_closures =
    check_closure (subst_var env set_of_closures)
      (Var set_of_closures)
  in
  match Closure_id.Set.elements closure_id with
  | [] -> Uunreachable
  | [closure_id] ->
    (* Note that we must use [build_uoffset] to ensure that we do not generate
       a [Uoffset] construction in the event that the offset is zero, otherwise
       we might break pattern matches in Cmmgen (in particular for the
       compilation of "let rec"). *)
    check_closure (
      build_uoffset
        uset_of_closures
        (get_fun_offset t closure_id))
      named
  | first_closure_id :: other_closure_ids ->
    (* A projection from multiple potential set of closures is
       compiled as an if tree. The set is recognized by the code
       pointer of the first function of the set (at offset 0). *)
    let make_project_closure closure_id =
      build_uoffset
        uset_of_closures
        (get_fun_offset t closure_id)
    in
    let test_set_of_closures closure_id : Clambda.ulambda =
      let code_pointer_of_the_first_function_of_the_set,
          arity_of_the_first_function_of_the_set =
        let function_declarations =
          match function_declaration_position t closure_id with
          | Current_unit declarations
          | Imported_unit declarations ->
            declarations
          | Not_declared ->
            Misc.fatal_errorf "Flambda_to_clambda: missing closure %a"
              Closure_id.print closure_id
        in
        let (first_closure_var, ({ params } : Flambda.Function_declaration.t)) =
          (* The set fields order follow is the comparison function order *)
          Variable.Map.min_binding function_declarations.funs
        in
        let first_closure_id = Closure_id.wrap first_closure_var in
        Clambda.Uconst(Uconst_ref
          (Compilenv.function_label first_closure_id, None)),
          List.length params
      in
      let code_pointer_from_the_set_of_closure_value =
        (* If the arity is zero or one, the code pointer is at offset 0
           othewise it is at offset 2 *)
        let code_pointer_field =
          if arity_of_the_first_function_of_the_set <= 1 then 0 else 2
        in
        Clambda.Uprim (Pfield code_pointer_field, [uset_of_closures],
          Debuginfo.none)
      in
      Uprim (Pintcomp Ceq,
             [code_pointer_of_the_first_function_of_the_set;
              code_pointer_from_the_set_of_closure_value],
             Debuginfo.none)
    in
    let ulam =
      List.fold_left (fun body closure_id : Clambda.ulambda ->
        Uifthenelse(
          test_set_of_closures closure_id,
          make_project_closure closure_id,
          body))
        (make_project_closure first_closure_id)
        other_closure_ids
    in
    check_closure ulam named

and to_clambda_switch _t env arg cases possible_values default
      : Clambda.ulambda =
  let min_possible_value = Numbers.Int.Set.min_elt possible_values in
  let max_possible_value = Numbers.Int.Set.max_elt possible_values in
  (* CR mshinwell: Think about how to do this properly. *)
  if min_possible_value >= 0 && max_possible_value < 256 then
    let indexes, actions =
      let possible_values =
        if Numbers.Int.Set.cardinal possible_values = 0 then 0
        else Numbers.Int.Set.max_elt possible_values + 1
      in
      let index = Array.make possible_values 0 in
      let store = Flambda_utils.Switch_storer.mk_store () in
      begin match default with
      | Some def when List.length cases < possible_values ->
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
    in
    Uswitch (arg,
      { us_index_consts = indexes;
        us_actions_consts = actions;
        us_index_blocks = [| |];
        us_actions_blocks = [| |];
      }, Debuginfo.none)  (* debug info will be added by a later GPR *)
  else
    let default : Clambda.ulambda =
      match default with
      | None -> Uunreachable
      | Some cont -> apply_cont_returning_unit env cont
    in
    List.fold_right (fun (possible_value, cont) expr : Clambda.ulambda ->
        Uifthenelse (
          Uprim (Pintcomp Ceq, [
              Clambda.Uconst (Uconst_int possible_value);
              arg;
            ], Debuginfo.none),
          apply_cont_returning_unit env cont,
          expr))
      cases
      default

and to_clambda_direct_apply t func args direct_func dbg env
      ~return_arity : Clambda.ulambda =
  let closed = is_function_constant t direct_func in
  let label = Compilenv.function_label direct_func in
  let uargs =
    let uargs = subst_vars env args in
    (* Remove the closure argument if the closure is closed.  (Note that the
       closure argument is always a variable, so we can be sure we are not
       dropping any side effects.) *)
    if closed then uargs else uargs @ [subst_var env func]
  in
  Udirect_apply (label, uargs, return_arity, dbg)

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
        (closure_id, (function_decl : Flambda.Function_declaration.t))
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
          let id, env = Env.add_fresh_ident env (Parameter.var var) in
          env, id :: params)
        function_decl.params (env, [])
    in
    { label = Compilenv.function_label closure_id;
      arity = Flambda_utils.function_arity function_decl;
      params = params @ [env_var];
      body = to_clambda t env_body function_decl.body;
      dbg = function_decl.dbg;
      env = Some env_var;
    }
  in
  let funs = List.map to_clambda_function all_functions in
  let free_vars =
    Variable.Map.bindings (Variable.Map.map (
      fun (free_var : Flambda.free_var) ->
        subst_var env free_var.var) free_vars)
  in
  Uclosure (funs, List.map snd free_vars)

and to_clambda_closed_set_of_closures t env symbol
      ({ function_decls; } : Flambda.set_of_closures)
      : Clambda.ustructured_constant =
  let functions = Variable.Map.bindings function_decls.funs in
  let to_clambda_function (id, (function_decl : Flambda.Function_declaration.t))
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
          let id, env = Env.add_fresh_ident env (Parameter.var var) in
          env, id :: params)
        function_decl.params (env, [])
    in
    { label = Compilenv.function_label (Closure_id.wrap id);
      arity = Flambda_utils.function_arity function_decl;
      params;
      body = to_clambda t env_body function_decl.body;
      dbg = function_decl.dbg;
      env = None;
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
    Uprim (Psetfield (index, Pointer, Root_initialization),
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
          Clambda.Ucatch (Normal Non_recursive,
              [prev_cont, [id], Usequence (build_setfield prev_pos id, field)],
              acc),
            pos, cont)
        (first, first_pos, first_cont)
        rest
    in
    let id = Ident.create "prev_field" in
    let return_cont = Continuation.create () in
    Clambda.Ucatch (Normal Non_recursive,
        [Continuation.to_int prev_cont, [id],
          Usequence (build_setfield prev_pos id,
            Ustaticfail (Continuation.to_int return_cont, []))],
        acc),
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
        Ucatch (Normal Non_recursive, [Continuation.to_int cont, [], e2], e1),
          constants
      end
    | Effect (expr, cont, program) ->
      let e1 = to_clambda t env expr in
      let e2, constants = loop env constants program in
      let cont = Continuation.to_int cont in
      let unused = Ident.create "unused" in
      Ucatch (Normal Non_recursive, [cont, [unused], e2], e1), constants
    | End _ ->
      Uconst (Uconst_ptr 0), constants
  in
  loop env constants program.program_body

(* CR mshinwell: we don't need "traps" now that Pop contains the continuation *)
let collect_traps program =
  let traps = ref Trap_id.Map.empty in
  let exn_handlers = ref Continuation.Set.empty in
  Flambda_iterators.iter_exprs_at_toplevel_of_program program
    ~f:(fun ~continuation_arity:_ _cont expr ->
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

let closure_arities program =
  let table = ref Closure_id.Map.empty in
  Flambda_iterators.iter_on_set_of_closures_of_program program
    ~f:(fun ~constant:_ { function_decls = { funs } } ->
        Variable.Map.iter
          (fun closure_var ({ params }:Flambda.Function_declaration.t) ->
             table :=
               Closure_id.Map.add (Closure_id.wrap closure_var)
                 (List.length params)
                 !table)
          funs);
  !table

let import_arities ({sets_of_closures}:Export_info.t) =
  let table = ref Closure_id.Map.empty in
  Set_of_closures_id.Map.iter (fun _ ({ funs } : Flambda.Function_declarations.t) ->
      Variable.Map.iter
        (fun closure_var ({ params }:Flambda.Function_declaration.t) ->
           table :=
             Closure_id.Map.add (Closure_id.wrap closure_var)
               (List.length params)
               !table)
        funs)
    sets_of_closures;
  !table

*)

let convert (_program, _exported) : result = assert false

(*
  let current_unit =
    let offsets = Closure_offsets.compute program in
    { fun_offset_table = offsets.function_offsets;
      fv_offset_table = offsets.free_variable_offsets;
      closures = Flambda_utils.make_closure_map program;
      constant_sets_of_closures =
        Flambda_utils.all_lifted_constant_sets_of_closures program;
      arity_table = closure_arities program;
    }
  in
  let imported_units =
    let imported = Compilenv.approx_env () in
    { fun_offset_table = imported.offset_fun;
      fv_offset_table = imported.offset_fv;
      closures = imported.closures;
      constant_sets_of_closures = imported.constant_sets_of_closures;
      arity_table = import_arities imported;
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
*)

(*

let lambda_value_kind t =
  let module L = Lambda in
  match t with
  | Value Must_scan -> Some L.Pgenval
  | Value Can_scan -> Some L.Pintval
  | Naked_immediate -> Some L.Pnaked_intval
  | Naked_float -> Some L.Pfloatval
  | Naked_int32 -> Some (L.Pboxedintval Pint32)
  | Naked_int64 -> Some (L.Pboxedintval Pint64)
  | Naked_nativeint -> Some (L.Pboxedintval Pnativeint)

*)
