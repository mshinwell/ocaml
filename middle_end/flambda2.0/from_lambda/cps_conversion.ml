(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016--2019 OCamlPro SAS                                    *)
(*   Copyright 2016--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* "Use CPS".
   -- A. Kennedy, "Compiling with Continuations Continued", ICFP 2007.
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module I = Ilambda
module L = Lambda

type proto_switch = {
  numconsts : int;
  consts : (int * L.lambda) list;
  failaction : L.lambda option;
}

let check_let_rec_bindings bindings =
  List.map (fun (binding : Lambda.lambda) ->
      match binding with
      | Lfunction func -> func
      | _ ->
        Misc.fatal_errorf "Only [Lfunction] expressions are permitted in \
            [Lletrec] bindings upon entry to CPS conversion: %a"
          Printlambda.lambda binding)
    bindings

let name_for_function (func : Lambda.lfunction) =
  (* Name anonymous functions by their source location, if known. *)
  if func.loc = Location.none then "anon-fn"
  else Format.asprintf "anon-fn[%a]" Location.print_compact func.loc

(* CR-soon mshinwell: Remove mutable state. *)
let static_exn_env = ref Numbers.Int.Map.empty
let recursive_static_catches = ref Numbers.Int.Set.empty

let rec cps_non_tail (lam : L.lambda) (k : Ident.t -> Ilambda.t)
          (k_exn : Continuation.t) : Ilambda.t =
  match lam with
  | Lvar id -> k id
  | Lconst _ -> name_then_cps_non_tail "const" lam k k_exn
  | Lapply { ap_func; ap_args; ap_loc; ap_should_be_tailcall; ap_inlined;
      ap_specialised; } ->
    cps_non_tail_list ap_args (fun args ->
      cps_non_tail ap_func (fun func ->
        let continuation = Continuation.create () in
        let result_var = Ident.create_local "apply_result" in
        let after = k result_var in
        let exn_continuation : I.exn_continuation =
          { exn_handler = k_exn;
            extra_args = [];
          }
        in
        let apply : Ilambda.apply = {
          kind = Function;
          func;
          continuation;
          exn_continuation;
          args;
          loc = ap_loc;
          should_be_tailcall = ap_should_be_tailcall;
          inlined = ap_inlined;
          specialised = ap_specialised;
        } in
        I.Let_cont {
          name = continuation;
          is_exn_handler = false;
          params = [result_var, Pgenval];
          recursive = Nonrecursive;
          body = Apply apply;
          handler = after;
        }) k_exn)
      k_exn
  | Lfunction func ->
    name_then_cps_non_tail (name_for_function func) lam k k_exn
  | Llet (Variable, value_kind, id, defining_expr, body) ->
    let temp_id = Ident.create_local "let_mutable" in
    let body = cps_non_tail body k k_exn in
    let after_defining_expr = Continuation.create () in
    let defining_expr = cps_tail defining_expr after_defining_expr k_exn in
    let let_mutable : I.let_mutable =
      { id;
        initial_value = temp_id;
        contents_kind = value_kind;
        body;
      }
    in
    Let_cont {
      name = after_defining_expr;
      is_exn_handler = false;
      params = [temp_id, value_kind];
      recursive = Nonrecursive;
      body = defining_expr;
      handler = Let_mutable let_mutable;
    }
  | Llet (_let_kind, value_kind, id, defining_expr, body) ->
    let body = cps_non_tail body k k_exn in
    let after_defining_expr = Continuation.create () in
    let defining_expr = cps_tail defining_expr after_defining_expr k_exn in
    Let_cont {
      name = after_defining_expr;
      is_exn_handler = false;
      params = [id, value_kind];
      recursive = Nonrecursive;
      body = defining_expr;
      handler = body;
    }
  | Lletrec (bindings, body) ->
    let idents, bindings = List.split bindings in
    let bindings = List.map cps_function (check_let_rec_bindings bindings) in
    let body = cps_non_tail body k k_exn in
    Let_rec (List.combine idents bindings, body)
  | Lprim (prim, args, loc) ->
    let name = Printlambda.name_of_primitive prim in
    let result_var = Ident.create_local name in
    let exn_continuation : I.exn_continuation option =
      if L.primitive_can_raise prim then
        Some {
          exn_handler = k_exn;
          extra_args = [];
        }
      else None
    in
    cps_non_tail_list args (fun args ->
      I.Let (result_var,
             Pgenval,
             Prim { prim; args; loc; exn_continuation; },
             k result_var))
      k_exn
  | Lswitch (scrutinee,
      { sw_numconsts; sw_consts; sw_numblocks = _; sw_blocks; sw_failaction; },
      _loc) ->
    begin match sw_blocks with
    | [] -> ()
    | _ -> Misc.fatal_error "Lswitch `block' cases are forbidden"
    end;
    let after_switch = Continuation.create () in
    let result_var = Ident.create_local "switch_result" in
    let after = k result_var in
    let proto_switch : proto_switch =
      { numconsts = sw_numconsts;
        consts = sw_consts;
        failaction = sw_failaction;
      }
    in
    let body = cps_switch proto_switch ~scrutinee after_switch k_exn in
    Let_cont {
      name = after_switch;
      is_exn_handler = false;
      params = [result_var, Pgenval];
      recursive = Nonrecursive;
      body;
      handler = after;
    }
  | Lstringswitch _ ->
    Misc.fatal_error "Lstringswitch must be expanded prior to CPS conversion"
  | Lstaticraise (static_exn, args) ->
    let continuation =
      match Numbers.Int.Map.find static_exn !static_exn_env with
      | exception Not_found ->
        Misc.fatal_errorf "Unbound static exception %d" static_exn
      | continuation -> continuation
    in
    cps_non_tail_list args
      (fun args -> I.Apply_cont (continuation, args)) k_exn
  | Lstaticcatch (body, (static_exn, args), handler) ->
    let continuation = Continuation.create () in
    static_exn_env := Numbers.Int.Map.add static_exn continuation
      !static_exn_env;
    let after_continuation = Continuation.create () in
    let result_var = Ident.create_local "staticcatch_result" in
    let body = cps_tail body after_continuation k_exn in
    let handler = cps_tail handler after_continuation k_exn in
    let recursive : Asttypes.rec_flag =
      if Numbers.Int.Set.mem static_exn !recursive_static_catches then
        Recursive
      else
        Nonrecursive
    in
    Let_cont {
      name = after_continuation;
      is_exn_handler = false;
      params = [result_var, Pgenval];
      recursive = Nonrecursive;
      body =
        Let_cont {
          name = continuation;
          is_exn_handler = false;
          params = args;
          recursive;
          body;
          handler;
        };
      handler = k result_var;
    }
  | Lsend (meth_kind, meth, obj, args, loc) ->
    cps_non_tail obj (fun obj ->
      cps_non_tail meth (fun meth ->
        cps_non_tail_list args (fun args ->
          let continuation = Continuation.create () in
          let result_var = Ident.create_local "send_result" in
          let after = k result_var in
          let exn_continuation : I.exn_continuation =
            { exn_handler = k_exn;
              extra_args = [];
            }
          in
          let apply : Ilambda.apply = {
            kind = Method { kind = meth_kind; obj; };
            func = meth;
            continuation;
            exn_continuation;
            args;
            loc;
            should_be_tailcall = false;
            inlined = Default_inline;
            specialised = Default_specialise;
          } in
          I.Let_cont {
            name = continuation;
            is_exn_handler = false;
            params = [result_var, Pgenval];
            recursive = Nonrecursive;
            body = Apply apply;
            handler = after;
          }) k_exn) k_exn) k_exn
  | Ltrywith (body, id, handler) ->
    let handler_continuation = Continuation.create () in
    let after_continuation = Continuation.create () in
    let result_var = Ident.create_local "try_with_result" in
    let body = cps_tail body after_continuation handler_continuation in
    let handler = cps_tail handler after_continuation k_exn in
    Let_cont {
      name = after_continuation;
      is_exn_handler = false;
      params = [result_var, Pgenval];
      recursive = Nonrecursive;
      body =
        Let_cont {
          name = handler_continuation;
          is_exn_handler = true;
          params = [id, Pgenval];
          recursive = Nonrecursive;
          body;
          handler;
        };
      handler = k result_var;
    }
  | Lassign _ -> name_then_cps_non_tail "assign" lam k k_exn
  | Lsequence _ | Lifthenelse _ | Lwhile _ | Lfor _ | Lifused _ | Levent _ ->
    Misc.fatal_errorf "Term should have been eliminated by [Prepare_lambda]: %a"
      Printlambda.lambda lam

and cps_tail (lam : L.lambda) (k : Continuation.t) (k_exn : Continuation.t)
      : Ilambda.t =
  match lam with
  | Lvar id -> Apply_cont (k, [id])
  | Lconst _ -> name_then_cps_tail "const" lam k k_exn
  | Lapply apply ->
    cps_non_tail_list apply.ap_args (fun args ->
      cps_non_tail apply.ap_func (fun func ->
        let exn_continuation : I.exn_continuation =
          { exn_handler = k_exn;
            extra_args = [];
          }
        in
        let apply : I.apply = {
          kind = Function;
          func;
          continuation = k;
          exn_continuation;
          args;
          loc = apply.ap_loc;
          should_be_tailcall = apply.ap_should_be_tailcall;
          inlined = apply.ap_inlined;
          specialised = apply.ap_specialised;
        } in
        I.Apply apply) k_exn) k_exn
  | Lfunction func -> name_then_cps_tail (name_for_function func) lam k k_exn
  | Llet (Variable, value_kind, id, defining_expr, body) ->
    let temp_id = Ident.create_local "let_mutable" in
    let body = cps_tail body k k_exn in
    let after_defining_expr = Continuation.create () in
    let defining_expr =
      cps_tail defining_expr after_defining_expr k_exn
    in
    let let_mutable : I.let_mutable =
      { id;
        initial_value = temp_id;
        contents_kind = value_kind;
        body;
      }
    in
    Let_cont {
      name = after_defining_expr;
      is_exn_handler = false;
      params = [temp_id, value_kind];
      recursive = Nonrecursive;
      body = defining_expr;
      handler = Let_mutable let_mutable;
    }
  | Llet (_let_kind, value_kind, id, defining_expr, body) ->
    let body = cps_tail body k k_exn in
    let after_defining_expr = Continuation.create () in
    let defining_expr = cps_tail defining_expr after_defining_expr k_exn in
    Let_cont {
      name = after_defining_expr;
      is_exn_handler = false;
      params = [id, value_kind];
      recursive = Nonrecursive;
      body = defining_expr;
      handler = body;
    }
  | Lletrec (bindings, body) ->
    let idents, bindings = List.split bindings in
    let bindings = List.map cps_function (check_let_rec_bindings bindings) in
    let body = cps_tail body k k_exn in
    Let_rec (List.combine idents bindings, body)
  | Lprim (prim, args, loc) ->
    (* CR mshinwell: Arrange for "args" to be named. *)
    let name = Printlambda.name_of_primitive prim in
    let result_var = Ident.create_local name in
    let exn_continuation : I.exn_continuation option =
      if L.primitive_can_raise prim then
        Some {
          exn_handler = k_exn;
          extra_args = [];
        }
      else None
    in
    cps_non_tail_list args (fun args ->
      I.Let (result_var, Pgenval,
        Prim { prim; args; loc; exn_continuation; },
        Apply_cont (k, [result_var]))) k_exn
  | Lswitch (scrutinee,
      { sw_numconsts; sw_consts; sw_numblocks = _; sw_blocks; sw_failaction; },
      _loc) ->
    begin match sw_blocks with
    | [] -> ()
    | _ -> Misc.fatal_error "Lswitch `block' cases are forbidden"
    end;
    let proto_switch : proto_switch =
      { numconsts = sw_numconsts;
        consts = sw_consts;
        failaction = sw_failaction;
      }
    in
    cps_switch proto_switch ~scrutinee k k_exn
  | Lstringswitch _ ->
    Misc.fatal_error "Lstringswitch must be expanded prior to CPS conversion"
  | Lstaticraise (static_exn, args) ->
    let continuation =
      match Numbers.Int.Map.find static_exn !static_exn_env with
      | exception Not_found ->
        Misc.fatal_errorf "Unbound static exception %d" static_exn
      | continuation -> continuation
    in
    cps_non_tail_list args
      (fun args -> I.Apply_cont (continuation, args)) k_exn
  | Lstaticcatch (body, (static_exn, args), handler) ->
    let continuation = Continuation.create () in
    static_exn_env := Numbers.Int.Map.add static_exn continuation
      !static_exn_env;
    let body = cps_tail body k k_exn in
    let handler = cps_tail handler k k_exn in
    let recursive : Asttypes.rec_flag =
      if Numbers.Int.Set.mem static_exn !recursive_static_catches then
        Recursive
      else
        Nonrecursive
    in
    Let_cont {
      name = continuation;
      is_exn_handler = false;
      params = args;
      recursive;
      body;
      handler;
    }
  | Lsend (meth_kind, meth, obj, args, loc) ->
    cps_non_tail obj (fun obj ->
      cps_non_tail meth (fun meth ->
        cps_non_tail_list args (fun args ->
          let exn_continuation : I.exn_continuation =
            { exn_handler = k_exn;
              extra_args = [];
            }
          in
          let apply : Ilambda.apply = {
            kind = Method { kind = meth_kind; obj; };
            func = meth;
            continuation = k;
            exn_continuation;
            args;
            loc;
            should_be_tailcall = false;
            inlined = Default_inline;
            specialised = Default_specialise;
          } in
          I.Apply apply) k_exn) k_exn) k_exn
  | Lassign _ -> name_then_cps_tail "assign" lam k k_exn
  | Ltrywith (body, id, handler) ->
    let handler_continuation = Continuation.create () in
    let body = cps_tail body k handler_continuation in
    let handler = cps_tail handler k k_exn in
    Let_cont {
      name = handler_continuation;
      is_exn_handler = true;
      params = [id, Pgenval];
      recursive = Nonrecursive;
      body;
      handler;
    }
  | Lsequence _ | Lifthenelse _ | Lwhile _ | Lfor _ | Lifused _ | Levent _ ->
    Misc.fatal_errorf "Term should have been eliminated by [Prepare_lambda]: %a"
      Printlambda.lambda lam

and name_then_cps_non_tail name lam k k_exn =
  let var = Ident.create_local name in
  cps_non_tail (L.Llet (Strict, Pgenval, var, lam, Lvar var)) k k_exn

and name_then_cps_tail name lam k k_exn =
  let var = Ident.create_local name in
  cps_tail (L.Llet (Strict, Pgenval, var, lam, Lvar var)) k k_exn

and cps_non_tail_list lams k k_exn =
  let lams = List.rev lams in  (* Always evaluate right-to-left. *)
  cps_non_tail_list_core lams k k_exn

and cps_non_tail_list_core (lams : L.lambda list)
      (k : Ident.t list -> Ilambda.t)
      (k_exn : Continuation.t) =
  match lams with
  | [] -> k []
  | lam::lams ->
    cps_non_tail lam (fun id ->
      cps_non_tail_list_core lams (fun ids -> k (ids @ [id])) k_exn)
      k_exn

and cps_function ({ kind; params; return; body; attr; loc; } : L.lfunction)
      : Ilambda.function_declaration =
  let body_cont = Continuation.create () in
  let body_exn_cont = Continuation.create () in
  let stub, body =
    match body with
    | Lprim (Pccall { prim_name; _ }, [body], _)
       when prim_name = Prepare_lambda.stub_hack_prim_name -> true, body
    | body -> false, body
  in
  let free_idents_of_body = Lambda.free_variables body in
  let body = cps_tail body body_cont body_exn_cont in
  let exn_continuation : I.exn_continuation =
    { exn_handler = body_exn_cont;
      extra_args = [];
    }
  in
  { kind = kind;
    continuation_param = body_cont;
    exn_continuation;
    params = params;
    return;
    body;
    free_idents_of_body;
    attr = attr;
    loc = loc;
    stub;
  }

and cps_switch (switch : proto_switch) ~scrutinee (k : Continuation.t)
      (k_exn : Continuation.t) : Ilambda.t =
  let const_nums, consts = List.split switch.consts in
  let const_conts = List.map (fun _ -> Continuation.create ()) consts in
  let consts = List.combine consts const_conts in
  let failaction_cont, failaction =
    match switch.failaction with
    | None -> None, None
    | Some failaction ->
      let cont = Continuation.create () in
      Some cont, Some (cont, failaction)
  in
  let switch : Ilambda.switch =
    { numconsts = switch.numconsts;
      consts = List.combine const_nums const_conts;
      failaction = failaction_cont;
    }
  in
  let make_continuations desc ~init =
    List.fold_right (fun (case, cont) acc ->
        let handler = cps_tail case k k_exn in
        I.Let_cont {
          name = cont;
          is_exn_handler = false;
          params = [];
          recursive = Nonrecursive;
          body = acc;
          handler;
        })
      desc
      init
  in
  cps_non_tail scrutinee (fun scrutinee ->
      let body = I.Switch (scrutinee, switch) in
      let init =
        match failaction with
        | None -> body
        | Some (cont, failaction) ->
          let handler = cps_tail failaction k k_exn in
          I.Let_cont {
            name = cont;
            is_exn_handler = false;
            params = [];
            recursive = Nonrecursive;
            body;
            handler;
          }
      in
      make_continuations consts ~init)
    k_exn

let lambda_to_ilambda lam ~recursive_static_catches:recursive_static_catches'
      : Ilambda.program =
  static_exn_env := Numbers.Int.Map.empty;
  recursive_static_catches := recursive_static_catches';
  let the_end = Continuation.create () in
  let the_end_exn = Continuation.create () in
  let ilam = cps_tail lam the_end the_end_exn in
  let exn_continuation : I.exn_continuation =
    { exn_handler = the_end_exn;
      extra_args = [];
    }
  in
  { expr = ilam;
    return_continuation = the_end;
    exn_continuation;
  }
