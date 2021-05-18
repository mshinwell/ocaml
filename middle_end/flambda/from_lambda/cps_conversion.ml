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
module C = Lambda_conversions

module Env : sig
  type t

  val create : current_unit_id:Ident.t -> t

  val current_unit_id : t -> Ident.t

  val add_mutable : t -> Ident.t -> t
  val is_mutable : t -> Ident.t -> bool
  val has_mutables : t -> bool

  (* val add_continuation : t -> int -> t *)
end = struct
  type t = {
    current_unit_id : Ident.t;
    mutables : Ident.Set.t;
    has_mutables : bool ref; (* for now *)
  }

  let create ~current_unit_id =
    { current_unit_id;
      mutables = Ident.Set.empty;
      has_mutables = ref false;
    }

  let current_unit_id t = t.current_unit_id

  let add_mutable t id =
    assert (not (Ident.Set.mem id t.mutables));
    t.has_mutables := true;
    { t with mutables = Ident.Set.add id t.mutables; }

  let is_mutable t id =
    Ident.Set.mem id t.mutables

  let has_mutables t =
    !(t.has_mutables)
end

type primitive_transform_result =
  | Primitive of L.primitive * L.lambda list * L.scoped_location
  | Transformed of L.lambda

let name_for_function (func : Lambda.lfunction) =
  (* Name anonymous functions by their source location, if known. *)
  match func.loc with
  | Loc_unknown -> "anon-fn"
  | Loc_known { loc; _ } ->
    Format.asprintf "anon-fn[%a]" Location.print_compact loc

(* CR-soon mshinwell: Remove mutable state. *)
let static_exn_env = ref Numbers.Int.Map.empty
let try_stack = ref []
let try_stack_at_handler = ref Continuation.Map.empty
let recursive_static_catches = ref Numbers.Int.Set.empty

let mark_as_recursive_static_catch cont =
  if Numbers.Int.Set.mem cont !recursive_static_catches then begin
    Misc.fatal_errorf "Static catch with continuation %d already marked as \
        recursive -- is it being redefined?"
      cont
  end;
  recursive_static_catches := Numbers.Int.Set.add cont !recursive_static_catches

let _print_stack ppf stack =
  Format.fprintf ppf "%a"
    (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
      (fun ppf (_id, cont) -> Format.fprintf ppf "%a" Continuation.print cont))
    stack

(* Uses of [Lstaticfail] that jump out of try-with handlers need special care:
   the correct number of pop trap operations must be inserted. *)
let compile_staticfail ~(continuation : Continuation.t) ~args =
  let try_stack_at_handler =
    match Continuation.Map.find continuation !try_stack_at_handler with
    | exception Not_found ->
      Misc.fatal_errorf "No try stack recorded for handler %a"
        Continuation.print continuation
    | stack -> stack
  in
  let try_stack_now = !try_stack in
  if List.length try_stack_at_handler > List.length try_stack_now then begin
    Misc.fatal_errorf "Cannot jump to continuation %a: it would involve \
        jumping into a try-with body"
      Continuation.print continuation
  end;
  assert (Continuation.Set.subset
    (Continuation.Set.of_list try_stack_at_handler)
    (Continuation.Set.of_list try_stack_now));
  let rec add_pop_traps ~try_stack_now ~try_stack_at_handler =
    let add_pop cont ~try_stack_now after_pop =
      let mk_remaining_traps =
        add_pop_traps ~try_stack_now ~try_stack_at_handler
      in
      let wrapper_cont = Continuation.create () in
      let trap_action : I.trap_action =
        Pop { exn_handler = cont; }
      in
      let body = I.Apply_cont (wrapper_cont, Some trap_action, []) in
      I.Let_cont {
        name = wrapper_cont;
        is_exn_handler = false;
        params = [];
        recursive = Nonrecursive;
        body;
        handler = mk_remaining_traps after_pop;
      }
    in
    let no_pop after_pop = after_pop in
    match try_stack_now, try_stack_at_handler with
    | [], [] -> no_pop
    | cont1 :: try_stack_now, cont2 :: _ ->
      if Continuation.equal cont1 cont2 then no_pop
      else add_pop cont1 ~try_stack_now
    | cont :: try_stack_now, [] -> add_pop cont ~try_stack_now
    | [], _ :: _ -> assert false  (* see above *)
  in
  let mk_poptraps =
    add_pop_traps ~try_stack_now ~try_stack_at_handler
  in
  mk_poptraps (I.Apply_cont (continuation, None, args))

let switch_for_if_then_else ~cond ~ifso ~ifnot =
  (* CR mshinwell: We need to make sure that [cond] is {0, 1}-valued.
     The frontend should have been fixed on this branch for this. *)
  let switch : Lambda.lambda_switch =
    { sw_numconsts = 2;
      sw_consts = [0, ifnot; 1, ifso];
      sw_numblocks = 0;
      sw_blocks = [];
      sw_failaction = None;
    }
  in
  L.Lswitch (cond, switch, Loc_unknown)

let transform_primitive env (prim : L.primitive) args loc =
  match prim, args with
  | Psequor, [arg1; arg2] ->
    let const_true = Ident.create_local "const_true" in
    let cond = Ident.create_local "cond_sequor" in
    Transformed
      (L.Llet (Strict, Pgenval, const_true, Lconst (Const_base (Const_int 1)),
        (L.Llet (Strict, Pgenval, cond, arg1,
          switch_for_if_then_else
            ~cond:(L.Lvar cond)
            ~ifso:(L.Lvar const_true)
            ~ifnot:arg2))))
  | Psequand, [arg1; arg2] ->
    let const_false = Ident.create_local "const_false" in
    let cond = Ident.create_local "cond_sequand" in
    Transformed
      (L.Llet (Strict, Pgenval, const_false, Lconst (Const_base (Const_int 0)),
        (L.Llet (Strict, Pgenval, cond, arg1,
          switch_for_if_then_else
            ~cond:(L.Lvar cond)
            ~ifso:arg2
            ~ifnot:(L.Lvar const_false)))))
  | (Psequand | Psequor), _ ->
    Misc.fatal_error "Psequand / Psequor must have exactly two arguments"
  | (Pidentity | Pbytes_to_string | Pbytes_of_string), [arg] -> Transformed arg
  | Pignore, [arg] ->
    let ident = Ident.create_local "ignore" in
    let result = L.Lconst (Const_base (Const_int 0)) in
    Transformed (L.Llet (Strict, Pgenval, ident, arg, result))
  | Pdirapply, [funct; arg]
  | Prevapply, [arg; funct] ->
    let apply : L.lambda_apply =
      { ap_func = funct;
        ap_args = [arg];
        ap_loc = loc;
        ap_should_be_tailcall = false;
        (* CR-someday lwhite: it would be nice to be able to give
           inlined attributes to functions applied with the application
           operators. *)
        ap_inlined = Default_inline;
        ap_specialised = Default_specialise;
      }
    in
    Transformed (L.Lapply apply)
  | Pfield _, [L.Lprim (Pgetglobal id, [],_)]
      when Ident.same id (Env.current_unit_id env) ->
    Misc.fatal_error "[Pfield (Pgetglobal ...)] for the current compilation \
      unit is forbidden upon entry to the middle end"
  | Psetfield (_, _, _), [L.Lprim (Pgetglobal _, [], _); _] ->
    Misc.fatal_error "[Psetfield (Pgetglobal ...)] is \
      forbidden upon entry to the middle end"
  | Pfield ({ index; _ }, _), _ when index < 0 ->
    Misc.fatal_error "Pfield with negative field index"
  | Pfloatfield (i, _), _ when i < 0 ->
    Misc.fatal_error "Pfloatfield with negative field index"
  | Psetfield ({ index; _ }, _, _), _ when index < 0 ->
    Misc.fatal_error "Psetfield with negative field index"
  | Pmakeblock (tag, _, _), _
      when tag < 0 || tag >= Obj.no_scan_tag ->
    Misc.fatal_errorf "Pmakeblock with wrong or non-scannable block tag %d" tag
  | Pmakefloatblock _mut, args when List.length args < 1 ->
    Misc.fatal_errorf "Pmakefloatblock must have at least one argument"
  | Pfloatcomp CFnlt, args ->
    Primitive (L.Pnot, [L.Lprim (Pfloatcomp CFlt, args, loc)], loc)
  | Pfloatcomp CFngt, args ->
    Primitive (L.Pnot, [L.Lprim (Pfloatcomp CFgt, args, loc)], loc)
  | Pfloatcomp CFnle, args ->
    Primitive (L.Pnot, [L.Lprim (Pfloatcomp CFle, args, loc)], loc)
  | Pfloatcomp CFnge, args ->
    Primitive (L.Pnot, [L.Lprim (Pfloatcomp CFge, args, loc)], loc)
  | Pbigarrayref (_unsafe, num_dimensions, kind, layout), args ->
    begin match C.convert_bigarray_kind kind,
                C.convert_bigarray_layout layout with
    | Some _, Some _ ->
      Primitive (prim, args, loc)
    | None, None | None, Some _ | Some _, None ->
      if 1 <= num_dimensions && num_dimensions <= 3 then begin
        let arity = 1 + num_dimensions in
        let name = "caml_ba_get_" ^ string_of_int num_dimensions in
        let desc = Primitive.simple ~name ~arity ~alloc:true in
        Primitive (L.Pccall desc, args, loc)
      end else begin
        Misc.fatal_errorf
          "Cps_conversion.transform_primitive: \
           Pbigarrayref with unknown layout and elements should only have \
           dimensions between 1 and 3 (see translprim)."
      end
    end
  | Pbigarrayset (_unsafe, num_dimensions, kind, layout), args ->
    begin match C.convert_bigarray_kind kind,
                C.convert_bigarray_layout layout with
    | Some _, Some _ ->
      Primitive (prim, args, loc)
    | None, None | None, Some _ | Some _, None ->
      if 1 <= num_dimensions && num_dimensions <= 3 then begin
        let arity = 2 + num_dimensions in
        let name = "caml_ba_set_" ^ string_of_int num_dimensions in
        let desc = Primitive.simple ~name ~arity ~alloc:true in
        Primitive (L.Pccall desc, args, loc)
      end else begin
        Misc.fatal_errorf
          "Cps_conversion.transform_primimive: \
           Pbigarrayset with unknown layout and elements should only have \
           dimensions between 1 and 3 (see translprim)."
      end
    end
  | _, _ -> Primitive (prim, args, loc)

let rec_catch_for_while_loop cond body =
  let cont = L.next_raise_count () in
  mark_as_recursive_static_catch cont;
  let cond_result = Ident.create_local "while_cond_result" in
  let lam : L.lambda =
    Lstaticcatch (
      Lstaticraise (cont, []),
      (cont, []),
      Llet (Strict, Pgenval, cond_result, cond,
        Lifthenelse (Lvar cond_result,
          Lsequence (
            body,
            Lstaticraise (cont, [])),
          Lconst (Const_base (Const_int 0)))))
  in lam

let rec_catch_for_for_loop
      ident start stop (dir : Asttypes.direction_flag) body =
  let cont = L.next_raise_count () in
  mark_as_recursive_static_catch cont;
  let start_ident = Ident.create_local "for_start" in
  let stop_ident = Ident.create_local "for_stop" in
  let first_test : L.lambda =
    match dir with
    | Upto ->
       Lprim (Pintcomp Cle,
         [L.Lvar start_ident; L.Lvar stop_ident],
         Loc_unknown)
    | Downto ->
       Lprim (Pintcomp Cge,
         [L.Lvar start_ident; L.Lvar stop_ident],
         Loc_unknown)
  in
  let subsequent_test : L.lambda =
    Lprim (Pintcomp Cne, [L.Lvar ident; L.Lvar stop_ident], Loc_unknown)
  in
  let one : L.lambda = Lconst (Const_base (Const_int 1)) in
  let next_value_of_counter =
    match dir with
    | Upto -> L.Lprim (Paddint, [L.Lvar ident; one], Loc_unknown)
    | Downto -> L.Lprim (Psubint, [L.Lvar ident; one], Loc_unknown)
  in
  let lam : L.lambda =
    (* Care needs to be taken here not to cause overflow if, for an
       incrementing for-loop, the upper bound is [max_int]; likewise, for
       a decrementing for-loop, if the lower bound is [min_int]. *)
    Llet (Strict, Pgenval, start_ident, start,
      Llet (Strict, Pgenval, stop_ident, stop,
        Lifthenelse (first_test,
          Lstaticcatch (
            Lstaticraise (cont, [L.Lvar start_ident]),
            (cont, [ident, Pgenval]),
            Lsequence (
              body,
              Lifthenelse (subsequent_test,
                Lstaticraise (cont, [next_value_of_counter]),
                L.lambda_unit))),
          L.lambda_unit)))
  in lam

let rec cps_non_tail env (lam : L.lambda) (k : Ident.t -> Ilambda.t)
          (k_exn : Continuation.t) : Ilambda.t =
  match lam with
  | Lvar id ->
    if Env.is_mutable env id then
      name_then_cps_non_tail "mutable_read" (I.Mutable_read id) k k_exn
    else
      k id
  | Lconst const ->
    name_then_cps_non_tail "const" (I.Simple (Const const)) k k_exn
  | Lapply { ap_func; ap_args; ap_loc; ap_should_be_tailcall; ap_inlined;
      ap_specialised; } ->
    cps_non_tail_list env ap_args (fun args ->
      cps_non_tail env ap_func (fun func ->
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
          params = [result_var, I.Not_user_visible, Pgenval];
          recursive = Nonrecursive;
          body = Apply apply;
          handler = after;
        }) k_exn)
      k_exn
  | Lfunction func ->
    let id = Ident.create_local (name_for_function func) in
    let func = cps_function env ~stub:false func in
    let body = k id in
    Let_rec ([id, func], body)
  | Llet (Variable, value_kind, id, defining_expr, body) ->
    let env = Env.add_mutable env id in
    let temp_id = Ident.create_local "let_mutable" in
    let body = cps_non_tail env body k k_exn in
    let after_defining_expr = Continuation.create () in
    let defining_expr = cps_tail env defining_expr after_defining_expr k_exn in
    let let_mutable : I.let_mutable =
      { id;
        initial_value = Var temp_id;
        contents_kind = value_kind;
        body;
      }
    in
    Let_cont {
      name = after_defining_expr;
      is_exn_handler = false;
      params = [temp_id, I.Not_user_visible, value_kind];
      recursive = Nonrecursive;
      body = defining_expr;
      handler = Let_mutable let_mutable;
    }
  | Llet ((Strict | Alias | StrictOpt), Pgenval, fun_id,
      Lfunction func, body) ->
    (* This case is here to get function names right. *)
    let bindings = cps_function_bindings env [fun_id, L.Lfunction func] in
    let body = cps_non_tail env body k k_exn in
    List.fold_left (fun body (fun_id, func) ->
        I.Let_rec ([fun_id, func], body))
      body bindings
  | Llet (_, value_kind, id, Lconst const, body) ->
    (* This case avoids extraneous continuations. *)
    let body = cps_non_tail env body k k_exn in
    I.Let (id, User_visible, value_kind, Simple (Const const), body)
  | Llet (let_kind, value_kind, id, Lprim (prim, args, loc), body) ->
    begin match transform_primitive env prim args loc with
    | Primitive (prim, args, loc) ->
      (* This case avoids extraneous continuations. *)
      let exn_continuation : I.exn_continuation option =
        if L.primitive_can_raise prim then
          Some {
            exn_handler = k_exn;
            extra_args = [];
          }
        else None
      in
      cps_non_tail_list env args (fun args ->
          let body = cps_non_tail env body k k_exn in
          I.Let (id, User_visible, value_kind,
            Prim { prim; args; loc; exn_continuation; },
            body))
        k_exn
    | Transformed lam ->
      cps_non_tail env (L.Llet (let_kind, value_kind, id, lam, body)) k k_exn
    end
  | Llet (_let_kind, value_kind, id, defining_expr, body) ->
    let body = cps_non_tail env body k k_exn in
    let after_defining_expr = Continuation.create () in
    let defining_expr = cps_tail env defining_expr after_defining_expr k_exn in
    Let_cont {
      name = after_defining_expr;
      is_exn_handler = false;
      params = [id, I.User_visible, value_kind];
      recursive = Nonrecursive;
      body = defining_expr;
      handler = body;
    }
  | Lletrec (bindings, body) ->
    let bindings = cps_function_bindings env bindings in
    let body = cps_non_tail env body k k_exn in
    Let_rec (bindings, body)
  | Lprim (prim, args, loc) ->
    begin match transform_primitive env prim args loc with
    | Primitive (prim, args, loc) ->
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
      cps_non_tail_list env args (fun args ->
          I.Let (result_var,
            Not_user_visible,
            Pgenval,
            Prim { prim; args; loc; exn_continuation; },
            k result_var))
        k_exn
    | Transformed lam -> cps_non_tail env lam k k_exn
    end
  | Lswitch (scrutinee, switch, _loc) ->
    let after_switch = Continuation.create () in
    let result_var = Ident.create_local "switch_result" in
    let after = k result_var in
    let body = cps_switch env switch ~scrutinee after_switch k_exn in
    Let_cont {
      name = after_switch;
      is_exn_handler = false;
      params = [result_var, I.Not_user_visible, Pgenval];
      recursive = Nonrecursive;
      body;
      handler = after;
    }
  | Lstringswitch (scrutinee, cases, default, loc) ->
    cps_non_tail env (Matching.expand_stringswitch loc scrutinee cases default)
      k k_exn
  | Lstaticraise (static_exn, args) ->
    let continuation =
      match Numbers.Int.Map.find static_exn !static_exn_env with
      | exception Not_found ->
        Misc.fatal_errorf "Unbound static exception %d" static_exn
      | continuation -> continuation
    in
    cps_non_tail_list env args
      (fun args -> compile_staticfail ~continuation ~args) k_exn
  | Lstaticcatch (body, (static_exn, args), handler) ->
    let continuation = Continuation.create () in
    static_exn_env := Numbers.Int.Map.add static_exn continuation
      !static_exn_env;
    try_stack_at_handler := Continuation.Map.add continuation !try_stack
      !try_stack_at_handler;
    let after_continuation = Continuation.create () in
    let result_var = Ident.create_local "staticcatch_result" in
    let body = cps_tail env body after_continuation k_exn in
    let handler = cps_tail env handler after_continuation k_exn in
    let recursive : Asttypes.rec_flag =
      if Numbers.Int.Set.mem static_exn !recursive_static_catches then
        Recursive
      else
        Nonrecursive
    in
    Let_cont {
      name = after_continuation;
      is_exn_handler = false;
      params = [result_var, I.Not_user_visible, Pgenval];
      recursive = Nonrecursive;
      body =
        Let_cont {
          name = continuation;
          is_exn_handler = false;
          params = List.map (fun (arg, kind) -> arg, I.User_visible, kind) args;
          recursive;
          body;
          handler;
        };
      handler = k result_var;
    };
  | Lsend (meth_kind, meth, obj, args, loc) ->
    cps_non_tail_simple env obj (fun obj ->
      cps_non_tail env meth (fun meth ->
        cps_non_tail_list env args (fun args ->
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
            params = [result_var, Not_user_visible, Pgenval];
            recursive = Nonrecursive;
            body = Apply apply;
            handler = after;
          }) k_exn) k_exn) k_exn
  | Ltrywith (body, id, handler) ->
    let body_result = Ident.create_local "body_result" in
    let result_var = Ident.create_local "try_with_result" in
    let body_continuation = Continuation.create () in
    let handler_continuation = Continuation.create () in
    let poptrap_continuation = Continuation.create () in
    let after_continuation = Continuation.create () in
    let old_try_stack = !try_stack in
    try_stack := handler_continuation :: old_try_stack;
    let body =
      cps_tail env body poptrap_continuation handler_continuation
    in
    try_stack := old_try_stack;
    let handler = cps_tail env handler after_continuation k_exn in
    Let_cont {
      name = after_continuation;
      is_exn_handler = false;
      params = [result_var, Not_user_visible, Pgenval];
      recursive = Nonrecursive;
      body =
        Let_cont {
          name = handler_continuation;
          is_exn_handler = true;
          params = [id, User_visible, Pgenval];
          recursive = Nonrecursive;
          body =
            Let_cont {
              name = poptrap_continuation;
              is_exn_handler = false;
              params = [body_result, Not_user_visible, Pgenval];
              recursive = Nonrecursive;
              body =
                Let_cont {
                  name = body_continuation;
                  is_exn_handler = false;
                  params = [];
                  recursive = Nonrecursive;
                  body =
                    Apply_cont (body_continuation,
                      Some (I.Push {
                        exn_handler = handler_continuation;
                      }),
                      []);
                  handler = body;
                };
              handler = Apply_cont (after_continuation,
                Some (I.Pop { exn_handler = handler_continuation; }),
                [Ilambda.Var body_result]);
            };
          handler;
        };
      handler = k result_var;
    }
  | Lifthenelse (cond, ifso, ifnot) ->
    let lam = switch_for_if_then_else ~cond ~ifso ~ifnot in
    cps_non_tail env lam k k_exn
  | Lsequence (lam1, lam2) ->
    let ident = Ident.create_local "sequence" in
    cps_non_tail env (L.Llet (Strict, Pgenval, ident, lam1, lam2)) k k_exn
  | Lwhile (cond, body) ->
    let loop = rec_catch_for_while_loop cond body in
    cps_non_tail env loop k k_exn
  | Lfor (ident, start, stop, dir, body) ->
    let loop = rec_catch_for_for_loop ident start stop dir body in
    cps_non_tail env loop k k_exn
  | Lassign (being_assigned, new_value) ->
    if not (Env.is_mutable env being_assigned) then begin
      Misc.fatal_errorf "Lassign on non-mutable variable %a"
        Ident.print being_assigned
    end;
    cps_non_tail_simple env new_value (fun new_value ->
        name_then_cps_non_tail "assign"
          (I.Assign { being_assigned; new_value; })
          k k_exn)
      k_exn
  | Levent (body, _event) -> cps_non_tail env body k k_exn
  | Lifused _ ->
    (* [Lifused] is used to mark that this expression should be alive only if
       an identifier is.  Every use should have been removed by
       [Simplif.simplify_lets], either by replacing by the inner expression,
       or by completely removing it (replacing by unit). *)
    Misc.fatal_error "[Lifused] should have been removed by \
        [Simplif.simplify_lets]"

and cps_non_tail_simple env (lam : L.lambda) (k : Ilambda.simple -> Ilambda.t)
      (k_exn : Continuation.t) : Ilambda.t =
  match lam with
  | Lvar id when not (Env.is_mutable env id) -> k (Ilambda.Var id)
  | Lconst const -> k (Ilambda.Const const)
  | Lvar _ (* mutable read *)
  | Lapply _
  | Lfunction _
  | Llet _
  | Lletrec _
  | Lprim _
  | Lswitch _
  | Lstringswitch _
  | Lstaticraise _
  | Lstaticcatch _
  | Ltrywith _
  | Lifthenelse _
  | Lsequence _
  | Lwhile _
  | Lfor _
  | Lassign _
  | Lsend _
  | Levent _
  | Lifused _ -> cps_non_tail env lam (fun id -> k (Ilambda.Var id)) k_exn

and cps_tail env (lam : L.lambda) (k : Continuation.t) (k_exn : Continuation.t)
      : Ilambda.t =
  match lam with
  | Lvar id ->
    if Env.is_mutable env id then
      name_then_cps_tail "mutable_read" (I.Mutable_read id) k k_exn
    else
      Apply_cont (k, None, [Ilambda.Var id])
  | Lconst const ->
    name_then_cps_tail "const" (I.Simple (Const const)) k k_exn
  | Lapply apply ->
    cps_non_tail_list env apply.ap_args (fun args ->
      cps_non_tail env apply.ap_func (fun func ->
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
  | Lfunction func ->
    let id = Ident.create_local (name_for_function func) in
    let func = cps_function env ~stub:false func in
    Let_rec ([id, func], Apply_cont (k, None, [Ilambda.Var id]))
  | Llet (Variable, value_kind, id, defining_expr, body) ->
    let env = Env.add_mutable env id in
    let temp_id = Ident.create_local "let_mutable" in
    let body = cps_tail env body k k_exn in
    let after_defining_expr = Continuation.create () in
    let defining_expr =
      cps_tail env defining_expr after_defining_expr k_exn
    in
    let let_mutable : I.let_mutable =
      { id;
        initial_value = I.Var temp_id;
        contents_kind = value_kind;
        body;
      }
    in
    Let_cont {
      name = after_defining_expr;
      is_exn_handler = false;
      params = [temp_id, Not_user_visible, value_kind];
      recursive = Nonrecursive;
      body = defining_expr;
      handler = Let_mutable let_mutable;
    }
  | Llet ((Strict | Alias | StrictOpt), Pgenval, fun_id,
      Lfunction func, body) ->
    (* This case is here to get function names right. *)
    let bindings = cps_function_bindings env [fun_id, L.Lfunction func] in
    let body = cps_tail env body k k_exn in
    List.fold_left (fun body (fun_id, func) ->
        I.Let_rec ([fun_id, func], body))
      body bindings
  | Llet (_, value_kind, id, Lconst const, body) ->
    (* This case avoids extraneous continuations. *)
    let body = cps_tail env body k k_exn in
    I.Let (id, User_visible, value_kind, Simple (Const const), body)
  | Llet (let_kind, value_kind, id, Lprim (prim, args, loc), body) ->
    begin match transform_primitive env prim args loc with
    | Primitive (prim, args, loc) ->
      (* This case avoids extraneous continuations. *)
      let exn_continuation : I.exn_continuation option =
        if L.primitive_can_raise prim then
          Some {
            exn_handler = k_exn;
            extra_args = [];
          }
        else None
      in
      cps_non_tail_list env args (fun args ->
          let body = cps_tail env body k k_exn in
          I.Let (id, User_visible, value_kind,
            Prim { prim; args; loc; exn_continuation; },
            body))
        k_exn
    | Transformed lam ->
       cps_tail env (L.Llet (let_kind, value_kind, id, lam, body)) k k_exn
    end
  | Llet (_let_kind, _value_kind, id, Lassign (being_assigned, new_value),
      body) ->
    (* This case is also to avoid extraneous continuations in code that
       relies on the ref-conversion optimisation. *)
    if not (Env.is_mutable env being_assigned) then begin
      Misc.fatal_errorf "Lassign on non-mutable variable %a"
        Ident.print being_assigned
    end;
    cps_non_tail_simple env new_value (fun new_value ->
        let body = cps_tail env body k k_exn in
        I.Let (id, User_visible, Pgenval,
          I.Assign { being_assigned; new_value; },
          body))
      k_exn
  | Llet (_let_kind, value_kind, id, defining_expr, body) ->
    let body = cps_tail env body k k_exn in
    let after_defining_expr = Continuation.create () in
    let defining_expr = cps_tail env defining_expr after_defining_expr k_exn in
    Let_cont {
      name = after_defining_expr;
      is_exn_handler = false;
      params = [id, User_visible, value_kind];
      recursive = Nonrecursive;
      body = defining_expr;
      handler = body;
    }
  | Lletrec (bindings, body) ->
    let bindings = cps_function_bindings env bindings in
    let body = cps_tail env body k k_exn in
    Let_rec (bindings, body)
  | Lprim (prim, args, loc) ->
    begin match transform_primitive env prim args loc with
    | Primitive (prim, args, loc) ->
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
      cps_non_tail_list env args (fun args ->
          I.Let (result_var, Not_user_visible, Pgenval,
            Prim { prim; args; loc; exn_continuation; },
            Apply_cont (k, None, [Ilambda.Var result_var]))) k_exn
    | Transformed lam -> cps_tail env lam k k_exn
    end
  | Lswitch (scrutinee,switch, _loc) ->
    cps_switch env switch ~scrutinee k k_exn
  | Lstringswitch (scrutinee, cases, default, loc) ->
    cps_tail env (Matching.expand_stringswitch loc scrutinee cases default)
      k k_exn
  | Lstaticraise (static_exn, args) ->
    let continuation =
      match Numbers.Int.Map.find static_exn !static_exn_env with
      | exception Not_found ->
        Misc.fatal_errorf "Unbound static exception %d" static_exn
      | continuation -> continuation
    in
    cps_non_tail_list env args
      (fun args -> compile_staticfail ~continuation ~args) k_exn
  | Lstaticcatch (body, (static_exn, args), handler) ->
    let continuation = Continuation.create () in
    static_exn_env := Numbers.Int.Map.add static_exn continuation
      !static_exn_env;
    try_stack_at_handler := Continuation.Map.add continuation !try_stack
      !try_stack_at_handler;
    let body = cps_tail env body k k_exn in
    let handler = cps_tail env handler k k_exn in
    let recursive : Asttypes.rec_flag =
      if Numbers.Int.Set.mem static_exn !recursive_static_catches then
        Recursive
      else
        Nonrecursive
    in
    Let_cont {
      name = continuation;
      is_exn_handler = false;
      params = List.map (fun (arg, kind) -> arg, I.User_visible, kind) args;
      recursive;
      body;
      handler;
    }
  | Lsend (meth_kind, meth, obj, args, loc) ->
    cps_non_tail_simple env obj (fun obj ->
      cps_non_tail env meth (fun meth ->
        cps_non_tail_list env args (fun args ->
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
  | Lassign (being_assigned, new_value) ->
    if not (Env.is_mutable env being_assigned) then begin
      Misc.fatal_errorf "Lassign on non-mutable variable %a"
        Ident.print being_assigned
    end;
    cps_non_tail_simple env new_value (fun new_value ->
        name_then_cps_tail "assign" (I.Assign { being_assigned; new_value; })
        k k_exn)
      k_exn
  | Ltrywith (body, id, handler) ->
    let body_result = Ident.create_local "body_result" in
    let body_continuation = Continuation.create () in
    let handler_continuation = Continuation.create () in
    let poptrap_continuation = Continuation.create () in
    let old_try_stack = !try_stack in
    try_stack := handler_continuation :: old_try_stack;
    let body = cps_tail env body poptrap_continuation handler_continuation in
    try_stack := old_try_stack;
    let handler = cps_tail env handler k k_exn in
    Let_cont {
      name = handler_continuation;
      is_exn_handler = true;
      params = [id, User_visible, Pgenval];
      recursive = Nonrecursive;
      body =
        Let_cont {
          name = poptrap_continuation;
          is_exn_handler = false;
          params = [body_result, Not_user_visible, Pgenval];
          recursive = Nonrecursive;
          body =
            Let_cont {
              name = body_continuation;
              is_exn_handler = false;
              params = [];
              recursive = Nonrecursive;
              body =
                Apply_cont (body_continuation,
                  Some (I.Push { exn_handler = handler_continuation; }),
                  []);
              handler = body;
            };
          handler = Apply_cont (k, Some (
            I.Pop { exn_handler = handler_continuation; }),
            [Ilambda.Var body_result]);
        };
      handler;
    }
  | Lifthenelse (cond, ifso, ifnot) ->
    let lam = switch_for_if_then_else ~cond ~ifso ~ifnot in
    cps_tail env lam k k_exn
  | Lsequence (lam1, lam2) ->
    let ident = Ident.create_local "sequence" in
    cps_tail env (L.Llet (Strict, Pgenval, ident, lam1, lam2)) k k_exn
  | Lwhile (cond, body) ->
    let loop = rec_catch_for_while_loop cond body in
    cps_tail env loop k k_exn
  | Lfor (ident, start, stop, dir, body) ->
    let loop = rec_catch_for_for_loop ident start stop dir body in
    cps_tail env loop k k_exn
  | Levent (body, _event) -> cps_tail env body k k_exn
  | Lifused _ ->
    (* [Lifused] is used to mark that this expression should be alive only if
       an identifier is.  Every use should have been removed by
       [Simplif.simplify_lets], either by replacing by the inner expression,
       or by completely removing it (replacing by unit). *)
    Misc.fatal_error "[Lifused] should have been removed by \
        [Simplif.simplify_lets]"

and name_then_cps_non_tail name defining_expr k _k_exn : I.t =
  let id = Ident.create_local name in
  let body = k id in
  Let (id, Not_user_visible, Pgenval, defining_expr, body)

and name_then_cps_tail name defining_expr k _k_exn : I.t =
  let id = Ident.create_local name in
  Let (id, Not_user_visible, Pgenval, defining_expr,
    Apply_cont (k, None, [Ilambda.Var id]))

and cps_non_tail_list env lams k k_exn =
  let lams = List.rev lams in  (* Always evaluate right-to-left. *)
  cps_non_tail_list_core env lams (fun ids -> k (List.rev ids)) k_exn

and cps_non_tail_list_core env (lams : L.lambda list)
      (k : Ilambda.simple list -> Ilambda.t)
      (k_exn : Continuation.t) =
  match lams with
  | [] -> k []
  | lam::lams ->
    cps_non_tail_simple env lam (fun simple ->
      cps_non_tail_list_core env lams
        (fun simples -> k (simple :: simples)) k_exn)
      k_exn

and cps_function_bindings env (bindings : (Ident.t * L.lambda) list) =
  List.concat_map (fun (fun_id, binding) ->
      match binding with
      | L.Lfunction { kind; params; body = fbody; attr; loc; return; _ } ->
        begin match
          Simplif.split_default_wrapper ~id:fun_id ~kind ~params
            ~body:fbody ~return ~attr ~loc
        with
        | [fun_id, L.Lfunction def] ->
          [fun_id, cps_function env ~stub:false def]
        | [fun_id, L.Lfunction def; inner_id, L.Lfunction inner_def] ->
          [fun_id, cps_function env ~stub:false def;
           inner_id, cps_function env ~stub:true inner_def]
        | [_, _] | [_, _; _, _] ->
          Misc.fatal_errorf "Expected `Lfunction` terms from \
              [split_default_wrapper] when translating:@ %a"
            Printlambda.lambda binding
        | _ ->
          Misc.fatal_errorf "Unexpected return value from \
              [split_default_wrapper] when translating:@ %a"
            Printlambda.lambda binding
        end
      | _ ->
        Misc.fatal_errorf "Only [Lfunction] expressions are permitted in \
            function bindings upon entry to CPS conversion: %a"
          Printlambda.lambda binding)
    bindings

and cps_function env ~stub
      ({ kind; params; return; body; attr; loc; } : L.lfunction)
      : Ilambda.function_declaration =
  let body_cont = Continuation.create ~sort:Return () in
  let body_exn_cont = Continuation.create () in
  let free_idents_of_body = Lambda.free_variables body in
  let body = cps_tail env body body_cont body_exn_cont in
  let exn_continuation : I.exn_continuation =
    { exn_handler = body_exn_cont;
      extra_args = [];
    }
  in
  { kind = kind;
    return_continuation = body_cont;
    exn_continuation;
    params = params;
    return;
    body;
    free_idents_of_body;
    attr = attr;
    loc = loc;
    stub;
  }

and cps_switch env (switch : L.lambda_switch) ~scrutinee (k : Continuation.t)
      (k_exn : Continuation.t) : Ilambda.t =
  let block_nums, sw_blocks = List.split switch.sw_blocks in
  let block_nums =
    List.map (fun ({ sw_tag; _ } : L.lambda_switch_block_key) ->
        begin match Tag.Scannable.create sw_tag with
        | Some tag ->
          let tag' = Tag.Scannable.to_tag tag in
          if Tag.is_structured_block_but_not_a_variant tag' then
            Misc.fatal_errorf "Bad tag %a in [Lswitch] (tag is that of a \
              scannable block, but not one treated like a variant; \
              [Lswitch] can only be used for variant matching)"
              Tag.print tag'
        | None ->
           Misc.fatal_errorf "Bad tag %d in [Lswitch] (not the tag \
             of a GC-scannable block)"
             sw_tag
        end;
        sw_tag)
      block_nums
  in
  if switch.sw_numblocks > Obj.last_non_constant_constructor_tag + 1
  then begin
      Misc.fatal_errorf "Too many blocks (%d) in [Lswitch], would \
        overlap into tag space for blocks that are not treated like variants; \
        [Lswitch] can only be used for variant matching"
        switch.sw_numblocks
    end;
  let convert_arms_rev cases wrappers =
    List.fold_left (fun (consts_rev, wrappers) (arm, (action : L.lambda)) ->
        match action with
        | Lvar var when not (Env.is_mutable env var) ->
          let consts_rev = (arm, k, None, [Ilambda.Var var]) :: consts_rev in
          consts_rev, wrappers
        | Lconst cst ->
          let consts_rev = (arm, k, None, [Ilambda.Const cst]) :: consts_rev in
          consts_rev, wrappers
        | Lvar _ (* mutable *)
        | Lapply _
        | Lfunction _
        | Llet _
        | Lletrec _
        | Lprim _
        | Lswitch _
        | Lstringswitch _
        | Lstaticraise _
        | Lstaticcatch _
        | Ltrywith _
        | Lifthenelse _
        | Lsequence _
        | Lwhile _
        | Lfor _
        | Lassign _
        | Lsend _
        | Levent _
        | Lifused _ ->
          let action = cps_tail env action k k_exn in
          match action with
          | Apply_cont (cont, trap, args) ->
            let consts_rev = (arm, cont, trap, args) :: consts_rev in
            consts_rev, wrappers
          | Let _ | Let_mutable _ | Let_rec _ | Let_cont _ | Apply _
          | Switch _ ->
            let cont = Continuation.create () in
            let consts_rev = (arm, cont, None, []) :: consts_rev in
            let wrappers = (cont, action) :: wrappers in
            consts_rev, wrappers)
      ([], wrappers)
      cases
  in
  let consts_rev, wrappers = convert_arms_rev switch.sw_consts [] in
  let blocks_rev, wrappers =
    convert_arms_rev (List.combine block_nums sw_blocks) wrappers
  in
  let consts = List.rev consts_rev in
  let blocks = List.rev blocks_rev in
  let failaction, wrappers =
    match switch.sw_failaction with
    | None -> None, wrappers
    | Some action ->
      let cont = Continuation.create () in
      let action = cps_tail env action k k_exn in
      let wrappers = (cont, action) :: wrappers in
      Some (cont, None, []), wrappers
  in
  let const_switch : I.switch =
    { numconsts = switch.sw_numconsts;
      consts;
      failaction;
    }
  in
  let block_switch : I.switch =
    { numconsts = switch.sw_numblocks;
      consts = blocks;
      failaction;
    }
  in
  let build_switch scrutinee wrappers =
    let const_switch = I.Switch (scrutinee, const_switch) in
    let scrutinee_tag = Ident.create_local "scrutinee_tag" in
    let block_switch =
      I.Let (scrutinee_tag,
        Not_user_visible,
        Pintval,
        Prim { prim = Pgettag;
          args = [Var scrutinee];
          loc = Loc_unknown;
          exn_continuation = None; },
        I.Switch(scrutinee_tag, block_switch))
    in
    if switch.sw_numblocks = 0 then const_switch, wrappers
    else if switch.sw_numconsts = 0 then block_switch, wrappers
    else
      let const_cont = Continuation.create () in
      let block_cont = Continuation.create () in
      let isint_switch : I.switch =
        { numconsts = 2;
          consts = [ (0, block_cont, None, []); (1, const_cont, None, []) ];
          failaction = None;
        }
      in
      let is_scrutinee_int = Ident.create_local "is_scrutinee_int" in
      let isint_switch =
        I.Let (is_scrutinee_int,
          Not_user_visible,
          Pintval,
          Prim { prim = Pisint;
            args = [Var scrutinee];
            loc = Loc_unknown;
            exn_continuation = None; },
          I.Switch(is_scrutinee_int, isint_switch))
      in
      isint_switch,
      ((const_cont, const_switch)::(block_cont, block_switch)::wrappers)
  in
  cps_non_tail env scrutinee (fun scrutinee ->
      let switch, wrappers = build_switch scrutinee wrappers in
      List.fold_left (fun body (cont, action) ->
          I.Let_cont {
            name = cont;
            is_exn_handler = false;
            params = [];
            recursive = Nonrecursive;
            body;
            handler = action;
          })
        switch wrappers)
    k_exn

let lambda_to_ilambda lam : Ilambda.program =
  static_exn_env := Numbers.Int.Map.empty;
  try_stack := [];
  try_stack_at_handler := Continuation.Map.empty;
  recursive_static_catches := Numbers.Int.Set.empty;
  let current_unit_id =
    Compilation_unit.get_persistent_ident
      (Compilation_unit.get_current_exn ())
  in
  let env = Env.create ~current_unit_id in
  let the_end = Continuation.create ~sort:Define_root_symbol () in
  let the_end_exn = Continuation.create () in
  let ilam = cps_tail env lam the_end the_end_exn in
  let exn_continuation : I.exn_continuation =
    { exn_handler = the_end_exn;
      extra_args = [];
    }
  in
  { expr = ilam;
    return_continuation = the_end;
    exn_continuation;
    uses_mutable_variables = Env.has_mutables env;
  }
