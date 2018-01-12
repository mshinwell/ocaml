(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module B = Inlining_cost.Benefit
module E = Simplify_env_and_result.Env
module R = Simplify_env_and_result.Result
module S = Simplify_simple
module T = Flambda_type

module Expr = Flambda.Expr
module Named = Flambda.Named
module Typed_parameter = Flambda.Typed_parameter

let simplify_name = Simplify_aux.simplify_name

let freshen_continuation env cont =
  Freshening.apply_continuation (E.freshening env) cont

(** Simplify an application of a continuation for a context where only a
    continuation is valid (e.g. a switch arm) and there are no opportunities
    for inlining or specialisation. *)
let simplify_continuation_use_cannot_inline env r cont ~arity =
  let cont = freshen_continuation env cont in
  let cont_type = E.find_continuation env cont in
  let cont =
    (* CR mshinwell: check: this alias logic should also apply in an
       inlinable context where no inlining happens *)
    match Continuation_approx.is_alias cont_type with
    | None -> Continuation_approx.name cont_type
    | Some alias_of ->
      Freshening.apply_continuation (E.freshening env) alias_of
  in
  let arg_tys = Flambda_type.unknown_types_from_arity arity in
  let r =
    R.use_continuation r env cont (Not_inlinable_or_specialisable arg_tys)
  in
  cont, r

let simplify_exn_continuation env r cont =
  simplify_continuation_use_cannot_inline env r cont
    ~arity:[Flambda_kind.value Unknown]

let for_defining_expr_of_let (env, r) var kind defining_expr =
  (* CR mshinwell: This handling of the typing environment in [R] needs to be
     added to the "simplify newly-introduced let bindings" function, below *)
  let r = R.clear_typing_judgements r in
  let new_bindings, defining_expr, ty, r =
    Simplify_named.simplify_named env r defining_expr ~result_var:var
  in
  let new_judgements = R.get_typing_judgements r in
  let new_kind = (E.type_accessor env T.kind) ty in
  if not (Flambda_kind.compatible new_kind ~if_used_at:kind) then begin
    Misc.fatal_errorf "Kind error during simplification of [Let] binding for %a"
      Variable.print var
  end;
  let defining_expr : Flambda.Reachable.t =
    match defining_expr with
    | Invalid _ -> defining_expr
    | Reachable _ ->
      if (E.type_accessor env T.is_bottom) ty then Flambda.Reachable.invalid ()
      else defining_expr
  in
  let var, freshening = Freshening.add_variable (E.freshening env) var in
  let env =
    (E.type_accessor env E.extend_typing_environment) env
      ~env_extension:new_judgements
  in
  let env = E.set_freshening env freshening in
  let env = E.add_variable env var ty in
  (env, r), new_bindings, var, new_kind, defining_expr

let filter_defining_expr_of_let r var (defining_expr : Named.t)
      free_names_of_body =
  if Name.Set.mem (Name.var var) free_names_of_body then
    r, var, Some defining_expr
  else if Named.maybe_generative_effects_but_no_coeffects defining_expr then
    match defining_expr with
    | Set_of_closures _ ->
      (* Don't delete closure definitions: there might be a reference to them
         (propagated through Flambda types) that is not in scope. *)
      r, var, Some defining_expr
    | _ ->
      let r = R.map_benefit r (B.remove_code_named defining_expr) in
      r, var, None
  else
    r, var, Some defining_expr

(** Simplify a set of [Let]-bindings introduced by a pass such as
    [Unbox_specialised_args] surrounding the term [around] that is in turn
    the defining expression of a [Let].  This is like simplifying a fragment
    of a context:

      let x0 = ... in
      ...
      let xn = ... in
      let var = around in  (* this is the original [Let] being simplified *)
      <hole>

    (In this example, [bindings] would map [x0] through [xn].)
*)
let _simplify_newly_introduced_let_bindings env r ~bindings
      ~(around : Named.t) =
  let bindings, env, r, invalid_term_semantics =
    List.fold_left (fun ((bindings, env, r, stop) as acc)
            (var, kind, defining_expr) ->
        match stop with
        | Some _ -> acc
        | None ->
          let (env, r), new_bindings, var, kind, defining_expr =
            for_defining_expr_of_let (env, r) var kind defining_expr
          in
          match (defining_expr : Flambda.Reachable.t) with
          | Reachable defining_expr ->
            let bindings =
              (var, kind, defining_expr) :: (List.rev new_bindings) @ bindings
            in
            bindings, env, r, None
          | Invalid invalid_term_semantics ->
            let bindings = (List.rev new_bindings) @ bindings in
            bindings, env, r, Some invalid_term_semantics)
      ([], env, r, None)
      bindings
  in
  let new_bindings, around, _ty, r =
    (* XXX provide [result_var] *)
    Simplify_named.simplify_named env r around ~result_var:(assert false)
  in
  let around_free_names =
    match around with
    | Reachable around -> Named.free_names around
    | Invalid _ -> Name.Set.empty
  in
  let bindings, r, _free_names =
    List.fold_left (fun (bindings, r, free_names) (var, kind, defining_expr) ->
        let r, var, defining_expr =
          filter_defining_expr_of_let r var defining_expr free_names
        in
        match defining_expr with
        | Some defining_expr ->
          let free_names =
            Name.Set.union (Named.free_names defining_expr)
              (Name.Set.remove (Name.var var) free_names)
          in
          (var, kind, defining_expr)::bindings, r, free_names
        | None ->
          bindings, r, free_names)
      ([], r, around_free_names)
      ((List.rev new_bindings) @ bindings)
  in
  bindings, around, invalid_term_semantics, r

module Make_simplify_switch (S : sig
  module Arm : sig
    type t
    module Map : Map.S with type key = t
  end

  val switch_arms
     : (T.t
      -> arms:Continuation.t Arm.Map.t
      -> (T.Typing_environment.t * Continuation.t) Arm.Map.t) T.type_accessor

  val create_switch'
     : scrutinee:Name.t
    -> arms:Continuation.t Arm.Map.t
    -> Expr.t * bool

  val type_of_scrutinee : Arm.t -> T.t
end) = struct
  let simplify_switch env r ~(scrutinee : Name.t)
        (arms : Continuation.t S.Arm.Map.t)
        : Expr.t * R.t =
    let scrutinee, scrutinee_ty = simplify_name env scrutinee in
    let arms = (E.type_accessor env S.switch_arms) scrutinee_ty ~arms in
    let destination_is_unreachable cont =
      (* CR mshinwell: This unreachable thing should be tidied up and also
         done on [Apply_cont]. *)
      let cont = freshen_continuation env cont in
      let cont_type = E.find_continuation env cont in
      match Continuation_approx.handlers cont_type with
      | None | Some (Recursive _) -> false
      | Some (Non_recursive handler) ->
        match handler.handler with
        | Invalid Treat_as_unreachable -> true
        | _ -> false
    in
    let arms =
      S.Arm.Map.filter (fun _arm (_env, cont) ->
          not (destination_is_unreachable cont))
        arms
    in
    let env = E.inside_branch env in
    let arms, r =
      S.Arm.Map.fold (fun arm (env_extension, cont) (arms, r) ->
          let cont, r =
            let scrutinee_ty = S.type_of_scrutinee arm in
            let env =
              match scrutinee with
              | Var scrutinee ->
                (E.type_accessor env E.add_or_meet_variable)
                  env scrutinee scrutinee_ty
              | Symbol _ -> env
            in
            let env =
              (E.type_accessor env E.extend_typing_environment) env
                ~env_extension
            in
            simplify_continuation_use_cannot_inline env r cont ~arity:[]
          in
          let arms = S.Arm.Map.add arm cont arms in
          arms, r)
        arms
        (S.Arm.Map.empty, r)
    in
    let switch, removed_branch =
      S.create_switch' ~scrutinee:scrutinee ~arms
    in
    if removed_branch then switch, R.map_benefit r B.remove_branch
    else switch, r
end

module Simplify_int_switch = Make_simplify_switch (struct
  module Arm = Targetint.OCaml
  let switch_arms = T.int_switch_arms
  let create_switch' = Expr.create_int_switch'
  let type_of_scrutinee arm = T.this_tagged_immediate (Immediate.int arm)
end)

module Simplify_tag_switch = Make_simplify_switch (struct
  module Arm = Tag
  let switch_arms = T.tag_switch_arms
  let create_switch' = Expr.create_tag_switch'
  let type_of_scrutinee arm = T.this_tag arm
end)

let simplify_switch env r ~(scrutinee : Name.t) (switch : Flambda0.Switch.t)
      : Expr.t * R.t =
  match switch with
  | Value arms ->
    Simplify_int_switch.simplify_switch env r ~scrutinee arms
  | Fabricated arms ->
    Simplify_tag_switch.simplify_switch env r ~scrutinee arms

(* Prepare an environment for the simplification of the given continuation
   handler when its parameters are being used at types [arg_tys]. *)
let environment_for_let_cont_handler ~env cont
      ~(handler : Flambda.Continuation_handler.t) ~arg_tys =
  let params = handler.params in
  let _freshened_vars, freshening =
    Freshening.add_variables' (E.freshening env)
      (Typed_parameter.List.vars params)
  in
  if List.length params <> List.length arg_tys then begin
    Misc.fatal_errorf "simplify_let_cont_handler (%a): params are %a but \
        arg_tys has length %d"
      Continuation.print cont
      Typed_parameter.List.print params
      (List.length arg_tys)
  end;
  let params =
    List.map (fun (param, arg_ty) : Typed_parameter.t ->
(*        let unfreshened_param = param in *)
        let param =
          Typed_parameter.map_var param
            ~f:(fun var -> Freshening.apply_variable freshening var)
        in
(*
        let param_ty = Typed_parameter.ty param in
        if !Clflags.flambda_invariant_checks then begin
          if not ((E.type_accessor env T.as_or_more_precise)
            arg_ty ~than:param_ty)
          then begin
            Misc.fatal_errorf "Parameter %a of continuation %a supplied \
                with argument which has regressed in preciseness of type: %a"
              Typed_parameter.print unfreshened_param
              Continuation.print cont
              T.print arg_ty
          end
        end;
*)
        let ty =
          T.rename_variables arg_ty
            ~f:(fun var -> Freshening.apply_variable freshening var)
        in
        Typed_parameter.with_type param ty)
      (List.combine params arg_tys)
  in
  List.fold_left (fun env param ->
      E.add_variable env (Typed_parameter.var param)
        (Typed_parameter.ty param))
    (E.set_freshening env freshening)
    params

let rec simplify_let_cont_handler ~env ~r ~cont ~handler ~arg_tys =
  let env = environment_for_let_cont_handler ~env cont ~handler ~arg_tys in
  let { Flambda.Continuation_handler. params; stub; is_exn_handler; handler; } =
    handler
  in
  let handler, r = simplify_expr (E.inside_branch env) r handler in
  let handler : Flambda.Continuation_handler.t =
    { params;
      stub;
      is_exn_handler;
      handler;
    }
  in
  r, handler

and simplify_let_cont_handlers env r ~handlers
      ~(recursive : Flambda.recursive) ~freshening
      : Flambda.Let_cont_handlers.t option * R.t =
  Continuation.Map.iter (fun cont _handler ->
      let cont = Freshening.apply_continuation freshening cont in
      if R.continuation_defined r cont then begin
        Misc.fatal_errorf "Ready to simplify continuation handlers \
            defining (at least) %a but such continuation(s) is/are already \
            defined in [r]"
          Continuation.print cont
      end)
    handlers;
  (* If none of the handlers are used in the body, delete them all. *)
  let all_unused =
    Continuation.Map.for_all (fun cont _handler ->
        let cont = Freshening.apply_continuation freshening cont in
        R.continuation_unused r cont)
      handlers
  in
  if all_unused then begin
    (* We don't need to touch [r] since we haven't simplified any of
       the handlers. *)
    None, r
  end else
    let handlers =
      Continuation.Map.fold (fun cont
                (handler : Flambda.Continuation_handler.t) handlers ->
          let cont' = Freshening.apply_continuation freshening cont in
          let arg_tys, new_env =
            (* CR mshinwell: I have a suspicion that [r] may not contain the
               usage information for the continuation when it's come from
               [Unbox_continuation_params]. Check. *)
            R.continuation_args_types r cont
              ~arity:(Flambda.Continuation_handler.param_arity handler)
              ~default_env:(E.get_typing_environment env)
          in
          (* [new_env] contains everything we know holds at _all_ of the use
             points of the continuation, with anything out of scope at the
             definition site of [cont] marked as existential. *)
          let env = E.replace_typing_environment env new_env in
          let r, handler =
            simplify_let_cont_handler ~env ~r:(R.create ()) ~cont:cont'
              ~handler ~arg_tys
          in
          Continuation.Map.add cont' (handler, env, r) handlers)
        handlers
        Continuation.Map.empty
    in
    let continuation_unused cont =
      (* For a continuation being bound in the group to be unused, it must be
         unused within *all of the handlers* and the body. *)
      let unused_within_all_handlers =
        Continuation.Map.for_all (fun _cont (_handler, _env, r_from_handler) ->
            not (R.is_used_continuation r_from_handler cont))
          handlers
      in
      unused_within_all_handlers
        && not (R.is_used_continuation r cont)
    in
    (* Collect uses of the continuations and delete any unused ones.
       The usage information will subsequently be used by the continuation
       inlining and specialisation transformations. *)
    let r =
      Continuation.Map.fold (fun cont
              ((_handler : Flambda.Continuation_handler.t), _env,
               r_from_handler) r ->
          if continuation_unused cont then r
          else R.union r r_from_handler)
        handlers
        r
    in
    let r, handlers =
      Continuation.Map.fold (fun cont
              ((handler : Flambda.Continuation_handler.t), env, _r_from_handler)
              (r, handlers) ->
          let r, uses = R.exit_scope_of_let_cont r env cont in
          if continuation_unused cont then
            r, handlers
          else
            let handlers =
              Continuation.Map.add cont (handler, env, uses) handlers
            in
            r, handlers)
        handlers
        (r, Continuation.Map.empty)
    in
    Continuation.Map.iter (fun cont _handler ->
        assert (R.continuation_unused r cont))
      handlers;
    if Continuation.Map.is_empty handlers then begin
      None, r
    end else
      let r, handlers =
        Continuation.Map.fold (fun cont
                ((handler : Flambda.Continuation_handler.t), env, uses)
                (r, handlers') ->
            let ty =
              let handlers : Continuation_approx.continuation_handlers =
                match recursive with
                | Non_recursive ->
                  begin match Continuation.Map.bindings handlers with
                  | [_cont, (handler, _, _)] -> Non_recursive handler
                  | _ ->
                    Misc.fatal_errorf "Non_recursive Let_cont may only have one \
                        handler, but binds %a"
                      Continuation.Set.print (Continuation.Map.keys handlers)
                  end
                | Recursive ->
                  let handlers =
                    Continuation.Map.map (fun (handler, _env, _uses) -> handler)
                      handlers
                  in
                  Recursive handlers
              in
              Continuation_approx.create ~name:cont ~handlers
                ~arity:(Flambda.Continuation_handler.param_arity handler)
            in
            let r =
              R.define_continuation r cont env recursive uses ty
            in
            let handlers' = Continuation.Map.add cont handler handlers' in
            r, handlers')
          handlers
          (r, Continuation.Map.empty)
      in
      match recursive with
      | Non_recursive ->
        begin match Continuation.Map.bindings handlers with
        | [name, handler] ->
          Some (Flambda.Let_cont_handlers.Non_recursive { name; handler; }), r
        | _ -> assert false
        end
      | Recursive ->
        let is_non_recursive =
          if Continuation.Map.cardinal handlers > 1 then None
          else
            match Continuation.Map.bindings handlers with
            | [name, (handler : Flambda.Continuation_handler.t)] ->
              let fcs = Flambda.Expr.free_continuations handler.handler in
              if not (Continuation.Set.mem name fcs) then
                Some (name, handler)
              else
                None
            | _ -> None
        in
        match is_non_recursive with
        | Some (name, handler) ->
          Some (Flambda.Let_cont_handlers.Non_recursive { name; handler; }), r
        | None -> Some (Flambda.Let_cont_handlers.Recursive handlers), r

and simplify_let_cont env r ~body
      ~(handlers : Flambda.Let_cont_handlers.t) : Expr.t * R.t =
  (* In two stages we form the environment to be used for simplifying the
     [body].  If the continuations in [handlers] are recursive then
     that environment will also be used for simplifying the continuations
     themselves (otherwise the environment of the [Let_cont] is used). *)
  let conts_and_types, freshening =
    let normal_case ~handlers =
      Continuation.Map.fold (fun name
              (handler : Flambda.Continuation_handler.t)
              (conts_and_types, freshening) ->
          let freshened_name, freshening =
            Freshening.add_continuation freshening name
          in
          let ty =
            (* If it's a stub, we put the code for [handler] in the
               environment; this is unfreshened, but will be freshened up
               if we inline it.
               Note that stubs are not allowed to call themselves.
               The code for [handler] is also put in the environment if
               the continuation is just an [Apply_cont] acting as a
               continuation alias or just contains
               [Invalid Treat_as_unreachable].  This enables earlier [Switch]es
               that branch to such continuation to be simplified, in some cases
               removing them entirely. *)
            let alias_or_unreachable =
              match handler.handler with
              | Invalid Treat_as_unreachable -> true
              (* CR mshinwell: share somehow with [Continuation_approx].
                 Also, think about this in the multi-argument case -- need
                 to freshen. *)
              (* CR mshinwell: Check instead that the continuation doesn't
                 have any arguments and doesn't have any effects, to avoid
                 this syntactic match
                 ...except that we still need to know which continuation
                 it calls, if any *)
              | Apply_cont (_cont, None, []) -> true
              | _ -> false
            in
            let arity = Flambda.Continuation_handler.param_arity handler in
            if handler.stub || alias_or_unreachable then begin
              assert (not (Continuation.Set.mem name
                (Flambda.Expr.free_continuations handler.handler)));
              Continuation_approx.create ~name:freshened_name
                ~handlers:(Non_recursive handler) ~arity
            end else begin
              Continuation_approx.create_unknown ~name:freshened_name ~arity
            end
          in
          let conts_and_types =
            Continuation.Map.add freshened_name (name, ty) conts_and_types
          in
          conts_and_types, freshening)
        handlers
        (Continuation.Map.empty, E.freshening env)
    in
    let handlers = Flambda.Let_cont_handlers.to_continuation_map handlers in
    normal_case ~handlers
  in
  (* CR mshinwell: Is _unfreshened_name redundant? *)
  let body_env =
    let env = E.set_freshening env freshening in
    Continuation.Map.fold (fun name (_unfreshened_name, cont_approx) env ->
        E.add_continuation env name cont_approx)
      conts_and_types
      env
  in
  let body, r = simplify_expr body_env r body in
  begin match handlers with
  | Non_recursive { name; handler; } ->
    let with_wrapper : Expr.with_wrapper =
      (* CR mshinwell: Tidy all this up somehow. *)
      (* Unboxing of continuation parameters is done now so that in one pass
         of [Simplify] such unboxing will go all the way down the
         control flow. *)
      if handler.stub || E.never_unbox_continuations env
      then Unchanged { handler; }
      else
        let args_types =
          R.continuation_args_types r name
            ~arity:(Flambda.Continuation_handler.param_arity handler)
            ~default_env:(E.get_typing_environment env)
        in
        Unbox_continuation_params.for_non_recursive_continuation ~handler
          ~args_types ~name ~backend:(E.backend env)
    in
    let simplify_one_handler env r ~name ~handler ~body
            : Expr.t * R.t =
      (* CR mshinwell: Consider whether we should call [exit_scope_of_let_cont]
         for non-recursive ones before simplifying their body.  I'm not sure we
         need to, since we already ensure such continuations aren't in the
         environment when simplifying the [handlers].
         ...except for stubs... *)
      let handlers =
        Continuation.Map.add name handler Continuation.Map.empty
      in
      let recursive : Flambda.recursive = Non_recursive in
      let handlers, r =
        simplify_let_cont_handlers env r ~handlers ~recursive ~freshening
      in
      match handlers with
      | None -> body, r
      | Some handlers -> Let_cont { body; handlers; }, r
    in
    begin match with_wrapper with
    | Unchanged _ -> simplify_one_handler env r ~name ~handler ~body
    | With_wrapper { new_cont; new_handler; wrapper_handler; } ->
      let ty =
        Continuation_approx.create_unknown ~name:new_cont
          ~arity:(Flambda.Continuation_handler.param_arity new_handler)
      in
      let body, r =
        let env = E.add_continuation env new_cont ty in
        simplify_one_handler env r ~name ~handler:wrapper_handler ~body
      in
      let body, r =
        simplify_one_handler env r ~name:new_cont ~handler:new_handler ~body
      in
      let r =
        R.update_all_continuation_use_environments r
          ~if_present_in_env:name ~then_add_to_env:(new_cont, ty)
      in
      body, r
    end
  | Recursive handlers ->
    (* The sequence is:
       1. Simplify the recursive handlers with their parameter types as
          pre-existing in the term.
       2. If all of the handlers are unused, there's nothing more to do.
       3. Extract the (hopefully more precise) Flambda types for the
          handlers' parameters from [r].
       4. The code from the simplification is discarded.
       5. The continuation(s) is/are unboxed as required.
       6. The continuation(s) are simplified once again using the
          Flambda types deduced in step 2.
       We could continue to a fixed point, but it doesn't seem worth the
       complication.
    *)
    let original_r = r in
    let original_handlers = handlers in
    let recursive : Flambda.recursive = Recursive in
    let handlers, r =
      simplify_let_cont_handlers env r ~handlers ~recursive ~freshening
    in
    begin match handlers with
    | None -> body, r
    | Some _handlers ->
      let args_types =
        Continuation.Map.mapi (fun cont
                  (handler : Flambda.Continuation_handler.t) ->
            let cont =
              Freshening.apply_continuation (E.freshening body_env) cont
            in
            (* N.B. If [cont]'s handler was deleted, the following function
               will produce [Value_bottom] for the arguments, rather than
               failing. *)
            R.defined_continuation_args_types r cont
              ~arity:(Flambda.Continuation_handler.param_arity handler)
              ~default_env:(E.get_typing_environment env))
          original_handlers
      in
      let handlers = original_handlers in
      let r = original_r in
      let handlers, env, update_use_env =
        if E.never_unbox_continuations env then
          handlers, body_env, []
        else
          let with_wrappers =
            Unbox_continuation_params.for_recursive_continuations
              ~handlers ~args_types ~backend:(E.backend env)
          in
          (* CR mshinwell: move to Flambda, probably *)
          Continuation.Map.fold (fun cont
                  (with_wrapper : Expr.with_wrapper)
                  (handlers, env, update_use_env) ->
              match with_wrapper with
              | Unchanged { handler; } ->
                Continuation.Map.add cont handler handlers, env,
                  update_use_env
              | With_wrapper { new_cont; new_handler; wrapper_handler; } ->
                let handlers =
                  Continuation.Map.add new_cont new_handler
                    (Continuation.Map.add cont wrapper_handler handlers)
                in
                let arity =
                  Flambda.Continuation_handler.param_arity new_handler
                in
                let ty =
                  Continuation_approx.create_unknown ~name:new_cont ~arity
                in
                let env = E.add_continuation env new_cont ty in
                let update_use_env = (cont, (new_cont, ty)) :: update_use_env in
                handlers, env, update_use_env)
            with_wrappers
            (Continuation.Map.empty, body_env, [])
      in
      let handlers, r =
        simplify_let_cont_handlers env r ~handlers ~recursive ~freshening
      in
      let r =
        List.fold_left (fun r (if_present_in_env, then_add_to_env) ->
            R.update_all_continuation_use_environments r
              ~if_present_in_env ~then_add_to_env)
          r
          update_use_env
      in
      begin match handlers with
      | None -> body, r
      | Some handlers -> Let_cont { body; handlers; }, r
      end
    end
  end

and simplify_full_application env r ~callee
      ~callee's_closure_id ~function_decl ~set_of_closures ~args
      ~arg_tys ~continuation ~exn_continuation ~dbg ~inline_requested
      ~specialise_requested =
  Inlining_decision.for_call_site ~env ~r ~set_of_closures ~callee
    ~callee's_closure_id ~function_decl ~args ~arg_tys ~continuation
    ~exn_continuation ~dbg ~inline_requested ~specialise_requested

and simplify_partial_application env r ~callee
      ~callee's_closure_id
      ~(function_decl : Flambda_type.inlinable_function_declaration)
      ~(args : Simple.t list)
      ~continuation ~exn_continuation ~dbg ~inline_requested
      ~specialise_requested =
  let arity = List.length function_decl.params in
  assert (arity > List.length args);
  (* For simplicity, we disallow [@inline] attributes on partial
     applications.  The user may always write an explicit wrapper instead
     with such an attribute. *)
  (* CR-someday mshinwell: Pierre noted that we might like a function to be
     inlined when applied to its first set of arguments, e.g. for some kind
     of type class like thing. *)
  begin match (inline_requested : Flambda.inline_attribute) with
  | Always_inline | Never_inline ->
    Location.prerr_warning (Debuginfo.to_location dbg)
      (Warnings.Inlining_impossible "[@inlined] attributes may not be used \
        on partial applications")
  | Unroll _ ->
    Location.prerr_warning (Debuginfo.to_location dbg)
      (Warnings.Inlining_impossible "[@unroll] attributes may not be used \
        on partial applications")
  | Default_inline -> ()
  end;
  begin match (specialise_requested : Flambda.specialise_attribute) with
  | Always_specialise | Never_specialise ->
    Location.prerr_warning (Debuginfo.to_location dbg)
      (Warnings.Inlining_impossible "[@specialised] attributes may not be used \
        on partial applications")
  | Default_specialise -> ()
  end;
  let freshened_params =
    List.map (fun (param, ty) -> Parameter.rename param, ty)
      function_decl.params
  in
  let applied_args, remaining_args =
    Misc.Stdlib.List.map2_prefix (fun arg param -> param, arg)
      args freshened_params
  in
  let return_arity =
    (* CR mshinwell: Move to [Flambda_type] *)
    List.map (E.type_accessor env T.kind) function_decl.result
  in
  let wrapper_continuation_param = Continuation.create () in
  let wrapper_taking_remaining_args =
    let args =
      List.map (fun (param, _ty) -> Simple.var (Parameter.var param))
        freshened_params
    in
    let body : Expr.t =
      Apply {
        continuation = wrapper_continuation_param;
        exn_continuation;
        func = callee;
        args;
        call_kind = Function (Direct {
          closure_id = callee's_closure_id;
          return_arity;
        });
        dbg;
        inline = Default_inline;
        specialise = Default_specialise;
      }
    in
    let closure_variable =
      Variable.rename
        ~append:"_partial_fun"
        (Closure_id.unwrap callee's_closure_id)
    in
    let free_variable_kind var =
      (* There are no free variables in [body], above, except the wrapper's
         parameters. *)
      Misc.fatal_errorf "Variable %a should not be a free variable of [body]"
        Variable.print var
    in
    let params =
      List.map (fun (param, ty) ->
          let kind = (E.type_accessor env T.kind) ty in
          Typed_parameter.create_from_kind param kind)
        remaining_args
    in
    (* CR mshinwell: [make_closure_declaration] is only used here and it also
       calls [toplevel_substitution].  We should alter that function or else
       inline it here.  Note that the boxing stuff in that function isn't
       needed here because a partial application can only involve arguments
       of kind [Value] *)
    Expr.make_closure_declaration ~id:closure_variable
      ~free_variable_kind
      ~body
      ~params
      ~return_arity
      ~stub:true
      ~continuation_param:wrapper_continuation_param
      ~exn_continuation_param:exn_continuation
      ~continuation
      ~dbg
  in
  let bindings =
    List.map (fun ((param, param_ty), arg) ->
        let param_var = Parameter.var param in
        let param_kind = (E.type_accessor env T.kind) param_ty in
        param_var, param_kind, Named.Simple arg)
      applied_args
  in
  simplify_expr env r (Expr.bind ~bindings ~body:wrapper_taking_remaining_args)

and simplify_over_application env r ~args ~arg_tys ~continuation
      ~exn_continuation ~callee ~callee's_closure_id
      ~(function_decl : Flambda_type.inlinable_function_declaration)
      ~set_of_closures ~dbg ~inline_requested ~specialise_requested =
  let return_arity =
    (* CR mshinwell: Move to [Flambda_type] *)
    List.map (E.type_accessor env T.kind) function_decl.result
  in
  let continuation, r =
    simplify_continuation_use_cannot_inline env r continuation
      ~arity:return_arity
  in
  let arity = List.length return_arity in
  assert (arity < List.length args);
  assert (List.length args = List.length arg_tys);
  let full_app_args, remaining_args = Misc.Stdlib.List.split_at arity args in
  let full_app_types, _ = Misc.Stdlib.List.split_at arity arg_tys in
  let func_var = Variable.create "full_apply" in
  let func_var_kind = Flambda_kind.value Definitely_pointer in
  let func_param =
    Flambda.Typed_parameter.create_from_kind (Parameter.wrap func_var)
      func_var_kind
  in
  let handler : Flambda.Continuation_handler.t =
    { stub = false;
      is_exn_handler = false;
      params = [func_param];
      handler =
        Apply {
          continuation;
          exn_continuation;
          func = Name.var func_var;
          args = remaining_args;
          call_kind = Function Indirect_unknown_arity;
          dbg;
          inline = inline_requested;
          specialise = specialise_requested;
        };
    }
  in
  let after_full_application = Continuation.create () in
  let after_full_application_approx =
    Continuation_approx.create ~name:after_full_application
      ~handlers:(Non_recursive handler)
      ~arity:[func_var_kind]
  in
  let full_application, r =
    let env =
      E.add_continuation env after_full_application
        after_full_application_approx
    in
    simplify_full_application env r ~callee ~callee's_closure_id
      ~function_decl ~set_of_closures ~args:full_app_args
      ~arg_tys:full_app_types ~continuation:after_full_application
      (* CR mshinwell: check [exn_continuation] is correct *)
      ~exn_continuation ~dbg ~inline_requested ~specialise_requested
  in
  (* CR mshinwell: Maybe it would be better just to build a proper term
     including the full application as a normal Apply node and call simplify
     on that? *)
  let r, after_full_application_uses =
    R.exit_scope_of_let_cont r env after_full_application
  in
  let r =
    R.define_continuation r after_full_application env Non_recursive
      after_full_application_uses after_full_application_approx
  in
  let expr : Expr.t =
    Let_cont {
      body = full_application;
      handlers = Non_recursive {
        name = after_full_application;
        handler;
      };
    }
  in
  expr, r

and simplify_apply_shared env r (apply : Flambda.Apply.t)
      : T.t * (T.t list) * Flambda.Apply.t * R.t =
  let func, func_ty = simplify_name env apply.func in
  let args, args_tys = List.split (S.simplify_simples env apply.args) in
  let continuation, r =
    simplify_continuation_use_cannot_inline env r apply.continuation
      ~arity:(Flambda.Call_kind.return_arity apply.call_kind)
  in
  let exn_continuation, r =
    simplify_exn_continuation env r apply.exn_continuation
  in
  let dbg = E.add_inlined_debuginfo env ~dbg:apply.dbg in
  let apply : Flambda.Apply.t = {
    func;
    continuation;
    exn_continuation;
    args;
    call_kind = apply.call_kind;
    dbg;
    inline = apply.inline;
    specialise = apply.specialise;
  }
  in
  func_ty, args_tys, apply, r

and simplify_function_application env r (apply : Flambda.Apply.t)
      (call : Flambda.Call_kind.function_call) : Expr.t * R.t =
  let callee_ty, arg_tys, apply, r = simplify_apply_shared env r apply in
  let {
    Flambda.Apply. func = callee; args; call_kind = _; dbg;
    inline = inline_requested; specialise = specialise_requested;
    continuation; exn_continuation;
  } = apply in
  let unknown_closures () : Expr.t * R.t =
    let function_call : Flambda.Call_kind.function_call =
      match call with
      | Indirect_unknown_arity
      | Indirect_known_arity _ -> call
      | Direct { return_arity; _ } ->
        let param_arity =
          (* Some types have regressed in precision.  Since this was a
             direct call, we know exactly how many arguments the function
             takes. *)
          (* CR mshinwell: Add note about the GC scanning flag
             regressing?  (This should be ok because if it regresses it
             should still be conservative.) *)
          List.map (fun arg ->
              let _arg, ty = S.simplify_simple env arg in
              (E.type_accessor env T.kind) ty)
            args
        in
        Indirect_known_arity {
          param_arity;
          return_arity;
        }
    in
    let call_kind = Flambda.Call_kind.Function function_call in
    let arity = Flambda.Call_kind.return_arity call_kind in
    let continuation, r =
      simplify_continuation_use_cannot_inline env r continuation ~arity
    in
    Apply ({
      func = callee;
      args;
      call_kind;
      dbg;
      inline = inline_requested;
      specialise = specialise_requested;
      continuation;
      exn_continuation;
    }), r
  in
  let inlinable ~callee's_closure_id
        ~(function_decl : T.inlinable_function_declaration)
        ~(set_of_closures : T.set_of_closures) =
    let arity_of_application =
      Flambda.Call_kind.return_arity apply.call_kind
    in
    let result_arity =
      List.map (E.type_accessor env T.kind) function_decl.result
    in
    let arity_mismatch =
      not (Flambda_arity.equal arity_of_application result_arity)
    in
    if arity_mismatch then begin
      Misc.fatal_errorf "Application of %a (%a):@,function has return \
          arity %a but the application expression is expecting it \
          to have arity %a.  Function declaration is:@,%a"
        Name.print callee
        Simple.List.print args
        Flambda_arity.print result_arity
        Flambda_arity.print arity_of_application
        T.print_inlinable_function_declaration function_decl
    end;
    let r =
      match call with
      | Indirect_unknown_arity ->
        R.map_benefit r
          Inlining_cost.Benefit.direct_call_of_indirect_unknown_arity
      | Indirect_known_arity _ ->
        (* CR mshinwell: This should check that the [param_arity] inside
           the call kind is compatible with the kinds of [args]. *)
        R.map_benefit r
          Inlining_cost.Benefit.direct_call_of_indirect_known_arity
      | Direct _ -> r
    in
    let provided_num_args = List.length args in
    let num_args = List.length function_decl.params in
    let result, r =
      if provided_num_args = num_args then
        simplify_full_application env r
          ~callee ~callee's_closure_id ~function_decl ~set_of_closures
          ~args ~arg_tys ~continuation ~exn_continuation ~dbg
          ~inline_requested ~specialise_requested
      else if provided_num_args > num_args then
        simplify_over_application env r ~args ~arg_tys ~continuation
          ~exn_continuation ~callee ~callee's_closure_id ~function_decl
          ~set_of_closures ~dbg ~inline_requested
          ~specialise_requested
      else if provided_num_args > 0 && provided_num_args < num_args then
        simplify_partial_application env r ~callee ~callee's_closure_id
          ~function_decl ~args ~continuation ~exn_continuation ~dbg
          ~inline_requested ~specialise_requested
      else
        Misc.fatal_errorf "Function with %d/%d args when simplifying \
            application expression: %a"
          provided_num_args
          num_args
          Flambda.Apply.print apply
    in
    (* wrap <-- for direct call surrogates *) result, r
(* CR mshinwell: Have disabled direct call surrogates just for the moment
    let callee, callee's_closure_id,
          value_set_of_closures, env, wrap =
      (* If the call site is a direct call to a function that has a
         direct call surrogate, repoint the call to the surrogate. *)
      let surrogates = value_set_of_closures.direct_call_surrogates in
      match Closure_id.Map.find callee's_closure_id surrogates with
      | exception Not_found ->
        callee, callee's_closure_id,
          value_set_of_closures, env, (fun expr -> expr)
      | surrogate ->
        let rec find_transitively surrogate =
          match Closure_id.Map.find surrogate surrogates with
          | exception Not_found -> surrogate
          | surrogate -> find_transitively surrogate
        in
        let surrogate = find_transitively surrogate in
        let surrogate_var =
          Variable.rename callee ~append:"_surrogate"
        in
        let move_to_surrogate : Projection.move_within_set_of_closures =
          { closure = callee;
            move = Closure_id.Map.singleton callee's_closure_id
                     surrogate;
          }
        in
        let type_for_surrogate =
          T.closure ~closure_var:surrogate_var
            ?set_of_closures_var ?set_of_closures_symbol
            (Closure_id.Map.singleton surrogate value_set_of_closures)
        in
        let env = E.add env surrogate_var type_for_surrogate in
        let wrap expr =
          Expr.create_let surrogate_var
            (Move_within_set_of_closures move_to_surrogate)
            expr
        in
        surrogate_var, surrogate, value_set_of_closures, env, wrap
    in
*)
  in
  let non_inlinable ~(function_decls : T.non_inlinable_function_declarations) =
    ignore function_decls;
    (* CR mshinwell: Pierre to implement *)
    unknown_closures ()
  in
  match (E.type_accessor env T.prove_closures) callee_ty with
  | Proved closures ->
    begin match Closure_id.Map.get_singleton closures with
    | Some (callee's_closure_id, { set_of_closures = set_ty; }) ->
      let set_ty = T.of_ty_fabricated set_ty in
      let proof = (E.type_accessor env T.prove_sets_of_closures) set_ty in
      begin match proof with
      | Proved (_set_of_closures_name, set_of_closures) ->
        let closures = T.extensibility_contents set_of_closures.closures in
        begin match Closure_id.Map.find callee's_closure_id closures with
        | exception Not_found -> Expr.invalid (), r
        | closure_ty ->
          let closure_ty = T.of_ty_fabricated closure_ty in
          match (E.type_accessor env T.prove_closure) closure_ty with
          | Proved { function_decls = Inlinable function_decl; } ->
            inlinable ~callee's_closure_id ~function_decl ~set_of_closures
          | Proved { function_decls = Non_inlinable None; } ->
            unknown_closures ()
          | Proved { function_decls = Non_inlinable (Some function_decls); } ->
            non_inlinable ~function_decls
          | Unknown -> unknown_closures ()
          | Invalid -> Expr.invalid (), r
        end
      | Unknown -> unknown_closures ()
      | Invalid -> Expr.invalid (), r
      end
    | None -> unknown_closures ()
    end
  | Unknown -> unknown_closures ()
  | Invalid -> Expr.invalid (), r

and simplify_method_call env r (apply : Flambda.Apply.t) ~kind ~obj
      : Expr.t * R.t =
  let callee_ty, _args_tys, apply, r = simplify_apply_shared env r apply in
  let callee_kind = (E.type_accessor env T.kind) callee_ty in
  if not (Flambda_kind.is_value callee_kind) then begin
    Misc.fatal_errorf "Method call with callee of wrong kind %a: %a"
      Flambda_kind.print callee_kind
      T.print callee_ty
  end;
  let obj, _obj_ty = simplify_name env obj in
  let apply : Flambda.Apply.t = {
    apply with
    call_kind = Method { kind; obj; };
  }
  in
  Apply apply, r

and simplify_c_call env r apply ~alloc:_ ~param_arity:_ ~return_arity:_
      : Expr.t * R.t =
  let callee_ty, _args_tys, apply, r = simplify_apply_shared env r apply in
  let callee_kind = (E.type_accessor env T.kind) callee_ty in
  if not (Flambda_kind.is_value callee_kind) then begin
    Misc.fatal_errorf "C call with callee of wrong kind %a: %a"
      Flambda_kind.print callee_kind
      T.print callee_ty
  end;
  Apply apply, r

(** Simplify an application of a continuation. *)
and simplify_apply_cont env r cont ~(trap_action : Flambda.Trap_action.t option)
      ~(args : Simple.t list) : Expr.t * R.t =
  let cont = freshen_continuation env cont in
  let cont_approx = E.find_continuation env cont in
  let cont = Continuation_approx.name cont_approx in
  let args_and_types = S.simplify_simples env args in
  let args, arg_tys = List.split args_and_types in
  let param_arity_of_exn_handler = [Flambda_kind.value Unknown] in
  let freshen_trap_action env r (trap_action : Flambda.Trap_action.t) =
    match trap_action with
    | Push { id; exn_handler; } ->
      let id = Freshening.apply_trap (E.freshening env) id in
      let exn_handler, r =
        simplify_continuation_use_cannot_inline env r exn_handler
          ~arity:param_arity_of_exn_handler
      in
      Flambda.Trap_action.Push { id; exn_handler; }, r
    | Pop { id; exn_handler; take_backtrace; } ->
      let id = Freshening.apply_trap (E.freshening env) id in
      let exn_handler, r =
        simplify_continuation_use_cannot_inline env r exn_handler
          ~arity:param_arity_of_exn_handler
      in
      Flambda.Trap_action.Pop { id; exn_handler; take_backtrace; }, r
  in
  match Continuation_approx.handlers cont_approx with
  | Some (Non_recursive handler) when handler.stub && trap_action = None ->
    (* Stubs are unconditionally inlined out now for two reasons:
       - [Continuation_inlining] cannot do non-linear inlining;
       - Even if it could, we don't want to have to run that pass when
         doing a "noinline" run of [Simplify].
       Note that we don't call [R.use_continuation] here, because we're going
       to eliminate the use. *)
    let env = E.activate_freshening env in
    let env = E.disallow_continuation_inlining (E.set_never_inline env) in
    let env = E.disallow_continuation_specialisation env in
    let env = environment_for_let_cont_handler ~env cont ~handler ~arg_tys in
    let stub's_body : Expr.t =
      match trap_action with
      | None -> handler.handler
      | Some trap_action ->
        let new_cont = Continuation.create () in
        Let_cont {
          body = Apply_cont (new_cont, Some trap_action, []);
          handlers = Non_recursive {
            name = new_cont;
            handler = {
              handler = handler.handler;
              params = [];
              stub = false;
              is_exn_handler = false;
            };
          };
        }
    in
    simplify_expr env r stub's_body
  | Some _ | None ->
    let r =
      let kind : R.Continuation_uses.Use.Kind.t =
        match trap_action with
        | None -> Inlinable_and_specialisable args_and_types
        | Some _ -> Only_specialisable args_and_types
      in
      R.use_continuation r env cont kind
    in
    let trap_action, r =
      match trap_action with
      | None -> None, r
      | Some trap_action ->
        let trap_action, r = freshen_trap_action env r trap_action in
        Some trap_action, r
    in
    Apply_cont (cont, trap_action, args), r

and simplify_expr env r (tree : Expr.t) : Expr.t * R.t =
  match tree with
  | Let _ ->
    let for_last_body (env, r) body = simplify_expr env r body in
    Flambda.Expr.Folders.fold_lets_option tree
      ~init:(env, r)
      ~for_defining_expr:for_defining_expr_of_let
      ~for_last_body
      ~filter_defining_expr:filter_defining_expr_of_let
  | Let_mutable { var; initial_value; body; contents_type; } ->
    (* We don't currently do dead [Let_mutable] elimination.  Work in this
       area should concentrate on removing mutable variables entirely. *)
    let initial_value, initial_value_ty = S.simplify_simple env initial_value in
    let var, freshening =
      Freshening.add_mutable_variable (E.freshening env) var
    in
    let env = E.set_freshening env freshening in
    let contents_kind = (E.type_accessor env T.kind) contents_type in
    let ty = T.unknown contents_kind in
    let body, r = simplify_expr (E.add_mutable env var ty) r body in
    let initial_value_kind = (E.type_accessor env T.kind) initial_value_ty in
    if not (Flambda_kind.compatible initial_value_kind
        ~if_used_at:contents_kind)
    then begin
      Misc.fatal_errorf "Cannot put initial value %a of kind %a into \
          mutable variable %a of kind %a"
        Simple.print initial_value
        Flambda_kind.print initial_value_kind
        Mutable_variable.print var
        Flambda_kind.print contents_kind
    end;
    let expr : Expr.t =
      Let_mutable {
        var;
        initial_value;
        body;
        contents_type;
      }
    in
    expr, r
  | Let_cont { body; handlers; } -> simplify_let_cont env r ~body ~handlers
  | Apply apply ->
    begin match apply.call_kind with
    | Function call -> simplify_function_application env r apply call
    | Method { kind; obj; } -> simplify_method_call env r apply ~kind ~obj
    | C_call { alloc; param_arity; return_arity; } ->
      simplify_c_call env r apply ~alloc ~param_arity ~return_arity
    end
  | Apply_cont (cont, trap_action, args) ->
    simplify_apply_cont env r cont ~trap_action ~args
  | Switch (scrutinee, switch) ->
    simplify_switch env r ~scrutinee switch
  | Invalid _ -> tree, r
