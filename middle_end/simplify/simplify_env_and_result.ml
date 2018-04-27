(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module T = Flambda_type
module TE = Flambda_type.Typing_env

module rec Env : sig
  include Simplify_env_and_result_intf.Env
    with type result = Result.t
end = struct
  type result = Result.t
  type continuation_uses = Continuation_uses.t  (* CR mshinwell: remove *)

  type t = {
    backend : (module Backend_intf.S);
    simplify_toplevel:(
         t
      -> Result.t
      -> Flambda.Expr.t
      -> continuation:Continuation.t
      -> continuation_params:Flambda.Typed_parameter.t list
      -> exn_continuation:Continuation.t
      -> descr:string
      -> Flambda.Expr.t * result * continuation_uses
           * (Flambda_type.t * Flambda_kind.t * Flambda_static.Static_part.t)
               Symbol.Map.t);
    simplify_expr:(
         t
      -> Result.t
      -> Flambda.Expr.t
      -> Flambda.Expr.t * Result.t);
    simplify_continuation_use_cannot_inline:(
         t
      -> Result.t
      -> Continuation.t
      -> params:Flambda.Typed_parameter.t list
      -> Continuation.t * Result.t);
    round : int;
    typing_environment : TE.t;
    mutable_variables : T.t Mutable_variable.Map.t;
    continuations : (Scope_level.t * Continuation_approx.t) Continuation.Map.t;
    continuation_scope_level : Scope_level.t;
(*    projections : Variable.t Projection.Map.t; *)
    (* CR mshinwell: but we do need the CSEed pure primitives *)
    current_functions : Set_of_closures_origin.Set.t;
    (* The functions currently being declared: used to avoid inlining
       recursively *)
    inlining_level : int;
    (* Number of times "inline" has been called recursively *)
    inside_branch : int;
    freshening : Freshening.t;
    never_inline : bool;
    never_inline_inside_closures : bool;
    never_inline_outside_closures : bool;
    allow_continuation_inlining : bool;
    allow_continuation_specialisation : bool;
    unroll_counts : int Set_of_closures_origin.Map.t;
    inlining_counts : int Closure_origin.Map.t;
    actively_unrolling : int Set_of_closures_origin.Map.t;
    closure_depth : int;
    inlining_stats_closure_stack : Inlining_stats.Closure_stack.t;
    inlined_debuginfo : Debuginfo.t;
  }

  let create ~never_inline ~allow_continuation_inlining
        ~allow_continuation_specialisation ~round ~backend
        ~simplify_toplevel ~simplify_expr
        ~simplify_continuation_use_cannot_inline =
    (* XXX [resolver] should come from [backend] *)
    let resolver _export_id = None in
    { backend;
      round;
      simplify_toplevel;
      simplify_expr;
      simplify_continuation_use_cannot_inline;
      typing_environment = TE.create ~resolver;
      mutable_variables = Mutable_variable.Map.empty;
      continuations = Continuation.Map.empty;
      continuation_scope_level = Scope_level.initial;
(*      projections = Projection.Map.empty; *)
      current_functions = Set_of_closures_origin.Set.empty;
      inlining_level = 0;
      inside_branch = 0;
      freshening = Freshening.empty;
      never_inline;
      never_inline_inside_closures = false;
      never_inline_outside_closures = false;
      allow_continuation_inlining;
      allow_continuation_specialisation;
      unroll_counts = Set_of_closures_origin.Map.empty;
      inlining_counts = Closure_origin.Map.empty;
      actively_unrolling = Set_of_closures_origin.Map.empty;
      closure_depth = 0;
      inlining_stats_closure_stack =
        Inlining_stats.Closure_stack.create ();
      inlined_debuginfo = Debuginfo.none;
    }

  let print_scope_level_and_continuation_approx ppf (level, approx) =
    Format.fprintf ppf "@[((scope_level %a)@ (approx %a))@]"
      Scope_level.print level
      Continuation_approx.print approx

  let print ppf t =
    Format.fprintf ppf "@[(\
        @[(round@ %d)@]@ \
        @[(typing_environment@ %a)@]@ \
        @[(mutable_variables@ %a)@]@ \
        @[(continuations@ %a)@]@ \
        @[(continuation_scope_level@ %a)@]@ \
        @[(current_functions@ %a)@]@ \
        @[(inlining_level@ %d)@]@ \
        @[(inside_branch@ %d)@]@ \
        @[(freshening@ %a)@]@ \
        @[(never_inline@ %b)@]@ \
        @[(never_inline_inside_closures@ %b)@]@ \
        @[(never_inline_outside_closures@ %b)@]@ \
        @[(allow_continuation_inlining@ %b)@]@ \
        @[(allow_continuation_specialisation@ %b)@]@ \
        @[(unroll_counts@ %a)@]@ \
        @[(inlining_counts@ %a)@]@ \
        @[(actively_unrolling@ %a)@]@ \
        @[(closure_depth@ %d)@]@ \
        @[(inlining_stats_closure_stack@ %a)@]@ \
        @[(inlined_debuginfo@ %a)@]\
        )@]"
      t.round
      TE.print t.typing_environment
      (Mutable_variable.Map.print T.print) t.mutable_variables
      (Continuation.Map.print print_scope_level_and_continuation_approx)
        t.continuations
      Scope_level.print t.continuation_scope_level
      Set_of_closures_origin.Set.print t.current_functions
      t.inlining_level
      t.inside_branch
      Freshening.print t.freshening
      t.never_inline
      t.never_inline_inside_closures
      t.never_inline_outside_closures
      t.allow_continuation_inlining
      t.allow_continuation_specialisation
      (Set_of_closures_origin.Map.print Format.pp_print_int) t.unroll_counts
      (Closure_origin.Map.print Format.pp_print_int) t.inlining_counts
      (Set_of_closures_origin.Map.print Format.pp_print_int)
        t.actively_unrolling
      t.closure_depth
      Inlining_stats.Closure_stack.print t.inlining_stats_closure_stack
      Debuginfo.print t.inlined_debuginfo

  let resolver t = TE.resolver t.typing_environment
  let backend t = t.backend
  let round t = t.round
  let simplify_toplevel t = t.simplify_toplevel
  let simplify_expr t = t.simplify_expr
  let simplify_continuation_use_cannot_inline t =
    t.simplify_continuation_use_cannot_inline
  let continuation_scope_level t = t.continuation_scope_level

  let increment_continuation_scope_level t =
    { t with
      continuation_scope_level = Scope_level.next t.continuation_scope_level;
    }

  let decrement_continuation_scope_level t =
    { t with
      continuation_scope_level = Scope_level.prev t.continuation_scope_level;
    }

  let const_float_prop _t =
    (* CR mshinwell: Does this need to be in the environment?
       Also, the naming should be made consistent with Clflags *)
    !Clflags.float_const_prop

  let get_typing_environment t = t.typing_environment

  let replace_typing_environment t typing_environment =
    { t with typing_environment; }

  let extend_typing_environment t ~env_extension =
    let typing_environment =
      TE.add_or_meet_env_extension t.typing_environment env_extension
        (TE.max_level t.typing_environment)
    in
    { t with typing_environment; }

  (* CR mshinwell: Refactor [t] such there is one field which contains
     everything to be cleared when we do [local]. *)
  let local env =
    { env with
      typing_environment = TE.restrict_to_symbols env.typing_environment;
      continuations = Continuation.Map.empty;
(*      projections = Projection.Map.empty; *)
      freshening = Freshening.empty_preserving_activation_state env.freshening;
      inlined_debuginfo = Debuginfo.none;
      continuation_scope_level = Scope_level.initial;
    }

  let find_variable0 typing_environment var =
    let ty, binding_type = TE.find_exn typing_environment (Name.var var) in
    match binding_type with
    | Normal -> ty
    | Was_existential ->
      Misc.fatal_errorf "Variable %a was existentially bound"
        Variable.print var

  let find_variable t var =
    find_variable0 t.typing_environment var

  let find_variable_opt t var =
    match TE.find_opt t.typing_environment (Name.var var) with
    | None -> None
    | Some (ty, binding_type) ->
      match binding_type with
      | Normal -> Some ty
      | Was_existential ->
        Misc.fatal_errorf "Variable %a was existentially bound"
          Variable.print var

  let scope_level_of_name t name =
    Scope_level.With_sublevel.level (
      TE.scope_level_exn t.typing_environment name)

  let mem_variable t var =
    match find_variable_opt t var with
    | None -> false
    | Some _ -> true

  let add_variable t var ty =
    let typing_environment =
      TE.add t.typing_environment (Name.var var)
        t.continuation_scope_level (Definition ty)
    in
    { t with typing_environment; }

  (* CR mshinwell: rename "replace_meet" *)

  let replace_meet_variable t var ty =
    let typing_environment =
      TE.add_equation t.typing_environment (Name.var var) ty
    in
    { t with typing_environment; }

  let add_or_replace_meet_variable t var ty =
    let name = Name.var var in
    if TE.mem t.typing_environment name then
      replace_meet_variable t var ty
    else
      add_variable t var ty

  let add_symbol t sym ty =
    let typing_environment =
      TE.add t.typing_environment (Name.symbol sym) t.continuation_scope_level
        (Definition ty)
    in
    { t with typing_environment; }

  let add_equation_symbol t sym ty =
    let typing_environment =
      TE.add_equation t.typing_environment (Name.symbol sym) ty
    in
    { t with typing_environment; }

  let find_symbol0 typing_environment sym =
    let ty, binding_type =
      TE.find_exn typing_environment (Name.symbol sym)
    in
    match binding_type with
    | Normal -> ty
    | Was_existential ->
      Misc.fatal_errorf "Symbol %a is existentially bound"
        Symbol.print sym

  let find_symbol t sym =
    find_symbol0 t.typing_environment sym

  let find_symbol_opt t sym =
    match TE.find_opt t.typing_environment (Name.symbol sym) with
    | None -> None
    | Some (ty, binding_type) ->
      match binding_type with
      | Normal -> Some ty
      | Was_existential ->
        Misc.fatal_errorf "Symbol %a is existentially bound"
          Symbol.print sym

  (* CR mshinwell: Use this code as a basis for the find-by-variable/symbol
     functions, above *)
  let find_name0 typing_environment name =
    let ty, binding_type =
      match TE.find_exn typing_environment name with
      | exception Not_found ->
        Misc.fatal_errorf "Unbound name %a@ in@ %a"
          Name.print name
          TE.print typing_environment
      | result -> result
    in
    match binding_type with
    | Normal -> ty
    | Was_existential ->
      Misc.fatal_errorf "Name %a is existentially bound"
        Name.print name

  let find_name t name =
    find_name0 t.typing_environment name

  let mem_symbol t sym =
    match find_symbol_opt t sym with
    | None -> false
    | Some _ -> true

  let mem_name t (name : Name.t) =
    match name with
    | Var var -> mem_variable t var
    | Symbol sym -> mem_symbol t sym

  let mem_simple t (simple : Simple.t) =
    match simple with
    | Name name -> mem_name t name
    | Const _ | Discriminant _ -> true

  (* CR mshinwell: Rename this function---it takes a meet, not redefines *)
  let redefine_symbol t sym ty =
    let name = Name.symbol sym in
    if not (TE.mem t.typing_environment name) then
      Misc.fatal_errorf "Symbol %a cannot be redefined when it is not \
          already defined"
        Symbol.print sym
    else
      add_equation_symbol t sym ty

  let add_mutable t mut_var ty =
    { t with mutable_variables =
      Mutable_variable.Map.add mut_var ty t.mutable_variables;
    }

  let add_cse t prim ~bound_to =
    let typing_environment =
      TE.add t.typing_environment bound_to t.continuation_scope_level
        (CSE prim)
    in
    { t with typing_environment; }

  let find_cse t prim =
    TE.find_cse t.typing_environment prim

  let add_continuation t cont ty =
    let continuations =
      Continuation.Map.add cont (t.continuation_scope_level, ty)
        t.continuations
    in
    { t with
      continuations;
    }

  let find_continuation t cont =
    match Continuation.Map.find cont t.continuations with
    | exception Not_found ->
      Misc.fatal_errorf "Unbound continuation %a in environment:@ %a"
        Continuation.print cont
        print t
    | (_scope_level, ty) -> ty

  let scope_level_of_continuation t cont =
    match Continuation.Map.find cont t.continuations with
    | exception Not_found ->
      Misc.fatal_errorf "Unbound continuation %a in environment:@ %a"
        Continuation.print cont
        print t
    | (scope_level, _ty) -> scope_level

  let mem_continuation t cont =
    Continuation.Map.mem cont t.continuations

(*
  let does_not_bind t vars =
    not (List.exists (fun var -> mem t var) vars)

  let does_not_freshen t vars =
    Freshening.does_not_freshen t.freshening vars
*)

  let find_mutable_exn t mut_var =
    try Mutable_variable.Map.find mut_var t.mutable_variables
    with Not_found ->
      Misc.fatal_errorf "Env.find_mutable_exn: Unbound variable \
          %a@.%s@. Environment: %a@."
        Mutable_variable.print mut_var
        (Printexc.raw_backtrace_to_string (Printexc.get_callstack max_int))
        print t
(*
  let find_list_exn t vars =
    List.map (fun var -> find_exn t var) vars

  let variables_in_scope t = Variable.Map.keys t.variables

  let find_opt t id =
    try Some (really_import_ty t
                (snd (Variable.Map.find id t.variables)))
    with Not_found -> None
*)

  let activate_freshening t =
    { t with freshening = Freshening.activate t.freshening }

  (* CR-someday mshinwell: consider changing name to remove "declaration".
     Also, isn't this the inlining stack?  Maybe we can use that instead. *)
  let enter_set_of_closures_declaration t origin =
  (*
  Format.eprintf "Entering decl: have %a, adding %a, result %a\n%!"
  Set_of_closures_origin.Set.print t.current_functions
  Set_of_closures_origin.print origin
  Set_of_closures_origin.Set.print
    (Set_of_closures_origin.Set.add origin t.current_functions);
  *)
    { t with
      current_functions =
        Set_of_closures_origin.Set.add origin t.current_functions; }

  let inside_set_of_closures_declaration origin t =
    Set_of_closures_origin.Set.mem origin t.current_functions

  let at_toplevel t =
    t.closure_depth = 0

  let is_inside_branch env = env.inside_branch > 0

  let branch_depth env = env.inside_branch

  let inside_branch t =
    { t with inside_branch = t.inside_branch + 1 }

  let set_freshening t freshening  =
    { t with freshening; }

  let increase_closure_depth t =
    { t with
      closure_depth = t.closure_depth + 1;
    }

  let set_never_inline t =
    if t.never_inline then t
    else { t with never_inline = true }

  let set_never_inline_inside_closures t =
    if t.never_inline_inside_closures then t
    else { t with never_inline_inside_closures = true }

  let unset_never_inline_inside_closures t =
    if t.never_inline_inside_closures then
      { t with never_inline_inside_closures = false }
    else t

  let set_never_inline_outside_closures t =
    if t.never_inline_outside_closures then t
    else { t with never_inline_outside_closures = true }

  let unset_never_inline_outside_closures t =
    if t.never_inline_outside_closures then
      { t with never_inline_outside_closures = false }
    else t

  let inlining_level_up env =
    let max_level =
      Clflags.Int_arg_helper.get ~key:(env.round) !Clflags.inline_max_depth
    in
    if (env.inlining_level + 1) > max_level then begin
      (* CR mshinwell: Is this a helpful error?  Should we just make this
         robust? *)
      Misc.fatal_error "Inlining level increased above maximum"
    end;
    { env with inlining_level = env.inlining_level + 1 }

  let actively_unrolling t origin =
    match Set_of_closures_origin.Map.find origin t.actively_unrolling with
    | count -> Some count
    | exception Not_found -> None

  let start_actively_unrolling t origin i =
    let actively_unrolling =
      Set_of_closures_origin.Map.add origin i t.actively_unrolling
    in
    { t with actively_unrolling }

  let continue_actively_unrolling t origin =
    let unrolling =
      try
        Set_of_closures_origin.Map.find origin t.actively_unrolling
      with Not_found ->
        Misc.fatal_error "Unexpected actively unrolled function";
    in
    let actively_unrolling =
      Set_of_closures_origin.Map.add origin (unrolling - 1) t.actively_unrolling
    in
    { t with actively_unrolling }

  let unrolling_allowed t origin =
    let unroll_count =
      try
        Set_of_closures_origin.Map.find origin t.unroll_counts
      with Not_found ->
        Clflags.Int_arg_helper.get
          ~key:t.round !Clflags.inline_max_unroll
    in
    unroll_count > 0

  let inside_unrolled_function t origin =
    let unroll_count =
      try
        Set_of_closures_origin.Map.find origin t.unroll_counts
      with Not_found ->
        Clflags.Int_arg_helper.get
          ~key:t.round !Clflags.inline_max_unroll
    in
    let unroll_counts =
      Set_of_closures_origin.Map.add
        origin (unroll_count - 1) t.unroll_counts
    in
    { t with unroll_counts }

  let inlining_allowed t id =
    let inlining_count =
      try
        Closure_origin.Map.find id t.inlining_counts
      with Not_found ->
        max 1 (Clflags.Int_arg_helper.get
                 ~key:t.round !Clflags.inline_max_unroll)
    in
    inlining_count > 0

  let inside_inlined_function t id =
    let inlining_count =
      try
        Closure_origin.Map.find id t.inlining_counts
      with Not_found ->
        max 1 (Clflags.Int_arg_helper.get
                 ~key:t.round !Clflags.inline_max_unroll)
    in
    let inlining_counts =
      Closure_origin.Map.add id (inlining_count - 1) t.inlining_counts
    in
    { t with inlining_counts }

  let inlining_level t = t.inlining_level
  let freshening t = t.freshening
  let never_inline t = t.never_inline || t.never_inline_outside_closures

  let disallow_continuation_inlining t =
    { t with allow_continuation_inlining = false; }

  let never_inline_continuations t =
    not t.allow_continuation_inlining

  let disallow_continuation_specialisation t =
    { t with allow_continuation_specialisation = false; }

  let never_specialise_continuations t =
    not t.allow_continuation_specialisation

  (* CR mshinwell: may want to split this out properly *)
  let never_unbox_continuations = never_specialise_continuations

  let note_entering_closure t ~closure_id ~dbg =
    if t.never_inline then t
    else
      { t with
        inlining_stats_closure_stack =
          Inlining_stats.Closure_stack.note_entering_closure
            t.inlining_stats_closure_stack ~closure_id ~dbg;
      }

  let note_entering_call t ~closure_id ~dbg =
    if t.never_inline then t
    else
      { t with
        inlining_stats_closure_stack =
          Inlining_stats.Closure_stack.note_entering_call
            t.inlining_stats_closure_stack ~closure_id ~dbg;
      }

  let note_entering_inlined t =
    if t.never_inline then t
    else
      { t with
        inlining_stats_closure_stack =
          Inlining_stats.Closure_stack.note_entering_inlined
            t.inlining_stats_closure_stack;
      }

  let note_entering_specialised t ~closure_ids =
    if t.never_inline then t
    else
      { t with
        inlining_stats_closure_stack =
          Inlining_stats.Closure_stack.note_entering_specialised
            t.inlining_stats_closure_stack ~closure_ids;
      }

  let enter_closure t ~closure_id ~inline_inside ~dbg ~f =
    let t =
      if inline_inside && not t.never_inline_inside_closures then t
      else set_never_inline t
    in
    let t = unset_never_inline_outside_closures t in
    f (note_entering_closure t ~closure_id ~dbg)

  let record_decision t decision =
    Inlining_stats.record_decision decision
      ~closure_stack:t.inlining_stats_closure_stack

  let set_inline_debuginfo t ~dbg =
    { t with inlined_debuginfo = dbg }

  let add_inlined_debuginfo t ~dbg =
    Debuginfo.concat t.inlined_debuginfo dbg

  let continuations_in_scope t =
    Continuation.Map.map (fun (_scope_level, approx) -> approx)
      t.continuations

  let invariant t =
    if !Clflags.flambda_invariant_checks then begin
      (* Make sure that freshening a continuation through the given
         environment doesn't yield a continuation not bound by the
         environment. *)
      let from_freshening =
        Freshening.range_of_continuation_freshening t.freshening
      in
      Continuation.Set.iter (fun cont ->
          match Continuation.Map.find cont t.continuations with
          | exception Not_found ->
            Misc.fatal_errorf "The freshening in this environment maps to \
                continuation %a, but that continuation is unbound:@;%a"
              Continuation.print cont
              print t
          | _ -> ())
        from_freshening
    end
end and Result : sig
  include Simplify_env_and_result_intf.Result with type env = Env.t
end = struct
  module Roll_back_after_speculation = struct
    type t = {
      used_continuations : Continuation_uses.t Continuation.Map.t;
      defined_continuations :
        (Continuation_uses.t * Continuation_approx.t * Env.t
            * Flambda.recursive)
          Continuation.Map.t;
      lifted_constants :
        (Flambda_type.t * Flambda_kind.t * Flambda_static.Static_part.t)
          Symbol.Map.t;
    }

    let continuations_defined_between_snapshots ~before ~after =
      Continuation.Set.diff
        (Continuation.Map.keys after.defined_continuations)
        (Continuation.Map.keys before.defined_continuations)
  end

  type env = Env.t

  type t =
    { resolver : (Export_id.t -> Flambda_type.t option);
      (* CR mshinwell: What about combining these next two? *)
      used_continuations : Continuation_uses.t Continuation.Map.t;
      defined_continuations :
        (Continuation_uses.t * Continuation_approx.t * Env.t
            * Flambda.recursive)
          Continuation.Map.t;
      inlining_threshold : Inlining_cost.Threshold.t option;
      benefit : Inlining_cost.Benefit.t;
      num_direct_applications : int;
      env_extension : T.Typing_env_extension.t;
      newly_imported_symbols : Flambda_kind.t Symbol.Map.t;
      lifted_constants :
        (Flambda_type.t * Flambda_kind.t * Flambda_static.Static_part.t)
          Symbol.Map.t;
    }

  let create ~resolver =
    { resolver;
      used_continuations = Continuation.Map.empty;
      defined_continuations = Continuation.Map.empty;
      inlining_threshold = None;
      benefit = Inlining_cost.Benefit.zero;
      num_direct_applications = 0;
      env_extension = T.Typing_env_extension.empty;
      newly_imported_symbols = Symbol.Map.empty;
      lifted_constants = Symbol.Map.empty;
    }

  let union env t1 t2 =
    { resolver = t1.resolver;
      used_continuations =
        Continuation.Map.union_merge Continuation_uses.union
          t1.used_continuations t2.used_continuations;
      defined_continuations =
        Continuation.Map.disjoint_union
          t1.defined_continuations t2.defined_continuations;
      inlining_threshold = t1.inlining_threshold;
      benefit = Inlining_cost.Benefit.(+) t1.benefit t2.benefit;
      num_direct_applications =
        t1.num_direct_applications + t2.num_direct_applications;
      env_extension = T.Typing_env_extension.meet env
        t1.env_extension t2.env_extension;
      newly_imported_symbols =
        Symbol.Map.disjoint_union t1.newly_imported_symbols
          t2.newly_imported_symbols;
      lifted_constants =
        Symbol.Map.disjoint_union t1.lifted_constants
          t2.lifted_constants;
    }

  (* XXX  -- in fact this may now be needed now
  let update_use_environments t ~if_present_in_env ~then_add_to_env =
    let application_points =
      List.map (fun (use : Use.t) ->
          if Env.mem_continuation use.env if_present_in_env then
            let new_cont, approx = then_add_to_env in
            let env = Env.add_continuation use.env new_cont approx in
            { use with env; }
          else
            use)
        t.application_points
    in
    { t with application_points; }
  *)

  let use_continuation t env cont ~params kind =
    let args = Continuation_uses.Use.Kind.args kind in
    if not (List.for_all (fun arg -> Env.mem_simple env arg) args) then begin
      Misc.fatal_errorf "use_continuation %a: argument(s) (%a) not in \
          environment:@ %a"
        Continuation.print cont
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Simple.print) args
        Env.print env
    end;
(*
  let k = 34 in
  if Continuation.to_int cont = k then begin
  Format.eprintf "Adding use of continuation k%d, args %a approxs %a:\n%s\
      \nEnv:@ %a\n\n%!"
    k
    Simple.List.print args
    (Format.pp_print_list Flambda_type.print)
    (Continuation_uses.Use.Kind.arg_tys kind)
    (Printexc.raw_backtrace_to_string (Printexc.get_callstack 20))
    Env.print env
  end;
*)
    let uses =
      match Continuation.Map.find cont t.used_continuations with
      | exception Not_found ->
        Continuation_uses.create ~continuation:cont ~params
          ~definition_scope_level:(Env.scope_level_of_continuation env cont)
      | uses -> uses
    in
    let uses =
      Continuation_uses.add_use uses (Env.get_typing_environment env) kind
    in
  (*
  if Continuation.to_int cont = k then begin
  Format.eprintf "Join of args approxs for k%d: %a\n%!"
    k
    (Format.pp_print_list Flambda_type.print)
    (Continuation_uses.meet_of_args_approxs uses ~num_params:1)
  end;
  *)
    { t with
      used_continuations =
        Continuation.Map.add cont uses t.used_continuations;
    }

  let non_recursive_continuations_used_linearly_in_inlinable_position t =
    let used_linearly =
      Continuation.Map.filter (fun _cont (uses, _approx, _env, recursive) ->
  (*
  Format.eprintf "NRCUL: continuation %a number of uses %d\n%!"
  Continuation.print _cont
  (List.length uses.Continuation_uses.application_points);
  *)
          match (recursive : Flambda.recursive) with
          | Non_recursive ->
            Continuation_uses.linearly_used_in_inlinable_position uses
          | Recursive -> false)
        t.defined_continuations
    in
    Continuation.Map.keys used_linearly

  let forget_continuation_definition t cont =
    { t with
      defined_continuations =
        Continuation.Map.remove cont t.defined_continuations;
    }

  let is_used_continuation t i =
    Continuation.Map.mem i t.used_continuations

  let used_continuations t =
    Continuation.Map.keys t.used_continuations

  let continuation_uses t = t.used_continuations

  let continuation_uses_for t cont =
    Continuation.Map.find cont t.used_continuations

  let no_continuations_in_scope t =
    Continuation.Map.is_empty t.used_continuations

  let snapshot_continuation_uses t =
    { Roll_back_after_speculation.
      used_continuations = t.used_continuations;
      defined_continuations = t.defined_continuations;
      lifted_constants = t.lifted_constants;
    }

  let snapshot_and_forget_continuation_uses t =
    let snapshot = snapshot_continuation_uses t in
    let t =
      { t with
        used_continuations = Continuation.Map.empty;
        defined_continuations = Continuation.Map.empty;
      }
    in
    snapshot, t

  (* CR mshinwell: Rename these snapshot/rename functions *)
  let roll_back_continuation_uses t (snapshot : Roll_back_after_speculation.t) =
    { t with
      used_continuations = snapshot.used_continuations;
      defined_continuations = snapshot.defined_continuations;
      lifted_constants = snapshot.lifted_constants;
    }

  let continuation_unused t cont =
    not (Continuation.Map.mem cont t.used_continuations)

  let continuation_defined t cont =
    Continuation.Map.mem cont t.defined_continuations

  let continuation_args_types t cont ~arity ~freshening ~default_env =
    match Continuation.Map.find cont t.used_continuations with
    | exception Not_found ->
(*
      Format.eprintf "No uses of continuation %a\n%!" Continuation.print cont;
*)
      let tys = List.map (fun kind -> T.bottom kind) arity in
      tys, default_env
    | uses ->
(*
      Format.eprintf "Continuation uses for %a:@ %a\n%!"
        Continuation.print cont
        Continuation_uses.print uses;
*)
      let tys, env, _env_extension =
        Join_point.param_types_and_body_env uses freshening ~arity
          ~default_env
      in
      tys, env
  
  let defined_continuation_args_types t cont ~arity ~freshening ~default_env =
    match Continuation.Map.find cont t.defined_continuations with
    | exception Not_found ->
      let tys = List.map (fun kind -> T.bottom kind) arity in
      tys, default_env
    | (uses, _approx, _env, _recursive) ->
      let tys, env, _env_extension =
        Join_point.param_types_and_body_env uses ~arity freshening ~default_env
      in
      tys, env

  let exit_scope_of_let_cont t env cont ~params =
    let t, uses =
      match Continuation.Map.find cont t.used_continuations with
      | exception Not_found ->
        let uses =
          Continuation_uses.create ~continuation:cont ~params
            ~definition_scope_level:(Env.scope_level_of_continuation env cont)
        in
        t, uses
      | uses ->
        { t with
          used_continuations =
            Continuation.Map.remove cont t.used_continuations;
        }, uses
    in
    assert (continuation_unused t cont);
    t, uses

  let update_all_continuation_use_environments _t ~if_present_in_env:_
        ~then_add_to_env:_ =
    Misc.fatal_error "Not yet implemented"
(* XXX think about this
    let used_continuations =
      Continuation.Map.map (fun uses ->
            Continuation_uses.update_use_environments uses
              ~if_present_in_env ~then_add_to_env)
        t.used_continuations
    in
    let defined_continuations =
      Continuation.Map.map (fun (uses, approx, env, recursive) ->
          let uses =
            Continuation_uses.update_use_environments uses
              ~if_present_in_env ~then_add_to_env
          in
          uses, approx, env, recursive)
        t.defined_continuations
    in
    { t with
      used_continuations;
      defined_continuations;
    }
*)

  let update_continuation_parameters t cont
        ~params =
    let used_continuations =
      Continuation.Map.mapi (fun cont' uses ->
          if not (Continuation.equal cont cont') then uses
          else
            Continuation_uses.update_parameters
              uses ~params)
        t.used_continuations
    in
    let defined_continuations =
      Continuation.Map.mapi (fun cont' (uses, approx, env, recursive) ->
          if not (Continuation.equal cont cont') then
            uses, approx, env, recursive
          else
            let uses =
              Continuation_uses.update_parameters
                uses ~params
            in
            uses, approx, env, recursive)
        t.defined_continuations
    in
    { t with
      used_continuations;
      defined_continuations;
    }

  let define_continuation t cont env recursive uses approx =
  (*    Format.eprintf "define_continuation %a\n%!" Continuation.print cont;*)
  (*
  let k = 25987 in
  if Continuation.to_int cont = k then begin
  Format.eprintf "Defining continuation k%d:\n%s%!"
    k
    (Printexc.raw_backtrace_to_string (Printexc.get_callstack 30))
  end;
  *)
    Env.invariant env;
    if Continuation.Map.mem cont t.used_continuations then begin
      Misc.fatal_errorf "Must call exit_scope_catch before \
          define_continuation %a"
        Continuation.print cont
    end;
    if Continuation.Map.mem cont t.defined_continuations then begin
      Misc.fatal_errorf "Cannot redefine continuation %a"
        Continuation.print cont
    end;
    { t with
      defined_continuations =
        Continuation.Map.add cont (uses, approx, env, recursive)
          t.defined_continuations;
    }

  let update_defined_continuation_approx t cont approx =
    match Continuation.Map.find cont t.defined_continuations with
    | exception Not_found ->
      Misc.fatal_errorf "Cannot update approximation of undefined \
          continuation %a"
        Continuation.print cont
    | (uses, _old_approx, env, recursive) ->
      { t with
        defined_continuations =
          Continuation.Map.add cont (uses, approx, env, recursive)
            t.defined_continuations;
      }

  let continuation_definitions_with_uses t =
    t.defined_continuations

  let map_benefit t f =
    { t with benefit = f t.benefit }

  let add_benefit t b =
    { t with benefit = Inlining_cost.Benefit.(+) t.benefit b }

  let benefit t = t.benefit

  let reset_benefit t =
    { t with benefit = Inlining_cost.Benefit.zero; }

  let set_inlining_threshold t inlining_threshold =
    { t with inlining_threshold }

  let add_inlining_threshold t j =
    match t.inlining_threshold with
    | None -> t
    | Some i ->
      let inlining_threshold = Some (Inlining_cost.Threshold.add i j) in
      { t with inlining_threshold }

  let sub_inlining_threshold t j =
    match t.inlining_threshold with
    | None -> t
    | Some i ->
      let inlining_threshold = Some (Inlining_cost.Threshold.sub i j) in
      { t with inlining_threshold }

  let inlining_threshold t = t.inlining_threshold

  let seen_direct_application t =
    { t with num_direct_applications = t.num_direct_applications + 1; }

  let num_direct_applications t =
    t.num_direct_applications

  let clear_env_extension t =
    { t with
      env_extension = T.Typing_env_extension.empty;
    }

  (* CR mshinwell: delete [scope_level] *)
  let add_or_meet_equation t name _scope_level ty =
    let env_extension =
      T.Typing_env_extension.add_equation t.env_extension name ty
    in
    { t with
      env_extension;
    }

  let add_or_meet_env_extension t env env_extension =
    let env_extension =
      T.Typing_env_extension.meet env t.env_extension env_extension
    in
    { t with
      env_extension;
    }

  let add_cse t prim ~bound_to =
    let env_extension =
      T.Typing_env_extension.add_cse t.env_extension bound_to prim
    in
    { t with
      env_extension;
    }

  let get_env_extension t = t.env_extension

  (* CR mshinwell: There should be a function here which records the new
     imports in [newly_imported_symbols]. *)

  let newly_imported_symbols t = t.newly_imported_symbols

  let new_lifted_constant t ~name ty static_part =
    let kind = T.kind ty in
    let symbol =
      Symbol.create (Compilation_unit.get_current_exn ())
        (Linkage_name.create name)
    in
    let t =
      { t with
        lifted_constants =
          Symbol.Map.add symbol (ty, kind, static_part) t.lifted_constants;
      }
    in
    symbol, t

  let get_lifted_constants t = t.lifted_constants
end
