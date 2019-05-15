(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
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

  type t = {
    backend : (module Backend_intf.S);
    round : int;
    typing_environment : TE.t;
    continuations : (Scope_level.t * Continuation_in_env.t) Continuation.Map.t;
    continuation_scope_level : Scope_level.t;
    scope_level_for_lifted_constants : Scope_level.t;
    inlined_debuginfo : Debuginfo.t;
    never_inline : bool;
  }

  let invariant _t = ()

  let create ~never_inline ~round ~backend ~scope_level_for_lifted_constants =
    (* XXX [resolver] should come from [backend] *)
    let resolver _export_id = None in
    { backend;
      round;
      typing_environment = TE.create ~resolver;
      continuations = Continuation.Map.empty;
      continuation_scope_level = Scope_level.initial;
      scope_level_for_lifted_constants;
      inlined_debuginfo = Debuginfo.none;
      never_inline;
    }

  let print ppf { backend = _; round; typing_environment; continuations;
                  continuation_scope_level; scope_level_for_lifted_constants;
                  inlined_debuginfo; never_inline; } =
    Format.fprintf ppf "@[(\
        @[(round@ %d)@]@ \
        @[(typing_environment@ %a)@]@ \
        @[(continuations@ %a)@]@ \
        @[(continuation_scope_level@ %a)@]@ \
        @[(scope_level_for_lifted_constants@ %a)@]@ \
        @[(inlined_debuginfo@ %a)@]\
        @[(never_inline@ %b)@]@ \
        )@]"
      t.round
      TE.print t.typing_environment
      (Continuation.Map.print Continuation_in_env.print) t.continuations
      Scope_level.print t.continuation_scope_level
      Scope_level.print t.scope_level_for_lifted_constants
      Debuginfo.print t.inlined_debuginfo
      t.never_inline

  let resolver t = TE.resolver t.typing_environment
  let backend t = t.backend
  let round t = t.round
  let continuation_scope_level t = t.continuation_scope_level

  let increment_continuation_scope_level t =
    { t with
      continuation_scope_level = Scope_level.next t.continuation_scope_level;
    }

  let get_typing_environment t = t.typing_environment

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

  let set_never_inline t =
    if t.never_inline then t
    else { t with never_inline = true }

  let never_inline t = t.never_inline

  let note_entering_closure t ~closure_id ~dbg =
    if t.never_inline then t
    else
      { t with
        inlining_stats_closure_stack =
          Inlining_stats.Closure_stack.note_entering_closure
            t.inlining_stats_closure_stack ~closure_id ~dbg;
      }

  let enter_closure t ~closure_id ~inline_inside ~dbg ~f =
    let t =
      if inline_inside && not t.never_inline_inside_closures then t
      else set_never_inline t
    in
    let t = unset_never_inline_outside_closures t in
    f (note_entering_closure t ~closure_id ~dbg)

  let add_inlined_debuginfo t ~dbg =
    Debuginfo.concat t.inlined_debuginfo dbg

  let scope_level_for_lifted_constants t = t.scope_level_for_lifted_constants

  let set_scope_level_for_lifted_constants t scope_level =
    { t with scope_level_for_lifted_constants = scope_level; }
end and Result : sig
  include Simplify_env_and_result_intf.Result with type env = Env.t
end = struct
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
      env_extension : T.Typing_env_extension.t;
      newly_imported_symbols : Flambda_kind.t Symbol.Map.t;
      lifted_constants :
        (Flambda_type.t * Flambda_kind.t * Flambda_static.Static_part.t)
          Symbol.Map.t;
    }

(* XXX Need to ensure that newly_imported_symbols doesn't contain predef exn
   symbols *)

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

  let use_continuation t env cont ~params kind =
    let args = Continuation_uses.Use.Kind.args kind in
    if not (List.for_all (fun arg -> Env.mem_simple env arg) args) then begin
      Misc.fatal_errorf "use_continuation %a: argument(s) (%a) not in \
          environment:@ %a"
        Continuation.print cont
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Simple.print) args
        Env.print env
    end;
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
    { t with
      used_continuations =
        Continuation.Map.add cont uses t.used_continuations;
    }

  let map_benefit t f =
    { t with benefit = f t.benefit }

  let clear_env_extension t =
    { t with
      env_extension = T.Typing_env_extension.empty;
    }

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
