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

module KP = Kinded_parameter
module T = Flambda_type
module TE = Flambda_type.Typing_env

type lifted_constants =
  (Symbol.t * (Flambda_type.t * Flambda_static.Static_part.t)) list

module rec Env : sig
  include Simplify_env_and_result_intf.Env
    with type result = Result.t
end = struct
  type result = Result.t

  type t = {
    backend : (module Backend_intf.S);
    round : int;
    typing_env : TE.t;
    continuations : (Scope_level.t * Continuation_in_env.t) Continuation.Map.t;
    exn_continuations : Scope_level.t Exn_continuation.Map.t;
    continuation_aliases : Continuation.t Continuation.Map.t;
    continuation_scope_level : Scope_level.t;
    scope_level_for_lifted_constants : Scope_level.t;
    inlined_debuginfo : Debuginfo.t;
    can_inline : bool;
  }

  let invariant _t = ()

  let create ~round ~backend ~scope_level_for_lifted_constants =
    (* CR mshinwell: [resolver] should come from [backend] *)
    let resolver _export_id = None in
    { backend;
      round;
      typing_env = TE.create ~resolver;
      continuations = Continuation.Map.empty;
      exn_continuations = Exn_continuation.Map.empty;
      continuation_aliases = Continuation.Map.empty;
      continuation_scope_level = Scope_level.initial;
      scope_level_for_lifted_constants;
      inlined_debuginfo = Debuginfo.none;
      can_inline = false;
    }

  let print_scope_level_and_continuation_in_env ppf (scope_level, cont_in_env) =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(scope_level %a)@]@ \
        @[<hov 1>(cont_in_env %a)@]\
        )@]"
      Scope_level.print scope_level
      Continuation_in_env.print cont_in_env

  let print ppf { backend = _; round; typing_env; continuations;
                  continuation_scope_level; scope_level_for_lifted_constants;
                  inlined_debuginfo; can_inline;
                } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(round@ %d)@]@ \
        @[<hov 1>(typing_env@ %a)@]@ \
        @[<hov 1>(continuations@ %a)@]@ \
        @[<hov 1>(exn_continuations@ %a)@]@ \
        @[<hov 1>(continuation_scope_level@ %a)@]@ \
        @[<hov 1>(continuation_aliases@ %a)@]@ \
        @[<hov 1>(scope_level_for_lifted_constants@ %a)@]@ \
        @[<hov 1>(inlined_debuginfo@ %a)@]@ \
        @[<hov 1>(can_inline@ %b)@]\
        )@]"
      round
      TE.print typing_env
      (Continuation.Map.print print_scope_level_and_continuation_in_env)
      continuations
      (Exn_continuation.Map.print Scope_level.print) continuations
      (Continuation.Map.print Continuation.print) continuation_aliases
      Scope_level.print continuation_scope_level
      Scope_level.print scope_level_for_lifted_constants
      Debuginfo.print inlined_debuginfo
      can_inline

  let resolver t = TE.resolver t.typing_env
  let backend t = t.backend
  let typing_env t = t.typing_env
  let round t = t.round
  let continuation_scope_level t = t.continuation_scope_level
  let can_inline t = t.can_inline

  let increment_continuation_scope_level t =
    { t with
      continuation_scope_level = Scope_level.next t.continuation_scope_level;
    }

  let enter_closure { backend = _; round; typing_env; continuations;
                      continuation_scope_level;
                      scope_level_for_lifted_constants; inlined_debuginfo;
                    } =
    { backend;
      round;
      typing_env = TE.restrict_to_symbols env.typing_env;
      continuations = Continuation.Map.empty;
      exn_continuations = Exn_continuation.Map.empty;
      continuation_aliases = Continuation.Map.empty;
      continuation_scope_level = Scope_level.initial;
      scope_level_for_lifted_constants;
      inlined_debuginfo = Debuginfo.none;
    }

  let add_variable t var ty =
    let typing_env =
      TE.add t.typing_env (Name.var var)
        t.continuation_scope_level (Definition ty)
    in
    { t with typing_env; }

  let add_equation_on_variable t var ty =
    let typing_env =
      TE.add t.typing_env (Name.var var)
        t.continuation_scope_level (Equation ty)
    in
    { t with typing_env; }

  let find_name t name =
    match TE.find_exn t.typing_env name with
    | exception Not_found ->
      Misc.fatal_errorf "Unbound name %a in environment:@ %a"
        Name.print name
        print t

  let find_variable t var = find_name t (Name.var var)

  let add_symbol t sym ty =
    let typing_env =
      TE.add t.typing_env (Name.symbol sym)
        t.scope_level_for_lifted_constants (Definition ty)
    in
    { t with typing_env; }

  let add_symbol_if_not_defined t sym ty =
    let name = Name.symbol sym in
    if TE.mem t.typing_env name then t
    else add_symbol t sym ty

  let add_parameters t params ~arg_types =
    List.fold_left2 (fun t param arg_type ->
        add_variable (KP.var param) arg_type)
      t
      params

  let add_parameters_with_unknown_types t params =
    let arg_types =
      List.map (fun param -> T.unknown (KP.kind param)) params
    in
    add_parameters t params ~arg_types

  let add_continuation0 t cont cont_in_env =
    let continuations =
      Continuation.Map.add cont (t.continuation_scope_level, cont_in_env)
        t.continuations
    in
    { t with
      continuations;
    }

  let find_continuation t cont : Continuation_in_env.t =
    match Continuation.Map.find cont t.continuations with
    | exception Not_found ->
      Misc.fatal_errorf "Unbound continuation %a in environment:@ %a"
        Continuation.print cont
        print t
    | (_scope_level, cont_in_env) -> cont_in_env

  let resolve_continuation_aliases t cont =
    match Continuation.Map.find cont t.continuation_aliases with
    | exception Not_found -> cont
    | alias_for -> alias_for

  let continuation_arity t cont =
    match find_continuation t cont with
    | Unknown of { arity; }
    | Unreachable of { arity; }
    | Inline of { arity; _ } -> arity

  let add_continuation t cont arity =
    add_continuation0 t cont (Unknown { arity; })

  let add_unreachable_continuation t cont arity =
    add_continuation0 t cont (Unreachable { arity; })

  let add_continuation_alias t cont arity ~alias_for =
    let alias_for_arity = continuation_arity t alias_for in
    if not (Flambda_arity.equal arity alias_for_arity) then begin
      Misc.fatal_errorf "%a (arity %a) cannot be an alias for %a (arity %a) \
          since the two continuations differ in arity"
        Continuation.print cont
        Flambda_arity.print arity
        Continuation.print alias_for
        Flambda_arity.print alias_for_arity
    end;
    if Continuation.Map.mem cont t.continuation_aliases then begin
      Misc.fatal_errorf "Cannot add continuation alias %a (as alias for %a); \
          the continuation is already deemed to be an alias"
        Continuation.print cont
        Continuation.print alias_for
    end;
    let alias_for = resolve_continuation_aliases t alias_for in
    let continuation_aliases =
      Continuation.Map.add cont alias_for t.continuation_aliases
    in
    { t with
      continuation_aliases;
    }

  let add_continuation_to_inline t cont arity handler =
    add_continuation0 t cont (Inline { arity; handler; })

  let add_exn_continuation t exn_cont =
    let exn_continuations =
      Exn_continuation.Map.add exn_cont t.continuation_scope_level
        t.exn_continuations
    in
    { t with
      exn_continuations;
    }

  let extend_typing_environment t env_extension =
    let typing_env = TE.add_env_extension t.typing_env env_extension in
    { t with
      typing_env;
    }

  let check_variable_is_bound t var =
    if not (TE.mem t.typing_env (Name.var var)) then begin
      Misc.fatal_errorf "Unbound variable %a in environment:@ %a"
        Variable.print var
        print t
    end

  let check_symbol_is_bound t sym =
    if not (TE.mem t.typing_env (Name.symbol sym)) then begin
      Misc.fatal_errorf "Unbound symbol %a in environment:@ %a"
        Symbol.print sym
        print t
    end

  let add_inlined_debuginfo t dbg =
    { t with
      inlined_debuginfo = Debuginfo.concat t.inlined_debuginfo dbg;
    }

  let disable_function_inlining t =
    { t with
      can_inline = false;
    }

  let add_lifted_constants t lifted =
    Symbol.Map.fold (fun sym (ty, _static_part) ->
        add_symbol_if_not_defined t sym ty)
      t

  (* CR mshinwell: Think more about this -- may be re-traversing long lists *)
  let add_lifted_constants_from_r t r =
    add_lifted_constants t (Result.get_lifted_constants r)

  let set_scope_level_for_lifted_constants t scope_level =
    { t with
      scope_level_for_lifted_constants;
    }
end and Result : sig
  include Simplify_env_and_result_intf.Result with type env = Env.t
end = struct
  type env = Env.t

  module Defined_continuation = struct
    type t = {
      arity : Flambda_arity.t;
      uses : Flambda_type.t list list;
    }

    let print ppf t =
      Format.fprintf ppf ...
  end

  type t =
    { resolver : (Export_id.t -> Flambda_type.t option);
      continuations : Defined_continuation.t Continuation.Map.t;
      imported_symbols : Flambda_kind.t Symbol.Map.t;
      lifted_constants : lifted_constants;
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
      Continuation_uses.add_use uses (Env.typing_env env) kind
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
