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
    with type result := Result.t

  val continuation_scope_level : t -> Continuation.t -> Scope.t
end = struct
  type t = {
    backend : (module Flambda2_backend_intf.S);
    round : int;
    typing_env : TE.t;
    continuations : (Scope.t * Continuation_in_env.t) Continuation.Map.t;
    exn_continuations : Scope.t Exn_continuation.Map.t;
    continuation_aliases : Continuation.t Continuation.Map.t;
    continuation_scope_level : Scope.t;
    inlined_debuginfo : Debuginfo.t;
    can_inline : bool;
  }

  let invariant _t = ()

  let create ~round ~backend =
    (* CR mshinwell: [resolver] should come from [backend] *)
    let resolver _export_id = None in
    { backend;
      round;
      typing_env = TE.create ~resolver;
      continuations = Continuation.Map.empty;
      exn_continuations = Exn_continuation.Map.empty;
      continuation_aliases = Continuation.Map.empty;
      continuation_scope_level = Scope.initial;
      inlined_debuginfo = Debuginfo.none;
      can_inline = false;
    }

  let print_scope_level_and_continuation_in_env ppf (scope_level, cont_in_env) =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(scope_level %a)@]@ \
        @[<hov 1>(cont_in_env %a)@]\
        )@]"
      Scope.print scope_level
      Continuation_in_env.print cont_in_env

  let print ppf { backend = _; round; typing_env; continuations;
                  exn_continuations; continuation_aliases;
                  continuation_scope_level; inlined_debuginfo; can_inline;
                } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(round@ %d)@]@ \
        @[<hov 1>(typing_env@ %a)@]@ \
        @[<hov 1>(continuations@ %a)@]@ \
        @[<hov 1>(exn_continuations@ %a)@]@ \
        @[<hov 1>(continuation_scope_level@ %a)@]@ \
        @[<hov 1>(continuation_aliases@ %a)@]@ \
        @[<hov 1>(inlined_debuginfo@ %a)@]@ \
        @[<hov 1>(can_inline@ %b)@]\
        )@]"
      round
      TE.print typing_env
      (Continuation.Map.print print_scope_level_and_continuation_in_env)
      continuations
      (Exn_continuation.Map.print Scope.print) exn_continuations
      (Continuation.Map.print Continuation.print) continuation_aliases
      Scope.print continuation_scope_level
      Debuginfo.print inlined_debuginfo
      can_inline

  let resolver t = TE.resolver t.typing_env
  let backend t = t.backend
  let typing_env t = t.typing_env
  let round t = t.round
  let get_continuation_scope_level t = t.continuation_scope_level
  let can_inline t = t.can_inline

  (* CR mshinwell: remove "_level" *)
  let increment_continuation_scope_level t =
    let continuation_scope_level = Scope.next t.continuation_scope_level in
    let typing_env =
      TE.increment_scope_to t.typing_env continuation_scope_level
    in
    { t with
      typing_env;
      continuation_scope_level = Scope.next t.continuation_scope_level;
    }

  let enter_closure { backend; round; typing_env; continuations = _;
                      exn_continuations = _; continuation_scope_level = _;
                      continuation_aliases = _; inlined_debuginfo = _;
                      can_inline;
                    } =
    { backend;
      round;
      typing_env = TE.create_using_resolver_and_symbol_bindings_from typing_env;
      continuations = Continuation.Map.empty;
      exn_continuations = Exn_continuation.Map.empty;
      continuation_aliases = Continuation.Map.empty;
      continuation_scope_level = Scope.initial;
      inlined_debuginfo = Debuginfo.none;
      can_inline;
    }

  let add_variable t var ty =
    let typing_env =
      let var = Name.var var in
      TE.add_equation
        (TE.add_definition t.typing_env var (T.kind ty))
        var ty
    in
    { t with typing_env; }

  let add_equation_on_variable t var ty =
    let typing_env = TE.add_equation t.typing_env (Name.var var) ty in
    { t with typing_env; }

  let find_name t name =
    match TE.find t.typing_env name with
    | exception Not_found ->
      Misc.fatal_errorf "Unbound name %a in environment:@ %a"
        Name.print name
        print t
    | ty -> ty

  let find_variable t var = find_name t (Name.var var)

  let add_symbol t sym ty =
    let typing_env =
      let sym = Name.symbol sym in
      TE.add_equation
        (TE.add_definition t.typing_env sym (T.kind ty))
        sym ty
    in
    { t with typing_env; }

  let add_symbol_if_not_defined t sym ty =
    let name = Name.symbol sym in
    if TE.mem t.typing_env name then t
    else add_symbol t sym ty

  let add_parameters t params ~arg_types =
    List.fold_left2 (fun t param arg_type ->
        add_variable t (KP.var param) arg_type)
      t
      params arg_types

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

  let find_continuation t cont =
    match Continuation.Map.find cont t.continuations with
    | exception Not_found ->
      Misc.fatal_errorf "Unbound continuation %a in environment:@ %a"
        Continuation.print cont
        print t
    | (_scope_level, cont_in_env) -> cont_in_env

  let continuation_scope_level t cont =
    match Continuation.Map.find cont t.continuations with
    | exception Not_found ->
      Misc.fatal_errorf "Unbound continuation %a in environment:@ %a"
        Continuation.print cont
        print t
    | (scope_level, _cont_in_env) -> scope_level

  let resolve_continuation_aliases t cont =
    match Continuation.Map.find cont t.continuation_aliases with
    | exception Not_found -> cont
    | alias_for -> alias_for

  let continuation_arity t cont =
    match find_continuation t cont with
    | Unknown { arity; }
    | Unreachable { arity; }
    | Inline { arity; _ } -> arity

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

  let with_typing_environment t typing_env =
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

  let check_name_is_bound t name =
    if not (TE.mem t.typing_env name) then begin
      Misc.fatal_errorf "Unbound name %a in environment:@ %a"
        Name.print name
        print t
    end

  let check_continuation_is_bound t cont =
    if not (Continuation.Map.mem cont t.continuations) then begin
      Misc.fatal_errorf "Unbound continuation %a in environment:@ %a"
        Continuation.print cont
        print t
    end

  let check_exn_continuation_is_bound t exn_cont =
    if not (Exn_continuation.Map.mem exn_cont t.exn_continuations) then begin
      Misc.fatal_errorf "Unbound exception continuation %a in environment:@ %a"
        Exn_continuation.print exn_cont
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

  let add_lifted_constants t (lifted : lifted_constants) =
    List.fold_left (fun t (sym, (ty, _static_part)) ->
        add_symbol_if_not_defined t sym ty)
      t
      lifted

  (* CR mshinwell: Think more about this -- may be re-traversing long lists *)
  let add_lifted_constants_from_r t r =
    add_lifted_constants t (Result.get_lifted_constants r)
end and Result : sig
  include Simplify_env_and_result_intf.Result
    with type env := Env.t
end = struct
  type t =
    { resolver : (Export_id.t -> Flambda_type.t option);
      continuations : Continuation_uses.t Continuation.Map.t;
      imported_symbols : Flambda_kind.t Symbol.Map.t;
      lifted_constants_rev : lifted_constants;
    }

  (* CR mshinwell: Add [print] *)

  let create ~resolver =
    { resolver;
      continuations = Continuation.Map.empty;
      imported_symbols = Symbol.Map.empty;
      lifted_constants_rev = [];
    }

  let add_continuation t env cont =
    match Continuation.Map.find cont t.continuations with
    | exception Not_found ->
      (* CR mshinwell: Avoid repeated lookups. *)
      let arity = Env.continuation_arity env cont in
      let definition_scope_level = Env.continuation_scope_level env cont in
      let uses =
        Continuation_uses.create arity ~definition_scope_level
      in
      { t with
        continuations = Continuation.Map.add cont uses t.continuations;
      }
    | _uses ->
      Misc.fatal_errorf "Cannot redefine continuation %a that is already \
          present in [r]"
        Continuation.print cont

  let record_continuation_use t env cont ~arg_types =
    match Continuation.Map.find cont t.continuations with
    | exception Not_found ->
      Misc.fatal_errorf "Continuation %a not present in [r]"
        Continuation.print cont
    | uses ->
      let uses =
        Continuation_uses.add_use uses (Env.typing_env env) ~arg_types
      in
      { t with
        continuations = Continuation.Map.add cont uses t.continuations;
      }

  let continuation_env_and_arg_types t env cont =
    match Continuation.Map.find cont t.continuations with
    | exception Not_found ->
      Misc.fatal_errorf "Continuation %a not present in [r]"
        Continuation.print cont
    | uses -> Continuation_uses.env_and_arg_types uses (Env.typing_env env)

  let imported_symbols t = t.imported_symbols

  (* CR mshinwell: We need a better representation for the lifted
     constants; maybe in a separate module. *)

  let new_lifted_constant t ~name ty static_part =
    let symbol =
      Symbol.create (Compilation_unit.get_current_exn ())
        (Linkage_name.create name)
    in
    let t =
      { t with
        lifted_constants_rev =
          (symbol, (ty, static_part)) :: t.lifted_constants_rev;
      }
    in
    symbol, t

  let add_lifted_constants t ~from =
    { t with
      lifted_constants_rev = from.lifted_constants_rev @ t.lifted_constants_rev;
    }

  let get_lifted_constants t = List.rev t.lifted_constants_rev
end
