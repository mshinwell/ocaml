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

module rec Downwards_env : sig
  include Simplify_env_and_result_intf.Downwards_env
    with type result := Result.t
end = struct
  type t = {
    backend : (module Flambda2_backend_intf.S);
    round : int;
    typing_env : TE.t;
    continuation_scope_level : Scope.t;
    inlined_debuginfo : Debuginfo.t;
    can_inline : bool;
    inlining_depth_increment : int;
    float_const_prop : bool;
  }

  let print ppf { backend = _; round; typing_env;
                  continuation_scope_level; inlined_debuginfo; can_inline;
                  inlining_depth_increment; float_const_prop;
                } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(round@ %d)@]@ \
        @[<hov 1>(typing_env@ %a)@]@ \
        @[<hov 1>(continuation_scope_level@ %a)@]@ \
        @[<hov 1>(inlined_debuginfo@ %a)@]@ \
        @[<hov 1>(can_inline@ %b)@]@ \
        @[<hov 1>(inlining_depth_increment@ %d)@]@ \
        @[<hov 1>(float_const_prop@ %b)@]\
        )@]"
      round
      TE.print typing_env
      Scope.print continuation_scope_level
      Debuginfo.print inlined_debuginfo
      can_inline
      inlining_depth_increment
      float_const_prop

  let invariant _t = ()

  let create ~round ~backend ~float_const_prop =
    (* CR mshinwell: [resolver] should come from [backend] *)
    let resolver _export_id = None in
    { backend;
      round;
      typing_env = TE.create ~resolver;
      continuation_scope_level = Scope.initial;
      inlined_debuginfo = Debuginfo.none;
      can_inline = true;
      inlining_depth_increment = 0;
      float_const_prop;
    }

  let resolver t = TE.resolver t.typing_env
  let backend t = t.backend
  let typing_env t = t.typing_env
  let round t = t.round
  let get_continuation_scope_level t = t.continuation_scope_level
  let can_inline t = t.can_inline
  let float_const_prop t = t.float_const_prop
  let get_inlining_depth_increment t = t.inlining_depth_increment

  let set_inlining_depth_increment t inlining_depth_increment =
    { t with inlining_depth_increment; }

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

  let increment_continuation_scope_level_twice t =
    increment_continuation_scope_level
      (increment_continuation_scope_level t)

  let enter_closure { backend; round; typing_env;
                      inlined_debuginfo = _; can_inline;
                      continuation_scope_level = _;
                      inlining_depth_increment = _;
                      float_const_prop;
                    } =
    { backend;
      round;
      typing_env = TE.create_using_resolver_and_symbol_bindings_from typing_env;
      continuation_scope_level = Scope.initial;
      inlined_debuginfo = Debuginfo.none;
      can_inline;
      inlining_depth_increment = 0;
      float_const_prop;
    }

  let define_variable t var kind =
    let typing_env =
      let var = Name_in_binding_pos.var var in
      TE.add_definition t.typing_env var kind
    in
    { t with typing_env; }

  let add_name t name ty =
    let typing_env =
      TE.add_equation
        (TE.add_definition t.typing_env name (T.kind ty))
        (Name_in_binding_pos.name name) ty
    in
    { t with typing_env; }

  let add_variable t var ty =
    let typing_env =
      let var' = Name_in_binding_pos.var var in
      TE.add_equation
        (TE.add_definition t.typing_env var' (T.kind ty))
        (Name.var (Var_in_binding_pos.var var)) ty
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

  let define_symbol t sym kind =
    let typing_env =
      let sym =
        Name_in_binding_pos.create (Name.symbol sym)
          Name_occurrence_kind.normal
      in
      TE.add_definition t.typing_env sym kind
    in
    { t with typing_env; }

  let add_symbol t sym ty =
    let typing_env =
      let sym = Name.symbol sym in
      let sym' = Name_in_binding_pos.create sym Name_occurrence_kind.normal in
      TE.add_equation
        (TE.add_definition t.typing_env sym' (T.kind ty))
        sym ty
    in
    { t with typing_env; }

  let add_equation_on_symbol t sym ty =
    let typing_env =
      let sym = Name.symbol sym in
      TE.add_equation t.typing_env sym ty
    in
    { t with typing_env; }

  let find_symbol t sym = find_name t (Name.symbol sym)

  let define_name t name kind =
    let typing_env =
      TE.add_definition t.typing_env name kind
    in
    { t with typing_env; }

  let add_equation_on_name t name ty =
    let typing_env = TE.add_equation t.typing_env name ty in
    { t with typing_env; }

(*
  let add_symbol_if_not_defined t sym ty =
    let name = Name.symbol sym in
    if TE.mem t.typing_env name then t
    else add_symbol t sym ty
*)

  let define_parameters t ~params =
    List.fold_left (fun t param ->
        let var =
          Var_in_binding_pos.create (KP.var param) Name_occurrence_kind.normal
        in
        define_variable t var (KP.kind param))
      t
      params

  let add_parameters t params ~param_types =
    if List.compare_lengths params param_types <> 0 then begin
      Misc.fatal_errorf "Mismatch between number of [params] and \
          [param_types]:@ (%a)@ and@ %a"
        Kinded_parameter.List.print params
        (Format.pp_print_list ~pp_sep:Format.pp_print_space T.print) param_types
    end;
    List.fold_left2 (fun t param param_type ->
        let var =
          Var_in_binding_pos.create (KP.var param) Name_occurrence_kind.normal
        in
        add_variable t var param_type)
      t
      params param_types

  let add_parameters_with_unknown_types t params =
    let param_types =
      List.map (fun param -> T.unknown (KP.kind param)) params
    in
    add_parameters t params ~param_types

  let extend_typing_environment t env_extension =
    let typing_env = TE.add_env_extension t.typing_env ~env_extension in
    { t with
      typing_env;
    }

  let with_typing_env t typing_env =
    { t with
      typing_env;
    }

  let map_typing_env t ~f = with_typing_env t (f t.typing_env)

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

  let check_simple_is_bound t (simple : Simple.t) =
    match Simple.descr simple with
    | Name name -> check_name_is_bound t name
    (* CR mshinwell: Convert [Typing_env] to map from [Simple]s. *)
    | Const _ -> ()

  let add_inlined_debuginfo' t dbg =
    Debuginfo.concat t.inlined_debuginfo dbg

  let add_inlined_debuginfo t dbg =
    { t with
      inlined_debuginfo = add_inlined_debuginfo' t dbg
    }

  let disable_function_inlining t =
    { t with
      can_inline = false;
    }

  let add_lifted_constants t ~lifted =
    let typing_env =
      List.fold_left (fun typing_env lifted_constant ->
          Lifted_constant.introduce lifted_constant typing_env)
        (typing_env t)
        lifted
    in
    with_typing_env t typing_env

  (* CR mshinwell: Think more about this -- may be re-traversing long lists *)
  let add_lifted_constants_from_r t r =
    add_lifted_constants t ~lifted:(Result.get_lifted_constants r)
end and Upwards_env : sig
  include Simplify_env_and_result_intf.Upwards_env
    with type downwards_env := Downwards_env.t
end = struct
  type t = {
    continuations : (Scope.t * Continuation_in_env.t) Continuation.Map.t;
    exn_continuations : Scope.t Exn_continuation.Map.t;
    continuation_aliases : Continuation.t Continuation.Map.t;
    apply_cont_rewrites : Apply_cont_rewrite.t Continuation.Map.t;
  }

  let invariant _t = ()

  let empty =
    { continuations = Continuation.Map.empty;
      exn_continuations = Exn_continuation.Map.empty;
      continuation_aliases = Continuation.Map.empty;
      apply_cont_rewrites = Continuation.Map.empty;
    }

  let print_scope_level_and_continuation_in_env ppf (scope_level, cont_in_env) =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(scope_level@ %a)@]@ \
        @[<hov 1>(cont_in_env@ %a)@]\
        )@]"
      Scope.print scope_level
      Continuation_in_env.print cont_in_env

  let print ppf { continuations; exn_continuations; continuation_aliases;
                  apply_cont_rewrites;
                } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(continuations@ %a)@]@ \
        @[<hov 1>(exn_continuations@ %a)@]@ \
        @[<hov 1>(continuation_aliases@ %a)@]@ \
        @[<hov 1>(apply_cont_rewrites@ %a)@]\
        )@]"
      (Continuation.Map.print print_scope_level_and_continuation_in_env)
      continuations
      (Exn_continuation.Map.print Scope.print) exn_continuations
      (Continuation.Map.print Continuation.print) continuation_aliases
      (Continuation.Map.print Apply_cont_rewrite.print)
      apply_cont_rewrites

  let find_continuation t cont =
    match Continuation.Map.find cont t.continuations with
    | exception Not_found ->
      Misc.fatal_errorf "Unbound continuation %a in upwards environment:@ %a"
        Continuation.print cont
        print t
    | (_scope_level, cont_in_env) -> cont_in_env

  let resolve_continuation_aliases t cont =
    match Continuation.Map.find cont t.continuation_aliases with
    | exception Not_found -> cont
    | alias_for -> alias_for

  let resolve_exn_continuation_aliases t exn_cont =
    let cont = Exn_continuation.exn_handler exn_cont in
    match Continuation.Map.find cont t.continuation_aliases with
    | exception Not_found -> exn_cont
    | alias_for -> Exn_continuation.with_exn_handler exn_cont alias_for

  let continuation_arity t cont =
    match find_continuation t cont with
    | Unknown { arity; }
    | Unreachable { arity; }
    | Apply_cont_with_constant_arg { cont = _; arg = _; arity; }
    | Inline { arity; _ } -> arity

  let add_continuation0 t cont scope cont_in_env =
    let continuations =
      Continuation.Map.add cont (scope, cont_in_env) t.continuations
    in
    { t with
      continuations;
    }

  let add_continuation t cont scope arity =
    add_continuation0 t cont scope (Unknown { arity; })

  let add_unreachable_continuation t cont scope arity =
    add_continuation0 t cont scope (Unreachable { arity; })

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
(* CR mshinwell: This should check that they are either both exn handlers
   or both non-exn handlers
    if Continuation.is_exn cont || Continuation.is_exn alias_for then begin
      Misc.fatal_errorf "Cannot alias exception handlers: %a (exn handler? %b) \
          as alias for %a (exn handler? %b)"
        Continuation.print cont
        (Continuation.is_exn cont)
        Continuation.print alias_for
        (Continuation.is_exn alias_for)
    end;
*)
    let alias_for = resolve_continuation_aliases t alias_for in
    let continuation_aliases =
      Continuation.Map.add cont alias_for t.continuation_aliases
    in
    { t with
      continuation_aliases;
    }

  let add_continuation_apply_cont_with_constant_arg t cont scope arity
        ~destination_cont ~destination_arg =
    add_continuation0 t cont scope (Apply_cont_with_constant_arg {
      cont = destination_cont;
      arg = destination_arg;
      arity;
    })

  let add_continuation_to_inline t cont scope arity handler =
    add_continuation0 t cont scope (Inline { arity; handler; })

  let add_exn_continuation t exn_cont scope =
    (* CR mshinwell: Think more about keeping these in both maps *)
    let continuations =
      let cont = Exn_continuation.exn_handler exn_cont in
      let cont_in_env : Continuation_in_env.t =
        Unknown { arity = Exn_continuation.arity exn_cont; }
      in
      Continuation.Map.add cont (scope, cont_in_env) t.continuations
    in
    let exn_continuations =
      Exn_continuation.Map.add exn_cont scope t.exn_continuations
    in
    { t with
      continuations;
      exn_continuations;
    }

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

  let add_apply_cont_rewrite t cont rewrite =
    if Continuation.Map.mem cont t.apply_cont_rewrites then begin
      Misc.fatal_errorf "Cannot redefine [Apply_cont_rewrite] for %a"
        Continuation.print cont
    end;
    let apply_cont_rewrites =
      Continuation.Map.add cont rewrite t.apply_cont_rewrites
    in
    { t with
      apply_cont_rewrites;
    }

  let find_apply_cont_rewrite t cont =
    match Continuation.Map.find cont t.apply_cont_rewrites with
    | exception Not_found -> None
    | rewrite -> Some rewrite
end and Result : sig
  include Simplify_env_and_result_intf.Result
end = struct
  type t =
    { resolver : (Export_id.t -> Flambda_type.t option);
      imported_symbols : Flambda_kind.t Symbol.Map.t;
      lifted_constants_innermost_first : Lifted_constant.t list;
    }

  let print ppf { resolver = _; imported_symbols;
                  lifted_constants_innermost_first;
                } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(imported_symbols@ %a)@]@ \
        @[<hov 1>(lifted_constants_innermost_first@ %a)@]\
        )@]"
      (Symbol.Map.print Flambda_kind.print) imported_symbols
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Lifted_constant.print)
        lifted_constants_innermost_first

  let create ~resolver =
    { resolver;
      imported_symbols = Symbol.Map.empty;
      lifted_constants_innermost_first = [];
    }

  let imported_symbols t = t.imported_symbols

  let new_lifted_constant t lifted_constant =
    { t with
      lifted_constants_innermost_first =
        lifted_constant :: t.lifted_constants_innermost_first;
    }

(*
  let add_lifted_constants t ~from =
    { t with
      lifted_constants_innermost_first =
        from.lifted_constants_innermost_first
          @ t.lifted_constants_innermost_first;
    }
*)

  let get_lifted_constants t = t.lifted_constants_innermost_first

  let clear_lifted_constants t =
    { t with
      lifted_constants_innermost_first = [];
    }
end
