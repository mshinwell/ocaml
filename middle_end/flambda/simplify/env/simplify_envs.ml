(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Flambda.Import

module DE = Downwards_env
module I = Simplify_envs_intf
module T = Flambda_type
module TE = Flambda_type.Typing_env

module rec Upwards_env : I.Upwards_env = struct
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

  let mem_continuation t cont =
    Continuation.Map.mem cont t.continuations

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
    | Other { arity; handler = _; }
    | Unreachable { arity; }
    | Linearly_used_and_inlinable { arity; _ } -> arity

  let add_continuation0 t cont scope cont_in_env =
    let continuations =
      Continuation.Map.add cont (scope, cont_in_env) t.continuations
    in
    { t with
      continuations;
    }

  let add_continuation t cont scope arity =
    add_continuation0 t cont scope (Other { arity; handler = None; })

  let add_continuation_with_handler t cont scope arity handler =
    add_continuation0 t cont scope (Other { arity; handler = Some handler; })

  let add_unreachable_continuation t cont scope arity =
    add_continuation0 t cont scope (Unreachable { arity; })

  let add_continuation_alias t cont arity ~alias_for =
    let arity = Flambda_arity.With_subkinds.to_arity arity in
    let alias_for = resolve_continuation_aliases t alias_for in
    let alias_for_arity =
      continuation_arity t alias_for
      |> Flambda_arity.With_subkinds.to_arity
    in
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

  let add_linearly_used_inlinable_continuation t cont scope arity ~params
        ~handler ~free_names_of_handler =
    add_continuation0 t cont scope
      (Linearly_used_and_inlinable { arity; handler; free_names_of_handler;
        params; })

  let add_exn_continuation t exn_cont scope =
    (* CR mshinwell: Think more about keeping these in both maps *)
    let continuations =
      let cont = Exn_continuation.exn_handler exn_cont in
      let cont_in_env : Continuation_in_env.t =
        Other { arity = Exn_continuation.arity exn_cont; handler = None; }
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

  let delete_apply_cont_rewrite t cont =
    { t with
      apply_cont_rewrites = Continuation.Map.remove cont t.apply_cont_rewrites;
    }

  let will_inline_continuation t cont =
    match find_continuation t cont with
    | Other _ | Unreachable _ -> false
    | Linearly_used_and_inlinable _ -> true
end and Lifted_constant : I.Lifted_constant = struct
  module Definition = struct
    type descr =
      | Code of Code_id.t
      | Set_of_closures of {
          denv : Downwards_env.t;
          closure_symbols_with_types
            : (Symbol.t * Flambda_type.t) Closure_id.Lmap.t;
          symbol_projections : Symbol_projection.t Variable.Map.t;
        }
      | Block_like of {
          symbol : Symbol.t;
          denv : Downwards_env.t;
          ty : Flambda_type.t;
          symbol_projections : Symbol_projection.t Variable.Map.t;
        }

    type t = {
      descr : descr;
      defining_expr : Static_const_with_free_names.t;
    }

    let binds_symbol t sym =
      match t.descr with
      | Code _ -> false
      | Set_of_closures { closure_symbols_with_types; _ } ->
        Closure_id.Lmap.exists (fun _ (sym', _) -> Symbol.equal sym sym')
          closure_symbols_with_types
      | Block_like { symbol; _ } -> Symbol.equal sym symbol

    let free_names t =
      match t.descr with
      | Code _ -> Static_const_with_free_names.free_names t.defining_expr
      | Set_of_closures { symbol_projections; _ }
      | Block_like { symbol_projections; _ } ->
        (* The symbols mentioned in any symbol projections must be counted
           as free names, so that the definition doesn't get placed too high
           in the code. *)
        Variable.Map.fold (fun _var proj free_names ->
            Name_occurrences.add_symbol free_names
              (Symbol_projection.symbol proj) Name_mode.normal)
          symbol_projections
          (Static_const_with_free_names.free_names t.defining_expr)

    let print_descr ppf descr =
      match descr with
      | Code code_id -> Code_id.print ppf code_id
      | Set_of_closures { closure_symbols_with_types; _ } ->
        let symbols =
          Closure_id.Lmap.data closure_symbols_with_types
          |> List.map fst
        in
        Format.fprintf ppf "@[<hov 1>(%a)@]"
          (Format.pp_print_list ~pp_sep:Format.pp_print_space Symbol.print)
          symbols
      | Block_like { symbol; _ } -> Symbol.print ppf symbol

    let print ppf { descr; defining_expr; } =
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(descr@ %a)@]@ \
          @[<hov 1>(defining_expr@ %a)@]\
          @]"
        print_descr descr
        Static_const_with_free_names.print defining_expr

    let descr t = t.descr
    let defining_expr t = t.defining_expr

    let symbol_projections t =
      match t.descr with
      | Code _ -> Variable.Map.empty
      | Set_of_closures { symbol_projections; _ }
      | Block_like { symbol_projections; _ } -> symbol_projections

    let code code_id defining_expr =
      match Static_const_with_free_names.const defining_expr with
      | Code code ->
        if Code_id.equal code_id (Code.code_id code) then
          { descr = Code code_id;
            defining_expr;
          }
        else
          Misc.fatal_errorf "Mismatched code ids: %a vs.@ %a"
            Code_id.print code_id
            Code_id.print (Code.code_id code)
      | _ ->
        Misc.fatal_errorf "Not a code definition: %a"
          Static_const_with_free_names.print defining_expr

    let set_of_closures denv ~closure_symbols_with_types
          ~symbol_projections defining_expr =
      { descr = Set_of_closures {
          denv;
          closure_symbols_with_types;
          symbol_projections;
        };
        defining_expr;
      }

    let block_like denv symbol ty ~symbol_projections defining_expr =
      { descr = Block_like {
          symbol;
          denv;
          ty;
          symbol_projections;
        };
        defining_expr;
      }

    let denv t =
      match t.descr with
      | Code _ -> None
      | Set_of_closures { denv; _ } | Block_like { denv; _ } -> Some denv

    let bound_symbols_pattern t =
      let module P = Bound_symbols.Pattern in
      match t.descr with
      | Code code_id -> P.code code_id
      | Set_of_closures { closure_symbols_with_types; _; } ->
        P.set_of_closures (Closure_id.Lmap.map fst closure_symbols_with_types)
      | Block_like { symbol; _ } -> P.block_like symbol

    let bound_symbols t =
      Bound_symbols.create [bound_symbols_pattern t]

    let types_of_symbols t =
      match t.descr with
      | Code _ -> Symbol.Map.empty
      | Set_of_closures { denv; closure_symbols_with_types; _ } ->
        Closure_id.Lmap.fold (fun _closure_id (symbol, ty) types_of_symbols ->
            Symbol.Map.add symbol (denv, ty) types_of_symbols)
          closure_symbols_with_types
          Symbol.Map.empty
      | Block_like { symbol; denv; ty; _ } ->
        Symbol.Map.singleton symbol (denv, ty)
  end

  type t = {
    definitions : Definition.t list;
    bound_symbols : Bound_symbols.t;
    defining_exprs : Static_const_with_free_names.Group.t;
    symbol_projections : Symbol_projection.t Variable.Map.t;
    is_fully_static : bool;
  }

  let definitions t = t.definitions
  let symbol_projections t = t.symbol_projections

  let free_names_of_defining_exprs t =
    Static_const_with_free_names.Group.free_names t.defining_exprs

  let is_fully_static t = t.is_fully_static

  let print ppf
        { definitions; bound_symbols = _; defining_exprs = _;
          is_fully_static = _; symbol_projections = _; } =
    Format.fprintf ppf "@[<hov 1>(%a)@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Definition.print)
      definitions

  let compute_bound_symbols definitions =
    ListLabels.map definitions ~f:Definition.bound_symbols_pattern
    |> Bound_symbols.create

  let compute_defining_exprs definitions =
    ListLabels.map definitions ~f:Definition.defining_expr
    |> Static_const_with_free_names.Group.create

  let create_block_like symbol ~symbol_projections defining_expr denv ty =
    (* CR mshinwell: check that [defining_expr] is not a set of closures
       or code *)
    let definition =
      Definition.block_like denv symbol ty ~symbol_projections defining_expr
    in
    let definitions = [definition] in
    { definitions;
      bound_symbols = compute_bound_symbols definitions;
      defining_exprs = compute_defining_exprs definitions;
      is_fully_static =
        Static_const_with_free_names.is_fully_static defining_expr;
      symbol_projections = Definition.symbol_projections definition;
    }

  let create_set_of_closures denv ~closure_symbols_with_types
        ~symbol_projections defining_expr =
    let definition =
      Definition.set_of_closures denv ~closure_symbols_with_types
        ~symbol_projections defining_expr
    in
    let definitions = [definition] in
    { definitions;
      bound_symbols = compute_bound_symbols definitions;
      defining_exprs = compute_defining_exprs definitions;
      is_fully_static =
        Static_const_with_free_names.is_fully_static defining_expr;
      symbol_projections = Definition.symbol_projections definition;
    }

  let create_code code_id defining_expr =
    let definition = Definition.code code_id defining_expr in
    let definitions = [definition] in
    { definitions;
      bound_symbols = compute_bound_symbols definitions;
      defining_exprs = compute_defining_exprs definitions;
      is_fully_static =
        Static_const_with_free_names.is_fully_static defining_expr;
      symbol_projections = Definition.symbol_projections definition;
    }

  let concat ts =
    let definitions =
      List.fold_left (fun definitions t ->
          t.definitions @ definitions)
        []
        ts
    in
    let bound_symbols =
      List.fold_left (fun bound_symbols t ->
          Bound_symbols.concat t.bound_symbols bound_symbols)
        Bound_symbols.empty
        ts
    in
    let defining_exprs =
      List.fold_left (fun defining_exprs t ->
          Static_const_with_free_names.Group.concat t.defining_exprs
            defining_exprs)
        Static_const_with_free_names.Group.empty
        ts
    in
    let is_fully_static =
      List.fold_left (fun is_fully_static t ->
          t.is_fully_static && is_fully_static)
        true
        ts
    in
    let symbol_projections =
      List.fold_left (fun symbol_projections t ->
          Variable.Map.disjoint_union ~eq:Symbol_projection.equal
            t.symbol_projections symbol_projections)
        Variable.Map.empty
        ts
    in
    { definitions;
      bound_symbols;
      defining_exprs;
      is_fully_static;
      symbol_projections;
    }

  let defining_exprs t =
    Static_const_with_free_names.Group.create
      (List.map Definition.defining_expr t.definitions)

  let bound_symbols t =
    Bound_symbols.create
      (List.map Definition.bound_symbols_pattern t.definitions)

  let types_of_symbols t =
    ListLabels.fold_left t.definitions
      ~init:Symbol.Map.empty
      ~f:(fun types_of_symbols definition ->
        Symbol.Map.disjoint_union (Definition.types_of_symbols definition)
          types_of_symbols)

  let all_defined_symbols t =
    Symbol.Map.keys (types_of_symbols t)

  let apply_projection t proj =
    let symbol = Symbol_projection.symbol proj in
    let matching_defining_exprs =
      ListLabels.filter_map t.definitions ~f:(fun definition ->
        if Definition.binds_symbol definition symbol then
          Some (Definition.defining_expr definition)
        else
          None)
    in
    match matching_defining_exprs with
    | [defining_expr] ->
      let simple =
        match
          Symbol_projection.projection proj,
          Static_const_with_free_names.const defining_expr
        with
        | Block_load { index; }, Block (tag, mut, fields) ->
          if not (Tag.Scannable.equal tag Tag.Scannable.zero) then begin
            Misc.fatal_errorf "Symbol projection@ %a@ on block which doesn't \
                have tag zero:@ %a"
              Symbol_projection.print proj
              Static_const_with_free_names.print defining_expr
          end;
          if Mutability.is_mutable mut then begin
            Misc.fatal_errorf "Symbol projection@ %a@ on mutable block:@ %a"
              Symbol_projection.print proj
              Static_const_with_free_names.print defining_expr
          end;
          let index = Targetint.OCaml.to_int_exn index in
          begin match List.nth_opt fields index with
          | Some field ->
            begin match field with
            | Symbol symbol -> Simple.symbol symbol
            | Tagged_immediate imm ->
              Simple.const_int (Target_imm.to_targetint imm)
            | Dynamically_computed var -> Simple.var var
            end
          | None ->
            Misc.fatal_errorf "Symbol projection@ %a@ has out-of-range \
                index:@ %a"
              Symbol_projection.print proj
              Static_const_with_free_names.print defining_expr
          end;
        | Project_var { project_from; var; }, Set_of_closures set ->
          let decls = Set_of_closures.function_decls set in
          if not (Function_declarations.binds_closure_id decls project_from)
          then begin
            Misc.fatal_errorf "Symbol projection@ %a@ has closure ID not \
                bound by this set of closures:@ %a"
              Symbol_projection.print proj
              Static_const_with_free_names.print defining_expr
          end;
          let closure_env = Set_of_closures.closure_elements set in
          begin match Var_within_closure.Map.find var closure_env with
          | exception Not_found ->
            Misc.fatal_errorf "Symbol projection@ %a@ has closure var not \
                defined in the environment of this set of closures:@ %a"
              Symbol_projection.print proj
              Static_const_with_free_names.print defining_expr
          | closure_entry -> closure_entry
          end
        | Block_load _,
          (Code _ | Set_of_closures _ | Boxed_float _ | Boxed_int32 _
          | Boxed_int64 _ | Boxed_nativeint _ | Immutable_float_block _
          | Immutable_float_array _ | Mutable_string _ | Immutable_string _)
        | Project_var _,
          (Code _ | Block _ | Boxed_float _ | Boxed_int32 _
          | Boxed_int64 _ | Boxed_nativeint _ | Immutable_float_block _
          | Immutable_float_array _ | Mutable_string _ | Immutable_string _) ->
          Misc.fatal_errorf "Symbol projection@ %a@ cannot be applied to:@ %a"
            Symbol_projection.print proj
            Static_const_with_free_names.print defining_expr
      in
      Some simple
    | [] -> None
    | _::_::_ ->
      Misc.fatal_errorf "Symbol projection@ %a@ matches more than one \
          constant in:@ %a"
        Symbol_projection.print proj
        print t

end and Lifted_constant_state : sig
  include I.Lifted_constant_state
    with type lifted_constant := Lifted_constant.t
end = struct
  type t =
    | Empty
    | Leaf of Lifted_constant.t
    | Leaf_array of { innermost_first : Lifted_constant.t array; }
    | Union of { outer : t; inner : t; }

  let to_list_outermost_first t =
    let rec to_list t acc =
      match t with
      | Empty -> acc
      | Leaf const -> const :: acc
      | Leaf_array { innermost_first; } ->
        (List.rev (Array.to_list innermost_first)) @ acc
      | Union { inner; outer; } -> to_list outer (to_list inner acc)
    in
    to_list t []

  let print ppf t =
    Format.fprintf ppf "@[<hov 1>(outermost_first@ %a)@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Lifted_constant.print)
      (to_list_outermost_first t)

  let empty = Empty

  let is_empty t =
    match t with
    | Empty -> true
    | Leaf _ | Leaf_array _ | Union _ -> false

  let singleton const = Leaf const

  let singleton_sorted_array_of_constants ~innermost_first =
    if Array.length innermost_first < 1 then empty
    else Leaf_array { innermost_first; }

  let union_ordered ~innermost ~outermost =
    match innermost, outermost with
    | Empty, _ -> outermost
    | _, Empty -> innermost
    | inner, outer -> Union { inner; outer; }

  let union t1 t2 = union_ordered ~innermost:t1 ~outermost:t2

  let add_innermost t const =
    if is_empty t then Leaf const
    else Union { inner = Leaf const; outer = t; }

  let add_outermost t const =
    if is_empty t then Leaf const
    else Union { outer = Leaf const; inner = t; }

  let add = add_innermost

  let rec fold_outermost_first t ~init ~f =
    match t with
    | Empty -> init
    | Leaf const -> f init const
    | Leaf_array { innermost_first; } ->
      (* Avoid [Array.fold_right] as it would require a closure allocation. *)
      let acc = ref init in
      for i = Array.length innermost_first - 1 downto 0 do
        acc := f !acc innermost_first.(i)
      done;
      !acc
    | Union { inner; outer; } ->
      let init = fold_outermost_first outer ~init ~f in
      fold_outermost_first inner ~init ~f

  let rec fold_innermost_first t ~init ~f =
    match t with
    | Empty -> init
    | Leaf const -> f init const
    | Leaf_array { innermost_first; } ->
      ArrayLabels.fold_left innermost_first ~init ~f
    | Union { inner; outer; } ->
      let init = fold_innermost_first inner ~init ~f in
      fold_innermost_first outer ~init ~f

  let fold = fold_innermost_first

  let all_defined_symbols t =
    fold t ~init:Symbol.Set.empty ~f:(fun symbols const ->
      Lifted_constant.all_defined_symbols const
      |> Symbol.Set.union symbols)

  let add_to_denv ?maybe_already_defined denv lifted =
    let maybe_already_defined =
      match maybe_already_defined with
      | None -> false
      | Some () -> true
    in
    let denv =
      fold lifted ~init:denv ~f:(fun denv lifted_constant ->
        let types_of_symbols =
          Lifted_constant.types_of_symbols lifted_constant
        in
        Symbol.Map.fold (fun sym (_denv, typ) denv ->
            if maybe_already_defined && DE.mem_symbol denv sym then denv
            else DE.define_symbol denv sym (T.kind typ))
          types_of_symbols
          denv)
    in
    let typing_env =
      let typing_env = DE.typing_env denv in
      fold lifted ~init:typing_env ~f:(fun typing_env lifted_constant ->
        let types_of_symbols =
          Lifted_constant.types_of_symbols lifted_constant
        in
        Symbol.Map.fold (fun sym (denv_at_definition, typ) typing_env ->
            if maybe_already_defined && DE.mem_symbol denv sym then typing_env
            else
              let sym = Name.symbol sym in
              let env_extension =
                (* CR mshinwell: Sometimes we might already have the types
                   "made suitable" in the [closure_env] field of the typing
                   environment, perhaps?  For example when lifted constants'
                   types are coming out of a closure into the enclosing
                   scope. *)
                T.make_suitable_for_environment typ
                  (DE.typing_env denv_at_definition)
                  ~suitable_for:typing_env
                  ~bind_to:sym
              in
              TE.add_env_extension_with_extra_variables typing_env
                env_extension)
          types_of_symbols
          typing_env)
    in
    fold lifted ~init:(DE.with_typing_env denv typing_env)
      ~f:(fun denv lifted_constant ->
        let pieces_of_code =
          Lifted_constant.defining_exprs lifted_constant
          |> Static_const_with_free_names.Group.pieces_of_code
        in
        Code_id.Map.fold (fun code_id code denv ->
            match Code.params_and_body code with
            | Present _ ->
              if maybe_already_defined && DE.mem_code denv code_id then denv
              else DE.define_code denv ~code_id ~code
            | Deleted -> denv)
          pieces_of_code
          denv)

  let add_singleton_to_denv t const =
    add_to_denv t (singleton const)

  let add_list_to_denv t consts =
    ListLabels.fold_left consts ~init:t ~f:add_singleton_to_denv
end
