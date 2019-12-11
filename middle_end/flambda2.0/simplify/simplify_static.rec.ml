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

open! Simplify_import

module Bound_symbols = Flambda_static.Program_body.Bound_symbols
module Definition = Flambda_static.Program_body.Definition
module Of_kind_value = Flambda_static.Of_kind_value
module Program = Flambda_static.Program
module Program_body = Flambda_static.Program_body
module Static_part = Flambda_static.Static_part
module Static_structure = Flambda_static.Program_body.Static_structure

module Return_cont_handler = struct
  type t = {
    computed_values : KP.t list;
    static_structure : Static_structure.t;
  }

  let print ppf { computed_values; static_structure; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(computed_values@ %a)@]@ \
        @[<hov 1>(static_structure@ %a)@]\
        )@]"
      KP.List.print computed_values
      Static_structure.print static_structure

  let is_exn_handler _t = false
  let stub _t = false

  (* CR mshinwell: Stop duplicating types of this form *)
  type behaviour =
    | Unreachable of { arity : Flambda_arity.t; }
    | Alias_for of { arity : Flambda_arity.t; alias_for : Continuation.t; }
    | Apply_cont_with_constant_arg of {
        cont : Continuation.t;
        arg : Simple.Const.t;
        arity : Flambda_arity.t;
      }
    | Unknown of { arity : Flambda_arity.t; }

  (* Silence warning 37. *)
  let _ = Unreachable { arity = []; }
  let _ = Apply_cont_with_constant_arg {
    cont = Continuation.dummy;
    arg = Tagged_immediate Immediate.zero;
    arity = [];
  }
  let _ = Alias_for { arity = []; alias_for = Continuation.dummy; }

  let arity t = KP.List.arity t.computed_values

  let behaviour t = Unknown { arity = arity t; }

  let real_handler _t = None

  module Opened = struct
    type nonrec t = t
    (* CR mshinwell: Document why these aren't freshened *)
    let params t = t.computed_values
  end

  let pattern_match t ~f = f t
end

module Exn_cont_handler = struct
  type t = unit

  let print ppf () = Format.pp_print_string ppf "()"

  let is_exn_handler _t = true
  let stub _t = false

  type behaviour =
    | Unreachable of { arity : Flambda_arity.t; }
    | Alias_for of { arity : Flambda_arity.t; alias_for : Continuation.t; }
    | Apply_cont_with_constant_arg of {
        cont : Continuation.t;
        arg : Simple.Const.t;
        arity : Flambda_arity.t;
      }
    | Unknown of { arity : Flambda_arity.t; }

  (* Silence warning 37. *)
  let _ = Unreachable { arity = []; }
  let _ = Apply_cont_with_constant_arg {
    cont = Continuation.dummy;
    arg = Tagged_immediate Immediate.zero;
    arity = [];
  }
  let _ = Alias_for { arity = []; alias_for = Continuation.dummy; }

  let arity _t = [K.value]

  let behaviour t = Unknown { arity = arity t; }

  let real_handler _t = None

  module Opened = struct
    type nonrec t = KP.t
    let params t = [t]
  end

  let pattern_match _t ~f =
    f (KP.create (Parameter.wrap (Variable.create "exn")) K.value)
end

module Simplify_return_cont =
  Generic_simplify_let_cont.Make (Return_cont_handler)

module Simplify_exn_cont =
  Generic_simplify_let_cont.Make (Exn_cont_handler)

(* CR-someday mshinwell: Finish improved simplification using types *)

let simplify_of_kind_value dacc (of_kind_value : Of_kind_value.t) =
  let denv = DA.denv dacc in
  match of_kind_value with
  | Symbol sym ->
    let ty = DE.find_symbol denv sym in
    of_kind_value, ty
  | Tagged_immediate i -> of_kind_value, T.this_tagged_immediate i
  | Dynamically_computed var ->
    let min_name_mode = Name_mode.normal in
    match S.simplify_simple dacc (Simple.var var) ~min_name_mode with
    | Bottom, ty ->
      assert (K.equal (T.kind ty) K.value);
      (* CR mshinwell: This should be "invalid" and propagate up *)
      of_kind_value, T.bottom K.value
    | Ok simple, ty ->
      match Simple.descr simple with
      | Name (Symbol sym) -> Of_kind_value.Symbol sym, ty
      | Name (Var _) -> of_kind_value, ty
      | Const (Tagged_immediate imm) -> Of_kind_value.Tagged_immediate imm, ty
      | Const (Naked_immediate _ | Naked_float _ | Naked_int32 _
          | Naked_int64 _ | Naked_nativeint _) ->
        (* CR mshinwell: This should be "invalid" and propagate up *)
        of_kind_value, ty

let simplify_or_variable dacc type_for_const
      (or_variable : _ Static_part.or_variable) =
  let denv = DA.denv dacc in
  match or_variable with
  | Const const -> or_variable, type_for_const const
  | Var var ->
    (* CR mshinwell: This needs to check the type of the variable according
       to the various cases below. *)
    or_variable, DE.find_variable denv var

let simplify_set_of_closures0 dacc ~result_dacc set_of_closures
      ~closure_symbols ~closure_elements ~closure_element_types =
  let closure_bound_names =
    Closure_id.Map.map Name_in_binding_pos.symbol closure_symbols
  in
  let set_of_closures, closure_types_by_bound_name, dacc, result_dacc =
    Simplify_named.simplify_set_of_closures0 dacc ~result_dacc set_of_closures
      ~closure_bound_names ~closure_elements ~closure_element_types
  in
  let static_structure : Program_body.Static_structure.t =
    let static_part : K.fabricated Static_part.t =
      Set_of_closures set_of_closures
    in
    let bound_symbols : K.fabricated Program_body.Bound_symbols.t =
      Set_of_closures { closure_symbols; }
    in
    [S (bound_symbols, static_part)]
  in
  let static_structure_types =
    Name_in_binding_pos.Map.fold
      (fun name closure_type static_structure_types ->
        let symbol = Name_in_binding_pos.must_be_symbol name in
        Symbol.Map.add symbol closure_type static_structure_types)
      closure_types_by_bound_name
      Symbol.Map.empty
  in
  set_of_closures, dacc, result_dacc, static_structure_types, static_structure

let simplify_set_of_closures dacc ~result_dacc set_of_closures
      ~closure_symbols =
  let _can_lift, closure_elements, closure_element_types =
    Simplify_named.type_closure_elements_and_make_lifting_decision dacc
      ~min_name_mode:Name_mode.normal set_of_closures
  in
  simplify_set_of_closures0 dacc ~result_dacc set_of_closures
    ~closure_symbols ~closure_elements ~closure_element_types

let simplify_static_part_of_kind_value dacc ~result_dacc
      (static_part : K.value Static_part.t) ~result_sym
      : K.value Static_part.t * DA.t * DA.t =
  (* [dacc] holds the environment for simplifying the [Static_part]; it contains
     bindings for any computed values in addition to the types of everything
     prior to the current [Define_symbol].

     [result_dacc] holds the environment, which we are computing, that will be
     in effect immediately after the current [Define_symbol].

     We have to return both a new [dacc] and a new [result_dacc] since there may
     be multiple (ordered) symbol bindings in any given [Static_structure]. For
     simplifying the (N+1)th of those, we need the starting [dacc], plus the
     types for the (0..N)th bindings too. We can't just use [result_dacc] as it
     doesn't contain the bindings for the computed values. *)
  let bind_result_sym typ =
    (* CR mshinwell: tidy up. *)
    let existing_typing_env = DE.typing_env (DA.denv dacc) in
    let dacc =
      DA.map_denv dacc ~f:(fun denv ->
        let suitable_for =
          (* [result_sym] will already be defined when we are lifting
             reified computed values (see below). *)
          DE.typing_env (DE.define_symbol_if_undefined denv result_sym K.value)
        in
        let env_extension =
          T.make_suitable_for_environment typ
            existing_typing_env
            ~suitable_for
            ~bind_to:(Name.symbol result_sym)
        in
        DE.with_typing_env denv
          (TE.add_env_extension suitable_for ~env_extension))
    in
    let result_dacc =
      DA.map_denv result_dacc ~f:(fun result_denv ->
        let suitable_for =
          DE.typing_env
            (DE.define_symbol_if_undefined result_denv result_sym K.value)
        in
        let env_extension =
          T.make_suitable_for_environment typ
            existing_typing_env
            ~suitable_for
            ~bind_to:(Name.symbol result_sym)
        in
        DE.with_typing_env result_denv
          (TE.add_env_extension suitable_for ~env_extension))
    in
    dacc, result_dacc
  in
  match static_part with
  | Block (tag, is_mutable, fields) ->
    let fields_with_tys =
      List.map (fun of_kind_value ->
          simplify_of_kind_value dacc of_kind_value)
        fields
    in
    let fields, field_tys = List.split fields_with_tys in
    let ty =
      T.immutable_block (Tag.Scannable.to_tag tag) ~field_kind:K.value
        ~fields:field_tys
    in
    let dacc, result_dacc = bind_result_sym ty in
    Block (tag, is_mutable, fields), dacc, result_dacc
  | Fabricated_block var ->
    DE.check_variable_is_bound (DA.denv dacc) var;
    let dacc, result_dacc = bind_result_sym (T.any_value ()) in
    static_part, dacc, result_dacc
  (* CR mshinwell: Need to reify to change Equals types into new terms *)
  | Boxed_float or_var ->
    let or_var, ty =
      simplify_or_variable dacc (fun f -> T.this_boxed_float f) or_var
    in
    let dacc, result_dacc = bind_result_sym ty in
    Boxed_float or_var, dacc, result_dacc
  | Boxed_int32 or_var ->
    let or_var, ty =
      simplify_or_variable dacc (fun f -> T.this_boxed_int32 f) or_var
    in
    let dacc, result_dacc = bind_result_sym ty in
    Boxed_int32 or_var, dacc, result_dacc
  | Boxed_int64 or_var ->
    let or_var, ty =
      simplify_or_variable dacc (fun f -> T.this_boxed_int64 f) or_var
    in
    let dacc, result_dacc = bind_result_sym ty in
    Boxed_int64 or_var, dacc, result_dacc
  | Boxed_nativeint or_var ->
    let or_var, ty =
      simplify_or_variable dacc (fun f -> T.this_boxed_nativeint f) or_var
    in
    let dacc, result_dacc = bind_result_sym ty in
    Boxed_nativeint or_var, dacc, result_dacc
  | Immutable_float_array fields ->
    let fields_with_tys =
      List.map (fun field ->
          simplify_or_variable dacc
            (fun f -> T.this_naked_float f)
            field)
        fields
    in
    let fields, _field_tys = List.split fields_with_tys in
    let dacc, result_dacc = bind_result_sym (T.any_value ()) in
    Immutable_float_array fields, dacc, result_dacc
  | Mutable_string { initial_value; } ->
    let initial_value, str_ty =
      simplify_or_variable dacc (fun initial_value ->
          T.mutable_string ~size:(String.length initial_value))
        initial_value
    in
    let static_part : K.value Static_part.t =
      Mutable_string {
        initial_value;
      }
    in
    let dacc, result_dacc = bind_result_sym str_ty in
    static_part, dacc, result_dacc
  | Immutable_string or_var ->
    let or_var, ty =
      simplify_or_variable dacc (fun str -> T.this_immutable_string str)
        or_var
    in
    let dacc, result_dacc = bind_result_sym ty in
    Immutable_string or_var, dacc, result_dacc

let simplify_static_part_of_kind_fabricated dacc ~result_dacc
      (static_part : K.fabricated Static_part.t)
      ~closure_symbols
    : K.fabricated Static_part.t * DA.t * DA.t =
  match static_part with
  | Set_of_closures set_of_closures ->
     let set_of_closures, dacc, result_dacc, _static_structure_types,
         _static_structure =
       simplify_set_of_closures dacc ~result_dacc set_of_closures
         ~closure_symbols
     in
     Set_of_closures set_of_closures, dacc, result_dacc

let simplify_piece_of_static_structure (type k) dacc ~result_dacc
      (bound_syms : k Program_body.Bound_symbols.t)
      (static_part : k Static_part.t)
      : k Static_part.t * DA.t * DA.t =
  match bound_syms with
  | Singleton result_sym ->
    simplify_static_part_of_kind_value dacc ~result_dacc static_part
      ~result_sym
  | Set_of_closures { closure_symbols; } ->
    simplify_static_part_of_kind_fabricated dacc ~result_dacc static_part
      ~closure_symbols

let simplify_static_structure dacc ~result_dacc pieces
      : DA.t * Program_body.Static_structure.t =
  let str_rev, _dacc, result_dacc =
    List.fold_left
      (fun (str_rev, dacc, result_dacc)
           (Static_structure.S (bound_syms, static_part)) ->
        let static_part, dacc, result_dacc =
          simplify_piece_of_static_structure dacc ~result_dacc
            bound_syms static_part
        in
        let binding : Program_body.Static_structure.t0 =
          S (bound_syms, static_part)
        in
        let str_rev = binding :: str_rev in
        str_rev, dacc, result_dacc)
      ([], dacc, result_dacc)
      pieces
  in
  result_dacc, List.rev str_rev

let reify_types_of_computed_values dacc ~result_dacc computed_values =
  let typing_env = DE.typing_env (DA.denv dacc) in
  Variable.Set.fold
    (fun var (result_dacc, dacc, reified_definitions) ->
      let ty = TE.find typing_env (Name.var var) in
      let existing_symbol =
        (* We must avoid attempting to create aliases between symbols or
           (equivalently) defining static parts that already have symbols.
           This could happen if [var] is actually equal to another of the
           computed value variables. *)
        let typing_env = DE.typing_env (DA.denv dacc) in
        match
          TE.get_canonical_simple typing_env ~min_name_mode:NM.normal
            (Simple.var var)
        with
        | Bottom | Ok None -> None
        | Ok (Some simple) ->
          match Simple.descr simple with
          | Name (Symbol symbol) -> Some symbol
          | Name (Var _) | Const _ -> None
      in
      match existing_symbol with
      | Some _ -> result_dacc, dacc, reified_definitions
      | None ->
        begin match
          T.reify ~allowed_free_vars:computed_values typing_env
            ~min_name_mode:NM.normal ty
        with
        | Lift to_lift ->
          let static_part = Reification.create_static_part to_lift in
          let symbol =
            Symbol.create (Compilation_unit.get_current_exn ())
              (Linkage_name.create (Variable.unique_name var))
          in
          let dacc =
            DA.map_denv dacc ~f:(fun denv ->
              DE.add_equation_on_name (DE.define_symbol denv symbol K.value)
                (Name.var var)
                (T.alias_type_of K.value (Simple.symbol symbol)))
          in
          let static_structure_part : Static_structure.t0 =
            S (Singleton symbol, static_part)
          in
          result_dacc, dacc, (var, static_structure_part) :: reified_definitions
      | Lift_set_of_closures { closure_id; function_decls; closure_vars; } ->
        let closure_symbols =
          Closure_id.Map.mapi (fun closure_id _function_decl ->
              (* CR mshinwell: share name computation with [Simplify_named] *)
              let name =
                closure_id
                |> Closure_id.rename
                |> Closure_id.to_string
                |> Linkage_name.create 
              in
              Symbol.create (Compilation_unit.get_current_exn ()) name)
            function_decls
        in
        (*
        Format.eprintf "Var %a: set of closures:@ %a@ vars:@ %a\n%!"
          Variable.print var
          (Closure_id.Map.print Symbol.print) closure_symbols
          (Var_within_closure.Map.print Simple.print) closure_vars;
        *)
        let dacc =
          DA.map_denv dacc ~f:(fun denv ->
            let denv =
              Closure_id.Map.fold (fun _closure_id symbol denv ->
                  DE.define_symbol denv symbol K.value)
                closure_symbols
                denv
            in
            match Closure_id.Map.find closure_id closure_symbols with
            | exception Not_found ->
              Misc.fatal_errorf "Variable %a claimed to hold closure with \
                  closure ID %a, but no symbol was found for that closure ID"
                Variable.print var
                Closure_id.print closure_id
            | symbol ->
              DE.add_equation_on_name denv
                (Name.var var)
                (T.alias_type_of K.value (Simple.symbol symbol)))
        in
        let function_decls = Function_declarations.create function_decls in
        let set_of_closures =
          Set_of_closures.create function_decls ~closure_elements:closure_vars
        in
        let static_structure_part : Static_structure.t0 =
          S (Set_of_closures { closure_symbols; },
             Set_of_closures set_of_closures)
        in
        result_dacc, dacc, (var, static_structure_part) :: reified_definitions
      | Simple _ | Cannot_reify | Invalid ->
        result_dacc, dacc, reified_definitions
      end)
    computed_values
    (result_dacc, dacc, [])

module Bindings_top_sort =
  Top_closure.Make
    (struct
      type t = Variable.Set.t
      type elt = Variable.t
      let empty = Variable.Set.empty
      let add t elt = Variable.Set.add elt t
      let mem t elt = Variable.Set.mem elt t
    end)
    (struct
      type 'a t = 'a
      let return t = t
      let (>>=) t f = f t
    end)

let simplify_return_continuation_handler dacc
      ~(extra_params_and_args : Continuation_extra_params_and_args.t)
      cont (return_cont_handler : Return_cont_handler.Opened.t)
      ~user_data:result_dacc _k =
  let result_dacc = DA.with_r result_dacc (DA.r dacc) in
  let allowed_free_vars =
    Variable.Set.union
      (Variable.Set.of_list
        (List.map KP.var return_cont_handler.computed_values))
      (Variable.Set.of_list
        (List.map KP.var extra_params_and_args.extra_params))
  in
  let static_structure, result_dacc =
    let result_dacc, dacc, reified_definitions =
      (* If the type of a computed value can be reified (to a term possibly
         involving any extra params and args that are present, from unboxing
         etc.) then lift that computed value to its own [Static_part]. *)
      reify_types_of_computed_values dacc ~result_dacc allowed_free_vars
    in
    let top_sorted_reified_definitions =
      (* Use a topological sort to try to order the bindings so that as many
         computed value variables can be changed into symbols as possible.
         If there is a cycle, we just pick an order.  The worst that will happen
         is that some variables won't simplify to symbols (and will remain
         as computed values). *)
      (* CR mshinwell: I'm a bit concerned that if the top-sort fails, then
         we could end up with symbols being defined after they are used,
         in the list of static parts.  Should we completely abort in this
         case and fall back to the original environment that doesn't have the
         equalities to symbols in it? *)
      match
        Bindings_top_sort.top_closure reified_definitions
          ~key:(fun (var, Static_structure.S (_symbols, _static_part)) -> var)
          ~deps:(fun (_var, Static_structure.S (symbols, static_part)) ->
            let var_deps =
              static_part
              |> Static_part.free_names
              |> Name_occurrences.variables
              |> Variable.Set.elements
            in
            (*
            Format.eprintf "Deps for %a are %a\n%!"
              Variable.print var
              Variable.Set.print (Variable.Set.of_list var_deps);
            *)
            (* Everything except the [var] in the following list will be
               ignored. *)
            List.map (fun var -> var, Static_structure.S (symbols, static_part))
              var_deps)
      with
      | Ok sorted -> sorted
      | Error _ -> reified_definitions
    in
    let static_structure : Static_structure.t =
      let top_sorted_reified_definitions =
        (* The [List.rev] relies on the following property:
             Let the list L be a topological sort of a directed graph G.
             Then the reverse of L is a topological sort of the transpose of G.
        *)
        List.map (fun (_var, static_structure_part) -> static_structure_part)
          (List.rev top_sorted_reified_definitions)
      in
      (*
      Format.eprintf "New defs:@ %a\n"
        Static_structure.print top_sorted_reified_definitions;
      Format.eprintf "Existing defs:@ %a\n"
        Static_structure.print return_cont_handler.static_structure;
      *)
      top_sorted_reified_definitions @ return_cont_handler.static_structure
    in
    let result_dacc, static_structure =
      simplify_static_structure dacc ~result_dacc static_structure
    in
    static_structure, result_dacc
  in
  let handler, result_dacc, uacc =
    let free_variables =
      Name_occurrences.variables
        (Static_structure.free_names static_structure)
    in
    let used_computed_values =
      List.filter (fun param ->
          Variable.Set.mem (KP.var param) free_variables)
        return_cont_handler.computed_values
    in
    let used_extra_params =
      List.filter (fun extra_param ->
          Variable.Set.mem (KP.var extra_param) free_variables)
        extra_params_and_args.extra_params
    in
    let computed_values = used_computed_values @ used_extra_params in
    let handler : Return_cont_handler.t =
      { computed_values;
        static_structure;
      }
    in
    let rewrite =
      Apply_cont_rewrite.create
        ~original_params:return_cont_handler.computed_values
        ~used_params:(KP.Set.of_list used_computed_values)
        ~extra_params:extra_params_and_args.extra_params
        ~extra_args:extra_params_and_args.extra_args
        ~used_extra_params:(KP.Set.of_list used_extra_params)
    in
    let uenv = UE.add_apply_cont_rewrite UE.empty cont rewrite in
    handler, result_dacc, UA.create uenv (DA.r result_dacc)
  in
  (* CR mshinwell: It would maybe be easier to avoid returning [result_dacc].
     This would also match [Simplify_expr]. *)
  (* CR mshinwell: Returning [computed_values] and [static_structure] is
     redundant, they're in [handler]. *)
  handler,
    (handler.Return_cont_handler.computed_values,
     handler.Return_cont_handler.static_structure,
     result_dacc), uacc

let simplify_exn_continuation_handler dacc
      ~extra_params_and_args:_ _cont
      (_handler : Exn_cont_handler.Opened.t) ~user_data:_ k =
  let handler : Exn_cont_handler.t = () in
  let user_data, uacc = k (DA.continuation_uses_env dacc) (DA.r dacc) in
  handler, user_data, uacc

let simplify_definition dacc (defn : Program_body.Definition.t) =
  let dacc, computation, static_structure =
    match defn.computation with
    | None ->
      let dacc, static_structure =
        simplify_static_structure dacc ~result_dacc:dacc defn.static_structure
      in
      dacc, None, static_structure
    | Some computation ->
      let return_continuation = computation.return_continuation in
      let return_cont_handler : Return_cont_handler.t =
        { computed_values = computation.computed_values;
          static_structure = defn.static_structure;
        }
      in
      let exn_continuation = computation.exn_continuation in
      let exn_cont_handler : Exn_cont_handler.t = () in
      let result_dacc = dacc in
      let expr, _handler, (computed_values, static_structure, result_dacc),
          uacc =
        let simplify_body dacc expr k =
          let expr, _handler, user_data, uacc =
            let simplify_body : _ Simplify_exn_cont.simplify_body =
              { simplify_body = Simplify_expr.simplify_expr; }
            in
            Simplify_exn_cont.simplify_body_of_non_recursive_let_cont dacc
              (Exn_continuation.exn_handler exn_continuation)
              exn_cont_handler
              ~simplify_body
              ~body:expr
              ~simplify_continuation_handler_like:
                simplify_exn_continuation_handler
              ~user_data:result_dacc
              k
          in
          expr, user_data, uacc
        in
        let simplify_body : _ Simplify_return_cont.simplify_body =
          { simplify_body; }
        in
        Simplify_return_cont.simplify_body_of_non_recursive_let_cont dacc
          return_continuation
          return_cont_handler
          ~simplify_body
          ~body:computation.expr
          ~simplify_continuation_handler_like:
            simplify_return_continuation_handler
          ~user_data:result_dacc
          (fun _cont_uses_env r ->
            let uacc = UA.create UE.empty r in
            (* CR mshinwell: This should return an "invalid" node. *)
            (computation.computed_values, defn.static_structure,
              result_dacc), uacc)
      in
      let dacc = result_dacc in
      let dacc = DA.with_r dacc (UA.r uacc) in (* CR mshinwell: needed? *)
      let computation_can_be_deleted =
        match Expr.descr expr with
        | Apply_cont apply_cont ->
          begin match Apply_cont.to_goto apply_cont with
          | Some cont when Continuation.equal cont return_continuation -> true
          | _ -> false
          end
        | Let _ | Let_cont _ | Apply _ | Switch _ | Invalid _ -> false
      in
      let computation : Program_body.Computation.t option =
        if computation_can_be_deleted then None
        else
          Some ({
            expr;
            return_continuation;
            exn_continuation = computation.exn_continuation;
            computed_values;
          })
      in
      dacc, computation, static_structure
  in
  let definition : Program_body.Definition.t =
    { static_structure;
      computation;
    }
  in
  definition, dacc

let define_lifted_constants lifted_constants (body : Program_body.t) =
  List.fold_left (fun body lifted_constant : Program_body.t ->
      let definition = Lifted_constant.definition lifted_constant in
      let static_structure =
        (* CR mshinwell: We should have deletion of unused symbols
           automatically -- needs to be done for non-lifted constants too *)
        Static_structure.delete_bindings definition.static_structure
          ~allowed:(Name_occurrences.symbols (Program_body.free_names body))
      in
      if Static_structure.is_empty static_structure then body
      else
        let definition : Program_body.Definition.t =
          { computation = definition.computation;
            static_structure;
          }
        in
        Program_body.define_symbol definition ~body)
    body
    lifted_constants

let rec simplify_program_body0 dacc (body : Program_body.t) k =
  match Program_body.descr body with
  | Define_symbol (defn, body) ->
    let dacc = DA.map_r dacc ~f:(fun r -> R.clear_lifted_constants r) in
    let defn, dacc = simplify_definition dacc defn in
    let r = DA.r dacc in
    simplify_program_body0 dacc body (fun body dacc ->
      let body = Program_body.define_symbol defn ~body in
      let body = define_lifted_constants (R.get_lifted_constants r) body in
      k body dacc)
  | Root _ -> k body dacc

let simplify_program_body dacc body =
  simplify_program_body0 dacc body (fun body dacc -> body, dacc)

let check_imported_symbols_don't_overlap_predef_exns
      ~imported_symbols ~predef_exn_symbols ~descr =
  let wrong_symbols =
    Symbol.Set.inter (Symbol.Map.keys imported_symbols)
      (Symbol.Map.keys predef_exn_symbols)
  in
  if not (Symbol.Set.is_empty wrong_symbols) then begin
    Misc.fatal_errorf "Program's [imported_symbols] (%s) must not contain \
        predefined exception symbols"
      descr
  end

let simplify_program denv (program : Program.t) : Program.t =
  let backend = DE.backend denv in
  let module Backend = (val backend : Flambda2_backend_intf.S) in
  let predef_exn_symbols =
    Symbol.Set.fold (fun symbol predef_exn_symbols ->
        Symbol.Map.add symbol K.value predef_exn_symbols)
      Backend.all_predefined_exception_symbols
      Symbol.Map.empty
  in
  let denv =
    Symbol.Map.fold (fun symbol kind denv ->
        DE.add_symbol denv symbol (T.unknown kind))
      (Symbol.Map.disjoint_union program.imported_symbols predef_exn_symbols)
      denv
  in
  check_imported_symbols_don't_overlap_predef_exns
    ~imported_symbols:program.imported_symbols ~predef_exn_symbols
    ~descr:"before simplification";
  let r = R.create ~resolver:(DE.resolver denv) in
  let dacc = DA.create denv Continuation_uses_env.empty r in
  let body, dacc = simplify_program_body dacc program.body in
  let r = DA.r dacc in
  let imported_symbols = R.imported_symbols r in
  check_imported_symbols_don't_overlap_predef_exns
    ~imported_symbols:imported_symbols ~predef_exn_symbols
    ~descr:"after simplification";
  { imported_symbols;
    body;
  }
