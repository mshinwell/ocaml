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
  let { Simplify_named.
        set_of_closures;
        closure_types_by_bound_name;
        newer_versions_of;
        code;
        dacc;
        result_dacc;
      } =
    Simplify_named.simplify_set_of_closures0 dacc ~result_dacc set_of_closures
      ~closure_bound_names ~closure_elements ~closure_element_types
  in
  let static_structure : Static_structure.t =
    let code =
      Code_id.Map.mapi (fun code_id params_and_body : Static_part.code ->
          { params_and_body = Present params_and_body;
            newer_version_of = Code_id.Map.find_opt code_id newer_versions_of;
          })
        code
    in
    let static_part : K.fabricated Static_part.t =
      Code_and_set_of_closures {
        code;
        set_of_closures = Some set_of_closures;
      }
    in
    let bound_symbols : K.fabricated Program_body.Bound_symbols.t =
      Code_and_set_of_closures {
        code_ids = Code_id.Map.keys code;
        closure_symbols;
      }
    in
    Static_structure.create [S (bound_symbols, static_part)]
      ~symbol_placeholders:Symbol.Map.empty
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
  let { Simplify_named.
        can_lift = _;
        closure_elements;
        closure_element_types;
      } =
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
     prior to the current [Definition].

     [result_dacc] holds the environment, which we are computing, that will be
     in effect immediately after the current [Definition].

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
          DE.typing_env (DE.define_symbol result_denv result_sym K.value)
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
      ~code_ids ~closure_symbols
    : K.fabricated Static_part.t * DA.t * DA.t =
  match static_part with
  | Code_and_set_of_closures { code; set_of_closures; } ->
    let code_ids' = Code_id.Map.keys code in
    if not (Code_id.Set.equal code_ids code_ids') then begin
      Misc.fatal_errorf "Mismatch on declared code IDs (%a and %a):@ %a"
        Code_id.Set.print code_ids
        Code_id.Set.print code_ids'
        Static_part.print static_part
    end; 
    let dacc, result_dacc =
      Code_id.Map.fold
        (fun code_id ({ params_and_body; newer_version_of; } : Static_part.code)
             (dacc, result_dacc) ->
          (* CR mshinwell: Add invariant check to ensure there are no
             unbound names in the code, since we're not simplifying on the
             way down. *)
          let define_code denv =
            match params_and_body with
            | Deleted -> denv
            | Present params_and_body ->
              DE.define_code denv ?newer_version_of ~code_id ~params_and_body
          in
          let dacc = DA.map_denv dacc ~f:define_code in
          let result_dacc = DA.map_denv result_dacc ~f:define_code in
          dacc, result_dacc)
        code
        (dacc, result_dacc)
    in
    let set_of_closures, dacc, result_dacc =
      match set_of_closures with
      | None -> None, dacc, result_dacc
      | Some set_of_closures ->
        let set_of_closures, dacc, result_dacc, _static_structure_types,
            _static_structure =
          simplify_set_of_closures dacc ~result_dacc set_of_closures
            ~closure_symbols
        in
        Some set_of_closures, dacc, result_dacc
    in
    Code_and_set_of_closures { code; set_of_closures; }, dacc, result_dacc

let simplify_piece_of_static_structure (type k) dacc ~result_dacc
      (bound_syms : k Program_body.Bound_symbols.t)
      (static_part : k Static_part.t)
      : k Static_part.t * DA.t * DA.t =
  (*
  Format.eprintf "Piece: dacc:@ %a@ result_dacc:@ %a@ %a\n%!"
    DA.print dacc
    DA.print result_dacc
    Static_part.print static_part;
  *)
  match bound_syms with
  | Singleton result_sym ->
    simplify_static_part_of_kind_value dacc ~result_dacc static_part
      ~result_sym
  | Code_and_set_of_closures { code_ids; closure_symbols; } ->
    simplify_static_part_of_kind_fabricated dacc ~result_dacc static_part
      ~code_ids ~closure_symbols

let simplify_static_structure dacc ~result_dacc static_structure
      : DA.t * Static_structure.t =
  let str_rev, _dacc, result_dacc =
    List.fold_left
      (fun (str_rev, dacc, result_dacc)
           (Static_structure.S (bound_syms, static_part)) ->
        let static_part, dacc, result_dacc =
          simplify_piece_of_static_structure dacc ~result_dacc
            bound_syms static_part
        in
        let binding : Static_structure.t0 =
          S (bound_syms, static_part)
        in
        let str_rev = binding :: str_rev in
        str_rev, dacc, result_dacc)
      ([], dacc, result_dacc)
      (Static_structure.bindings static_structure)
  in
  let static_structure =
    Static_structure.create (List.rev str_rev)
      ~symbol_placeholders:
        (Static_structure.symbol_placeholders static_structure)
  in
  result_dacc, static_structure

let reify_types_of_computed_values dacc computed_values =
  let typing_env = DE.typing_env (DA.denv dacc) in
  Variable.Set.fold
    (fun var (dacc, computed_value_vars_to_symbols, reified_definitions,
              closure_symbols_by_set) ->
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
      | Some _ ->
        dacc, computed_value_vars_to_symbols, reified_definitions,
          closure_symbols_by_set
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
          let reified_definitions =
            static_structure_part :: reified_definitions
          in
          let computed_value_vars_to_symbols =
            Variable.Map.add var symbol computed_value_vars_to_symbols
          in
          dacc, computed_value_vars_to_symbols, reified_definitions,
            closure_symbols_by_set
      | Lift_set_of_closures { closure_id; function_decls; closure_vars; } ->
        let module I = T.Function_declaration_type.Inlinable in
        let set_of_closures =
          let function_decls =
            Closure_id.Map.mapi (fun closure_id inlinable ->
              Function_declaration.create ~code_id:(I.code_id inlinable)
                ~params_arity:(I.param_arity inlinable)
                ~result_arity:(I.result_arity inlinable)
                ~stub:(I.stub inlinable)
                ~dbg:(I.dbg inlinable)
                ~inline:(I.inline inlinable)
                ~is_a_functor:(I.is_a_functor inlinable)
                ~recursive:(I.recursive inlinable))
              function_decls
            |> Function_declarations.create
          in
          Set_of_closures.create function_decls ~closure_elements:closure_vars
        in
        let bind_computed_value_var_to_symbol dacc ~closure_symbols =
          let dacc, symbol =
            DA.map_denv2 dacc ~f:(fun denv ->
              match Closure_id.Map.find closure_id closure_symbols with
              | exception Not_found ->
                Misc.fatal_errorf "Variable %a claimed to hold closure with \
                    closure ID %a, but no symbol was found for that closure ID"
                  Variable.print var
                  Closure_id.print closure_id
              | symbol ->
                let denv =
                  DE.add_equation_on_name denv
                    (Name.var var)
                    (T.alias_type_of K.value (Simple.symbol symbol))
                in
                denv, symbol)
          in
          let computed_value_vars_to_symbols =
            Variable.Map.add var symbol computed_value_vars_to_symbols
          in
          dacc, computed_value_vars_to_symbols
        in
        begin match
          Set_of_closures.Map.find set_of_closures closure_symbols_by_set
        with
        | exception Not_found ->
          let closure_symbols =
            Closure_id.Map.mapi (fun closure_id _function_decl ->
                (* CR mshinwell: share name computation with
                   [Simplify_named] *)
                let name =
                  closure_id
                  |> Closure_id.rename
                  |> Closure_id.to_string
                  |> Linkage_name.create 
                in
                Symbol.create (Compilation_unit.get_current_exn ()) name)
              function_decls
          in
          let static_structure_part =
            Static_structure.pieces_of_code
              ~newer_versions_of:Code_id.Map.empty
              ~set_of_closures:(closure_symbols, set_of_closures)
              Code_id.Map.empty
          in
          let reified_definitions =
            static_structure_part :: reified_definitions
          in
          let closure_symbols_by_set =
            Set_of_closures.Map.add set_of_closures closure_symbols
              closure_symbols_by_set
          in
          let dacc, computed_value_vars_to_symbols =
            bind_computed_value_var_to_symbol dacc ~closure_symbols
          in
          dacc, computed_value_vars_to_symbols, reified_definitions,
            closure_symbols_by_set
        | closure_symbols ->
          let dacc, computed_value_vars_to_symbols =
            bind_computed_value_var_to_symbol dacc ~closure_symbols
          in
          dacc, computed_value_vars_to_symbols, reified_definitions,
            closure_symbols_by_set
        end
      | Simple _ | Cannot_reify | Invalid ->
        dacc, computed_value_vars_to_symbols, reified_definitions,
          closure_symbols_by_set
      end)
    computed_values
    (dacc, Variable.Map.empty, [], Set_of_closures.Map.empty)

module Bindings_top_sort =
  Top_closure.Make
    (struct
      type t = Symbol.Set.t
      (* The following works because every binding that we will sort has
         a different symbol (including, for set-of-closures bindings,
         different symbols for each closure). *)
      type elt = Symbol.Set.t
      let empty = Symbol.Set.empty
      let add t elt = Symbol.Set.union elt t
      let mem t elt = Symbol.Set.subset elt t
    end)
    (struct
      type 'a t = 'a
      let return t = t
      let (>>=) t f = f t
    end)

let simplify_return_continuation_handler dacc
      ~(extra_params_and_args : Continuation_extra_params_and_args.t)
      cont (return_cont_handler : Return_cont_handler.Opened.t)
      ~user_data:(result_dacc, symbol_placeholders) _k =
  let result_dacc =
    (* [DA.r dacc] contains the lifted constants etc. arising from the
       simplification of any associated [computation]. *)
    let r = DA.r dacc in
    DA.map_denv (DA.with_r result_dacc r) ~f:(fun denv ->
      DE.add_lifted_constants denv ~lifted:(R.get_lifted_constants r))
  in
  let allowed_free_vars =
    Variable.Set.union (KP.List.var_set return_cont_handler.computed_values)
      (KP.List.var_set extra_params_and_args.extra_params)
  in
  let static_structure, result_dacc =
    let dacc, computed_value_vars_to_symbols, reified_definitions,
        closure_symbols_by_set =
      (* If the type of a computed value can be reified (to a term possibly
         involving any extra params and args that are present, from unboxing
         etc.) then lift that computed value to its own [Static_part]. *)
      reify_types_of_computed_values dacc allowed_free_vars
    in
    (* CR mshinwell: If recursion extends beyond that which can be handled
       by the set-of-closures cases, then we would need a strongly connected
       components analysis, prior to the top sort.  Any set arising from SCC
       that has more than one element must be a complicated recursive case,
       which can be dealt with using the "symbol placeholder" approach
       (variables that are substituted for the computed values, which are
       in turn substituted for symbols at the Cmm translation phase).
       (Any case containing >1 set of closures is maybe a bug?) *)
    let reified_definitions =
      let sym_dep_graph =
        List.fold_left
          (fun sym_dep_graph (Static_structure.S (syms, static_part)) ->
            let being_defined = Bound_symbols.being_defined syms in
            let var_deps =
              static_part
              |> Static_part.free_names
              |> Name_occurrences.variables
              |> Variable.Set.elements
            in
            let sym_deps =
              List.fold_left (fun sym_deps var ->
                  match
                    Variable.Map.find var computed_value_vars_to_symbols
                  with
                  | exception Not_found ->
                    Misc.fatal_errorf "No symbol for computed value var %a"
                      Variable.print var
                  | symbol -> Symbol.Set.add symbol sym_deps)
                Symbol.Set.empty
                var_deps
            in
            let sym_deps =
              let closure_symbols_being_defined =
                (* Closure symbols are bound recursively within the same set.
                   We need to remove them from the dependency sets to avoid
                   the topological sort seeing cycles.  We don't unilaterally
                   remove all of the symbols in [being_defined] (as opposed to
                   [closure_symbols_being_defined]) as, in the case where
                   illegal recursion has been constructed, it could cause the
                   topological sort to succeed and the fatal error below to be
                   concealed. *)
                Bound_symbols.closure_symbols_being_defined syms
              in
              Symbol.Set.diff sym_deps closure_symbols_being_defined
            in
            Symbol.Set.Map.add being_defined sym_deps sym_dep_graph)
          Symbol.Set.Map.empty
          reified_definitions
      in
      match
        Bindings_top_sort.top_closure
          (Symbol.Set.Set.elements (Symbol.Set.Map.keys sym_dep_graph))
          ~key:Fun.id
          ~deps:(fun key ->
            let deps = Symbol.Set.Map.find key in
            Symbol.Set.fold (fun sym result ->
                (Symbol.Set.singleton sym) :: result)
              deps
              [])
      with
      | Ok sorted ->
        (* The [List.rev] relies on the following property:
             Let the list L be a topological sort of a directed graph G.
             Then the reverse of L is a topological sort of the transpose of G.
        *)
        List.map (fun (_, _, static_structure_part) -> static_structure_part)
          (List.rev sorted)
      | Error _ ->
        Misc.fatal_errorf "Static structure arising from reified \
            computed values contains recursion that cannot be compiled:@ %a"
          Static_structure.print
          (Static_structure.create static_structure_pieces
            ~symbol_placeholders)
    in
    let static_structure_pieces : Static_structure.t0 list =
      reified_definitions @
        (Static_structure.bindings return_cont_handler.static_structure)
    in
    let static_structure =
      Static_structure.create static_structure_pieces ~symbol_placeholders
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
      let symbol_placeholders =
        Static_structure.symbol_placeholders defn.static_structure
      in
      let dacc =
        DA.map_denv dacc ~f:(fun denv ->
          Symbol.Set.fold (fun symbol denv ->
              match Symbol.Map.find symbol symbol_placeholders with
              | exception Not_found -> denv
              | symbol_placeholder ->
                DE.add_variable denv
                  (Var_in_binding_pos.create symbol_placeholder NM.normal)
                  (T.any_value ()))
            (Static_structure.being_defined defn.static_structure)
            denv)
      in
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
          ~user_data:(result_dacc, symbol_placeholders)
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

(* CR mshinwell: We should simplify code on the way up if we don't delete it. *)

let define_lifted_constants lifted_constants ~current_definition =
  let defined_symbols_current_definition =
    Definition.being_defined current_definition
  in
  let defined_code_ids_current_definition =
    Definition.code_being_defined current_definition
  in
  List.fold_left
    (fun (add_to_current_definition, add_around_body) lifted_constant ->
      let definition = Lifted_constant.definition lifted_constant in
      assert (Option.is_none definition.computation);
      let free_names =
        Static_structure.free_names definition.static_structure
      in
      let free_symbols = Name_occurrences.symbols free_names in
      let free_code_ids = Name_occurrences.code_ids free_names in
      (* XXX When checking overlap, we need to take into account symbols that
         we have lifted into the current definition, so that their dependencies
         (which also need to come into the current definition) are handled
         correctly. *)
      let no_overlap_with_current_definition =
        let no_overlap_with_symbols =
          Symbol.Set.is_empty
            (Symbol.Set.inter free_symbols
              defined_symbols_current_definition)
        in
        let no_overlap_with_code_ids =
          Code_id.Set.is_empty
            (Code_id.Set.inter free_code_ids
              defined_code_ids_current_definition)
        in
        no_overlap_with_symbols && no_overlap_with_code_ids
      in
      if no_overlap_with_current_definition then
        (* XXX This should add things to the current definition if there
           is recursion

           - Where do they get added though?  They may need to go in some
           particular set of closures, and there might be multiple of those
           in a given definition.  Maybe this function is being called too
           late, and we should add lifted constants after each static
           structure _piece_ has been simplified?  Or record in the environment
           which symbol's definition is currently being simplified (also
           useful for reify, as below), and then record that also on each
           lifted constant.  Then we can check here which symbol's
           definition needs augmenting.  Seems like that might be quite easy.
           (Do this first on Monday, plus the set-of-closures equiv class
           stuff and revised top-sort.)

           - Also for normal reify (not computed values reify), we also need
           to check if we have symbols currently being defined in the reified
           type, and if so not lift (except for closures).
           An alternative: add a computation that reads from the symbol
           being defined and fills in the fields.
           [ How does this relate to the new symbol_placeholders? ] *)
        let add_around_body = definition :: add_around_body in
        add_to_current_definition, add_around_body
      else
        let add_to_current_definition =
          List.flatten [
            Static_structure.bindings definition.static_structure;
            add_to_current_definition;
          ]
        in
        add_to_current_definition, add_around_body)
    ([], [])
    lifted_constants

let rec simplify_program_body0 dacc (body : Program_body.t) k =
  match Program_body.descr body with
  | Definition (defn, body) ->
    let dacc = DA.map_r dacc ~f:(fun r -> R.clear_lifted_constants r) in
    let defn, dacc = simplify_definition dacc defn in
    let r = DA.r dacc in
    simplify_program_body0 dacc body (fun body dacc ->
      let add_to_current_definition, add_around_body =
        define_lifted_constants (R.get_lifted_constants r)
          ~current_definition:defn
      in
      let static_structure =
        Static_structure.prepend_bindings defn.static_structure
          ~prepend:add_to_current_definition;
      in
      let defn =
        { defn with
          static_structure;
        }
      in
      let body =
        Program_body.define_symbol defn ~body
          (TE.code_age_relation (DE.typing_env (DA.denv dacc)))
      in
      let body =
        List.fold_left (fun body (defn : Definition.t) ->
            let static_structure =
              (* CR mshinwell: We should have deletion of unused symbols
                 automatically -- needs to be done for non-lifted constants
                 too *)
              Static_structure.delete_bindings defn.static_structure
                ~free_names_after:(Program_body.free_names body)
                (TE.code_age_relation (DE.typing_env (DA.denv dacc)))
            in
            if Static_structure.has_no_bindings static_structure then body
            else
              let defn : Program_body.Definition.t =
                { computation = None;
                  static_structure;
                }
              in
              Program_body.define_symbol defn ~body
                (TE.code_age_relation (DE.typing_env (DA.denv dacc))))
          body
          (List.rev add_around_body)
      in
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
