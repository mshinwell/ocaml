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
(*   special exception on linking described in the file LICENSDE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Simplify_import

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
    cont = Continuation.create ();
    arg = Tagged_immediate Immediate.zero;
    arity = [];
  }
  let _ = Alias_for { arity = []; alias_for = Continuation.create (); }

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
    cont = Continuation.create ();
    arg = Tagged_immediate Immediate.zero;
    arity = [];
  }
  let _ = Alias_for { arity = []; alias_for = Continuation.create (); }

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
    let min_occurrence_kind = Name_occurrence_kind.normal in
    match S.simplify_simple dacc (Simple.var var) ~min_occurrence_kind with
    | Bottom, ty ->
      assert (K.equal (T.kind ty) K.value);
      (* CR mshinwell: This should be "invalid" and propagate up *)
      of_kind_value, T.bottom K.value
    | Ok simple, ty ->
      match Simple.descr simple with
      | Name (Symbol sym) -> Of_kind_value.Symbol sym, ty
      | Name (Var _) -> of_kind_value, ty
      | Const (Tagged_immediate imm) -> Of_kind_value.Tagged_immediate imm, ty
      | Const (Naked_float _ | Naked_int32 _
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
  let set_of_closures, closure_types_by_bound_name, result_dacc =
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
    S [bound_symbols, static_part]
  in
  let static_structure_types =
    Name_in_binding_pos.Map.fold
      (fun name closure_type static_structure_types ->
        let symbol = Name_in_binding_pos.must_be_symbol name in
        Symbol.Map.add symbol closure_type static_structure_types)
      closure_types_by_bound_name
      Symbol.Map.empty
  in
  set_of_closures, result_dacc, static_structure_types, static_structure

let simplify_set_of_closures dacc ~result_dacc set_of_closures
      ~closure_symbols =
  let can_lift, closure_elements, closure_element_types =
    Simplify_named.type_closure_elements_and_make_lifting_decision dacc
      ~min_occurrence_kind:Name_occurrence_kind.normal set_of_closures
  in
  if not can_lift then begin
    Misc.fatal_errorf "Set of closures cannot be statically allocated:@ %a"
      Set_of_closures.print set_of_closures
  end;
  simplify_set_of_closures0 dacc ~result_dacc set_of_closures
    ~closure_symbols ~closure_elements ~closure_element_types

let simplify_static_part_of_kind_value dacc
      (static_part : K.value Static_part.t) ~result_sym
      : K.value Static_part.t * DA.t =
  let bind_result_sym ty =
    DA.map_denv dacc ~f:(fun denv -> DE.add_symbol denv result_sym ty)
  in
  match static_part with
  | Block (tag, is_mutable, fields) ->
    let fields_with_tys =
      List.map (fun of_kind_value ->
          simplify_of_kind_value dacc of_kind_value)
        fields
    in
    let fields, field_tys = List.split fields_with_tys in
    let ty = T.immutable_block (Tag.Scannable.to_tag tag) ~fields:field_tys in
    let dacc = bind_result_sym ty in
    Block (tag, is_mutable, fields), dacc
  | Fabricated_block var ->
    DE.check_variable_is_bound (DA.denv dacc) var;
    let dacc = bind_result_sym (T.any_value ()) in
    static_part, dacc
  (* CR mshinwell: Need to reify to change Equals types into new terms *)
  | Boxed_float or_var ->
    let or_var, ty =
      simplify_or_variable dacc (fun f -> T.this_boxed_float f) or_var
    in
    let dacc = bind_result_sym ty in
    Boxed_float or_var, dacc
  | Boxed_int32 or_var ->
    let or_var, ty =
      simplify_or_variable dacc (fun f -> T.this_boxed_int32 f) or_var
    in
    let dacc = bind_result_sym ty in
    Boxed_int32 or_var, dacc
  | Boxed_int64 or_var ->
    let or_var, ty =
      simplify_or_variable dacc (fun f -> T.this_boxed_int64 f) or_var
    in
    let dacc = bind_result_sym ty in
    Boxed_int64 or_var, dacc
  | Boxed_nativeint or_var ->
    let or_var, ty =
      simplify_or_variable dacc (fun f -> T.this_boxed_nativeint f) or_var
    in
    let dacc = bind_result_sym ty in
    Boxed_nativeint or_var, dacc
  | Immutable_float_array fields ->
    let fields_with_tys =
      List.map (fun field ->
          simplify_or_variable dacc
            (fun f -> T.this_naked_float f)
            field)
        fields
    in
    let fields, _field_tys = List.split fields_with_tys in
    let dacc = bind_result_sym (T.any_value ()) in
    Immutable_float_array fields, dacc
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
    let dacc = bind_result_sym str_ty in
    static_part, dacc
  | Immutable_string or_var ->
    let or_var, ty =
      simplify_or_variable dacc (fun str -> T.this_immutable_string str)
        or_var
    in
    let dacc = bind_result_sym ty in
    Immutable_string or_var, dacc

let simplify_static_part_of_kind_fabricated dacc ~result_dacc
      (static_part : K.fabricated Static_part.t)
      ~closure_symbols
    : K.fabricated Static_part.t * DA.t =
  match static_part with
  | Set_of_closures set_of_closures ->
     let set_of_closures, dacc, _static_structure_types, _static_structure =
       simplify_set_of_closures dacc ~result_dacc set_of_closures
         ~closure_symbols
     in
     Set_of_closures set_of_closures, dacc

let simplify_piece_of_static_structure (type k) dacc ~result_dacc
      (bound_syms : k Program_body.Bound_symbols.t)
      (static_part : k Static_part.t)
      : k Static_part.t * DA.t =
  match bound_syms with
  | Singleton result_sym ->
    simplify_static_part_of_kind_value dacc static_part ~result_sym
  | Set_of_closures { closure_symbols; } ->
    simplify_static_part_of_kind_fabricated dacc ~result_dacc static_part
      ~closure_symbols

let simplify_static_structure dacc
      ((S pieces) : Program_body.Static_structure.t)
      : DA.t * Program_body.Static_structure.t =
  let str_rev, next_dacc =
    (* The bindings in the individual pieces of the [Static_structure] are
       simultaneous, so we keep a [result_dacc] accumulating the final
       environment, but always use [dacc] for the simplification of the
       pieces. *)
    List.fold_left (fun (str_rev, result_dacc) (bound_syms, static_part) ->
        let static_part, result_dacc =
          simplify_piece_of_static_structure dacc ~result_dacc
            bound_syms static_part
        in
        let str_rev = (bound_syms, static_part) :: str_rev in
        str_rev, result_dacc)
      ([], dacc)
      pieces
  in
  next_dacc, S (List.rev str_rev)

let simplify_return_continuation_handler dacc
      ~extra_params_and_args:_ cont
      (return_cont_handler : Return_cont_handler.Opened.t) _k =
  let dacc, static_structure =
    simplify_static_structure dacc return_cont_handler.static_structure
  in
  let handler, used_computed_values, uenv =
    let free_variables = Static_structure.free_variables static_structure in
    let original_computed_values = return_cont_handler.computed_values in
    let used_computed_values =
      List.filter (fun param ->
          Variable.Set.mem (KP.var param) free_variables)
        original_computed_values
    in
    let handler : Return_cont_handler.t =
      { computed_values = used_computed_values;
        static_structure;
      }
    in
    let rewrite =
      Apply_cont_rewrite.create ~original_params:original_computed_values
        ~used_params:(KP.Set.of_list used_computed_values)
        ~extra_params:[]
        ~extra_args:Apply_cont_rewrite_id.Map.empty
        ~used_extra_params:KP.Set.empty
    in
    let uenv = UE.add_apply_cont_rewrite UE.empty cont rewrite in
    handler, used_computed_values, uenv
  in
  let uacc = UA.create uenv (DA.r dacc) in
  handler, (used_computed_values, static_structure, dacc), uacc

let simplify_exn_continuation_handler dacc
      ~extra_params_and_args:_ _cont
      (_handler : Exn_cont_handler.Opened.t) k =
  let handler : Exn_cont_handler.t = () in
  let user_data, uacc = k (DA.continuation_uses_env dacc) (DA.r dacc) in
  handler, user_data, uacc

let simplify_definition dacc (defn : Program_body.Definition.t) =
  let dacc, computation, static_structure =
    match defn.computation with
    | None ->
      let dacc, static_structure =
        simplify_static_structure dacc defn.static_structure
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
      let expr, _handler, (computed_values, static_structure, dacc), uacc =
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
          (fun _cont_uses_env r ->
            let uacc = UA.create UE.empty r in
            (* CR mshinwell: This should return an "invalid" node. *)
            (computation.computed_values, defn.static_structure, dacc), uacc)
      in
      let dacc = DA.with_r dacc (UA.r uacc) in
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
      let static_structure =
        (* CR mshinwell: We should have deletion of unused symbols
           automatically -- needs to be done for non-lifted constants too *)
        Static_structure.delete_bindings
          (Lifted_constant.static_structure lifted_constant)
          ~allowed:(Program_body.free_symbols body)
      in
      if Static_structure.is_empty static_structure then body
      else
        let definition : Program_body.Definition.t =
          { computation = None;
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
