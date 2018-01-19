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

module E = Simplify_env_and_result.Env
module K = Flambda_kind
module R = Simplify_env_and_result.Result
module T = Flambda_type

module Of_kind_value = Flambda_static.Of_kind_value
module Program = Flambda_static.Program
module Program_body = Flambda_static.Program_body
module Static_part = Flambda_static.Static_part

type 'a or_invalid =
  | Ok of 'a
  | Invalid

let simplify_static_part env (static_part : Static_part.t) : _ or_invalid =
  let type_of_name = E.type_of_name env in
  let simplify_float_fields (mut : Flambda_primitive.mutable_or_immutable)
        fields =
    let or_unknown field =
      match mut with
      | Immutable -> Some field
      | Mutable -> None
    in
    let done_something, fields_rev =
      List.fold_left
        (fun (done_something, fields_rev) (field : _ Static_part.or_variable) ->
          match field with
          | Const f -> done_something, ((field, or_unknown f) :: fields_rev)
          | Var var ->
            let ty = E.find_variable env var in
            begin match T.prove_naked_float ~type_of_name ty with
            | Unknown ->
              done_something, ((field, None) :: fields_rev)
            | Proved fs ->
              begin match Numbers.Float_by_bit_pattern.Set.get_singleton fs with
              | None ->
                done_something, ((field, None) :: fields_rev)
              | Some f ->
                true, ((Static_part.Const f, or_unknown f) :: fields_rev)
              end
            | Invalid ->
              (* CR mshinwell: fill in this case *)
              Misc.fatal_error "Not yet implemented"
            end)
        (false, [])
        fields
    in
    let static_part_fields, fields = List.split (List.rev fields_rev) in
    done_something, static_part_fields, fields
  in
  match static_part with
  | Block (tag, mut, fields) ->
    let or_unknown scanning ty =
      match mut with
      | Immutable -> ty
      | Mutable -> T.any_value scanning
    in
    let fields_and_types =
      List.map (fun (field : Of_kind_value.t) ->
          match field with
          | Symbol sym ->
            let ty = E.find_symbol env sym in
            field, or_unknown Definitely_immediate ty
          | Tagged_immediate imm ->
            field, or_unknown Definitely_immediate (T.this_tagged_immediate imm)
          | Dynamically_computed var ->
            let ty = E.find_variable env var in
            let ty, canonical_name =
              T.resolve_aliases ~type_of_name ty
            in
            let canonical_var =
              match canonical_name with
              | Some (Var var) -> var
              | (Some (Symbol _)) | None -> var
            in
            begin match canonical_name with
            | Some (Symbol sym) ->
              Of_kind_value.Symbol sym, or_unknown Definitely_immediate ty
            | (Some (Var _)) | None ->
              match T.prove_tagged_immediate ~type_of_name ty with
              | Proved imms ->
                begin match Immediate.Set.get_singleton imms with
                | None ->
                  Of_kind_value.Dynamically_computed canonical_var,
                    or_unknown Unknown ty
                | Some imm ->
                  Of_kind_value.Tagged_immediate imm,
                    or_unknown Definitely_immediate
                      (T.this_tagged_immediate imm)
                end
              | Unknown | Invalid ->
                (* Note that [Invalid] does not propagate here: all it means
                   is that this is something of kind [Value], but not a
                   tagged immediate.  Such things are still legal in this
                   context. *)
                Of_kind_value.Dynamically_computed var, or_unknown Unknown ty
            end)
        fields
    in
    let fields, field_types = List.split fields_and_types in
    assert (match mut with
      | Immutable -> true
      | Mutable ->
        List.for_all (fun ty -> not (T.is_known ~type_of_name ty))
          field_types);
    let field_types =
      List.map (fun field_type : _ T.mutable_or_immutable ->
          Immutable (T.force_to_kind_value field_type))
        field_types
    in
    let ty = T.block_of_values tag ~fields:(Array.of_list field_types) in
    Ok (Static_part.Block (tag, mut, fields), ty)
  | Fabricated_block field ->
    let field_ty : _ T.mutable_or_immutable =
      Immutable (E.find_variable env field)
    in
    let ty = T.block Tag.Scannable.zero ~fields:[| field_ty |] in
    Ok (Static_part.Fabricated_block field, ty)
  | Set_of_closures set ->
    let r = R.create () in
    let set, ty, _r = Simplify_named.simplify_set_of_closures env r set in
    Ok (Static_part.Set_of_closures set, ty)
  | Closure _ -> assert false (* XXX to do with Pierre (sym, closure_id) ->
    let ty = E.find_symbol env sym in
    begin match T.prove_sets_of_closures ~type_of_name ty with
    | Proved (Exactly sets) ->
      let closure_ty =
        T.Joined_sets_of_closures.type_for_closure_id sets closure_id
      in
      Ok (static_part, closure_ty)
    | Proved Not_all_values_known ->
      Ok (static_part, T.any_value Definitely_pointer)
    | Invalid -> Invalid
    end*)
  | Boxed_float (Const f) -> Ok (static_part, T.this_boxed_float f)
  | Mutable_string { initial_value = Const str; } ->
    let size = Targetint.OCaml.of_int (String.length str) in
    Ok (static_part, T.mutable_string ~size)
  | Immutable_string (Const str) ->
    let ty = T.this_immutable_string str in
    Ok (static_part, ty)
  | Boxed_float (Var var) ->
    (* CR mshinwell: Share code between these float/int32/int64/nativeint cases.
       [Number_adjuncts] may help *)
    let ty = E.find_variable env var in
    begin match T.prove_boxed_float ~type_of_name ty with
    | Proved ty_naked_float ->
      let ty = T.of_ty_naked_number ty_naked_float Naked_float in
      begin match T.prove_naked_float ~type_of_name ty with
      | Proved fs ->
        begin match Numbers.Float_by_bit_pattern.Set.get_singleton fs with
        | Some f ->
          Ok (Static_part.Boxed_float (Const f), T.this_boxed_float f)
        | None ->
          Ok (static_part, T.any_boxed_float ())
        end
      | Unknown -> Ok (static_part, T.any_boxed_float ())
      | Invalid -> Invalid
      end
    | Unknown -> Ok (static_part, T.any_boxed_float ())
    | Invalid -> Invalid
    end
  | Boxed_int32 (Const n) -> Ok (static_part, T.this_boxed_int32 n)
  | Boxed_int32 (Var var) ->
    let ty = E.find_variable env var in
    begin match T.prove_boxed_int32 ~type_of_name ty with
    | Proved ty_naked_int32 ->
      let ty = T.of_ty_naked_number ty_naked_int32 Naked_int32 in
      begin match T.prove_naked_int32 ~type_of_name ty with
      | Proved fs ->
        begin match Numbers.Int32.Set.get_singleton fs with
        | Some f ->
          Ok (Static_part.Boxed_int32 (Const f), T.this_boxed_int32 f)
        | None ->
          Ok (static_part, T.any_boxed_int32 ())
        end
      | Unknown -> Ok (static_part, T.any_boxed_int32 ())
      | Invalid -> Invalid
      end
    | Unknown -> Ok (static_part, T.any_boxed_int32 ())
    | Invalid -> Invalid
    end
  | Boxed_int64 (Const n) -> Ok (static_part, T.this_boxed_int64 n)
  | Boxed_int64 (Var var) ->
    let ty = E.find_variable env var in
    begin match T.prove_boxed_int64 ~type_of_name ty with
    | Proved ty_naked_int64 ->
      let ty = T.of_ty_naked_number ty_naked_int64 Naked_int64 in
      begin match T.prove_naked_int64 ~type_of_name ty with
      | Proved fs ->
        begin match Numbers.Int64.Set.get_singleton fs with
        | Some f ->
          Ok (Static_part.Boxed_int64 (Const f), T.this_boxed_int64 f)
        | None ->
          Ok (static_part, T.any_boxed_int64 ())
        end
      | Unknown -> Ok (static_part, T.any_boxed_int64 ())
      | Invalid -> Invalid
      end
    | Unknown -> Ok (static_part, T.any_boxed_int64 ())
    | Invalid -> Invalid
    end
  | Boxed_nativeint (Const n) -> Ok (static_part, T.this_boxed_nativeint n)
  | Boxed_nativeint (Var var) ->
    let ty = E.find_variable env var in
    begin match T.prove_boxed_nativeint ~type_of_name ty with
    | Proved ty_naked_nativeint ->
      let ty = T.of_ty_naked_number ty_naked_nativeint Naked_nativeint in
      begin match T.prove_naked_nativeint ~type_of_name ty with
      | Proved fs ->
        begin match Targetint.Set.get_singleton fs with
        | Some f ->
          Ok (Static_part.Boxed_nativeint (Const f), T.this_boxed_nativeint f)
        | None ->
          Ok (static_part, T.any_boxed_nativeint ())
        end
      | Unknown -> Ok (static_part, T.any_boxed_nativeint ())
      | Invalid -> Invalid
      end
    | Unknown -> Ok (static_part, T.any_boxed_nativeint ())
    | Invalid -> Invalid
    end
  | Mutable_float_array { initial_value = fields; } ->
    let done_something, initial_value, fields =
      simplify_float_fields Mutable fields
    in
    let size = Targetint.OCaml.of_int (List.length fields) in
    let ty = T.mutable_float_array ~size in
    if not done_something then Ok (static_part, ty)
    else Ok (Static_part.Mutable_float_array { initial_value; }, ty)
  | Immutable_float_array fields ->
    let done_something, static_part_fields, fields =
      simplify_float_fields Immutable fields
    in
    let fields =
      List.map (fun field ->
          match field with
          | None -> T.any_naked_float_as_ty_naked_float ()
          | Some f -> T.this_naked_float_as_ty_naked_float f)
        fields
    in
    let ty = T.immutable_float_array (Array.of_list fields) in
    if not done_something then Ok (static_part, ty)
    else Ok (Static_part.Immutable_float_array static_part_fields, ty)
  | Mutable_string { initial_value = Var var; } ->
    let ty = E.find_variable env var in
    begin match T.prove_string ~type_of_name ty with
    | Proved strs ->
      begin match T.String_info.Set.get_singleton strs with
      | Some str ->
        let ty = T.mutable_string ~size:str.size in
        begin match str.contents with
        | Unknown_or_mutable -> Ok (static_part, ty)
        | Contents str ->
          Ok (Static_part.Mutable_string { initial_value = Const str; }, ty)
        end
      | None -> Ok (static_part, T.any_value Definitely_pointer)
      end
    | Unknown ->
      Ok (static_part, T.any_value Definitely_pointer)
    | Invalid -> Invalid
    end
  | Immutable_string (Var var) ->
    let ty = E.find_variable env var in
    begin match T.prove_string ~type_of_name ty with
    | Proved strs ->
      begin match T.String_info.Set.get_singleton strs with
      | Some str ->
        begin match str.contents with
        | Contents s ->
          let ty = T.this_immutable_string s in
          Ok (Static_part.Immutable_string (Const s), ty)
        | Unknown_or_mutable ->
          let ty = T.immutable_string ~size:str.size in
          Ok (static_part, ty)
        end
      | None -> Ok (static_part, T.any_value Definitely_pointer)
      end
    | Unknown ->
      Ok (static_part, T.any_value Definitely_pointer)
    | Invalid -> Invalid
    end

let simplify_static_structure initial_env (recursive : Flambda.recursive) str =
  let unreachable, env, str =
    List.fold_left
      (fun ((now_unreachable, env, str) as acc) (sym, kind, static_part) ->
        if now_unreachable then
          acc
        else
          match simplify_static_part initial_env static_part with
          | Ok (static_part, ty) ->
            let env =
              match recursive with
              | Non_recursive -> E.add_symbol env sym ty
              | Recursive -> E.redefine_symbol env sym ty
            in
            false, env, ((sym, kind, static_part) :: str)
          | Invalid ->
            true, env, str)
      (false, initial_env, [])
      str
  in
  unreachable, env, List.rev str

let initial_environment_for_recursive_symbols env
      (defn : Program_body.definition) =
  let env =
    List.fold_left (fun env (symbol, kind, _static_part) ->
        E.add_symbol env symbol (T.unknown kind))
      env defn.static_structure
  in
  let _unreachable, env, _str =
    simplify_static_structure env Recursive defn.static_structure
  in
  env

let simplify_define_symbol env (recursive : Flambda.recursive)
      (defn : Program_body.definition) =
  let env, computation, newly_imported_symbols, lifted_constants =
    match defn.computation with
    | None -> env, defn.computation, Symbol.Map.empty, Symbol.Map.empty
    | Some computation ->
      let arity =
        List.map (fun (_var, kind) -> kind) computation.computed_values
      in
      let name = computation.return_cont in
      let return_cont_approx =
        Continuation_approx.create_unknown ~name ~arity
      in
      let exn_cont_approx =
        Continuation_approx.create_unknown ~name:computation.exception_cont
          ~arity:[Flambda_kind.value Unknown]
      in
      let expr, r, continuation_uses, lifted_constants =
        let env = E.add_continuation env name return_cont_approx in
        let env =
          E.add_continuation env computation.exception_cont exn_cont_approx
        in
        let r = R.create () in
        let descr =
          let symbol_names =
            List.map (fun (sym, _, _) ->
                Format.asprintf "%a" Symbol.print sym)
              defn.static_structure
          in
          Printf.sprintf "Toplevel binding(s) of: %s"
            (String.concat "+" symbol_names)
        in
        (E.simplify_toplevel env) env r computation.expr
          ~continuation:name
          ~exn_continuation:computation.exception_cont
          ~descr
      in
      (* CR mshinwell: Add unboxing of the continuation here.  This will look
         like half of Unbox_returns (same analysis and the same thing to
         happen to [expr]; but instead of generating a function wrapper, we
         need to do something else here).  Note that the linearity check
         for Unbox_returns will enable us to handle mutable returned values
         too. *)
      let args_types, _typing_env =
        R.Continuation_uses.join_of_arg_types continuation_uses ~arity
          ~default_env:(E.get_typing_environment env)
      in
(*
Format.eprintf "Args for %a: %a\n%!"
  Continuation.print name
  (Format.pp_print_list ~pp_sep:Format.pp_print_space T.print) args_types;
*)
      assert (List.for_all2 (fun (_var, kind1) ty ->
          let kind2 = T.kind ~type_of_name:(E.type_of_name env) ty in
          Flambda_kind.compatible kind2 ~if_used_at:kind1)
        computation.computed_values args_types);
      let env =
        List.fold_left2 (fun env (var, _kind) ty ->
            E.add_variable env var ty)
          env
          computation.computed_values args_types
      in
      let computation =
        match expr with
        | Apply_cont (cont, None, []) ->
          assert (Continuation.equal cont computation.return_cont);
          None
        | _ ->
          Some ({
            expr;
            return_cont = computation.return_cont;
            exception_cont = computation.exception_cont;
            computed_values = computation.computed_values;
          } : Program_body.computation)
      in
      env, computation, R.newly_imported_symbols r, lifted_constants
  in
  let env =
    match recursive with
    | Non_recursive -> env
    | Recursive -> initial_environment_for_recursive_symbols env defn
  in
  let unreachable, env, static_structure =
    simplify_static_structure env recursive defn.static_structure
  in
  (* CR mshinwell: [unreachable] should also be set to [true] if
     [computation] is [Some (Invalid _)]. *)
  let computation, static_structure =
    (* CR-someday mshinwell: We could imagine propagating an "unreachable"
       (if that's what [invalid ()] turns into, rather than a trap) back to
       previous [Define_symbol]s. *)
    if not unreachable then
      computation, static_structure
    else
      match computation with
      | None ->
        let computation : Program_body.computation =
          { expr = Flambda.Expr.invalid ();
            return_cont = Continuation.create ();
            exception_cont = Continuation.create ();
            computed_values = [];
          }
        in
        Some computation, []
      | Some computation ->
        let params =
          List.map (fun (var, kind) ->
              let param = Parameter.wrap var in
              Flambda.Typed_parameter.create_from_kind param kind)
            computation.computed_values
        in
        let expr : Flambda.Expr.t =
          Let_cont {
            body = computation.expr;
            handlers = Non_recursive {
              name = computation.return_cont;
              handler = {
                params;
                stub = false;
                is_exn_handler = false;
                handler = Flambda.Expr.invalid ();
              };
            };
          }
        in
        let new_return_cont =
          (* This continuation will never be called. *)
          Continuation.create ()
        in
        let computation : Program_body.computation =
          { expr;
            return_cont = new_return_cont;
            (* CR mshinwell: Think more about exception continuations here *)
            exception_cont = computation.exception_cont;
            computed_values = [];
          }
        in
        Some computation, []
  in
  let definition : Program_body.definition =
    { static_structure;
      computation;
    }
  in
  definition, env, newly_imported_symbols, lifted_constants

let add_lifted_constants lifted_constants
      (definition : Flambda_static.Program_body.definition) =
  let static_structure =
    Symbol.Map.fold (fun symbol (_ty, constant) static_structure ->
        let kind = K.value Definitely_pointer in
        (symbol, kind, constant) :: static_structure)
      lifted_constants
      definition.static_structure
  in
  { definition with static_structure; }

let rec simplify_program_body env (body : Program_body.t)
      : Program_body.t * (K.t Symbol.Map.t) =
  match body with
  | Define_symbol (defn, body) ->
    let defn, env, newly_imported_symbols1, lifted_constants =
      simplify_define_symbol env Non_recursive defn
    in
    let body, newly_imported_symbols2 =
      simplify_program_body env body
    in
    let newly_imported_symbols =
      Symbol.Map.disjoint_union newly_imported_symbols1 newly_imported_symbols2
    in
    let defn = add_lifted_constants lifted_constants defn in
    Define_symbol (defn, body), newly_imported_symbols
  | Define_symbol_rec (defn, body) ->
    let defn, env, newly_imported_symbols1, lifted_constants =
      simplify_define_symbol env Recursive defn
    in
    let body, newly_imported_symbols2 =
      simplify_program_body env body
    in
    let newly_imported_symbols =
      Symbol.Map.disjoint_union newly_imported_symbols1 newly_imported_symbols2
    in
    let defn = add_lifted_constants lifted_constants defn in
    Define_symbol_rec (defn, body), newly_imported_symbols
  | Root _ -> body, Symbol.Map.empty

let simplify_program env (program : Program.t) =
  let backend = E.backend env in
  let module Backend = (val backend : Backend_intf.S) in
  let predef_exn_symbols =
    Symbol.Set.fold (fun symbol predef_exn_symbols ->
        Symbol.Map.add symbol (K.value Definitely_pointer) predef_exn_symbols)
      (Backend.all_predefined_exception_symbols ())
      Symbol.Map.empty
  in
  let env =
    Symbol.Map.fold (fun symbol kind env ->
        E.add_symbol env symbol (T.unknown kind))
      (Symbol.Map.disjoint_union program.imported_symbols predef_exn_symbols)
      env
  in
  let body, newly_imported_symbols = simplify_program_body env program.body in
  { program with body; }, newly_imported_symbols
