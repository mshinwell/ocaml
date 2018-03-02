(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017--2018 OCamlPro SAS                                    *)
(*   Copyright 2017--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module E = Simplify_env_and_result.Env
module T = Flambda_type

type unboxing_spec = {
  constant_ctors : Immediate.Set.t;
  block_sizes_by_tag : Targetint.OCaml.t Tag.Map.t;
}

module type Unboxing_spec = sig
  type t

  val create : T.unboxable_proof -> (t * unboxing_spec) option

  val unboxed_kind : Flambda_kind.t

  val get_field
     : boxed_value:Simple.t
    -> index:int
    -> Debuginfo.t
    -> Flambda.Named.t

  val box : Simple.t list -> Debuginfo.t -> Flambda.Named.t

  val refine_unboxee_ty
     : (t -> unboxee_ty:T.t -> all_fields:Name.t list -> T.t) T.type_accessor
end

module Unboxing_spec_variant : Unboxing_spec = struct
  type t = {
    is_int_param : Name.t;
    get_tag_param : Name.t;
    no_discriminant_needed : Tag.t option;
  }

  let create (proof : T.unboxable_proof) =
    match proof with
    | Variant variant ->
      let no_discriminant_needed =
        match Tag.Map.get_singleton variant.block_sizes_by_tag with
        | Some (tag, _) -> Some tag
        | None -> None
      in
      let t : t =
        { is_int_param = Name.create "is_int";
          get_tag_param = Name.create "get_tag";
          no_discriminant_needed;
        }
      in
      let unboxing_spec : unboxing_spec =
        { constant_ctors = variant.immediates;
          block_sizes_by_tag = variant.block_sizes_by_tag;
        }
      in
      Some (t, unboxing_spec)
    | _ -> None

  let unboxed_kind = K.value ()

  let get_field ~boxed_value ~index dbg : Flambda.Named.t =
    (* CR mshinwell: We should be able to do better than
       [Unknown], based on the type of the unboxee. *)
    Prim (Binary (Block_load (Value Unknown, Immutable), boxed_value, index),
      dbg)

  let box fields dbg : Flambda.Named.t =
    Prim (Variadic (Make_block (Value Unknown, Immutable), fields), dbg)

  let refine_unboxee_ty ~type_of_name t ~unboxee_ty ~all_fields =
    match t.no_discriminant_needed with
    | None ->
      let unboxee_discriminants =
        T.variant_whose_discriminants_are ~is_int:t.is_int_param
          ~get_tag:t.get_tag_param
      in
      T.join ~type_of_name unboxee_ty unboxee_discriminants
    | Some unique_tag ->
      let fields =
        List.map (fun field -> T.alias_type_of unboxed_kind field) all_fields
      in
      T.block unique_tag ~fields:(Immutable (Array.of_list fields))
end

module Unboxing_spec_naked_number (N : sig
  include Number_adjuncts.Boxable_number_kind

  val ty_naked_number_of_proof : T.unboxable_proof -> t T.ty_naked_number option
end) = struct
  type t = {
    contents : N.t T.ty_naked_number;
  }

  let create (proof : T.unboxable_proof) =
    match S.ty_naked_number_of_proof proof with
    | None -> None
    | Some ty_naked_number ->
      let t : t =
        { contents = ty_naked_number;
        }
      in
      let block_sizes_by_tag =
        (* The choice of [Tag.custom_tag] is correct in some sense, but
           arbitrary: since there is only one possible tag, no discriminant will
           be generated in any case. *)
        Tag.Map.singleton Tag.custom_tag Targetint.OCaml.one
      in
      let unboxing_spec : unboxing_spec =
        { constant_ctors = Immediate.Set.empty;
          block_sizes_by_tag;
        }
      in
      Some (t, unboxing_spec)

  let unboxed_kind = Flambda_kind.Standard_int_or_float.to_kind N.kind

  let get_field ~boxed_value ~index dbg : Flambda.Named.t =
    if index <> 0 then begin
      Misc.fatal_errorf "Bad field index %d for [get_field]" index
    end;
    Prim (Unary (Unbox_number N.boxable_number_kind, boxed_value), dbg)

  let box fields dbg : Flambda.Named.t =
    match fields with
    | [naked_number] ->
      Prim (Unary (Box_number N.boxable_number_kind, naked_number), dbg)
    | _ ->
      Misc.fatal_errorf "Bad number of fields for [box]: %d"
        (List.length fields)

  let refine_unboxee_ty ~type_of_name:_ t ~unboxee_ty:_ ~all_fields =
    match all_fields with
    | [naked_number] ->
      N.box (T.alias_type_of unboxed_kind naked_number)
    | _ ->
      Misc.fatal_errorf "Bad number of fields for [refine_unboxee_ty]: %d"
        (List.length fields)
end

module Unboxing_spec_float = Unboxing_spec_naked_number (struct
  include Number_adjuncts.For_floats

  let ty_naked_number_of_proof (proof : T.unboxable_proof) =
    match proof with
    | Boxed_float ty_naked_number -> Some ty_naked_number
    | _ -> None
end)

module Unboxing_spec_int32 = Unboxing_spec_naked_number (struct
  include Number_adjuncts.For_int32s

  let ty_naked_number_of_proof (proof : T.unboxable_proof) =
    match proof with
    | Boxed_int32 ty_naked_number -> Some ty_naked_number
    | _ -> None
end)

module Unboxing_spec_int64 = Unboxing_spec_naked_number (struct
  include Number_adjuncts.For_int64s

  let ty_naked_number_of_proof (proof : T.unboxable_proof) =
    match proof with
    | Boxed_int64 ty_naked_number -> Some ty_naked_number
    | _ -> None
end)

module Unboxing_spec_nativeint = Unboxing_spec_naked_number (struct
  include Number_adjuncts.For_nativeints

  let ty_naked_number_of_proof (proof : T.unboxable_proof) =
    match proof with
    | Boxed_nativeint ty_naked_number -> Some ty_naked_number
    | _ -> None
end)

module How_to_unbox = struct
  type t = {
    unboxee_to_wrapper_params_unboxee : Variable.t Variable.Map.t;
    add_bindings_in_wrapper : Flambda.Expr.t -> Flambda.Expr.t;
    new_arguments_for_call_in_wrapper : Variable.t list;
    new_params : Flambda.Typed_parameter.t list;
    new_unboxee_types : (Variable.t * Flambda_type.t) list;
    build_boxed_value_from_new_params :
      (Flambda.Typed_parameter.t * (Flambda.Expr.t -> Flambda.Expr.t)) list;
  }

  let create () =
    { unboxee_to_wrapper_params_unboxee = Variable.Map.empty;
      add_bindings_in_wrapper = (fun expr -> expr);
      new_arguments_for_call_in_wrapper = [];
      new_params = [];
      new_unboxee_types = [];
      build_boxed_value_from_new_params = [];
    }

  let merge t1 t2 =
    { unboxee_to_wrapper_params_unboxee =
        Variable.Map.union (fun _ param1 param2 ->
            assert (Variable.equal param1 param2);
            Some param1)
          t1.unboxee_to_wrapper_params_unboxee
          t2.unboxee_to_wrapper_params_unboxee;
      add_bindings_in_wrapper = (fun expr ->
        t2.add_bindings_in_wrapper (
          t1.add_bindings_in_wrapper expr));
      new_arguments_for_call_in_wrapper =
        t1.new_arguments_for_call_in_wrapper
          @ t2.new_arguments_for_call_in_wrapper;
      new_params = t1.new_params @ t2.new_params;
      new_unboxee_types = t1.new_unboxee_types @ t2.new_unboxee_types;
      build_boxed_value_from_new_params =
        t1.build_boxed_value_from_new_params
          @ t2.build_boxed_value_from_new_params;
    }

  let merge_variable_map t_map =
    Variable.Map.fold (fun _param t1 t2 -> merge t1 t2) t_map (create ())
end

let how_to_unbox_core ~type_of_name ~env ~unboxee ~unboxee_ty
      ~unboxing_spec_user_data ~unboxing_spec ~is_unbox_returns
      : How_to_unbox.t =
  let dbg = Debuginfo.none in
  let num_constant_ctors = Numbers.Int.Set.cardinal constant_ctors in
  assert (num_constant_ctors >= 0);
  (* CR mshinwell: We need to think about this more.
     Suppose we have code that deconstructs an "int option".  That code uses
     Pisint.  However we know that the thing is only ever going to be
     "Some x" and try to elide the "_is_int" parameter.  However that means
     we don't know that Pisint foo_option = false.  For the moment we don't
     elide the "_is_int".  Note that for Unbox_continuation_params the
     extra argument isn't really a problem---it will be removed---but for
     is_unbox_returns we really don't want to generate an extra return value
     if it isn't needed.
     Follow-up: think this might be ok for is_unbox_returns only, since we don't
     need the Pisint = false judgements etc.
  *)
  let num_tags = Tag.Map.cardinal blocks in
  assert (num_tags >= 1);  (* see below *)
  let wrapper_param_unboxee = Variable.rename unboxee in
  let unboxee_to_wrapper_params_unboxee =
    Variable.Map.add unboxee wrapper_param_unboxee
      Variable.Map.empty
  in
  let max_size =
    Tag.Map.fold (fun _tag fields max_size ->
        max (Array.length fields) max_size)
      blocks 0
  in
  let field_arguments_for_call_in_wrapper =
    Array.to_list (Array.init max_size (fun index ->
      Variable.create (Printf.sprintf "field%d" index)))
  in
  let is_int = Variable.rename ~append:"_is_int" unboxee in
  let is_int_in_wrapper = Variable.rename is_int in
  let is_int_known_value =
    if no_constant_ctors then Some (Simple Simple.zero : Flambda.Named.t)
    else None
  in
  (* CR-soon mshinwell: On [discriminant] add information that tells us
     about the individual unboxed field parameters _given that_ we are
     in some particular case of a match on [discriminant] (GADT-style). *)
  let discriminant = Variable.rename ~append:"_discr" unboxee in
  let discriminant_in_wrapper = Variable.rename discriminant in
  let discriminant_known_value =
    let discriminant_possible_values =
      let all_tags =
        Tag.Map.fold (fun tag _ all_tags ->
            Numbers.Int.Set.add (Tag.to_int tag) all_tags)
          blocks
          Numbers.Int.Set.empty
      in
      Numbers.Int.Set.union constant_ctors all_tags
    in
    match Numbers.Int.Set.elements discriminant_possible_values with
    | [] -> assert false  (* see the bottom of [how_to_unbox], below *)
    | [tag] -> Some (Simple (Simple.const_int tag) : Flambda.Named.t)
    | _tags -> None
  in
  let needs_discriminant =
    match discriminant_known_value with
    | None -> true
    | Some _ -> false
  in
  let is_int_in_wrapper' = Variable.rename is_int_in_wrapper in
  let discriminant_in_wrapper' = Variable.rename discriminant_in_wrapper in
  let new_arguments_for_call_in_wrapper =
    let is_int =
      if no_constant_ctors then [] else [is_int_in_wrapper']
    in
    let discriminant =
      if not needs_discriminant then [] else [discriminant_in_wrapper']
    in
    is_int @ discriminant @ field_arguments_for_call_in_wrapper
  in
  let new_arguments_for_call_in_wrapper_with_types =
    (* CR mshinwell: should be able to do better for the type here *)
    List.map (fun arg -> Parameter.wrap arg, T.any_value ())
      new_arguments_for_call_in_wrapper
  in
  let tags_to_sizes = Tag.Map.map (fun fields -> Array.length fields) blocks in
  let all_tags = Tag.Map.keys blocks in
  let sizes_to_filler_conts =
    List.fold_left (fun sizes_to_filler_conts size ->
        Numbers.Int.Map.add size (Continuation.create ()) sizes_to_filler_conts)
      Numbers.Int.Map.empty
      (Tag.Map.data tags_to_sizes)
  in
  let tags_to_sizes_and_boxing_conts =
    Tag.Map.map (fun size -> size, Continuation.create ()) tags_to_sizes
  in
  let all_units = Array.to_list (Array.init max_size (fun _ -> Simple.unit)) in
  let add_bindings_in_wrapper expr =
    let is_int_cont = Continuation.create () in
    let is_block_cont = Continuation.create () in
    let join_cont = Continuation.create () in
    let tag = Variable.create "tag" in
    let is_int_switch =
      let arms =
        Targetint.OCaml.Map.of_list [
          Targetint.OCaml.zero, is_block_cont;
          Targetint.OCaml.one, is_int_cont;
        ]
      in
      Flambda.Expr.create_int_switch ~scrutinee:is_int_in_wrapper ~arms
    in
    let add_fill_fields_conts expr =
      Numbers.Int.Map.fold (fun size filler_cont expr : Flambda.Expr.t ->
          let fields =
            Array.init max_size (fun index ->
              if index < size then
                let name = Printf.sprintf "_field%d" index in
                index, Some (Variable.rename ~append:name unboxee)
              else
                index, None)
          in
          let fields_for_apply =
            List.map (fun (_index, var_opt) ->
                match var_opt with
                | None -> Simple.const_unit
                | Some var -> Simple.var var)
              (Array.to_list fields)
          in
          let filler : Flambda.Expr.t =
            let filler : Flambda.Expr.t =
              let is_int_in_wrapper =
                if no_constant_ctors then [] else [is_int_in_wrapper]
              in
              let tag = if not needs_discriminant then [] else [tag] in
              Apply_cont (join_cont, None,
                is_int_in_wrapper @ tag @ fields_for_apply)
            in
            Array.fold_right (fun (index, var_opt) filler ->
                match var_opt with
                | None -> filler
                | Some var ->
                  Flambda.Expr.create_let var
                    (S.get_field ~boxed_value:wrapper_param_unboxee ~index dbg)
                    filler)
              fields
              filler
          in
          Let_cont {
            body = expr;
            handlers = Non_recursive {
              name = filler_cont;
              handler = {
                params = [];
                (* CR mshinwell: All of the "stub" settings in this file are
                   "true" so we don't try to unbox their arguments over and
                   over.  Maybe instead we should have a "kind" field which
                   could include the stub, is_exn_handler, etc data plus
                   something saying not to unbox *)
                stub = true;
                is_exn_handler = false;
                handler = filler;
              };
            }
          })
        sizes_to_filler_conts
        expr      
    in
    let fill_fields_switch =
      let all_possible_values =
        Tag.Set.fold (fun tag all_possible_values ->
            Numbers.Int.Set.add (Tag.to_int tag) all_possible_values)
          all_tags
          Numbers.Int.Set.empty
      in
      let arms =
        Numbers.Int.Map.fold (fun size filler_cont consts ->
            Tag.Map.fold (fun tag fields consts ->
                if Array.length fields = size then
                  (Tag.to_int tag, filler_cont) :: consts
                else
                  consts)
              blocks
              consts)
          sizes_to_filler_conts
          []
      in
      Flambda.Expr.create_switch ~scrutinee:tag
        ~all_possible_values
        ~arms
        ~default:None
    in
    Flambda.Expr.create_let is_int_in_wrapper
      (if no_constant_ctors then Simple Simple.const_zero
       else Prim (Unary (Is_int, wrapper_param_unboxee), dbg))
      (Let_cont {
        body = Let_cont {
          body = Let_cont {
            body = is_int_switch;
            handlers = Non_recursive {
              name = is_int_cont;
              handler = {
                params = [];
                handler =
                  (let is_int_in_wrapper =
                    if no_constant_ctors then [] else [is_int_in_wrapper]
                  in
                  let wrapper_param_unboxee =
                    if not needs_discriminant then []
                    else [wrapper_param_unboxee]
                  in
                  Apply_cont (join_cont, None,
                    is_int_in_wrapper @ wrapper_param_unboxee
                      @ all_units));
                stub = true;
                is_exn_handler = false;
              };
            };
          };
          handlers = Non_recursive {
            name = is_block_cont;
            handler = {
              params = [];
              handler =
                Flambda.Expr.create_let tag
                  (match discriminant_known_value with
                   | Some known -> known
                   | None ->
                     Prim (Unary (Get_tag, wrapper_param_unboxee), dbg))
                  (add_fill_fields_conts fill_fields_switch);
              stub = true;
              is_exn_handler = false;
            };
          };
        };
        handlers = Non_recursive {
          name = join_cont;
          handler = {
            params = Flambda.Typed_parameter.List.create
              new_arguments_for_call_in_wrapper_with_types;
            handler = expr;
            stub = true;
            is_exn_handler = false;
          };
        }
      })
  in
  let fields_with_kinds0 =
    Array.init max_size (fun index ->
      let append = string_of_int index in
      let var = Variable.rename ~append unboxee in
      var, field_kind)
  in
  let fields_with_kinds = Array.to_list fields_with_kinds0 in
  (* CR mshinwell: This next section is only needed for [is_unbox_returns] at
     present; we shouldn't run it unless required. *)
  let boxing_is_int_cont = Continuation.create () in
  let boxing_is_block_cont = Continuation.create () in
  let boxing_is_int_switch =
    let arms =
      Targetint.OCaml.Map.of_list [
        Targetint.OCaml.zero, boxing_is_block_cont;
        Targetint.OCaml.one, boxing_is_int_cont;
      ]
    in
    Flambda.Expr.create_int_switch ~scrutinee:is_int ~arms
  in
  let boxing_switch =
    let arms =
      Tag.Map.map (fun (_size, boxing_cont) -> boxing_cont)
        tags_to_sizes_and_boxing_conts
    in
    Flambda.Expr.create_tag_switch ~scrutinee:discriminant ~arms
  in
  let build_boxed_value_from_new_params =
    let boxed = Variable.rename ~append:"_boxed" unboxee in
    let boxed_param = Parameter.wrap boxed in
    (* CR mshinwell: Should be able to do better for [boxed_ty] *)
    let boxed_ty = T.any_value () in
    let join_cont = Continuation.create () in
    let build (expr : Flambda.Expr.t) : Flambda.Expr.t =
      let arms =
        Numbers.Int.Set.fold (fun ctor_index arms ->
            let tag = Tag.create_exn ctor_index in
            let cont = Continuation.create () in
            Tag.Map.add tag cont arms)
          constant_ctors
          Tag.Map.empty
      in
      let constant_ctor_switch =
        Flambda.Expr.create_int_switch ~scrutinee:discriminant ~arms
      in
      let add_constant_ctor_conts expr =
        List.fold_left (fun expr (ctor_index, cont) ->
            let ctor_index_var = Variable.create "ctor_index" in
            Flambda.Expr.create_let ctor_index_var
              (Simple (Simple.int ctor_index))
              (Let_cont {
                body = expr;
                handlers = Non_recursive {
                  name = cont;
                  handler = {
                    handler = Apply_cont (
                      join_cont, None, [Simple.var ctor_index_var]);
                    params = [];
                    stub = true;
                    is_exn_handler = false;
                  };
                };
              }))
          expr
          consts
      in
      let add_boxing_conts expr =
        Tag.Map.fold (fun tag (size, boxing_cont) expr : Flambda.Expr.t ->
            let boxed = Variable.rename boxed in
            let fields =
              let fields, _index =
                List.fold_left (fun (fields, index) field ->
                    if index >= size then fields, index + 1
                    else (field :: fields), index + 1)
                  ([], 0)
                  fields_with_kinds
              in
              List.rev fields
            in
            let handler : Flambda.Expr.t =
              Flambda.Expr.create_let boxed (S.box fields dbg)
                (Flambda.Apply_cont (join_cont, None, [boxed]))
            in
            Let_cont {
              body = expr;
              handlers = Non_recursive {
                name = boxing_cont;
                handler = {
                  params = [];
                  handler;
                  stub = true;
                  is_exn_handler = false;
                };
              };
            })
          tags_to_sizes_and_boxing_conts
          expr
      in
      let body : Flambda.Expr.t =
        Let_cont {
          body = Let_cont {
            body = Let_cont {
              body = boxing_is_int_switch;
              handlers = Non_recursive {
                name = boxing_is_block_cont;
                handler = {
                  params = [];
                  handler = add_boxing_conts boxing_switch;
                  stub = true;
                  is_exn_handler = false;
                };
              };
            };
            handlers = Non_recursive {
              name = boxing_is_int_cont;
              handler = {
                params = [];
                (* We could just call [join_cont] with [discriminant] as the
                   argument, but that wouldn't pass on the knowledge to the
                   place in which this stub gets inlined that [discriminant]
                   is an integer. *)
                (* CR-someday mshinwell: Maybe adding some kind of support for
                   coercions would help here.  Perhaps another approach would be
                   to do CSE on "Pisint discriminant" (which would rewrite to
                   the "is_int" variable returned from the callee).  This would
                   require propagation of the projection information from the
                   stub function generated by is_unbox_returns to the place it's
                   being inlined. *)
                handler = add_constant_ctor_conts constant_ctor_switch;
                stub = true;
                is_exn_handler = false;
              };
            };
          };
          handlers = Non_recursive {
            name = join_cont;
            handler = {
              params = [Flambda.Typed_parameter.create boxed_param boxed_ty];
              handler = expr;
              stub = true;
              is_exn_handler = false;
            };
          };
        }
      in
      let body =
        match is_int_known_value with
        | None -> body
        | Some named -> Flambda.Expr.create_let is_int named body
      in
      match discriminant_known_value with
      | None -> body
      | Some named -> Flambda.Expr.create_let discriminant named body
    in
    [boxed, build]
  in
  let is_int =
    if no_constant_ctors then []
    else
      let is_int_ty =
        let by_constant_ctor_index =
          Targetint.OCaml.Set.fold (fun ctor_index by_constant_ctor_index ->
              let tag = Tag.of_targetint_ocaml ctor_index in
              let env = T.Typing_environment.create () in
              Tag.Map.add tag env by_constant_ctor_index)
            constant_ctors
            Tag.Map.empty
        in
        let by_tag =
          Tag.Map.map (fun size ->
              let env = ref (T.Typing_environment.create ()) in
              for field = 0 to size - 1 do
                let field, kind = fields_with_kinds0.(field) in
                let field_scope_level = E.continuation_scope_level env in
                (* CR mshinwell: We could refine the types of the actual fields
                   themselves according to the tag. *)
                let field_ty = T.unknown kind in
                env := T.Typing_environment.add !env field field_scope_level
                  field_ty
              done;
              !env)
            tags_to_sizes
        in
        let discriminant_env_is_int =
          T.Typing_environment.add discriminant
            (E.continuation_scope_level env)
            (T.these_tags by_constant_ctor_index)
        in
        let discriminant_env_is_block =
          T.Typing_environment.add discriminant
            (E.continuation_scope_level env)
            (T.these_tags by_tag)
        in
        let by_is_int_result =
          Immediate.Map.of_list [
            Immediate.const_true, discriminant_env_is_int;
            Immediate.const_false, discriminant_env_is_block;
          ]
        in
        T.these_tagged_immediates_with_envs by_is_int_result
      in
      let is_int = Parameter.wrap is_int in
      [Flambda.Typed_parameter.create is_int is_int_ty]
  in
  let discriminant =
    if not needs_discriminant then []
    else
      let discriminant = Parameter.wrap discriminant in
      [Flambda.Typed_parameter.create discriminant discriminant_ty]
  in
  let fields =
    List.map (fun (field, kind) ->
        let field = Parameter.wrap field in
        Flambda.Typed_parameter.create field S.unboxed_kind)
      fields_with_kinds
  in
  let unboxee_ty =
    let all_fields =
      List.map (fun (field, _kind) -> field) fields_with_kinds
    in
    S.refine_unboxee_ty ~type_of_name unboxing_spec_user_data
      ~unboxee_ty ~all_fields
  in
  { unboxee_to_wrapper_params_unboxee;
    add_bindings_in_wrapper;
    new_arguments_for_call_in_wrapper;
    new_type_for_unboxees = [unboxee, unboxee_ty];
    new_params = is_int @ discriminant @ fields;
    new_unboxee_types = [unboxee, unboxee_ty];
    build_boxed_value_from_new_params;
  }

let how_to_unbox ~type_of_name ~env ~unboxee ~unboxee_ty ~is_unbox_returns =
  let unbox ~unboxing_spec_user_data ~unboxing_spec =
    Some (how_to_unbox_core ~type_of_name ~env ~unboxee ~unboxee_ty
      ~unboxing_spec_user_data ~unboxing_spec ~is_unbox_returns)
  in
  match T.prove_unboxable ~type_of_name ~unboxee_ty with
  | Cannot_unbox -> None
  | proof ->
    match Unboxing_spec_variant.create proof with
    | Some (unboxing_spec_user_data, unboxing_spec) ->
      unbox ~unboxing_spec_user_data ~unboxing_spec
    | None ->
      match Unboxing_spec_float.create proof with
      | Some (unboxing_spec_user_data, unboxing_spec) ->
        unbox ~unboxing_spec_user_data ~unboxing_spec
      | None ->
        match Unboxing_spec_int32.create proof with
        | Some (unboxing_spec_user_data, unboxing_spec) ->
          unbox ~unboxing_spec_user_data ~unboxing_spec
        | None ->
          match Unboxing_spec_int64.create proof with
          | Some (unboxing_spec_user_data, unboxing_spec) ->
            unbox ~unboxing_spec_user_data ~unboxing_spec
          | None ->
            match Unboxing_spec_nativeint.create proof with
            | Some (unboxing_spec_user_data, unboxing_spec) ->
              unbox ~unboxing_spec_user_data ~unboxing_spec
            | None -> None
