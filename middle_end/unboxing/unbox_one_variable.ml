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
module K = Flambda_kind
module T = Flambda_type

type unboxing_spec = {
  constant_ctors : Immediate.Set.t;
  block_sizes_by_tag : Targetint.OCaml.t Tag.Map.t;
}

module type Unboxing_spec = sig
  type t

  val create : T.unboxable_proof -> (t * unboxing_spec) option

  val unboxed_kind : K.t

  val get_field
     : boxed_value:Simple.t
    -> index:int
    -> Debuginfo.t
    -> Flambda.Named.t

  val box : t -> Tag.t -> Simple.t list -> Debuginfo.t -> Flambda.Named.t

  val refine_unboxee_ty
     : (t -> unboxee_ty:T.t -> all_fields:Name.t list -> T.t) T.type_accessor
end

module Unboxing_spec_variant : Unboxing_spec = struct
  type t = {
    is_int_param : Name.t;
    get_tag_param : Name.t;
    no_discriminant_needed : Tag.Scannable.t option;
  }

  let create (proof : T.unboxable_proof) =
    match proof with
    | Variant_or_block_of_values { block_sizes_by_tag; constant_ctors; } ->
      let no_discriminant_needed =
        match Tag.Scannable.Map.get_singleton block_sizes_by_tag with
        | Some (tag, _) -> Some tag
        | None -> None
      in
      let t : t =
        { is_int_param = Name.var (Variable.create "is_int");
          get_tag_param = Name.var (Variable.create "get_tag");
          no_discriminant_needed;
        }
      in
      let block_sizes_by_tag =
        Tag.Scannable.Map.fold (fun tag size ->
            Tag.Map.add (Tag.Scannable.to_tag tag) size)
          block_sizes_by_tag
          Tag.Map.empty
      in
      let unboxing_spec : unboxing_spec =
        { constant_ctors;
          block_sizes_by_tag;
        }
      in
      Some (t, unboxing_spec)
    | _ -> None

  let unboxed_kind = K.value ()

  let get_field ~boxed_value ~index dbg : Flambda.Named.t =
    (* CR mshinwell: We should be able to do better than
       [Unknown], based on the type of the unboxee. *)
    Prim (Binary (Block_load (Block (Value Unknown), Immutable),
        boxed_value, Simple.const_int index),
      dbg)

  let box _t tag fields dbg : Flambda.Named.t =
    let kinds =
      (* CR mshinwell: We should be able to do better than [Unknown] *)
      List.map (fun _field -> Flambda_primitive.Value_kind.Unknown) fields
    in
    match Tag.Scannable.of_tag tag with
    | Some tag ->
      Prim (Variadic (Make_block (Full_of_values (tag, kinds), Immutable),
        fields), dbg)
    | None -> assert false  (* See [create], above. *)

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
        List.map (fun field : T.t T.mutable_or_immutable ->
            Immutable (T.alias_type_of unboxed_kind field))
          all_fields
      in
      T.block (Tag.Scannable.to_tag unique_tag) ~fields:(Array.of_list fields)
end

module Unboxing_spec_float_array = struct
  type t = unit

  let create (proof : T.unboxable_proof) =
    match proof with
    | Float_array { length; } ->
      let block_sizes_by_tag =
        (* The choice of [Tag.double_array_tag] is correct in some sense, but
           arbitrary: since there is only one possible tag, no discriminant will
           be generated in any case. *)
        Tag.Map.singleton Tag.double_array_tag length
      in
      let unboxing_spec : unboxing_spec =
        { constant_ctors = Immediate.Set.empty;
          block_sizes_by_tag;
        }
      in
      Some ((), unboxing_spec)
    | _ -> None

  let unboxed_kind = K.naked_float ()

  let get_field ~boxed_value ~index dbg : Flambda.Named.t =
    Prim (Binary (Block_load (Block Naked_float, Immutable),
        boxed_value, Simple.const_int index),
      dbg)

  let box _t _tag fields dbg : Flambda.Named.t =
    Prim (Variadic (Make_block (Full_of_naked_floats, Immutable), fields), dbg)

  let refine_unboxee_ty ~type_of_name:_ _t ~unboxee_ty:_ ~all_fields =
    let fields =
      List.map (fun field : T.t T.mutable_or_immutable ->
          Immutable (T.alias_type_of (K.naked_float ()) field))
        all_fields
    in
    T.block Tag.double_array_tag ~fields:(Array.of_list fields)
end

module Unboxing_spec_naked_number (N : sig
  include Number_adjuncts.Boxable_number_kind

  val check_proof : T.unboxable_proof -> bool
end) = struct
  type t = unit

  let create (proof : T.unboxable_proof) =
    if not (N.check_proof proof) then None
    else
      let block_sizes_by_tag =
        (* [Tag.custom_tag] is arbitrary, as above. *)
        Tag.Map.singleton Tag.custom_tag Targetint.OCaml.one
      in
      let unboxing_spec : unboxing_spec =
        { constant_ctors = Immediate.Set.empty;
          block_sizes_by_tag;
        }
      in
      Some ((), unboxing_spec)

  let unboxed_kind = Flambda_kind.Standard_int_or_float.to_kind N.kind

  let get_field ~boxed_value ~index dbg : Flambda.Named.t =
    if index <> 0 then begin
      Misc.fatal_errorf "Bad field index %d for [get_field]" index
    end;
    Prim (Unary (Unbox_number N.boxable_number_kind, boxed_value), dbg)

  let box _t _tag fields dbg : Flambda.Named.t =
    match fields with
    | [naked_number] ->
      Prim (Unary (Box_number N.boxable_number_kind, naked_number), dbg)
    | _ ->
      Misc.fatal_errorf "Bad number of fields for [box]: %d"
        (List.length fields)

  let refine_unboxee_ty ~type_of_name:_ _t ~unboxee_ty:_ ~all_fields =
    match all_fields with
    | [naked_number] ->
      N.box (T.alias_type_of unboxed_kind naked_number)
    | _ ->
      Misc.fatal_errorf "Bad number of fields for [refine_unboxee_ty]: %d"
        (List.length all_fields)
end

module Unboxing_spec_boxed_float = Unboxing_spec_naked_number (struct
  include Number_adjuncts.For_floats

  let check_proof (proof : T.unboxable_proof) =
    match proof with
    | Boxed_float -> true
    | _ -> false
end)

module Unboxing_spec_boxed_int32 = Unboxing_spec_naked_number (struct
  include Number_adjuncts.For_int32s

  let check_proof (proof : T.unboxable_proof) =
    match proof with
    | Boxed_int32 -> true
    | _ -> false
end)

module Unboxing_spec_boxed_int64 = Unboxing_spec_naked_number (struct
  include Number_adjuncts.For_int64s

  let check_proof (proof : T.unboxable_proof) =
    match proof with
    | Boxed_int64 -> true
    | _ -> false
end)

module Unboxing_spec_boxed_nativeint = Unboxing_spec_naked_number (struct
  include Number_adjuncts.For_nativeints

  let check_proof (proof : T.unboxable_proof) =
    match proof with
    | Boxed_nativeint -> true
    | _ -> false
end)

module How_to_unbox = struct
  type t = {
    unboxee_to_wrapper_params_unboxee :
      Flambda.Typed_parameter.t Parameter.Map.t;
    add_bindings_in_wrapper : Flambda.Expr.t -> Flambda.Expr.t;
    new_arguments_for_call_in_wrapper : Variable.t list;
    new_params : Flambda.Typed_parameter.t list;
    new_unboxee_types : (Variable.t * Flambda_type.t) list;
    build_boxed_value_from_new_params :
      (Flambda.Typed_parameter.t * (Flambda.Expr.t -> Flambda.Expr.t)) list;
  }

  let create () =
    { unboxee_to_wrapper_params_unboxee = Parameter.Map.empty;
      add_bindings_in_wrapper = (fun expr -> expr);
      new_arguments_for_call_in_wrapper = [];
      new_params = [];
      new_unboxee_types = [];
      build_boxed_value_from_new_params = [];
    }

  let merge t1 t2 =
    { unboxee_to_wrapper_params_unboxee =
        Parameter.Map.union (fun _ param1 param2 ->
            let module TP = Flambda.Typed_parameter in
            assert (Parameter.equal (TP.param param1) (TP.param param2));
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

module Make (S : Unboxing_spec) = struct
  let unbox ~type_of_name ~env ~unboxee ~unboxee_ty
        ~unboxing_spec_user_data ~unboxing_spec ~is_unbox_returns:_
        : How_to_unbox.t =
    let dbg = Debuginfo.none in
    let constant_ctors = unboxing_spec.constant_ctors in
    let blocks = unboxing_spec.block_sizes_by_tag in
    let num_constant_ctors = Immediate.Set.cardinal constant_ctors in
    assert (num_constant_ctors >= 0);
    let no_constant_ctors = (num_constant_ctors = 0) in
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
      let wrapper_param_unboxee =
        Flambda.Typed_parameter.create (Parameter.wrap wrapper_param_unboxee)
          unboxee_ty
      in
      Parameter.Map.add (Parameter.wrap unboxee) wrapper_param_unboxee
        Parameter.Map.empty
    in
    let max_size =
      Tag.Map.fold (fun _tag size max_size -> Targetint.OCaml.max size max_size)
        blocks
        Targetint.OCaml.zero
    in
    let field_arguments_for_call_in_wrapper =
      Array.to_list (Array.init (Targetint.OCaml.to_int max_size) (fun index ->
        Variable.create (Printf.sprintf "field%d" index)))
    in
    let is_int = Variable.rename ~append:"_is_int" unboxee in
    let is_int_in_wrapper = Variable.rename is_int in
    let is_int_known_value, is_int_ty =
      if no_constant_ctors then
        Some (Simple Simple.const_zero : Flambda.Named.t),
          T.this_tagged_immediate Immediate.bool_false
      else
        None, T.any_tagged_bool ()
    in
    (* CR-soon mshinwell: On [discriminant] add information that tells us
       about the individual unboxed field parameters _given that_ we are
       in some particular case of a match on [discriminant] (GADT-style). *)
    let discriminant = Variable.rename ~append:"_discr" unboxee in
    let discriminant_in_wrapper = Variable.rename discriminant in
    let discriminant_known_value, discriminant_ty =
      let discriminant_possible_values, discriminant_ty =
        let all_tags =
          Tag.Map.fold (fun tag _ all_tags ->
              Immediate.Set.add (Immediate.int (Targetint.OCaml.of_int (
                Tag.to_int tag))) all_tags)
            blocks
            Immediate.Set.empty
        in
        let by_tag =
          Immediate.Set.fold (fun ctor_index by_tag ->
              let tag =
                (* CR mshinwell: too verbose *)
                Tag.create_exn (
                  Targetint.OCaml.to_int (Immediate.to_targetint ctor_index))
              in
              Tag.Map.add tag (T.Typing_environment.create ()) by_tag)
            constant_ctors
            Tag.Map.empty
        in
        let by_tag =
          Tag.Map.fold (fun tag _ by_tag ->
              Tag.Map.add tag (T.Typing_environment.create ()) by_tag)
            blocks
            by_tag
        in
        Immediate.Set.union constant_ctors all_tags,
          T.these_tags by_tag
      in
      match Immediate.Set.elements discriminant_possible_values with
      | [] -> assert false  (* see the bottom of [how_to_unbox], below *)
      | [tag] ->
        Some (Simple (Simple.const (Tagged_immediate tag)) : Flambda.Named.t),
          discriminant_ty
      | _tags -> None, discriminant_ty
    in
    let needs_discriminant =
      match discriminant_known_value with
      | None -> true
      | Some _ -> false
    in
    (* CR mshinwell: Form discriminant type using the known value *)
    let is_int_in_wrapper' = Variable.rename is_int_in_wrapper in
    let discriminant_in_wrapper' = Variable.rename discriminant_in_wrapper in
    let new_arguments_for_call_in_wrapper,
        new_arguments_for_call_in_wrapper_with_types =
      let is_int =
        if no_constant_ctors then [] else [is_int_in_wrapper']
      in
      let discriminant =
        if not needs_discriminant then [] else [discriminant_in_wrapper']
      in
      let without_types =
        is_int @ discriminant @ field_arguments_for_call_in_wrapper
      in
      let with_types =
        (* XXX These should just get the same types as the is_int/discr.
           in the main handler. *)
        let is_int =
          (* CR mshinwell: these "map"s are gross, remove *)
          List.map (fun is_int -> Parameter.wrap is_int, is_int_ty)
            is_int
        in
        let discriminant =
          List.map (fun discriminant ->
              Parameter.wrap discriminant, discriminant_ty)
            discriminant
        in
        let args =
          (* CR mshinwell: should be able to do better for the type here *)
          List.map (fun arg -> Parameter.wrap arg, T.unknown S.unboxed_kind)
            field_arguments_for_call_in_wrapper
        in
        is_int @ discriminant @ args
      in
      without_types, with_types
    in
    let tags_to_sizes = blocks in (* CR mshinwell: remove alias *)
    let sizes_to_filler_conts =
      List.fold_left (fun sizes_to_filler_conts size ->
          Immediate.Map.add (Immediate.int size) (Continuation.create ())
            sizes_to_filler_conts)
        Immediate.Map.empty
        (Tag.Map.data tags_to_sizes)
    in
    let tags_to_sizes_and_boxing_conts =
      Tag.Map.map (fun size -> size, Continuation.create ()) tags_to_sizes
    in
    let all_units =
      Array.to_list (Array.init (Targetint.OCaml.to_int_exn max_size)
        (fun _ -> Simple.unit))
    in
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
        Flambda.Expr.create_int_switch ~scrutinee:(Name.var is_int_in_wrapper)
          ~arms
      in
      let add_fill_fields_conts expr =
        Immediate.Map.fold (fun size filler_cont expr : Flambda.Expr.t ->
            let fields =
              let size =
                Targetint.OCaml.to_int_exn (Immediate.to_targetint size)
              in
              Array.init (Targetint.OCaml.to_int_exn max_size) (fun index ->
                if index < size then
                  let name = Printf.sprintf "_field%d" index in
                  index, Some (Variable.rename ~append:name unboxee)
                else
                  index, None)
            in
            let fields_for_apply =
              List.map (fun (_index, var_opt) ->
                  match var_opt with
                  | None ->
                    assert (K.equal S.unboxed_kind (K.value ()));
                    Simple.unit
                  | Some var -> Simple.var var)
                (Array.to_list fields)
            in
            let filler : Flambda.Expr.t =
              let filler : Flambda.Expr.t =
                let is_int_in_wrapper =
                  if no_constant_ctors then []
                  else [Simple.var is_int_in_wrapper]
                in
                let tag =
                  if not needs_discriminant then []
                  else [Simple.var tag]
                in
                Apply_cont (join_cont, None,
                  is_int_in_wrapper @ tag @ fields_for_apply)
              in
              Array.fold_right (fun (index, var_opt) filler ->
                  match var_opt with
                  | None -> filler
                  | Some var ->
                    Flambda.Expr.create_let var
                      S.unboxed_kind
                      (S.get_field
                        ~boxed_value:(Simple.var wrapper_param_unboxee)
                        ~index dbg)
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
        let arms =
          Immediate.Map.fold (fun size filler_cont arms ->
              let size = Immediate.to_targetint size in
              Tag.Map.fold (fun tag this_size arms ->
                  if Targetint.OCaml.equal this_size size then
                    Tag.Map.add tag filler_cont arms
                  else
                    arms)
                blocks
                arms)
            sizes_to_filler_conts
            Tag.Map.empty
        in
        Flambda.Expr.create_tag_switch ~scrutinee:(Name.var tag) ~arms
      in
      Flambda.Expr.create_let is_int_in_wrapper
        (K.value ())
        (if no_constant_ctors then Simple Simple.const_zero
         else Prim (Unary (Is_int, Simple.var wrapper_param_unboxee), dbg))
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
                      if no_constant_ctors then []
                      else [Simple.var is_int_in_wrapper]
                    in
                    let wrapper_param_unboxee =
                      if not needs_discriminant then []
                      else [Simple.var wrapper_param_unboxee]
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
                    (K.fabricated ())
                    (match discriminant_known_value with
                     | Some known -> known
                     | None ->
                       Prim (Unary (Get_tag { tags_to_sizes; },
                         Simple.var wrapper_param_unboxee), dbg))
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
      Array.init (Targetint.OCaml.to_int_exn max_size) (fun index ->
        let append = string_of_int index in
        let var = Variable.rename ~append unboxee in
        (* CR mshinwell: per-field kinds are not needed *)
        var, S.unboxed_kind)
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
      Flambda.Expr.create_int_switch ~scrutinee:(Name.var is_int) ~arms
    in
    let boxing_switch =
      let arms =
        Tag.Map.map (fun (_size, boxing_cont) -> boxing_cont)
          tags_to_sizes_and_boxing_conts
      in
      Flambda.Expr.create_tag_switch ~scrutinee:(Name.var discriminant) ~arms
    in
    let build_boxed_value_from_new_params =
      let boxed = Variable.rename ~append:"_boxed" unboxee in
      let boxed_param = Parameter.wrap boxed in
      (* CR mshinwell: Should be able to do better for [boxed_ty] *)
      let boxed_ty = T.any_value () in
      let boxed_typed_param =
        Flambda.Typed_parameter.create boxed_param boxed_ty
      in
      let join_cont = Continuation.create () in
      let build (expr : Flambda.Expr.t) : Flambda.Expr.t =
        let arms =
          Immediate.Set.fold (fun ctor_index arms ->
              let ctor_index =
                Targetint.OCaml.to_int (Immediate.to_targetint ctor_index)
              in
              let tag = Tag.create_exn ctor_index in
              let cont = Continuation.create () in
              Tag.Map.add tag cont arms)
            constant_ctors
            Tag.Map.empty
        in
        let constant_ctor_switch =
          Flambda.Expr.create_tag_switch ~scrutinee:(Name.var discriminant)
            ~arms
        in
        let add_constant_ctor_conts expr =
          Tag.Map.fold (fun ctor_index cont expr : Flambda.Expr.t ->
              let ctor_index = Tag.to_int ctor_index in
              Let_cont {
                body = expr;
                handlers = Non_recursive {
                  name = cont;
                  handler = {
                    handler = Apply_cont (
                      join_cont, None, [Simple.const_int ctor_index]);
                    params = [];
                    stub = true;
                    is_exn_handler = false;
                  };
                };
              })
            arms
            expr
        in
        let add_boxing_conts expr =
          Tag.Map.fold (fun tag (size, boxing_cont) expr : Flambda.Expr.t ->
              let size = Targetint.OCaml.to_int size in
              let boxed = Variable.rename boxed in
              let fields =
                let fields, _index =
                  List.fold_left (fun (fields, index) (field, _kind) ->
                      if index >= size then fields, index + 1
                      else (Simple.var field :: fields), index + 1)
                    ([], 0)
                    fields_with_kinds
                in
                List.rev fields
              in
              let handler : Flambda.Expr.t =
                Flambda.Expr.create_let boxed
                  (K.value ())
                  (S.box unboxing_spec_user_data tag fields dbg)
                  (Apply_cont (join_cont, None, [Simple.var boxed]))
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
          | Some named ->
            Flambda.Expr.create_let is_int (K.value ()) named body
        in
        match discriminant_known_value with
        | None -> body
        | Some named ->
          Flambda.Expr.create_let discriminant (K.fabricated ()) named body
      in
      [boxed_typed_param, build]
    in
    let is_int =
      if no_constant_ctors then []
      else
        let is_int_ty =
          let by_constant_ctor_index =
            Immediate.Set.fold (fun ctor_index by_constant_ctor_index ->
                (* CR mshinwell: bad conversion *)
                let tag =
                  Tag.create_exn (Targetint.OCaml.to_int (
                    Immediate.to_targetint ctor_index))
                in
                let env = T.Typing_environment.create () in
                Tag.Map.add tag env by_constant_ctor_index)
              constant_ctors
              Tag.Map.empty
          in
          let by_tag =
            Tag.Map.mapi (fun tag size ->
                (* CR mshinwell: rewrite for-loop to avoid conversion *)
                let size = Targetint.OCaml.to_int size in
                let scope_level = E.continuation_scope_level env in
                let unboxee_ty_refinement =
                  let fields =
                    match Misc.Stdlib.List.first_n fields_with_kinds size with
                    | Some (fields_with_kinds, _) ->
                      List.map (fun (field, kind) : _ T.mutable_or_immutable ->
                          let field = Name.var field in
                          Immutable (T.alias_type_of kind field))
                        fields_with_kinds
                    | None -> assert false
                  in
                  T.block tag ~fields:(Array.of_list fields)
                in
                let initial_env =
                  T.Typing_environment.singleton (Name.var unboxee) scope_level
                    unboxee_ty_refinement
                in
                let env = ref initial_env in
                for field = 0 to size - 1 do
                  let field, kind = fields_with_kinds0.(field) in
                  (* CR mshinwell: We could refine the types of the actual fields
                     themselves according to the tag. *)
                  let field_ty = T.unknown kind in
                  env := T.Typing_environment.add !env (Name.var field)
                    scope_level field_ty
                done;
                !env)
              tags_to_sizes
          in
          let discriminant_env_is_int =
            T.Typing_environment.singleton (Name.var discriminant)
              (E.continuation_scope_level env)
              (T.these_tags by_constant_ctor_index)
          in
          let discriminant_env_is_block =
            T.Typing_environment.singleton (Name.var discriminant)
              (E.continuation_scope_level env)
              (T.these_tags by_tag)
          in
          let by_is_int_result =
            Immediate.Map.of_list [
              Immediate.bool_true, discriminant_env_is_int;
              Immediate.bool_false, discriminant_env_is_block;
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
        let discriminant_ty = T.any_fabricated () in (* CR mshinwell: improve *)
        (* XXX better calc. for [discriminant_ty] is above, maybe *)
        [Flambda.Typed_parameter.create discriminant discriminant_ty]
    in
    let fields =
      List.map (fun (field, _kind) ->
          let field = Parameter.wrap field in
          Flambda.Typed_parameter.create_from_kind field S.unboxed_kind)
        fields_with_kinds
    in
    let unboxee_ty =
      let all_fields =
        (* CR mshinwell: Just make [fields_with_kinds] a value (renamed) of
           type [Name.t list]? *)
        List.map (fun (field, _kind) -> Name.var field) fields_with_kinds
      in
      S.refine_unboxee_ty ~type_of_name unboxing_spec_user_data
        ~unboxee_ty ~all_fields
    in
    { unboxee_to_wrapper_params_unboxee;
      add_bindings_in_wrapper;
      new_arguments_for_call_in_wrapper;
      new_params = is_int @ discriminant @ fields;
      new_unboxee_types = [unboxee, unboxee_ty];
      build_boxed_value_from_new_params;
    }
end

module Unbox_variant = Make (Unboxing_spec_variant)
module Unbox_float_array = Make (Unboxing_spec_float_array)
module Unbox_boxed_float = Make (Unboxing_spec_boxed_float)
module Unbox_boxed_int32 = Make (Unboxing_spec_boxed_int32)
module Unbox_boxed_int64 = Make (Unboxing_spec_boxed_int64)
module Unbox_boxed_nativeint = Make (Unboxing_spec_boxed_nativeint)

let how_to_unbox ~type_of_name ~env ~unboxee ~unboxee_ty ~is_unbox_returns =
  let unbox ~f ~unboxing_spec_user_data ~unboxing_spec =
    Some (f ~type_of_name ~env ~unboxee ~unboxee_ty
      ~unboxing_spec_user_data ~unboxing_spec ~is_unbox_returns)
  in
  match T.prove_unboxable ~type_of_name ~unboxee_ty with
  | Cannot_unbox -> None
  | proof ->
    match Unboxing_spec_variant.create proof with
    | Some (unboxing_spec_user_data, unboxing_spec) ->
      unbox ~f:Unbox_variant.unbox ~unboxing_spec_user_data ~unboxing_spec
    | None ->
      match Unboxing_spec_float_array.create proof with
      | Some (unboxing_spec_user_data, unboxing_spec) ->
        unbox ~f:Unbox_float_array.unbox ~unboxing_spec_user_data ~unboxing_spec
      | None ->
        match Unboxing_spec_boxed_float.create proof with
        | Some (unboxing_spec_user_data, unboxing_spec) ->
          unbox ~f:Unbox_boxed_float.unbox ~unboxing_spec_user_data
            ~unboxing_spec
        | None ->
          match Unboxing_spec_boxed_int32.create proof with
          | Some (unboxing_spec_user_data, unboxing_spec) ->
            unbox ~f:Unbox_boxed_int32.unbox ~unboxing_spec_user_data
              ~unboxing_spec
          | None ->
            match Unboxing_spec_boxed_int64.create proof with
            | Some (unboxing_spec_user_data, unboxing_spec) ->
              unbox ~f:Unbox_boxed_int64.unbox ~unboxing_spec_user_data
                ~unboxing_spec
            | None ->
              match Unboxing_spec_boxed_nativeint.create proof with
              | Some (unboxing_spec_user_data, unboxing_spec) ->
                unbox ~f:Unbox_boxed_nativeint.unbox ~unboxing_spec_user_data
                  ~unboxing_spec
              | None -> None
