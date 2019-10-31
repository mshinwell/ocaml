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

(* CR mshinwell: Add a command-line flag. *)
let max_unboxing_depth = 1

module type Unboxing_spec = sig
  val var_name : string

  val make_boxed_value : Tag.t -> fields:T.t list -> T.t

  val make_boxed_value_with_size_at_least
     : n:Targetint.OCaml.t
    -> field_n_minus_one:Variable.t
    -> T.t

  val project_field : block:Simple.t -> index:Simple.t -> P.t
end

module Make (U : Unboxing_spec) = struct
  let unbox_one_field_of_one_parameter ~extra_param ~index
        ~arg_types_by_use_id =
    let param_kind = KP.kind extra_param in
    let field_var = Variable.create "field_at_use" in
    let field_name =
      Name_in_binding_pos.create (Name.var field_var) Name_mode.in_types
    in
    let shape =
      U.make_boxed_value_with_size_at_least
        ~n:(Targetint.OCaml.of_int (index + 1))
        ~field_n_minus_one:field_var
    in
    (* Don't unbox parameters unless, at all use sites, there is a
       non-irrelevant [Simple] available for the corresponding field of the
       block. *)
    Apply_cont_rewrite_id.Map.fold
      (fun id (typing_env_at_use, arg_type_at_use)
           (extra_args, field_types_by_id) ->
        let env_extension =
          let result_var =
            Var_in_binding_pos.create field_var Name_mode.normal
          in
          T.meet_shape typing_env_at_use arg_type_at_use
            ~shape ~result_var ~result_kind:param_kind
        in
        let field = Simple.var field_var in
        match env_extension with
        | Bottom ->
          let field_types_by_id =
            Apply_cont_rewrite_id.Map.add id
              (typing_env_at_use, T.bottom param_kind)
              field_types_by_id
          in
          None, field_types_by_id
        | Ok env_extension ->
          let typing_env_at_use =
            TE.add_definition typing_env_at_use field_name param_kind
          in
          let typing_env_at_use =
            TE.add_env_extension typing_env_at_use ~env_extension
          in
          let field_type = T.alias_type_of param_kind field in
          let field_types_by_id =
            Apply_cont_rewrite_id.Map.add id
              (typing_env_at_use, field_type)
              field_types_by_id
          in
          match extra_args with
          | None -> None, field_types_by_id
          | Some extra_args ->
            let canonical_simple, kind' =
              TE.get_canonical_simple_with_kind typing_env_at_use
                ~min_name_mode:Name_mode.normal
                field
            in
            assert (Flambda_kind.equal param_kind kind');
            (* CR-someday pchambart: This shouldn't add another load if
               there is already one in the list of parameters.  That is,
                 apply_cont k (a, b) a
                 where k x y
               should become:
                 apply_cont k (a, b) a b
                 where k x y b'
               not:
                 apply_cont k (a, b) a a b
                 where k x y a' b'
               mshinwell: We never add any loads now, but might in the future.
            *)
            match canonical_simple with
            | Bottom | Ok None -> None, field_types_by_id
            | Ok (Some simple) ->
              if Simple.equal simple field then None, field_types_by_id
              else
                let extra_arg : EA.t = Already_in_scope simple in
                let extra_args =
                  Apply_cont_rewrite_id.Map.add id extra_arg extra_args
                in
                Some extra_args, field_types_by_id)
      arg_types_by_use_id
      (Some Apply_cont_rewrite_id.Map.empty, Apply_cont_rewrite_id.Map.empty)

  let unbox_fields_of_one_parameter ~new_param_vars ~arg_types_by_use_id
        extra_params_and_args =
    let _index, param_types_rev, all_field_types_by_id_rev,
        extra_params_and_args =
      List.fold_left
        (fun (index, param_types_rev, all_field_types_by_id_rev,
              extra_params_and_args) extra_param ->
          let extra_args, field_types_by_id =
            unbox_one_field_of_one_parameter ~extra_param ~index
              ~arg_types_by_use_id
          in
          let param_type =
            let param_kind = KP.kind extra_param in
            match extra_args with
            | None -> T.unknown param_kind
            | Some _ -> T.alias_type_of param_kind (KP.simple extra_param)
          in
          let all_field_types_by_id_rev =
            field_types_by_id :: all_field_types_by_id_rev
          in
          match extra_args with
          | None ->
            index + 1, param_type :: param_types_rev,
              all_field_types_by_id_rev, extra_params_and_args
          | Some extra_args ->
            let extra_params_and_args =
              EPA.add extra_params_and_args ~extra_param ~extra_args
            in
            index + 1, param_type :: param_types_rev,
              all_field_types_by_id_rev, extra_params_and_args)
        (0, [], [], extra_params_and_args)
        new_param_vars
    in
    List.rev param_types_rev, List.rev all_field_types_by_id_rev,
      extra_params_and_args

  let unbox_one_parameter typing_env ~depth ~arg_types_by_use_id ~param_type
        extra_params_and_args ~unbox_value tag size kind =
    let new_param_vars =
      List.init (Targetint.OCaml.to_int size) (fun index ->
        let name = Printf.sprintf "%s%d" U.var_name index in
        let var = Variable.create name in
        KP.create (Parameter.wrap var) kind)
    in
    let fields, all_field_types_by_id, extra_params_and_args =
      unbox_fields_of_one_parameter ~new_param_vars ~arg_types_by_use_id
        extra_params_and_args
    in
    let block_type = U.make_boxed_value tag ~fields in
    let typing_env =
      TE.add_definitions_of_params typing_env ~params:new_param_vars
    in
    match T.meet typing_env block_type param_type with
    | Bottom ->
      Misc.fatal_errorf "[meet] between %a and %a should not have failed"
        T.print block_type
        T.print param_type
    | Ok (param_type, env_extension) ->
      (* CR mshinwell: We can remove [New_let_binding] if we are always going to
         restrict ourselves to types where the field components are known
         [Simple]s. *)
      let typing_env = TE.add_env_extension typing_env ~env_extension in
      assert (List.compare_lengths fields all_field_types_by_id = 0);
      let typing_env, extra_params_and_args =
        List.fold_left2
          (fun (typing_env, extra_params_and_args) 
               field_type field_types_by_id ->
            (* For any field of kind [Value] of the parameter being unboxed,
               then attempt to unbox its contents too. *)
            let field_kind = T.kind field_type in
            if not (K.equal field_kind K.value) then
              typing_env, extra_params_and_args
            else begin
              let typing_env, _, extra_params_and_args =
                unbox_value typing_env ~depth:(depth + 1)
                  ~arg_types_by_use_id:field_types_by_id
                  ~param_type:field_type
                  extra_params_and_args
              in
              typing_env, extra_params_and_args
            end)
          (typing_env, extra_params_and_args)
          fields all_field_types_by_id
      in
      typing_env, param_type, extra_params_and_args
end

module Block_spec : Unboxing_spec = struct
  let var_name = "unboxed"

  let make_boxed_value = T.immutable_block
  let make_boxed_value_with_size_at_least = T.immutable_block_with_size_at_least

  let project_field ~block ~index =
    P.Binary (Block_load (Block (Value Anything), Immutable), block, index)
end

module Make_unboxed_number_spec (N : sig
  val name : string
  val var_name : string

  val tag : Tag.t

  val unboxed_kind : K.t
  val boxable_number_kind : K.Boxable_number.t

  val box : T.t -> T.t
end) = struct
  let var_name = N.var_name

  let make_boxed_value tag ~fields =
    assert (Tag.equal tag N.tag);
    match fields with
    | [field] -> N.box field
    | _ -> Misc.fatal_errorf "Boxed %ss only have one field" N.name

  let make_boxed_value_with_size_at_least ~n ~field_n_minus_one =
    if not (Targetint.OCaml.equal n Targetint.OCaml.one) then begin
       Misc.fatal_errorf "Boxed %ss only have one field" N.name
    end;
    N.box (T.alias_type_of N.unboxed_kind (Simple.var field_n_minus_one))

  let project_field ~block ~index:_ =
    P.Unary (Unbox_number N.boxable_number_kind, block)
end

module Immediate_spec : Unboxing_spec = Make_unboxed_number_spec (struct
  let name = "immediate"
  let var_name = "untagged_imm"
  let tag = Tag.zero  (* CR mshinwell: make optional *)
  let unboxed_kind = K.naked_immediate
  let boxable_number_kind = K.Boxable_number.Untagged_immediate
  let box = T.tag_immediate
end)

module Float_spec : Unboxing_spec = Make_unboxed_number_spec (struct
  let name = "float"
  let var_name = "unboxed_float"
  let tag = Tag.double_tag
  let unboxed_kind = K.naked_float
  let boxable_number_kind = K.Boxable_number.Naked_float
  let box = T.box_float
end)

module Int32_spec : Unboxing_spec = Make_unboxed_number_spec (struct
  let name = "int32"
  let var_name = "unboxed_int32"
  let tag = Tag.custom_tag
  let unboxed_kind = K.naked_int32
  let boxable_number_kind = K.Boxable_number.Naked_int32
  let box = T.box_int32
end)

module Int64_spec : Unboxing_spec = Make_unboxed_number_spec (struct
  let name = "int64"
  let var_name = "unboxed_int64"
  let tag = Tag.custom_tag
  let unboxed_kind = K.naked_int64
  let boxable_number_kind = K.Boxable_number.Naked_int64
  let box = T.box_int64
end)

module Nativeint_spec : Unboxing_spec = Make_unboxed_number_spec (struct
  let name = "nativeint"
  let var_name = "unboxed_nativeint"
  let tag = Tag.custom_tag
  let unboxed_kind = K.naked_nativeint
  let boxable_number_kind = K.Boxable_number.Naked_nativeint
  let box = T.box_nativeint
end)

module Blocks = Make (Block_spec)
module Immediates = Make (Immediate_spec)
module Floats = Make (Float_spec)
module Int32s = Make (Int32_spec)
module Int64s = Make (Int64_spec)
module Nativeints = Make (Nativeint_spec)

let unboxed_number_decisions = [
  T.prove_is_a_tagged_immediate, Immediates.unbox_one_parameter,
    Tag.zero, K.naked_immediate;
  T.prove_is_a_boxed_float, Floats.unbox_one_parameter,
    Tag.double_tag, K.naked_float;
  T.prove_is_a_boxed_int32, Int32s.unbox_one_parameter,
    Tag.custom_tag, K.naked_int32;
  T.prove_is_a_boxed_int64, Int64s.unbox_one_parameter,
    Tag.custom_tag, K.naked_int64;
  T.prove_is_a_boxed_nativeint, Nativeints.unbox_one_parameter,
    Tag.custom_tag, K.naked_nativeint;
]

let rec make_unboxing_decision typing_env ~depth ~arg_types_by_use_id
      ~param_type extra_params_and_args =
  if depth > max_unboxing_depth then
    typing_env, param_type, extra_params_and_args
  else
    match T.prove_unique_tag_and_size typing_env param_type with
    | Proved (tag, size) ->
      Blocks.unbox_one_parameter typing_env ~depth ~arg_types_by_use_id
        ~param_type extra_params_and_args ~unbox_value:make_unboxing_decision
        tag size K.value
    | Wrong_kind | Invalid | Unknown ->
      let rec try_unboxing = function
        | [] -> typing_env, param_type, extra_params_and_args
        | (prover, unboxer, tag, kind) :: decisions ->
          let proof : _ T.proof_allowing_kind_mismatch =
            prover typing_env param_type
          in
          match proof with
          | Proved () ->
            unboxer typing_env ~depth ~arg_types_by_use_id ~param_type
              extra_params_and_args ~unbox_value:make_unboxing_decision
              tag Targetint.OCaml.one kind
          | Wrong_kind | Invalid | Unknown -> try_unboxing decisions
      in
      try_unboxing unboxed_number_decisions

let make_unboxing_decisions typing_env ~arg_types_by_use_id ~params ~param_types
      extra_params_and_args =
  assert (List.compare_lengths params param_types = 0);
  let typing_env, param_types_rev, extra_params_and_args =
    List.fold_left (fun (typing_env, param_types_rev, extra_params_and_args)
              (arg_types_by_use_id, param_type) ->
        let typing_env, param_type, extra_params_and_args =
          make_unboxing_decision typing_env ~depth:0 ~arg_types_by_use_id
            ~param_type extra_params_and_args
        in
        typing_env, param_type :: param_types_rev, extra_params_and_args)
      (typing_env, [], extra_params_and_args)
      (List.combine arg_types_by_use_id param_types)
  in
  let typing_env =
    TE.add_equations_on_params typing_env
      ~params ~param_types:(List.rev param_types_rev)
  in
  typing_env, extra_params_and_args
