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

module type Unboxing_spec = sig
  val make_boxed_value : Tag.t -> fields:T.t list -> T.t

  val make_boxed_value_with_size_at_least
     : n:Targetint.OCaml.t
    -> field_n_minus_one:Variable.t
    -> T.t
end

module Make (U : Unboxing_spec) = struct
  let parameters_to_unbox_with_extra_args ~new_param_vars ~args_by_use_id
        extra_params_and_args =
    List.fold_left
      (fun (index, param_types_rev, extra_params_and_args) extra_param ->
        let param_kind = KP.kind extra_param in
        let field_var = Variable.create "field_at_use" in
        let field_name =
          Name_in_binding_pos.create (Name.var field_var)
            Name_occurrence_kind.normal
        in
        let shape =
          U.make_boxed_value_with_size_at_least
            ~n:(Targetint.OCaml.of_int (index + 1))
            ~field_n_minus_one:field_var
        in
        let extra_args =
          (* Don't unbox parameters unless, at all use sites, there is
             a non-irrelevant [Simple] available for the corresponding
             field of the block. *)
          Apply_cont_rewrite_id.Map.fold
            (fun id (typing_env_at_use, arg) extra_args ->
              match extra_args with
              | None -> None
              | Some extra_args ->
                let env_extension =
                  let arg_type_at_use = T.alias_type_of K.value arg in
                  let result_var =
                    Var_in_binding_pos.create field_var
                      Name_occurrence_kind.normal
                  in
                  T.meet_shape typing_env_at_use arg_type_at_use
                    ~shape ~result_var ~result_kind:param_kind
                in
                match env_extension with
                | Bottom -> None
                | Ok env_extension ->
                  let typing_env_at_use =
                    TE.add_definition typing_env_at_use field_name param_kind
                  in
                  let typing_env_at_use =
                    TE.add_env_extension typing_env_at_use env_extension
                  in
                  let field = Simple.var field_var in
                  let canonical_simple, kind' =
                    TE.get_canonical_simple_with_kind typing_env_at_use
                      ~min_occurrence_kind:Name_occurrence_kind.normal
                      field
                  in
                  assert (Flambda_kind.equal param_kind kind');
                  (* CR pchambart: This shouldn't add another load if
                     there is already one in the list of parameters

                       apply_cont k (a, b) a
                       where k x y

                     should become

                       apply_cont k (a, b) a b
                       where k x y b'

                     not

                       apply_cont k (a, b) a a b
                       where k x y a' b'

                     mshinwell: Think about this now that we're not adding extra
                     loads.  Probably still relevant.
                  *)
                  match canonical_simple with
                  | Bottom | Ok None -> None
                  | Ok (Some simple) ->
                    if Simple.equal simple field then None
                    else
                      let extra_arg : EA.t = Already_in_scope simple in
                      let extra_args =
                        Apply_cont_rewrite_id.Map.add id extra_arg extra_args
                      in
                      Some extra_args)
            args_by_use_id
            (Some Apply_cont_rewrite_id.Map.empty)
        in
        match extra_args with
        | None ->
          let param_type = T.unknown param_kind in
          index + 1, param_type :: param_types_rev, extra_params_and_args
        | Some extra_args ->
          let extra_params_and_args =
            EPA.add extra_params_and_args ~extra_param ~extra_args
          in
          let param_type = T.alias_type_of param_kind (KP.simple extra_param) in
          index + 1, param_type :: param_types_rev, extra_params_and_args)
      (0, [], extra_params_and_args)
      new_param_vars

  let make_unboxing_decision typing_env ~args_by_use_id ~param_type
        extra_params_and_args tag size kind =
    let new_param_vars =
      List.init (Targetint.OCaml.to_int size) (fun index ->
        let name = Printf.sprintf "unboxed%d" index in
        let var = Variable.create name in
        KP.create (Parameter.wrap var) kind)
    in
    let _index, param_types_rev, extra_params_and_args =
      parameters_to_unbox_with_extra_args ~new_param_vars ~args_by_use_id
        extra_params_and_args
    in
    let fields = List.rev param_types_rev in
    let block_type = U.make_boxed_value tag ~fields in
    let typing_env =
      List.fold_left (fun typing_env param ->
          let name =
            Name_in_binding_pos.create (KP.name param)
              Name_occurrence_kind.normal
          in
          TE.add_definition typing_env name (KP.kind param))
        typing_env
        new_param_vars
    in
    match T.meet typing_env block_type param_type with
    | Bottom ->
      Misc.fatal_errorf "[meet] between %a and %a should not have failed"
        T.print block_type
        T.print param_type
    | Ok (param_type, env_extension) ->
      (* CR mshinwell: We can probably remove [New_let_binding] if we
         are always going to restrict ourselves to types where the
         field components are known [Simple]s. *)
      let typing_env = TE.add_env_extension typing_env env_extension in
      typing_env, param_type, extra_params_and_args
end

module Block_spec : Unboxing_spec = struct
  let make_boxed_value = T.immutable_block
  let make_boxed_value_with_size_at_least = T.immutable_block_with_size_at_least
end

module Float_spec : Unboxing_spec = struct
  let make_boxed_value tag ~fields =
    assert (Tag.equal tag Tag.double_tag);
    match fields with
    | [field] -> T.box_float field
    | _ -> Misc.fatal_error "Boxed floats only have one field"

  let make_boxed_value_with_size_at_least ~n ~field_n_minus_one =
    if not (Targetint.OCaml.equal n Targetint.OCaml.one) then begin
       Misc.fatal_error "Boxed floats only have one field"
    end;
    T.box_float (T.alias_type_of K.naked_float (Simple.var field_n_minus_one))
end

module Blocks = Make (Block_spec)
module Floats = Make (Float_spec)

let make_unboxing_decision typing_env ~args_by_use_id ~param_type
      extra_params_and_args =
  match T.prove_unique_tag_and_size typing_env param_type with
  | Proved (tag, size) ->
    Blocks.make_unboxing_decision typing_env ~args_by_use_id ~param_type
      extra_params_and_args tag size K.value
  | Wrong_kind | Invalid | Unknown ->
    match T.prove_is_a_boxed_float typing_env param_type with
    | Proved () ->
      Floats.make_unboxing_decision typing_env ~args_by_use_id ~param_type
        extra_params_and_args Tag.double_tag Targetint.OCaml.one K.naked_float
    | Wrong_kind | Invalid | Unknown ->
      typing_env, param_type, extra_params_and_args

let make_unboxing_decisions typing_env ~args_by_use_id ~param_types
      extra_params_and_args =
  let typing_env, param_types_rev, extra_params_and_args =
    List.fold_left (fun (typing_env, param_types_rev, extra_params_and_args)
              (args_by_use_id, param_type) ->
        let typing_env, param_type, extra_params_and_args =
          make_unboxing_decision typing_env ~args_by_use_id ~param_type
            extra_params_and_args
        in
        typing_env, param_type :: param_types_rev, extra_params_and_args)
      (typing_env, [], extra_params_and_args)
      (List.combine args_by_use_id param_types)
  in
  typing_env, List.rev param_types_rev, extra_params_and_args
