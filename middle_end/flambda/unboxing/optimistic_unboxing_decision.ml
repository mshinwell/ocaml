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

open! Simplify_import

open Unboxing_types
module U = Unboxers

(* small helper used later *)
let pp_tag print_tag ppf tag =
  if print_tag then Format.fprintf ppf "_%d" (Tag.to_int tag)

(* Unfold a type into a decision tree *)
(* ********************************** *)

(* CR: make this a Clflag *)
let max_unboxing_depth = 1
let unbox_numbers = true
let unbox_blocks = true
let unbox_variants = true
let unbox_closures = true

let make_optimistic_const_ctor () =
  let is_int = new_param "is_int" in
  let unboxed_const_ctor = new_param "unboxed_const_ctor" in
  let ctor = Unbox (Number (Naked_immediate, unboxed_const_ctor)) in
  At_least_one { is_int; ctor; }

let make_optimistic_number_decision tenv param_type
      (decider : Unboxers.number_decider) : decision option =
  match decider.prove_is_a_boxed_number tenv param_type with
  | Proved () ->
    let naked_number = new_param decider.param_name in
    Some (Unbox (Number (decider.kind, naked_number)))
  | Wrong_kind | Invalid | Unknown ->
    None

let decide tenv param_type deciders : decision option =
  List.find_map (make_optimistic_number_decision tenv param_type) deciders

let deciders = [
  U.Immediate.decider;
  U.Float.decider;
  U.Int32.decider;
  U.Int64.decider;
  U.Nativeint.decider;
]

let rec make_optimistic_decision ~depth tenv ~param_type : decision =
  match decide tenv param_type deciders with
  | Some decision ->
    if unbox_numbers then decision else Do_not_unbox Incomplete_parameter_type
  | None ->
    if depth >= max_unboxing_depth then Do_not_unbox Max_depth_exceeded
    else match T.prove_unique_tag_and_size tenv param_type with
      | Proved (tag, size) when unbox_blocks ->
        let fields =
          make_optimistic_fields
            ~add_tag_to_name:false ~depth
            tenv param_type tag size
        in
        Unbox (Unique_tag_and_size { tag; fields; })
      | Proved _ | Wrong_kind | Invalid | Unknown ->
        match T.prove_variant_like tenv param_type with
        | Proved { const_ctors; non_const_ctors_with_sizes; }
            when unbox_variants ->
          let tag = new_param "tag" in
          let constant_constructors =
            match const_ctors with
            | Known set when Target_imm.Set.is_empty set -> Zero
            | Unknown | Known _ -> make_optimistic_const_ctor ()
          in
          let fields_by_tag =
            Tag.Scannable.Map.mapi (fun scannable_tag size ->
              let tag = Tag.Scannable.to_tag scannable_tag in
              make_optimistic_fields
                ~add_tag_to_name:true ~depth
                tenv param_type tag size
            ) non_const_ctors_with_sizes
          in
          Unbox (Variant { tag; constant_constructors; fields_by_tag; })
        | Proved _ | Wrong_kind | Invalid | Unknown ->
          begin match T.prove_single_closures_entry' tenv param_type with
          | Proved (closure_id, closures_entry, _fun_decl)
              when unbox_closures ->
            let vars_within_closure =
              make_optimistic_vars_within_closure ~depth tenv closures_entry
            in
            Unbox (Closure_single_entry { closure_id; vars_within_closure })
          | Proved _ | Wrong_kind | Invalid | Unknown ->
            Do_not_unbox Incomplete_parameter_type
          end

and make_optimistic_fields
      ~add_tag_to_name ~depth
      tenv param_type (tag : Tag.t) size =
  let field_kind, field_base_name =
    if Tag.equal tag Tag.double_array_tag
    then K.naked_float, "unboxed_float_field"
    else K.value, "unboxed_field"
  in
  let field_name n =
    Format.asprintf "%s%a_%d" field_base_name (pp_tag add_tag_to_name) tag n
  in
  let field_vars =
    List.init (Targetint.OCaml.to_int size)
      (fun i -> new_param (field_name i))
  in
  let type_of_var epa =
    Flambda_type.alias_type_of field_kind (Simple.var epa.param)
  in
  let field_types = List.map type_of_var field_vars in
  let tenv =
    List.fold_left (fun acc { param = var; args = _; } ->
      let name = Name_in_binding_pos.create (Name.var var) Name_mode.normal in
      TE.add_definition acc name field_kind
    ) tenv field_vars
  in
  let shape =
    Flambda_type.immutable_block ~is_unique:false tag
      ~field_kind ~fields:field_types
  in
  let env_extension =
    match T.meet tenv param_type shape with
    | Ok (_, env_extension) -> env_extension
    | Bottom ->
      Misc.fatal_errorf "Meet failed whereas prove previously succeeded"
  in
  let tenv = TE.add_env_extension tenv env_extension in
  let fields =
    List.map2 (fun epa var_type ->
      let decision =
        make_optimistic_decision ~depth:(depth + 1) tenv ~param_type:var_type
      in
      { epa; decision; }
    ) field_vars field_types
  in
  fields

and make_optimistic_vars_within_closure
      ~depth tenv closures_entry =
  let map = T.Closures_entry.closure_var_types closures_entry in
  Var_within_closure.Map.mapi (fun var_within_closure var_type ->
    let epa = new_param (Var_within_closure.to_string var_within_closure) in
    let decision =
      make_optimistic_decision ~depth:(depth + 1) tenv ~param_type:var_type
    in
    { epa; decision; }
  ) map
