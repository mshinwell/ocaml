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

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

open Unboxing_types
module U = Unboxers

exception Prevent_current_unboxing

let prevent_current_unboxing () = raise Prevent_current_unboxing

type unboxed_arg =
  | Poison (* used for recursive calls *)
  | Available of Simple.t
  | Generated of Variable.t
  | Added_by_wrapper_at_rewrite_use of { nth_arg : int; }

let _print_unboxed_arg ppf = function
  | Poison -> Format.fprintf ppf "poison"
  | Available simple -> Format.fprintf ppf "simple: %a" Simple.print simple
  | Generated v -> Format.fprintf ppf "generated: %a" Variable.print v
  | Added_by_wrapper_at_rewrite_use {nth_arg } ->
    Format.fprintf ppf "added_by_wrapper(%d)" nth_arg

let type_of_arg_being_unboxed unboxed_arg =
  let aux simple = T.alias_type_of K.value simple in
  match unboxed_arg with
  | Poison -> None
  | Available simple -> Some (aux simple)
  | Generated var -> Some (aux (Simple.var var))
  | Added_by_wrapper_at_rewrite_use _ -> prevent_current_unboxing ()

let arg_being_unboxed_of_extra_arg extra_arg =
  match (extra_arg : EPA.Extra_arg.t) with
  | Already_in_scope simple -> Available simple
  | New_let_binding (var, _)
  | New_let_binding_with_named_args (var, _) -> Generated var

let extra_arg_of_arg_being_unboxed (unboxer : Unboxers.unboxer)
      ~typing_env_at_use arg_being_unboxed =
  match arg_being_unboxed with
  | Poison ->
    EPA.Extra_arg.Already_in_scope (Simple.const unboxer.invalid_const)
  | Available arg_at_use ->
    let arg_type = T.alias_type_of K.value arg_at_use in
    begin match unboxer.prove_simple typing_env_at_use arg_type
                  ~min_name_mode:Name_mode.normal with
    | Proved simple ->
      EPA.Extra_arg.Already_in_scope simple
    | Invalid ->
      EPA.Extra_arg.Already_in_scope (Simple.const unboxer.invalid_const)
    | Unknown ->
      let var = Variable.create unboxer.var_name in
      let prim = unboxer.unboxing_prim arg_at_use in
      EPA.Extra_arg.New_let_binding (var, prim)
    end
  | Generated var ->
    let arg_at_use = Simple.var var in
    let var = Variable.create unboxer.var_name in
    let prim = unboxer.unboxing_prim arg_at_use in
    EPA.Extra_arg.New_let_binding (var, prim)
  | Added_by_wrapper_at_rewrite_use { nth_arg; } ->
    let var = Variable.create "unboxed_field" in
    EPA.Extra_arg.New_let_binding_with_named_args (var, (fun args ->
      let arg_simple = List.nth args nth_arg in
      unboxer.unboxing_prim arg_simple
    ))


(* Helpers for the variant case *)
(* **************************** *)

type variant_argument =
  | Not_a_constant_constructor
  | Maybe_constant_constructor of {
      is_int : Simple.t;
      arg_being_unboxed : unboxed_arg;
    }

let extra_arg_for_is_int = function
  | Maybe_constant_constructor { is_int; _ } ->
    EPA.Extra_arg.Already_in_scope is_int
  | Not_a_constant_constructor ->
    EPA.Extra_arg.Already_in_scope Simple.untagged_const_false

let extra_arg_for_ctor ~typing_env_at_use = function
  | Not_a_constant_constructor ->
    EPA.Extra_arg.Already_in_scope (
      Simple.untagged_const_int (Targetint.OCaml.of_int 0))
  | Maybe_constant_constructor { arg_being_unboxed; _ } ->
    match type_of_arg_being_unboxed arg_being_unboxed with
    | None ->
      EPA.Extra_arg.Already_in_scope (
        Simple.untagged_const_int (Targetint.OCaml.of_int 0))
    | Some arg_type ->
      match T.prove_could_be_tagging_of_simple typing_env_at_use
              ~min_name_mode:Name_mode.normal arg_type with
      | Proved simple ->
        EPA.Extra_arg.Already_in_scope simple
      | Unknown -> prevent_current_unboxing ()
      | Invalid ->
        (* Presumably, this means that we are in an impossible-to-reach
           case, and thus as in other cases, we only need to provide
           well-kinded values. *)
        EPA.Extra_arg.Already_in_scope (
          Simple.untagged_const_int (Targetint.OCaml.of_int 0))

let extra_args_for_const_ctor_of_variant
      constant_constructors_decision ~typing_env_at_use
      rewrite_id variant_arg =
  match constant_constructors_decision with
  | Zero ->
    begin match variant_arg with
    | Not_a_constant_constructor -> constant_constructors_decision
    | Maybe_constant_constructor _ ->
      Misc.fatal_errorf
        "The unboxed variant parameter was determined to have \
         no constant cases when deciding to unbox it (using the \
         parameter type), but at the use site, it is a constant \
         constructor."
    end
  | At_least_one { ctor = Do_not_unbox reason; is_int; } ->
    let is_int =
      Extra_param_and_args.update_param_args is_int rewrite_id
        (extra_arg_for_is_int variant_arg)
    in
    At_least_one { ctor = Do_not_unbox reason; is_int; }

  | At_least_one { ctor = Unbox Number (Naked_immediate, ctor); is_int; } ->
    let is_int =
      Extra_param_and_args.update_param_args is_int rewrite_id
        (extra_arg_for_is_int variant_arg)
    in
    begin try
      let ctor =
        Extra_param_and_args.update_param_args ctor rewrite_id
          (extra_arg_for_ctor ~typing_env_at_use variant_arg)
      in
      At_least_one { ctor = Unbox (Number (Naked_immediate, ctor)); is_int; }
    with Prevent_current_unboxing ->
      At_least_one {
        ctor = Do_not_unbox Not_enough_information_at_use;
        is_int;
      }
    end
  | At_least_one {
      ctor = Unbox (
        Unique_tag_and_size _ | Variant _ | Closure_single_entry _
      | Number ((Naked_float | Naked_int32 | Naked_int64 | Naked_nativeint),
                _));
      is_int = _; } ->
    Misc.fatal_errorf "Bad kind for unboxing the constant constructor \
                       of a variant"

(* Helpers for the number case *)
(* *************************** *)

let compute_extra_arg_for_number kind unboxer epa
      rewrite_id ~typing_env_at_use arg_being_unboxed =
  let extra_arg =
    extra_arg_of_arg_being_unboxed unboxer
      ~typing_env_at_use arg_being_unboxed
  in
  let epa = Extra_param_and_args.update_param_args epa rewrite_id extra_arg in
  Unbox (Number (kind, epa))


(* Recursive descent on decisions *)
(* ****************************** *)

let are_there_unknown_use_sites = function
  | Filter { recursive; } -> recursive
  | Compute_all_extra_args -> false

let rec compute_extra_args_for_one_decision_and_use ~pass
          rewrite_id ~typing_env_at_use arg_being_unboxed decision =
  try
    compute_extra_args_for_one_decision_and_use_aux ~pass
      rewrite_id ~typing_env_at_use arg_being_unboxed decision
  with Prevent_current_unboxing ->
    begin match pass with
    | Filter _ -> Do_not_unbox Not_enough_information_at_use
    | Compute_all_extra_args ->
      Misc.fatal_errorf "This case should have been filtered out before."
    end

and compute_extra_args_for_one_decision_and_use_aux ~pass
          rewrite_id ~typing_env_at_use arg_being_unboxed decision =
  match decision with
  | Do_not_unbox _ -> decision
  | Unbox Unique_tag_and_size { tag; fields; } ->
    compute_extra_args_for_block ~pass
      rewrite_id ~typing_env_at_use arg_being_unboxed tag fields
  | Unbox Closure_single_entry { closure_id; vars_within_closure; } ->
    if are_there_unknown_use_sites pass then
      prevent_current_unboxing ()
    else
      compute_extra_args_for_closure ~pass
        rewrite_id ~typing_env_at_use arg_being_unboxed
        closure_id vars_within_closure
  | Unbox Variant { tag; constant_constructors; fields_by_tag; } ->
    if are_there_unknown_use_sites pass then
      prevent_current_unboxing ()
    else begin
      let invalid () =
        (* Invalid here means that the apply_cont is unreachable, i.e. the args
           we generated will never be actually used at runtime, so the values of
           the args do not matter, they are here to make the kind checker
           happy. *)
        compute_extra_args_for_variant ~pass
          rewrite_id ~typing_env_at_use arg_being_unboxed
          tag constant_constructors fields_by_tag
          (Or_unknown.Known Target_imm.Set.empty) Tag.Scannable.Map.empty
      in
      begin match type_of_arg_being_unboxed arg_being_unboxed with
      | None -> invalid ()
      | Some arg_type ->
        begin match T.prove_variant_like typing_env_at_use arg_type with
        | Wrong_kind -> Misc.fatal_errorf "Kind error while unboxing a variant"
        | Unknown -> prevent_current_unboxing ()
        | Invalid -> invalid ()
        | Proved { const_ctors; non_const_ctors_with_sizes; } ->
          compute_extra_args_for_variant ~pass
            rewrite_id ~typing_env_at_use arg_being_unboxed
            tag constant_constructors fields_by_tag
            const_ctors non_const_ctors_with_sizes
        end
      end
    end
  | Unbox Number (Naked_float, epa) ->
    compute_extra_arg_for_number Naked_float U.Float.unboxer epa
      rewrite_id ~typing_env_at_use arg_being_unboxed
  | Unbox Number (Naked_int32, epa) ->
    compute_extra_arg_for_number Naked_int32 U.Int32.unboxer epa
      rewrite_id ~typing_env_at_use arg_being_unboxed
  | Unbox Number (Naked_int64, epa) ->
    compute_extra_arg_for_number Naked_int64 U.Int64.unboxer epa
      rewrite_id ~typing_env_at_use arg_being_unboxed
  | Unbox Number (Naked_nativeint, epa) ->
    compute_extra_arg_for_number Naked_nativeint U.Nativeint.unboxer epa
      rewrite_id ~typing_env_at_use arg_being_unboxed
  | Unbox Number (Naked_immediate, epa) ->
    compute_extra_arg_for_number Naked_immediate U.Immediate.unboxer epa
      rewrite_id ~typing_env_at_use arg_being_unboxed

and compute_extra_args_for_block ~pass
      rewrite_id ~typing_env_at_use arg_being_unboxed
      tag fields =
  let size = Or_unknown.Known (Targetint.OCaml.of_int (List.length fields)) in
  let bak, invalid_const =
    if Tag.equal tag Tag.double_array_tag then
      P.Block_access_kind.Naked_floats { size; },
      Const.naked_float Numbers.Float_by_bit_pattern.zero
    else
      P.Block_access_kind.Values {
        size;
        tag = Option.get (Tag.Scannable.of_tag tag);
        field_kind = Any_value;
      }, Const.const_zero
  in
  let _, fields =
    List.fold_left_map
      (fun field_nth { epa; decision; } ->
         let unboxer = U.Field.unboxer ~invalid_const bak ~index:field_nth in
         let new_extra_arg =
           extra_arg_of_arg_being_unboxed unboxer
             ~typing_env_at_use arg_being_unboxed
         in
         let epa =
           Extra_param_and_args.update_param_args epa rewrite_id new_extra_arg
         in
         let decision =
           compute_extra_args_for_one_decision_and_use ~pass
             rewrite_id ~typing_env_at_use
             (arg_being_unboxed_of_extra_arg new_extra_arg) decision
         in
         Target_imm.(add one field_nth), { epa; decision; }
      ) Target_imm.zero fields
  in
  Unbox (Unique_tag_and_size { tag; fields; })

and compute_extra_args_for_closure ~pass
      rewrite_id ~typing_env_at_use arg_being_unboxed
      closure_id vars_within_closure =
  let vars_within_closure =
    Var_within_closure.Map.mapi (fun var { epa; decision; } ->
      let unboxer = U.Closure_field.unboxer closure_id var in
      let new_extra_arg =
        extra_arg_of_arg_being_unboxed unboxer
          ~typing_env_at_use arg_being_unboxed
      in
      let epa =
        Extra_param_and_args.update_param_args epa rewrite_id new_extra_arg
      in
      let decision =
        compute_extra_args_for_one_decision_and_use ~pass
          rewrite_id ~typing_env_at_use
          (arg_being_unboxed_of_extra_arg new_extra_arg) decision
      in
      { epa; decision; }
    ) vars_within_closure
  in
  Unbox (Closure_single_entry { closure_id; vars_within_closure; })

and compute_extra_args_for_variant ~pass
      rewrite_id ~typing_env_at_use arg_being_unboxed
      tag constant_constructors fields_by_tag (* unboxing decision *)
      const_ctors non_const_ctors_with_sizes (* type info at the use site *)
  =
  let are_there_constant_constructors =
    match (const_ctors : _ Or_unknown.t) with
    | Unknown -> true
    | Known set -> not (Target_imm.Set.is_empty set)
  in
  let are_there_non_constant_constructors =
    not (Tag.Scannable.Map.is_empty non_const_ctors_with_sizes)
  in
  let constant_constructors =
    if not are_there_constant_constructors then
      extra_args_for_const_ctor_of_variant
        constant_constructors ~typing_env_at_use
        rewrite_id Not_a_constant_constructor
    else if not are_there_non_constant_constructors then
      extra_args_for_const_ctor_of_variant
        constant_constructors ~typing_env_at_use rewrite_id
        (Maybe_constant_constructor
           { arg_being_unboxed; is_int = Simple.untagged_const_true;})
    else begin
      (* CR: one might want to try and use the cse at use to allow
             unboxing when the tag is not know statically but can be
             recovered through the cse. *)
      prevent_current_unboxing ()
    end
  in
  let tag_at_use_site =
    if not are_there_non_constant_constructors then
      Tag.Scannable.zero
    else
      match Tag.Scannable.Map.get_singleton non_const_ctors_with_sizes with
      | None -> prevent_current_unboxing ()
      | Some (tag, _) -> tag
  in
  let tag_extra_arg =
    tag_at_use_site
    |> Tag.Scannable.to_targetint
    |> Targetint.OCaml.of_targetint
    |> Const.untagged_const_int
    |> Simple.const
    |> (fun x -> EPA.Extra_arg.Already_in_scope x)
  in
  let tag =
    Extra_param_and_args.update_param_args tag rewrite_id tag_extra_arg
  in
  let fields_by_tag =
    Tag.Scannable.Map.mapi (fun tag_decision block_fields ->
      let size = List.length block_fields in
      (* See doc/unboxing.md about invalid constants, poison and aliases. *)
      let invalid_const = Const.const_int (Targetint.OCaml.of_int 0xbaba) in
      let bak : Flambda_primitive.Block_access_kind.t =
        Values {
          size = Known (Targetint.OCaml.of_int size);
          tag = tag_decision;
          field_kind = Any_value;
        }
      in
      let new_fields_decisions, _ =
        List.fold_left (fun (new_decisions, field_nth) { epa; decision; } ->
          let new_extra_arg, new_arg_being_unboxed =
            if are_there_non_constant_constructors
            && Tag.Scannable.equal tag_at_use_site tag_decision then begin
              let unboxer =
                U.Field.unboxer ~invalid_const bak ~index:field_nth
              in
              let new_extra_arg =
                extra_arg_of_arg_being_unboxed unboxer
                  ~typing_env_at_use arg_being_unboxed
              in
              let new_arg_being_unboxed =
                arg_being_unboxed_of_extra_arg new_extra_arg
              in
              new_extra_arg, new_arg_being_unboxed
            end else begin
              EPA.Extra_arg.Already_in_scope (Simple.const invalid_const),
              Poison
            end
          in
          let epa =
            Extra_param_and_args.update_param_args epa rewrite_id new_extra_arg
          in
          let decision =
            compute_extra_args_for_one_decision_and_use ~pass
              rewrite_id ~typing_env_at_use
              new_arg_being_unboxed decision
          in
          let new_decisions = { epa; decision; } :: new_decisions in
          (new_decisions, Target_imm.(add one field_nth))
        ) ([], Target_imm.zero) block_fields
      in
      List.rev new_fields_decisions
    ) fields_by_tag
  in
  Unbox (Variant { tag; constant_constructors; fields_by_tag; })

let add_extra_params_and_args extra_params_and_args decision =
  let rec aux extra_params_and_args = function
    | Do_not_unbox _ -> extra_params_and_args
    | Unbox Unique_tag_and_size { tag; fields; } ->
      List.fold_left (fun extra_params_and_args { epa; decision; } ->
        let kind =
          if Tag.equal Tag.double_array_tag tag then
            K.With_subkind.naked_float
          else
            K.With_subkind.any_value
        in
        let extra_param = KP.create epa.param kind in
        let extra_params_and_args =
          EPA.add extra_params_and_args ~extra_param ~extra_args:epa.args
        in
        aux extra_params_and_args decision
      ) extra_params_and_args fields
    | Unbox Closure_single_entry { closure_id = _; vars_within_closure; } ->
      Var_within_closure.Map.fold
        (fun _ { epa; decision; } extra_params_and_args ->
          let extra_param = KP.create epa.param K.With_subkind.any_value in
          let extra_params_and_args =
            EPA.add extra_params_and_args ~extra_param ~extra_args:epa.args
          in
          aux extra_params_and_args decision)
        vars_within_closure extra_params_and_args
    | Unbox Variant { tag; constant_constructors; fields_by_tag; } ->
      let extra_params_and_args =
        Tag.Scannable.Map.fold (fun _ block_fields extra_params_and_args ->
          List.fold_left (fun extra_params_and_args { epa; decision; } ->
            let extra_param = KP.create epa.param K.With_subkind.any_value in
            let extra_params_and_args =
              EPA.add extra_params_and_args ~extra_param ~extra_args:epa.args
            in
            aux extra_params_and_args decision
          ) extra_params_and_args block_fields
        ) fields_by_tag extra_params_and_args
      in
      let extra_params_and_args =
        match constant_constructors with
        | Zero -> extra_params_and_args
        | At_least_one { is_int; ctor = Do_not_unbox _; _ } ->
          let extra_param =
            KP.create is_int.param K.With_subkind.naked_immediate
          in
          EPA.add extra_params_and_args ~extra_param ~extra_args:is_int.args
        | At_least_one { is_int; ctor = Unbox Number (Naked_immediate, ctor); } ->
          let extra_param =
            KP.create is_int.param K.With_subkind.naked_immediate
          in
          let extra_params_and_args =
            EPA.add extra_params_and_args ~extra_param ~extra_args:is_int.args
          in
          let extra_param =
            KP.create ctor.param K.With_subkind.naked_immediate
          in
          EPA.add extra_params_and_args ~extra_param ~extra_args:ctor.args
        | At_least_one {
            ctor = Unbox (
              Unique_tag_and_size _ | Variant _ | Closure_single_entry _
            | Number ((Naked_float | Naked_int32 | Naked_int64
                       | Naked_nativeint), _));
            is_int = _; } ->
          Misc.fatal_errorf
            "Trying to unbox the constant constructor of a variant \
             with a kind other than naked_immediate."
      in
      let extra_param = KP.create tag.param K.With_subkind.naked_immediate in
      EPA.add extra_params_and_args ~extra_param ~extra_args:tag.args
    | Unbox Number (naked_number_kind, epa) ->
      (* CR pchambart: This is seriously too contrived for what it's doing.
         Flambda_kind.With_subkind needs a function for doing that *)
      let boxable_number_kind =
        K.Boxable_number.of_naked_number_kind naked_number_kind
      in
      let kind = K.Boxable_number.to_kind boxable_number_kind in
      let kind_with_subkind =
        K.With_subkind.create kind K.With_subkind.Subkind.Anything
      in
      let extra_param = KP.create epa.param kind_with_subkind in
      EPA.add extra_params_and_args ~extra_param ~extra_args:epa.args
  in
  aux extra_params_and_args decision
