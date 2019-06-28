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

module A = Number_adjuncts
module B = Inlining_cost.Benefit
module E = Simplify_env_and_result.Env
module K = Flambda_kind
module R = Simplify_env_and_result.Result
module S = Simplify_simple
module T = Flambda_type

module Float_by_bit_pattern = Numbers.Float_by_bit_pattern
module Int32 = Numbers.Int32
module Int64 = Numbers.Int64
module Named = Flambda.Named
module Reachable = Flambda.Reachable

let meet_skeleton env r ~original_term ~deconstructing ~skeleton ~result
      ~result_kind : _ Or_bottom.t =
  let env = E.typing_env env in
  let ty = TE.find_exn env deconstructing in
  match T.meet_skeleton env ty ~skeleton ~result ~result_kind with
  | Bottom -> Bottom
  | Ok env_extension -> Ok (original_term, env_extension, r)

let simplify_project_closure env r ~original_term ~closure ~set_of_closures
      ~result =
  meet_skeleton env r ~original_term ~deconstructing:set_of_closures
    ~skeleton:(T.set_of_closures_containing_at_least closure)
    ~result ~result_kind:(K.value ())

let simplify_project_var env r ~original_term ~closure ~closure_element
      ~result : _ Or_bottom.t =
  meet_skeleton env r ~original_term ~deconstructing:closure
    ~skeleton:(T.closure_containing_at_least closure_element)
    ~result ~result_kind:(K.value ())

(*
let simplify_move_within_set_of_closures env r prim ~move_from ~move_to
      ~closures dbg ~result_var:_ =
  let closures, ty = S.simplify_simple env closures in
  let original_term () : Named.t = Prim (Unary (prim, closures), dbg) in
  let invalid r =
    Reachable.invalid (), T.bottom (K.value ()),
      R.map_benefit r (B.remove_primitive (Unary prim))
  in
  let proof = T.prove_closures (E.get_typing_environment env) ty in
  match proof with
  | Proved closures ->
    begin match Closure_id.Map.find move_from closures with
    | exception Not_found -> invalid r
    | { set_of_closures = set_ty; } ->
      let set_ty = T.of_ty_fabricated set_ty in
      let proof =
        T.prove_sets_of_closures (E.get_typing_environment env) set_ty
      in
      begin match proof with
      | Proved (set_of_closures_name, set_of_closures) ->
(*
        let r =
          match set_of_closures_name with
          | None -> r
          | Some set_of_closures_name ->
            (E.type_accessor env
                refine_set_of_closures_type_to_identify_projection)
              env r
              ~set_of_closures_name
              ~result_var
              ~closure_id:move_to
        in
*)
        let closures = T.extensibility_contents set_of_closures.closures in
        begin match Closure_id.Map.find move_to closures with
        | exception Not_found -> invalid r
        | closure_ty ->
          begin match set_of_closures_name with
          | None ->
            Reachable.reachable (original_term ()),
              T.of_ty_fabricated closure_ty, r
          | Some name ->
            let r =
              R.map_benefit
                (R.map_benefit r (B.remove_primitive (Unary prim)))
                (B.add_primitive (Unary (Project_closure move_to)))
            in
            let new_term : Named.t =
              Prim (Unary (Project_closure move_to, Simple.name name), dbg)
            in
            Reachable.reachable new_term, T.of_ty_fabricated closure_ty, r
          end
        end
      | Unknown
      | Invalid -> invalid r
      end
    end
  | Unknown ->
    Reachable.reachable (original_term ()), T.any_value (), r
  | Invalid -> invalid r
*)

let simplify_duplicate_block _env _r _prim _arg _dbg
      ~(kind : Flambda_primitive.duplicate_block_kind)
      ~source_mutability:_ ~destination_mutability:_ =
  ignore kind;
  Named.prim (Unary (prim, arg)) dbg, T.any_value (), r

let initial_env_extension prim ~result =
  match Flambda_primitive.With_fixed_value.create prim with
  | None -> TEE.empty ()
  | Some prim -> TEE.add_cse (TEE.empty ()) (Simple.name result) prim

let pre_simplification env prim dbg ~result arg =
  let typing_env = E.typing_env env in
  let arg, arg_ty = S.simplify_simple typing_env arg in
  let env_extension = initial_env_extension prim ~result in
  let original_term : Named.t = Prim (prim, dbg) in
  typing_env, original_term, arg, arg_ty, env_extension

let simplify_is_int typing_env r env_extension ~original_term prim arg arg_ty
      ~result : _ Or_bottom.t =
  let proof = T.prove_is_tagged_immediate typing_env ty in
  let proved discriminant : _ Or_bottom.t =
    let result_term : Named.t = Simple (Simple.discriminant discriminant) in
    let result_ty = T.this_discriminant discriminant in
    let env_extension = TEE.add_equation env_extension result result_ty in
    let r = R.map_benefit r (B.remove_primitive (Unary prim)) in
    Ok (result_term, env_extension, r)
  in
  match proof with
  | Proved Always_a_tagged_immediate -> proved Discriminant.bool_true
  | Proved Never_a_tagged_immediate -> proved Discriminant.bool_false
  | Unknown ->
    let result_ty =
      T.these_discriminants (Discriminant.Map.of_list [
        Discriminant.bool_false, TEE.empty ();
        Discriminant.bool_true, TEE.empty ();
      ])
    in
    let env_extension = TEE.add_equation env_extension result result_ty in
    Ok (original_term, env_extension, r)
  | Invalid -> Bottom

let post_simplification typing_env r prim (result : _ Or_bottom.t) =
  match result with
  | Bottom ->
    let r = R.map_benefit r (B.remove_primitive (Unary prim)) in
    let term : _ Or_bottom.t = Bottom in
    typing_env, r, term
  | Ok (term, env_extension, r) ->
    let kind = Flambda_primitive.result_kind' prim in
    let typing_env = TE.add_definition typing_env result kind in
    let typing_env = TE.add_env_extension typing_env result env_extension in
    let term : _ Or_bottom.t = Ok term in
    typing_env, r, term

let simplify_get_tag env r prim ~tags_to_sizes block block_ty dbg ~result_var =
  let result_name = Name.var result_var in
  let inferred_tags = T.prove_tags (E.get_typing_environment env) block_ty in
  let possible_tags = Tag.Map.keys tags_to_sizes in
  let invalid r =
    Reachable.invalid (), T.bottom (K.fabricated ()),
      R.map_benefit r (B.remove_primitive (Unary prim))
  in
  let result_var_type ~tags_to_sizes =
    let discriminants_to_env_extension =
      Tag.Map.fold (fun tag size discriminants_to_env_extension ->
          (* CR mshinwell: think about this conversion *)
          let size = Targetint.OCaml.to_int size in
          let block_ty =
            let tag =
              match Tag.Scannable.of_tag tag with
              | Some tag -> tag
              | None ->
                (* CR mshinwell: We need to get our story straight here as
                   to which blocks [Get_tag] may be used on.  Then that should
                   be exactly reflected in the types. *)
                assert false
            in
            T.block_of_unknown_values tag ~size
          in
          let env =
            match block with
            | Const _ | Discriminant _ ->
              (* CR mshinwell: This is kind of silly---it will never be a
                 [Const] or [Discriminant] *)
              T.Typing_env_extension.empty
            | Name block ->
              T.Typing_env_extension.add_equation T.Typing_env_extension.empty
                block block_ty
          in
          let discriminant = Discriminant.of_tag tag in
          Discriminant.Map.add discriminant env discriminants_to_env_extension)
        tags_to_sizes
        Discriminant.Map.empty
    in
    T.these_discriminants discriminants_to_env_extension
  in
  match inferred_tags with
  | Proved (Tags inferred_tags) ->
    let tags = Tag.Set.inter inferred_tags possible_tags in
    let r =
      R.map_benefit r (B.remove_primitive (Unary (Get_tag { tags_to_sizes; })))
    in
    if Tag.Set.is_empty tags then begin
      invalid r
    end else begin
      let tags_to_sizes =
        Tag.Map.filter
          (fun tag _size -> Tag.Set.mem tag inferred_tags)
          tags_to_sizes
      in
      assert (not (Tag.Map.is_empty tags_to_sizes));
      let prim : Flambda_primitive.unary_primitive =
        Get_tag { tags_to_sizes; }
      in
      let term : Named.t = Prim (Unary (prim, block), dbg) in
      let result_var_type = result_var_type ~tags_to_sizes in
      Reachable.reachable term, result_var_type, r
    end
  | Unknown ->
    let prim : Flambda_primitive.unary_primitive =
      Get_tag { tags_to_sizes; }
    in
    let term : Named.t = Prim (Unary (prim, block), dbg) in
    let result_var_type = result_var_type ~tags_to_sizes in
    Reachable.reachable term, result_var_type, r
  | Invalid -> invalid r

module Make_simplify_unbox_number (P : A.Boxable_number_kind) = struct
  let simplify env r prim arg dbg ~result_var =
    let arg, ty = S.simplify_simple env arg in
    let original_term () : Named.t = Prim (Unary (prim, arg), dbg) in
    let proof = P.boxed_prover (E.get_typing_environment env) ty in
    let kind = K.Standard_int_or_float.to_kind P.kind in
    let unknown r =
      Reachable.reachable (original_term ()), T.unknown kind, r
    in
    let invalid r =
      Reachable.invalid (), T.bottom kind,
        R.map_benefit r (B.remove_primitive (Unary prim))
    in
    let r =
      match arg with
      | Const _ | Discriminant _ | Name (Symbol _) -> r
      | Name ((Var _) as boxed_name) ->
        let kind = K.Standard_int_or_float.to_kind P.kind in
        let boxed_ty_refinement =
          P.box (T.alias_type_of kind (Name.var result_var))
        in
        let r =
          let scope_level = E.continuation_scope_level env in
          R.add_or_meet_equation r (Name.var result_var)
            scope_level (T.unknown kind)
        in
        R.add_or_meet_equation r boxed_name
          (E.scope_level_of_name env boxed_name) boxed_ty_refinement
    in
    match proof with
    | Proved unboxed_ty ->
      let unboxed_ty = P.t_of_ty unboxed_ty in
      let proof = P.unboxed_prover (E.get_typing_environment env) unboxed_ty in
      begin match proof with
      | Proved nums ->
        Reachable.reachable (original_term ()), P.these_unboxed nums,
          R.map_benefit r (B.remove_primitive (Unary prim))
      | Unknown ->
        (* In this case, [unboxed_ty] might actually be an alias, meaning
           that we can replace the primitive with a variable.  This will be
           done automagically by the code in [Simplify_named] using
           [reify]. *)
        Reachable.reachable (original_term ()), unboxed_ty, r
      | Invalid -> invalid r
      end
    | Unknown -> unknown r
    | Invalid -> invalid r
end

module Simplify_unbox_number_float = Make_simplify_unbox_number (A.For_floats)
module Simplify_unbox_number_int32 = Make_simplify_unbox_number (A.For_int32s)
module Simplify_unbox_number_int64 = Make_simplify_unbox_number (A.For_int64s)
module Simplify_unbox_number_nativeint =
  Make_simplify_unbox_number (A.For_nativeints)

module Make_simplify_box_number (P : A.Boxable_number_kind) = struct
  let simplify env r prim arg dbg =
    (* CR mshinwell: If [arg] is already a [Const] we shouldn't have to do
       much work... *)
    let arg, ty = S.simplify_simple env arg in
    let original_term () : Named.t = Prim (Unary (prim, arg), dbg) in
    let proof = P.unboxed_prover (E.get_typing_environment env) ty in
    match proof with
    | Proved nums ->
      (* begin match P.Num.Set.get_singleton nums with
      | Some n ->
        let symbol, r = R.new_lifted_constant r (Boxed_float (Const n)) in
        let r = R.map_benefit r (B.remove_primitive (Unary prim)) in
        let named : Named.t = Simple (Simple.name (Name.symbol symbol)) in
        Reachable.reachable named, P.this n, r
      | None -> ...
      end *)
      assert (not (P.Num.Set.is_empty nums));
      Reachable.reachable (original_term ()), P.these_boxed nums, r
    | Unknown ->
      let ty =
        match arg with
        | Const _ | Discriminant _ -> ty
        | Name arg ->
          let kind = K.Standard_int_or_float.to_kind P.kind in
          P.box (T.alias_type_of kind arg)
      in
      Reachable.reachable (original_term ()), ty, r
    | Invalid -> 
      Reachable.invalid (), T.bottom (K.value ()), r
end

module Simplify_box_number_float = Make_simplify_box_number (A.For_floats)
module Simplify_box_number_int32 = Make_simplify_box_number (A.For_int32s)
module Simplify_box_number_int64 = Make_simplify_box_number (A.For_int64s)
module Simplify_box_number_nativeint =
  Make_simplify_box_number (A.For_nativeints)

module Unary_int_arith (I : A.Int_number_kind) = struct
  let simplify env r prim dbg (op : Flambda_primitive.unary_int_arith_op) arg =
    let arg, arg_ty = S.simplify_simple env arg in
    let proof = I.unboxed_prover (E.get_typing_environment env) arg_ty in
    let original_term () : Named.t = Prim (Unary (prim, arg), dbg) in
    let result_unknown () =
      (* One might imagine doing something complicated to [ty] to reflect
         the operation that has happened, but we don't.  As such we cannot
         propagate [ty] and must return "unknown". *)
      Reachable.reachable (original_term ()),
        T.unknown (K.Standard_int_or_float.to_kind I.kind),
        r
    in
    let result_invalid () =
      Reachable.invalid (),
        T.bottom (K.Standard_int_or_float.to_kind I.kind),
        R.map_benefit r (B.remove_primitive (Unary prim))
    in
    match proof with
    | Proved ints ->
      assert (not (I.Num.Set.is_empty ints));
      begin match op with
      | Neg ->
        let possible_results = I.Num.Set.map (fun i -> I.Num.neg i) ints in
        Reachable.reachable (original_term ()),
          I.these_unboxed possible_results, r
      | Swap_byte_endianness ->
        let possible_results =
          I.Num.Set.map (fun i -> I.Num.swap_byte_endianness i) ints
        in
        Reachable.reachable (original_term ()),
          I.these_unboxed possible_results, r
      end
    | Unknown -> result_unknown ()
    | Invalid -> result_invalid ()
end

module Unary_int_arith_tagged_immediate =
  Unary_int_arith (A.For_tagged_immediates)
module Unary_int_arith_naked_int32 = Unary_int_arith (A.For_int32s)
module Unary_int_arith_naked_int64 = Unary_int_arith (A.For_int64s)
module Unary_int_arith_naked_nativeint = Unary_int_arith (A.For_nativeints)

module Make_simplify_int_conv (N : A.Number_kind) = struct
  module F = Float_by_bit_pattern

  let simplify env r prim arg ~(dst : K.Standard_int_or_float.t) dbg =
    let arg, arg_ty = S.simplify_simple env arg in
    if K.Standard_int_or_float.equal N.kind dst then
      if T.is_bottom (E.get_typing_environment env) arg_ty then
        Reachable.invalid (),
          T.bottom (K.Standard_int_or_float.to_kind dst),
          R.map_benefit r (B.remove_primitive (Unary prim))
      else
        Reachable.reachable (Flambda.Named.Simple arg), arg_ty, r
    else
      let proof = N.unboxed_prover (E.get_typing_environment env) arg_ty in
      let original_term () : Named.t = Prim (Unary (prim, arg), dbg) in
      match proof with
      | Proved is ->
        assert (N.Num.Set.cardinal is > 0);
        begin match dst with
        | Tagged_immediate ->
          let imms =
            N.Num.Set.fold (fun i imms ->
                Immediate.Set.add (N.Num.to_tagged_immediate i) imms)
              is
              Immediate.Set.empty
          in
          Reachable.reachable (original_term ()),
            T.these_tagged_immediates imms,
            r
        | Naked_float ->
          let is =
            N.Num.Set.fold (fun i is ->
                F.Set.add (N.Num.to_naked_float i) is)
              is
              F.Set.empty
          in
          Reachable.reachable (original_term ()), T.these_naked_floats is, r
        | Naked_int32 ->
          let is =
            N.Num.Set.fold (fun i is ->
                Int32.Set.add (N.Num.to_naked_int32 i) is)
              is
              Int32.Set.empty
          in
          Reachable.reachable (original_term ()), T.these_naked_int32s is, r
        | Naked_int64 ->
          let is =
            N.Num.Set.fold (fun i is ->
                Int64.Set.add (N.Num.to_naked_int64 i) is)
              is
              Int64.Set.empty
          in
          Reachable.reachable (original_term ()), T.these_naked_int64s is, r
        | Naked_nativeint ->
          let is =
            N.Num.Set.fold (fun i is ->
                Targetint.Set.add (N.Num.to_naked_nativeint i) is)
              is
              Targetint.Set.empty
          in
          Reachable.reachable (original_term ()),
            T.these_naked_nativeints is, r
        end
      | Unknown ->
        Reachable.reachable (original_term ()),
          T.unknown (K.Standard_int_or_float.to_kind dst), r
      | Invalid ->
        Reachable.invalid (), T.bottom (K.Standard_int_or_float.to_kind dst),
          R.map_benefit r (B.remove_primitive (Unary prim))
end

module Simplify_int_conv_tagged_immediate =
  Make_simplify_int_conv (A.For_tagged_immediates)
module Simplify_int_conv_naked_float = Make_simplify_int_conv (A.For_floats)
module Simplify_int_conv_naked_int32 = Make_simplify_int_conv (A.For_int32s)
module Simplify_int_conv_naked_int64 = Make_simplify_int_conv (A.For_int64s)
module Simplify_int_conv_naked_nativeint =
  Make_simplify_int_conv (A.For_nativeints)

let simplify_boolean_not env r prim arg dbg =
  let arg, ty = S.simplify_simple env arg in
  let original_term () : Named.t = Prim (Unary (prim, arg), dbg) in
  let proof = T.prove_tagged_immediate (E.get_typing_environment env) ty in
  let invalid () =
    Reachable.invalid (), T.bottom (K.value ()),
      R.map_benefit r (B.remove_primitive (Unary prim))
  in
  match proof with
  | Proved imms ->
    let imms_ok =
      Immediate.Set.for_all (fun imm ->
          Immediate.equal imm Immediate.zero
            || Immediate.equal imm Immediate.one)
        imms
    in
    if not imms_ok then invalid ()
    else
      let imms =
        Immediate.Set.map (fun imm ->
            if Immediate.equal imm Immediate.zero then
              Immediate.one
            else
              Immediate.zero)
          imms
      in
      Reachable.reachable (original_term ()),
        T.these_tagged_immediates imms, r
  | Unknown ->
    (* CR mshinwell: This should say something like (in the type) "when the
       input is 0, the value is 1" and vice-versa. *)
    Reachable.reachable (original_term ()),
      T.these_tagged_immediates Immediate.all_bools, r
  | Invalid -> invalid ()

let simplify_unary_float_arith_op env r prim
      (op : Flambda_primitive.unary_float_arith_op) arg dbg =
  let module F = Numbers.Float_by_bit_pattern in
  let arg, arg_ty = S.simplify_simple env arg in
  let proof = T.prove_naked_float (E.get_typing_environment env) arg_ty in
  let original_term () : Named.t = Prim (Unary (prim, arg), dbg) in
  let result_unknown () =
    Reachable.reachable (original_term ()), T.unknown (K.naked_float ()), r
  in
  let result_invalid () =
    Reachable.invalid (), T.bottom (K.naked_float ()),
      R.map_benefit r (B.remove_primitive (Unary prim))
  in
  match proof with
  | Proved fs when E.const_float_prop env ->
    assert (not (F.Set.is_empty fs));
    let possible_results =
      match op with
      | Abs -> F.Set.map (fun f -> F.IEEE_semantics.abs f) fs
      | Neg -> F.Set.map (fun f -> F.IEEE_semantics.neg f) fs
    in
    Reachable.reachable (original_term ()),
      T.these_naked_floats possible_results,
      r
  | Proved _ | Unknown -> result_unknown ()
  | Invalid -> result_invalid ()

let simplify_discriminant_of_int env r prim arg dbg ~result_var:_ =
  let arg, arg_ty = S.simplify_simple env arg in
  let proof =
    T.prove_tagged_immediate_as_discriminants (E.get_typing_environment env)
      arg_ty
  in
  let original_term () : Named.t = Prim (Unary (prim, arg), dbg) in
  let result_kind = K.fabricated () in
  let result_invalid () =
    Reachable.invalid (), T.bottom result_kind,
      R.map_benefit r (B.remove_primitive (Unary prim))
  in
(*
  (* XXX Experiment about identity switches *)
  let r =
    match arg with
    | Const _ | Discriminant _ -> r
    | Name arg ->
      let arg_ty =
        T.variant_whose_discriminants_are ~is_int:None
          ~get_tag:(Some (Name.var result_var))
      in
      let r =
        R.add_or_meet_equation r (Name.var result_var)
          (E.continuation_scope_level env)
          (T.unknown (K.fabricated ()))
      in
      let scope_level = E.scope_level_of_name env arg in
      R.add_or_meet_equation r arg scope_level arg_ty
  in
*)
  match proof with
  | Proved (By_discriminant by_discriminant) ->
    Reachable.reachable (original_term ()),
      T.these_discriminants by_discriminant, r
  | Unknown ->
    Reachable.reachable (original_term ()), T.unknown result_kind, r
  | Invalid -> result_invalid ()

let simplify_string_length env r prim arg dbg =
  Reachable.reachable (Named.prim (Unary (prim, arg)) dbg),
    T.any_tagged_immediate (), r

let simplify_array_length env r prim arg ~block_access_kind:_ dbg =
  Reachable.reachable (Named.prim (Unary (prim, arg)) dbg),
    T.any_tagged_immediate (), r

let simplify_bigarray_length env r prim bigarray ~dimension:_ dbg =
  Reachable.reachable (Named.prim (Unary (prim, bigarray)) dbg),
    T.any_tagged_immediate (), r

(* When [Bottom] is received here from a simplifier, we should call
   [B.remove_primitive] *)
let simplify_unary_primitive env r (prim : Flambda_primitive.unary_primitive)
      arg dbg ~result_var : Reachable.t * T.t * R.t =
  match prim with
  | Duplicate_block { kind; source_mutability;
      destination_mutability; } ->
    simplify_duplicate_block env r prim arg dbg ~kind
      ~source_mutability ~destination_mutability
  | Is_int -> simplify_is_int env r prim arg dbg ~result_var
  | Get_tag { tags_to_sizes; } ->
    simplify_get_tag env r prim ~tags_to_sizes ~block:arg dbg ~result_var
  | Discriminant_of_int ->
    simplify_discriminant_of_int env r prim arg dbg ~result_var
  | String_length _string_or_bytes ->
    simplify_string_length env r prim arg dbg
  | Int_as_pointer ->
    let arg, _arg_ty = S.simplify_simple env arg in
    Reachable.reachable (Prim (Unary (prim, arg), dbg)),
      T.unknown (K.value ()), r
  | Opaque_identity ->
    let arg, arg_ty = S.simplify_simple env arg in
    let kind = T.kind arg_ty in
    Reachable.reachable (Prim (Unary (prim, arg), dbg)),
      T.unknown kind, r
  | Int_arith (kind, op) ->
    begin match kind with
    | Tagged_immediate ->
      Unary_int_arith_tagged_immediate.simplify env r prim dbg op arg
    | Naked_int32 ->
      Unary_int_arith_naked_int32.simplify env r prim dbg op arg
    | Naked_int64 ->
      Unary_int_arith_naked_int64.simplify env r prim dbg op arg
    | Naked_nativeint ->
      Unary_int_arith_naked_nativeint.simplify env r prim dbg op arg
    end
  | Num_conv { src = Tagged_immediate; dst; } ->
    Simplify_int_conv_tagged_immediate.simplify env r prim arg ~dst dbg
  | Num_conv { src = Naked_float; dst; } ->
    Simplify_int_conv_naked_float.simplify env r prim arg ~dst dbg
  | Num_conv { src = Naked_int32; dst; } ->
    Simplify_int_conv_naked_int32.simplify env r prim arg ~dst dbg
  | Num_conv { src = Naked_int64; dst; } ->
    Simplify_int_conv_naked_int64.simplify env r prim arg ~dst dbg
  | Num_conv { src = Naked_nativeint; dst; } ->
    Simplify_int_conv_naked_nativeint.simplify env r prim arg ~dst dbg
  | Boolean_not ->
    simplify_boolean_not env r prim arg dbg
  | Float_arith op -> simplify_unary_float_arith_op env r prim op arg dbg
  | Array_length block_access_kind ->
    simplify_array_length env r prim arg ~block_access_kind dbg
  | Bigarray_length { dimension : int; } ->
    simplify_bigarray_length env r prim arg ~dimension dbg
  | Unbox_number Naked_float ->
    Simplify_unbox_number_float.simplify env r prim arg dbg ~result_var
  | Unbox_number Naked_int32 ->
    Simplify_unbox_number_int32.simplify env r prim arg dbg ~result_var
  | Unbox_number Naked_int64 ->
    Simplify_unbox_number_int64.simplify env r prim arg dbg ~result_var
  | Unbox_number Naked_nativeint ->
    Simplify_unbox_number_nativeint.simplify env r prim arg dbg ~result_var
  | Box_number Naked_float ->
    Simplify_box_number_float.simplify env r prim arg dbg
  | Box_number Naked_int32 ->
    Simplify_box_number_int32.simplify env r prim arg dbg
  | Box_number Naked_int64 ->
    Simplify_box_number_int64.simplify env r prim arg dbg
  | Box_number Naked_nativeint ->
    Simplify_box_number_nativeint.simplify env r prim arg dbg
  | Project_closure closure ->
    simplify_project_closure env r prim ~closure ~set_of_closures:arg dbg
      ~result_var
  | Move_within_set_of_closures _ -> Misc.fatal_error "Not yet implemented"
(*
  | Move_within_set_of_closures { move_from; move_to; } ->
    simplify_move_within_set_of_closures env r prim ~move_from ~move_to
      ~closures:arg dbg ~result_var
*)
  | Project_var (closure_id, var_within_closure) ->
    simplify_project_var env r prim ~closure_id ~var_within_closure
      ~closures:arg dbg ~result_var
