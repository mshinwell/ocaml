(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Int32 = Numbers.Int32
module Int64 = Numbers.Int64

module Make
    (T : Flambda_type0_internal_intf.S)
    (Expr : Expr_intf.S with type t := T.expr)
    (Typing_env_extension : Typing_env_extension_intf.S with module T := T) =
struct
  open T

  let fast_equal (t1 : t) (t2 : t) =
    t1 == t2

  let _equal_mutable_or_immutable equal_contents (mut1 : _ mutable_or_immutable)
        (mut2 : _ mutable_or_immutable) =
    match mut1, mut2 with
    | Immutable contents1, Immutable contents2 ->
      equal_contents contents1 contents2
    | Mutable, Mutable -> true
    | (Immutable _ | Mutable), _ -> false

  let equal_or_alias equal_contents (or_alias1 : _ or_alias)
        (or_alias2 : _ or_alias) =
    match or_alias1, or_alias2 with
    | No_alias contents1, No_alias contents2 ->
      equal_contents contents1 contents2
    | Type export_id1, Type export_id2 -> Export_id.equal export_id1 export_id2
    | Equals simple1, Equals simple2 -> Simple.equal simple1 simple2
    | (No_alias _ | Type _ | Equals _), _ -> false

  let equal_extensibility equal_contents
       (ex1 : _ extensibility) (ex2 : _ extensibility) =
    match ex1, ex2 with
    | Open contents1, Open contents2
    | Exactly contents1, Exactly contents2 -> equal_contents contents1 contents2
    | (Open _ | Exactly _), _ -> false

  let equal_unknown_or_join equal_of_kind_foo (uj1 : _ unknown_or_join)
        (uj2 : _ unknown_or_join) =
    match uj1, uj2 with
    | Unknown, Unknown -> true
    | Join join1, Join join2 ->
      Misc.Stdlib.List.equal equal_of_kind_foo join1 join2
    | Unknown, _
    | Join _, _ -> false

  let equal_or_unknown equal_contents
        (or_unknown1 : _ Or_unknown.t) (or_unknown2 : _ Or_unknown.t) =
    match or_unknown1, or_unknown2 with
    | Unknown, Unknown -> true
    | Known contents1, Known contents2 -> equal_contents contents1 contents2
    | Unknown, Known _
    | Known _, Unknown -> false

  let equal_ty equal_of_kind_foo ty1 ty2 =
    equal_or_alias (equal_unknown_or_join equal_of_kind_foo) ty1 ty2

  let rec equal (({ descr = descr1; } : t) as t1)
        (({ descr = descr2; } : t) as t2) =
    if fast_equal t1 t2 then true
    else
      equal_descr descr1 descr2

  and equal_descr (descr1 : descr) (descr2 : descr) =
    match descr1, descr2 with
    | Value ty_value1, Value ty_value2 ->
      equal_ty_value ty_value1 ty_value2
    | Naked_number (ty_naked_number1, Naked_immediate),
        Naked_number (ty_naked_number2, Naked_immediate) ->
      equal_ty_naked_number ty_naked_number1 ty_naked_number2
    | Naked_number (ty_naked_number1, Naked_float),
        Naked_number (ty_naked_number2, Naked_float) ->
      equal_ty_naked_number ty_naked_number1 ty_naked_number2
    | Naked_number (ty_naked_number1, Naked_int32),
        Naked_number (ty_naked_number2, Naked_int32) ->
      equal_ty_naked_number ty_naked_number1 ty_naked_number2
    | Naked_number (ty_naked_number1, Naked_int64),
        Naked_number (ty_naked_number2, Naked_int64) ->
      equal_ty_naked_number ty_naked_number1 ty_naked_number2
    | Naked_number (ty_naked_number1, Naked_nativeint),
        Naked_number (ty_naked_number2, Naked_nativeint) ->
      equal_ty_naked_number ty_naked_number1 ty_naked_number2
    | Fabricated ty_fabricated1, Fabricated ty_fabricated2 ->
      equal_ty_fabricated ty_fabricated1 ty_fabricated2
    | Value _, _ -> false
    | Naked_number _, _ -> false
    | Fabricated _, _ -> false

  and equal_ty_value ty_value1 ty_value2 =
    equal_ty equal_of_kind_value ty_value1 ty_value2

  and equal_ty_naked_number
        : type a. a ty_naked_number -> a ty_naked_number -> bool =
  fun (ty_naked_number1 : a ty_naked_number)
      (ty_naked_number2 : a ty_naked_number) ->
    equal_or_alias (equal_unknown_or_join equal_of_kind_naked_number)
      ty_naked_number1 ty_naked_number2

  and equal_ty_fabricated ty_fabricated1 ty_fabricated2 =
    equal_ty equal_of_kind_fabricated ty_fabricated1 ty_fabricated2

  and equal_of_kind_value ((v1 : of_kind_value), perm1)
        ((v2 : of_kind_value), perm2) =
    match v1, v2 with
    | Blocks_and_tagged_immediates blocks1,
        Blocks_and_tagged_immediates blocks2 ->
      equal_blocks_and_tagged_immediates perm1 perm2 blocks1 blocks2
    | Boxed_number (Boxed_float ty_naked_number1),
        Boxed_number (Boxed_float ty_naked_number2) ->
      equal_ty_naked_number ty_naked_number1 ty_naked_number2
    | Boxed_number (Boxed_int32 ty_naked_number1),
        Boxed_number (Boxed_int32 ty_naked_number2) ->
      equal_ty_naked_number ty_naked_number1 ty_naked_number2
    | Boxed_number (Boxed_int64 ty_naked_number1),
        Boxed_number (Boxed_int64 ty_naked_number2) ->
      equal_ty_naked_number ty_naked_number1 ty_naked_number2
    | Boxed_number (Boxed_nativeint ty_naked_number1),
        Boxed_number (Boxed_nativeint ty_naked_number2) ->
      equal_ty_naked_number ty_naked_number1 ty_naked_number2
    | Closures closures1, Closures closures2 ->
      equal_closures closures1 closures2
    | String string_set1, String string_set2 ->
      String_info.Set.equal string_set1 string_set2
    | (Blocks_and_tagged_immediates _ | Boxed_number _
        | Closures _ | String _), _ -> false

  and equal_immediate_case perm1 perm2
        ({ env_extension = env_extension1; } : immediate_case)
        ({ env_extension = env_extension2; } : immediate_case) =
    let env_extension1 = T.apply_name_permutation env_extension1 perm1 in
    let env_extension2 = T.apply_name_permutation env_extension2 perm2 in
    equal_env_extension env_extension1 env_extension2

  and equal_blocks perm1 perm2
        ({ known_tags_and_sizes = known_tags_and_sizes1;
           size_at_least_n = size_at_least_n1;
         } : blocks)
        ({ known_tags_and_sizes = known_tags_and_sizes2;
           size_at_least_n = size_at_least_n2;
         } : blocks) =
    let apply_perm_then_equal_parameters params1 params2 =
      let params1 = Parameters.apply_name_permutation perm1 params1 in
      let params2 = Parameters.apply_name_permutation perm2 params2 in
      equal_parameters params1 params2
    in
    Tag_and_size.Map.equal apply_perm_then_equal_parameters
        known_tags_and_sizes1 known_tags_and_sizes2
      &&
        Targetint.OCaml.Map.equal apply_perm_then_equal_parameters
          size_at_least_n1 size_at_least_n2

  and equal_blocks_and_tagged_immediates perm1 perm2
        ({ immediates = immediates1; blocks = blocks1; })
        ({ immediates = immediates2; blocks = blocks2; }) =
    equal_or_unknown (Immediate.Map.equal (equal_immediate_case perm1 perm2))
        immediates1 immediates2
      && equal_or_unknown (equal_blocks perm1 perm2) blocks1 blocks2

  and equal_function_declarations perm1 perm2
        (decl1 : function_declarations)
        (decl2 : function_declarations) =
    match decl1, decl2 with
    | Inlinable decls1, Inlinable decls2 ->
      (* CR-soon mshinwell: It doesn't seem great that these lists are
         ordered. *)
      List.for_all2 (fun
          { closure_origin = closure_origin1;
            continuation_param = continuation_param1;
            exn_continuation_param = exn_continuation_param1;
            is_classic_mode = is_classic_mode1;
            body = _;
            free_names_in_body = _;
            code_id = code_id1;
            stub = stub1;
            dbg = dbg1;
            inline = inline1;
            specialise = specialise1;
            is_a_functor = is_a_functor1;
            invariant_params = invariant_params1;
            size = size1;
            direct_call_surrogate = direct_call_surrogate1;
            my_closure = my_closure1;
          }
          { closure_origin = closure_origin2;
            continuation_param = continuation_param2;
            exn_continuation_param = exn_continuation_param2;
            is_classic_mode = is_classic_mode2;
            body = _;
            free_names_in_body = _;
            code_id = code_id2;
            stub = stub2;
            dbg = dbg2;
            inline = inline2;
            specialise = specialise2;
            is_a_functor = is_a_functor2;
            invariant_params = invariant_params2;
            size = size2;
            direct_call_surrogate = direct_call_surrogate2;
            my_closure = my_closure2;
          } ->
        let module NP = Name_permutation in
        let continuation_param1 =
          Name_permutation.apply_continuation perm1 continuation_param1
        in
        let continuation_param2 =
          Name_permutation.apply_continuation perm2 continuation_param2
        in
        let exn_continuation_param1 =
          Name_permutation.apply_continuation perm1 exn_continuation_param1
        in
        let exn_continuation_param2 =
          Name_permutation.apply_continuation perm2 exn_continuation_param2
        in
        let my_closure1 = Name_permutation.apply_variable perm1 my_closure1 in
        let my_closure2 = Name_permutation.apply_variable perm2 my_closure2 in
        Closure_origin.equal closure_origin1 closure_origin2
          && Continuation.equal continuation_param1 continuation_param2
          && Continuation.equal exn_continuation_param1 exn_continuation_param2
          && Pervasives.compare is_classic_mode1 is_classic_mode2 = 0
          && Code_id.equal code_id1 code_id2
          && Pervasives.compare stub1 stub2 = 0
          && Debuginfo.equal dbg1 dbg2
          && Pervasives.compare inline1 inline2 = 0
          && Pervasives.compare specialise1 specialise2 = 0
          && Pervasives.compare is_a_functor1 is_a_functor2 = 0
          && Variable.Set.equal (Lazy.force invariant_params1)
               (Lazy.force invariant_params2)
          && Misc.Stdlib.Option.equal Numbers.Int.equal
               (Lazy.force size1) (Lazy.force size2)
          && Misc.Stdlib.Option.equal Closure_id.equal
            direct_call_surrogate1 direct_call_surrogate2
          && Variable.equal my_closure1 my_closure2)
        decls1 decls2
    | Non_inlinable ({
        direct_call_surrogate = direct_call_surrogate1;
      }),
      Non_inlinable ({
        direct_call_surrogate = direct_call_surrogate2;
      }) ->
      Misc.Stdlib.Option.equal Closure_id.equal
        direct_call_surrogate1 direct_call_surrogate2
    | Inlinable _, Non_inlinable _
    | Non_inlinable _, Inlinable _ -> false

  and equal_closures_entry
        ({ set_of_closures = set_of_closures1; } : closures_entry)
        ({ set_of_closures = set_of_closures2; } : closures_entry) =
    equal_ty_fabricated set_of_closures1 set_of_closures2

  and equal_closures
        ({ ty = ty1; by_closure_id = by_closure_id1; } : closures)
        ({ ty = ty2; by_closure_id = by_closure_id2; } : closures) =
    equal_dependent_function_type ty1 ty2
      && Closure_id.Map.equal equal_closures_entry by_closure_id1 by_closure_id2

  and equal_of_kind_naked_number : type a b.
    a of_kind_naked_number -> b of_kind_naked_number -> bool =
  fun of_kind_naked_number1 of_kind_naked_number2 ->
    match of_kind_naked_number1, of_kind_naked_number2 with
    | Immediate imms1, Immediate imms2 -> Immediate.Set.equal imms1 imms2
    | Float floats1, Float floats2 ->
      Numbers.Float_by_bit_pattern.Set.equal floats1 floats2
    | Int32 ints1, Int32 ints2 -> Int32.Set.equal ints1 ints2
    | Int64 ints1, Int64 ints2 -> Int64.Set.equal ints1 ints2
    | Nativeint ints1, Nativeint ints2 -> Targetint.Set.equal ints1 ints2
    | Immediate _, _ -> false
    | Float _, _ -> false
    | Int32 _, _ -> false
    | Int64 _, _ -> false
    | Nativeint _, _ -> false

  and equal_discriminant_case
        ({ env_extension = env_extension1; } : discriminant_case)
        ({ env_extension = env_extension2; } : discriminant_case) =
    equal_env_extension env_extension1 env_extension2

  and equal_of_kind_fabricated (of_kind_fabricated1 : of_kind_fabricated)
        (of_kind_fabricated2 : of_kind_fabricated) =
    match of_kind_fabricated1, of_kind_fabricated2 with
    | Discriminant discrs1, Discriminant discrs2 ->
      Discriminant.Map.equal equal_discriminant_case discrs1 discrs2
    | Set_of_closures set1, Set_of_closures set2 ->
      equal_set_of_closures set1 set2
    | Closure closure1, Closure closure2 ->
      equal_closure closure1 closure2
    | Discriminant _, _
    | Set_of_closures _, _
    | Closure _, _ -> false

  and equal_set_of_closures
        ({ closures = closures1; closure_elements = closure_elements1; }
          : set_of_closures)
        ({ closures = closures2; closure_elements = closure_elements2; }
          : set_of_closures) =
    equal_extensibility (Closure_id.Map.equal equal_ty_fabricated)
        closures1 closures2
      && equal_extensibility (Var_within_closure.Map.equal equal_ty_value)
           closure_elements1 closure_elements2

  and equal_closure ({ function_decls = function_decls1; } : closure)
        ({ function_decls = function_decls2; } : closure) =
    equal_function_declarations function_decls1 function_decls2

  and equal_env_extension env_extension1 env_extension2 =
    Typing_env_extension.equal ~equal_type:equal env_extension1 env_extension2

  and equal_parameters
        ({ params = params1; env_extension = env_extension1; } : parameters)
        ({ params = params2; env_extension = env_extension2; } : parameters) =
    Misc.Stdlib.List.equal Kinded_parameter.equal params1 params2
      && equal_env_extension env_extension1 env_extension2

  and equal_dependent_function_type
        ({ params = params1; results = results1; } : dependent_function_type)
        ({ params = params2; results = results2; } : dependent_function_type) =
    equal_parameters params1 params2 && equal_parameters results1 results2
end
