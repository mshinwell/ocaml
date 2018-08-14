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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Float = Numbers.Float_by_bit_pattern
module Int32 = Numbers.Int32
module Int64 = Numbers.Int64

(* CR mshinwell: Delete >= 4.08 *)
[@@@ocaml.warning "-60"]
module Flambda_type0_core = struct end
module Flambda_types = struct end
module Typing_env = struct end
module Typing_env_extension = struct end

module Make (W : Typing_world.S) = struct
  open! W

  let fast_equal (t1 : Flambda_types.t) (t2 : Flambda_types.t) =
    t1 == t2

  let equal_or_alias equal_contents env
        (or_alias1 : _ Flambda_types.or_alias)
        (or_alias2 : _ Flambda_types.or_alias) =
    match or_alias1, or_alias2 with
    | No_alias contents1, No_alias contents2 ->
      equal_contents env contents1 contents2
    | Type export_id1, Type export_id2 -> Export_id.equal export_id1 export_id2
    | Equals simple1, Equals simple2 ->
      let simple1 =
        Name_permutation.apply_simple (Type_equality_env.perm_left env)
          simple1
      in
      let simple2 =
        Name_permutation.apply_simple (Type_equality_env.perm_right env)
          simple2
      in
      Simple.equal simple1 simple2
    | (No_alias _ | Type _ | Equals _), _ -> false

  let equal_unknown_or_join equal_of_kind_foo env
        (uj1 : _ Flambda_types.unknown_or_join)
        (uj2 : _ Flambda_types.unknown_or_join) =
    match uj1, uj2 with
    | Unknown, Unknown -> true
    | Join join1, Join join2 ->
      Misc.Stdlib.List.equal (equal_of_kind_foo env) join1 join2
    | Unknown, _
    | Join _, _ -> false

  let equal_ty equal_of_kind_foo env ty1 ty2 =
    equal_or_alias (equal_unknown_or_join equal_of_kind_foo) env ty1 ty2

  let rec equal_with_env env (t1 : Flambda_types.t) (t2 : Flambda_types.t) =
    match t1, t2 with
    | Value ty_value1, Value ty_value2 ->
      equal_ty_value env ty_value1 ty_value2
    | Naked_number (ty_naked_number1, Naked_immediate),
        Naked_number (ty_naked_number2, Naked_immediate) ->
      equal_ty_naked_number env ty_naked_number1 ty_naked_number2
    | Naked_number (ty_naked_number1, Naked_float),
        Naked_number (ty_naked_number2, Naked_float) ->
      equal_ty_naked_number env ty_naked_number1 ty_naked_number2
    | Naked_number (ty_naked_number1, Naked_int32),
        Naked_number (ty_naked_number2, Naked_int32) ->
      equal_ty_naked_number env ty_naked_number1 ty_naked_number2
    | Naked_number (ty_naked_number1, Naked_int64),
        Naked_number (ty_naked_number2, Naked_int64) ->
      equal_ty_naked_number env ty_naked_number1 ty_naked_number2
    | Naked_number (ty_naked_number1, Naked_nativeint),
        Naked_number (ty_naked_number2, Naked_nativeint) ->
      equal_ty_naked_number env ty_naked_number1 ty_naked_number2
    | Fabricated ty_fabricated1, Fabricated ty_fabricated2 ->
      equal_ty_fabricated env ty_fabricated1 ty_fabricated2
    | Value _, _ -> false
    | Naked_number _, _ -> false
    | Fabricated _, _ -> false

  and equal_ty_value env ty_value1 ty_value2 =
    equal_ty equal_of_kind_value env ty_value1 ty_value2

  and equal_ty_naked_number
     : type a.
       Type_equality_env.t
    -> a Flambda_types.ty_naked_number
    -> a Flambda_types.ty_naked_number
    -> bool =
  fun env
      (ty_naked_number1 : a Flambda_types.ty_naked_number)
      (ty_naked_number2 : a Flambda_types.ty_naked_number) ->
    equal_or_alias (equal_unknown_or_join equal_of_kind_naked_number) env
      ty_naked_number1 ty_naked_number2

  and equal_ty_fabricated env ty_fabricated1 ty_fabricated2 =
    equal_ty equal_of_kind_fabricated env ty_fabricated1 ty_fabricated2

  and equal_of_kind_value env ((v1 : Flambda_types.of_kind_value), perm1)
        ((v2 : Flambda_types.of_kind_value), perm2) =
    let env =
      Type_equality_env.compose_name_permutations env
        ~perm_left:perm1 ~perm_right:perm2
    in
    match v1, v2 with
    | Blocks_and_tagged_immediates blocks1,
        Blocks_and_tagged_immediates blocks2 ->
      equal_blocks_and_tagged_immediates env blocks1 blocks2
    | Boxed_number (Boxed_float ty_naked_number1),
        Boxed_number (Boxed_float ty_naked_number2) ->
      equal_ty_naked_number env ty_naked_number1 ty_naked_number2
    | Boxed_number (Boxed_int32 ty_naked_number1),
        Boxed_number (Boxed_int32 ty_naked_number2) ->
      equal_ty_naked_number env ty_naked_number1 ty_naked_number2
    | Boxed_number (Boxed_int64 ty_naked_number1),
        Boxed_number (Boxed_int64 ty_naked_number2) ->
      equal_ty_naked_number env ty_naked_number1 ty_naked_number2
    | Boxed_number (Boxed_nativeint ty_naked_number1),
        Boxed_number (Boxed_nativeint ty_naked_number2) ->
      equal_ty_naked_number env ty_naked_number1 ty_naked_number2
    | Closures { by_closure_id = by_closure_id1; },
        Closures { by_closure_id = by_closure_id2; } ->
      Closures_entry_by_closure_id.equal env by_closure_id1 by_closure_id2
    | String string_set1, String string_set2 ->
      String_info.Set.equal string_set1 string_set2
    | (Blocks_and_tagged_immediates _ | Boxed_number _
        | Closures _ | String _), _ -> false

  and equal_blocks_and_tagged_immediates env
        ({ immediates = immediates1; blocks = blocks1; }
          : Flambda_types.blocks_and_tagged_immediates)
        ({ immediates = immediates2; blocks = blocks2; }
          : Flambda_types.blocks_and_tagged_immediates) =
    Immediates.equal env immediates1 immediates2
      && Blocks.equal env blocks1 blocks2

  and equal_function_declaration _env
        (decl1 : Flambda_types.function_declaration)
        (decl2 : Flambda_types.function_declaration) =
    match decl1, decl2 with
    | Inlinable decl1, Inlinable decl2 ->
      (* CR mshinwell: Add assertions like in the meet/join code? *)
      Code_id.equal decl1.code_id decl2.code_id
    | Non_inlinable, Non_inlinable -> true
    | Inlinable _, Non_inlinable
    | Non_inlinable, Inlinable _ -> false

  and equal_of_kind_naked_number
     : type a b.
       Type_equality_env.t
    -> (a Flambda_types.of_kind_naked_number * Name_permutation.t)
    -> (b Flambda_types.of_kind_naked_number * Name_permutation.t)
    -> bool =
  fun _env (of_kind_naked_number1, _) (of_kind_naked_number2, _) ->
    match of_kind_naked_number1, of_kind_naked_number2 with
    | Immediate imms1, Immediate imms2 -> Immediate.Set.equal imms1 imms2
    | Float floats1, Float floats2 -> Float.Set.equal floats1 floats2
    | Int32 ints1, Int32 ints2 -> Int32.Set.equal ints1 ints2
    | Int64 ints1, Int64 ints2 -> Int64.Set.equal ints1 ints2
    | Nativeint ints1, Nativeint ints2 -> Targetint.Set.equal ints1 ints2
    | Immediate _, _ -> false
    | Float _, _ -> false
    | Int32 _, _ -> false
    | Int64 _, _ -> false
    | Nativeint _, _ -> false

  and equal_of_kind_fabricated env
        ((of_kind_fabricated1 : Flambda_types.of_kind_fabricated), perm1)
        ((of_kind_fabricated2 : Flambda_types.of_kind_fabricated), perm2) =
    let env =
      Type_equality_env.compose_name_permutations env
        ~perm_left:perm1 ~perm_right:perm2
    in
    match of_kind_fabricated1, of_kind_fabricated2 with
    | Discriminants discrs1, Discriminants discrs2 ->
      Discriminants.equal env discrs1 discrs2
    | Set_of_closures { closures = closures1; },
        Set_of_closures { closures = closures2; } ->
      Closure_ids.equal env closures1 closures2
    | (Discriminants _ | Set_of_closures _), _ -> false

  and equal_closures_entry env
        ({ function_decl = function_decl1;
           ty = ty1;
           closure_elements = closure_elements1;
           set_of_closures = set_of_closures1;
         } : Flambda_types.closures_entry)
        ({ function_decl = function_decl2;
           ty = ty2;
           closure_elements = closure_elements2;
           set_of_closures = set_of_closures2;
         } : Flambda_types.closures_entry) =
    equal_function_declaration env function_decl1 function_decl2
      && Function_type.equal env ty1 ty2
      && Closure_elements.equal env closure_elements1 closure_elements2
      && equal_ty_fabricated env set_of_closures1 set_of_closures2

  and equal_set_of_closures_entry env
        ({ by_closure_id = by_closure_id1; }
          : Flambda_types.set_of_closures_entry)
        ({ by_closure_id = by_closure_id2; }
          : Flambda_types.set_of_closures_entry) =
    Types_by_closure_id.equal env by_closure_id1 by_closure_id2

  let equal t1 t2 = equal_with_env Type_equality_env.empty t1 t2
end
