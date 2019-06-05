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

module Float = Numbers.Float_by_bit_pattern
module Int32 = Numbers.Int32
module Int64 = Numbers.Int64

(* CR mshinwell: Remind ourselves why [bound_name] is needed *)

let equal_or_alias ?bound_name equal_unknown_or_join env
      ~force_to_kind ~print_ty
      (or_alias1 : _ Flambda_types.or_alias)
      (or_alias2 : _ Flambda_types.or_alias) =
  let unknown_or_join1, canonical_simple1 =
    Typing_env.resolve_any_toplevel_alias_on_ty0
      (Type_equality_env.typing_env_left env)
      ~force_to_kind
      ~print_ty
      or_alias1
  in
  let unknown_or_join2, canonical_simple2 =
    Typing_env.resolve_any_toplevel_alias_on_ty0
      (Type_equality_env.typing_env_right env)
      ~force_to_kind
      ~print_ty
      or_alias2
  in
  let already_comparing =
    match canonical_simple1, canonical_simple2 with
    | Some simple1, Some simple2 ->
      Type_equality_env.already_comparing env simple1 simple2
    | _, _ -> false
  in
  if already_comparing then true
  else
    let env =
      match canonical_simple1, canonical_simple2 with
      | Some simple1, Some simple2 ->
        Type_equality_env.now_comparing env simple1 simple2
      | _, _ -> env
    in
    let all_aliases1 =
      match canonical_simple1 with
      | None -> Name.Set.empty
      | Some canonical_simple ->
        Typing_env.aliases_of_simple (Type_equality_env.typing_env_left env)
          canonical_simple
    in
    let all_aliases2 =
      match canonical_simple2 with
      | None -> Name.Set.empty
      | Some canonical_simple ->
        Typing_env.aliases_of_simple (Type_equality_env.typing_env_right env)
          canonical_simple
    in
    let all_aliases1 =
      match bound_name with
      | None -> all_aliases1
      | Some bound_name -> Name.Set.remove bound_name all_aliases1
    in
    let all_aliases2 =
      match bound_name with
      | None -> all_aliases2
      | Some bound_name -> Name.Set.remove bound_name all_aliases2
    in
    if not (Name.Set.equal all_aliases1 all_aliases2) then
      false
    else
      equal_unknown_or_join env unknown_or_join1 unknown_or_join2

let equal_unknown_or_join equal_of_kind_foo env
      (uj1 : _ Flambda_types.unknown_or_join)
      (uj2 : _ Flambda_types.unknown_or_join) =
  Or_unknown_or_bottom.equal (equal_of_kind_foo env) uj1 uj2

let equal_ty ?bound_name equal_of_kind_foo env ~force_to_kind
      ~print_ty (ty1 : _ Flambda_types.ty) (ty2 : _ Flambda_types.ty) =
  equal_or_alias ?bound_name (equal_unknown_or_join equal_of_kind_foo)
    env ~force_to_kind ~print_ty ty1 ty2

let rec equal_with_env ?bound_name env
      (t1 : Flambda_types.t) (t2 : Flambda_types.t) =
  match t1, t2 with
  | Value ty_value1, Value ty_value2 ->
    equal_ty_value ?bound_name env
      ~force_to_kind:Flambda_type0_core.force_to_kind_value
      ~print_ty:Type_printers.print_ty_value
      ty_value1 ty_value2
  | Naked_number (ty_naked_number1, Naked_immediate),
      Naked_number (ty_naked_number2, Naked_immediate) ->
    equal_ty_naked_number ?bound_name env
      ~force_to_kind:Flambda_type0_core.force_to_kind_naked_immediate
      ~print_ty:Type_printers.print_ty_naked_immediate
      ty_naked_number1 ty_naked_number2
  | Naked_number (ty_naked_number1, Naked_float),
      Naked_number (ty_naked_number2, Naked_float) ->
    equal_ty_naked_number ?bound_name env
      ~force_to_kind:Flambda_type0_core.force_to_kind_naked_float
      ~print_ty:Type_printers.print_ty_naked_float
      ty_naked_number1 ty_naked_number2
  | Naked_number (ty_naked_number1, Naked_int32),
      Naked_number (ty_naked_number2, Naked_int32) ->
    equal_ty_naked_number ?bound_name env
      ~force_to_kind:Flambda_type0_core.force_to_kind_naked_int32
      ~print_ty:Type_printers.print_ty_naked_int32
      ty_naked_number1 ty_naked_number2
  | Naked_number (ty_naked_number1, Naked_int64),
      Naked_number (ty_naked_number2, Naked_int64) ->
    equal_ty_naked_number ?bound_name env
      ~force_to_kind:Flambda_type0_core.force_to_kind_naked_int64
      ~print_ty:Type_printers.print_ty_naked_int64
      ty_naked_number1 ty_naked_number2
  | Naked_number (ty_naked_number1, Naked_nativeint),
      Naked_number (ty_naked_number2, Naked_nativeint) ->
    equal_ty_naked_number ?bound_name env
      ~force_to_kind:Flambda_type0_core.force_to_kind_naked_nativeint
      ~print_ty:Type_printers.print_ty_naked_nativeint
      ty_naked_number1 ty_naked_number2
  | Fabricated ty_fabricated1, Fabricated ty_fabricated2 ->
    equal_ty_fabricated ?bound_name env
      ~force_to_kind:Flambda_type0_core.force_to_kind_fabricated
      ~print_ty:Type_printers.print_ty_fabricated
      ty_fabricated1 ty_fabricated2
  | Value _, _ -> false
  | Naked_number _, _ -> false
  | Fabricated _, _ -> false

and equal_ty_value ?bound_name env ~force_to_kind ~print_ty
      ty_value1 ty_value2 =
  equal_ty ?bound_name equal_of_kind_value env
    ~force_to_kind ~print_ty ty_value1 ty_value2

and equal_ty_naked_number
   : type a.
     ?bound_name:Name.t
  -> Type_equality_env.t
  -> force_to_kind:(Flambda_types.t -> a Flambda_types.ty_naked_number)
  -> print_ty:(Format.formatter -> a Flambda_types.ty_naked_number -> unit)
  -> a Flambda_types.ty_naked_number
  -> a Flambda_types.ty_naked_number
  -> bool =
fun ?bound_name env ~force_to_kind ~print_ty
    (ty_naked_number1 : a Flambda_types.ty_naked_number)
    (ty_naked_number2 : a Flambda_types.ty_naked_number) ->
  equal_or_alias ?bound_name
    (equal_unknown_or_join equal_of_kind_naked_number)
    env
    ~force_to_kind ~print_ty
    ty_naked_number1 ty_naked_number2

and equal_ty_fabricated ?bound_name env ~force_to_kind ~print_ty
      ty_fabricated1 ty_fabricated2 =
  equal_ty ?bound_name equal_of_kind_fabricated env
    ~force_to_kind ~print_ty ty_fabricated1 ty_fabricated2

and equal_of_kind_value env
      (v1 : Flambda_types.of_kind_value)
      (v2 : Flambda_types.of_kind_value) =
  match v1, v2 with
  | Blocks_and_tagged_immediates blocks1,
      Blocks_and_tagged_immediates blocks2 ->
    equal_blocks_and_tagged_immediates env blocks1 blocks2
  | Boxed_number (Boxed_float ty_naked_number1),
      Boxed_number (Boxed_float ty_naked_number2) ->
    equal_ty_naked_number env
      ~force_to_kind:Flambda_type0_core.force_to_kind_naked_float
      ~print_ty:Type_printers.print_ty_naked_float
      ty_naked_number1 ty_naked_number2
  | Boxed_number (Boxed_int32 ty_naked_number1),
      Boxed_number (Boxed_int32 ty_naked_number2) ->
    equal_ty_naked_number env
      ~force_to_kind:Flambda_type0_core.force_to_kind_naked_int32
      ~print_ty:Type_printers.print_ty_naked_int32
      ty_naked_number1 ty_naked_number2
  | Boxed_number (Boxed_int64 ty_naked_number1),
      Boxed_number (Boxed_int64 ty_naked_number2) ->
    equal_ty_naked_number env
      ~force_to_kind:Flambda_type0_core.force_to_kind_naked_int64
      ~print_ty:Type_printers.print_ty_naked_int64
      ty_naked_number1 ty_naked_number2
  | Boxed_number (Boxed_nativeint ty_naked_number1),
      Boxed_number (Boxed_nativeint ty_naked_number2) ->
    equal_ty_naked_number env
      ~force_to_kind:Flambda_type0_core.force_to_kind_naked_nativeint
      ~print_ty:Type_printers.print_ty_naked_nativeint
      ty_naked_number1 ty_naked_number2
  | Closures { by_closure_id = by_closure_id1; },
      Closures { by_closure_id = by_closure_id2; } ->
    Closures_entry_by_closure_id.equal env
      by_closure_id1 by_closure_id2
  | String string_set1, String string_set2 ->
    String_info.Set.equal string_set1 string_set2
  | (Blocks_and_tagged_immediates _ | Boxed_number _
      | Closures _ | String _), _ ->
    false

and equal_blocks_and_tagged_immediates env
      ({ immediates = immediates1; blocks = blocks1; }
        : Flambda_types.blocks_and_tagged_immediates)
      ({ immediates = immediates2; blocks = blocks2; }
        : Flambda_types.blocks_and_tagged_immediates) =
  Or_unknown.equal (Immediates.equal env) immediates1 immediates2
    && Or_unknown.equal (Blocks.equal env) blocks1 blocks2

and equal_function_declaration _env
      (decl1 : Flambda_types.function_declaration)
      (decl2 : Flambda_types.function_declaration) =
  match decl1, decl2 with
  | Inlinable decl1, Inlinable decl2 ->
    let module TFD = Term_language_function_declaration in
    let code_id1 = TFD.code_id decl1.function_decl in
    let code_id2 = TFD.code_id decl2.function_decl in
    Code_id.equal code_id1 code_id2
  | Non_inlinable {
      param_arity = param_arity1;
      result_arity = result_arity1;
    }, Non_inlinable {
      param_arity = param_arity2;
      result_arity = result_arity2;
    } -> 
    Flambda_arity.equal param_arity1 param_arity2
      && Flambda_arity.equal result_arity1 result_arity2
  | Inlinable _, Non_inlinable _
  | Non_inlinable _, Inlinable _ -> false

and equal_of_kind_naked_number
   : type a b.
     Type_equality_env.t
  -> (a Flambda_types.of_kind_naked_number)
  -> (b Flambda_types.of_kind_naked_number)
  -> bool =
fun _env of_kind_naked_number1 of_kind_naked_number2 ->
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
      (of_kind_fabricated1 : Flambda_types.of_kind_fabricated)
      (of_kind_fabricated2 : Flambda_types.of_kind_fabricated) =
  match of_kind_fabricated1, of_kind_fabricated2 with
  | Discriminants discrs1, Discriminants discrs2 ->
    Discriminants.equal env discrs1 discrs2
  | Set_of_closures { closures = closures1; },
      Set_of_closures { closures = closures2; } ->
    Closure_ids.equal env closures1 closures2
  | (Discriminants _ | Set_of_closures _), _ ->
    false

and equal_closures_entry env
      ({ function_decl = function_decl1;
         closure_elements = closure_elements1;
         set_of_closures = set_of_closures1;
       } : Flambda_types.closures_entry)
      ({ function_decl = function_decl2;
         closure_elements = closure_elements2;
         set_of_closures = set_of_closures2;
       } : Flambda_types.closures_entry) =
  Or_unknown.equal (equal_function_declaration env)
    function_decl1 function_decl2
  && Closure_elements.equal env closure_elements1 closure_elements2
  && equal_ty_fabricated env
    ~force_to_kind:Flambda_type0_core.force_to_kind_fabricated
    ~print_ty:Type_printers.print_ty_fabricated
    set_of_closures1 set_of_closures2

and equal_set_of_closures_entry env
      ({ by_closure_id = by_closure_id1; }
        : Flambda_types.set_of_closures_entry)
      ({ by_closure_id = by_closure_id2; }
        : Flambda_types.set_of_closures_entry) =
  Types_by_closure_id.equal env by_closure_id1 by_closure_id2

let equal ~(bound_name : Name.t option)
      typing_env_left typing_env_right t1 t2 =
  let env = Type_equality_env.create ~typing_env_left ~typing_env_right in
  equal_with_env ?bound_name env t1 t2
