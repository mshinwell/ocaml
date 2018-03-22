(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module F0 = Flambda0
module K = Flambda_kind

(* module Expr = F0.Expr *)
module Named = F0.Named

module Float_by_bit_pattern = Numbers.Float_by_bit_pattern
module Int32 = Numbers.Int32
module Int64 = Numbers.Int64

include F0.Flambda_type

let bottom_types_from_arity t =
  List.map (fun kind -> bottom kind) t

let unknown_types_from_arity t =
  List.map (fun kind -> unknown kind) t

let unknown_like_array t_array =
  Array.map (fun t -> unknown_like t) t_array

let rename_variables t ~f:_ = t
(* XXX need to fix [Flambda_type0.clean]
  clean t (fun var -> Available_different_name (f var))
*)

let unit () =
  this_tagged_immediate Immediate.zero

let unit_bottom () =
  bottom (K.value ())

let this_tagged_immediate_named n : Named.t * t =
  Simple (Simple.const (Tagged_immediate n)), this_tagged_immediate n

let this_tagged_bool_named b : Named.t * t =
  let imm =
    if b then Immediate.bool_true
    else Immediate.bool_false
  in
  Simple (Simple.const (Tagged_immediate imm)), this_tagged_immediate imm

let this_untagged_immediate_named n : Named.t * t =
  Simple (Simple.const (Untagged_immediate n)), this_naked_immediate n

let this_naked_float_named f : Named.t * t =
  Simple (Simple.const (Naked_float f)), this_naked_float f

let this_naked_int32_named n : Named.t * t =
  Simple (Simple.const (Naked_int32 n)), this_naked_int32 n

let this_naked_int64_named n : Named.t * t =
  Simple (Simple.const (Naked_int64 n)), this_naked_int64 n

let this_naked_nativeint_named n : Named.t * t =
  Simple (Simple.const (Naked_nativeint n)), this_naked_nativeint n

let equal_mutable_or_immutable equal_contents (mut1 : _ mutable_or_immutable)
      (mut2 : _ mutable_or_immutable) =
  match mut1, mut2 with
  | Immutable contents1, Immutable contents2 ->
    equal_contents contents1 contents2
  | Mutable, Mutable -> true
  | (Immutable _ | Mutable), _ -> false

let equal_or_alias equal_contents (or_alias1 : _ or_alias)
      (or_alias2 : _ or_alias) =
  match or_alias1, or_alias2 with
  | No_alias contents1, No_alias contents2 -> equal_contents contents1 contents2
  | Type export_id1, Type export_id2 -> Export_id.equal export_id1 export_id2
  | Equals name1, Equals name2 -> Name.equal name1 name2
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
      (or_unknown1 : _ or_unknown) (or_unknown2 : _ or_unknown) =
  match or_unknown1, or_unknown2 with
  | Unknown, Unknown -> true
  | Known contents1, Known contents2 -> equal_contents contents1 contents2
  | Unknown, Known _
  | Known _, Unknown -> false

let equal_ty equal_of_kind_foo ty1 ty2 =
  equal_or_alias (equal_unknown_or_join equal_of_kind_foo) ty1 ty2

let rec equal (({ descr = descr1; phantom = phantom1; } : t) as t1)
      (({ descr = descr2; phantom = phantom2; } : t) as t2) =
  if t1 == t2 then true
  else
    equal_descr descr1 descr2
      && Misc.Stdlib.Option.equal Flambda_kind.Phantom_kind.equal_occurrences
        phantom1 phantom2

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

and equal_of_kind_value (v1 : of_kind_value) (v2 : of_kind_value) =
  match v1, v2 with
  | Blocks_and_tagged_immediates blocks1,
      Blocks_and_tagged_immediates blocks2 ->
    equal_blocks_and_tagged_immediates blocks1 blocks2
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

and equal_immediate_case ({ equations = equations1; } : immediate_case)
      ({ equations = equations2; } : immediate_case) =
  equal_equations equations1 equations2

and equal_singleton_block
      ({ equations = equations1; fields = fields1; } : singleton_block)
      ({ equations = equations2; fields = fields2; } : singleton_block)
      =
  equal_equations equations1 equations2
    && Misc.Stdlib.Array.equal (equal_mutable_or_immutable equal)
         fields1 fields2

and equal_block_cases (Join { by_length = by_length1; })
      (Join { by_length = by_length2; }) =
  Targetint.OCaml.Map.equal equal_singleton_block by_length1 by_length2

and equal_blocks_and_tagged_immediates
      ({ immediates = immediates1; blocks = blocks1;
         is_int = is_int1; get_tag = get_tag1; })
      ({ immediates = immediates2; blocks = blocks2;
         is_int = is_int2; get_tag = get_tag2; }) =
  equal_or_unknown (Immediate.Map.equal equal_immediate_case)
      immediates1 immediates2
    && equal_or_unknown (Tag.Map.equal equal_block_cases)
         blocks1 blocks2
    && Misc.Stdlib.Option.equal Name.equal is_int1 is_int2
    && Misc.Stdlib.Option.equal Name.equal get_tag1 get_tag2

and equal_function_declarations
      (decl1 : function_declarations)
      (decl2 : function_declarations) =
  match decl1, decl2 with
  | Inlinable {
      closure_origin = closure_origin1;
      continuation_param = continuation_param1;
      exn_continuation_param = exn_continuation_param1;
      is_classic_mode = is_classic_mode1;
      params = params1;
      code_id = code_id1;
      body = _;
      free_names_in_body = _;
      result = result1;
      result_equations = result_equations1;
      stub = stub1;
      dbg = dbg1;
      inline = inline1;
      specialise = specialise1;
      is_a_functor = is_a_functor1;
      invariant_params = invariant_params1;
      size = size1;
      direct_call_surrogate = direct_call_surrogate1;
      my_closure = my_closure1;
    },
    Inlinable {
      closure_origin = closure_origin2;
      continuation_param = continuation_param2;
      exn_continuation_param = exn_continuation_param2;
      is_classic_mode = is_classic_mode2;
      params = params2;
      code_id = code_id2;
      body = _;
      free_names_in_body = _;
      result = result2;
      result_equations = result_equations2;
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
    Closure_origin.equal closure_origin1 closure_origin2
      && Continuation.equal continuation_param1 continuation_param2
      && Continuation.equal exn_continuation_param1 exn_continuation_param2
      && Pervasives.compare is_classic_mode1 is_classic_mode2 = 0
      && Misc.Stdlib.List.equal (fun (param1, t1) (param2, t2) ->
          Parameter.equal param1 param2 && equal t1 t2)
        params1 params2
      && Code_id.equal code_id1 code_id2
      && Misc.Stdlib.List.equal equal result1 result2
      && equal_equations result_equations1 result_equations2
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
      && Variable.equal my_closure1 my_closure2
  | Non_inlinable None, Non_inlinable None -> true
  | Non_inlinable None, Non_inlinable (Some _)
  | Non_inlinable (Some _), Non_inlinable None -> false
  | Non_inlinable (Some {
      params = params1;
      result = result1;
      result_equations = result_equations1;
      direct_call_surrogate = direct_call_surrogate1;
    }),
    Non_inlinable (Some {
      params = params2;
      result = result2;
      result_equations = result_equations2;
      direct_call_surrogate = direct_call_surrogate2;
    }) ->
    Misc.Stdlib.List.equal (fun param1 param2 -> equal param1 param2)
        params1 params2
      && Misc.Stdlib.List.equal equal result1 result2
      && equal_equations result_equations1 result_equations2
      && Misc.Stdlib.Option.equal Closure_id.equal
        direct_call_surrogate1 direct_call_surrogate2
  | Inlinable _, Non_inlinable _
  | Non_inlinable _, Inlinable _ -> false

and equal_closures_entry
      ({ set_of_closures = set_of_closures1; } : closures_entry)
      ({ set_of_closures = set_of_closures2; } : closures_entry) =
  equal_ty_fabricated set_of_closures1 set_of_closures2

and equal_closures closures1 closures2 =
  Closure_id.Map.equal equal_closures_entry closures1 closures2

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

and equal_discriminant_case ({ equations = equations1; } : discriminant_case)
      ({ equations = equations2; } : discriminant_case) =
  equal_equations equations1 equations2

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

and equal_equations equations1 equations2 =
  Equations.equal ~equal_type:equal equations1 equations2

let strictly_more_precise (t_env, t) ~than:(than_env, than) =
  (* The [bias_towards] choice is arbitrary here. *)
  let _env, meet =
    let output_env =
      Typing_environment0.create_using_resolver_from t_env
    in
    meet ~output_env ~bias_towards:(t_env, t) (than_env, than)
  in
  not (equal than meet)

module Simplified_type : sig
  (* Simplified types omit the following at top level:
     - alias information;
     - joins between incompatible types (these turn into "Unknown").
  *)
  type t = private {
    descr : descr;
    phantom : Flambda_kind.Phantom_kind.occurrences option;
  }

  and descr = private
    | Value of ty_value
    | Naked_number :
        'kind ty_naked_number * 'kind Flambda_kind.Naked_number.t -> descr
    | Fabricated of ty_fabricated

  and ty_value = of_kind_value ty
  and 'a ty_naked_number = 'a of_kind_naked_number ty
  and ty_fabricated = of_kind_fabricated ty

  and 'a ty = private
    | Unknown
    | Ok of 'a
    | Bottom

  (* Create a simple type from a type.  If the type has an alias at its
     top level stating that it is the type of some named value, that alias
     is (recursively) expanded, and the final ("canonical") name
     returned. *)
  val create : (flambda_type -> t * (Name.t option)) type_accessor

  val is_unknown : t -> bool
  val is_bottom : t -> bool

  val is_phantom : t -> bool
  val check_not_phantom : t -> string -> unit
end = struct
  type 'a normal_ty = 'a ty

  type t = {
    descr : descr;
    phantom : Flambda_kind.Phantom_kind.occurrences option;
  }

  and descr =
    | Value of ty_value
    | Naked_number :
        'kind ty_naked_number * 'kind Flambda_kind.Naked_number.t -> descr
    | Fabricated of ty_fabricated

  and ty_value = of_kind_value ty
  and 'a ty_naked_number = 'a of_kind_naked_number ty
  and ty_fabricated = of_kind_fabricated ty

  and 'a ty =
    | Unknown
    | Ok of 'a
    | Bottom

  let is_unknown t =
    match t.descr with
    | Value Unknown -> true
    | Naked_number (Unknown, _) -> true
    | Fabricated Unknown -> true
    | _ -> false

  let is_bottom t =
    match t.descr with
    | Value Bottom -> true
    | Naked_number (Bottom, _) -> true
    | Fabricated Bottom -> true
    | _ -> false

  let ty_from_ty (ty : _ normal_ty) : _ ty =
    match ty with
    | Type _ | Equals _ -> Unknown
    | No_alias unknown_or_join ->
      match unknown_or_join with
      | Unknown -> Unknown
      | Join [] -> Bottom
      | Join [of_kind_foo] -> Ok of_kind_foo
      | Join _ -> Unknown

  let create env (t : flambda_type) : t * (Name.t option) =
    let t, canonical_name = resolve_aliases (env, t) in
    let (descr : descr) =
      match t.descr with
      | Value ty_value ->
        let ty_value : ty_value = ty_from_ty ty_value in
        Value ty_value
      | Naked_number (ty_naked_number, kind) ->
        let ty_naked_number : _ ty_naked_number = ty_from_ty ty_naked_number in
        Naked_number (ty_naked_number, kind)
      | Fabricated ty_fabricated ->
        let ty_fabricated : ty_fabricated = ty_from_ty ty_fabricated in
        Fabricated ty_fabricated
    in
    { descr;
      phantom = t.phantom;
    }, canonical_name

  let is_phantom t =
    match t.phantom with
    | None -> false
    | Some _ -> true

  let check_not_phantom (t : t) reason =
    match t.phantom with
    | None -> ()
    | Some _ ->
      Misc.fatal_errorf "Simplified type given to [%s] cannot be phantom"
        reason
end

let is_bottom env t =
  let simplified, _canonical_name = Simplified_type.create env t in
  Simplified_type.is_bottom simplified

let is_unknown env t =
  let simplified, _canonical_name = Simplified_type.create env t in
  Simplified_type.is_unknown simplified

let is_known env t =
  not (is_unknown env t)

let is_useful env t =
  let simplified, _canonical_name = Simplified_type.create env t in
  (not (Simplified_type.is_unknown simplified))
    && (not (Simplified_type.is_bottom simplified))

let all_not_useful env ts =
  List.for_all (fun t -> not (is_useful env t)) ts

type 'a proof =
  | Proved of 'a
  | Unknown
  | Invalid

let unknown_proof () = Unknown

let prove_naked_float env t
      : Numbers.Float_by_bit_pattern.Set.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a naked \
        float: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create env t in
  Simplified_type.check_not_phantom simplified "prove_naked_float";
  match simplified.descr with
  | Simplified_type.Naked_number (ty, K.Naked_number.Naked_float) ->
    begin match ty with
    | Unknown -> Unknown
    | Bottom -> Invalid
    | Ok (Float fs) -> Proved fs
    | Ok _ ->
      (* CR mshinwell: Find out why this case is still possible *)
      wrong_kind ()
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Value _
  | Fabricated _ -> wrong_kind ()

let prove_naked_int32 env t : Int32.Set.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a naked \
        int32: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create env t in
  Simplified_type.check_not_phantom simplified "prove_naked_int32";
  match simplified.descr with
  | Simplified_type.Naked_number (ty, K.Naked_number.Naked_int32) ->
    begin match ty with
    | Unknown -> Unknown
    | Bottom -> Invalid
    | Ok (Int32 is) -> Proved is
    | Ok _ -> wrong_kind ()
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Value _
  | Fabricated _ -> wrong_kind ()

let prove_naked_int64 env t : Int64.Set.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a naked \
        int64: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create env t in
  Simplified_type.check_not_phantom simplified "prove_naked_int64";
  match simplified.descr with
  | Simplified_type.Naked_number (ty, K.Naked_number.Naked_int64) ->
    begin match ty with
    | Unknown -> Unknown
    | Bottom -> Invalid
    | Ok (Int64 is) -> Proved is
    | Ok _ -> wrong_kind ()
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Value _
  | Fabricated _ -> wrong_kind ()

let prove_naked_nativeint env t : Targetint.Set.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a naked \
        nativeint: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create env t in
  Simplified_type.check_not_phantom simplified "prove_naked_nativeint";
  match simplified.descr with
  | Simplified_type.Naked_number (ty, K.Naked_number.Naked_nativeint) ->
    begin match ty with
    | Unknown -> Unknown
    | Bottom -> Invalid
    | Ok (Nativeint is) -> Proved is
    | Ok _ -> wrong_kind ()
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Value _
  | Fabricated _ -> wrong_kind ()

let prove_unique_naked_float env t : _ proof =
  match prove_naked_float env t with
  | Proved fs ->
    begin match Float_by_bit_pattern.Set.get_singleton fs with
    | Some f -> Proved f
    | None -> Unknown
    end
  | Unknown -> Unknown
  | Invalid -> Invalid

let prove_unique_naked_int32 env t : _ proof =
  match prove_naked_int32 env t with
  | Proved is ->
    begin match Int32.Set.get_singleton is with
    | Some f -> Proved f
    | None -> Unknown
    end
  | Unknown -> Unknown
  | Invalid -> Invalid

let prove_unique_naked_int64 env t : _ proof =
  match prove_naked_int64 env t with
  | Proved is ->
    begin match Int64.Set.get_singleton is with
    | Some f -> Proved f
    | None -> Unknown
    end
  | Unknown -> Unknown
  | Invalid -> Invalid

let prove_unique_naked_nativeint env t : _ proof =
  match prove_naked_nativeint env t with
  | Proved is ->
    begin match Targetint.Set.get_singleton is with
    | Some f -> Proved f
    | None -> Unknown
    end
  | Unknown -> Unknown
  | Invalid -> Invalid

let prove_closure env t : _ proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a closure: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create env t in
  Simplified_type.check_not_phantom simplified "prove_closure";
  match simplified.descr with
  | Fabricated ty_fabricated ->
    begin match ty_fabricated with
    | Unknown -> Unknown
    | Bottom -> Invalid
    | Ok (Closure closure) -> Proved closure
    | Ok _ -> Invalid
    end
  | Value _ -> wrong_kind ()
  | Simplified_type.Naked_number _ -> wrong_kind ()

type reification_result =
  | Term of Simple.t * t
  | Lift of Flambda_static0.Static_part.t
  | Cannot_reify
  | Invalid

let reify env ~allow_free_variables t : reification_result =
  let t, canonical_name = resolve_aliases (env, t) in
(*
Format.eprintf "CN is %a\n%!" (Misc.Stdlib.Option.print Name.print)
  canonical_name;
*)
  let can_lift =
    Name.Set.for_all (fun (name : Name.t) ->
        match name with
        | Var _ -> false
        | Symbol _ -> true)
      (Name_occurrences.everything (free_names t))
  in
  let simplified, _canonical_name = Simplified_type.create env t in
  if Simplified_type.is_bottom simplified then
    Invalid
  else if Simplified_type.is_phantom simplified then
    Cannot_reify
  else
    let try_name () : reification_result =
      match canonical_name with
      | None -> Cannot_reify
      | Some name when Typing_environment0.is_existential env name ->
        Cannot_reify
      | Some name ->
        match name with
        | Var _ when not allow_free_variables -> Cannot_reify
        | Var _ | Symbol _ ->
          (* This is the only case where we return [Term] with a term that
             cannot be produced just from the type.  As such, we may wish to
             make the type more precise later, so we return an alias type rather
             than [t]. *)
          let kind = kind t in
          let t = alias_type_of kind name in
          Term (Simple.name name, t)
    in
    match simplified.descr with
    | Value ty_value ->
      begin match ty_value with
      | Unknown -> try_name ()
      | Bottom -> Invalid
      | Ok (Blocks_and_tagged_immediates blocks_imms) ->
        begin match blocks_imms.blocks with
        | Unknown -> try_name ()
        | Known blocks ->
          if not (Tag.Map.is_empty blocks) then try_name ()
          else
            begin match blocks_imms.immediates with
            | Unknown -> try_name ()
            | Known imms ->
              begin match Immediate.Map.get_singleton imms with
              | Some (imm, _) -> Term (Simple.const (Tagged_immediate imm), t)
              | None -> try_name ()
              end
            end
        end
      | Ok (Boxed_number (Boxed_float ty_naked_number)) ->
        begin match canonical_name with
        | Some ((Symbol _) as name) ->
          let kind = K.value () in
          let t = alias_type_of kind name in
          Term (Simple.name name, t)
        | Some (Var _) | None ->
          if not can_lift then try_name ()
          else
            let contents =
              of_ty_naked_number ty_naked_number K.Naked_number.Naked_float
            in
            match prove_unique_naked_float env contents with
            | Proved f -> Lift (Boxed_float (Const f))
            | Unknown -> try_name ()
            | Invalid -> try_name ()
        end
      (* CR mshinwell: Factor out boxed number cases *)
      | Ok (Boxed_number (Boxed_int32 ty_naked_number)) ->
        begin match canonical_name with
        | Some ((Symbol _) as name) ->
          let kind = K.value () in
          let t = alias_type_of kind name in
          Term (Simple.name name, t)
        | Some (Var _) | None ->
          if not can_lift then try_name ()
          else
            let contents =
              of_ty_naked_number ty_naked_number K.Naked_number.Naked_int32
            in
            match prove_unique_naked_int32 env contents with
            | Proved f -> Lift (Boxed_int32 (Const f))
            | Unknown -> try_name ()
            | Invalid -> try_name ()
        end
      | Ok (Boxed_number (Boxed_int64 ty_naked_number)) ->
        begin match canonical_name with
        | Some ((Symbol _) as name) ->
          let kind = K.value () in
          let t = alias_type_of kind name in
          Term (Simple.name name, t)
        | Some (Var _) | None ->
          if not can_lift then try_name ()
          else
            let contents =
              of_ty_naked_number ty_naked_number K.Naked_number.Naked_int64
            in
            match prove_unique_naked_int64 env contents with
            | Proved f -> Lift (Boxed_int64 (Const f))
            | Unknown -> try_name ()
            | Invalid -> try_name ()
        end
      | Ok (Boxed_number (Boxed_nativeint ty_naked_number)) ->
        begin match canonical_name with
        | Some ((Symbol _) as name) ->
          let kind = K.value () in
          let t = alias_type_of kind name in
          Term (Simple.name name, t)
        | Some (Var _) | None ->
          if not can_lift then try_name ()
          else
            let contents =
              of_ty_naked_number ty_naked_number K.Naked_number.Naked_nativeint
            in
            match prove_unique_naked_nativeint env contents with
            | Proved f -> Lift (Boxed_nativeint (Const f))
            | Unknown -> try_name ()
            | Invalid -> try_name ()
        end
      | Ok (Closures _ | String _) -> try_name ()
      end
    | Simplified_type.Naked_number (ty_naked_number, _) ->
      begin match ty_naked_number with
      | Unknown -> try_name ()
      | Bottom -> Invalid
      | Ok (Immediate imms) ->
        begin match Immediate.Set.get_singleton imms with
        | Some imm -> Term (Simple.const (Untagged_immediate imm), t)
        | None -> try_name ()
        end
      | Ok (Float fs) ->
        begin match Float_by_bit_pattern.Set.get_singleton fs with
        | Some f -> Term (Simple.const (Naked_float f), t)
        | None -> try_name ()
        end
      | Ok (Int32 is) ->
        begin match Int32.Set.get_singleton is with
        | Some i -> Term (Simple.const (Naked_int32 i), t)
        | None -> try_name ()
        end
      | Ok (Int64 is) ->
        begin match Int64.Set.get_singleton is with
        | Some i -> Term (Simple.const (Naked_int64 i), t)
        | None -> try_name ()
        end
      | Ok (Nativeint is) ->
        begin match Targetint.Set.get_singleton is with
        | Some i -> Term (Simple.const (Naked_nativeint i), t)
        | None -> try_name ()
        end
      end
    | Fabricated (Ok (Set_of_closures set_of_closures)) ->
      begin match canonical_name with
      | Some ((Symbol _) as name) ->
        let kind = K.fabricated () in
        let t = alias_type_of kind name in
        Term (Simple.name name, t)
      | Some (Var _) | None ->
        if not can_lift then try_name ()
        else
          begin match set_of_closures.closures,
              set_of_closures.closure_elements
          with
          | Open _, _ | _, Open _ -> try_name ()
          | Exactly closures, Exactly closure_elements ->
            (* The following assertion holds since [can_lift] is [true]. *)
            assert (Var_within_closure.Map.is_empty closure_elements);
            let cannot_lift = ref false in
            let closures =
              Closure_id.Map.filter_map closures
                ~f:(fun _closure_id ty_fabricated ->
                  let t = of_ty_fabricated ty_fabricated in
                  match prove_closure env t with
                  | Proved closure -> Some closure.function_decls
                  | Unknown | Invalid ->
                    cannot_lift := true;
                    None)
            in
            let funs =
              Closure_id.Map.filter_map closures
                ~f:(fun _closure_id (decls : function_declarations) ->
                  match decls with
                  | Inlinable decl ->
                    let params =
                      List.map (fun (param, ty) ->
                          Flambda0.Typed_parameter.create param ty)
                        decl.params
                    in
                    let return_arity = List.map (fun t -> kind t) decl.result in
                    (* CR mshinwell: Put this conversion in a function
                      somewhere *)
                    let inline : Flambda0.inline_attribute =
                      match decl.inline with
                      | Always_inline -> Always_inline
                      | Never_inline -> Never_inline
                      | Unroll n -> Unroll n
                      | Default_inline -> Default_inline
                    in
                    let specialise : Flambda0.specialise_attribute =
                      match decl.specialise with
                      | Always_specialise -> Always_specialise
                      | Never_specialise -> Never_specialise
                      | Default_specialise -> Default_specialise
                    in
                    let decl =
                      Flambda0.Function_declaration.create
                        ~params
                        ~continuation_param:decl.continuation_param
                        ~exn_continuation_param:decl.exn_continuation_param
                        ~return_arity
                        ~my_closure:decl.my_closure
                        ~body:decl.body
                        ~stub:decl.stub
                        ~dbg:decl.dbg
                        ~inline
                        ~specialise
                        ~is_a_functor:decl.is_a_functor
                        ~closure_origin:decl.closure_origin
                    in
                    Some decl
                  | Non_inlinable _ ->
                    cannot_lift := true;
                    None)
            in
            if !cannot_lift then try_name ()
            else
              let function_decls = Flambda0.Function_declarations.create ~funs in
              let set_of_closures =
                Flambda0.Set_of_closures.create ~function_decls
                  ~in_closure:Var_within_closure.Map.empty
                  ~direct_call_surrogates:Closure_id.Map.empty (* XXX *)
              in
              let static_part =
                Flambda_static0.Static_part.Set_of_closures set_of_closures
              in
              Lift static_part
          end
      end
    | Fabricated (Ok (Closure _)) -> try_name ()
    | Fabricated (Ok (Discriminant discriminants)) ->
      begin match Discriminant.Map.get_singleton discriminants with
      | None -> try_name ()
      | Some (discriminant, { equations = _; }) ->
        Term (Simple.discriminant discriminant, t)
      end
    | Fabricated Unknown -> try_name ()
    | Fabricated Bottom -> Invalid

(* CR mshinwell: rename to "prove_must_be_tagged_immediate" *)
let prove_tagged_immediate env t
      : Immediate.Set.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a tagged \
        immediate: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create env t in
  Simplified_type.check_not_phantom simplified "prove_tagged_immediate";
  match simplified.descr with
  | Value ty_value ->
    begin match ty_value with
    | Unknown -> Unknown
    | Bottom -> Invalid
    | Ok (Blocks_and_tagged_immediates blocks_imms) ->
      begin match blocks_imms.blocks, blocks_imms.immediates with
      | Unknown, _ | _, Unknown -> Unknown
      | Known blocks, Known imms ->
        match Tag.Map.is_empty blocks, Immediate.Map.is_empty imms with
        | true, true -> Invalid
        | false, false -> Unknown
        | true, false -> Proved (Immediate.Map.keys imms)
        | false, true -> Invalid
      end
    | Ok (Boxed_number _) -> Invalid
    | Ok (Closures _ | String _) -> Invalid
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _ -> wrong_kind ()

type tagged_immediate_as_discriminants_proof =
  | By_discriminant of Equations.t Discriminant.Map.t
  | Answer_given_by of Name.t

let prove_tagged_immediate_as_discriminants env t
      : tagged_immediate_as_discriminants_proof proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a tagged \
        immediate: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create env t in
  Simplified_type.check_not_phantom simplified
    "prove_tagged_immediate_as_discriminants";
  match simplified.descr with
  | Value ty_value ->
    begin match ty_value with
    | Unknown -> Unknown
    | Bottom -> Invalid
    | Ok (Blocks_and_tagged_immediates blocks_imms) ->
      let use_get_tag () =
        match blocks_imms.get_tag with
        | None -> Unknown
        | Some get_tag -> Proved (Answer_given_by get_tag)
      in
      begin match blocks_imms.blocks, blocks_imms.immediates with
      | Unknown, _ | _, Unknown -> use_get_tag ()
      | Known blocks, Known imms ->
        match Tag.Map.is_empty blocks, Immediate.Map.is_empty imms with
        | true, true -> Invalid
        | false, false -> use_get_tag ()
        | true, false ->
          let by_discr =
            Immediate.Map.fold (fun imm (imm_case : immediate_case) by_discr ->
                let bad_discriminant () =
                  Misc.fatal_errorf "Immediate %a cannot be interpreted \
                      as a discriminant.  In type: %a"
                    Immediate.print imm
                    print t
                in
                let imm = Immediate.to_targetint imm in
                match Discriminant.create imm with
                | None -> bad_discriminant ()
                | Some discr ->
                  Discriminant.Map.add discr imm_case.equations by_discr)
              imms
              Discriminant.Map.empty
          in
          Proved (By_discriminant by_discr)
        | false, true -> Invalid
      end
    | Ok (Boxed_number _) -> Invalid
    | Ok (Closures _ | String _) -> Invalid
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _ -> wrong_kind ()

type is_tagged_immediate =
  | Never_a_tagged_immediate
  | Always_a_tagged_immediate
  | Answer_given_by of Name.t

let prove_is_tagged_immediate env t : is_tagged_immediate proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a tagged \
        immediate: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create env t in
  Simplified_type.check_not_phantom simplified "prove_is_tagged_immediate";
  match simplified.descr with
  | Value ty_value ->
    begin match ty_value with
    | Unknown -> Unknown
    | Bottom -> Invalid
    | Ok (Blocks_and_tagged_immediates blocks_imms) ->
      let use_is_int () =
        match blocks_imms.is_int with
        | None -> Unknown
        | Some is_int -> Proved (Answer_given_by is_int)
      in
      begin match blocks_imms.blocks, blocks_imms.immediates with
      | Unknown, _ | _, Unknown -> use_is_int ()
      | Known blocks, Known imms ->
        match Tag.Map.is_empty blocks, Immediate.Map.is_empty imms with
        | true, true -> Invalid
        | false, false -> use_is_int ()
        | true, false -> Proved Always_a_tagged_immediate
        | false, true -> Proved Never_a_tagged_immediate
      end
    | Ok (Boxed_number _) -> Invalid
    | Ok (Closures _ | String _) -> Invalid
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _ -> wrong_kind ()

let valid_block_tag_for_kind ~tag ~(field_kind : K.t) =
  (* CR-someday mshinwell: Note that we could easily extend
     this to handle blocks of the other unboxed number kinds. *)
  match field_kind with
  | Value -> Tag.is_structured_block tag
  | Fabricated -> Tag.equal tag Tag.zero
  | Naked_number Naked_float -> Tag.equal tag Tag.double_array_tag
  | Naked_number _ | Phantom _ ->
    Misc.fatal_errorf "Bad kind for block field: %a"
      K.print field_kind

let prove_get_field_from_block env t ~index ~field_kind : t proof =
(*
Format.eprintf "get_field_from_block index %a type@ %a\n"
  Targetint.OCaml.print index print t;
*)
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a block: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create env t in
  Simplified_type.check_not_phantom simplified "prove_get_field_from_block";
  match simplified.descr with
  | Value ty_value ->
    begin match ty_value with
    | Unknown -> Unknown
    | Bottom -> Invalid
    | Ok (Blocks_and_tagged_immediates blocks_imms) ->
      if Targetint.OCaml.compare index Targetint.OCaml.zero < 0 then Invalid
      else
        begin match blocks_imms.blocks with
        | Unknown -> Unknown
        | Known blocks ->
          assert (not (Tag.Map.is_empty blocks));
          let field_ty =
            Tag.Map.fold
              (fun tag ((Join { by_length; }) : block_cases) field_ty ->
                let tag_is_valid =
                  valid_block_tag_for_kind ~tag ~field_kind
                in
                if not tag_is_valid then field_ty
                else
                  Targetint.OCaml.Map.fold
                    (fun len (block : singleton_block) field_ty ->
                      if Targetint.OCaml.compare index len >= 0 then begin
                        None
                      end else begin
                        (* CR mshinwell: Should this check the kind of all
                           fields, like [prove_is_a_block] below? *)
                        (* CR mshinwell: should use more robust conversion *)
                        let index = Targetint.OCaml.to_int index in
                        assert (Array.length block.fields > index);
                        let this_field_ty =
                          match block.fields.(index) with
                          | Immutable this_field_ty ->
(*
                            Format.eprintf "this_field_ty index %d: %a\n%!"
                              index
                              print this_field_ty;
*)
                            this_field_ty
                          | Mutable -> unknown field_kind
                        in
                        match field_ty with
                        | None -> None
                        | Some field_ty ->
                          let field_ty =
                            join (env, this_field_ty) (env, field_ty)
                          in
                          Some field_ty
                      end)
                    by_length
                    field_ty)
              blocks
              (Some (bottom field_kind))
          in
(*
Format.eprintf "returned field type is %a\n"
  (Misc.Stdlib.Option.print print) field_ty;
*)
          match field_ty with
          | None -> Invalid
          | Some field_ty -> Proved field_ty
        end 
    | Ok (Boxed_number _) -> Invalid
    | Ok (Closures _ | String _) -> Invalid
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _ -> wrong_kind ()

let tags_all_valid t blocks ~kind_of_all_fields =
  Tag.Map.for_all (fun tag ((Join { by_length; }) : block_cases) ->
      Targetint.OCaml.Map.iter
        (fun _length (block : singleton_block) ->
          Array.iter (fun (field : _ mutable_or_immutable) ->
              match field with
              | Mutable -> ()
              | Immutable field ->
                let field_kind = kind field in
                let compatible =
                  K.compatible field_kind
                    ~if_used_at:kind_of_all_fields
                in
                if not compatible then begin
                  Misc.fatal_errorf "Kind %a is not compatible \
                      with all fields of this block: %a"
                    K.print kind_of_all_fields
                    print t
                end)
            block.fields)
        by_length;
      valid_block_tag_for_kind ~tag ~field_kind:kind_of_all_fields)
    blocks

let prove_must_be_a_block env t ~kind_of_all_fields : unit proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a block: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create env t in
  Simplified_type.check_not_phantom simplified "prove_is_a_block";
  match simplified.descr with
  | Value ty_value ->
    begin match ty_value with
    | Unknown -> Unknown
    | Bottom -> Invalid
    | Ok (Blocks_and_tagged_immediates blocks_imms) ->
      begin match blocks_imms.blocks, blocks_imms.immediates with
      | Unknown, _ | _, Unknown -> Unknown
      | Known blocks, Known imms ->
        match Tag.Map.is_empty blocks, Immediate.Map.is_empty imms with
        | true, true -> Invalid
        | false, false -> Unknown
        | true, false -> Invalid
        | false, true ->
          let tags_all_valid = tags_all_valid t blocks ~kind_of_all_fields in
          if tags_all_valid then Proved () else Invalid
      end
    | Ok (Boxed_number _) -> Invalid
    | Ok (Closures _ | String _) -> Invalid
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _ -> wrong_kind ()

type unboxable_variant_or_block_of_values0 = {
  block_sizes_by_tag : Targetint.OCaml.t Tag.Scannable.Map.t;
  constant_ctors : Immediate.Set.t;
}

type unboxable_variant_or_block_of_values =
  | Unboxable of unboxable_variant_or_block_of_values0
  | Not_unboxable

let prove_unboxable_variant_or_block_of_values env t
      : unboxable_variant_or_block_of_values proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a \
        variant or block of values: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create env t in
  Simplified_type.check_not_phantom simplified
    "prove_unboxable_variant_or_block_of_values";
  match simplified.descr with
  | Value ty_value ->
    begin match ty_value with
    | Unknown -> Unknown
    | Bottom -> Invalid
    | Ok (Blocks_and_tagged_immediates blocks_imms) ->
      begin match blocks_imms.blocks, blocks_imms.immediates with
      | Unknown, _ | _, Unknown -> Unknown
      | Known blocks, Known imms ->
        if Tag.Map.is_empty blocks then
          Proved Not_unboxable
        else
          let cannot_unbox = ref false in
          let block_sizes_by_tag =
            Tag.Map.fold (fun tag (Join { by_length; }) blocks ->
                match Targetint.OCaml.Map.get_singleton by_length with
                | Some (length, _) ->
                  begin match Tag.Scannable.of_tag tag with
                  | Some tag ->
                    Tag.Scannable.Map.add tag length blocks
                  | None ->
                    cannot_unbox := true;
                    blocks
                  end
                | None ->
                  cannot_unbox := true;
                  blocks)
              blocks
              Tag.Scannable.Map.empty
          in
          if !cannot_unbox then Proved Not_unboxable
          else
            let imms = Immediate.Map.keys imms in
            if tags_all_valid t blocks ~kind_of_all_fields:(K.value ()) then
              Proved (Unboxable {
                block_sizes_by_tag;
                constant_ctors = imms;
              })
            else Proved Not_unboxable
      end
    | Ok (Boxed_number _) | Ok (Closures _ | String _) -> Invalid
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _ -> wrong_kind ()

type float_array_proof =
  | Of_length of Targetint.OCaml.t
  | Not_unique_length

(* CR mshinwell: This should probably return the field types rather than
   just the length; then it can be exposed in the .mli. *)
let prove_float_array env t : float_array_proof proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a float array: \
        %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create env t in
  Simplified_type.check_not_phantom simplified "prove_float_array";
  match simplified.descr with
  | Value ty_value ->
    begin match ty_value with
    | Unknown -> Unknown
    | Bottom -> Invalid
    | Ok (Blocks_and_tagged_immediates blocks_imms) ->
      begin match blocks_imms.blocks, blocks_imms.immediates with
      | Unknown, _ | _, Unknown -> Unknown
      | Known blocks, Known imms ->
        if Tag.Map.is_empty blocks
          || not (Immediate.Map.is_empty imms)
        then Invalid
        else
          let cannot_unbox = ref false in
          (* CR mshinwell: share with previous function (maybe) *)
          let block_sizes_by_tag =
            Tag.Map.fold (fun tag (Join { by_length; }) blocks ->
                match Targetint.OCaml.Map.get_singleton by_length with
                | Some (length, _) ->
                  Tag.Map.add tag length blocks
                | None ->
                  cannot_unbox := true;
                  blocks)
              blocks
              Tag.Map.empty
          in
          if !cannot_unbox then Proved Not_unique_length
          else if tags_all_valid t blocks ~kind_of_all_fields:(K.naked_float ())
          then
            match Tag.Map.get_singleton block_sizes_by_tag with
            | Some (tag, size) ->
              if Tag.equal tag Tag.double_array_tag then
                Proved (Of_length size)
              else Invalid
            | None -> Invalid (* CR mshinwell: double-check *)
          else Invalid
      end
    | Ok (Boxed_number _) | Ok (Closures _ | String _) -> Invalid
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _ -> wrong_kind ()

type tags =
  | Tags of Tag.Set.t
  | Answer_given_by of Name.t

(* CR mshinwell: There's a bit of a wart here (in conjunction with the
   [Get_tag] primitive -- some of these tags don't really make any sense in
   that context, e.g. for closures, since there isn't a
   [Blocks_and_tagged_immediates] description for them.  Double_array_tag
   is of course an exception... maybe there should be a submodule of Tag
   which permits < No_scan_tag and also Double_array_tag? *)
let prove_tags env t : tags proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a value: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create env t in
  Simplified_type.check_not_phantom simplified "prove_tags";
  match simplified.descr with
  | Value ty_value ->
    begin match ty_value with
    | Unknown -> Unknown
    | Bottom -> Invalid
    | Ok (Blocks_and_tagged_immediates blocks_imms) ->
      begin match blocks_imms.blocks with
      | Known blocks -> Proved (Tags (Tag.Map.keys blocks))
      | Unknown ->
        match blocks_imms.get_tag with
        | None -> Unknown
        | Some get_tag -> Proved (Answer_given_by get_tag)
        end
    | Ok (Boxed_number (Boxed_float _)) ->
      Proved (Tags (Tag.Set.singleton Tag.double_tag))
    | Ok (Boxed_number (Boxed_int32 _)) ->
      Proved (Tags (Tag.Set.singleton Tag.custom_tag))
    | Ok (Boxed_number (Boxed_int64 _)) ->
      Proved (Tags (Tag.Set.singleton Tag.custom_tag))
    | Ok (Boxed_number (Boxed_nativeint _)) ->
      Proved (Tags (Tag.Set.singleton Tag.custom_tag))
    | Ok (Closures _) ->
      Proved (Tags (Tag.Set.singleton Tag.closure_tag))
    | Ok (String _) ->
      Proved (Tags (Tag.Set.singleton Tag.string_tag))
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _ -> wrong_kind ()

let prove_string env t : String_info.Set.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a string: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create env t in
  Simplified_type.check_not_phantom simplified "prove_string";
  match simplified.descr with
  | Value ty_value ->
    begin match ty_value with
    | Unknown -> Unknown
    | Bottom -> Invalid
    | Ok (String strs) -> Proved strs
    | Ok (Blocks_and_tagged_immediates _ | Closures _) -> Invalid
    | Ok (Boxed_number _) -> Invalid
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _ -> wrong_kind ()

let prove_boxed_float env t
      : Float_by_bit_pattern.Set.t ty_naked_number proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a boxed \
        float: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create env t in
  Simplified_type.check_not_phantom simplified "prove_boxed_float";
  match simplified.descr with
  | Value ty_value ->
    begin match ty_value with
    | Unknown -> Unknown
    | Bottom -> Invalid
    | Ok (Boxed_number (Boxed_float ty_naked_number)) -> Proved ty_naked_number
    | Ok _ -> Invalid
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _ -> wrong_kind ()

let prove_boxed_int32 env t
      : Int32.Set.t ty_naked_number proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a boxed \
        int32: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create env t in
  Simplified_type.check_not_phantom simplified "prove_boxed_int32";
  match simplified.descr with
  | Value ty_value ->
    begin match ty_value with
    | Unknown -> Unknown
    | Bottom -> Invalid
    | Ok (Boxed_number (Boxed_int32 ty_naked_number)) -> Proved ty_naked_number
    | Ok _ -> Invalid
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _ -> wrong_kind ()

let prove_boxed_int64 env t
      : Int64.Set.t ty_naked_number proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a boxed \
        int64: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create env t in
  Simplified_type.check_not_phantom simplified "prove_boxed_int64";
  match simplified.descr with
  | Value ty_value ->
    begin match ty_value with
    | Unknown -> Unknown
    | Bottom -> Invalid
    | Ok (Boxed_number (Boxed_int64 ty_naked_number)) -> Proved ty_naked_number
    | Ok _ -> Invalid
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _ -> wrong_kind ()

let prove_boxed_nativeint env t
      : Targetint.Set.t ty_naked_number proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a boxed \
        nativeint: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create env t in
  Simplified_type.check_not_phantom simplified "prove_boxed_nativeint";
  match simplified.descr with
  | Value ty_value ->
    begin match ty_value with
    | Unknown -> Unknown
    | Bottom -> Invalid
    | Ok (Boxed_number (Boxed_nativeint ty_naked_number)) ->
      Proved ty_naked_number
    | Ok _ -> Invalid
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _ -> wrong_kind ()

let prove_closures env t : closures proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be one or more \
        closures: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create env t in
  Simplified_type.check_not_phantom simplified "prove_closures";
  match simplified.descr with
  | Value ty_value ->
    begin match ty_value with
    | Unknown -> Unknown
    | Bottom -> Invalid
    | Ok (Closures closures) -> Proved closures
    | Ok _ -> Invalid
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _ -> wrong_kind ()

let prove_sets_of_closures env t : _ proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a set of \
        closures: %a"
      print t
  in
  let simplified, canonical_name = Simplified_type.create env t in
  Simplified_type.check_not_phantom simplified "prove_sets_of_closures";
  match simplified.descr with
  | Fabricated ty_fabricated ->
    begin match ty_fabricated with
    | Unknown -> Unknown
    | Bottom -> Invalid
    | Ok (Set_of_closures set_of_closures) ->
      Proved (canonical_name, set_of_closures)
    | Ok _ -> Invalid
    end
  | Value _ -> wrong_kind ()
  | Simplified_type.Naked_number _ -> wrong_kind ()

(* XXX What about [Obj.truncate]?
   In fact, what happens regarding this for block access too? *)

(* XXX Lengths of strings: for this, I think we can assume that Obj.truncate
   is always illegal here *)

let prove_lengths_of_arrays_or_blocks env t
      : Targetint.OCaml.Set.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a block: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create env t in
  Simplified_type.check_not_phantom simplified
    "prove_lengths_of_arrays_or_blocks";
  match simplified.descr with
  | Value ty_value ->
    begin match ty_value with
    | Unknown -> Unknown
    | Bottom -> Invalid
    | Ok (Blocks_and_tagged_immediates blocks_imms) ->
      let no_immediates =
        match blocks_imms.immediates with
        | Known imms when Immediate.Map.is_empty imms -> true
        | Known _ | Unknown -> false
      in
      if not no_immediates then begin
        Invalid
      end else begin
        match blocks_imms.blocks with
        | Unknown -> Unknown
        | Known blocks ->
          assert (not (Tag.Map.is_empty blocks));
          let lengths =
            Tag.Map.fold
              (fun _tag ((Join { by_length; }) : block_cases) result ->
                Targetint.OCaml.Map.fold (fun length _block result ->
                    Targetint.OCaml.Set.add length result)
                  by_length
                  result)
              blocks
              Targetint.OCaml.Set.empty
          in
          assert (not (Targetint.OCaml.Set.is_empty lengths));
          Proved lengths
      end
    | Ok (Boxed_number _) -> Invalid
    | Ok (Closures _ | String _) -> Invalid
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _ -> wrong_kind ()

let prove_of_kind_value t =
  let actual_kind = kind t in
  let expected_kind = K.value () in
  if not (Flambda_kind.compatible actual_kind ~if_used_at:expected_kind)
  then begin
    Misc.fatal_errorf "Type should be compatible with kind %a but \
        has incompatible kind %a: %a"
      Flambda_kind.print expected_kind
      Flambda_kind.print actual_kind
      print t
    end;
  force_to_kind_value t

let prove_of_kind_naked_float t =
  let actual_kind = kind t in
  let expected_kind = K.naked_float () in
  if not (Flambda_kind.compatible actual_kind ~if_used_at:expected_kind)
  then begin
    Misc.fatal_errorf "Type should be compatible with kind %a but \
        has incompatible kind %a: %a"
      Flambda_kind.print expected_kind
      Flambda_kind.print actual_kind
      print t
    end;
  force_to_kind_naked_float t

let values_physically_equal (t1 : t) (t2 : t) =
  let check_aliases (ty1 : _ ty) (ty2 : _ ty) =
    match ty1, ty2 with
    | No_alias _, _ | _, No_alias _ ->
      (* We don't need to check cases such as immediates, where we could
         prove physical equality, since [simplify_eq_comp] in
         [Simplify_binary_primitive] will have already done that. *)
      false
    | Type _, _ | _, Type _ ->
      (* CR mshinwell: Presumably we could look up the Export_id.t in the
         environment and continue?  Maybe not worth it. *)
      false
    | Equals name1, Equals name2 ->
      (* CR mshinwell: (see comment in simplify_primitives.ml in the existing
         Flambda)  We didn't used to check equality on variable aliases in case
         the variables weren't bound.  However everything should be bound now,
         so this seems like it should be ok.  (Remember that Name covers both
         variables and symbols.) *)
      Name.equal name1 name2
  in
  match t1.phantom, t2.phantom with
  | Some _, _ | _, Some _ -> false
  | None, None ->
    match t1.descr, t2.descr with
    | Value ty_value1, Value ty_value2 ->
      check_aliases ty_value1 ty_value2
    | Naked_number (ty_naked_number1, _), Naked_number (ty_naked_number2, _) ->
      check_aliases ty_naked_number1 ty_naked_number2
    | Fabricated _, Fabricated _ -> false
    | _, _ ->
      Misc.fatal_errorf "Kind mismatch for [physically_equal]: %a versus %a"
        print t1
        print t2

let values_structurally_distinct (env1, (t1 : t)) (env2, (t2 : t)) =
Format.eprintf "SD check: %a vs %a\n%!" print t1 print t2;
  let simplified1, _canonical_name1 = Simplified_type.create env1 t1 in
  let simplified2, _canonical_name2 = Simplified_type.create env2 t2 in
  let module S = Simplified_type in
  if S.is_phantom simplified1 || S.is_phantom simplified2 then false
  else
    (* Note: this function relies on the fact that sets of "known values" are
       always exact, and never a subset of the possible known values.  (This
       distinction would be important if, for example, a type's knowledge has
       to be cut down because it is getting too large.  Some random subset cannot
       be chosen: we must forget all of the possible values. *)
    match simplified1.descr, simplified2.descr with
    | Value ty_value1, Value ty_value2 ->
      begin match ty_value1, ty_value2 with
      | Unknown, _ | _, Unknown | Bottom, _ | _, Bottom -> false
      | Ok of_kind_value1, Ok of_kind_value2 ->
        begin match of_kind_value1, of_kind_value2 with
        | Blocks_and_tagged_immediates
            { blocks = blocks1; immediates = imms1;
              is_int = _; get_tag = _; },
            Blocks_and_tagged_immediates
              { blocks = blocks2; immediates = imms2;
                is_int = _; get_tag = _; } ->
          (* CR-someday mshinwell: This could be improved if required. *)
          begin match blocks1, blocks2 with
          | Unknown, _ | _, Unknown -> false
          | Known blocks1, Known blocks2 ->
            begin match imms1, imms2 with
            | Unknown, _ | _, Unknown -> false
            | Known imms1, Known imms2 ->
              (* CR mshinwell: This should actually collect a set of
                 (tag, block length) pairs and prove the intersection of two
                 such sets is empty. *)
              let tag_intersection =
                Tag.Set.inter (Tag.Map.keys blocks1) (Tag.Map.keys blocks2)
              in
              let imm_intersection =
                Immediate.Set.inter (Immediate.Map.keys imms1)
                  (Immediate.Map.keys imms2)
              in
              Tag.Set.is_empty tag_intersection
                && Immediate.Set.is_empty imm_intersection
            end
          end
        | Blocks_and_tagged_immediates _, _
        | _, Blocks_and_tagged_immediates _ -> true
        | Boxed_number (Boxed_float ty_naked_number1),
            Boxed_number (Boxed_float ty_naked_number2) ->
          begin match
            prove_naked_float env1
              (of_ty_naked_number ty_naked_number1
                K.Naked_number.Naked_float),
            prove_naked_float env2
              (of_ty_naked_number ty_naked_number2
                K.Naked_number.Naked_float)
          with
          | Proved nums1, Proved nums2 ->
            Float_by_bit_pattern.Set.is_empty
              (Float_by_bit_pattern.Set.inter nums1 nums2)
          | _, _ -> false
          end
        | Boxed_number (Boxed_int32 ty_naked_number1),
            Boxed_number (Boxed_int32 ty_naked_number2) ->
          begin match
            prove_naked_int32 env1
              (of_ty_naked_number ty_naked_number1
                K.Naked_number.Naked_int32),
            prove_naked_int32 env2
              (of_ty_naked_number ty_naked_number2
                K.Naked_number.Naked_int32)
          with
          | Proved nums1, Proved nums2 ->
            Int32.Set.is_empty (Int32.Set.inter nums1 nums2)
          | _, _ -> false
          end
        | Boxed_number (Boxed_int64 ty_naked_number1),
            Boxed_number (Boxed_int64 ty_naked_number2) ->
          begin match
            prove_naked_int64 env1
              (of_ty_naked_number ty_naked_number1
                K.Naked_number.Naked_int64),
            prove_naked_int64 env2
              (of_ty_naked_number ty_naked_number2
                K.Naked_number.Naked_int64)
          with
          | Proved nums1, Proved nums2 ->
            Int64.Set.is_empty (Int64.Set.inter nums1 nums2)
          | _, _ -> false
          end
        | Boxed_number (Boxed_nativeint ty_naked_number1),
            Boxed_number (Boxed_nativeint ty_naked_number2) ->
          begin match
            prove_naked_nativeint env1
              (of_ty_naked_number ty_naked_number1
                K.Naked_number.Naked_nativeint),
            prove_naked_nativeint env2
              (of_ty_naked_number ty_naked_number2
                K.Naked_number.Naked_nativeint)
          with
          | Proved nums1, Proved nums2 ->
            Targetint.Set.is_empty (Targetint.Set.inter nums1 nums2)
          | _, _ -> false
          end
        | Boxed_number _, _ -> true
        | _, Boxed_number _ -> true
        | Closures _, Closures _ -> false
        | Closures _, _ | _, Closures _ -> true
        | String strs1, String strs2 ->
          String_info.Set.is_empty (String_info.Set.inter strs1 strs2)
        end
      end
    | S.Naked_number (_ty_naked_number1, K.Naked_number.Naked_immediate),
        S.Naked_number (_ty_naked_number2, K.Naked_number.Naked_immediate) ->
      (* CR-someday mshinwell: Support to be implemented later. *)
      false
    | S.Naked_number (_, K.Naked_number.Naked_float),
        S.Naked_number (_, K.Naked_number.Naked_float) ->
      begin match
        prove_naked_float env1 t1,
          prove_naked_float env2 t2
      with
      | Proved nums1, Proved nums2 ->
        Float_by_bit_pattern.Set.is_empty
          (Float_by_bit_pattern.Set.inter nums1 nums2)
      | _, _ -> false
      end
    | S.Naked_number (_, K.Naked_number.Naked_int32),
        S.Naked_number (_, K.Naked_number.Naked_int32) ->
      begin match
        prove_naked_int32 env1 t1,
          prove_naked_int32 env2 t2
      with
      | Proved nums1, Proved nums2 ->
        Int32.Set.is_empty
          (Int32.Set.inter nums1 nums2)
      | _, _ -> false
      end
    | S.Naked_number (_, K.Naked_number.Naked_int64),
        S.Naked_number (_, K.Naked_number.Naked_int64) ->
      begin match
        prove_naked_int64 env1 t1,
          prove_naked_int64 env2 t2
      with
      | Proved nums1, Proved nums2 ->
        Int64.Set.is_empty
          (Int64.Set.inter nums1 nums2)
      | _, _ -> false
      end
    | S.Naked_number (_, K.Naked_number.Naked_nativeint),
        S.Naked_number (_, K.Naked_number.Naked_nativeint) ->
      begin match
        prove_naked_nativeint env1 t1,
          prove_naked_nativeint env2 t2
      with
      | Proved nums1, Proved nums2 ->
        Targetint.Set.is_empty
          (Targetint.Set.inter nums1 nums2)
      | _, _ -> false
      end
    | Fabricated _, Fabricated _ -> false
    | _, _ ->
      Misc.fatal_errorf "Kind mismatch for [structurally_different]: %a \
          versus %a"
        print t1
        print t2

let switch_arms env t ~arms =
  let no_equations = Equations.create () in
  let wrong_kind () =
    Misc.fatal_errorf
      "Wrong kind for something claimed to be a discriminant: %a"
      print t
  in
  let unknown () =
    Discriminant.Map.fold (fun arm cont result ->
        Discriminant.Map.add arm (no_equations, cont) result)
      arms
      Discriminant.Map.empty
  in
  let invalid () = Discriminant.Map.empty in
  let simplified, _canonical_name = Simplified_type.create env t in
  Simplified_type.check_not_phantom simplified "discriminant_switch_arms";
  match simplified.descr with
  | Fabricated ty_fabricated ->
    begin match ty_fabricated with
    | Unknown -> unknown ()
    | Bottom -> invalid ()
    | Ok (Discriminant discriminant_map) ->
      Discriminant.Map.fold (fun arm cont result ->
          match Discriminant.Map.find arm discriminant_map with
          | exception Not_found -> result
          | { equations; } ->
            Discriminant.Map.add arm (equations, cont) result)
        arms
        Discriminant.Map.empty
    | Ok (Set_of_closures _) | Ok (Closure _) -> invalid ()
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Value _ -> wrong_kind ()

type unboxable_proof =
  | Variant_or_block_of_values of unboxable_variant_or_block_of_values0
  | Float_array of { length : Targetint.OCaml.t; }
  | Boxed_float
  | Boxed_int32
  | Boxed_int64
  | Boxed_nativeint
  | Cannot_unbox

let prove_unboxable env ~unboxee_ty : unboxable_proof =
  let kind = kind unboxee_ty in
  if not (K.is_value kind) then Cannot_unbox
  else
    match prove_unboxable_variant_or_block_of_values env unboxee_ty with
    | Proved (Unboxable unboxable) -> Variant_or_block_of_values unboxable
    | Proved Not_unboxable -> Cannot_unbox
    | Invalid | Unknown ->
      match prove_float_array env unboxee_ty with
      | Proved (Of_length length) -> Float_array { length; }
      | Proved Not_unique_length -> Cannot_unbox
      | Invalid | Unknown ->
        match prove_boxed_float env unboxee_ty with
        | Proved _ty_naked_number -> Boxed_float
        | Invalid | Unknown ->
          match prove_boxed_int32 env unboxee_ty with
          | Proved _ty_naked_number -> Boxed_int32
          | Invalid | Unknown ->
            match prove_boxed_int64 env unboxee_ty with
            | Proved _ty_naked_number -> Boxed_int64
            | Invalid | Unknown ->
              match prove_boxed_nativeint env unboxee_ty with
              | Proved _ty_naked_number -> Boxed_nativeint
              | Invalid | Unknown -> Cannot_unbox

module Typing_environment = struct
  include Typing_environment0

  let diff t1 t2 = diff ~strictly_more_precise t1 t2
end
