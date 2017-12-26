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

module Expr = F0.Expr
module Named = F0.Named

module Float_by_bit_pattern = Numbers.Float_by_bit_pattern
module Int = Numbers.Int
module Int32 = Numbers.Int32
module Int64 = Numbers.Int64

include F0.Flambda_type

let unknown_types_from_arity t =
  List.map (fun kind -> unknown kind) t

let bottom_like ~type_of_name t =
  let kind = kind ~type_of_name t in
  bottom kind

let unknown_like ~type_of_name t =
  let kind = kind ~type_of_name t in
  unknown kind

let unknown_like_array ~type_of_name t_array =
  Array.map (fun t -> unknown_like ~type_of_name t) t_array

let rename_variables _t ~f:_ =
  assert false
(* XXX need to fix [Flambda_type0.clean]
  clean t (fun var -> Available_different_name (f var))
*)

let unresolved_symbol _sym =
  any_value Definitely_immediate

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

let equal_function_declaration ~equal_type
      (decl1 : function_declaration)
      (decl2 : function_declaration) =
  match decl1, decl2 with
  | Inlinable {
      closure_origin = closure_origin1;
      continuation_param = continuation_param1;
      is_classic_mode = is_classic_mode1;
      params = params1;
      body = body1;
      free_names_in_body = free_names_in_body1;
      result = result1;
      stub = stub1;
      dbg = dbg1;
      inline = inline1;
      specialise = specialise1;
      is_a_functor = is_a_functor1;
      invariant_params = invariant_params1;
      size = size1;
      direct_call_surrogate = direct_call_surrogate1;
    },
    Inlinable {
      closure_origin = closure_origin2;
      continuation_param = continuation_param2;
      is_classic_mode = is_classic_mode2;
      params = params2;
      body = body2;
      free_names_in_body = free_names_in_body2;
      result = result2;
      stub = stub2;
      dbg = dbg2;
      inline = inline2;
      specialise = specialise2;
      is_a_functor = is_a_functor2;
      invariant_params = invariant_params2;
      size = size2;
      direct_call_surrogate = direct_call_surrogate2;
    } ->
    Closure_origin.equal closure_origin1 closure_origin2
      && Continuation.equal continuation_param1 continuation_param2
      && Pervasives.compare is_classic_mode1 is_classic_mode2 = 0
      && Misc.Stdlib.List.equal (fun (param1, t1) (param2, t2) ->
          Parameter.equal param1 param2 && equal_type t1 t2)
        params1 params2
      && Expr.equal ~equal_type body1 body2
      && Name.Set.equal free_names_in_body1 free_names_in_body2
      && Misc.Stdlib.List.equal equal_type result1 result2
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
  | Non_inlinable {
      result = result1;
      direct_call_surrogate = direct_call_surrogate1;
    },
    Non_inlinable {
      result = result2;
      direct_call_surrogate = direct_call_surrogate2;
    } ->
    List.compare_lengths result1 result2 = 0
      && List.for_all2 (fun t1 t2 -> equal_type t1 t2)
        result1 result2
      && Misc.Stdlib.Option.equal Closure_id.equal
        direct_call_surrogate1 direct_call_surrogate2
  | Inlinable _, Non_inlinable _
  | Non_inlinable _, Inlinable _ -> false

(*
let is_float_array t =
  match descr t with
  | Float_array _ -> true
  | Unknown _ | Bottom | Union _
  | Immutable_string _ | Mutable_string _
  | Sets_of_closures _ | Closure _ | Load_lazily _ | Boxed_number _
  | Unboxed_float _ | Unboxed_int32 _ | Unboxed_int64 _
  | Unboxed_nativeint _ -> false

let type_for_bound_var (set_of_closures : set_of_closures) var =
  try Var_within_closure.Map.find var set_of_closures.bound_vars
  with Not_found ->
    Misc.fatal_errorf "The set-of-closures type %a@ does not \
        bind the variable %a@.%s@."
      print_set_of_closures set_of_closures
      Var_within_closure.print var
      (Printexc.raw_backtrace_to_string (Printexc.get_callstack max_int))

let physically_same_values (types : t list) =
  match types with
  | [] | [_] | _ :: _ :: _ :: _ ->
    Misc.fatal_error "wrong number of arguments for equality"
  | [a1; a2] ->
    (* N.B. The following would be incorrect if the variables are not
       bound in the environment:
       match a1.var, a2.var with
       | Some v1, Some v2 when Variable.equal v1 v2 -> true
       | _ -> ...
    *)
    match a1.symbol, a2.symbol with
    | Some (s1, None), Some (s2, None) -> Symbol.equal s1 s2
    | Some (s1, Some f1), Some (s2, Some f2) -> Symbol.equal s1 s2 && f1 = f2
    | _ -> false

let physically_different_values (types : t list) =
  let rec definitely_different (arg1 : t) (arg2 : t) =
    let module Int = Numbers.Int in
    let module Immediate = Unionable.Immediate in
    let immediates_different s1 s2 =
      (* The frontend isn't precise about "int" and "const pointer", for
         example generating "(!= b/1006 0)" for a match against a bool, which
         is a "const pointer".  The same presumably might happen with "char".
         As such for we treat immediates whose runtime representations are
         the same as equal. *)
      let s1 =
        Immediate.Set.fold (fun imm s1 ->
            Int.Set.add (Immediate.represents imm) s1)
          s1 Int.Set.empty
      in
      let s2 =
        Immediate.Set.fold (fun imm s2 ->
            Int.Set.add (Immediate.represents imm) s2)
          s2 Int.Set.empty
      in
      Int.Set.is_empty (Int.Set.inter s1 s2)
    in
    let blocks_different b1 b2 =
      let tags1 = Tag.Scannable.Map.keys b1 in
      let tags2 = Tag.Scannable.Map.keys b2 in
      let overlapping_tags = Tag.Scannable.Set.inter tags1 tags2 in
      Tag.Scannable.Set.exists (fun tag ->
          let fields1 = Tag.Scannable.Map.find tag b1 in
          let fields2 = Tag.Scannable.Map.find tag b2 in
          Array.length fields1 <> Array.length fields2
            || Misc.Stdlib.Array.exists2 definitely_different fields1 fields2)
        overlapping_tags
    in
    match arg1.descr, arg2.descr with
    | Unknown _, _ | _, Unknown _
    (* CR mshinwell: Should [Load_lazily] be an error here?  What about for the
       reification functions below?  [invalid_to_mutate] above has an
       assertion failure for this. *)
    | Load_lazily _, _ | _, Load_lazily _
    | Bottom, _ | _, Bottom -> false
    | Union (Immediates s1), Union (Immediates s2) ->
      immediates_different s1 s2
    | Union (Blocks b1), Union (Blocks b2) ->
      blocks_different b1 b2
    | Union (Blocks_and_immediates (b1, imms1)),
      Union (Blocks_and_immediates (b2, imms2)) ->
      immediates_different imms1 imms2 || blocks_different b1 b2
    | Union _, Union _ -> false
    | Union _, _ | _, Union _ -> true
    | Unboxed_float fs1, Unboxed_float fs2 ->
      Float_by_bit_pattern.Set.is_empty (Float_by_bit_pattern.Set.inter fs1 fs2)
    | Unboxed_float _, _ | _, Unboxed_float _ -> true
    | Unboxed_int32 ns1, Unboxed_int32 ns2 ->
      Int32.Set.is_empty (Int32.Set.inter ns1 ns2)
    | Unboxed_int32 _, _ | _, Unboxed_int32 _ -> true
    | Unboxed_int64 ns1, Unboxed_int64 ns2 ->
      Int64.Set.is_empty (Int64.Set.inter ns1 ns2)
    | Unboxed_int64 _, _ | _, Unboxed_int64 _ -> true
    | Unboxed_nativeint ns1, Unboxed_nativeint ns2 ->
      Nativeint.Set.is_empty (Nativeint.Set.inter ns1 ns2)
    | Unboxed_nativeint _, _ | _, Unboxed_nativeint _ -> true
    | Boxed_number (kind1, t1), Boxed_number (kind2, t2) ->
      (not (Boxed_number_kind.equal kind1 kind2))
        || definitely_different t1 t2
    | Boxed_number _, _ | _, Boxed_number _ -> true
    | Sets_of_closures _, Sets_of_closures _ -> false
    | Sets_of_closures _, _ | _, Sets_of_closures _ -> true
    | Closure _, Closure _ -> false
    | Closure _, _ | _, Closure _ -> true
    | Immutable_string s1, Immutable_string s2 -> String.compare s1 s2 <> 0
    | Immutable_string _, _ | _, Immutable_string _ -> true
    | Mutable_string _, Mutable_string _ -> false
    | Mutable_string _, _ | _, Mutable_string _ -> true
    | Float_array { contents = contents1; size = size1; },
      Float_array { contents = contents2; size = size2; } ->
      size1 <> size2
        || begin match contents1, contents2 with
           | Contents ts1, Contents ts2 ->
             Misc.Stdlib.Array.exists2 definitely_different ts1 ts2
           | Contents _, Unknown_or_mutable
           | Unknown_or_mutable, Contents _
           | Unknown_or_mutable, Unknown_or_mutable -> false
           end
  in
  match types with
  | [] | [_] | _ :: _ :: _ :: _ ->
    Misc.fatal_error "Wrong number of arguments for physical inequality"
  | [a1; a2] -> definitely_different a1 a2
*)

(*

let reify_as_unboxed_float_array (fa : float_array) : float list option =
  match fa.contents with
  | Unknown_or_mutable -> None
  | Contents contents ->
    Array.fold_right (fun elt acc ->
        match acc, descr elt with
        | Some acc, Unboxed_float fs ->
          begin match Float_by_bit_pattern.Set.get_singleton fs with
          | None -> None
          | Some f -> Some (f :: acc)
          end
        | None, _
        | Some _, _ -> None)
      contents (Some [])

let reify_as_string t : string option =
  match descr t with
  | Immutable_string str -> Some str
  | Union _ | Boxed_number _ | Unboxed_float _ | Unboxed_int32 _
  | Unboxed_int64 _ | Unboxed_nativeint _ | Unknown _ | Mutable_string _
  | Float_array _ | Bottom | Sets_of_closures _ | Closure _
  | Load_lazily _ -> None

type 'a or_wrong =
  | Ok of 'a
  | Wrong

module Or_not_all_values_known = struct
  type 'a t =
    | Exactly of 'a
    | Not_all_values_known

  let join join_contents t1 t2 : _ t or_wrong =
    match t1, t2 with
    | Exactly e1, Exactly e2 ->
      begin match join_contents e1 e2 with
      | Ok e -> Ok (Exactly e)
      | Wrong -> Wrong
      end
    | Exactly _, Not_all_values_known
    | Not_all_values_known, Exactly _
    | Not_all_values_known, Not_all_values_known -> Ok Not_all_values_known

  let meet meet_contents t1 t2 : _ t or_wrong =
    match t1, t2 with
    | Exactly e1, Exactly e2 ->
      begin match meet_contents e1 e2 with
      | Ok e -> Ok (Exactly e)
      | Wrong -> Wrong
      end
    | Exactly _, Not_all_values_known -> Ok t1
    | Not_all_values_known, Exactly _ -> Ok t2
    | Not_all_values_known, Not_all_values_known -> Ok Not_all_values_known

  let equal equal_contents t1 t2 =
    match t1, t2 with
    | Exactly c1, Exactly c2 -> equal_contents c1 c2
    | Not_all_values_known, Not_all_values_known -> true
    | Exactly _, Not_all_values_known
    | Not_all_values_known, Exactly _ -> false

  let print f ppf t =
    match t with
    | Exactly thing -> f ppf thing
    | Not_all_values_known -> Format.pp_print_string ppf "Not_all_values_known"
end

type get_field_result =
  | Ok of t
  | Invalid

module Blocks : sig
  type t

  val empty : t

  val is_empty : t -> bool

  val create_singleton : Tag.Scannable.t -> ty_value array -> t

  val unique_tag_and_size : t -> (Tag.Scannable.t * int) option

  val join : (t -> t -> t or_wrong) type_accessor

  val meet : (t -> t -> t or_wrong) type_accessor

  val equal
     : equal_type:(flambda_type -> flambda_type -> bool)
    -> t
    -> t
    -> bool

  val tags : t -> Tag.Scannable.Set.t

  val all_possible_sizes : t -> Targetint.OCaml.Set.t

  val print : Format.formatter -> t -> unit
end = struct
  (* CR-someday mshinwell: Random note: use of [array] here could in theory
     cause trouble for 32-bit -> 64-bit cross compilation. *)
  type t = ty_value array Tag.Scannable.Map.t

  let print ppf t =
    Tag.Scannable.Map.print print_ty_value_array ppf t

  let empty = Tag.Scannable.Map.empty

  let is_empty t = Tag.Scannable.Map.is_empty t

  let all_possible_sizes t =
    Tag.Scannable.Map.fold (fun _tag fields sizes ->
        let size = Targetint.OCaml.of_int (Array.length fields) in
        Targetint.OCaml.Set.add size sizes)
      t
      Targetint.OCaml.Set.empty

  let create_singleton tag fields =
    Tag.Scannable.Map.add tag fields Tag.Scannable.Map.empty

  let join ~type_of_name t1 t2 : t or_wrong =
    let exception Same_tag_different_arities in
    try
      let map =
        Tag.Scannable.Map.union (fun _tag fields1 fields2 ->
            if Array.length fields1 <> Array.length fields2 then
              raise Same_tag_different_arities
            else
              let fields =
                Array.map2 (fun ty_value1 ty_value2 ->
                    join_ty_value ~type_of_name ty_value1 ty_value2)
                  fields1 fields2
              in
              Some fields)
          t1 t2
      in
      Ok map
    with Same_tag_different_arities -> Wrong

  let meet ~type_of_name t1 t2 : t or_wrong =
    let exception Same_tag_different_arities in
    try
      let map =
        Tag.Scannable.Map.union (fun _tag fields1 fields2 ->
            if Array.length fields1 <> Array.length fields2 then
              raise Same_tag_different_arities
            else
              let fields =
                Array.map2 (fun ty_value1 ty_value2 ->
                    meet_ty_value ~type_of_name ty_value1 ty_value2)
                  fields1 fields2
              in
              Some fields)
          t1 t2
      in
      Ok map
    with Same_tag_different_arities -> Wrong

  let unique_tag_and_size t =
    match Tag.Scannable.Map.get_singleton t with
    | None -> None
    | Some (tag, fields) -> Some (tag, Array.length fields)

  let tags t = Tag.Scannable.Map.keys t

  let equal ~equal_type t1 t2 =
    Tag.Scannable.Map.equal (fun ty_values1 ty_values2 ->
        Array.length ty_values1 = Array.length ty_values2
          && Misc.Stdlib.Array.for_all2 (fun ty_value1 ty_value2 ->
              let t1 = t_of_ty_value ty_value1 in
              let t2 = t_of_ty_value ty_value2 in
              equal_type t1 t2)
            ty_values1 ty_values2)
      t1 t2
end

module Float_array = struct
  type t = {
    size : Targetint.OCaml.t;
    fields : ty_naked_float array option;
  }

  let size t = t.size

  let fields t =
    match t.fields with
    | Some fields -> fields
    | None ->
      Array.init (Targetint.OCaml.to_int t.size)
        (fun _index -> any_naked_float_as_ty_naked_float ())
end
*)

module Simplified_type : sig
  (* Simplified types omit the following at top level:
     - alias information;
     - joins between incompatible types (these turn into "Unknown").
  *)
  type t = private
    | Value of ty_value
    | Naked_number :
        'kind ty_naked_number * 'kind Flambda_kind.Naked_number.t -> t
    | Fabricated of ty_fabricated
    | Phantom of ty_phantom

  and ty_value = (of_kind_value, Flambda_kind.Value_kind.t) ty
  and 'a ty_naked_number = ('a of_kind_naked_number, unit) ty
  and ty_fabricated = (of_kind_fabricated, Flambda_kind.Value_kind.t) ty
  and ty_phantom = (of_kind_phantom, Flambda_kind.Phantom_kind.t) ty

  and ('a, 'u) ty = private
    | Unknown of 'u
    | Ok of 'a
    | Bottom

  (* Create a simple type from a type.  If the type has an alias at its
     top level stating that it is the type of some named value, that alias
     is (recursively) expanded, and the final ("canonical") name
     returned. *)
  val create : (flambda_type -> t * (Name.t option)) type_accessor

  val is_unknown : t -> bool
  val is_bottom : t -> bool
end = struct
  type t =
    | Value of ty_value
    | Naked_number :
        'kind ty_naked_number * 'kind Flambda_kind.Naked_number.t -> t
    | Fabricated of ty_fabricated
    | Phantom of ty_phantom

  and ty_value = (of_kind_value, Flambda_kind.Value_kind.t) ty
  and 'a ty_naked_number = ('a of_kind_naked_number, unit) ty
  and ty_fabricated = (of_kind_fabricated, Flambda_kind.Value_kind.t) ty
  and ty_phantom = (of_kind_phantom, Flambda_kind.Phantom_kind.t) ty

  and ('a, 'u) ty =
    | Unknown of 'u
    | Ok of 'a
    | Bottom

  let is_unknown t =
    match t with
    | Value (Unknown _) -> true
    | Naked_number (Unknown _, _) -> true
    | Fabricated (Unknown _) -> true
    | Phantom (Unknown _) -> true
    | _ -> false

  let is_bottom t =
    match t with
    | Value Bottom -> true
    | Naked_number (Bottom, _) -> true
    | Fabricated Bottom -> true
    | Phantom Bottom -> true
    | _ -> false

  let ty_from_or_unknown_or_join (unknown_or_join : _ unknown_or_join)
        ~unknown_payload : _ ty =
    match unknown_or_join with
    | Unknown payload -> Unknown payload
    | Join [] -> Bottom
    | Join [of_kind_foo] -> Ok of_kind_foo
    | Join _ -> Unknown unknown_payload

  let create ~type_of_name (ty : flambda_type) : t * (Name.t option) =
    match ty with
    | Value ty_value ->
      let unknown_or_join, canonical_name =
        resolve_aliases_and_squash_unresolved_names_on_ty
          ~type_of_name
          ~force_to_kind:force_to_kind_value
          ~unknown_payload:K.Value_kind.Unknown
          ty_value
      in
      let ty_value : ty_value =
        ty_from_or_unknown_or_join unknown_or_join
          ~unknown_payload:K.Value_kind.Unknown
      in
      Value ty_value, canonical_name
    | Naked_number (ty_naked_number, kind) ->
      let unknown_or_join, canonical_name =
        resolve_aliases_and_squash_unresolved_names_on_ty
          ~type_of_name
          ~force_to_kind:(force_to_kind_naked_number kind)
          ~unknown_payload:()
          ty_naked_number
      in
      let ty_naked_number : _ ty_naked_number =
        ty_from_or_unknown_or_join unknown_or_join
          ~unknown_payload:()
      in
      Naked_number (ty_naked_number, kind), canonical_name
    | Fabricated ty_fabricated ->
      let unknown_or_join, canonical_name =
        resolve_aliases_and_squash_unresolved_names_on_ty
          ~type_of_name
          ~force_to_kind:force_to_kind_fabricated
          ~unknown_payload:K.Value_kind.Unknown
          ty_fabricated
      in
      let ty_fabricated : ty_fabricated =
        ty_from_or_unknown_or_join unknown_or_join
          ~unknown_payload:K.Value_kind.Unknown
      in
      Fabricated ty_fabricated, canonical_name
    | Phantom ty_phantom ->
      let unknown_or_join, canonical_name =
        resolve_aliases_and_squash_unresolved_names_on_ty
          ~type_of_name
          ~force_to_kind:force_to_kind_phantom
          ~unknown_payload:K.Phantom_kind.Unknown
          ty_phantom
      in
      let ty_phantom : ty_phantom =
        ty_from_or_unknown_or_join unknown_or_join
          ~unknown_payload:K.Phantom_kind.Unknown
      in
      Phantom ty_phantom, canonical_name
end

(*

module Joined_closures : sig
  type t

  val create : (Closure.t list -> t) type_accessor

  val is_bottom : t -> bool

  val sets_of_closures : t -> flambda_type Closure_id.Map.t

  val print : Format.formatter -> t -> unit

  val to_type : t -> flambda_type

  val equal
     : equal_type:(flambda_type -> flambda_type -> bool)
    -> t
    -> t
    -> bool
end = struct
  type t = {
    sets_of_closures : ty_value Closure_id.Map.t;
  }

  let print ppf t =
    Format.fprintf ppf "@[(sets_of_closures@ %a)@]"
      (Closure_id.Map.print print_ty_value) t.sets_of_closures

  let sets_of_closures t =
    Closure_id.Map.map t_of_ty_value t.sets_of_closures

  let is_bottom t = Closure_id.Map.is_empty t.sets_of_closures

  let of_closure (closure : Closure.t) : t =
    let sets_of_closures =
      Closure_id.Map.add closure.closure_id closure.set_of_closures
        Closure_id.Map.empty
    in
    { sets_of_closures;
    }

  let join ~type_of_name t1 t2 =
    let sets_of_closures =
      Closure_id.Map.union (fun _closure_id ty_value1 ty_value2 ->
          Some (join_ty_value ~type_of_name ty_value1 ty_value2))
        t1.sets_of_closures
        t2.sets_of_closures
    in
    { sets_of_closures;
    }

  let create ~type_of_name (closures : Closure.t list) =
    let sets = List.map of_closure closures in
    match sets with
    | [] ->
      { sets_of_closures = Closure_id.Map.empty;
      }
    | set::sets ->
      List.fold_left (fun result t ->
          join ~type_of_name result t)
        set sets

  let to_type _t =
    assert false

  let equal ~equal_type { sets_of_closures = sets1; }
        { sets_of_closures = sets2; } =
    Closure_id.Map.equal (fun ty_value1 ty_value2 ->
        equal_type (t_of_ty_value ty_value1) (t_of_ty_value ty_value2))
      sets1 sets2
end

module Joined_sets_of_closures : sig
  type t
  val create : (Set_of_closures.t list -> t) type_accessor
  val type_for_closure_id : t -> Closure_id.t -> flambda_type
  val to_type : t -> flambda_type
  val to_unique_set_of_closures : t -> Set_of_closures.t option
  val equal
     : equal_type:(flambda_type -> flambda_type -> bool)
    -> t
    -> t
    -> bool
  val print : Format.formatter -> t -> unit
end = struct
  type t = {
    set_of_closures_id_and_origin :
      (Set_of_closures_id.t * Set_of_closures_origin.t)
        Or_not_all_values_known.t;
    function_decls : function_declaration Closure_id.Map.t;
    closure_elements : ty_value Var_within_closure.Map.t;
  }

  let print ppf t =
    Format.fprintf ppf "@[((function_decls %a)@ (closure_elements %a))@]"
      Closure_id.Set.print
      (Closure_id.Map.keys t.function_decls)
      Var_within_closure.Set.print
      (Var_within_closure.Map.keys t.closure_elements)

  let of_set_of_closures (set : Set_of_closures.t) : t =
    { set_of_closures_id_and_origin =
        Exactly (set.set_of_closures_id, set.set_of_closures_origin);
      function_decls = set.function_decls;
      closure_elements = set.closure_elements;
    }

  let type_for_closure_id _t _closure_id =
    (* CR mshinwell for pchambart: ... *)
    assert false

  let to_type t =
    match t.set_of_closures_id_and_origin with
    | Not_all_values_known ->
      any_value Must_scan Other
    | Exactly (set_of_closures_id, set_of_closures_origin) ->
      set_of_closures ~set_of_closures_id
        ~set_of_closures_origin
        ~function_decls:t.function_decls
        ~closure_elements:t.closure_elements

  let to_unique_set_of_closures t =
    match t.set_of_closures_id_and_origin with
    | Not_all_values_known -> None
    | Exactly (set_of_closures_id, set_of_closures_origin) ->
      let set =
        create_set_of_closures ~set_of_closures_id
          ~set_of_closures_origin
          ~function_decls:t.function_decls
          ~closure_elements:t.closure_elements
      in
      Some set

  let make_non_inlinable_function_declaration (f : function_declaration)
        : function_declaration =
    match f with
    | Inlinable decl ->
      let decl =
        create_non_inlinable_function_declaration ~result:decl.result
          ~direct_call_surrogate:decl.direct_call_surrogate
      in
      Non_inlinable decl
    | Non_inlinable _ -> f

  let join_and_make_all_functions_non_inlinable ~type_of_name
        (t1 : t) (t2 : t) : t =
    let join_results_and_make_non_inlinable (f1 : function_declaration)
          (f2 : function_declaration) : function_declaration =
      let f1_result =
        match f1 with
        | Inlinable f1 -> f1.result
        | Non_inlinable f1 -> f1.result
      in
      let f2_result =
        match f2 with
        | Inlinable f2 -> f2.result
        | Non_inlinable f2 -> f2.result
      in
      if List.length f1_result <> List.length f2_result then begin
        Misc.fatal_errorf "Function appears with two different return arities: \
            %a and %a"
          print t1
          print t2
      end;
      let result =
        List.map2 (join ~type_of_name) f1_result f2_result
      in
      let decl =
        create_non_inlinable_function_declaration ~result
          ~direct_call_surrogate:None
      in
      Non_inlinable decl
    in
    let function_decls =
      Closure_id.Map.union_both
        (fun f -> make_non_inlinable_function_declaration f)
        (fun f1 f2 -> join_results_and_make_non_inlinable f1 f2)
        t1.function_decls t2.function_decls
    in
    let closure_elements =
      Var_within_closure.Map.union_both
        (fun ty ->
          let scanning = scanning_ty_value ~type_of_name ty in
          any_value_as_ty_value scanning Other)
        (fun ty1 ty2 -> join_ty_value ~type_of_name ty1 ty2)
        t1.closure_elements t2.closure_elements
    in
    { set_of_closures_id_and_origin = Not_all_values_known;
      function_decls;
      closure_elements;
    }

  let join ~type_of_name (t1 : t) (t2 : t) : t =
    let set_of_closures_id_and_origin =
      Or_not_all_values_known.join (fun (id1, origin1) (id2, origin2) ->
          if Set_of_closures_id.equal id1 id2 then begin
            (* CR mshinwell: We should think more about [Set_of_closures_id]
               particularly in the context of recursive cases vs. the previous
               version of a set of closures *)
            assert (Set_of_closures_origin.equal origin1 origin2);
            Ok (id1, origin1)
          end else begin
            Wrong
          end)
        t1.set_of_closures_id_and_origin
        t2.set_of_closures_id_and_origin
    in
    match set_of_closures_id_and_origin with
    | Ok ((Exactly _) as set_of_closures_id_and_origin) ->
      (* If the [set_of_closures_id]s are the same, the result is eligible for
         inlining, when the input function declarations are.

         The real constraint is that the union of two functions is inlinable
         if either of the two functions can be replaced by the other.  As such
         our behaviour here is conservative but hopefully not too restrictive in
         practice. *)
      (* CR pchambart: this is too strong, but should hold in general.
         It can be kept for now to help debugging *)
      assert (t1.function_decls == t2.function_decls);
      let closure_elements =
        Var_within_closure.Map.union_merge
          (join_ty_value ~type_of_name)
          t1.closure_elements t2.closure_elements
      in
      { set_of_closures_id_and_origin;
        function_decls = t1.function_decls;
        closure_elements;
      }
    | Ok Not_all_values_known | Wrong ->
      (* CR mshinwell: Wrong needs to do something else *)
      join_and_make_all_functions_non_inlinable ~type_of_name t1 t2

  let create ~type_of_name (sets : Set_of_closures.t list)
        : t =
    let sets = List.map of_set_of_closures sets in
    match sets with
    | [] ->
      { (* CR mshinwell: This is a bit strange: should there be a proper
           constructor for "bottom" here? *)
        set_of_closures_id_and_origin = Not_all_values_known;
        function_decls = Closure_id.Map.empty;
        closure_elements = Var_within_closure.Map.empty;
      }
    | set::sets ->
      List.fold_left (fun result t ->
          join ~type_of_name result t)
        set sets

  let equal ~equal_type t1 t2 =
    Or_not_all_values_known.equal (fun (id1, origin1) (id2, origin2) ->
        Set_of_closures_id.equal id1 id2
          && Set_of_closures_origin.equal origin1 origin2)
      t1.set_of_closures_id_and_origin
      t2.set_of_closures_id_and_origin
    && Closure_id.Map.equal (equal_function_declaration ~equal_type)
      t1.function_decls
      t2.function_decls
    && Var_within_closure.Map.equal (fun ty_value1 ty_value2 ->
        equal_type (t_of_ty_value ty_value1) (t_of_ty_value ty_value2))
      t1.closure_elements
      t2.closure_elements
end

  let tags (t_values : t_values) : Targetint.Set.t Or_not_all_values_known.t =
    let singleton tag : _ Or_not_all_values_known.t =
      Exactly (Targetint.Set.singleton (Tag.to_targetint tag))
    in
    match t_values with
    | Unknown -> Not_all_values_known
    | Bottom -> Exactly Targetint.Set.empty
    | Blocks_and_tagged_immediates (blocks, imms) ->
      assert (not (Blocks.is_empty blocks));
      if not (Immediate.Set.is_empty imms) then Not_all_values_known
      else
        let tags =
          Tag.Scannable.Set.fold (fun tag tags ->
              Targetint.Set.add (Tag.Scannable.to_targetint tag) tags)
            (Blocks.tags blocks)
        in
        Exactly tags
    | Tagged_immediates_only _ -> Exactly Targetint.Set.empty
    | Boxed_floats _ -> singleton Tag.double_tag
    | Boxed_int32s _
    | Boxed_int64s _
    | Boxed_nativeints _ -> singleton Tag.custom_tag
    | Closures _ -> singleton Tag.closure_tag
    | Sets_of_closures _ -> singleton Tag.closure_tag
    | Strings _ -> singleton Tag.string_tag
    | Float_arrays _ -> singleton Tag.double_array_tag

let equal ~type_of_name t1 t2 =
  Evaluated.equal_type ~type_of_name t1 t2

let as_or_more_precise ~type_of_name t ~than =
  equal ~type_of_name t (meet ~type_of_name t than)

*)

let is_bottom ~type_of_name t =
  let simplified, _canonical_name = Simplified_type.create ~type_of_name t in
  Simplified_type.is_bottom simplified

let is_unknown ~type_of_name t =
  let simplified, _canonical_name = Simplified_type.create ~type_of_name t in
  Simplified_type.is_unknown simplified

let is_known ~type_of_name t =
  not (is_unknown ~type_of_name t)

let is_useful ~type_of_name t =
  let simplified, _canonical_name = Simplified_type.create ~type_of_name t in
  (not (Simplified_type.is_unknown simplified))
    && (not (Simplified_type.is_bottom simplified))

let all_not_useful ~type_of_name ts =
  List.for_all (fun t -> not (is_useful ~type_of_name t)) ts

type reification_result =
  | Term of Simple.t * t
  | Cannot_reify
  | Invalid

let reify ~type_of_name ~allow_free_variables t
      : reification_result =
  let original_t = t in
  let t, _canonical_name = resolve_aliases ~type_of_name t in
  let simplified, canonical_name = Simplified_type.create ~type_of_name t in
  if Simplified_type.is_bottom simplified then
    Invalid
  else
    let try_name () : reification_result =
      match canonical_name with
      | None -> Cannot_reify
      | Some name ->
        match name with
        | Var _ when not allow_free_variables -> Cannot_reify
        | Var _ | Symbol _ ->
          (* This is the only case where we return [Term] with a term that
             cannot be produced just from the type.  As such, we may wish to
             make the type more precise later, so we return an alias type rather
             than [t]. *)
          let kind = kind ~type_of_name t in
          let t = alias_type_of kind name in
          Term (Simple.name name, t)
    in
    match simplified with
    | Value ty_value ->
      begin match ty_value with
      | Unknown _ -> try_name ()
      | Bottom -> Invalid
      | Ok (Blocks_and_tagged_immediates blocks_imms) ->
        if not (Tag.Map.is_empty blocks_imms.blocks) then try_name ()
        else
          begin match
            Immediate.Or_unknown.Map.get_singleton blocks_imms.immediates
          with
          | Some (Ok imm, _) -> Term (Simple.const (Tagged_immediate imm), t)
          | Some (Unknown, _) | None -> try_name ()
          end
      | Ok (Boxed_number _) -> try_name ()
      | Ok (Closure _ | String _) -> try_name ()
      end
    | Simplified_type.Naked_number (ty_naked_number, _) ->
      begin match ty_naked_number with
      | Unknown _ -> try_name ()
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
    | Fabricated _ | Phantom _ -> Cannot_reify

(*
type 'a proof =
  | Proved of 'a
  | Unknown
  | Invalid

type 'a known_values = 'a Or_not_all_values_known.t proof

let prove_tagged_immediate ~type_of_name t
      : Immediate.Set.t known_values =
  let t_evaluated, _canonical_name =
    Evaluated.create ~type_of_name t
  in
  match t_evaluated with
  | Values values ->
    begin match values with
    | Unknown -> Unknown
    | Tagged_immediates_only imms -> Proved imms
    | Boxed_floats _
    | Blocks_and_tagged_immediates _
    | Bottom
    | Boxed_int32s _
    | Boxed_int64s _
    | Boxed_nativeints _
    | Closures _
    | Sets_of_closures _
    | Strings _
    | Float_arrays _ -> Invalid
    end
  | Naked_immediates _
  | Naked_floats _
  | Naked_int32s _
  | Naked_int64s _
  | Naked_nativeints _ ->
    Misc.fatal_errorf "Wrong kind for something claimed to be a tagged \
        immediate: %a"
      print t

let prove_naked_float ~type_of_name t
      : Numbers.Float_by_bit_pattern.Set.t known_values0 =
  let t_evaluated, _canonical_name =
    Evaluated.create ~type_of_name t
  in
  match t_evaluated with
  | Naked_floats fs -> fs
  | Values _
  | Naked_immediates _
  | Naked_int32s _
  | Naked_int64s _
  | Naked_nativeints _ ->
    Misc.fatal_errorf "Wrong kind for something claimed to be a naked \
        float: %a"
      print t

let prove_naked_int32 ~type_of_name t
      : Numbers.Int32.Set.t known_values0 =
  let t_evaluated, _canonical_name =
    Evaluated.create ~type_of_name t
  in
  match t_evaluated with
  | Naked_int32s ns -> ns
  | Values _
  | Naked_immediates _
  | Naked_floats _
  | Naked_int64s _
  | Naked_nativeints _ ->
    Misc.fatal_errorf "Wrong kind for something claimed to be a naked \
        int32: %a"
      print t

let prove_naked_int64 ~type_of_name t
      : Numbers.Int64.Set.t known_values0 =
  let t_evaluated, _canonical_name =
    Evaluated.create ~type_of_name t
  in
  match t_evaluated with
  | Naked_int64s ns -> ns
  | Values _
  | Naked_immediates _
  | Naked_floats _
  | Naked_int32s _
  | Naked_nativeints _ ->
    Misc.fatal_errorf "Wrong kind for something claimed to be a naked \
        int64: %a"
      print t

let prove_naked_nativeint ~type_of_name t
      : Numbers.Nativeint.Set.t known_values0 =
  let t_evaluated, _canonical_name =
    Evaluated.create ~type_of_name t
  in
  match t_evaluated with
  | Naked_nativeints ns -> ns
  | Values _
  | Naked_immediates _
  | Naked_floats _
  | Naked_int32s _
  | Naked_int64s _ ->
    Misc.fatal_errorf "Wrong kind for something claimed to be a naked \
        nativeint: %a"
      print t

let prove_blocks ~type_of_name t : Blocks.t proof =
  let t_evaluated, _canonical_name =
    Evaluated.create ~type_of_name t
  in
  match t_evaluated with
  | Values values ->
    begin match values with
    | Unknown -> Unknown
    | Blocks_and_tagged_immediates (blocks, _imms) -> Proved blocks
    | Tagged_immediates_only _
    | Boxed_floats _
    | Bottom
    | Boxed_int32s _
    | Boxed_int64s _
    | Boxed_nativeints _
    | Closures _
    | Sets_of_closures _
    | Strings _
    | Float_arrays _ -> Invalid
    end
  | Naked_immediates _
  | Naked_floats _
  | Naked_int32s _
  | Naked_int64s _
  | Naked_nativeints _ ->
    Misc.fatal_errorf "Wrong kind for something claimed to be a block: %a"
      print t

let prove_blocks_and_immediates ~type_of_name t
      : (Blocks.t * (Immediate.Set.t Or_not_all_values_known.t)) proof =
  let t_evaluated, _canonical_name =
    Evaluated.create ~type_of_name t
  in
  match t_evaluated with
  | Values values ->
    begin match values with
    | Unknown -> Unknown
    | Blocks_and_tagged_immediates (blocks, imms) -> Proved (blocks, imms)
    | Tagged_immediates_only _
    | Boxed_floats _
    | Bottom
    | Boxed_int32s _
    | Boxed_int64s _
    | Boxed_nativeints _
    | Closures _
    | Sets_of_closures _
    | Strings _
    | Float_arrays _ -> Invalid
    end
  | Naked_immediates _
  | Naked_floats _
  | Naked_int32s _
  | Naked_int64s _
  | Naked_nativeints _ ->
    Misc.fatal_errorf "Wrong kind for something claimed to be a variant: %a"
      print t

let prove_float_array ~type_of_name t
      : Float_array.t list known_values =
  let t_evaluated, _canonical_name =
    Evaluated.create ~type_of_name t
  in
  match t_evaluated with
  | Values values ->
    begin match values with
    | Unknown -> Unknown
    | Float_arrays float_arrays -> Proved float_arrays
    | Strings _
    | Tagged_immediates_only _
    | Boxed_nativeints _
    | Blocks_and_tagged_immediates _
    | Bottom
    | Boxed_floats _
    | Boxed_int32s _
    | Boxed_int64s _
    | Closures _
    | Sets_of_closures _ -> Invalid
    end
  | Naked_immediates _
  | Naked_floats _
  | Naked_int32s _
  | Naked_int64s _
  | Naked_nativeints _ ->
    Misc.fatal_errorf "Wrong kind for something claimed to be a float \
        array: %a"
      print t

let prove_string ~type_of_name t : String_info.Set.t known_values =
  let t_evaluated, _canonical_name =
    Evaluated.create ~type_of_name t
  in
  match t_evaluated with
  | Values values ->
    begin match values with
    | Unknown -> Unknown
    | Strings strs -> Proved strs
    | Tagged_immediates_only _
    | Boxed_nativeints _
    | Blocks_and_tagged_immediates _
    | Bottom
    | Boxed_floats _
    | Boxed_int32s _
    | Boxed_int64s _
    | Closures _
    | Sets_of_closures _
    | Float_arrays _ -> Invalid
    end
  | Naked_immediates _
  | Naked_floats _
  | Naked_int32s _
  | Naked_int64s _
  | Naked_nativeints _ ->
    Misc.fatal_errorf "Wrong kind for something claimed to be a string: %a"
      print t

let prove_boxed_int32 ~type_of_name t : ty_naked_int32 proof =
  let t_evaluated, _canonical_name =
    Evaluated.create ~type_of_name t
  in
  match t_evaluated with
  | Values values ->
    begin match values with
    | Unknown -> Unknown
    | Boxed_int32s ns -> Proved ns
    | Blocks_and_tagged_immediates _
    | Bottom
    | Tagged_immediates_only _
    | Boxed_floats _
    | Boxed_int64s _
    | Boxed_nativeints _
    | Closures _
    | Sets_of_closures _
    | Strings _
    | Float_arrays _ -> Invalid
    end
  | Naked_immediates _
  | Naked_floats _
  | Naked_int32s _
  | Naked_int64s _
  | Naked_nativeints _ ->
    Misc.fatal_errorf "Wrong kind for something claimed to be a boxed \
        int32: %a"
      print t

let prove_boxed_int64 ~type_of_name t : ty_naked_int64 proof =
  let t_evaluated, _canonical_name =
    Evaluated.create ~type_of_name t
  in
  match t_evaluated with
  | Values values ->
    begin match values with
    | Unknown -> Unknown
    | Boxed_int64s ns -> Proved ns
    | Blocks_and_tagged_immediates _
    | Bottom
    | Tagged_immediates_only _
    | Boxed_floats _
    | Boxed_int32s _
    | Boxed_nativeints _
    | Closures _
    | Sets_of_closures _
    | Strings _
    | Float_arrays _ -> Invalid
    end
  | Naked_immediates _
  | Naked_floats _
  | Naked_int32s _
  | Naked_int64s _
  | Naked_nativeints _ ->
    Misc.fatal_errorf "Wrong kind for something claimed to be a boxed \
        int64: %a"
      print t

let prove_boxed_nativeint ~type_of_name t : ty_naked_nativeint proof =
  let t_evaluated, _canonical_name =
    Evaluated.create ~type_of_name t
  in
  match t_evaluated with
  | Values values ->
    begin match values with
    | Unknown -> Unknown
    | Boxed_nativeints ns -> Proved ns
    | Blocks_and_tagged_immediates _
    | Bottom
    | Tagged_immediates_only _
    | Boxed_floats _
    | Boxed_int32s _
    | Boxed_int64s _
    | Closures _
    | Sets_of_closures _
    | Strings _
    | Float_arrays _ -> Invalid
    end
  | Naked_immediates _
  | Naked_floats _
  | Naked_int32s _
  | Naked_int64s _
  | Naked_nativeints _ ->
    Misc.fatal_errorf "Wrong kind for something claimed to be a boxed \
        nativeint: %a"
      print t

let prove_boxed_float ~type_of_name t : ty_naked_float Proof.t =
  fold t
    ~init:(Invalid : _ Proof.t)
    ~singleton:(fun (proof : _ Proof.t)
            (singleton : of_kind_value) : _ Proof.t ->
      match singleton with
      | Boxed_float ty_naked_float -> Ok ty_naked_float
      | Tagged_immediate _
      | Boxed_int32 _
      | Boxed_int64 _
      | Boxed_nativeint _
      | Block _
      | Closure _
      | String _
      | Float_array _ -> Invalid)
    ~meet:(fun proof (constr : constraint_of_kind_value) : _ or_invalid ->
      match constr with
      | Boxed_float -> Ok proof
      | Tagged_immediate
      | Boxed_int32
      | Boxed_int64
      | Boxed_nativeint
      | Block _
      | Closure _
      | String
      | Float_array _ -> Invalid)
    ~join:(fun proofs ->
      Proof.join_list proofs
        ~join_contents:(fun ty_naked_float1 ty_naked_float2 ->
          join_ty_naked_float ~type_of_name
            ty_naked_float1 ty_naked_float2))


let prove_boxed_float ~type_of_name t : ty_naked_float proof =
  let t_evaluated, _canonical_name =
    Evaluated.create ~type_of_name t
  in
  match t_evaluated with
  | Values values ->
    begin match values with
    | Unknown -> Unknown
    | Boxed_floats ns -> Proved ns
    | Blocks_and_tagged_immediates _
    | Bottom
    | Tagged_immediates_only _
    | Boxed_int32s _
    | Boxed_int64s _
    | Boxed_nativeints _
    | Closures _
    | Sets_of_closures _
    | Strings _
    | Float_arrays _ -> Invalid
    end
  | Naked_immediates _
  | Naked_floats _
  | Naked_int32s _
  | Naked_int64s _
  | Naked_nativeints _ ->
    Misc.fatal_errorf "Wrong kind for something claimed to be a boxed \
        float: %a"
      print t

let prove_closures ~type_of_name t : Joined_closures.t known_values =
  let t_evaluated, _canonical_name =
    Evaluated.create ~type_of_name t
  in
  match t_evaluated with
  | Values values ->
    begin match values with
    | Unknown -> Unknown
    | Closures closures -> Proved closures
    | Bottom
    | Boxed_floats _
    | Blocks_and_tagged_immediates _
    | Tagged_immediates_only _
    | Boxed_int32s _
    | Boxed_int64s _
    | Boxed_nativeints _
    | Sets_of_closures _
    | Strings _
    | Float_arrays _ -> Invalid
    end
  | Naked_immediates _
  | Naked_floats _
  | Naked_int32s _
  | Naked_int64s _
  | Naked_nativeints _ ->
    Misc.fatal_errorf "Wrong kind for something claimed to be one or more \
        closures: %a"
      print t

let prove_sets_of_closures ~type_of_name t
      : Joined_sets_of_closures.t known_values =
  let t_evaluated, _canonical_name =
    Evaluated.create ~type_of_name t
  in
  match t_evaluated with
  | Values values ->
    begin match values with
    | Unknown -> Unknown
    | Sets_of_closures set -> Proved set
    | Bottom
    | Boxed_floats _
    | Blocks_and_tagged_immediates _
    | Tagged_immediates_only _
    | Boxed_int32s _
    | Boxed_int64s _
    | Boxed_nativeints _
    | Closures _
    | Strings _
    | Float_arrays _ -> Invalid
    end
  | Naked_immediates _
  | Naked_floats _
  | Naked_int32s _
  | Naked_int64s _
  | Naked_nativeints _ ->
    Misc.fatal_errorf "Wrong kind for something claimed to be a set of \
        closures: %a"
      print t

(* XXX What about [Obj.truncate]?
   In fact, what happens regarding this for block access too? *)

(* XXX Lengths of strings: for this, I think we can assume that Obj.truncate
   is always illegal here *)

let prove_lengths_of_arrays_or_blocks ~type_of_name t
      : Targetint.OCaml.Set.t proof =
  let t_evaluated, _canonical_name =
    Evaluated.create ~type_of_name t
  in
  match t_evaluated with
  | Values values ->
    begin match values with
    | Unknown -> Unknown
    | Float_arrays Not_all_values_known -> Unknown
    | Float_arrays (Exactly float_arrays) ->
      let sizes = List.map Float_array.size float_arrays in
      Proved (Targetint.OCaml.Set.of_list sizes)
    | Blocks_and_tagged_immediates (blocks, _) ->
      Proved (Blocks.all_possible_sizes blocks)
    | Boxed_floats _
    | Bottom
    | Blocks_and_tagged_immediates _
    | Tagged_immediates_only _
    | Boxed_int32s _
    | Boxed_int64s _
    | Boxed_nativeints _
    | Closures _
    | Sets_of_closures _
    | Strings _ -> Invalid
    end
  | Naked_immediates _
  | Naked_floats _
  | Naked_int32s _
  | Naked_int64s _
  | Naked_nativeints _ ->
    Misc.fatal_errorf "Wrong kind for something claimed to be an array \
        or structured block: %a"
      print t

let prove_is_tagged_immediate ~type_of_name t : bool proof =
  let t_evaluated, _canonical_name =
    Evaluated.create ~type_of_name t
  in
  match t_evaluated with
  | Values values ->
    begin match values with
    | Unknown -> Unknown
    | Tagged_immediates_only _ -> Proved true
    | Boxed_floats _
    | Blocks_and_tagged_immediates _
    | Bottom
    | Boxed_int32s _
    | Boxed_int64s _
    | Boxed_nativeints _
    | Closures _
    | Sets_of_closures _
    | Strings _
    | Float_arrays _ -> Proved false
    end
  | Naked_immediates _
  | Naked_floats _
  | Naked_int32s _
  | Naked_int64s _
  | Naked_nativeints _ -> Invalid

let force_to_kind_value_with_expected_value_kind ~type_of_name
        t expected_kind =
  let ty_value = force_to_kind_value t in
  let actual_kind = value_kind ~type_of_name ty_value in
  if not (Flambda_kind.compatible actual_kind ~if_used_at:expected_kind)
  then begin
    Misc.fatal_errorf "Type should be compatible with kind [Value %a] but \
        is not: %a"
      Flambda_kind.print_value_kind expected_kind
      print t
    end

let force_to_kind_value_with_expected_value_kinds ~type_of_name
        ts expected_kind =
  List.iter (fun t ->
      force_to_kind_value_with_expected_value_kind ~type_of_name
        t expected_kind)
    ts

let force_to_kind_value_with_expected_value_kinds ~type_of_name
        ts_and_expected_kinds =
  List.iter (fun (t, expected_kind) ->
      force_to_kind_value_with_expected_value_kind ~type_of_name
        t expected_kind)
    ts_and_expected_kinds

let force_to_kind_naked_float_list ts =
  List.iter force_to_kind_naked_float ts

let tags ~type_of_name t =
  let t_evaluated, _canonical_name =
    Evaluated.create ~type_of_name t
  in
  match t_evaluated with
  | Values values -> Evaluated.tags values
  | Naked_immediates _
  | Naked_floats _
  | Naked_int32s _
  | Naked_int64s _
  | Naked_nativeints _ ->
    Misc.fatal_errorf "Type should be of kind [Value] but is not: %a"
      print t

type switch_branch_classification =
  | Cannot_be_taken
  | Can_be_taken
  | Must_be_taken

let classify_switch_branch ~type_of_name t ~scrutinee branch
      : switch_branch_classification =
  let t_evaluated, _canonical_name =
    Evaluated.create ~type_of_name t
  in
  match t_evaluated with
  | Values values ->
    begin match values with
    | Unknown
    | Tagged_immediates_only Not_all_values_known -> Can_be_taken
    | Tagged_immediates_only (Exactly all_possible_values) ->
      let all_possible_values =
        Immediate.set_to_targetint_set all_possible_values
      in
      if Targetint.Set.mem branch all_possible_values then Must_be_taken
      else Cannot_be_taken
    | Bottom
    | Blocks_and_tagged_immediates _
    | Boxed_floats _
    | Boxed_int32s _
    | Boxed_int64s _
    | Boxed_nativeints _
    | Closures _
    | Sets_of_closures _
    | Strings _
    | Float_arrays _ -> Cannot_be_taken
    end
  | Naked_immediates _
  | Naked_floats _
  | Naked_int32s _
  | Naked_int64s _
  | Naked_nativeints _ ->
    Misc.fatal_errorf "Switch on %a has wrong kind: the scrutinee must have \
        kind [Value]"
      Name.print scrutinee

(* New stuff *)

(* [prove_tag] does something like [reify] *)

module Blocks : sig
  type t = private ty_value array length_constraint Tag.Scannable.Map.t

  val add
     : (t
    -> tags:Tag.Scannable.Set.t
    -> fields:ty_value array length_constraint
    -> t) type_accessor
end = struct
  type t = ty_value array length_constraint Tag.Scannable.Map.t

  let add_one_tag ~type_of_name t ~tag
        ~(fields : ty_value length_constraint) =
    match Tag.Scannable.Map.find t tag with
    | exception Not_found -> Tag.Scannable.Map.add tag fields t
    | existing_fields ->
      let fields : ty_value length_constraint or_invalid =
        match existing_fields, fields with
        | Of_length exact_size1, Of_length exact_size2 ->
          if not (Targetint.OCaml.equal exact_size1 exact_size2) then Invalid
          else Ok (Of_length exact_size1)
        | Exactly fields, Of_length exact_size1
        | Of_length exact_size1, Exactly fields ->
          let exact_size2 = Array.length fields in
          if not (Targetint.OCaml.equal exact_size1 exact_size2) then Invalid
          else Ok (Exactly fields)
        | Exactly fields1, Exactly fields2 ->
          if Array.length fields1 <> Array.length fields2 then Invalid
          else
            let fields =
              Array.map2 (fun field1 field2 ->
                  meet_ty_value ~type_of_name field1 field2)
                fields1 fields2
            in
            Ok (Exactly fields)
      in
      match fields with
      | Invalid -> t
      | Ok fields -> Tag.Scannable.Map.add tag fields t

  let add t ~tags ~(fields : ty_value array length_constraint) =
    Tag.Scannable.Set.fold (fun tag t ->
        add_one_tag t ~tag ~fields)
      tags
end

let prove_blocks ~type_of_name t : ... Proof.t =
  fold t
    ~init:(Invalid : _ Proof.t)
    ~singleton:(fun (proof : _ Proof.t)
            (singleton : of_kind_value) : blocks ->
      match singleton with
      | Blocks { env; tag; fields; } ->
        let tag_proof = prove_tag ~type_of_name tag in
        begin match tag_proof with
        | Invalid -> Invalid
        | Unknown -> Unknown
        | Ok tags ->
          let 
        end
      | Tagged_immediate _
      | Boxed_float _
      | Boxed_int32 _
      | Boxed_int64 _
      | Boxed_nativeint _
      | Block _
      | Closure _
      | String _
      | Float_array _ -> Invalid)


    ~meet:(fun proof (constr : constraint_of_kind_value) : _ or_invalid ->
      match constr with
      | Boxed_float -> Ok proof
      | Tagged_immediate
      | Boxed_int32
      | Boxed_int64
      | Boxed_nativeint
      | Block _
      | Closure _
      | String
      | Float_array _ -> Invalid)
    ~join:(fun proofs ->
      Proof.join_list proofs
        ~join_contents:(fun ty_naked_float1 ty_naked_float2 ->
          join_ty_naked_float ~type_of_name
            ty_naked_float1 ty_naked_float2))
*)
