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

let unknown_like_array ~type_of_name t_array =
  Array.map (fun t -> unknown_like ~type_of_name t) t_array

let rename_variables t ~f:_ = t
(* XXX need to fix [Flambda_type0.clean]
  clean t (fun var -> Available_different_name (f var))
*)

let unit () =
  this_tagged_immediate Immediate.zero

let unit_bottom () =
  bottom (K.value Definitely_immediate)

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

(*
let _equal_function_declaration ~equal_type
      (decl1 : function_declarations)
      (decl2 : function_declarations) =
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
*)

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

*)

type 'a or_wrong =
  | Ok of 'a
  | Wrong

module Or_not_all_values_known = struct
  type 'a t =
    | Exactly of 'a
    | Not_all_values_known

  let _join join_contents t1 t2 : _ t or_wrong =
    match t1, t2 with
    | Exactly e1, Exactly e2 ->
      begin match join_contents e1 e2 with
      | Ok e -> Ok (Exactly e)
      | Wrong -> Wrong
      end
    | Exactly _, Not_all_values_known
    | Not_all_values_known, Exactly _
    | Not_all_values_known, Not_all_values_known -> Ok Not_all_values_known

  let _meet meet_contents t1 t2 : _ t or_wrong =
    match t1, t2 with
    | Exactly e1, Exactly e2 ->
      begin match meet_contents e1 e2 with
      | Ok e -> Ok (Exactly e)
      | Wrong -> Wrong
      end
    | Exactly _, Not_all_values_known -> Ok t1
    | Not_all_values_known, Exactly _ -> Ok t2
    | Not_all_values_known, Not_all_values_known -> Ok Not_all_values_known

  let _equal equal_contents t1 t2 =
    match t1, t2 with
    | Exactly c1, Exactly c2 -> equal_contents c1 c2
    | Not_all_values_known, Not_all_values_known -> true
    | Exactly _, Not_all_values_known
    | Not_all_values_known, Exactly _ -> false

  let _print f ppf t =
    match t with
    | Exactly thing -> f ppf thing
    | Not_all_values_known -> Format.pp_print_string ppf "Not_all_values_known"
end

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

type 'a proof =
  | Proved of 'a
  | Unknown
  | Invalid

type 'a known_values = 'a Or_not_all_values_known.t proof

let unknown_proof () = Unknown

let prove_naked_float ~type_of_name t
      : Numbers.Float_by_bit_pattern.Set.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a naked \
        float: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create ~type_of_name t in
  match simplified with
  | Simplified_type.Naked_number (ty, K.Naked_number.Naked_float) ->
    begin match ty with
    | Unknown _ -> Unknown
    | Bottom -> Invalid
    | Ok (Float fs) -> Proved fs
    | Ok _ ->
      (* CR mshinwell: Find out why this case is still possible *)
      wrong_kind ()
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Value _
  | Fabricated _
  | Phantom _ -> wrong_kind ()

let prove_naked_int32 ~type_of_name t : Int32.Set.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a naked \
        int32: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create ~type_of_name t in
  match simplified with
  | Simplified_type.Naked_number (ty, K.Naked_number.Naked_int32) ->
    begin match ty with
    | Unknown _ -> Unknown
    | Bottom -> Invalid
    | Ok (Int32 is) -> Proved is
    | Ok _ -> wrong_kind ()
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Value _
  | Fabricated _
  | Phantom _ -> wrong_kind ()

let prove_naked_int64 ~type_of_name t : Int64.Set.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a naked \
        int64: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create ~type_of_name t in
  match simplified with
  | Simplified_type.Naked_number (ty, K.Naked_number.Naked_int64) ->
    begin match ty with
    | Unknown _ -> Unknown
    | Bottom -> Invalid
    | Ok (Int64 is) -> Proved is
    | Ok _ -> wrong_kind ()
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Value _
  | Fabricated _
  | Phantom _ -> wrong_kind ()

let prove_naked_nativeint ~type_of_name t : Targetint.Set.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a naked \
        nativeint: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create ~type_of_name t in
  match simplified with
  | Simplified_type.Naked_number (ty, K.Naked_number.Naked_nativeint) ->
    begin match ty with
    | Unknown _ -> Unknown
    | Bottom -> Invalid
    | Ok (Nativeint is) -> Proved is
    | Ok _ -> wrong_kind ()
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Value _
  | Fabricated _
  | Phantom _ -> wrong_kind ()

let prove_unique_naked_float ~type_of_name t : _ proof =
  match prove_naked_float ~type_of_name t with
  | Proved fs ->
    begin match Float_by_bit_pattern.Set.get_singleton fs with
    | Some f -> Proved f
    | None -> Unknown
    end
  | Unknown -> Unknown
  | Invalid -> Invalid

let prove_unique_naked_int32 ~type_of_name t : _ proof =
  match prove_naked_int32 ~type_of_name t with
  | Proved is ->
    begin match Int32.Set.get_singleton is with
    | Some f -> Proved f
    | None -> Unknown
    end
  | Unknown -> Unknown
  | Invalid -> Invalid

let prove_unique_naked_int64 ~type_of_name t : _ proof =
  match prove_naked_int64 ~type_of_name t with
  | Proved is ->
    begin match Int64.Set.get_singleton is with
    | Some f -> Proved f
    | None -> Unknown
    end
  | Unknown -> Unknown
  | Invalid -> Invalid

let prove_unique_naked_nativeint ~type_of_name t : _ proof =
  match prove_naked_nativeint ~type_of_name t with
  | Proved is ->
    begin match Targetint.Set.get_singleton is with
    | Some f -> Proved f
    | None -> Unknown
    end
  | Unknown -> Unknown
  | Invalid -> Invalid

let prove_closure ~type_of_name t : _ proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a closure: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create ~type_of_name t in
  match simplified with
  | Fabricated ty_fabricated ->
    begin match ty_fabricated with
    | Unknown _ -> Unknown
    | Bottom -> Invalid
    | Ok (Closure closure) -> Proved closure
    | Ok _ -> Invalid
    end
  | Value _ -> wrong_kind ()
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Phantom _ -> wrong_kind ()

type reification_result =
  | Term of Simple.t * t
  | Lift of Flambda_static0.Static_part.t
  | Cannot_reify
  | Invalid

let reify ~type_of_name ~allow_free_variables t : reification_result =
  let t, canonical_name = resolve_aliases ~type_of_name t in
(*
Format.eprintf "CN is %a\n%!" (Misc.Stdlib.Option.print Name.print)
  canonical_name;
*)
  let can_lift =
    Name.Set.for_all (fun (name : Name.t) ->
        match name with
        | Var _ -> false
        | Symbol _ -> true)
      (free_names t)
  in
  let simplified, _canonical_name = Simplified_type.create ~type_of_name t in
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
          begin match blocks_imms.immediates with
          | Unknown -> try_name ()
          | Known imms ->
            begin match Immediate.Map.get_singleton imms with
            | Some (imm, _) -> Term (Simple.const (Tagged_immediate imm), t)
            | None -> try_name ()
            end
          end
      | Ok (Boxed_number (Boxed_float ty_naked_number)) ->
        begin match canonical_name with
        | Some ((Symbol _) as name) ->
          let kind = K.value Definitely_pointer in
          let t = alias_type_of kind name in
          Term (Simple.name name, t)
        | Some (Var _) | None ->
          if not can_lift then Cannot_reify
          else
            let contents =
              of_ty_naked_number ty_naked_number K.Naked_number.Naked_float
            in
            match prove_unique_naked_float ~type_of_name contents with
            | Proved f -> Lift (Boxed_float (Const f))
            | Unknown -> try_name ()
            | Invalid -> Cannot_reify
        end
      (* CR mshinwell: Factor out boxed number cases *)
      | Ok (Boxed_number (Boxed_int32 ty_naked_number)) ->
        begin match canonical_name with
        | Some ((Symbol _) as name) ->
          let kind = K.value Definitely_pointer in
          let t = alias_type_of kind name in
          Term (Simple.name name, t)
        | Some (Var _) | None ->
          if not can_lift then Cannot_reify
          else
            let contents =
              of_ty_naked_number ty_naked_number K.Naked_number.Naked_int32
            in
            match prove_unique_naked_int32 ~type_of_name contents with
            | Proved f -> Lift (Boxed_int32 (Const f))
            | Unknown -> try_name ()
            | Invalid -> Cannot_reify
        end
      | Ok (Boxed_number (Boxed_int64 ty_naked_number)) ->
        begin match canonical_name with
        | Some ((Symbol _) as name) ->
          let kind = K.value Definitely_pointer in
          let t = alias_type_of kind name in
          Term (Simple.name name, t)
        | Some (Var _) | None ->
          if not can_lift then Cannot_reify
          else
            let contents =
              of_ty_naked_number ty_naked_number K.Naked_number.Naked_int64
            in
            match prove_unique_naked_int64 ~type_of_name contents with
            | Proved f -> Lift (Boxed_int64 (Const f))
            | Unknown -> try_name ()
            | Invalid -> Cannot_reify
        end
      | Ok (Boxed_number (Boxed_nativeint ty_naked_number)) ->
        begin match canonical_name with
        | Some ((Symbol _) as name) ->
          let kind = K.value Definitely_pointer in
          let t = alias_type_of kind name in
          Term (Simple.name name, t)
        | Some (Var _) | None ->
          if not can_lift then Cannot_reify
          else
            let contents =
              of_ty_naked_number ty_naked_number K.Naked_number.Naked_nativeint
            in
            match prove_unique_naked_nativeint ~type_of_name contents with
            | Proved f -> Lift (Boxed_nativeint (Const f))
            | Unknown -> try_name ()
            | Invalid -> Cannot_reify
        end
      | Ok (Closures _ | String _) -> try_name ()
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
    | Fabricated (Ok (Set_of_closures set_of_closures)) ->
      begin match canonical_name with
      | Some ((Symbol _) as name) ->
        let kind = K.fabricated Definitely_pointer in
        let t = alias_type_of kind name in
        Term (Simple.name name, t)
      | Some (Var _) | None ->
        if not can_lift then Cannot_reify
        else
          begin match set_of_closures.closures,
              set_of_closures.closure_elements
          with
          | Open _, _ | _, Open _ -> Cannot_reify
          | Exactly closures, Exactly closure_elements ->
            (* The following assertion holds since [can_lift] is [true]. *)
            assert (Var_within_closure.Map.is_empty closure_elements);
            let cannot_lift = ref false in
            let closures =
              Closure_id.Map.filter_map closures
                ~f:(fun _closure_id ty_fabricated ->
                  let t = of_ty_fabricated ty_fabricated in
                  match prove_closure ~type_of_name t with
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
                          Flambda0.Typed_parameter.create ~type_of_name param ty)
                        decl.params
                    in
                    let return_arity =
                      List.map (fun t -> kind ~type_of_name t) decl.result
                    in
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
            if !cannot_lift then Cannot_reify
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
    | Fabricated _
    | Phantom _ -> Cannot_reify

let prove_tagged_immediate ~type_of_name t
      : Immediate.Set.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a tagged \
        immediate: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create ~type_of_name t in
  match simplified with
  | Value ty_value ->
    begin match ty_value with
    | Unknown _ -> Unknown
    | Bottom -> Invalid
    | Ok (Blocks_and_tagged_immediates blocks_imms) ->
      if not (Tag.Map.is_empty blocks_imms.blocks) then begin
        Invalid
      end else begin
        match blocks_imms.immediates with
        | Unknown -> Unknown
        | Known imms ->
          assert (not (Immediate.Map.is_empty imms));
          Proved (Immediate.Map.keys imms)
        end
    | Ok (Boxed_number _) -> Invalid
    | Ok (Closures _ | String _) -> Invalid
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _
  | Phantom _ -> wrong_kind ()

let prove_is_tagged_immediate ~type_of_name t : bool proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a tagged \
        immediate: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create ~type_of_name t in
  match simplified with
  | Value ty_value ->
    begin match ty_value with
    | Unknown _ -> Unknown
    | Bottom -> Invalid
    | Ok (Blocks_and_tagged_immediates blocks_imms) ->
      if not (Tag.Map.is_empty blocks_imms.blocks) then begin
        Proved false
      end else begin
        match blocks_imms.immediates with
        | Unknown -> Proved true
        | Known imms ->
          assert (not (Immediate.Map.is_empty imms));
          Proved true
      end
    (* CR mshinwell: If we marked this function as specifically for dealing
       with the "is_int" primitive, then these next cases could probably
       all be [Invalid]. *)
    | Ok (Boxed_number _) -> Proved false
    | Ok (Closures _ | String _) -> Proved false
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _
  | Phantom _ -> wrong_kind ()

let valid_block_tag_for_kind ~tag ~(field_kind : K.t) =
  (* CR-someday mshinwell: Note that we could easily extend
     this to handle blocks of the other unboxed number kinds. *)
  match field_kind with
  | Value _ -> Tag.is_structured_block tag
  | Fabricated _ -> Tag.equal tag Tag.zero
  | Naked_number Naked_float -> Tag.equal tag Tag.double_array_tag
  | Naked_number _
  | Phantom _ ->
    Misc.fatal_errorf "Bad kind for block field: %a"
      K.print field_kind

let prove_get_field_from_block ~type_of_name t ~index ~field_kind : t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a block: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create ~type_of_name t in
  match simplified with
  | Value ty_value ->
    begin match ty_value with
    | Unknown _ -> Unknown
    | Bottom -> Invalid
    | Ok (Blocks_and_tagged_immediates blocks_imms) ->
      if Targetint.OCaml.compare index Targetint.OCaml.zero < 0 then Invalid
      else
        let no_immediates =
          match blocks_imms.immediates with
          | Known imms when Immediate.Map.is_empty imms -> true
          | Known _ | Unknown -> false
        in
        if not no_immediates then begin
          Invalid
        end else begin
          assert (not (Tag.Map.is_empty blocks_imms.blocks));
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
                          | Immutable this_field_ty -> this_field_ty
                          | Mutable -> unknown field_kind
                        in
                        match field_ty with
                        | None -> None
                        | Some field_ty ->
                          let field_ty =
                            join ~type_of_name this_field_ty field_ty
                          in
                          Some field_ty
                      end)
                    by_length
                    field_ty)
              blocks_imms.blocks
              (Some (bottom field_kind))
          in
          match field_ty with
          | None -> Invalid
          | Some field_ty -> Proved field_ty
        end 
    | Ok (Boxed_number _) -> Invalid
    | Ok (Closures _ | String _) -> Invalid
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _
  | Phantom _ -> wrong_kind ()

let prove_is_a_block ~type_of_name t ~kind_of_all_fields : bool proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a block: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create ~type_of_name t in
  match simplified with
  | Value ty_value ->
    begin match ty_value with
    | Unknown _ -> Unknown
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
        assert (not (Tag.Map.is_empty blocks_imms.blocks));
        let tags_all_valid =
          Tag.Map.for_all (fun tag ((Join { by_length; }) : block_cases) ->
              Targetint.OCaml.Map.iter (fun _length (block : singleton_block) ->
                  Array.iter (fun (field : _ mutable_or_immutable) ->
                      match field with
                      | Mutable -> ()
                      | Immutable field ->
                        let field_kind = kind ~type_of_name field in
                        let compatible =
                          K.compatible field_kind ~if_used_at:kind_of_all_fields
                        in
                        if not compatible then begin
                          Misc.fatal_errorf "Kind %a is not compatible with \
                              all fields of this block: %a"
                            K.print kind_of_all_fields
                            print t
                        end)
                    block.fields)
                by_length;
              valid_block_tag_for_kind ~tag ~field_kind:kind_of_all_fields)
            blocks_imms.blocks
        in
        if tags_all_valid then Proved true else Invalid
      end
    | Ok (Boxed_number _) -> Proved false
    | Ok (Closures _ | String _) -> Proved false
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _
  | Phantom _ -> wrong_kind ()

(* This can wait until we fix the unboxing passes.  It will just need to
   return a map from tags to field arrays.

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
*)

(* CR mshinwell: There's a bit of a wart here (in conjunction with the
   [Get_tag] primitive -- some of these tags don't really make any sense in
   that context, e.g. for closures, since there isn't a
   [Blocks_and_tagged_immediates] description for them.  Double_array_tag
   is of course an exception... maybe there should be a submodule of Tag
   which permits < No_scan_tag and also Double_array_tag? *)
let prove_tags ~type_of_name t : Tag.Set.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a value: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create ~type_of_name t in
  match simplified with
  | Value ty_value ->
    begin match ty_value with
    | Unknown _ -> Unknown
    | Bottom -> Invalid
    | Ok (Blocks_and_tagged_immediates blocks_imms) ->
      Proved (Tag.Map.keys blocks_imms.blocks)
    | Ok (Boxed_number (Boxed_float _)) ->
      Proved (Tag.Set.singleton Tag.double_tag)
    | Ok (Boxed_number (Boxed_int32 _)) ->
      Proved (Tag.Set.singleton Tag.custom_tag)
    | Ok (Boxed_number (Boxed_int64 _)) ->
      Proved (Tag.Set.singleton Tag.custom_tag)
    | Ok (Boxed_number (Boxed_nativeint _)) ->
      Proved (Tag.Set.singleton Tag.custom_tag)
    | Ok (Closures _) ->
      Proved (Tag.Set.singleton Tag.closure_tag)
    | Ok (String _) ->
      Proved (Tag.Set.singleton Tag.string_tag)
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _
  | Phantom _ -> wrong_kind ()

let prove_string ~type_of_name t : String_info.Set.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a string: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create ~type_of_name t in
  match simplified with
  | Value ty_value ->
    begin match ty_value with
    | Unknown _ -> Unknown
    | Bottom -> Invalid
    | Ok (String strs) -> Proved strs
    | Ok (Blocks_and_tagged_immediates _ | Closures _) -> Invalid
    | Ok (Boxed_number _) -> Invalid
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _
  | Phantom _ -> wrong_kind ()

let prove_boxed_float ~type_of_name t
      : Float_by_bit_pattern.Set.t ty_naked_number proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a boxed \
        float: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create ~type_of_name t in
  match simplified with
  | Value ty_value ->
    begin match ty_value with
    | Unknown _ -> Unknown
    | Bottom -> Invalid
    | Ok (Boxed_number (Boxed_float ty_naked_number)) -> Proved ty_naked_number
    | Ok _ -> Invalid
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _
  | Phantom _ -> wrong_kind ()

let prove_boxed_int32 ~type_of_name t
      : Int32.Set.t ty_naked_number proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a boxed \
        int32: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create ~type_of_name t in
  match simplified with
  | Value ty_value ->
    begin match ty_value with
    | Unknown _ -> Unknown
    | Bottom -> Invalid
    | Ok (Boxed_number (Boxed_int32 ty_naked_number)) -> Proved ty_naked_number
    | Ok _ -> Invalid
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _
  | Phantom _ -> wrong_kind ()

let prove_boxed_int64 ~type_of_name t
      : Int64.Set.t ty_naked_number proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a boxed \
        int64: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create ~type_of_name t in
  match simplified with
  | Value ty_value ->
    begin match ty_value with
    | Unknown _ -> Unknown
    | Bottom -> Invalid
    | Ok (Boxed_number (Boxed_int64 ty_naked_number)) -> Proved ty_naked_number
    | Ok _ -> Invalid
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _
  | Phantom _ -> wrong_kind ()

let prove_boxed_nativeint ~type_of_name t
      : Targetint.Set.t ty_naked_number proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a boxed \
        nativeint: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create ~type_of_name t in
  match simplified with
  | Value ty_value ->
    begin match ty_value with
    | Unknown _ -> Unknown
    | Bottom -> Invalid
    | Ok (Boxed_number (Boxed_nativeint ty_naked_number)) ->
      Proved ty_naked_number
    | Ok _ -> Invalid
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _
  | Phantom _ -> wrong_kind ()

let prove_closures ~type_of_name t : closures proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be one or more \
        closures: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create ~type_of_name t in
  match simplified with
  | Value ty_value ->
    begin match ty_value with
    | Unknown _ -> Unknown
    | Bottom -> Invalid
    | Ok (Closures closures) -> Proved closures
    | Ok _ -> Invalid
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _
  | Phantom _ -> wrong_kind ()

let prove_sets_of_closures ~type_of_name t : _ proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a set of \
        closures: %a"
      print t
  in
  let simplified, canonical_name = Simplified_type.create ~type_of_name t in
  match simplified with
  | Fabricated ty_fabricated ->
    begin match ty_fabricated with
    | Unknown _ -> Unknown
    | Bottom -> Invalid
    | Ok (Set_of_closures set_of_closures) ->
      Proved (canonical_name, set_of_closures)
    | Ok _ -> Invalid
    end
  | Value _ -> wrong_kind ()
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Phantom _ -> wrong_kind ()

(* XXX What about [Obj.truncate]?
   In fact, what happens regarding this for block access too? *)

(* XXX Lengths of strings: for this, I think we can assume that Obj.truncate
   is always illegal here *)

let prove_lengths_of_arrays_or_blocks ~type_of_name t
      : Targetint.OCaml.Set.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a block: %a"
      print t
  in
  let simplified, _canonical_name = Simplified_type.create ~type_of_name t in
  match simplified with
  | Value ty_value ->
    begin match ty_value with
    | Unknown _ -> Unknown
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
        assert (not (Tag.Map.is_empty blocks_imms.blocks));
        let lengths =
          Tag.Map.fold (fun _tag ((Join { by_length; }) : block_cases) result ->
              Targetint.OCaml.Map.fold (fun length _block result ->
                  Targetint.OCaml.Set.add length result)
                by_length
                result)
            blocks_imms.blocks
            Targetint.OCaml.Set.empty
        in
        assert (not (Targetint.OCaml.Set.is_empty lengths));
        Proved lengths
      end
    | Ok (Boxed_number _) -> Invalid
    | Ok (Closures _ | String _) -> Invalid
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _
  | Phantom _ -> wrong_kind ()

let prove_of_kind_value_with_expected_value_kind ~type_of_name
        t expected_value_kind =
  let actual_kind = kind ~type_of_name t in
  let expected_kind = K.value expected_value_kind in
  if not (Flambda_kind.compatible actual_kind ~if_used_at:expected_kind)
  then begin
    Misc.fatal_errorf "Type should be compatible with kind %a but \
        has incompatible kind %a: %a"
      Flambda_kind.print expected_kind
      Flambda_kind.print actual_kind
      print t
    end;
  force_to_kind_value t

let prove_of_kind_naked_float ~type_of_name t =
  let actual_kind = kind ~type_of_name t in
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

let physically_equal ~type_of_name:_ (t1 : t) (t2 : t) =
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
    | Type_of name1, Type_of name2 ->
      (* CR mshinwell: (see comment in simplify_primitives.ml in the existing
         Flambda)  We didn't used to check equality on variable aliases in case
         the variables weren't bound.  However everything should be bound now,
         so this seems like it should be ok.  (Remember that Name covers both
         variables and symbols.) *)
      Name.equal name1 name2
  in
  match t1, t2 with
  | Value ty_value1, Value ty_value2 ->
    check_aliases ty_value1 ty_value2
  | Naked_number (ty_naked_number1, _), Naked_number (ty_naked_number2, _) ->
    check_aliases ty_naked_number1 ty_naked_number2
  | Fabricated _, Fabricated _
  | Phantom _, Phantom _ -> false
  | _, _ ->
    Misc.fatal_errorf "Kind mismatch for [physically_equal]: %a versus %a"
      print t1
      print t2

(* CR mshinwell: We only use [structurally_distinct] on things of kind
   [Value] at the moment (same for [physically_equal], above). *)
let structurally_distinct ~type_of_name (t1 : t) (t2 : t) =
  let simplified1, _canonical_name1 = Simplified_type.create ~type_of_name t1 in
  let simplified2, _canonical_name2 = Simplified_type.create ~type_of_name t2 in
  let module S = Simplified_type in
  (* Note: this function relies on the fact that sets of "known values" are
     always exact, and never a subset of the possible known values.  (This
     distinction would be important if, for example, a type's knowledge has
     to be cut down because it is getting too large.  Some random subset cannot
     be chosen: we must forget all of the possible values. *)
  match simplified1, simplified2 with
  | Value ty_value1, Value ty_value2 ->
    begin match ty_value1, ty_value2 with
    | Unknown _, _ | _, Unknown _ | Bottom, _ | _, Bottom -> false
    | Ok of_kind_value1, Ok of_kind_value2 ->
      begin match of_kind_value1, of_kind_value2 with
      | Blocks_and_tagged_immediates { blocks = blocks1; immediates = imms1; },
          Blocks_and_tagged_immediates
            { blocks = blocks2; immediates = imms2; } ->
        (* CR-someday mshinwell: This could be improved if required. *)
        if (Tag.Map.is_empty blocks1 && not (Tag.Map.is_empty blocks2))
          || (not (Tag.Map.is_empty blocks1) && Tag.Map.is_empty blocks2)
        then
          true
        else
          begin match imms1, imms2 with
          | Unknown, _ | _, Unknown -> false
          | Known imms1, Known imms2 ->
            let imms1 = Immediate.Map.keys imms1 in
            let imms2 = Immediate.Map.keys imms2 in
            Immediate.Set.is_empty (Immediate.Set.inter imms1 imms2)
          end
      | Blocks_and_tagged_immediates _, _
      | _, Blocks_and_tagged_immediates _ -> true
      | Boxed_number (Boxed_float ty_naked_number1),
          Boxed_number (Boxed_float ty_naked_number2) ->
        begin match
          prove_naked_float ~type_of_name
            (of_ty_naked_number ty_naked_number1
              K.Naked_number.Naked_float),
          prove_naked_float ~type_of_name
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
          prove_naked_int32 ~type_of_name
            (of_ty_naked_number ty_naked_number1
              K.Naked_number.Naked_int32),
          prove_naked_int32 ~type_of_name
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
          prove_naked_int64 ~type_of_name
            (of_ty_naked_number ty_naked_number1
              K.Naked_number.Naked_int64),
          prove_naked_int64 ~type_of_name
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
          prove_naked_nativeint ~type_of_name
            (of_ty_naked_number ty_naked_number1
              K.Naked_number.Naked_nativeint),
          prove_naked_nativeint ~type_of_name
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
      prove_naked_float ~type_of_name t1,
        prove_naked_float ~type_of_name t2
    with
    | Proved nums1, Proved nums2 ->
      Float_by_bit_pattern.Set.is_empty
        (Float_by_bit_pattern.Set.inter nums1 nums2)
    | _, _ -> false
    end
  | S.Naked_number (_, K.Naked_number.Naked_int32),
      S.Naked_number (_, K.Naked_number.Naked_int32) ->
    begin match
      prove_naked_int32 ~type_of_name t1,
        prove_naked_int32 ~type_of_name t2
    with
    | Proved nums1, Proved nums2 ->
      Int32.Set.is_empty
        (Int32.Set.inter nums1 nums2)
    | _, _ -> false
    end
  | S.Naked_number (_, K.Naked_number.Naked_int64),
      S.Naked_number (_, K.Naked_number.Naked_int64) ->
    begin match
      prove_naked_int64 ~type_of_name t1,
        prove_naked_int64 ~type_of_name t2
    with
    | Proved nums1, Proved nums2 ->
      Int64.Set.is_empty
        (Int64.Set.inter nums1 nums2)
    | _, _ -> false
    end
  | S.Naked_number (_, K.Naked_number.Naked_nativeint),
      S.Naked_number (_, K.Naked_number.Naked_nativeint) ->
    begin match
      prove_naked_nativeint ~type_of_name t1,
        prove_naked_nativeint ~type_of_name t2
    with
    | Proved nums1, Proved nums2 ->
      Targetint.Set.is_empty
        (Targetint.Set.inter nums1 nums2)
    | _, _ -> false
    end
  | Fabricated _, Fabricated _
  | Phantom _, Phantom _ -> false
  | _, _ ->
    Misc.fatal_errorf "Kind mismatch for [structurally_different]: %a versus %a"
      print t1
      print t2

let int_switch_arms ~type_of_name t ~arms =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a tagged \
        immediate: %a"
      print t
  in
  let unknown () =
    let empty_env = Typing_environment.create () in
    Targetint.OCaml.Map.fold (fun arm cont result ->
        Targetint.OCaml.Map.add arm (empty_env, cont) result)
      arms
      Targetint.OCaml.Map.empty
  in
  let invalid () = Targetint.OCaml.Map.empty in
  let simplified, _canonical_name = Simplified_type.create ~type_of_name t in
  match simplified with
  | Value ty_value ->
    begin match ty_value with
    | Unknown _ -> unknown ()
    | Bottom -> invalid ()
    | Ok (Blocks_and_tagged_immediates blocks_imms) ->
      if not (Tag.Map.is_empty blocks_imms.blocks) then begin
        invalid ()
      end else begin
        match blocks_imms.immediates with
        | Unknown -> unknown ()
        | Known imms ->
          assert (not (Immediate.Map.is_empty imms));
          Targetint.OCaml.Map.fold (fun arm cont result ->
              match Immediate.Map.find (Immediate.int arm) imms with
              | exception Not_found -> result
              | { env_extension; } ->
                Targetint.OCaml.Map.add arm (env_extension, cont) result)
            arms
            Targetint.OCaml.Map.empty
      end
    | Ok (Boxed_number _) -> invalid ()
    | Ok (Closures _ | String _) -> invalid ()
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _
  | Phantom _ -> wrong_kind ()

let tag_switch_arms ~type_of_name t ~arms =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a tag: %a"
      print t
  in
  let unknown () =
    let empty_env = Typing_environment.create () in
    Tag.Map.fold (fun arm cont result ->
        Tag.Map.add arm (empty_env, cont) result)
      arms
      Tag.Map.empty
  in
  let invalid () = Tag.Map.empty in
  let simplified, _canonical_name = Simplified_type.create ~type_of_name t in
  match simplified with
  | Fabricated ty_fabricated ->
    begin match ty_fabricated with
    | Unknown _ -> unknown ()
    | Bottom -> invalid ()
    | Ok (Tag tag_map) ->
      Tag.Map.fold (fun arm cont result ->
          match Tag.Map.find arm tag_map with
          | exception Not_found -> result
          | { env_extension; } ->
            Tag.Map.add arm (env_extension, cont) result)
        arms
        Tag.Map.empty
    | Ok (Set_of_closures _) | Ok (Closure _) -> invalid ()
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Value _ | Phantom _ -> wrong_kind ()
