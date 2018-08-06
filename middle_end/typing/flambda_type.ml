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

module F0 = Flambda0
module K = Flambda_kind

(* module Expr = F0.Expr *)
module Named = F0.Named

module Float_by_bit_pattern = Numbers.Float_by_bit_pattern
module Int32 = Numbers.Int32
module Int64 = Numbers.Int64

include F0.Flambda_type

module JE = Join_env

type 'a type_accessor = Typing_env.t -> 'a

let bottom_types_from_arity t =
  List.map (fun kind -> bottom kind) t

let unknown_types_from_arity t =
  List.map (fun kind -> unknown kind) t

let unknown_like_array t_array =
  Array.map (fun t -> unknown_like t) t_array

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
     is (recursively) expanded, and the final ("canonical") simple value
     returned. *)
  val create : (flambda_type -> t * (Simple.t option)) type_accessor

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

  let create env (t : flambda_type) : t * (Simple.t option) =
    let t, canonical_simple = Typing_env.resolve_aliases (env, t) in
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
    }, canonical_simple

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
  let simplified, _canonical_simple = Simplified_type.create env t in
  Simplified_type.is_bottom simplified

let is_unknown env t =
  let simplified, _canonical_simple = Simplified_type.create env t in
  Simplified_type.is_unknown simplified

let is_known env t =
  not (is_unknown env t)

let is_useful env t =
  let simplified, _canonical_simple = Simplified_type.create env t in
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
  let simplified, _canonical_simple = Simplified_type.create env t in
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
  let simplified, _canonical_simple = Simplified_type.create env t in
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
  let simplified, _canonical_simple = Simplified_type.create env t in
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
  let simplified, _canonical_simple = Simplified_type.create env t in
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
  let simplified, _canonical_simple = Simplified_type.create env t in
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

let no_blocks ({ known_tags_and_sizes; size_at_least_n; } : blocks) =
  Tag_and_size.Map.is_empty known_tags_and_sizes
    && Targetint.OCaml.Map.is_empty size_at_least_n

type reification_result =
  | Term of Simple.t * t
  | Lift of Flambda_static0.Static_part.t
  | Cannot_reify
  | Invalid

let reify env ~allow_free_variables t : reification_result =
  let t, canonical_simple = Typing_env.resolve_aliases (env, t) in
(*
Format.eprintf "CN is %a\n%!" (Misc.Stdlib.Option.print Name.print)
  canonical_simple;
*)
  let can_lift =
    Name.Set.for_all (fun (name : Name.t) ->
        match name with
        | Var _ -> false
        | Symbol _ -> true)
      (Name_occurrences.everything (free_names t))
  in
  let simplified, canonical_simple' = Simplified_type.create env t in
  assert (Misc.Stdlib.Option.equal Simple.equal
    canonical_simple canonical_simple');
  if Simplified_type.is_bottom simplified then Invalid
  else if Simplified_type.is_phantom simplified then Cannot_reify
  else
    let result, canonical_var =
      match canonical_simple with
      | Some ((Name (Symbol _) | Const _ | Discriminant _) as simple) ->
        Some (Term (simple, alias_type_of (kind t) simple)), None
      | Some ((Name ((Var _) as name)) as simple) ->
        if allow_free_variables
          && (not (Typing_env.was_existential_exn env name))
        then None, Some simple
        else None, None
      | None -> None, None
    in
    match result with
    | Some result -> result
    | None ->
      let try_canonical_var () : reification_result =
        match canonical_var with
        | Some simple -> Term (simple, alias_type_of (kind t) simple)
        | None -> Cannot_reify
      in
      match simplified.descr with
      | Value ty_value ->
        begin match ty_value with
        | Unknown -> try_canonical_var ()
        | Bottom -> Invalid
        | Ok (Blocks_and_tagged_immediates blocks_imms) ->
          begin match blocks_imms.blocks with
          | Unknown -> try_canonical_var ()
          | Known blocks ->
            if not (no_blocks blocks) then try_canonical_var ()
            else
              begin match blocks_imms.immediates with
              | Unknown -> try_canonical_var ()
              | Known imms ->
                begin match Immediate.Map.get_singleton imms with
                | Some (imm, _) -> Term (Simple.const (Tagged_immediate imm), t)
                | None -> try_canonical_var ()
                end
              end
          end
        | Ok (Boxed_number (Boxed_float ty_naked_number)) ->
          if not can_lift then try_canonical_var ()
          else
            let contents =
              of_ty_naked_number ty_naked_number K.Naked_number.Naked_float
            in
            begin match prove_unique_naked_float env contents with
            | Proved f -> Lift (Boxed_float (Const f))
            | Unknown -> try_canonical_var ()
            | Invalid -> try_canonical_var ()
            end
        | Ok (Boxed_number (Boxed_int32 ty_naked_number)) ->
          if not can_lift then try_canonical_var ()
          else
            let contents =
              of_ty_naked_number ty_naked_number K.Naked_number.Naked_int32
            in
            begin match prove_unique_naked_int32 env contents with
            | Proved f -> Lift (Boxed_int32 (Const f))
            | Unknown -> try_canonical_var ()
            | Invalid -> try_canonical_var ()
            end
        | Ok (Boxed_number (Boxed_int64 ty_naked_number)) ->
          if not can_lift then try_canonical_var ()
          else
            let contents =
              of_ty_naked_number ty_naked_number K.Naked_number.Naked_int64
            in
            begin match prove_unique_naked_int64 env contents with
            | Proved f -> Lift (Boxed_int64 (Const f))
            | Unknown -> try_canonical_var ()
            | Invalid -> try_canonical_var ()
            end
        | Ok (Boxed_number (Boxed_nativeint ty_naked_number)) ->
          if not can_lift then try_canonical_var ()
          else
            let contents =
              of_ty_naked_number ty_naked_number K.Naked_number.Naked_nativeint
            in
            begin match prove_unique_naked_nativeint env contents with
            | Proved f -> Lift (Boxed_nativeint (Const f))
            | Unknown -> try_canonical_var ()
            | Invalid -> try_canonical_var ()
            end
        | Ok (Closures _ | String _) -> try_canonical_var ()
        end
      | Simplified_type.Naked_number (ty_naked_number, _) ->
        begin match ty_naked_number with
        | Unknown -> try_canonical_var ()
        | Bottom -> Invalid
        | Ok (Immediate imms) ->
          begin match Immediate.Set.get_singleton imms with
          | Some imm -> Term (Simple.const (Untagged_immediate imm), t)
          | None -> try_canonical_var ()
          end
        | Ok (Float fs) ->
          begin match Float_by_bit_pattern.Set.get_singleton fs with
          | Some f -> Term (Simple.const (Naked_float f), t)
          | None -> try_canonical_var ()
          end
        | Ok (Int32 is) ->
          begin match Int32.Set.get_singleton is with
          | Some i -> Term (Simple.const (Naked_int32 i), t)
          | None -> try_canonical_var ()
          end
        | Ok (Int64 is) ->
          begin match Int64.Set.get_singleton is with
          | Some i -> Term (Simple.const (Naked_int64 i), t)
          | None -> try_canonical_var ()
          end
        | Ok (Nativeint is) ->
          begin match Targetint.Set.get_singleton is with
          | Some i -> Term (Simple.const (Naked_nativeint i), t)
          | None -> try_canonical_var ()
          end
        end
      | Fabricated (Ok (Set_of_closures _set_of_closures)) ->
        try_canonical_var ()
      | Fabricated (Ok (Closure _)) -> try_canonical_var ()
      | Fabricated (Ok (Discriminant discriminants)) ->
        begin match Discriminant.Map.get_singleton discriminants with
        | None -> try_canonical_var ()
        | Some (discriminant, { env_extension = _; }) ->
          Term (Simple.discriminant discriminant, t)
        end
      | Fabricated Unknown -> try_canonical_var ()
      | Fabricated Bottom -> Invalid

(* CR mshinwell: rename to "prove_must_be_tagged_immediate" *)
let prove_tagged_immediate env t
      : Immediate.Set.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a tagged \
        immediate: %a"
      print t
  in
  let simplified, _canonical_simple = Simplified_type.create env t in
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
        match no_blocks blocks, Immediate.Map.is_empty imms with
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
  | By_discriminant of Typing_env_extension.t Discriminant.Map.t

let prove_tagged_immediate_as_discriminants env t
      : tagged_immediate_as_discriminants_proof proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a tagged \
        immediate: %a"
      print t
  in
  let simplified, _canonical_simple = Simplified_type.create env t in
  Simplified_type.check_not_phantom simplified
    "prove_tagged_immediate_as_discriminants";
  match simplified.descr with
  | Value ty_value ->
    begin match ty_value with
    | Unknown -> Unknown
    | Bottom -> Invalid
    | Ok (Blocks_and_tagged_immediates blocks_imms) ->
      begin match blocks_imms.blocks, blocks_imms.immediates with
      | Unknown, _ | _, Unknown -> Unknown
      | Known blocks, Known imms ->
        match no_blocks blocks, Immediate.Map.is_empty imms with
        | true, true -> Invalid
        | false, false -> Unknown
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
                  Discriminant.Map.add discr imm_case.env_extension by_discr)
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

let prove_is_tagged_immediate env t : is_tagged_immediate proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a tagged \
        immediate: %a"
      print t
  in
  let simplified, _canonical_simple = Simplified_type.create env t in
  Simplified_type.check_not_phantom simplified "prove_is_tagged_immediate";
  match simplified.descr with
  | Value ty_value ->
    begin match ty_value with
    | Unknown -> Unknown
    | Bottom -> Invalid
    | Ok (Blocks_and_tagged_immediates blocks_imms) ->
      begin match blocks_imms.blocks, blocks_imms.immediates with
      | Unknown, _ | _, Unknown -> Unknown
      | Known blocks, Known imms ->
        match no_blocks blocks, Immediate.Map.is_empty imms with
        | true, true -> Invalid
        | false, false -> Unknown
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

let field_n_of_block env ({ known_tags_and_sizes; size_at_least_n; } : blocks)
      ~index ~field_kind =
  let params =
    Tag_and_size.Map.fold (fun tag_and_size params acc ->
        let tag = Tag_and_size.tag tag_and_size in
        if not (valid_block_tag_for_kind ~tag ~field_kind) then acc
        else
          let size = Tag_and_size.size tag_and_size in
          if Targetint.OCaml.(<) index size then params::acc
          else acc)
      known_tags_and_sizes
      []
  in
  let from_size_at_least_n =
    Targetint.OCaml.Map.find_first_opt
      (fun size -> Targetint.OCaml.(<) index size)
      size_at_least_n
  in
  let params =
    match from_size_at_least_n with
    | None -> params
    | Some (_size, new_params) -> new_params::params
  in
  let env = JE.create env in
  match params with
  | [] -> None
  | params0::params ->
    let params =
      List.fold_left (fun joined_params params ->
          Parameters.join env joined_params params)
        params0
        params
    in
    match Parameters.nth params index with
    | Some kinded_param ->
      let name = Kinded_parameter.name kinded_param in
      let env_extension = Parameters.standalone_extension params in
      Some (name, env_extension)
    | None ->
      Misc.fatal_errorf "[Parameters.t] should contain index %a:@ %a"
        Targetint.OCaml.print index
        Parameters.print params

let prove_get_field_from_block env t ~index ~field_kind
      : (Name.t * Typing_env_extension.t) proof =
(*
Format.eprintf "get_field_from_block index %a type@ %a\n"
  Targetint.OCaml.print index print t;
*)
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a block: %a"
      print t
  in
  let simplified, _canonical_simple = Simplified_type.create env t in
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
          assert (not (no_blocks blocks));
          begin match field_n_of_block env blocks ~index ~field_kind with
          | None -> Invalid
          | Some result -> Proved result
          end
        end 
    | Ok (Boxed_number _) -> Invalid
    | Ok (Closures _ | String _) -> Invalid
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _ -> wrong_kind ()

(* XXX re-enable *)
let tags_all_valid _t (_blocks : blocks) ~kind_of_all_fields:_ = true
(*
  Tag.Map.for_all (fun tag ((Blocks { by_length; }) : block_cases) ->
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
*)

let prove_must_be_a_block env t ~kind_of_all_fields : unit proof =
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a block: %a"
      print t
  in
  let simplified, _canonical_simple = Simplified_type.create env t in
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
        match no_blocks blocks, Immediate.Map.is_empty imms with
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
  let simplified, _canonical_simple = Simplified_type.create env t in
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
      | Known { known_tags_and_sizes; size_at_least_n; }, Known imms ->
        if not (Targetint.OCaml.Map.is_empty size_at_least_n) then
          Proved Not_unboxable
        else
          let block_sizes_by_tag =
            Tag_and_size.Map.fold
              (fun tag_and_size _params block_sizes_by_tag ->
                match block_sizes_by_tag with
                | None -> None
                | Some block_sizes_by_tag ->
                  let tag = Tag_and_size.tag tag_and_size in
                  match Tag.Scannable.of_tag tag with
                  | None -> None
                  | Some tag ->
                    let size = Tag_and_size.size tag_and_size in
                    let size_mismatch = ref false in
                    let block_sizes_by_tag =
                      Tag.Scannable.Map.update tag (function
                          | None -> Some size
                          | Some size' ->
                            if Targetint.OCaml.equal size size' then begin
                              Some size
                            end else begin
                              size_mismatch := true;
                              None
                            end)
                        block_sizes_by_tag
                    in
                    if !size_mismatch then None
                    else Some block_sizes_by_tag)
              known_tags_and_sizes
              (Some Tag.Scannable.Map.empty)
          in
          match block_sizes_by_tag with
          | None -> Proved Not_unboxable
          | Some block_sizes_by_tag ->
            let imms = Immediate.Map.keys imms in
(* XXX re-enable
            if tags_all_valid t blocks ~kind_of_all_fields:(K.value ()) then
*)
              Proved (Unboxable {
                block_sizes_by_tag;
                constant_ctors = imms;
              })
(*
            else Proved Not_unboxable
*)
      end
    | Ok (Boxed_number _) | Ok (Closures _ | String _) -> Invalid
    end
  | Simplified_type.Naked_number _ -> wrong_kind ()
  | Fabricated _ -> wrong_kind ()

type float_array_proof =
  | Of_length of Targetint.OCaml.t
  | Not_unique_length

let _ = Of_length Targetint.OCaml.zero
let _ = Not_unique_length

(* CR mshinwell: This should probably return the field types rather than
   just the length; then it can be exposed in the .mli. *)
let prove_float_array _env _t : float_array_proof proof =
  Misc.fatal_error "Not yet implemented"
(* XXX
  let wrong_kind () =
    Misc.fatal_errorf "Wrong kind for something claimed to be a float array: \
        %a"
      print t
  in
  let simplified, _canonical_simple = Simplified_type.create env t in
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
        if no_blocks blocks
          || not (Immediate.Map.is_empty imms)
        then Invalid
        else
          let cannot_unbox = ref false in
          (* CR mshinwell: share with previous function (maybe) *)
          let block_sizes_by_tag =
            Tag.Map.fold (fun tag (Blocks { by_length; }) blocks ->
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
*)

type tags =
  | Tags of Tag.Set.t

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
  let simplified, _canonical_simple = Simplified_type.create env t in
  Simplified_type.check_not_phantom simplified "prove_tags";
  match simplified.descr with
  | Value ty_value ->
    begin match ty_value with
    | Unknown -> Unknown
    | Bottom -> Invalid
    | Ok (Blocks_and_tagged_immediates blocks_imms) ->
      begin match blocks_imms.blocks with
      | Known { known_tags_and_sizes; size_at_least_n; } ->
        if not (Targetint.OCaml.Map.is_empty size_at_least_n) then Unknown
        else
          let tags_and_sizes = Tag_and_size.Map.keys known_tags_and_sizes in
          let tags =
            Tag_and_size.Set.fold (fun tag_and_size tags ->
                Tag.Set.add (Tag_and_size.tag tag_and_size) tags)
              tags_and_sizes
              Tag.Set.empty
          in
          Proved (Tags tags)
      | Unknown -> Unknown
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
  let simplified, _canonical_simple = Simplified_type.create env t in
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
  let simplified, _canonical_simple = Simplified_type.create env t in
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
  let simplified, _canonical_simple = Simplified_type.create env t in
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
  let simplified, _canonical_simple = Simplified_type.create env t in
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
  let simplified, _canonical_simple = Simplified_type.create env t in
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
  let simplified, _canonical_simple = Simplified_type.create env t in
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
  let simplified, canonical_simple = Simplified_type.create env t in
  Simplified_type.check_not_phantom simplified "prove_sets_of_closures";
  match simplified.descr with
  | Fabricated ty_fabricated ->
    begin match ty_fabricated with
    | Unknown -> Unknown
    | Bottom -> Invalid
    | Ok (Set_of_closures set_of_closures) ->
      begin match canonical_simple with
      | None -> Proved (None, set_of_closures)
      | Some (Name name) -> Proved (Some name, set_of_closures)
      | Some (Const _ | Discriminant _) -> Invalid
      end
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
  let simplified, _canonical_simple = Simplified_type.create env t in
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
        | Known { known_tags_and_sizes; size_at_least_n; } ->
          if not (Targetint.OCaml.Map.is_empty size_at_least_n) then Unknown
          else
            let tags_and_sizes = Tag_and_size.Map.keys known_tags_and_sizes in
            let sizes =
              Tag_and_size.Set.fold (fun tag_and_size sizes ->
                  Targetint.OCaml.Set.add (Tag_and_size.size tag_and_size)
                    sizes)
                tags_and_sizes
                Targetint.OCaml.Set.empty
            in
            Proved sizes
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
    | Equals simple1, Equals simple2 ->
      (* CR mshinwell: (see comment in simplify_primitives.ml in the existing
         Flambda)  We didn't used to check equality on variable aliases in case
         the variables weren't bound.  However everything should be bound now,
         so this seems like it should be ok.  (Remember that Name covers both
         variables and symbols.) *)
      Simple.equal simple1 simple2
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
  let simplified1, _canonical_simple1 = Simplified_type.create env1 t1 in
  let simplified2, _canonical_simple2 = Simplified_type.create env2 t2 in
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
              { blocks = blocks1; immediates = imms1; },
            Blocks_and_tagged_immediates
              { blocks = blocks2; immediates = imms2; } ->
          (* CR-someday mshinwell: This could be improved if required. *)
          begin match blocks1, blocks2 with
          | Unknown, _ | _, Unknown -> false
          | Known { known_tags_and_sizes = known_tags_and_sizes1;
                    size_at_least_n = size_at_least_n1;
                  },
            Known { known_tags_and_sizes = known_tags_and_sizes2;
                    size_at_least_n = size_at_least_n2;
                  } ->
            begin match imms1, imms2 with
            | Unknown, _ | _, Unknown -> false
            | Known imms1, Known imms2 ->
              let imm_intersection =
                Immediate.Set.inter (Immediate.Map.keys imms1)
                  (Immediate.Map.keys imms2)
              in
              if (not (Immediate.Set.is_empty imm_intersection))
                || (not (Targetint.OCaml.Map.is_empty size_at_least_n1))
                || (not (Targetint.OCaml.Map.is_empty size_at_least_n2))
              then
                false
              else
                let tags_and_sizes1 =
                  Tag_and_size.Map.keys known_tags_and_sizes1
                in
                let tags_and_sizes2 =
                  Tag_and_size.Map.keys known_tags_and_sizes2
                in
                Tag_and_size.Set.is_empty
                  (Tag_and_size.Set.inter tags_and_sizes1 tags_and_sizes2)
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
  let no_env_extension = Typing_env_extension.empty in
  let wrong_kind () =
    Misc.fatal_errorf
      "Wrong kind for something claimed to be a discriminant: %a"
      print t
  in
  let unknown () =
    Discriminant.Map.fold (fun arm cont result ->
        Discriminant.Map.add arm (no_env_extension, cont) result)
      arms
      Discriminant.Map.empty
  in
  let invalid () = Discriminant.Map.empty in
  let simplified, _canonical_simple = Simplified_type.create env t in
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
          | { env_extension; } ->
            Discriminant.Map.add arm (env_extension, cont) result)
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
