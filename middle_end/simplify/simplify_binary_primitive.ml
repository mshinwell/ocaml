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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module B = Inlining_cost.Benefit
module E = Simplify_env_and_result.Env
module K = Flambda_kind
module R = Simplify_env_and_result.Result
module T = Flambda_type

module Float_by_bit_pattern = Numbers.Float_by_bit_pattern
module Int = Numbers.Int
module Named = Flambda.Named
module Reachable = Flambda.Reachable

type named_simplifier =
  (Variable.t * Named.t) list * Reachable.t
    * Flambda_type.t * R.t

type 'a binary_arith_outcome_for_one_side_only =
  | Exactly of 'a
  | This_primitive of Flambda_primitive.t
  | The_other_side
  | Cannot_simplify
  | Invalid

module type Binary_arith_like_sig = sig
  module Lhs : Identifiable.S
  module Rhs : Identifiable.S

  module Pair : sig
    type nonrec t = Lhs.t * Rhs.t

    include Identifiable.S with type t := t
  end

  module Result : sig
    include Identifiable.S
  end

  val ok_to_evaluate : Env.t -> unit

  val cross_product : Lhs.Set.t -> Rhs.Set.t -> Pair.Set.t

  val kind : K.t
  val term : Result.t -> Named.t

  val prover_lhs : (flambda_type -> Lhs.Set.t) T.type_accessor
  val prover_rhs : (flambda_type -> Rhs.Set.t) T.type_accessor

  val these : Result.Set.t -> flambda_type

  type op

  val op : op -> Lhs.t -> Rhs.t -> Result.t option

  val op_lhs_unknown
     : op
    -> rhs:Rhs.t
    -> Result.t binary_arith_outcome_for_one_side_only

  val op_rhs_unknown
     : op
    -> lhs:Lhs.t
    -> Result.t binary_arith_outcome_for_one_side_only
end

module Binary_arith_like (N : Binary_arith_like_sig) : sig
  val simplify
     : E.t
    -> R.t
    -> Flambda_primitive.t
    -> Debuginfo.t
    -> op:N.op
    -> Simple.t
    -> Simple.t
    -> Named.t * R.t
end = struct
  module Possible_result = struct
    type t =
      | Var of Variable.t
      | Prim of Flambda_primitive.t
      | Exactly of N.t

    include Identifiable.Make_no_hash (struct
      type nonrec t = t

      let compare t1 t2 =
        match t1, t2 with
        | Var var1, Var var2 -> Variable.compare var1 var2
        | Prim prim1, Prim prim2 -> Flambda_primitive.compare prim1 prim2
        | Exactly i1, Exactly i2 -> N.compare i1 i2
        | Var _, (Prim _ | Exactly _) -> -1
        | Prim _, Var _ -> 1
        | Prim _, Exactly _ -> -1
        | Exactly, (Var _ | Prim _) -> 1

      let print _ppf _t = Misc.fatal_error "Not yet implemented"
    end)
  end

  let simplify env r prim dbg op arg1 arg2 : Named.t * R.t =
    let module P = Possible_result in
    let arg1, ty1 = S.simplify_simple env arg1 in
    let arg2, ty2 = S.simplify_simple env arg2 in
    let proof1 = (E.type_accessor env N.prover_lhs) arg1 in
    let proof2 = (E.type_accessor env N.prover_rhs) arg2 in
    let original_term () : Named.t = Prim (Binary (prim, arg1, arg2), dbg) in
    let result_unknown () =
      Reachable.reachable (original_term ()), T.unknown N.kind Other
    in
    let result_invalid () = Reachable.invalid (), T.bottom N.kind in
    let check_possible_results ~possible_results =
      (* CR mshinwell: We may want to bound the size of the set. *)
      let named, ty =
        if N.Result.Set.is_empty possible_results then
          result_invalid ()
        else
          let named =
            match N.Result.Set.get_singleton possible_results with
            | Some (Exactly i) -> N.term i
            | Some (Prim prim) -> Named.Prim (prim, dbg)
            | Some (Var var) -> Named.Simple (Simple.var var)
            | None -> original_term ()
          in
          let ty =
            let is =
              List.filter_map (function
                  | Exactly i -> Some i
                  | Prim _ | Var _ -> None)
                (N.Result.Set.to_list possible_results)
            in
            if List.length is = N.Result.Set.cardinal possible_results then
              N.these (N.Result.Set.of_list is)
            else
              match N.Result.Set.get_singleton possible_results with
              | Some (Var var) -> T.alias kind var
              | Some (Exactly _)
              | Some (Prim _)
              | None -> T.unknown kind Other
          in
          named, ty
      in
      Reachable.reachable named, ty
    in
    let only_one_side_known op nums ~folder ~other_side_var =
      let possible_results =
        folder (fun i possible_results ->
            match possible_results with
            | None -> None
            | Some possible_results ->
              match op i with
              | Exactly result ->
                P.Set.add (Exactly result) possible_results
              | This_primitive prim ->
                P.Set.add (Prim prim) possible_results
              | The_other_side ->
                P.Set.add (Var other_side_var) possible_results
              | Cannot_simplify -> None
              | Invalid -> possible_results)
          nums
          Some (N.Result.Set.empty)
      in
      match possible_results with
      | Some results -> check_possible_results ~possible_results
      | None -> result_unknown ()
    in
    let term, ty =
      match proof1, proof2 with
      | (Proved (Exactly nums1), Proved (Exactly nums2))
          when N.ok_to_evaluate env ->
        assert (not (N.Lhs.Set.is_empty nums1));
        assert (not (N.Rhs.Set.is_empty nums2));
        let all_pairs = N.cross_product nums1 nums2 in
        let possible_results =
          N.Pair.Set.fold (fun (i1, i2) possible_results ->
              match N.op op i1 i2 with
              | None -> possible_results
              | Some result ->
                N.Result.Set.add (Exactly result) possible_results)
            all_pairs
            N.Result.Set.empty
        in
        check_possible_results ~possible_results
      | (Proved (Exactly nums1), Proved Not_all_values_known)
          when N.ok_to_evaluate env ->
        assert (not (N.Lhs.Set.is_empty nums1));
        only_one_side_known (fun i -> N.op_rhs_unknown op ~lhs:i) nums1
          ~folder:N.Lhs.Set.fold
          ~other_side_var:arg2
      | (Proved Not_all_values_known, Proved (Exactly nums2))
          when N.ok_to_evaluate env ->
        assert (not (N.Rhs.Set.is_empty nums2));
        only_one_side_known (fun i -> N.op_lhs_unknown op ~rhs:i) nums2
          ~folder:N.Rhs.Set.fold
          ~other_side_var:arg1
      | Proved _, Proved _ ->
        result_unknown ()
      | Invalid, _ | _, Invalid ->
        result_invalid ()
    in
    term, ty, r
end

module Int_ops_for_binary_arith (I : sig
  type t

  val kind : K.Standard_int.t
  val term : t -> Named.t

  val zero : t
  val one : t
  val minus_one : t

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  (* CR mshinwell: We should think very carefully to make sure this is right.
     Also see whether unsafe division can be exposed to the user.  The
     current assumption that division by zero reaching here is dead code. *)
  val div : t -> t -> t option
  val mod_ : t -> t -> t option
  val and_ : t -> t -> t
  val or_ : t -> t -> t
  val xor : t -> t -> t

  include Identifiable.S with type t := t

  val these : Set.t -> flambda_type

  val prover : (flambda_type -> Set.t) T.type_accessor

  module Pair : sig
    type nonrec t = t * t

    include Identifiable.S with type t := t
  end

  val cross_product : Set.t -> Set.t -> Pair.Set.t
end) : sig
  include Binary_arith_like_sig
    with type op = binary_int_arith_op
end = struct
  module Lhs = I
  module Rhs = I
  module Result = I

  let ok_to_evaluate _env = true

  let prover_lhs = I.prover
  let prover_rhs = I.prover

  let op (op : binary_int_arith_op) n1 n2 =
    let always_some f = Some (f n1 n2) in
    match op with
    | Add -> always_some I.add
    | Sub -> always_some I.sub
    | Mul -> always_some I.mul
    | Div -> I.div n1 n2
    | Mod -> I.mod_ n1 n2
    | And -> always_some I.and_
    | Or -> always_some I.or_
    | Xor -> always_some I.xor

  type symmetric_op =
    | Add
    | Mul
    | And
    | Or
    | Xor

  let symmetric_op_one_side_unknown (op : symmetric_op) ~this_side
        : N.t binary_arith_outcome_for_one_side_only =
    let negate_lhs () : N.t binary_arith_outcome_for_one_side_only =
      This_primitive (Unary (Int_arith Neg, arg1))
    in
    match op with
    | Add ->
      if I.equal rhs I.zero then The_other_side
      else Cannot_simplify
    | Mul ->
      if I.equal rhs I.zero then Exactly I.zero
      else if I.equal rhs I.one then The_other_side
      else if I.equal rhs I.minus_one then negate_lhs ()
      else Cannot_simplify
    | And ->
      if I.equal rhs I.minus_one then The_other_side
      else if I.equal rhs I.zero then Exactly I.zero
      else Cannot_simplify
    | Or ->
      if I.equal rhs I.minus_one then Exactly I.minus_one
      else if I.equal rhs I.zero then The_other_side
      else Cannot_simplify
    | Xor ->
      if I.equal lhs I.zero then The_other_side
      (* CR mshinwell: We don't have bitwise NOT
      else if I.equal lhs I.minus_one then bitwise NOT of rhs *)
      else Cannot_simplify

  let op_lhs_unknown ~rhs : N.t binary_arith_outcome_for_one_side_only =
    let negate_the_other_side () : N.t binary_arith_outcome_for_one_side_only =
      This_primitive (Unary (Int_arith Neg, arg1))
    in
    match op with
    | Add -> symmetric_op_one_side_unknown Add ~this_side:rhs
    | Mul -> symmetric_op_one_side_unknown Mul ~this_side:rhs
    | And -> symmetric_op_one_side_unknown And ~this_side:rhs
    | Or -> symmetric_op_one_side_unknown Or ~this_side:rhs
    | Xor -> symmetric_op_one_side_unknown Xor ~this_side:rhs
    | Sub ->
      if I.equal rhs I.zero then The_other_side
      else Cannot_simplify
    | Div ->
      if I.equal rhs I.zero then Invalid
      else if I.equal rhs I.one then The_other_side
      else if I.equal rhs I.minus_one then negate_the_other_side ()
      (* CR mshinwell: Add 0 / x = 0 when x <> 0 *)
      else Cannot_simplify
    | Mod ->
      (* CR mshinwell: We could be more clever for Mod and And *)
      if I.equal rhs I.zero then Invalid
      else if I.equal rhs I.one then Exactly I.zero
      else if I.equal rhs I.minus_one then Exactly I.zero
      else Cannot_simplify

  let op_rhs_unknown ~lhs : N.t binary_arith_outcome_for_one_side_only =
    let negate_the_other_side () : N.t binary_arith_outcome_for_one_side_only =
      This_primitive (Unary (Int_arith Neg, arg2))
    in
    match op with
    | Add -> symmetric_op_one_side_unknown Add ~this_side:lhs
    | Mul -> symmetric_op_one_side_unknown Mul ~this_side:lhs
    | And -> symmetric_op_one_side_unknown And ~this_side:lhs
    | Or -> symmetric_op_one_side_unknown Or ~this_side:lhs
    | Xor -> symmetric_op_one_side_unknown Xor ~this_side:lhs
    | Sub ->
      if I.equal lhs I.zero then negate_the_other_side ()
      else Cannot_simplify
    | Div | Mod -> Cannot_simplify
end

module Int_ops_for_binary_arith_tagged_immediate =
  Int_ops_for_binary_arith (Targetint.OCaml)
module Int_ops_for_binary_arith_int32 =
  Int_ops_for_binary_arith (Int32)
module Int_ops_for_binary_arith_int64 =
  Int_ops_for_binary_arith (Int64)
module Int_ops_for_binary_arith_nativeint =
  Int_ops_for_binary_arith (Targetint)

module Binary_int_arith_tagged_immediate =
  Binary_arith_like (Int_ops_for_binary_arith_tagged_immediate)
module Binary_int_arith_int32 =
  Binary_arith_like (Int_ops_for_binary_arith_int32)
module Binary_int_arith_int64 =
  Binary_arith_like (Int_ops_for_binary_arith_int64)
module Binary_int_arith_nativeint =
  Binary_arith_like (Int_ops_for_binary_arith_nativeint)

module Int_ops_for_binary_shift (I : sig
  type t

  val kind : K.Standard_int.t
  val term : t -> Named.t

  val zero : t

  val shift_left : t -> t -> t
  (* [shift_right] is arithmetic shift right, matching [Int32], [Int64], etc. *)
  val shift_right : t -> t -> t
  val shift_right_logical : t -> t -> t

  include Identifiable.S with type t := t

  val these : Set.t -> flambda_type

  val prover : (flambda_type -> Set.t) T.type_accessor

  module Pair : sig
    type nonrec t = t * t

    include Identifiable.S with type t := t
  end

  val cross_product : Set.t -> Set.t -> Pair.Set.t
end) : sig
  include Binary_arith_sig
    with type op = binary_int_arith_op
end = struct
  module Lhs = I
  module Rhs = Immediate
  module Result = I

  let ok_to_evaluate _env = true

  let prover_lhs = I.prover
  let prover_rhs = T.prove_tagged_immediate

  let op (op : binary_int_arith_op) n1 n2 =
    let always_some f = Some (f n1 n2) in
    match op with
    | Lsl -> always_some I.shift_left
    | Lsr -> always_some I.shift_right_logical
    | Asr -> always_some I.shift_right

  let op_lhs_unknown ~rhs : N.t binary_arith_outcome_for_one_side_only =
    let module O = Targetint.OCaml in
    let rhs = Immediate.to_targetint rhs in
    match op with
    | Lsl | Lsr | Asr ->
      (* Shifting either way by [Targetint.size] or above, or by a negative
         amount, is undefined.
         However note that we cannot produce [Invalid] unless the code is
         type unsafe, which it is not here.  (Otherwise a GADT match might
         be reduced to only one possible case which it would be wrong to
         take.) *)
      if O.equal rhs O.zero then The_other_side
      else Cannot_simplify

  let op_rhs_unknown ~lhs : N.t binary_arith_outcome_for_one_side_only =
    (* In these cases we are giving a semantics for some cases where the
       right-hand side may be less than zero or greater than or equal to
       [Targetint.size].  These cases have undefined semantics, as above;
       however, it seems fine to give them a semantics since there is benefit
       to doing so in this particular case.  (This is not the case for
       the situation in [op_lhs_unknown], above, where there would be no
       such benefit.) *)
    match op with
    | Lsl | Lsr ->
      if I.equal lhs I.zero then Exactly I.zero
      else Cannot_simplify
    | Asr ->
      if I.equal lhs I.zero then Exactly I.zero
      else if I.equal lhs I.minus_one then Exactly I.minus_one
      else Cannot_simplify
end

module Int_ops_for_binary_shift_tagged_immediate =
  Int_ops_for_binary_shift (Targetint.OCaml)
module Int_ops_for_binary_shift_int32 =
  Int_ops_for_binary_shift (Int32)
module Int_ops_for_binary_shift_int64 =
  Int_ops_for_binary_shift (Int64)
module Int_ops_for_binary_shift_nativeint =
  Int_ops_for_binary_shift (Targetint)

module Binary_int_shift_tagged_immediate =
  Binary_arith_like (Int_ops_for_binary_shift_tagged_immediate)
module Binary_int_shift_int32 =
  Binary_arith_like (Int_ops_for_binary_shift_int32)
module Binary_int_shift_int64 =
  Binary_arith_like (Int_ops_for_binary_shift_int64)
module Binary_int_shift_nativeint =
  Binary_arith_like (Int_ops_for_binary_shift_nativeint)

module Int_ops_for_binary_comp (I : sig
  type t

  val kind : K.Standard_int.t
  val term : t -> Named.t

  val zero : t

  val compare : t -> t -> int

  include Identifiable.S with type t := t

  val these : Set.t -> flambda_type

  val prover : (flambda_type -> Set.t) T.type_accessor

  module Pair : sig
    type nonrec t = t * t

    include Identifiable.S with type t := t
  end

  val cross_product : Set.t -> Set.t -> Pair.Set.t
end) : sig
  include Binary_arith_sig
    with type op = Flambda_primitive.comparison
end = struct
  module Lhs = I
  module Rhs = I
  module Result = Immediate

  let ok_to_evaluate _env = true

  let prover_lhs = I.prover
  let prover_rhs = I.prover

  let op (op : Flambda_primitive.comparison) n1 n2 =
    let bool b =
      if b then Immediate.const_true else Immediate.const_false
    in
    match op with
    | Eq -> Some (bool (I.equal n1 n2))
    | Neq -> Some (bool (not (I.equal n1 n2)))
    | Lt -> Some (bool (I.compare n1 n2 < 0))
    | Gt -> Some (bool (I.compare n1 n2 > 0))
    | Le -> Some (bool (I.compare n1 n2 <= 0))
    | Ge -> Some (bool (I.compare n1 n2 >= 0))

  let op_lhs_unknown _op ~rhs:_ : N.t binary_arith_outcome_for_one_side_only =
    Cannot_simplify

  let op_rhs_unknown _op ~lhs:_ : N.t binary_arith_outcome_for_one_side_only =
    Cannot_simplify
end

module Int_ops_for_binary_comp_tagged_immediate =
  Int_ops_for_binary_comp (Targetint.OCaml)
module Int_ops_for_binary_comp_int32 =
  Int_ops_for_binary_comp (Int32)
module Int_ops_for_binary_comp_int64 =
  Int_ops_for_binary_comp (Int64)
module Int_ops_for_binary_comp_nativeint =
  Int_ops_for_binary_comp (Targetint)

(* CR mshinwell: The old code used to check inequality on types to simplify
   comparisons.  What did this apply to though---are we misunderstanding
   something about Pintcomp? *)

module Binary_int_comp_tagged_immediate =
  Binary_arith_like (Int_ops_for_binary_comp_tagged_immediate)
module Binary_int_comp_int32 =
  Binary_arith_like (Int_ops_for_binary_comp_int32)
module Binary_int_comp_int64 =
  Binary_arith_like (Int_ops_for_binary_comp_int64)
module Binary_int_comp_nativeint =
  Binary_arith_like (Int_ops_for_binary_comp_nativeint)

module Int_ops_for_binary_comp_unsigned : sig
  include Binary_arith_sig
    with type op = Flambda_primitive.comparison
end = struct
  module Lhs = Targetint
  module Rhs = Targetint
  module Result = Immediate

  let ok_to_evaluate _env = true

  let prover_lhs = T.prove_naked_immediate
  let prover_rhs = T.prove_naked_immediate

  let op (op : Flambda_primitive.comparison) n1 n2 =
    let bool b =
      if b then Immediate.const_true else Immediate.const_false
    in
    match op with
    | Eq -> Some (bool (Targetint.equal n1 n2))
    | Neq -> Some (bool (not (Targetint.equal n1 n2)))
    | Lt -> Some (bool (Targetint.compare_unsigned n1 n2 < 0))
    | Gt -> Some (bool (Targetint.compare_unsigned n1 n2 > 0))
    | Le -> Some (bool (Targetint.compare_unsigned n1 n2 <= 0))
    | Ge -> Some (bool (Targetint.compare_unsigned n1 n2 >= 0))

  let op_lhs_unknown _op ~rhs:_ : N.t binary_arith_outcome_for_one_side_only =
    Cannot_simplify

  let op_rhs_unknown _op ~lhs:_ : N.t binary_arith_outcome_for_one_side_only =
    Cannot_simplify
end

module Binary_int_comp_unsigned =
  Binary_arith_like (Int_ops_for_binary_comp_unsigned)

module Float_ops_for_binary_arith : sig
  include Binary_arith_sig
    with type op = binary_float_arith_op
end = struct
  module F = Numbers.Float_by_bit_pattern

  let ok_to_evaluate env = E.float_const_prop env

  let op op n1 n2 =
    let always_some f = Some (f n1 n2) in
    match op with
    | Add -> always_some F.add
    | Sub -> always_some F.sub
    | Mul -> always_some F.mul
    | Div -> always_some F.div

  type symmetric_op =
    | Add
    | Mul

  let symmetric_op_one_side_unknown (op : symmetric_op) ~this_side
        : N.t binary_arith_outcome_for_one_side_only =
    let negate_the_other_side () : N.t binary_arith_outcome_for_one_side_only =
      This_primitive (Unary (Float_arith Neg, arg1))
    in
    match op with
    | Add ->
      (* You might think that "x + 0" has the same representation as "x".
         However it doesn't in the case where that constant zero is +0 and
         x is equal to -0. *)
      (* CR mshinwell: Shall we add a compiler flag to allow this? *)
      Cannot_simplify
    | Mul ->
      if F.equal this_side F.one then
        The_other_side
        [@z3 check_float_binary_neutral `Mul (+1.0) `Right]
        [@z3 check_float_binary_neutral `Mul (+1.0) `Left]
      else if F.equal this_side F.minus_one then
        negate_the_other_side ()
        [@z3 check_float_binary_opposite `Mul (-1.0) `Left]
        [@z3 check_float_binary_opposite `Mul (-1.0) `Right]
      else
        Cannot_simplify

  let op_lhs_unknown ~rhs : N.t binary_arith_outcome_for_one_side_only =
    let negate_the_other_side () : N.t binary_arith_outcome_for_one_side_only =
      This_primitive (Unary (Float_arith Neg, arg1))
    in
    match op with
    | Add -> symmetric_op_one_side_unknown Add ~this_side:rhs
    | Mul -> symmetric_op_one_side_unknown Mul ~this_side:rhs
    | Sub -> Cannot_simplify
    | Div ->
      if F.equal rhs F.one then
        The_other_side
        [@z3 check_float_binary_neutral `Div (+1.0) `Right]
      else if F.equal rhs F.minus_one then
        negate_the_other_side ()
        [@z3 check_float_binary_opposite `Div (-1.0) `Right]
      else
        Cannot_simplify

  let op_rhs_unknown ~lhs : N.t binary_arith_outcome_for_one_side_only =
    match op with
    | Add -> symmetric_op_one_side_unknown Add ~this_side:lhs
    | Mul -> symmetric_op_one_side_unknown Mul ~this_side:lhs
    | Sub -> Cannot_simplify
    | Div -> Cannot_simplify
end

module Binary_float_arith = Binary_arith_like (Float_ops_for_binary_arith)

module Float_ops_for_binary_comp : sig
  include Binary_arith_like_sig
    with type op = binary_float_comp_op
end = struct
  module Lhs = I
  module Rhs = I
  module Result = Immediate

  let prover_lhs = I.prover
  let prover_rhs = I.prover

  let ok_to_evaluate env = E.float_const_prop env

  module F = Numbers.Float_by_bit_pattern

  let op (op : Flambda_primitive.comparison) n1 n2 =
    let bool b =
      if b then Immediate.const_true else Immediate.const_false
    in
    match op with
    | Eq -> Some (bool (F.equal_ieee n1 n2))
    | Neq -> Some (bool (not (F.equal_ieee n1 n2)))
    | Lt -> Some (bool (F.compare_ieee n1 n2 < 0))
    | Gt -> Some (bool (F.compare_ieee n1 n2 > 0))
    | Le -> Some (bool (F.compare_ieee n1 n2 <= 0))
    | Ge -> Some (bool (F.compare_ieee n1 n2 >= 0))

  let result_of_comparison_with_nan op =
    match op with
    | Neq -> Immediate.const_true
    | Eq | Lt | Gt | Le | Ge -> Immediate.const_false

  let op_lhs_unknown op ~rhs : N.t binary_arith_outcome_for_one_side_only =
    if F.is_any_nan rhs then result_of_comparison_with_nan op
    else Cannot_simplify

  let op_rhs_unknown op ~lhs : N.t binary_arith_outcome_for_one_side_only =
    if F.is_any_nan lhs then result_of_comparison_with_nan op
    else Cannot_simplify
end

module Binary_float_comp = Binary_arith_like (Float_ops_for_binary_comp)

let get_field ~importer ~type_of_name t ~field_index ~expected_result_kind
      ~field_is_mutable ~is_unknown : get_field_result =
  let block_t = t in
  (* XXX not sure this is right -- see comment below *)
  match Tag.Scannable.Map.get_singleton t with
  | None -> Invalid
  | Some (_tag, fields) ->
    if field_index < 0 || field_index >= Array.length fields then
      Invalid
    else
      let ty = fields.(field_index) in
      let scanning = scanning_ty_value ~importer ~type_of_name ty in
      let actual_kind = K.value scanning in
      if not (K.compatible actual_kind ~if_used_at:expected_result_kind)
      then begin
        Misc.fatal_errorf "Expected field %d of block with the following \
            type to have kind %a, but it has kind %a: %a"
          field_index
          K.print expected_result_kind
          K.print actual_kind
          print t
      end;
      let t = t_of_ty_value ty in
      if field_is_mutable then begin
        if not (is_unknown ~importer ~type_of_name t) then begin
          Misc.fatal_errorf "Field %d of type %a in block with the following \
              type is apparently mutable, yet its type is not unknown: %a"
            field_index
            print block_t
            print t
        end
      end;
      Ok t

let get_field ~importer ~type_of_name t ~field_index ~field_is_mutable
      ~(block_access_kind : Flambda_primitive.block_access_kind)
      : get_field_result =
  let t_evaluated, _canonical_name =
    Evaluated.create ~importer ~type_of_name t
  in
  let expected_result_kind =
    match block_access_kind with
    | Dynamic_must_scan_or_naked_float -> K.value Must_scan
    | Must_scan -> K.value Must_scan
    | Definitely_immediate -> K.value Definitely_immediate
    | Naked_float -> K.naked_float ()
  in
  match t_evaluated with
  | Values values ->
    begin match values with
    | Unknown -> Ok (unknown expected_result_kind Other)
    | Blocks_and_tagged_immediates (Exactly (blocks, imms)) ->
      (* XXX this needs reviewing again in the light of the work in
         Simplify_primitive (for block set).  I suspect this next conditional
         should go *)
      if not (Immediate.Set.is_empty imms) then
        Invalid
      else
        (* XXX we shouldn't be doing this if [field_kind] is [Float] -- and
           vice-versa in the float array case *)
        Blocks.get_field ~importer ~type_of_name blocks ~field_index
          ~expected_result_kind ~field_is_mutable ~is_unknown
    | Float_arrays { lengths; } ->
      let if_used_at = Flambda_kind.naked_float () in
      (* CR mshinwell: If this check fails, maybe it's always a compiler bug?
         We need to check how the kind for [Block_load] is set in the frontend
         (i.e. Pfield / Pfloatfield). *)
      if not (Flambda_kind.compatible expected_result_kind ~if_used_at) then
        Invalid
      else
        let index_is_out_of_range_for_all_lengths =
          match lengths with
          | Not_all_values_known -> false
          | Exactly lengths ->
            Int.Set.for_all (fun length ->
                field_index < 0 || field_index >= length)
              lengths
        in
        if index_is_out_of_range_for_all_lengths then
          Invalid
        else
          Ok (unknown (Flambda_kind.naked_float ()) Other)
    | Bottom
    | Blocks_and_tagged_immediates _
    | Tagged_immediates_only _
    | Boxed_floats _
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
    Misc.fatal_errorf "Cannot extract field %d from block with the following \
        type (invalid kind): %a"
      field_index
      print t

type block_access_op =
  | Immutable_load of { result_var : Variable.t option; }
  | Mutable_load
  | Store

let refine_block_ty_upon_access env r ~block ~block_ty ~field_index
      ~block_access_kind (op : block_access_op) =
  let unknown_kind =
    match block_access_kind with
    | Block Any_value
    | Array Any_value -> K.value Unknown
    | Block Definitely_immediate -> K.value Unknown
    | Array Definitely_immediate -> K.value Definitely_immediate
    | Block Naked_float
    | Array Naked_float -> K.naked_float ()
    | Generic_array No_specialisation -> K.value Unknown
    | Generic_array Full_of_naked_floats -> K.naked_float ()
    | Generic_array Full_of_immediates -> K.value Definitely_immediate
    | Generic_array Full_of_arbitrary_values_but_not_floats -> K.value Unknown
  in
  let field_kind =
    match block_access_kind with
    | Block Definitely_immediate -> K.value Definitely_immediate
    | _ -> unknown_kind
  in
  let first_unknown_fields =
    if field_index > 0 then T.this_many_unknowns field_index unknown_kind
    else [| |]
  in
  let first_known_field =
    match access with
    | Immutable_load { result_var; } -> [| T.alias field_kind result_var |]
    | Mutable_load
    | Store -> [| T.unknown field_kind |]
  in
  let first_fields =
    Array.append first_unknown_fields first_known_field
  in
  assert (Array.length first_fields = field_index + 1);
  let block_ty_refinement =
    match block_access_kind with
    | Any_value | Definitely_immediate ->
      let block_case =
        T.block_case_size_possibly_longer
          ~env_extension:(T.Type_environment.create ())
          ~first_fields
      in
      T.block ~tag:Unknown ~block_case
    | Naked_float ->
      T.float_array_size_possibly_longer ~first_fields
    | Generic_array _ -> Misc.fatal_error "Not yet implemented"
  in
  let block_ty =
    (E.type_accessor env T.meet) block_ty block_ty_refinement
  in
  let r = R.add_typing_judgement r block block_ty in
  block_ty, r

let simplify_block_load_known_index env r ~result_var prim ~block ~block_ty dbg
      ~field_index ~block_access_kind ~field_is_mutable =
  let original_term () : Named.t =
    let field_index = Simple.const (Tagged_immediate field_index) in
    Prim (Binary (prim, block, field_index), dbg)
  in
  let invalid () =
    Reachable.invalid (), T.bottom field_kind,
      R.map_benefit (B.remove_primitive Block_load) r
  in
  let unknown () =
    let op : block_access_op =
      match field_is_mutable with
      | Mutable -> Mutable_load
      | Immutable -> Immutable_load { result_var = Some result_var; }
    in
    let block_ty, r =
      refine_block_ty_upon_access env r ~block ~block_ty ~field_index
        ~block_access_kind op
    in
  in
  let block_ty, r =
    let op : block_access_op =
      match field_is_mutable with
      | Mutable -> Mutable_load
      | Immutable -> Immutable_load { result_var = None; }
    in
    refine_block_ty_upon_access env r ~block ~block_ty ~field_index
      ~block_access_kind op
  in
  let proof = (E.type_accessor env T.prove_blocks) block in
  match proof with
  | Ok (blocks, imms) ->
    begin match T.Blocks.get_singleton blocks with
    | Some block ->
      begin match T.Blocks.Block.get_field block ~field_index with
      | Ok field_ty -> original_term (), field_ty, r
      | Unknown -> unknown r
      | Invalid -> invalid ()
      end
    | None -> unknown r
    end
  | Unknown -> unknown r
  | Invalid -> invalid ()

(* XXX this could maybe be shared with the equivalent [block_set] wrapper *)
let simplify_block_load env r ~result_var prim ~block ~index
      ~field_kind ~field_is_mutable dbg =
  let index, index_ty = S.simplify_simple env index in
  let block, block_ty = S.simplify_simple env block in
  let original_term () : Named.t = Prim (Binary (prim, block, index), dbg) in
  let invalid () = Reachable.invalid (), T.bottom field_kind in
  let proof = (E.type_accessor env T.prove_tagged_immediate) arg in
  let unique_index_unknown () =
    (* XXX maybe this isn't good enough; we should check [block] is actually
       a block.  What constraints on tags/sizes are there? *)
    if (E.type_accessor env T.is_bottom) ty then
      invalid ()
    else
      Reachable.reachable (original_term ()), T.unknown field_kind Other
  in
  let term, ty =
    match proof with
    | Proved (Exactly indexes) ->
      begin match Immediate.Set.get_singleton indexes with
      | Some field_index ->
        simplify_block_load_known_index env r prim ~block ~block_ty dbg
          ~field_index ~field_kind ~field_is_mutable
      | None -> unique_index_unknown ()
      end
    | Proved Not_all_values_known -> unique_index_unknown ()
    | Invalid -> invalid ()
  in
  term, ty, r

module String_info_and_immediate =
  Identifiable.Make_pair (T.String_info) (Immediate)

type bounds_check_result =
  | In_range
  | Out_of_range

(* CR mshinwell: This function will also be needed when producing the
   bounds check code when compiling from [Lambda]. *)
let bounds_check ~string_length_in_bytes ~index_in_bytes
      ~result_size_in_bytes : bounds_check_result =
  if Targetint.OCaml.(<) index_in_bytes Targetint.OCaml.zero then
    Out_of_range
  else
    let string_length_in_bytes =
      Targetint.OCaml.of_int string_length_in_bytes
    in
    let result_size_in_bytes =
      Targetint.OCaml.of_int
        (Flambda_primitive.byte_width_of_string_accessor_width width)
    in
    (* We are careful here to avoid overflow for ease of reasoning. *)
    let highest_index_allowed =
      Targetint.OCaml.(-) string_length_in_bytes result_size_in_bytes
    in
    if Targetint.OCaml.(>=) index_in_bytes highest_index_allowed then
      Out_of_range
    else
      In_range

let all_indexes_out_of_range indexes ~max_string_length
      ~result_size_in_bytes =
  Immediate.Set.for_all (fun index_in_bytes ->
      let index_in_bytes = Immediate.to_targetint index_in_bytes in
      let in_range =
        bounds_check ~string_length_in_bytes:max_string_length
          ~index_in_bytes ~result_size_in_bytes
      in
      match in_range with
      | Out_of_range -> true
      | In_range -> false)
    strs

external string_unsafe_get16
   : string
  -> int
  -> int
  = "%caml_string_get16u" [@@noalloc]

external string_unsafe_get32
   : string
  -> int
  -> Int32.t
  = "%caml_string_get32u" [@@noalloc]

external string_unsafe_get64
   : string
  -> int
  -> Int64.t
  = "%caml_string_get64u" [@@noalloc]

let simplify_string_or_bigstring_load env r prim dbg
      (string_like_value : Flambda_primitive.string_like_value)
      (width : Flambda_primitive.string_accessor_width)
      str index =
  let str, str_ty = S.simplify_simple env str in
  let index, index_ty = S.simplify_simple env index in
  let original_term () : Named.t = Prim (Binary (prim, str, index), dbg) in
  let result_kind = Flambda_primitive.kind_of_string_accessor_width width in
  let invalid () = Reachable.invalid (), T.bottom result_kind in
  let unknown () =
    Reachable.reachable (original_term ()), T.unknown result_kind Other
  in
  let str_proof : T.string_proof =
    match string_like_value with
    | String | Bytes -> (E.type_accessor env T.prove_string) str
    | Bigstring ->
      (* For the moment just check that the bigstring is of kind [Value]. *)
      let proof =
        (E.type_accessor env T.prove_of_kind_value_with_expected_scanning
          Must_scan) str
      in
      match proof with
      | Proved _ ->
        (* At the moment we don't track anything in the type system about
           bigarrays. *)
        Proved Not_all_values_known
      | Invalid -> Invalid
  in
  let index_proof = (E.type_accessor env T.prove_tagged_immediate) index in
  let all_the_empty_string strs =
    T.String_info.Set.for_all (fun (info : T.String_info.t) ->
        info.size = 0)
      strs
  in
  let result_size_in_bytes =
    Flambda_primitive.byte_width_of_string_accessor_width width
  in
  match str_proof, index_proof with
  | Proved (Exactly strs), Proved (Exactly indexes) ->
    (* CR-someday mshinwell: Here, and also for block load cases etc., we
       could actually refine the _container_ type (the string in this case)
       based on the indexes. *)
    assert (not (T.String_info.Set.is_empty strs));
    assert (not (Immediate.Set.is_empty indexes));
    let strs_and_indexes =
      String_info_and_immediate.Set.create_from_cross_product strs indexes
    in
    let tys =
      String_info_and_immediate.Set.fold
        (fun ((info : T.String_info.t), index_in_bytes) tys ->
          let in_range =
            bounds_check ~string_length_in_bytes:info.size ~index_in_bytes
              ~result_size_in_bytes
          in
          match in_range with
          | Out_of_range -> tys
          | In_range ->
            match info.contents with
            | Unknown_or_mutable ->
              begin match width with
              | Eight -> T.any_tagged_immediate ()
              | Sixteen -> T.any_tagged_immediate ()
              | Thirty_two -> T.any_naked_int32 ()
              | Sixty_four -> T.any_naked_int64 ()
              end
            | Contents str ->
              match Targetint.OCaml.to_int index with
              | None ->
                (* The existence of [Contents str] and the checks done on
                   [index] above form a proof that the [index] fits into
                   type [int] on the host machine (in fact, below
                   [Sys.max_string_length] on the host). *)
                Misc.fatal_errorf "Inconsistent [String_info]: access at \
                    index %a to %a"
                  Targetint.OCaml.print index
                  T.String_info.print info
              | Some index_in_bytes ->
                (* Note that we cannot be in the [Bigstring] case here. *)
                assert (string_like_value <> Bigstring);
                (* XXX if the target endianness does not match the host
                   endianness, we need to swap the bytes *)
                match width with
                | Eight ->
                  T.this_tagged_immediate
                    (String.unsafe_get str index_in_bytes)
                | Sixteen ->
                  T.this_tagged_immediate
                    (string_unsafe_get16 str index_in_bytes)
                | Thirty_two ->
                  T.this_naked_int32 (string_unsafe_get32 str index_in_bytes)
                | Sixty_four ->
                  T.this_naked_int64 (string_unsafe_get64 str index_in_bytes))
        strs_and_indexes
        []
    in
    begin match tys with
    | [] -> invalid ()
    | tys ->
      let ty = (E.type_accessor env T.join) tys in
      (* CR mshinwell: add benefit to [r] *)
      [], Reachable.reachable (original_term ()), ty, r
    end
  | Proved strs, Proved Not_all_values_known ->
    assert (not (T.String_info.Set.is_empty strs));
    (* CR-someday mshinwell: We could return the union of all the characters
       in the strings, within reason... *)
    if all_the_empty_string strs then invalid ()
    else unknown ()
  | Proved Not_all_values_known, Proved indexes ->
    assert (not (Immediate.Set.is_empty indexes));
    let max_string_length =
      match string_like_value with
      | String | Bytes -> Targetint.OCaml.max_string_length
      | Bigstring -> Targetint.OCaml.max_int
    in
    let all_indexes_out_of_range =
      all_indexes_out_of_range indexes ~max_string_length
        ~result_size_in_bytes
    in
    if all_indexes_out_of_range indexes then invalid ()
    else unknown ()
  | Invalid _, _ | _, Invalid _ -> invalid ()

let simplify_binary_primitive env r ~result_var prim arg1 arg2 dbg =
  match prim with
  | Block_load ->
    simplify_block_load env r ~result_var prim ~block:arg1 ~index:arg2 dbg
  | Block_set (field, field_kind, init_or_assign) ->
    simplify_block_set env r prim ~field ~field_kind ~init_or_assign
      ~block:arg1 ~new_value:arg2 dbg
  | Int_arith (kind, op) ->
    begin match kind with
    | Tagged_immediate ->
      Binary_int_arith_tagged_immediate.simplify env r prim dbg op arg1 arg2
    | Naked_int32 ->
      Binary_int_arith_naked_int32.simplify env r prim dbg op arg1 arg2
    | Naked_int64 ->
      Binary_int_arith_naked_int64.simplify env r prim dbg op arg1 arg2
    | Naked_nativeint ->
      Binary_int_arith_naked_nativeint.simplify env r prim dbg op arg1 arg2
    end
  | Int_shift (kind, op) ->
    begin match kind with
    | Tagged_immediate ->
      Binary_int_shift_tagged_immediate.simplify env r prim dbg op arg1 arg2
    | Naked_int32 ->
      Binary_int_shift_naked_int32.simplify env r prim dbg op arg1 arg2
    | Naked_int64 ->
      Binary_int_shift_naked_int64.simplify env r prim dbg op arg1 arg2
    | Naked_nativeint ->
      Binary_int_shift_naked_nativeint.simplify env r prim dbg op arg1 arg2
    end
  | Int_comp (kind, op) ->
    begin match kind with
    | Tagged_immediate ->
      Binary_int_comp_tagged_immediate.simplify env r prim dbg op arg1 arg2
    | Naked_int32 ->
      Binary_int_comp_naked_int32.simplify env r prim dbg op arg1 arg2
    | Naked_int64 ->
      Binary_int_comp_naked_int64.simplify env r prim dbg op arg1 arg2
    | Naked_nativeint ->
      Binary_int_comp_naked_nativeint.simplify env r prim dbg op arg1 arg2
    end
  | Int_comp_unsigned op ->
    Binary_int_comp_unsigned.simplify env r prim dbg op arg1 arg2
  | Float_arith op ->
    Binary_float_arith.simplify env r prim dbg op arg1 arg2
  | Float_comp op ->
    Binary_float_comp.simplify env r prim dbg op arg1 arg2
  | String_or_bigstring_load (string_like_value, width) ->
    simplify_string_load env r prim dbg string_like_value width
      ~str:arg1 ~index:arg2
