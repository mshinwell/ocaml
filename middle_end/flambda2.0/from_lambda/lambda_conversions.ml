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

module K = Flambda_kind
module L = Lambda
module P = Flambda_primitive

let value_kind (value_kind : L.value_kind) =
  match value_kind with
  | Pgenval
  | Pfloatval
  | Pboxedintval Pint32
  | Pboxedintval Pint64
  | Pboxedintval Pnativeint
  | Pintval -> K.value

let inline_attribute (attr : L.inline_attribute) : Inline_attribute.t =
  match attr with
  | Always_inline -> Always_inline
  | Never_inline -> Never_inline
  | Unroll i -> Unroll i
  | Default_inline -> Default_inline

let kind_of_primitive_native_repr (repr : Primitive.native_repr) =
  match repr with
  | Same_as_ocaml_repr -> K.value
  | Unboxed_float -> K.naked_float
  | Unboxed_integer Pnativeint -> K.naked_nativeint
  | Unboxed_integer Pint32 -> K.naked_int32
  | Unboxed_integer Pint64 -> K.naked_int64
  | Untagged_int -> K.naked_immediate

let method_kind (kind : L.meth_kind) : Call_kind.method_kind =
  match kind with
  | Self -> Self
  | Public -> Public
  | Cached -> Cached

let raise_kind (kind : L.raise_kind) : Trap_action.raise_kind =
  match kind with
  | Raise_regular -> Regular
  | Raise_reraise -> Reraise
  | Raise_notrace -> No_trace

let convert_block_shape (shape : L.block_shape) ~num_fields =
  match shape with
  | None ->
    List.init num_fields (fun _field : P.Value_kind.t -> Anything)
  | Some shape ->
    let shape_length = List.length shape in
    if num_fields <> shape_length then begin
      Misc.fatal_errorf "Flambda_arity.of_block_shape: num_fields is %d \
          yet the shape has %d fields"
        num_fields
        shape_length
    end;
    List.map (fun (kind : L.value_kind) : P.Value_kind.t ->
        match kind with
        | Pgenval -> Anything
        | Pfloatval | Pboxedintval _ -> Definitely_pointer
        | Pintval -> Definitely_immediate)
      shape

let convert_mutable_flag (flag : Asttypes.mutable_flag)
      : Effects.mutable_or_immutable =
  match flag with
  | Mutable -> Mutable
  | Immutable -> Immutable

let convert_integer_comparison_prim (comp : L.integer_comparison)
      : P.binary_primitive =
  match comp with
  | Ceq -> Phys_equal (K.value, Eq)
  | Cne -> Phys_equal (K.value, Neq)
  | Clt -> Int_comp (Tagged_immediate, Signed, Lt)
  | Cgt -> Int_comp (Tagged_immediate, Signed, Gt)
  | Cle -> Int_comp (Tagged_immediate, Signed, Le)
  | Cge -> Int_comp (Tagged_immediate, Signed, Ge)

let convert_boxed_integer_comparison_prim
      (kind : L.boxed_integer) (comp : L.integer_comparison)
      : P.binary_primitive =
  match kind, comp with
  | Pint32, Ceq -> Phys_equal (K.naked_int32, Eq)
  | Pint32, Cne -> Phys_equal (K.naked_int32, Neq)
  | Pint32, Clt -> Int_comp (Naked_int32, Signed, Lt)
  | Pint32, Cgt -> Int_comp (Naked_int32, Signed, Gt)
  | Pint32, Cle -> Int_comp (Naked_int32, Signed, Le)
  | Pint32, Cge -> Int_comp (Naked_int32, Signed, Ge)
  | Pint64, Ceq -> Phys_equal (K.naked_int64, Eq)
  | Pint64, Cne -> Phys_equal (K.naked_int64, Neq)
  | Pint64, Clt -> Int_comp (Naked_int64, Signed, Lt)
  | Pint64, Cgt -> Int_comp (Naked_int64, Signed, Gt)
  | Pint64, Cle -> Int_comp (Naked_int64, Signed, Le)
  | Pint64, Cge -> Int_comp (Naked_int64, Signed, Ge)
  | Pnativeint, Ceq -> Phys_equal (K.naked_nativeint, Eq)
  | Pnativeint, Cne -> Phys_equal (K.naked_nativeint, Neq)
  | Pnativeint, Clt -> Int_comp (Naked_nativeint, Signed, Lt)
  | Pnativeint, Cgt -> Int_comp (Naked_nativeint, Signed, Gt)
  | Pnativeint, Cle -> Int_comp (Naked_nativeint, Signed, Le)
  | Pnativeint, Cge -> Int_comp (Naked_nativeint, Signed, Ge)

let convert_float_comparison (comp : L.float_comparison) : P.comparison =
  match comp with
  | CFeq -> Eq
  | CFneq -> Neq
  | CFlt -> Lt
  | CFgt -> Gt
  | CFle -> Le
  | CFge -> Ge
  | CFnlt | CFngt | CFnle | CFnge ->
    Misc.fatal_error "Negated floating-point comparisons should have been \
      removed by [Prepare_lambda]"

let boxable_number_of_boxed_integer (bint : L.boxed_integer)
  : Flambda_kind.Boxable_number.t =
  match bint with
  | Pnativeint -> Naked_nativeint
  | Pint32 -> Naked_int32
  | Pint64 -> Naked_int64

let standard_int_of_boxed_integer (bint : L.boxed_integer)
  : Flambda_kind.Standard_int.t =
  match bint with
  | Pnativeint -> Naked_nativeint
  | Pint32 -> Naked_int32
  | Pint64 -> Naked_int64

let standard_int_or_float_of_boxed_integer (bint : L.boxed_integer)
  : Flambda_kind.Standard_int_or_float.t =
  match bint with
  | Pnativeint -> Naked_nativeint
  | Pint32 -> Naked_int32
  | Pint64 -> Naked_int64

(* let const_of_boxed_integer (i:int32) (bint : L.boxed_integer) *)
(*   : Reg_width_const.t = *)
(*   match bint with *)
(*   | Pnativeint -> Naked_nativeint (Targetint.of_int32 i) *)
(*   | Pint32 -> Naked_int32 i *)
(*   | Pint64 -> Naked_int64 (Int64.of_int32 i) *)

(* let convert_record_representation (repr : Types.record_representation) *)
(*    : P.record_representation = *)
(*   match repr with *)
(*   | Record_regular -> Regular *)
(*   | Record_float -> Float *)
(*   | Record_unboxed inlined -> Unboxed { inlined } *)
(*   | Record_inlined tag -> Inlined (Tag.Scannable.create_exn tag) *)
(*   | Record_extension -> Extension *)

let convert_access_kind i_or_p : P.Block_access_kind.t0 =
  match i_or_p with
  | L.Immediate -> Value Definitely_immediate
  | L.Pointer -> Value Anything

let convert_init_or_assign (i_or_a : L.initialization_or_assignment)
   : P.init_or_assign =
  match i_or_a with
  | Assignment -> Assignment
  | Heap_initialization -> Initialization
  (* Root initialization cannot exist in lambda. This is
     represented by the static part of expressions in flambda. *)
  | Root_initialization -> assert false

let convert_array_kind (kind : L.array_kind)
   : P.Block_access_kind.t =
  match kind with
  | Pgenarray ->
    Generic_array
      (P.Generic_array_specialisation.no_specialisation ())
  | Paddrarray -> Array (Value Anything)
  | Pintarray -> Array (Value Definitely_immediate)
  | Pfloatarray -> Array Naked_float

let convert_array_kind_to_duplicate_block_kind (kind : L.array_kind)
      : P.duplicate_block_kind =
  match kind with
  | Pgenarray ->
    Generic_array
      (P.Generic_array_specialisation.no_specialisation ())
    (* CR mshinwell: Are these next two cases correct?  Should they be under
       "generic array"? *)
  | Paddrarray ->
    Generic_array (P.Generic_array_specialisation.
      full_of_arbitrary_values_but_not_floats ())
  | Pintarray ->
    Generic_array (P.Generic_array_specialisation.full_of_immediates ())
  | Pfloatarray -> Full_of_naked_floats { length = None; }

let convert_bigarray_kind (kind : L.bigarray_kind) : P.bigarray_kind =
  match kind with
  | Pbigarray_unknown -> Unknown
  | Pbigarray_float32 -> Float32
  | Pbigarray_float64 -> Float64
  | Pbigarray_sint8 -> Sint8
  | Pbigarray_uint8 -> Uint8
  | Pbigarray_sint16 -> Sint16
  | Pbigarray_uint16 -> Uint16
  | Pbigarray_int32 -> Int32
  | Pbigarray_int64 -> Int64
  | Pbigarray_caml_int -> Int_width_int
  | Pbigarray_native_int -> Targetint_width_int
  | Pbigarray_complex32 -> Complex32
  | Pbigarray_complex64 -> Complex64

let convert_bigarray_layout (layout : L.bigarray_layout) : P.bigarray_layout =
  match layout with
  | Pbigarray_unknown_layout -> Unknown
  | Pbigarray_c_layout -> C
  | Pbigarray_fortran_layout -> Fortran

let convert_field_read_semantics (sem : L.field_read_semantics)
      : Effects.mutable_or_immutable =
  match sem with
  | Reads_agree -> Immutable
  | Reads_vary -> Mutable
