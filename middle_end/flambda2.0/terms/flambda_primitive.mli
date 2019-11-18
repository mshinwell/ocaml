(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*            Mark Shinwell and Xavier Clerc, Jane Street Europe          *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   Copyright 2017--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** "Primitive" operations: those that perform computation but never affect
    control flow.

    Primitives that accept float, int32, int64 or nativeint values always
    take (or return) the unboxed versions.

    No primitive raises an exception.  (Bounds checking is handled
    separately.)
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Value_kind : sig
  type t =
    | Anything
    | Definitely_pointer  (* CR mshinwell: Is this for "addr" arrays? Name? *)
    | Definitely_immediate

  val compare : t -> t -> int

  val print : Format.formatter -> t -> unit
end

module Generic_array_specialisation : sig
  type t = private
    | No_specialisation
    | Full_of_naked_floats
    | Full_of_immediates
    | Full_of_arbitrary_values_but_not_floats

  val no_specialisation : unit -> t
  val full_of_naked_floats : unit -> t
  val full_of_immediates : unit -> t
  val full_of_arbitrary_values_but_not_floats : unit -> t
end

type make_block_kind =
  | Full_of_values of Tag.Scannable.t * (Value_kind.t list)
  | Full_of_naked_floats
  | Generic_array of Generic_array_specialisation.t

type duplicate_block_kind =
  | Full_of_values_known_length of Tag.Scannable.t
  | Full_of_values_unknown_length of Tag.Scannable.t
  (* CR mshinwell: An "int" case? *)
  | Full_of_naked_floats of { length : Targetint.OCaml.t option; }
  | Generic_array of Generic_array_specialisation.t

(* CR-someday mshinwell: We should have unboxed arrays of int32, int64 and
   nativeint. *)

module Block_access_kind : sig
  (* CR mshinwell: This module needs documenting well to avoid any
     misconceptions about the semantics of the various constructors *)

  type t0 =
    | Value of Value_kind.t
    | Naked_float

  type t =
    | Block of t0
    | Array of t0
    | Generic_array of Generic_array_specialisation.t

  val element_kind : t -> Flambda_kind.t
end

(* CR mshinwell:
   Notes for producing [make_block_kind] / [Duplicate_scannable_block] from
   [Pduparray] and [Pduprecord]:  (Now out of date)

   - For Pduparray:
     - Pgenarray --> Choose_tag_at_runtime
     - Paddrarray --> Full_of_values (tag zero, [Must_scan, ...])
     - Pintarray --> Full_of_values (tag_zero, [Definitely_immediate, ...])
     - Pfloatarray --> Full_of_naked_floats

   - For Pduprecord:

     - Record_regular --> Must_scan (unless none of the contents need
         value_kind in which case, Definitely_immediate)
     - Record_float --> Naked_float
     - Record_unboxed --> suspect this is never generated?
     - Record_inlined --> Must_scan.  Pduprecord doesn't seem to ever change
         the tag, so the new tag doesn't need specifying.
     - Record_extension --> Must_scan, even if the record elements don't need
         value_kind (the first field is a block with tag [Object_tag]).

  * We should check Pgenarray doesn't occur when the float array optimization
    is disabled.

  * Another note: the "bit test" primitive now needs to be compiled out in
    Prepare_lambda.  It indexes into a string using a number of bits.
    (See cmmgen.ml)  Something that is odd about this primitive is that it
    does not appear to have a bounds check.  Maybe it should?
*)

type string_or_bytes = String | Bytes

type init_or_assign = Initialization | Assignment

type comparison = Eq | Neq | Lt | Gt | Le | Ge

type ordered_comparison = Lt | Gt | Le | Ge

type equality_comparison = Eq | Neq

type bigarray_kind =
  | Unknown
  | Float32 | Float64
  | Sint8 | Uint8
  | Sint16 | Uint16
  | Int32 | Int64
  | Int_width_int | Targetint_width_int
  | Complex32 | Complex64

val element_kind_of_bigarray_kind : bigarray_kind -> Flambda_kind.t

type is_safe = Safe | Unsafe

type bigarray_layout = Unknown | C | Fortran

(* CR xclerc: We can use array_kind instead
type block_set_kind =
  | Immediate
  | Pointer
  | Float

val kind_of_block_set_kind : block_set_kind -> Flambda_kind.t
*)

type string_accessor_width =
  | Eight
  | Sixteen
  | Thirty_two
  | Sixty_four

val kind_of_string_accessor_width : string_accessor_width -> Flambda_kind.t

val byte_width_of_string_accessor_width : string_accessor_width -> int

type string_like_value =
  | String
  | Bytes
  | Bigstring

type bytes_like_value =
  | Bytes
  | Bigstring

type num_dimensions = int

type signed_or_unsigned =
  | Signed
  | Unsigned

(** Untagged binary integer arithmetic operations.

    [Swap_byte_endianness] on a [Tagged_immediate] treats the immediate as
    encoding a 16-bit quantity (described in the least significant 16 bits
    of the immediate after untagging) and exchanges the two halves of the
    16-bit quantity.  The higher-order bits are zeroed. *)
type unary_int_arith_op = Neg | Swap_byte_endianness

(** Naked float unary arithmetic operations. *)
type unary_float_arith_op = Abs | Neg

(** Primitives taking exactly one argument. *)
type unary_primitive =
  | Duplicate_block of {
      kind : duplicate_block_kind;
      source_mutability : Effects.mutable_or_immutable;
      destination_mutability : Effects.mutable_or_immutable;
    }
    (** [Duplicate_block] may not be used to change the tag of a block. *)
  | Is_int
  | Get_tag
  | Array_length of Block_access_kind.t
    (* XXX Bigarray_length needs layout & total num. of dimensions *)
  | Bigarray_length of { dimension : int; }
    (* CR mshinwell/xclerc: Invariant check: dimension >= 0 *)
  | String_length of string_or_bytes
  (* XCR pchambart: There are 32 and 64 bits swap, that probably need
     to be represented differently
     mshinwell: I think this should be ok now, please check *)
  | Int_as_pointer
  | Opaque_identity
  | Int_arith of Flambda_kind.Standard_int.t * unary_int_arith_op
  | Float_arith of unary_float_arith_op
  | Num_conv of {
      src : Flambda_kind.Standard_int_or_float.t;
      dst : Flambda_kind.Standard_int_or_float.t;
    }
  | Boolean_not
  (* CR-someday mshinwell: We should maybe change int32.ml and friends to
     use a %-primitive instead of directly calling C stubs for conversions;
     then we could have a single primitive here taking two
     [Flambda_kind.Of_naked_number.t] arguments (one input, one output). *)
  | Unbox_number of Flambda_kind.Boxable_number.t
  | Box_number of Flambda_kind.Boxable_number.t
  | Select_closure of {
      move_from : Closure_id.t;
      move_to : Closure_id.t;
    }
    (** Given the pointer to one closure in some particular set of closures,
        return the pointer to another closure in the same set. *)
  | Project_var of {
      closure_id : Closure_id.t;
      var : Var_within_closure.t;
    }
  (** Read a value from the environment of a closure. Also specifies
      the id of the closure pointed at in the set of closures
      given as argument. *)

(** Binary arithmetic operations on integers. *)
type binary_int_arith_op =
  | Add | Sub | Mul | Div | Mod | And | Or | Xor

(** Shift operations on integers. *)
type int_shift_op = Lsl | Lsr | Asr

(** Naked float binary arithmetic operations. *)
type binary_float_arith_op = Add | Sub | Mul | Div

(* CR mshinwell: Note that the backend primitives for string/bigstring
   access will need a new flag to return (or take) unboxed numbers. *)

(** Primitives taking exactly two arguments. *)
type binary_primitive =
  | Block_load of Block_access_kind.t * Effects.mutable_or_immutable
  | String_or_bigstring_load of string_like_value * string_accessor_width
  | Phys_equal of Flambda_kind.t * equality_comparison
  | Int_arith of Flambda_kind.Standard_int.t * binary_int_arith_op
  | Int_shift of Flambda_kind.Standard_int.t * int_shift_op
  | Int_comp of Flambda_kind.Standard_int.t * signed_or_unsigned
      * ordered_comparison
  | Float_arith of binary_float_arith_op
  | Float_comp of comparison

(** Primitives taking exactly three arguments. *)
type ternary_primitive =
  | Block_set of Block_access_kind.t * init_or_assign
  | Bytes_or_bigstring_set of bytes_like_value * string_accessor_width

(** Primitives taking zero or more arguments. *)
type variadic_primitive =
  (* CR pchambart / mshinwell: Effects of Make_block? *)
  | Make_block of make_block_kind * Effects.mutable_or_immutable
  (* CR mshinwell: Invariant checks -- e.g. that the number of arguments
     matches [num_dimensions] *)
  | Bigarray_set of is_safe * num_dimensions * bigarray_kind * bigarray_layout
    (** Bigarray accesses are an exception to the usual convention here that
        operations are unsafe.  The downside of doing this is that we lose
        the potential to eliminate the ">= 0" part of the bounds check (we
        are never going to eliminate the other part at present as Bigarrays
        are not tracked in the Flambda type system).  However the upsides are
        significant: for safe accesses, the code will be improved for bigarrays
        with dimension >= 2 (since the code for checking the indexes can be
        combined with the accesses themselves -- see [Cmm_helpers]).
        Furthermore, the complexity of expanding the bounds checks does not
        need to be in the Flambda code. *)
  | Bigarray_load of is_safe * num_dimensions * bigarray_kind * bigarray_layout

(** The application of a primitive to its arguments. *)
type t =
  | Unary of unary_primitive * Simple.t
  | Binary of binary_primitive * Simple.t * Simple.t
  | Ternary of ternary_primitive * Simple.t * Simple.t * Simple.t
  | Variadic of variadic_primitive * (Simple.t list)

type primitive_application = t

val invariant : Invariant_env.t -> t -> unit

include Contains_names.S with type t := t

(** Simpler version (e.g. for [Inlining_cost]), where only the actual
    primitive matters, not the arguments. *)
module Without_args : sig
  type t =
    | Unary of unary_primitive
    | Binary of binary_primitive
    | Ternary of ternary_primitive
    | Variadic of variadic_primitive

  val print : Format.formatter -> t -> unit
end

(** A description of the kind of values which a unary primitive expects as
    its arguments. *)
val arg_kind_of_unary_primitive : unary_primitive -> Flambda_kind.t

val args_kind_of_binary_primitive
   : binary_primitive
  -> Flambda_kind.t * Flambda_kind.t

val args_kind_of_ternary_primitive
   : ternary_primitive
  -> Flambda_kind.t * Flambda_kind.t * Flambda_kind.t

type arg_kinds =
  | Variadic of Flambda_kind.t list
  | Variadic_all_of_kind of Flambda_kind.t

val args_kind_of_variadic_primitive : variadic_primitive -> arg_kinds

(** A description of the kinds of values (or in the case of [Unit], the
    actual value) which a primitive returns. *)
type result_kind =
  | Singleton of Flambda_kind.t
  (** The primitive returns a single value of the given kind. *)
  | Unit
  (** The primitive returns the constant unit value. *)

val result_kind_of_unary_primitive : unary_primitive -> result_kind
val result_kind_of_binary_primitive : binary_primitive -> result_kind
val result_kind_of_ternary_primitive : ternary_primitive -> result_kind
val result_kind_of_variadic_primitive : variadic_primitive -> result_kind

(** Describe the kind of the result of the given primitive. *)
val result_kind : t -> result_kind

(** Like the [result_kind]s, but returns the appropriate [Flambda_kind]. *)
val result_kind_of_unary_primitive' : unary_primitive -> Flambda_kind.t
val result_kind_of_binary_primitive' : binary_primitive -> Flambda_kind.t
val result_kind_of_ternary_primitive' : ternary_primitive -> Flambda_kind.t
val result_kind_of_variadic_primitive' : variadic_primitive -> Flambda_kind.t
val result_kind' : t -> Flambda_kind.t


(** Describe the effects and coeffects that the application of the given
    primitive may have. *)
val effects_and_coeffects : t -> Effects.t * Coeffects.t

(** Returns [true] iff the given primitive has neither effects nor
    coeffects. *)
val no_effects_or_coeffects : t -> bool

val at_most_generative_effects : t -> bool

module Eligible_for_cse : sig
  (** Primitive applications that may be replaced by a variable which is let
      bound to a single instance of such application.  Primitives that are
      genuine projections (e.g. [Block_load], etc.) are not eligible, since
      the associated information is propagated through types, not CSE. *)
  type t

  include Contains_names.S with type t := t

  val create : primitive_application -> t option
  val create_exn : primitive_application -> t

  val create_is_int : immediate_or_block:Name.t -> t
  val create_get_tag : block:Name.t -> t

  val eligible : primitive_application -> bool

  val to_primitive : t -> primitive_application

  val fold_args
     : t
    -> init:'a
    -> f:('a -> Simple.t -> 'a * Simple.t)
    -> 'a * t

  (** Total ordering, equality, printing, sets, maps etc. *)
  include Identifiable.S with type t := t

(*
  val apply_name_permutation_set : Set.t -> Name_permutation.t -> Set.t
*)
end

(** Total ordering, printing, sets, maps etc. *)
include Identifiable.S with type t := t

val equal : t -> t -> bool
