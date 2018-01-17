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
(*   Copyright 2017 Jane Street Group LLC                                 *)
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
  | Full_of_values of Tag.Scannable.t * (Flambda_kind.Value_kind.t list)
  | Full_of_naked_floats
  | Generic_array of Generic_array_specialisation.t

type duplicate_block_kind =
  | Full_of_values_known_length of
      Tag.Scannable.t * (Flambda_kind.Value_kind.t list)
  | Full_of_values_unknown_length of Tag.Scannable.t * Flambda_kind.Value_kind.t
  | Full_of_naked_floats of { length : Targetint.OCaml.t option; }
  | Generic_array of Generic_array_specialisation.t

(* CR-someday mshinwell: We should have unboxed arrays of int32, int64 and
   nativeint. *)

module Block_access_kind : sig
  (* CR mshinwell: This module needs documenting well to avoid any
     misconceptions about the semantics of the various constructors *)

  type t0 =
    | Any_value
    | Definitely_immediate
    | Naked_float
    | Fabricated_definitely_pointer

  type t =
    | Block of t0
    | Array of t0
    | Generic_array of Generic_array_specialisation.t

  (** The kind of the field of the block being accessed. *)
  val kind_this_element : t -> Flambda_kind.t

  (** A suitable kind to assign to _all_ the fields of the block being
      accessed. *)
  val kind_all_elements : t -> Flambda_kind.t
end

(* Notes for producing [make_block_kind] / [Duplicate_scannable_block] from
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

type mutable_or_immutable = Immutable | Mutable

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

type bigarray_layout = Unknown | C | Fortran

(* We can use array_kind instead
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
    16-bit quantity. *)
type unary_int_arith_op = Neg | Swap_byte_endianness

(** Naked float unary arithmetic operations. *)
type unary_float_arith_op = Abs | Neg 

(** Primitives taking exactly one argument. *)
type unary_primitive =
  | Duplicate_block of {
      kind : duplicate_block_kind;
      source_mutability : mutable_or_immutable; 
      destination_mutability : mutable_or_immutable; 
    }
    (** [Duplicate_block] may not be used to change the tag of a block. *)
  | Is_int
  | Get_tag of {
      tags_to_sizes : Targetint.OCaml.t Tag.Scannable.Map.t;
    }
  | Array_length of Block_access_kind.t
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
  | Project_closure of Closure_id.t
    (** Extract a closure from some set of closures, ready for function
        application, etc. *)
  | Move_within_set_of_closures of {
      move_from : Closure_id.t;
      move_to : Closure_id.t;
    }
    (** Given the pointer to one closure in some particular set of closures,
        return the pointer to another closure in the same set. *)
  | Project_var of Closure_id.t * Var_within_closure.t
    (** Read a value from the environment of a closure. *)

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
  | Block_load of Block_access_kind.t * mutable_or_immutable
  | String_or_bigstring_load of string_like_value * string_accessor_width
  (* CR mshinwell: Eq_comp should maybe have "phys" in the name somewhere;
     this is physical (in)equality *)
  | Eq_comp of Flambda_kind.t * equality_comparison
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
  | Make_block of make_block_kind * mutable_or_immutable
  (* CR mshinwell: Invariant checks -- e.g. that the number of arguments
     matches [num_dimensions] *)
  | Bigarray_set of num_dimensions * bigarray_kind * bigarray_layout
  | Bigarray_load of num_dimensions * bigarray_kind * bigarray_layout

(** The application of a primitive to its arguments. *)
type t =
  | Unary of unary_primitive * Simple.t
  | Binary of binary_primitive * Simple.t * Simple.t
  | Ternary of ternary_primitive * Simple.t * Simple.t * Simple.t
  | Variadic of variadic_primitive * (Simple.t list)

type primitive_application = t

(** Total ordering, equality, printing, sets, maps etc. *)
include Identifiable.S_no_hash with type t := t

(** Simpler version (e.g. for [Inlining_cost]), where only the actual
    primitive matters, not the arguments. *)
type without_args =
  | Unary of unary_primitive
  | Binary of binary_primitive
  | Ternary of ternary_primitive
  | Variadic of variadic_primitive

(** All free names in a primitive application. *)
val free_names : t -> Name.Set.t

(** Rename variables in a primitive application. *)
val rename_variables : t -> f:(Variable.t -> Variable.t) -> t

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
    actual value) which a primitive expects as its arguments. *)
type result_kind =
  | Singleton of Flambda_kind.t
  (** The primitive returns a single value of the given kind. *)
  | Unit
  (** The primitive returns the constant unit value. *)
  | Never_returns
  (** The primitive does not terminate normally (e.g. by raising an
      exception). *)

(** Describe the kind of the result of the given primitive. *)
val result_kind : t -> result_kind

(** Things that a primitive application does to the world. *)
type effects =
  | No_effects
  (** The primitive does not change the observable state of the world. For
      example, it must not write to any mutable storage, call arbitrary external
      functions or change control flow (e.g. by raising an exception). Note that
      allocation is not "No effects" (see below).

      It is assumed in Flambda that applications of primitives with no
      effects, whose results are not used, may be eliminated.  It is further
      assumed that applications of primitives with no effects may be
      duplicated (and thus possibly executed more than once).

      Exceptions arising from allocation points, for example "out of memory" or
      exceptions propagated from finalizers or signal handlers, are treated as
      "effects out of the ether" and thus ignored for our determination here
      of effectfulness.  The same goes for floating point operations that may
      cause hardware traps on some platforms. *)
  | Only_generative_effects of mutable_or_immutable
  (** The primitive does not change the observable state of the world save for
      possibly affecting the state of the garbage collector by performing an
      allocation. Applications of primitives that only have generative effects
      and whose results are unused may be eliminated by the compiler. However,
      unlike "No effects" primitives, such applications will never be eligible
      for duplication.
      The argument to [Only_generative_effects] states whether the returned
      value from the primitive is mutable. *)
  | Arbitrary_effects
  (** The primitive may have effects beyond those described by [No_effects]
      and [Only_generative_effects]. *)

(** Things that the world does to a primitive application. *)
type coeffects =
  | No_coeffects
  (** "No coeffects" means that the primitive does not observe the effects (in
      the sense described above) of other expressions. For example, it must not
      read from any mutable storage or call arbitrary external functions.

      It is assumed in Flambda that, subject to data dependencies,
      expressions with neither effects nor coeffects may be reordered with
      respect to other expressions. *)
  | Has_coeffects
  (** The primitive may be affected by effects from other expressions. *)

(** Describe the effects and coeffects that the application of the given
    primitive may have. *)
val effects_and_coeffects : t -> effects * coeffects

(** Returns [true] iff the given primitive has neither effects nor
    coeffects. *)
val no_effects_or_coeffects : t -> bool

(** Returns [true] only when the given primitive has both:
    (a) no coeffects; and
    (b) either no effects, or only generative effects. *)
val maybe_generative_effects_but_no_coeffects : t -> bool

module With_fixed_value : sig
  (** Primitive applications that may be replaced by a variable which is let
      bound to a single instance of such application. *)
  type t

  val create : primitive_application -> t option

  val to_primitive : t -> primitive_application

  val free_names : t -> Name.Set.t

  (** Total ordering, equality, printing, sets, maps etc. *)
  include Identifiable.S_no_hash with type t := t
end
