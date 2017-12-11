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

    Primitives that accept int32, int64 or nativeint values always take (or
    return) the unboxed versions.

    No primitive raises an exception.  (Bounds checking is handled
    separately.)
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type array_kind =
  | Dynamic_must_scan_or_naked_float
  | Must_scan
  | Can_scan
  | Naked_float

(* CR mshinwell: "Float" -> "Naked_float"? *)
(* CR mshinwell: Don't call this "kind".  Kind errors are errors in the
   compiler code, whereas a mismatch here may be due to invalid code. *)
(* CR pchambart: Not_a_float can be a float
   Should it be called Any or something like that ? *)
type field_kind = Not_a_float | Float

type string_or_bytes = String | Bytes

type mutable_or_immutable = Immutable | Mutable

type init_or_assign = Initialization | Assignment

type comparison = Eq | Neq | Lt | Gt | Le | Ge

type bigarray_kind =
  | Unknown
  | Float32 | Float64
  | Sint8 | Uint8
  | Sint16 | Uint16
  | Int32 | Int64
  | Int_width_int | Targetint_width_int
  | Complex32 | Complex64

type bigarray_layout = Unknown | C | Fortran

type block_set_kind =
  | Immediate
  | Pointer
  | Float

type string_accessor_width =
  | Eight
  | Sixteen
  | Thirty_two
  | Sixty_four

type bigstring_accessor_width =
  | Sixteen
  | Thirty_two
  | Sixty_four

type num_dimensions = int

type record_representation =
  | Regular
  | Float
  | Unboxed of { inlined : bool; }
  | Inlined of Tag.Scannable.t
  | Extension

(** Untagged binary integer arithmetic operations. *)
type unary_int_arith_op = Neg

(** Naked float unary arithmetic operations. *)
type unary_float_arith_op = Abs | Neg

(** Primitives taking exactly one argument. *)
type unary_primitive =
  | Block_load of int * field_kind * mutable_or_immutable
  (* CR mshinwell: Clarify whether [array_kind] is the kind of the array
     being duplicated or the new array, and check that the effect/coeffect
     judgement is correct. *)
  | Duplicate_array of array_kind * mutable_or_immutable
  | Duplicate_record of {
      repr : record_representation;
      num_fields : int;
    }
  | Is_int
  | Get_tag
  | String_length of string_or_bytes
  (* CR pchambart: There are 32 and 64 bits swap, that probably need
     to be represented differently *)
  | Swap_byte_endianness of Flambda_kind.Standard_int.t
  (** [Swap_byte_endianness] on a [Tagged_immediate] treats the immediate as
      encoding a 16-bit quantity (described in the least significant 16 bits
      of the immediate after untagging) and exchanges the two halves of the
      16-bit quantity. *)
  | Int_as_pointer
  (** [Int_as_pointer] is semantically the same as [Opaque_identity] except
      that the result _cannot_ be scanned by the GC. *)
  | Opaque_identity
  | Int_arith of Flambda_kind.Standard_int.t * unary_int_arith_op
  | Int_conv of {
      src : Flambda_kind.Standard_int.t;
      dst : Flambda_kind.Standard_int.t;
    }
  | Float_arith of unary_float_arith_op
  (* CR-someday mshinwell: We should maybe change int32.ml and friends to
     use a %-primitive instead of directly calling C stubs for conversions;
     then we could have a single primitive here taking two
     [Flambda_kind.Of_naked_number.t] arguments (one input, one output). *)
  | Int_of_float
  | Float_of_int
  | Array_length of array_kind
  | Bigarray_length of { dimension : int; }
  | Unbox_number of Flambda_kind.Boxable_number.t
  | Box_number of Flambda_kind.Boxable_number.t
  | Project_closure of Closure_id.Set.t
    (* CR mshinwell for pchambart: Why is this a set? *)
    (** Every closure_id from the set must come from a different set.
        A projection with multiple potential closures represents a
        conditional projection depending on the given set of closures.
        The set of closures is implicit as there can also be only one
        set defining a given closure_id. *)
  | Move_within_set_of_closures of Closure_id.t Closure_id.Map.t
    (** For each possible value of closures, get a different closure
        from the set. *)
  | Project_var of Var_within_closure.t Closure_id.Map.t
    (** For each possible value of closure, get a different field of the
        closure. *)
  | Boolean_not

(** Binary arithmetic operations on integers. *)
type binary_int_arith_op =
  | Add | Sub | Mul | Div | Mod | And | Or | Xor

(** Shift operations on integers. *)
type int_shift_op = Lsl | Lsr | Asr

(** Naked float binary arithmetic operations. *)
type binary_float_arith_op = Add | Sub | Mul | Div

(** Primitives taking exactly two arguments. *)
type binary_primitive =
  | Block_load_computed_index
  | Block_set of int * block_set_kind * init_or_assign
  | Int_arith of Flambda_kind.Standard_int.t * binary_int_arith_op
  | Int_shift of Flambda_kind.Standard_int.t * int_shift_op
  | Int_comp of Flambda_kind.Standard_int.t * comparison
  | Int_comp_unsigned of comparison
  | Float_arith of binary_float_arith_op
  | Float_comp of comparison
  | Bit_test
  | Array_load of array_kind
  | String_load of string_accessor_width
  (* CR-someday mshinwell: It seems as if [Cmmgen]'s handling of the
     bigstring accessors could be tidied up so as to integrate it with the
     (older) bigarray accessor (Pbigarrayref). *)
  | Bigstring_load of bigstring_accessor_width

(** Primitives taking exactly three arguments. *)
type ternary_primitive =
  | Block_set_computed of Flambda_kind.scanning * init_or_assign
  | Bytes_set of string_accessor_width
  | Array_set of array_kind
  | Bigstring_set of bigstring_accessor_width

(** Primitives taking zero or more arguments. *)
type variadic_primitive =
  (* CR pchambart / mshinwell: Effects of Make_block? *)
  | Make_block of Tag.Scannable.t * mutable_or_immutable * Flambda_arity.t
  | Make_array of array_kind * mutable_or_immutable
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
