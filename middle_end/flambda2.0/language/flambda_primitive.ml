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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module K = Flambda_kind

module Value_kind = struct
  type t =
    | Anything
    | Definitely_pointer
    | Definitely_immediate

  let print ppf t =
    match t with
    | Anything -> Format.pp_print_string ppf "Anything"
    | Definitely_pointer -> Format.pp_print_string ppf "Definitely_pointer"
    | Definitely_immediate -> Format.pp_print_string ppf "Definitely_immediate"

  let compare = Stdlib.compare
end

module Generic_array_specialisation = struct
  type t =
    | No_specialisation
    | Full_of_naked_floats
    | Full_of_immediates
    | Full_of_arbitrary_values_but_not_floats

  let check () =
    if not Config.flat_float_array then begin
      Misc.fatal_errorf "Generic arrays cannot be used unless the \
        float array optimisation has been enabled at compiler configuration \
        time"
    end

  let no_specialisation () =
    check ();
    No_specialisation

  let full_of_naked_floats () =
    check ();
    Full_of_naked_floats

  let full_of_immediates () =
    check ();
    Full_of_immediates

  let full_of_arbitrary_values_but_not_floats () =
    check ();
    Full_of_arbitrary_values_but_not_floats

  let print ppf t =
    match t with
    | No_specialisation -> Format.pp_print_string ppf "not-specialised"
    | Full_of_naked_floats -> Format.pp_print_string ppf "floats"
    | Full_of_immediates -> Format.pp_print_string ppf "imms"
    | Full_of_arbitrary_values_but_not_floats ->
      Format.pp_print_string ppf "non-floats"

  let compare = Stdlib.compare
end

type mutable_or_immutable = Immutable | Mutable

let print_mutable_or_immutable ppf mut =
  match mut with
  | Immutable -> Format.pp_print_string ppf "Immutable"
  | Mutable -> Format.pp_print_string ppf "Mutable"

let compare_mutable_or_immutable mut1 mut2 =
  match mut1, mut2 with
  | Immutable, Immutable
  | Mutable, Mutable -> 0
  | Immutable, Mutable -> -1
  | Mutable, Immutable -> 1

type effects =
  | No_effects
  | Only_generative_effects of mutable_or_immutable
  | Arbitrary_effects

type coeffects = No_coeffects | Has_coeffects

(* CR mshinwell: These types should probably go in their own modules with
   a comparison function next to each. *)

type make_block_kind =
  | Full_of_values of Tag.Scannable.t * (Value_kind.t list)
  | Full_of_naked_floats
  | Generic_array of Generic_array_specialisation.t

let print_make_block_kind ppf kind =
  match kind with
  | Full_of_values (tag, shape) ->
    Format.fprintf ppf "(Full_of_values (tag %a) (%a))"
      Tag.Scannable.print tag
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
        Value_kind.print) shape
  | Full_of_naked_floats -> Format.pp_print_string ppf "Full_of_naked_floats"
  | Generic_array generic ->
    Format.fprintf ppf "(Generic %a)"
      Generic_array_specialisation.print generic

let compare_make_block_kind kind1 kind2 =
  match kind1, kind2 with
  | Full_of_values (tag1, shape1), Full_of_values (tag2, shape2) ->
    let c = Tag.Scannable.compare tag1 tag2 in
    if c <> 0 then c
    else Misc.Stdlib.List.compare Value_kind.compare shape1 shape2
  | Full_of_values _, _ -> -1
  | _, Full_of_values _ -> 1
  | Full_of_naked_floats, Full_of_naked_floats -> 0
  | Generic_array spec1, Generic_array spec2 ->
    Generic_array_specialisation.compare spec1 spec2
  | _, Generic_array _ -> -1
  | Generic_array _, _ -> 1

type duplicate_block_kind =
  | Full_of_values_known_length of Tag.Scannable.t
  | Full_of_values_unknown_length of Tag.Scannable.t
  | Full_of_naked_floats of { length : Targetint.OCaml.t option; }
  | Generic_array of Generic_array_specialisation.t

let print_duplicate_block_kind ppf (kind : duplicate_block_kind) =
  match kind with
  | Full_of_values_known_length tag ->
    Format.fprintf ppf "%a" Tag.Scannable.print tag
  | Full_of_values_unknown_length tag ->
    Format.fprintf ppf "%a" Tag.Scannable.print tag
  | Full_of_naked_floats { length = None; } ->
    Format.pp_print_string ppf "floats"
  | Full_of_naked_floats { length = Some length; } ->
    Format.fprintf ppf "%a floats"
      Targetint.OCaml.print length
  | Generic_array spec ->
    Format.fprintf ppf "generic %a"
      Generic_array_specialisation.print spec

(* CR-someday mshinwell: We should have unboxed arrays of int32, int64 and
   nativeint. *)

module Block_access_kind = struct
  type t0 =
    | Value of Value_kind.t
    | Naked_float
    | Fabricated of Value_kind.t

  type t =
    | Block of t0
    | Array of t0
    | Generic_array of Generic_array_specialisation.t

  let element_kind t =
    match t with
    | Block (Value _) | Array (Value _) -> K.value
    | Block Naked_float | Array Naked_float -> K.naked_float
    | Block (Fabricated _) | Array (Fabricated _) -> K.fabricated
    | Generic_array _ -> Misc.fatal_error "Not yet implemented"

  let compare_t0 (t0_1 : t0) t0_2 = Stdlib.compare t0_1 t0_2

  let compare t1 t2 =
    match t1, t2 with
    | Block _, Array _ -> -1
    | Block _, Generic_array _ -> -1
    | Array _, Block _ -> 1
    | Array _, Generic_array _ -> -1
    | Generic_array _, Block _ -> 1
    | Generic_array _, Array _ -> 1
    | Block t0_1, Block t0_2 -> compare_t0 t0_1 t0_2
    | Array t0_1, Array t0_2 -> compare_t0 t0_1 t0_2
    | Generic_array spec1, Generic_array spec2 ->
      Generic_array_specialisation.compare spec1 spec2

  let print_t0 ppf t0 =
    match t0 with
    | Value kind ->
      Format.fprintf ppf "@[(Value %a)@]" Value_kind.print kind
    | Naked_float -> Format.pp_print_string ppf "Naked_float"
    | Fabricated kind ->
      Format.fprintf ppf "@[(Fabricated %a)@]" Value_kind.print kind

  let print ppf kind =
    match kind with
    | Block t0 -> Format.fprintf ppf "(Block %a)" print_t0 t0
    | Array t0 -> Format.fprintf ppf "(Array %a)" print_t0 t0
    | Generic_array spec ->
      Format.fprintf ppf "(Generic %a)"
        Generic_array_specialisation.print spec
end

type string_or_bytes = String | Bytes

type init_or_assign = Initialization | Assignment

let print_init_or_assign ppf init_or_assign =
  let fprintf = Format.fprintf in
  match init_or_assign with
  | Initialization -> fprintf ppf "init"
  | Assignment -> fprintf ppf "assign"

type array_like_operation = Reading | Writing

let effects_of_operation operation =
  match operation with
  | Reading -> No_effects
  | Writing -> Arbitrary_effects

let reading_from_an_array_like_thing =
  let effects = effects_of_operation Reading in
  effects, Has_coeffects

(* CR-someday mshinwell: Change this when [Obj.truncate] is removed (although
   beware, bigarrays will still be resizable). *)
let writing_to_an_array_like_thing =
  let effects = effects_of_operation Writing in
  (* Care: the bounds check may read a mutable place---namely the size of
     the block (for [Bytes_set] and [Array_set]) or the dimension of the
     bigarray.  As such these primitives have coeffects. *)
  (* XXX But there are no bounds checks now *)
  effects, Has_coeffects

let array_like_thing_index_kind = K.value

(* CR mshinwell: Improve naming *)
let bigarray_kind = K.value
let bigstring_kind = K.value
let block_kind = K.value
let block_element_kind = K.value
let string_or_bytes_kind = K.value

type comparison = Eq | Neq | Lt | Gt | Le | Ge

let print_comparison ppf c =
  let fprintf = Format.fprintf in
  begin match c with
  | Neq -> fprintf ppf "<>"
  | Eq -> fprintf ppf "="
  | Lt -> fprintf ppf "<"
  | Le -> fprintf ppf "<="
  | Gt -> fprintf ppf ">"
  | Ge -> fprintf ppf ">="
  end

type signed_or_unsigned =
  | Signed
  | Unsigned

type ordered_comparison = Lt | Gt | Le | Ge

let print_ordered_comparison ppf signedness c =
  let fprintf = Format.fprintf in
  match signedness with
  | Unsigned ->
    begin match c with
    | Lt -> fprintf ppf "<"
    | Le -> fprintf ppf "<="
    | Gt -> fprintf ppf ">"
    | Ge -> fprintf ppf ">="
    end
  | Signed ->
    begin match c with
    | Lt -> fprintf ppf "<u"
    | Le -> fprintf ppf "<=u"
    | Gt -> fprintf ppf ">u"
    | Ge -> fprintf ppf ">=u"
    end

type equality_comparison = Eq | Neq

let print_equality_comparison ppf op =
  match op with
  | Eq -> Format.pp_print_string ppf "Eq"
  | Neq -> Format.pp_print_string ppf "Neq"

type bigarray_kind =
  | Unknown
  | Float32 | Float64
  | Sint8 | Uint8
  | Sint16 | Uint16
  | Int32 | Int64
  | Int_width_int | Targetint_width_int
  | Complex32 | Complex64

let element_kind_of_bigarray_kind k =
  match k with
  | Unknown -> K.value
  | Float32
  | Float64 -> K.naked_float
  | Sint8
  | Uint8
  | Sint16
  | Uint16 -> K.naked_immediate
  | Int32 -> K.naked_int32
  | Int64 -> K.naked_int64
  | Int_width_int -> K.naked_immediate
  | Targetint_width_int -> K.naked_nativeint
  | Complex32
  | Complex64 ->
    (* See [copy_two_doubles] in bigarray_stubs.c. *)
    K.value

(*
let print_bigarray_kind ppf k =
  let fprintf = Format.fprintf in
  match k with
  | Unknown -> fprintf ppf "unknown"
  | Float32 -> fprintf ppf "float32"
  | Float64 -> fprintf ppf "float64"
  | Sint8 -> fprintf ppf "sint8"
  | Uint8 -> fprintf ppf "uint8"
  | Sint16 -> fprintf ppf "sint16"
  | Uint16 -> fprintf ppf "uint16"
  | Int32 -> fprintf ppf "int32"
  | Int64 -> fprintf ppf "int64"
  | Int_width_int -> fprintf ppf "int_width_int"
  | Targetint_width_int -> fprintf ppf "targetint_width_int"
  | Complex32 -> fprintf ppf "complex32"
  | Complex64 -> fprintf ppf "complex64"
*)

type bigarray_layout = Unknown | C | Fortran

(*
let print_bigarray_layout ppf l =
  let fprintf = Format.fprintf in
  match l with
  | Unknown -> fprintf ppf "unknown"
  | C -> fprintf ppf "C"
  | Fortran -> fprintf ppf "fortran"
*)

type string_like_value =
  | String
  | Bytes
  | Bigstring

let print_string_like_value ppf s =
  match s with
  | String -> Format.pp_print_string ppf "string"
  | Bytes -> Format.pp_print_string ppf "bytes"
  | Bigstring -> Format.pp_print_string ppf "bigstring"

type bytes_like_value =
  | Bytes
  | Bigstring

let print_bytes_like_value ppf b =
  match b with
  | Bytes -> Format.pp_print_string ppf "bytes"
  | Bigstring -> Format.pp_print_string ppf "bigstring"

type string_accessor_width =
  | Eight
  | Sixteen
  | Thirty_two
  | Sixty_four

let print_string_accessor_width ppf w =
  let fprintf = Format.fprintf in
  match w with
  | Eight -> fprintf ppf "8"
  | Sixteen -> fprintf ppf "16"
  | Thirty_two -> fprintf ppf "32"
  | Sixty_four -> fprintf ppf "64"

let byte_width_of_string_accessor_width width =
  match width with
  | Eight -> 1
  | Sixteen -> 2
  | Thirty_two -> 4
  | Sixty_four -> 8

let kind_of_string_accessor_width width =
  match width with
  | Eight | Sixteen -> K.value
  | Thirty_two -> K.naked_int32
  | Sixty_four -> K.naked_int64

type num_dimensions = int

let print_num_dimensions ppf d =
  Format.fprintf ppf "%d" d

type unary_int_arith_op = Neg | Swap_byte_endianness

let print_unary_int_arith_op ppf o =
  let fprintf = Format.fprintf in
  match o with
  | Neg -> fprintf ppf "~"
  | Swap_byte_endianness -> fprintf ppf "bswap"

type unary_float_arith_op = Abs | Neg

let print_unary_float_arith_op ppf o =
  let fprintf = Format.fprintf in
  match o with
  | Abs -> fprintf ppf "abs"
  | Neg -> fprintf ppf "~"

type arg_kinds =
  | Variadic of K.t list
  | Variadic_all_of_kind of K.t

type result_kind =
  | Singleton of K.t
  | Unit

type unary_primitive =
  | Duplicate_block of {
      kind : duplicate_block_kind;
      source_mutability : mutable_or_immutable; 
      destination_mutability : mutable_or_immutable; 
    }
  | Is_int
  | Get_tag of {
      tags_to_sizes : Targetint.OCaml.t Tag.Map.t;
    }
  | Discriminant_of_int
  | Array_length of Block_access_kind.t
  | Bigarray_length of { dimension : int; }
  | String_length of string_or_bytes
  | Int_as_pointer
  | Opaque_identity
  | Int_arith of Flambda_kind.Standard_int.t * unary_int_arith_op
  | Float_arith of unary_float_arith_op
  | Num_conv of {
      src : Flambda_kind.Standard_int_or_float.t;
      dst : Flambda_kind.Standard_int_or_float.t;
    }
  | Boolean_not
  | Unbox_number of Flambda_kind.Boxable_number.t
  | Box_number of Flambda_kind.Boxable_number.t
  | Project_closure of Closure_id.t
  | Move_within_set_of_closures of {
      move_from : Closure_id.t;
      move_to : Closure_id.t;
    }
  | Project_var of Var_within_closure.t

let compare_unary_primitive p1 p2 =
  let unary_primitive_numbering p =
    match p with
    | Duplicate_block _ -> 0
    | Is_int -> 1
    | Get_tag _ -> 2
    | Discriminant_of_int -> 3
    | Array_length _ -> 4
    | Bigarray_length _ -> 5
    | String_length _ -> 6
    | Int_as_pointer -> 7
    | Opaque_identity -> 8
    | Int_arith _ -> 9
    | Float_arith _ -> 10
    | Num_conv _ -> 11
    | Boolean_not -> 12
    | Unbox_number _ -> 13
    | Box_number _ -> 14
    | Project_closure _ -> 15
    | Move_within_set_of_closures _ -> 16
    | Project_var _ -> 17
  in
  match p1, p2 with
  | Duplicate_block { kind = kind1;
        source_mutability = source_mutability1;
        destination_mutability = destination_mutability1;
      },
    Duplicate_block { kind = kind2;
        source_mutability = source_mutability2;
        destination_mutability = destination_mutability2;
      } ->
    let c = Stdlib.compare kind1 kind2 in
    if c <> 0 then c
    else
      let c = Stdlib.compare source_mutability1 source_mutability2 in
      if c <> 0 then c
      else
        Stdlib.compare destination_mutability1 destination_mutability2
  | Is_int, Is_int -> 0
  | Get_tag { tags_to_sizes = tags_to_sizes1; },
      Get_tag { tags_to_sizes = tags_to_sizes2; } ->
    Tag.Map.compare Targetint.OCaml.compare
      tags_to_sizes1 tags_to_sizes2
  | Discriminant_of_int, Discriminant_of_int -> 0
  | String_length kind1, String_length kind2 ->
    Stdlib.compare kind1 kind2
  | Int_arith (kind1, op1), Int_arith (kind2, op2) ->
    let c = K.Standard_int.compare kind1 kind2 in
    if c <> 0 then c
    else Stdlib.compare op1 op2
  | Num_conv { src = src1; dst = dst1; },
      Num_conv { src = src2; dst = dst2; } ->
    let c = K.Standard_int_or_float.compare src1 src2 in
    if c <> 0 then c
    else K.Standard_int_or_float.compare dst1 dst2
  | Float_arith op1, Float_arith op2 ->
    Stdlib.compare op1 op2
  | Array_length kind1, Array_length kind2 ->
    Stdlib.compare kind1 kind2
  | Bigarray_length { dimension = dim1; },
      Bigarray_length { dimension = dim2; } ->
    Stdlib.compare dim1 dim2
  | Unbox_number kind1, Unbox_number kind2 ->
    K.Boxable_number.compare kind1 kind2
  | Box_number kind1, Box_number kind2 ->
    K.Boxable_number.compare kind1 kind2
  | Project_closure closure_id1, Project_closure closure_id2 ->
    Closure_id.compare closure_id1 closure_id2
  | Move_within_set_of_closures {
        move_from = move_from1; move_to = move_to1; },
      Move_within_set_of_closures {
        move_from = move_from2; move_to = move_to2; } ->
    let c = Closure_id.compare move_from1 move_from2 in
    if c <> 0 then c
    else Closure_id.compare move_to1 move_to2
  | Project_var var_within_closure1, Project_var var_within_closure2 ->
    Var_within_closure.compare var_within_closure1 var_within_closure2
  | (Duplicate_block _
    | Is_int
    | Get_tag _
    | Discriminant_of_int
    | String_length _
    | Int_as_pointer
    | Opaque_identity
    | Int_arith _
    | Num_conv _
    | Boolean_not
    | Float_arith _
    | Array_length _
    | Bigarray_length _
    | Unbox_number _
    | Box_number _
    | Project_closure _
    | Move_within_set_of_closures _
    | Project_var _), _ ->
    Stdlib.compare (unary_primitive_numbering p1)
      (unary_primitive_numbering p2)

let print_unary_primitive ppf p =
  let fprintf = Format.fprintf in
  match p with
  | Duplicate_block { kind; source_mutability; destination_mutability; } ->
    fprintf ppf "(Duplicate_array %a (source %a) (dest %a)"
      print_duplicate_block_kind kind
      print_mutable_or_immutable source_mutability
      print_mutable_or_immutable destination_mutability
  | Is_int -> fprintf ppf "Is_int"
  | Get_tag _ -> fprintf ppf "Get_tag"
  | Discriminant_of_int -> fprintf ppf "Discriminant_of_int"
  | String_length _ -> fprintf ppf "String_length"
  | Int_as_pointer -> fprintf ppf "Int_as_pointer"
  | Opaque_identity -> fprintf ppf "Opaque_identity"
  | Int_arith (_k, o) -> print_unary_int_arith_op ppf o
  | Num_conv { src; dst; } ->
    fprintf ppf "Conv_%a_to_%a"
      Flambda_kind.Standard_int_or_float.print src
      Flambda_kind.Standard_int_or_float.print dst
  | Boolean_not -> fprintf ppf "Boolean_not"
  | Float_arith o -> print_unary_float_arith_op ppf o
  | Array_length _ -> fprintf ppf "Array_length"
  | Bigarray_length { dimension; } ->
    fprintf ppf "Bigarray_length %a" print_num_dimensions dimension
  | Unbox_number k ->
    fprintf ppf "Unbox_%a" K.Boxable_number.print_lowercase k
  | Box_number k ->
    fprintf ppf "Box_%a" K.Boxable_number.print_lowercase k
  | Project_closure closure_id ->
    Format.fprintf ppf "@[(Project_closure@ %a)@]"
      Closure_id.print closure_id
  | Move_within_set_of_closures { move_from; move_to; } ->
    Format.fprintf ppf "@[(Move_within_set_of_closures@ \
        (move_from %a)@ (move_to %a))@]"
      Closure_id.print move_from
      Closure_id.print move_to
  | Project_var var_within_closure ->
    Format.fprintf ppf "@[(Project_var@ %a)@]"
      Var_within_closure.print var_within_closure

let arg_kind_of_unary_primitive p =
  match p with
  | Duplicate_block _ -> K.value
  | Is_int -> K.value
  | Get_tag _ -> K.value
  | Discriminant_of_int -> K.value
  | String_length _ -> K.value
  | Int_as_pointer -> K.value
  | Opaque_identity -> K.value
  | Int_arith (kind, _) -> K.Standard_int.to_kind kind
  | Num_conv { src; dst = _; } -> K.Standard_int_or_float.to_kind src
  | Boolean_not -> K.value
  | Float_arith _ -> K.naked_float
  | Array_length _
  | Bigarray_length _ -> K.value
  | Unbox_number _ -> K.value
  | Box_number kind -> K.Boxable_number.to_kind kind
  | Project_closure _ -> K.fabricated
  | Move_within_set_of_closures _
  | Project_var _ -> K.value

let result_kind_of_unary_primitive p : result_kind =
  match p with
  | Duplicate_block _ -> Singleton K.value
  | Is_int -> Singleton K.fabricated
  | String_length _ -> Singleton K.value
  | Get_tag _ -> Singleton K.fabricated
  | Discriminant_of_int -> Singleton K.fabricated
  | Int_as_pointer ->
    (* This primitive is *only* to be used when the resulting pointer points
       at something which is a valid OCaml value (even if outside of the
       heap). *)
    Singleton K.value
  | Opaque_identity -> Singleton K.value
  | Int_arith (kind, _) -> Singleton (K.Standard_int.to_kind kind)
  | Num_conv { src = _; dst; } ->
    Singleton (K.Standard_int_or_float.to_kind dst)
  | Boolean_not -> Singleton K.value
  | Float_arith _ -> Singleton K.naked_float
  | Array_length _
  | Bigarray_length _ -> Singleton K.value
  | Unbox_number kind -> Singleton (K.Boxable_number.to_kind kind)
  | Box_number _
  | Project_closure _
  | Move_within_set_of_closures _ -> Singleton K.value
  | Project_var _ -> Singleton K.value

let effects_and_coeffects_of_unary_primitive p =
  match p with
  | Duplicate_block { kind = _;
      source_mutability; destination_mutability; _ } ->
    begin match source_mutability with
    | Immutable ->
      (* [Obj.truncate] has now been removed. *)
      Only_generative_effects destination_mutability, No_coeffects
    | Mutable ->
      Only_generative_effects destination_mutability, Has_coeffects
    end
  | Is_int -> No_effects, No_coeffects
  | Get_tag _ ->
    (* [Obj.truncate] has now been removed. *)
    No_effects, No_coeffects
  | Discriminant_of_int -> No_effects, No_coeffects
  | String_length _ -> reading_from_an_array_like_thing
  | Int_as_pointer
  | Opaque_identity -> Arbitrary_effects, Has_coeffects
  | Int_arith (_, (Neg | Swap_byte_endianness))
  | Num_conv _
  | Boolean_not
  | Float_arith (Abs | Neg) -> No_effects, No_coeffects
  | Array_length _ ->
    reading_from_an_array_like_thing
  | Bigarray_length { dimension = _; } ->
    reading_from_an_array_like_thing
  | Unbox_number _ ->
    No_effects, No_coeffects
  | Box_number _ ->
    Only_generative_effects Immutable, No_coeffects
  | Project_closure _
  | Move_within_set_of_closures _
  | Project_var _ -> No_effects, No_coeffects

type binary_int_arith_op =
  | Add | Sub | Mul | Div | Mod | And | Or | Xor

let print_binary_int_arith_op ppf o =
  let fprintf = Format.fprintf in
  match o with
  | Add -> fprintf ppf "+"
  | Sub -> fprintf ppf "-"
  | Mul -> fprintf ppf "*"
  | Div -> fprintf ppf "/"
  | Mod -> fprintf ppf "mod"
  | And -> fprintf ppf "and"
  | Or -> fprintf ppf "or"
  | Xor -> fprintf ppf "xor"

type int_shift_op = Lsl | Lsr | Asr

let print_int_shift_op ppf o =
  let fprintf = Format.fprintf in
  match o with
  | Lsl -> fprintf ppf "lsl"
  | Lsr -> fprintf ppf "lsr"
  | Asr -> fprintf ppf "asr"

type binary_float_arith_op = Add | Sub | Mul | Div

let print_binary_float_arith_op ppf o =
  let fprintf = Format.fprintf in
  match o with
  | Add -> fprintf ppf "+."
  | Sub -> fprintf ppf "-."
  | Mul -> fprintf ppf "*."
  | Div -> fprintf ppf "/."

type binary_primitive =
  | Block_load of Block_access_kind.t * mutable_or_immutable
  | String_or_bigstring_load of string_like_value * string_accessor_width
  | Phys_equal of Flambda_kind.t * equality_comparison
  | Int_arith of Flambda_kind.Standard_int.t * binary_int_arith_op
  | Int_shift of Flambda_kind.Standard_int.t * int_shift_op
  | Int_comp of Flambda_kind.Standard_int.t * signed_or_unsigned
      * ordered_comparison
  | Float_arith of binary_float_arith_op
  | Float_comp of comparison

let compare_binary_primitive p1 p2 =
  let binary_primitive_numbering p =
    match p with
    | Block_load _ -> 0
    | String_or_bigstring_load _ -> 1
    | Phys_equal _ -> 2
    | Int_arith _ -> 3
    | Int_shift _ -> 4
    | Int_comp _ -> 5
    | Float_arith _ -> 6
    | Float_comp _ -> 7
  in
  match p1, p2 with
  | Block_load (kind1, mut1), Block_load (kind2, mut2) ->
    let c = Block_access_kind.compare kind1 kind2 in
    if c <> 0 then c
    else compare_mutable_or_immutable mut1 mut2
  | String_or_bigstring_load (string_like1, width1),
      String_or_bigstring_load (string_like2, width2) ->
    let c = Stdlib.compare string_like1 string_like2 in
    if c <> 0 then c
    else Stdlib.compare width1 width2
  | Phys_equal (kind1, comp1), Phys_equal (kind2, comp2) ->
    let c = K.compare kind1 kind2 in
    if c <> 0 then c
    else Stdlib.compare comp1 comp2
  | Int_arith (kind1, op1), Int_arith (kind2, op2) ->
    let c = K.Standard_int.compare kind1 kind2 in
    if c <> 0 then c
    else Stdlib.compare op1 op2
  | Int_shift (kind1, op1), Int_shift (kind2, op2) ->
    let c = K.Standard_int.compare kind1 kind2 in
    if c <> 0 then c
    else Stdlib.compare op1 op2
  | Int_comp (kind1, signedness1, comp1),
    Int_comp (kind2, signedness2, comp2) ->
    let c = K.Standard_int.compare kind1 kind2 in
    if c <> 0 then c
    else
      let c = Stdlib.compare signedness1 signedness2 in
      if c <> 0 then c
      else Stdlib.compare comp1 comp2
  | Float_arith op1, Float_arith op2 ->
    Stdlib.compare op1 op2
  | Float_comp comp1, Float_comp comp2 ->
    Stdlib.compare comp1 comp2
  | (Block_load _
    | String_or_bigstring_load _
    | Phys_equal _
    | Int_arith _
    | Int_shift _
    | Int_comp _
    | Float_arith _
    | Float_comp _), _ ->
    Stdlib.compare (binary_primitive_numbering p1)
      (binary_primitive_numbering p2)

let print_binary_primitive ppf p =
  let fprintf = Format.fprintf in
  match p with
  | Block_load (kind, mut) ->
    fprintf ppf "@[(Block_load %a %a)@]"
      Block_access_kind.print kind
      print_mutable_or_immutable mut
  | String_or_bigstring_load (string_like, width) ->
    fprintf ppf "@[(String_load %a %a)@]"
      print_string_like_value string_like
      print_string_accessor_width width
  | Phys_equal (kind, op) ->
    Format.fprintf ppf "@[(Phys_equal %a %a)@]"
      K.print kind
      print_equality_comparison op
  | Int_arith (_k, op) -> print_binary_int_arith_op ppf op
  | Int_shift (_k, op) -> print_int_shift_op ppf op
  | Int_comp (_, signedness, c) -> print_ordered_comparison ppf signedness c
  | Float_arith op -> print_binary_float_arith_op ppf op
  | Float_comp c -> print_comparison ppf c; fprintf ppf "."

let args_kind_of_binary_primitive p =
  match p with
  | Block_load _ ->
    block_kind, array_like_thing_index_kind
  | String_or_bigstring_load ((String | Bytes), _) ->
    string_or_bytes_kind, array_like_thing_index_kind
  | String_or_bigstring_load (Bigstring, _) ->
    bigstring_kind, array_like_thing_index_kind
  | Phys_equal (kind, _) -> kind, kind
  | Int_arith (kind, _) ->
    let kind = K.Standard_int.to_kind kind in
    kind, kind
  | Int_shift (kind, _) ->
    K.Standard_int.to_kind kind, K.naked_immediate
  | Int_comp (kind, _, _) ->
    let kind = K.Standard_int.to_kind kind in
    kind, kind
  | Float_arith _
  | Float_comp _ -> K.naked_float, K.naked_float

let result_kind_of_binary_primitive p : result_kind =
  match p with
  | Block_load (block_access_kind, _) ->
    Singleton (Block_access_kind.element_kind block_access_kind)
  | String_or_bigstring_load (_, (Eight | Sixteen)) ->
    Singleton K.value
  | String_or_bigstring_load (_, Thirty_two) ->
    Singleton K.naked_int32
  | String_or_bigstring_load (_, Sixty_four) ->
    Singleton K.naked_int64
  | Int_arith (kind, _)
  | Int_shift (kind, _) -> Singleton (K.Standard_int.to_kind kind)
  | Float_arith _ -> Singleton K.naked_float
  (* CR mshinwell: Change [Phys_equal] to return kind [Fabricated] *)
  | Phys_equal _
  | Int_comp _
  | Float_comp _ -> Singleton K.value

let effects_and_coeffects_of_binary_primitive p =
  match p with
  | Block_load _ -> reading_from_an_array_like_thing
  | Phys_equal _ -> No_effects, No_coeffects
  | Int_arith (_kind, (Add | Sub | Mul | Div | Mod | And | Or | Xor)) ->
    No_effects, No_coeffects
  | Int_shift _ -> No_effects, No_coeffects
  | Int_comp _ -> No_effects, No_coeffects
  | Float_arith (Add | Sub | Mul | Div) -> No_effects, No_coeffects
  | Float_comp _ -> No_effects, No_coeffects
  | String_or_bigstring_load _ -> reading_from_an_array_like_thing

type ternary_primitive =
  | Block_set of Block_access_kind.t * init_or_assign
  | Bytes_or_bigstring_set of bytes_like_value * string_accessor_width

let compare_ternary_primitive p1 p2 =
  let ternary_primitive_numbering p =
    match p with
    | Block_set _ -> 0
    | Bytes_or_bigstring_set _ -> 1
  in
  match p1, p2 with
  | Block_set (kind1, init_or_assign1),
      Block_set (kind2, init_or_assign2) ->
    let c = Block_access_kind.compare kind1 kind2 in
    if c <> 0 then c
    else Stdlib.compare init_or_assign1 init_or_assign2
  | Bytes_or_bigstring_set (kind1, width1),
      Bytes_or_bigstring_set (kind2, width2) ->
    let c = Stdlib.compare kind1 kind2 in
    if c <> 0 then c
    else Stdlib.compare width1 width2
  | (Block_set _
    | Bytes_or_bigstring_set _), _ ->
    Stdlib.compare (ternary_primitive_numbering p1)
      (ternary_primitive_numbering p2)

let print_ternary_primitive ppf p =
  let fprintf = Format.fprintf in
  match p with
  | Block_set (kind, init) ->
    fprintf ppf "block_set[%a,%a]"
      Block_access_kind.print kind
      print_init_or_assign init
  | Bytes_or_bigstring_set (kind, string_accessor_width) ->
    fprintf ppf "bytes_set[%a,%a]"
      print_bytes_like_value kind
      print_string_accessor_width string_accessor_width

let args_kind_of_ternary_primitive p =
  match p with
  | Block_set _ ->
    block_kind, array_like_thing_index_kind, block_element_kind
  | Bytes_or_bigstring_set (Bytes, (Eight | Sixteen)) ->
    string_or_bytes_kind, array_like_thing_index_kind,
      K.value
  | Bytes_or_bigstring_set (Bytes, Thirty_two) ->
    string_or_bytes_kind, array_like_thing_index_kind,
      K.naked_int32
  | Bytes_or_bigstring_set (Bytes, Sixty_four) ->
    string_or_bytes_kind, array_like_thing_index_kind,
      K.naked_int64
  | Bytes_or_bigstring_set (Bigstring, (Eight | Sixteen)) ->
    bigstring_kind, array_like_thing_index_kind,
      K.value
  | Bytes_or_bigstring_set (Bigstring, Thirty_two) ->
    bigstring_kind, array_like_thing_index_kind,
      K.naked_int32
  | Bytes_or_bigstring_set (Bigstring, Sixty_four) ->
    bigstring_kind, array_like_thing_index_kind,
      K.naked_int64

let result_kind_of_ternary_primitive p : result_kind =
  match p with
  | Block_set _
  | Bytes_or_bigstring_set _ -> Unit

let effects_and_coeffects_of_ternary_primitive p =
  match p with
  | Block_set _
  | Bytes_or_bigstring_set _ -> writing_to_an_array_like_thing

type variadic_primitive =
  | Make_block of make_block_kind * mutable_or_immutable
  | Bigarray_set of num_dimensions * bigarray_kind * bigarray_layout
  | Bigarray_load of num_dimensions * bigarray_kind * bigarray_layout

let compare_variadic_primitive p1 p2 =
  let variadic_primitive_numbering p =
    match p with
    | Make_block _ -> 0
    | Bigarray_set _ -> 1
    | Bigarray_load _ -> 2
  in
  match p1, p2 with
  | Make_block (kind1, mut1), Make_block (kind2, mut2) ->
    let c = compare_make_block_kind kind1 kind2 in
    if c <> 0 then c
    else Stdlib.compare mut1 mut2
  | Bigarray_set (num_dims1, kind1, layout1),
      Bigarray_set (num_dims2, kind2, layout2) ->
    let c = Stdlib.compare num_dims1 num_dims2 in
    if c <> 0 then c
    else
      let c = Stdlib.compare kind1 kind2 in
      if c <> 0 then c
      else Stdlib.compare layout1 layout2
  | (Make_block _
    | Bigarray_set _
    | Bigarray_load _
    ), _ ->
    Stdlib.compare (variadic_primitive_numbering p1)
      (variadic_primitive_numbering p2)

let print_variadic_primitive ppf p =
  let fprintf = Format.fprintf in
  match p with
  | Make_block (kind, mut) ->
    fprintf ppf "@[(Make_block %a %a)@]"
      print_make_block_kind kind
      print_mutable_or_immutable mut
  | Bigarray_set _ -> fprintf ppf "Bigarray_set"
  | Bigarray_load _ -> fprintf ppf "Bigarray_load"

let args_kind_of_variadic_primitive p : arg_kinds =
  match p with
  | Make_block (Full_of_values (_tag, _shape), _) ->
    Variadic_all_of_kind K.value
  | Make_block (Full_of_naked_floats, _) ->
    Variadic_all_of_kind K.naked_float
  | Make_block (Generic_array No_specialisation, _) ->
    Variadic_all_of_kind K.value
  | Make_block (Generic_array Full_of_naked_floats, _) ->
    Variadic_all_of_kind K.naked_float
  | Make_block (Generic_array Full_of_immediates, _) ->
    Variadic_all_of_kind K.value
  | Make_block (Generic_array Full_of_arbitrary_values_but_not_floats, _) ->
    Variadic_all_of_kind K.value
  | Bigarray_set (num_dims, kind, _) ->
    let index = List.init num_dims (fun _ -> array_like_thing_index_kind) in
    let new_value = element_kind_of_bigarray_kind kind in
    Variadic ([bigarray_kind] @ index @ [new_value])
  | Bigarray_load (num_dims, _, _) ->
    let index = List.init num_dims (fun _ -> array_like_thing_index_kind) in
    Variadic ([bigarray_kind] @ index)

let result_kind_of_variadic_primitive p : result_kind =
  match p with
  | Make_block _ -> Singleton K.value
  | Bigarray_set _ -> Unit
  | Bigarray_load (_, kind, _) ->
    Singleton (element_kind_of_bigarray_kind kind)

let effects_and_coeffects_of_variadic_primitive p =
  match p with
  (* CR mshinwell: Arrays of size zero? *)
  | Make_block (_, mut) ->
    Only_generative_effects mut, No_coeffects
  | Bigarray_set (_, _, _) ->
    writing_to_an_array_like_thing
  | Bigarray_load (_, (Unknown | Complex32 | Complex64), _) ->
    Only_generative_effects Immutable, Has_coeffects
  | Bigarray_load (_, _, _) ->
    reading_from_an_array_like_thing

type t =
  | Unary of unary_primitive * Simple.t
  | Binary of binary_primitive * Simple.t * Simple.t
  | Ternary of ternary_primitive * Simple.t * Simple.t * Simple.t
  | Variadic of variadic_primitive * (Simple.t list)

type primitive_application = t

let invariant env t =
  let module E = Invariant_env in
  match t with
  | Unary (prim, x0) ->
    let kind0 = arg_kind_of_unary_primitive prim in
    E.check_simple_is_bound_and_of_kind env x0 kind0;
    begin match prim, x0 with
    | Project_closure closure_id, set_of_closures ->
      E.check_simple_is_bound_and_of_kind env set_of_closures K.fabricated;
      E.add_use_of_closure_id env closure_id
    | Move_within_set_of_closures { move_from; move_to; }, closure ->
      E.check_simple_is_bound_and_of_kind env closure K.value;
      E.add_use_of_closure_id env move_from;
      E.add_use_of_closure_id env move_to
    | Project_var var, closure ->
      E.add_use_of_var_within_closure env var;
      E.check_simple_is_bound_and_of_kind env closure K.value
    | Duplicate_block _, _
    | Is_int, _
    | Get_tag _, _
    | Discriminant_of_int, _
    | Array_length _, _
    | Bigarray_length _, _
    | String_length _, _
    | Int_as_pointer, _
    | Opaque_identity, _
    | Int_arith _, _
    | Float_arith _, _
    | Num_conv _, _
    | Boolean_not, _
    | Unbox_number _, _
    | Box_number _, _ -> ()  (* None of these contain names. *)
    end
  | Binary (prim, x0, x1) ->
    let kind0, kind1 = args_kind_of_binary_primitive prim in
    E.check_simple_is_bound_and_of_kind env x0 kind0;
    E.check_simple_is_bound_and_of_kind env x1 kind1;
    begin match prim with
    (* None of these currently contain names: this is here so that we
       are reminded to check upon adding a new primitive. *)
    | Block_load _
    | String_or_bigstring_load _
    | Phys_equal _
    | Int_arith _
    | Int_shift _
    | Int_comp _
    | Float_arith _
    | Float_comp _ -> ()
    end
  | Ternary (prim, x0, x1, x2) ->
    let kind0, kind1, kind2 = args_kind_of_ternary_primitive prim in
    E.check_simple_is_bound_and_of_kind env x0 kind0;
    E.check_simple_is_bound_and_of_kind env x1 kind1;
    E.check_simple_is_bound_and_of_kind env x2 kind2;
    begin match prim with
    | Block_set _
    | Bytes_or_bigstring_set _ -> ()
    end
  | Variadic (prim, xs) ->
    let kinds =
      match args_kind_of_variadic_primitive prim with
      | Variadic kinds -> kinds
      | Variadic_all_of_kind kind ->
        List.init (List.length xs) (fun _index -> kind)
    in
    List.iter2 (fun var kind ->
        E.check_simple_is_bound_and_of_kind env var kind)
      xs kinds;
    begin match prim with
    | Make_block _
    | Bigarray_set _
    | Bigarray_load _ -> ()
    end

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    if t1 == t2 then 0
    else
      let numbering t =
        match t with
        | Unary _ -> 0
        | Binary _ -> 1
        | Ternary _ -> 2
        | Variadic _ -> 3
      in
      match t1, t2 with
      | Unary (p, s1), Unary (p', s1') ->
        let c = compare_unary_primitive p p' in
        if c <> 0 then c
        else Simple.compare s1 s1'
      | Binary (p, s1, s2), Binary (p', s1', s2') ->
        let c = compare_binary_primitive p p' in
        if c <> 0 then c
        else
          let c = Simple.compare s1 s1' in
          if c <> 0 then c
          else Simple.compare s2 s2'
      | Ternary (p, s1, s2, s3), Ternary (p', s1', s2', s3') ->
        let c = compare_ternary_primitive p p' in
        if c <> 0 then c
        else
          let c = Simple.compare s1 s1' in
          if c <> 0 then c
          else
            let c = Simple.compare s2 s2' in
            if c <> 0 then c
            else Simple.compare s3 s3'
      | Variadic (p, s), Variadic (p', s') ->
        let c = compare_variadic_primitive p p' in
        if c <> 0 then c
        else Simple.List.compare s s'
      | (Unary _ | Binary _ | Ternary _ | Variadic _), _ ->
        Stdlib.compare (numbering t1) (numbering t2)

  let equal t1 t2 = (compare t1 t2 = 0)

  let hash _t = Misc.fatal_error "Not implemented"

  let print ppf t =
    match t with
    | Unary (prim, v0) ->
      Format.fprintf ppf "@[<hov 1>(%s%a%s@ %a)@]"
        (Misc.Color.bold_green ())
        print_unary_primitive prim
        (Misc.Color.reset ())
        Simple.print v0
    | Binary (prim, v0, v1) ->
      Format.fprintf ppf "@[<hov 1>(%s%a%s@ %a@ %a)@]"
        (Misc.Color.bold_green ())
        print_binary_primitive prim
        (Misc.Color.reset ())
        Simple.print v0
        Simple.print v1
    | Ternary (prim, v0, v1, v2) ->
      Format.fprintf ppf "@[<hov 1>(%s%a%s@ %a@ %a@ %a)@]"
        (Misc.Color.bold_green ())
        print_ternary_primitive prim
        (Misc.Color.reset ())
        Simple.print v0
        Simple.print v1
        Simple.print v2
    | Variadic (prim, vs) ->
      Format.fprintf ppf "@[<hov 1>(%s%a%s@ %a)@]"
        (Misc.Color.bold_green ())
        print_variadic_primitive prim
        (Misc.Color.reset ())
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Simple.print) vs

  let output chan t =
    print (Format.formatter_of_out_channel chan) t
end)

let equal t1 t2 =
  compare t1 t2 = 0

let free_names t =
  match t with
  | Unary (_prim, x0) -> Simple.free_names x0
  | Binary (_prim, x0, x1) ->
    Name_occurrences.union_list [
      Simple.free_names x0;
      Simple.free_names x1;
    ]
  | Ternary (_prim, x0, x1, x2) ->
    Name_occurrences.union_list [
      Simple.free_names x0;
      Simple.free_names x1;
      Simple.free_names x2;
    ]
  | Variadic (_prim, xs) -> Simple.List.free_names xs

let apply_name_permutation t perm =
  (* CR mshinwell: add phys-equal checks *)
  let apply simple = Simple.apply_name_permutation simple perm in
  match t with
  | Unary (prim, x0) -> Unary (prim, apply x0)
  | Binary (prim, x0, x1) -> Binary (prim, apply x0, apply x1)
  | Ternary (prim, x0, x1, x2) -> Ternary (prim, apply x0, apply x1, apply x2)
  | Variadic (prim, xs) ->
    Variadic (prim, Simple.List.apply_name_permutation xs perm)

let result_kind (t : t) =
  match t with
  | Unary (prim, _) -> result_kind_of_unary_primitive prim
  | Binary (prim, _, _) -> result_kind_of_binary_primitive prim
  | Ternary (prim, _, _, _) -> result_kind_of_ternary_primitive prim
  | Variadic (prim, _) -> result_kind_of_variadic_primitive prim

let result_kind' t =
  match result_kind t with
  | Singleton kind -> kind
  | Unit -> K.value

let effects_and_coeffects (t : t) =
  match t with
  | Unary (prim, _) -> effects_and_coeffects_of_unary_primitive prim
  | Binary (prim, _, _) -> effects_and_coeffects_of_binary_primitive prim
  | Ternary (prim, _, _, _) -> effects_and_coeffects_of_ternary_primitive prim
  | Variadic (prim, _) -> effects_and_coeffects_of_variadic_primitive prim

let no_effects_or_coeffects t =
  match effects_and_coeffects t with
  | No_effects, No_coeffects -> true
  | _, _ -> false

let at_most_generative_effects t =
  match effects_and_coeffects t with
  | (No_effects | Only_generative_effects _), _ -> true
  | _, _ -> false

module With_fixed_value = struct
  type t = primitive_application

  let create t =
    match effects_and_coeffects t with
    | No_effects, No_coeffects -> Some t
    | Only_generative_effects Immutable, No_coeffects ->
      (* Allow constructions of immutable blocks to be shared. *)
      Some t
    | _, _ -> None

  let create_is_int ~immediate_or_block =
    Unary (Is_int, Simple.name immediate_or_block)

  let create_get_tag ~block ~tags_to_sizes =
    Unary (Get_tag { tags_to_sizes }, Simple.name block)

  let eligible t =
    match create t with
    | None -> false
    | Some _ -> true

  let to_primitive t = t

  let free_names = free_names
  let apply_name_permutation = apply_name_permutation

  include Identifiable.Make (struct
    type nonrec t = t

    let compare = compare
    let equal = equal
    let hash = hash
    let print = print
    let output = output
  end)

  let equal t1 t2 =
    compare t1 t2 = 0
end

type without_args =
  | Unary of unary_primitive
  | Binary of binary_primitive
  | Ternary of ternary_primitive
  | Variadic of variadic_primitive
