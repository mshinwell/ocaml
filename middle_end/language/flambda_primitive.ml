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

type mutable_or_immutable = Immutable | Mutable

type effects =
  | No_effects
  | Only_generative_effects of mutable_or_immutable
  | Arbitrary_effects

type coeffects = No_coeffects | Has_coeffects

(* CR mshinwell: These types should probably go in their own modules with
   a comparison function next to each. *)
type array_kind =
  | Dynamic_must_scan_or_naked_float
  | Must_scan
  | Can_scan
  | Naked_float

let print_array_kind ppf k =
  let fprintf = Format.fprintf in
  match k with
  | Dynamic_must_scan_or_naked_float -> fprintf ppf "dynamic"
  | Must_scan -> fprintf ppf "must_scan"
  | Can_scan -> fprintf ppf "can_scan"
  | Naked_float -> fprintf ppf "float"

type field_kind = Not_a_float | Float

type string_or_bytes = String | Bytes

type init_or_assign = Initialization | Assignment

let print_init_or_assign ppf init_or_assign =
  let fprintf = Format.fprintf in
  match init_or_assign with
  | Initialization -> fprintf ppf "init"
  | Assignment -> ()

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
  effects, Has_coeffects

let array_like_thing_index_kind = K.value Can_scan

let array_kind = K.value Must_scan
let bigarray_kind = K.value Must_scan
let bigstring_kind = K.value Must_scan
let bigstring_element_kind = K.naked_immediate ()
let block_kind = K.value Must_scan
let block_element_kind = K.value Must_scan
let string_or_bytes_kind = K.value Must_scan
let string_or_bytes_element_kind = K.naked_immediate ()

type comparison = Eq | Neq | Lt | Gt | Le | Ge

let print_comparison ppf c =
  let fprintf = Format.fprintf in
  match c with
  | Eq  -> fprintf ppf "=="
  | Neq -> fprintf ppf "!="
  | Lt  -> fprintf ppf "<"
  | Le  -> fprintf ppf "<="
  | Gt  -> fprintf ppf ">"
  | Ge  -> fprintf ppf ">="

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
  | Unknown -> K.value Must_scan
  | Float32
  | Float64 -> K.naked_float ()
  | Sint8
  | Uint8
  | Sint16
  | Uint16 -> K.naked_immediate ()
  | Int32 -> K.naked_int32 ()
  | Int64 -> K.naked_int64 ()
  | Int_width_int -> K.naked_immediate ()
  | Targetint_width_int -> K.naked_nativeint ()
  | Complex32
  | Complex64 ->
    (* See [copy_two_doubles] in bigarray_stubs.c. *)
    K.value Must_scan

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

type block_set_kind =
  | Immediate
  | Pointer
  | Float

let print_block_set_kind ppf k =
  let fprintf = Format.fprintf in
  match k with
  | Immediate -> fprintf ppf "imm"
  | Pointer -> fprintf ppf "ptr"
  | Float -> fprintf ppf "float"

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

type bigstring_accessor_width =
  | Sixteen
  | Thirty_two
  | Sixty_four

let print_bigstring_accessor_width ppf w =
  let fprintf = Format.fprintf in
  match w with
  | Sixteen -> fprintf ppf "16"
  | Thirty_two -> fprintf ppf "32"
  | Sixty_four -> fprintf ppf "64"

type num_dimensions = int

let print_num_dimesions ppf d =
  Format.fprintf ppf "%d" d

type record_representation =
  | Regular
  | Float
  | Unboxed of { inlined : bool; }
  | Inlined of Tag.Scannable.t
  | Extension

let print_record_representation ppf repr =
  let fprintf = Format.fprintf in
  match repr with
  | Regular -> fprintf ppf "regular"
  | Inlined tag -> fprintf ppf "inlined(%a)" Tag.Scannable.print tag
  | Unboxed { inlined = false; } -> fprintf ppf "unboxed"
  | Unboxed { inlined = true; } -> fprintf ppf "inlined(unboxed)"
  | Float -> fprintf ppf "float"
  | Extension -> fprintf ppf "ext"

type unary_int_arith_op = Neg

let print_unary_int_arith_op ppf o =
  let fprintf = Format.fprintf in
  match o with
  | Neg -> fprintf ppf "~"

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
  | Never_returns

type unary_primitive =
  | Block_load of int * field_kind * mutable_or_immutable
  | Duplicate_array of array_kind * mutable_or_immutable
  | Duplicate_record of {
      repr : record_representation;
      num_fields : int;
    }
  | Is_int
  | Get_tag
  | String_length of string_or_bytes
  | Swap_byte_endianness of K.Standard_int.t
  | Int_as_pointer
  | Opaque_identity
  | Int_arith of K.Standard_int.t * unary_int_arith_op
  | Int_conv of {
      src : Flambda_kind.Standard_int.t;
      dst : Flambda_kind.Standard_int.t;
    }
  | Float_arith of unary_float_arith_op
  | Int_of_float
  | Float_of_int
  | Array_length of array_kind
  | Bigarray_length of { dimension : int; }
  | Unbox_number of K.Boxable_number.t
  | Box_number of K.Boxable_number.t
  | Project_closure of Closure_id.Set.t
  | Move_within_set_of_closures of Closure_id.t Closure_id.Map.t
  | Project_var of Var_within_closure.t Closure_id.Map.t
  | Boolean_not

let compare_unary_primitive p1 p2 =
  let unary_primitive_numbering p =
    match p with
    | Block_load _ -> 0
    | Duplicate_array _ -> 1
    | Duplicate_record _ -> 2
    | Is_int -> 3
    | Get_tag -> 4
    | String_length _ -> 5
    | Swap_byte_endianness _ -> 6
    | Int_as_pointer -> 7
    | Opaque_identity -> 8
    | Int_arith _ -> 9
    | Int_conv _ -> 10
    | Float_arith _ -> 11
    | Int_of_float -> 12
    | Float_of_int -> 13
    | Array_length _ -> 14
    | Bigarray_length _ -> 15
    | Unbox_number _ -> 16
    | Box_number _ -> 17
    | Project_closure _ -> 18
    | Move_within_set_of_closures _ -> 19
    | Project_var _ -> 20
    | Boolean_not -> 21
  in
  match p1, p2 with
  | Block_load (size1, kind1, mut1), Block_load (size2, kind2, mut2) ->
    let c = Pervasives.compare size1 size2 in
    if c <> 0 then c
    else
      let c = Pervasives.compare kind1 kind2 in
      if c <> 0 then c
      else Pervasives.compare mut1 mut2
  | Duplicate_array (kind1, mut1), Duplicate_array (kind2, mut2) ->
    let c = Pervasives.compare kind1 kind2 in
    if c <> 0 then c
    else Pervasives.compare mut1 mut2
  | Duplicate_record { repr = repr1; num_fields = num_fields1; },
      Duplicate_record { repr = repr2; num_fields = num_fields2; } ->
    let c = Pervasives.compare repr1 repr2 in
    if c <> 0 then c
    else Pervasives.compare num_fields1 num_fields2
  | String_length kind1, String_length kind2 ->
    Pervasives.compare kind1 kind2
  | Swap_byte_endianness kind1, Swap_byte_endianness kind2 ->
    K.Standard_int.compare kind1 kind2
  | Int_arith (kind1, op1), Int_arith (kind2, op2) ->
    let c = K.Standard_int.compare kind1 kind2 in
    if c <> 0 then c
    else Pervasives.compare op1 op2
  | Int_conv { src = src1; dst = dst1; },
      Int_conv { src = src2; dst = dst2; } ->
    let c = K.Standard_int.compare src1 src2 in
    if c <> 0 then c
    else K.Standard_int.compare dst1 dst2
  | Float_arith op1, Float_arith op2 ->
    Pervasives.compare op1 op2
  | Array_length kind1, Array_length kind2 ->
    Pervasives.compare kind1 kind2
  | Bigarray_length { dimension = dim1; },
      Bigarray_length { dimension = dim2; } ->
    Pervasives.compare dim1 dim2
  | Unbox_number kind1, Unbox_number kind2 ->
    K.Boxable_number.compare kind1 kind2
  | Box_number kind1, Box_number kind2 ->
    K.Boxable_number.compare kind1 kind2
  | Project_closure set1, Project_closure set2 ->
    Closure_id.Set.compare set1 set2
  | Move_within_set_of_closures map1, Move_within_set_of_closures map2 ->
    Closure_id.Map.compare Closure_id.compare map1 map2
  | Project_var map1, Project_var map2 ->
    Closure_id.Map.compare Var_within_closure.compare map1 map2
  | (Block_load _
    | Duplicate_array _
    | Duplicate_record _
    | Is_int
    | Get_tag
    | String_length _
    | Swap_byte_endianness _
    | Int_as_pointer
    | Opaque_identity
    | Int_arith _
    | Int_conv _
    | Float_arith _
    | Int_of_float
    | Float_of_int
    | Array_length _
    | Bigarray_length _
    | Unbox_number _
    | Box_number _
    | Project_closure _
    | Move_within_set_of_closures _
    | Project_var _
    | Boolean_not), _ ->
    Pervasives.compare (unary_primitive_numbering p1)
      (unary_primitive_numbering p2)

let print_unary_primitive ppf p =
  let fprintf = Format.fprintf in
  match p with
  | Block_load (n, _k, _mut) ->
    fprintf ppf "block_load %i" n
  | Duplicate_array (k, Mutable) ->
    fprintf ppf "duplicate_array[%a]" print_array_kind k
  | Duplicate_array (k, Immutable) ->
    fprintf ppf "duplicate_array_imm[%a]" print_array_kind k
  | Duplicate_record { repr; num_fields; } ->
    fprintf ppf "duplicate_record %a %i"
      print_record_representation repr
      num_fields
  | Is_int -> fprintf ppf "is_int"
  | Get_tag -> fprintf ppf "get_tag"
  | String_length _ -> fprintf ppf "string_length"
  | Swap_byte_endianness _ -> fprintf ppf "swap_byte_endianness"
  | Int_as_pointer -> fprintf ppf "int_as_pointer"
  | Opaque_identity -> fprintf ppf "opaque_identity"
  | Int_arith (_k, o) -> print_unary_int_arith_op ppf o
  | Int_conv { src; dst; } ->
    fprintf ppf "conv_%a_to_%a"
      Flambda_kind.Standard_int.print src
      Flambda_kind.Standard_int.print dst
  | Float_arith o -> print_unary_float_arith_op ppf o
  | Int_of_float -> fprintf ppf "int_of_float"
  | Float_of_int -> fprintf ppf "float_of_int"
  | Array_length _ -> fprintf ppf "array_length"
  | Bigarray_length { dimension; } ->
    fprintf ppf "bigarray_length %a" print_num_dimesions dimension
  | Unbox_number k ->
    fprintf ppf "unbox_%a" K.Boxable_number.print_lowercase k
  | Box_number k ->
    fprintf ppf "box_%a" K.Boxable_number.print_lowercase k
  | Project_closure set ->
    Format.fprintf ppf "(project_closure@ %a)"
      Closure_id.Set.print set
  | Move_within_set_of_closures moves ->
    Format.fprintf ppf "(move_within_set_of_closures@ %a)"
      (Closure_id.Map.print Closure_id.print) moves
  | Project_var by_closure ->
    Format.fprintf ppf "(project_var@ %a)"
      (Closure_id.Map.print Var_within_closure.print) by_closure
  | Boolean_not -> fprintf ppf "not"

let arg_kind_of_unary_primitive p =
  match p with
  | Block_load (_, Not_a_float, _) -> K.value Must_scan
  | Block_load (_, Float, _) -> K.naked_float ()
  | Duplicate_array _
  | Duplicate_record _
  | Is_int
  | Get_tag
  | String_length _ -> K.value Must_scan
  | Swap_byte_endianness kind -> K.Standard_int.to_kind kind
  | Int_as_pointer -> K.value Can_scan
  | Opaque_identity -> K.value Must_scan
  | Int_arith (kind, _) -> K.Standard_int.to_kind kind
  | Int_conv { src; dst = _; } -> K.Standard_int.to_kind src
  | Float_arith _ -> K.naked_float ()
  | Int_of_float -> K.value Can_scan
  | Float_of_int -> K.naked_float ()
  | Array_length _
  | Bigarray_length _ -> K.value Must_scan
  | Unbox_number _ -> K.value Must_scan
  | Box_number kind -> K.Boxable_number.to_kind kind
  | Project_closure _
  | Move_within_set_of_closures _
  | Project_var _ -> K.value Must_scan
  | Boolean_not -> K.value Can_scan

let result_kind_of_unary_primitive p : result_kind =
  match p with
  | Block_load (_, Not_a_float, _) -> Singleton (K.value Must_scan)
  | Block_load (_, Float, _) -> Singleton (K.naked_float ())
  | Duplicate_array _
  | Duplicate_record _ -> Singleton (K.value Must_scan)
  | Is_int
  | Get_tag
  | String_length _ -> Singleton (K.value Can_scan)
  | Swap_byte_endianness kind -> Singleton (K.Standard_int.to_kind kind)
  | Int_as_pointer -> Singleton (K.naked_immediate ())
  | Opaque_identity -> Singleton (K.value Must_scan)
  | Int_arith (kind, _) -> Singleton (K.Standard_int.to_kind kind)
  | Int_conv { src = _; dst; } -> Singleton (K.Standard_int.to_kind dst)
  | Float_arith _ -> Singleton (K.naked_float ())
  | Int_of_float -> Singleton (K.value Can_scan)
  | Float_of_int -> Singleton (K.naked_float ())
  | Array_length _
  | Bigarray_length _ -> Singleton (K.value Can_scan)
  | Unbox_number kind -> Singleton (K.Boxable_number.to_kind kind)
  | Box_number _ -> Singleton (K.value Must_scan)
  | Project_closure _
  | Move_within_set_of_closures _
  | Project_var _ -> Singleton (K.value Must_scan)
  | Boolean_not -> Singleton (K.value Can_scan)

let effects_and_coeffects_of_unary_primitive p =
  match p with
  | Block_load (_, _, Immutable) -> No_effects, No_coeffects
  | Block_load (_, _, Mutable) ->
    reading_from_an_array_like_thing
  | Duplicate_array (_, Immutable) ->
    (* Duplicate_array (_, Immutable) is allowed only on immutable arrays. *)
    Only_generative_effects Immutable, No_coeffects
  | Duplicate_array (_, Mutable)
  | Duplicate_record { repr = _; num_fields = _; } ->
    Only_generative_effects Mutable, Has_coeffects
  | Is_int -> No_effects, No_coeffects
  | Get_tag -> No_effects, Has_coeffects
  | String_length _ -> reading_from_an_array_like_thing
  | Int_as_pointer
  | Opaque_identity -> Arbitrary_effects, Has_coeffects
  | Swap_byte_endianness _
  | Int_arith (_, Neg)
  | Int_conv _
  | Float_arith (Abs | Neg)
  | Int_of_float
  | Float_of_int -> No_effects, No_coeffects
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
  | Boolean_not -> No_effects, No_coeffects

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
  | Block_load_computed_index
  | Block_set of int * block_set_kind * init_or_assign
  | Int_arith of K.Standard_int.t * binary_int_arith_op
  | Int_shift of K.Standard_int.t * int_shift_op
  | Int_comp of K.Standard_int.t * comparison
  | Int_comp_unsigned of comparison
  | Float_arith of binary_float_arith_op
  | Float_comp of comparison
  | Bit_test
  | Array_load of array_kind
  | String_load of string_accessor_width
  | Bigstring_load of bigstring_accessor_width

let compare_binary_primitive p1 p2 =
  let binary_primitive_numbering p =
    match p with
    | Block_load_computed_index -> 0
    | Block_set _ -> 1
    | Int_arith _ -> 2
    | Int_shift _ -> 3
    | Int_comp _ -> 4
    | Int_comp_unsigned _ -> 5
    | Float_arith _ -> 6
    | Float_comp _ -> 7
    | Bit_test -> 8
    | Array_load _ -> 9
    | String_load _ -> 10
    | Bigstring_load _ -> 11
  in
  match p1, p2 with
  | Block_set (field1, kind1, init_or_assign1),
      Block_set (field2, kind2, init_or_assign2) ->
    let c = Pervasives.compare field1 field2 in
    if c <> 0 then c
    else
      let c = Pervasives.compare kind1 kind2 in
      if c <> 0 then c
      else Pervasives.compare init_or_assign1 init_or_assign2
  | Int_arith (kind1, op1), Int_arith (kind2, op2) ->
    let c = K.Standard_int.compare kind1 kind2 in
    if c <> 0 then c
    else Pervasives.compare op1 op2
  | Int_shift (kind1, op1), Int_shift (kind2, op2) ->
    let c = K.Standard_int.compare kind1 kind2 in
    if c <> 0 then c
    else Pervasives.compare op1 op2
  | Int_comp (kind1, comp1), Int_comp (kind2, comp2) ->
    let c = K.Standard_int.compare kind1 kind2 in
    if c <> 0 then c
    else Pervasives.compare comp1 comp2
  | Int_comp_unsigned comp1, Int_comp_unsigned comp2 ->
    Pervasives.compare comp1 comp2
  | Float_arith op1, Float_arith op2 ->
    Pervasives.compare op1 op2
  | Float_comp comp1, Float_comp comp2 ->
    Pervasives.compare comp1 comp2
  | Array_load kind1, Array_load kind2 ->
    Pervasives.compare kind1 kind2
  | String_load width1, String_load width2 ->
    Pervasives.compare width1 width2
  | Bigstring_load width1, Bigstring_load width2 ->
    Pervasives.compare width1 width2
  | (Block_load_computed_index
    | Block_set _
    | Int_arith _
    | Int_shift _
    | Int_comp _
    | Int_comp_unsigned _
    | Float_arith _
    | Float_comp _
    | Bit_test
    | Array_load _
    | String_load _
    | Bigstring_load _), _ ->
    Pervasives.compare (binary_primitive_numbering p1)
      (binary_primitive_numbering p2)

let print_binary_primitive ppf p =
  let fprintf = Format.fprintf in
  match p with
  | Block_load_computed_index ->
    fprintf ppf "block_load_computed"
  | Block_set (n, k, init) ->
    fprintf ppf "set_field_%a%a %i"
      print_block_set_kind k
      print_init_or_assign init
      n
  | Int_arith (_k, op) -> print_binary_int_arith_op ppf op
  | Int_shift (_k, op) -> print_int_shift_op ppf op
  | Int_comp (_, c) -> print_comparison ppf c
  | Int_comp_unsigned c -> fprintf ppf "%a[u]" print_comparison c
  | Float_arith op -> print_binary_float_arith_op ppf op
  | Float_comp c -> print_comparison ppf c; fprintf ppf "."
  | Bit_test -> fprintf ppf "bit_test"
  | Array_load array_kind ->
    fprintf ppf "array_load[%a]"
      print_array_kind array_kind
  | String_load string_accessor_width ->
    fprintf ppf "string_load_%a"
      print_string_accessor_width string_accessor_width
  | Bigstring_load bigstring_accessor_width ->
    fprintf ppf "bigstring_load_%a"
      print_bigstring_accessor_width bigstring_accessor_width

let args_kind_of_binary_primitive p =
  match p with
  | Block_load_computed_index ->
    block_kind, array_like_thing_index_kind
  | Block_set _ ->
    block_kind, block_element_kind
  | Int_arith (kind, _) ->
    let kind = K.Standard_int.to_kind kind in
    kind, kind
  | Int_shift (kind, _) ->
    K.Standard_int.to_kind kind, K.naked_immediate ()
  | Int_comp (kind, _) ->
    let kind = K.Standard_int.to_kind kind in
    kind, kind
  | Int_comp_unsigned _ -> K.naked_immediate (), K.naked_immediate ()
  | Float_arith _
  | Float_comp _ -> K.naked_float (), K.naked_float ()
  | Bit_test -> string_or_bytes_kind, array_like_thing_index_kind
  | Array_load _ -> array_kind, array_like_thing_index_kind
  | String_load _ -> string_or_bytes_kind, array_like_thing_index_kind
  | Bigstring_load _ -> bigstring_kind, array_like_thing_index_kind

let result_kind_of_binary_primitive p : result_kind =
  match p with
  | Block_load_computed_index -> Singleton (block_element_kind)
  | Block_set _ -> Unit
  | Int_arith (kind, _)
  | Int_shift (kind, _) -> Singleton (K.Standard_int.to_kind kind)
  | Float_arith _ -> Singleton (K.naked_float ())
  | Int_comp _
  | Float_comp _
  | Int_comp_unsigned _ -> Singleton (K.value Can_scan)
  | Bit_test -> Singleton (K.value Can_scan)
  | Array_load (Dynamic_must_scan_or_naked_float | Must_scan) ->
    Singleton (K.value Must_scan)
  | Array_load Can_scan -> Singleton (K.value Can_scan)
  | Array_load Naked_float -> Singleton (K.naked_float ())
  | String_load _ -> Singleton (string_or_bytes_element_kind)
  | Bigstring_load _ -> Singleton (bigstring_element_kind)

let effects_and_coeffects_of_binary_primitive p =
  match p with
  | Block_load_computed_index ->
    reading_from_an_array_like_thing
  | Block_set _ ->
    writing_to_an_array_like_thing
  | Int_arith (_kind,
      (Add | Sub | Mul | Div | Mod | And | Or | Xor)) ->
    No_effects, No_coeffects
  | Int_shift _ -> No_effects, No_coeffects
  | Int_comp _ | Int_comp_unsigned _ -> No_effects, No_coeffects
  | Float_arith (Add | Sub | Mul | Div) -> No_effects, No_coeffects
  | Float_comp _ -> No_effects, No_coeffects
  | Bit_test -> No_effects, No_coeffects
  | Array_load _
  | String_load _
  | Bigstring_load _ -> reading_from_an_array_like_thing

type ternary_primitive =
  | Block_set_computed of Flambda_kind.scanning * init_or_assign
  | Bytes_set of string_accessor_width
  | Array_set of array_kind
  | Bigstring_set of bigstring_accessor_width

let compare_ternary_primitive p1 p2 =
  let ternary_primitive_numbering p =
    match p with
    | Block_set_computed _ -> 0
    | Bytes_set _ -> 1
    | Array_set _ -> 2
    | Bigstring_set _ -> 3
  in
  match p1, p2 with
  | Block_set_computed (scan1, init_or_assign1),
      Block_set_computed (scan2, init_or_assign2) ->
    let c = Pervasives.compare scan1 scan2 in
    if c <> 0 then c
    else Pervasives.compare init_or_assign1 init_or_assign2
  | Bytes_set width1, Bytes_set width2 ->
    Pervasives.compare width1 width2
  | Array_set kind1, Array_set kind2 ->
    Pervasives.compare kind1 kind2
  | Bigstring_set width1, Bigstring_set width2 ->
    Pervasives.compare width1 width2
  | (Block_set_computed _
    | Bytes_set _
    | Array_set _
    | Bigstring_set _), _ ->
    Pervasives.compare (ternary_primitive_numbering p1)
      (ternary_primitive_numbering p2)

let print_ternary_primitive ppf p =
  let fprintf = Format.fprintf in
  match p with
  | Block_set_computed (_, init) ->
    fprintf ppf "block_set_computed%a" print_init_or_assign init
  | Bytes_set string_accessor_width ->
    fprintf ppf "bytes_set_%a"
      print_string_accessor_width string_accessor_width
  | Array_set array_kind ->
    fprintf ppf "array_set_%a"
      print_array_kind array_kind
  | Bigstring_set bigstring_accessor_width ->
    fprintf ppf "bigstring_set_%a"
      print_bigstring_accessor_width bigstring_accessor_width

let args_kind_of_ternary_primitive p =
  match p with
  | Block_set_computed _ ->
    block_kind, array_like_thing_index_kind, block_element_kind
  | Bytes_set _ ->
    string_or_bytes_kind, array_like_thing_index_kind, string_or_bytes_element_kind
  | Array_set _ ->
    array_kind, array_like_thing_index_kind, K.value Must_scan
  | Bigstring_set _ ->
    bigstring_kind, array_like_thing_index_kind, bigstring_element_kind

let result_kind_of_ternary_primitive p : result_kind =
  match p with
  | Block_set_computed _
  | Bytes_set _
  | Array_set _
  | Bigstring_set _ -> Unit

let effects_and_coeffects_of_ternary_primitive p =
  match p with
  | Block_set_computed _ -> writing_to_an_array_like_thing
  | Bytes_set _
  | Array_set _
  | Bigstring_set _ -> writing_to_an_array_like_thing

type variadic_primitive =
  | Make_block of Tag.Scannable.t * mutable_or_immutable * Flambda_arity.t
  | Make_array of array_kind * mutable_or_immutable
  | Bigarray_set of num_dimensions * bigarray_kind * bigarray_layout
  | Bigarray_load of num_dimensions * bigarray_kind * bigarray_layout

let compare_variadic_primitive p1 p2 =
  let variadic_primitive_numbering p =
    match p with
    | Make_block _ -> 0
    | Make_array _ -> 1
    | Bigarray_set _ -> 2
    | Bigarray_load _ -> 3
  in
  match p1, p2 with
  | Make_block (tag1, mut1, arity1), Make_block (tag2, mut2, arity2) ->
    let c = Tag.Scannable.compare tag1 tag2 in
    if c <> 0 then c
    else
      let c = Pervasives.compare mut1 mut2 in
      if c <> 0 then c
      else Flambda_arity.compare arity1 arity2
  | Make_array (kind1, mut1), Make_array (kind2, mut2) ->
    let c = Pervasives.compare kind1 kind2 in
    if c <> 0 then c
    else Pervasives.compare mut1 mut2
  | Bigarray_set (num_dims1, kind1, layout1),
      Bigarray_set (num_dims2, kind2, layout2) ->
    let c = Pervasives.compare num_dims1 num_dims2 in
    if c <> 0 then c
    else
      let c = Pervasives.compare kind1 kind2 in
      if c <> 0 then c
      else Pervasives.compare layout1 layout2
  | (Make_block _
    | Make_array _
    | Bigarray_set _
    | Bigarray_load _
    ), _ ->
    Pervasives.compare (variadic_primitive_numbering p1)
      (variadic_primitive_numbering p2)

let print_variadic_primitive ppf p =
  let fprintf = Format.fprintf in
  match p with
  | Make_block (tag, Immutable, arity) ->
    fprintf ppf "makeblock %a%a"
      Tag.Scannable.print tag
      Flambda_arity.print arity
  | Make_block (tag, Mutable, arity) ->
    fprintf ppf "makemutable %a%a"
      Tag.Scannable.print tag
      Flambda_arity.print arity
  | Make_array (k, Mutable) ->
    fprintf ppf "makearray[%a]" print_array_kind k
  | Make_array (k, Immutable) ->
    fprintf ppf "makearray_imm[%a]" print_array_kind k
  | Bigarray_set _ -> fprintf ppf "bigarray_set"
  | Bigarray_load _ -> fprintf ppf "bigarray_load"

let args_kind_of_variadic_primitive p : arg_kinds =
  match p with
  | Make_block (_tag, _mut, arity) -> Variadic arity
  | Make_array ((Dynamic_must_scan_or_naked_float | Must_scan), _) ->
    Variadic_all_of_kind (K.value Must_scan)
  | Make_array (Can_scan, _) ->
    Variadic_all_of_kind (K.value Can_scan)
  | Make_array (Naked_float, _) ->
    Variadic_all_of_kind (K.naked_float ())
  | Bigarray_set (num_dims, kind, _) ->
    let index = List.init num_dims (fun _ -> array_like_thing_index_kind) in
    let new_value = element_kind_of_bigarray_kind kind in
    Variadic ([bigarray_kind] @ index @ [new_value])
  | Bigarray_load (num_dims, _, _) ->
    let index = List.init num_dims (fun _ -> array_like_thing_index_kind) in
    Variadic ([bigarray_kind] @ index)

let result_kind_of_variadic_primitive p : result_kind =
  match p with
  | Make_block _ -> Singleton block_kind
  | Make_array _ -> Singleton array_kind
  | Bigarray_set _ -> Unit
  | Bigarray_load (_, kind, _) ->
    Singleton (element_kind_of_bigarray_kind kind)

let effects_and_coeffects_of_variadic_primitive p =
  match p with
  | Make_block _
  (* CR mshinwell: Arrays of size zero? *)
  | Make_array (_, Immutable) -> Only_generative_effects Immutable, No_coeffects
  | Make_array (_, Mutable) -> Only_generative_effects Mutable, No_coeffects
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

include Identifiable.Make_no_hash (struct
  type nonrec t = t

  let compare t1 t2 =
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
      Pervasives.compare (numbering t1) (numbering t2)

  let equal t1 t2 =
    compare t1 t2 = 0

  let print ppf t =
    match t with
    | Unary (prim, v0) ->
      Format.fprintf ppf "@[(Prim %a %a)@]"
        print_unary_primitive prim
        Simple.print v0
    | Binary (prim, v0, v1) ->
      Format.fprintf ppf "@[(Prim %a %a %a)@]"
        print_binary_primitive prim
        Simple.print v0
        Simple.print v1
    | Ternary (prim, v0, v1, v2) ->
      Format.fprintf ppf "@[(Prim %a %a %a %a)@]"
        print_ternary_primitive prim
        Simple.print v0
        Simple.print v1
        Simple.print v2
    | Variadic (prim, vs) ->
      Format.fprintf ppf "@[(Prim %a %a)@]"
        print_variadic_primitive prim
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Simple.print) vs
end)

let free_names t =
  match t with
  | Unary (_prim, x0) -> Simple.free_names x0
  | Binary (_prim, x0, x1) ->
    Name.Set.union (Simple.free_names x0) (Simple.free_names x1)
  | Ternary (_prim, x0, x1, x2) ->
    Name.Set.union (Simple.free_names x0)
      (Name.Set.union (Simple.free_names x1) (Simple.free_names x2))
  | Variadic (_prim, xs) -> Simple.List.free_names xs

let rename_variables t ~f =
  match t with
  | Unary (prim, x0) -> Unary (prim, Simple.map_var x0 ~f)
  | Binary (prim, x0, x1) ->
    Binary (prim, Simple.map_var x0 ~f, Simple.map_var x1 ~f)
  | Ternary (prim, x0, x1, x2) ->
    Ternary (prim, Simple.map_var x0 ~f, Simple.map_var x1 ~f,
      Simple.map_var x2 ~f)
  | Variadic (prim, xs) ->
    Variadic (prim, List.map (fun x -> Simple.map_var x ~f) xs)

(* Probably not required
let arg_kinds (t : t) : arg_kinds =
  match t with
  | Unary (prim, _) ->
    let kind = arg_kind_of_unary_primitive prim in
    Unary kind
  | Binary (prim, _, _) ->
    let kind0, kind1 = args_kind_of_binary_primitive prim in
    Binary (kind0, kind1)
  | Ternary (prim, _, _, _) ->
    let kind0, kind1, kind2 = args_kind_of_ternary_primitive prim in
    Ternary (kind0, kind1, kind2)
  | Variadic (prim, _) ->
    args_kind_of_variadic_primitive prim
*)

let result_kind (t : t) =
  match t with
  | Unary (prim, _) -> result_kind_of_unary_primitive prim
  | Binary (prim, _, _) -> result_kind_of_binary_primitive prim
  | Ternary (prim, _, _, _) -> result_kind_of_ternary_primitive prim
  | Variadic (prim, _) -> result_kind_of_variadic_primitive prim

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

let maybe_generative_effects_but_no_coeffects t =
  match effects_and_coeffects t with
  | (No_effects | Only_generative_effects _), No_coeffects -> true
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

  let to_primitive t = t

  let free_names = free_names

  include Identifiable.Make_no_hash (struct
    type nonrec t = t

    let compare = compare
    let equal = equal
    let print = print
  end)
end
