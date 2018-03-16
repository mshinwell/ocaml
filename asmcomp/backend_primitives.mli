
type compile_time_constant =
  | Big_endian
  | Word_size
  | Int_size
  | Max_wosize
  | Ostype_unix
  | Ostype_win32
  | Ostype_cygwin
  | Backend_type
(* TO REMOVE *)

type loc_kind =
  | Loc_FILE
  | Loc_LINE
  | Loc_MODULE
  | Loc_LOC
  | Loc_POS
(* TO REMOVE *)

type mutable_flag = Immutable | Mutable

type immediate_or_pointer =
  | Immediate
  | Pointer

type initialization_or_assignment =
  | Assignment
  (* Initialization of in heap values, like [caml_initialize] C primitive.  The
     field should not have been read before and initialization should happen
     only once. *)
  | Heap_initialization
  (* Initialization of roots only. Compiles to a simple store.
     No checks are done to preserve GC invariants.  *)
  | Root_initialization

type is_safe =
  | Safe
  | Unsafe

type boxed =
  | Boxed
  | Unboxed

type t =
  | Pidentity
  | Pbytes_to_string
  | Pbytes_of_string
  | Pignore
  | Prevapply
  | Pdirapply
  | Ploc of loc_kind
    (* Globals *)
  | Pgetglobal of Ident.t
  | Psetglobal of Ident.t
  (* Operations on heap blocks *)
  | Pmakeblock of int * mutable_flag * block_shape
  | Pfield of int
  | Pfield_computed
  | Psetfield of int * immediate_or_pointer * initialization_or_assignment
  | Psetfield_computed of immediate_or_pointer * initialization_or_assignment
  | Pfloatfield of int
  | Psetfloatfield of int * initialization_or_assignment
  | Pduprecord of Types.record_representation * int
  (* Force lazy values *)
  | Plazyforce
  (* External call *)
  | Pccall of Primitive.description
  | Pccall_unboxed of Primitive.description
  (* Exceptions *)
  | Praise of raise_kind
  (* Boolean operations *)
  | Psequand | Psequor | Pnot
  (* Integer operations *)
  | Pnegint | Paddint | Psubint | Pmulint
  | Pdivint of is_safe | Pmodint of is_safe
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pintcomp of integer_comparison
  | Poffsetint of int
  | Poffsetref of int
  (* Float operations *)
  | Pintoffloat
  | Pfloatofint
  | Pnegfloat
  | Pabsfloat
  | Paddfloat
  | Psubfloat
  | Pmulfloat
  | Pdivfloat
  | Pfloatcomp of float_comparison
  (* String operations *)
  | Pstringlength | Pstringrefu  | Pstringrefs
  | Pbyteslength | Pbytesrefu | Pbytessetu | Pbytesrefs | Pbytessets
  (* Array operations *)
  | Pmakearray of array_kind * mutable_flag
  | Pduparray of array_kind * mutable_flag
  (** For [Pduparray], the argument must be an immutable array.
      The arguments of [Pduparray] give the kind and mutability of the
      array being *produced* by the duplication. *)
  | Parraylength of array_kind
  | Parrayrefu of array_kind
  | Parraysetu of array_kind
  | Parrayrefs of array_kind
  | Parraysets of array_kind
  (* Test if the argument is a block or an immediate integer *)
  | Pisint
  (* Extract a block's tag *)
  | Pgettag
  (* Test if the (integer) argument is outside an interval *)
  | Pisout
  (* Bitvect operations *)
  | Pbittest
  (* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
  | Pbintofint of boxed_integer
  | Pintofbint of boxed_integer
  | Pcvtbint of boxed_integer (*source*) * boxed_integer (*destination*)
  | Pnegbint of boxed_integer
  | Paddbint of boxed_integer
  | Psubbint of boxed_integer
  | Pmulbint of boxed_integer
  | Pdivbint of { size : boxed_integer; is_safe : is_safe }
  | Pmodbint of { size : boxed_integer; is_safe : is_safe }
  | Pandbint of boxed_integer
  | Porbint of boxed_integer
  | Pxorbint of boxed_integer
  | Plslbint of boxed_integer
  | Plsrbint of boxed_integer
  | Pasrbint of boxed_integer
  | Pbintcomp of boxed_integer * integer_comparison
  (* Operations on big arrays: (unsafe, #dimensions, kind, layout, boxed) *)
  (* The boxing annotation is required because for not type annotated
     version, there is no way to unbox it. *)
  | Pbigarrayref of bool * int * bigarray_kind * bigarray_layout * boxed
  | Pbigarrayset of bool * int * bigarray_kind * bigarray_layout * boxed
  (* size of the nth dimension of a big array *)
  | Pbigarraydim of int
  (* load/set 16,32,64 bits from a string: (unsafe)*)
  | Pstring_load_16 of bool
  | Pstring_load_32 of bool
  | Pstring_load_64 of bool
  | Pstring_set_16 of bool
  | Pstring_set_32 of bool
  | Pstring_set_64 of bool
  (* load/set for bytes: *)
  | Pbytes_load_16 of bool
  | Pbytes_load_32 of bool
  | Pbytes_load_64 of bool
  | Pbytes_set_16 of bool
  | Pbytes_set_32 of bool
  | Pbytes_set_64 of bool
  (* load/set 16,32,64 bits from a
     (char, int8_unsigned_elt, c_layout) Bigarray.Array1.t : (unsafe) *)
  | Pbigstring_load_16 of bool
  | Pbigstring_load_32 of bool
  | Pbigstring_load_64 of bool
  | Pbigstring_set_16 of bool
  | Pbigstring_set_32 of bool
  | Pbigstring_set_64 of bool
  (* Compile time constants *)
  | Pctconst of compile_time_constant
  (* byte swap *)
  | Pbswap16
  | Pbbswap of boxed_integer
  (* Integer to external pointer *)
  | Pint_as_pointer
  (* Inhibition of optimisation *)
  | Popaque
  (* Marking of return points: only used between [Flambda_to_clambda] and
     [Un_anf]. *)
  | Preturn
  (* Construction and destruction of groups of multiple function results *)
  | Pmake_unboxed_tuple
  | Punboxed_tuple_field of int
  (* Boxing and unboxing of numbers *)
  | Punbox_float
  | Pbox_float
  | Punbox_int32
  | Pbox_int32
  | Punbox_int64
  | Pbox_int64
  | Punbox_nativeint
  | Pbox_nativeint
  (* Tagging and untagging of immediates (ints, chars, constptrs) *)
  | Puntag_immediate
  | Ptag_immediate

and integer_comparison =
    Ceq | Cne | Clt | Cgt | Cle | Cge

and float_comparison =
    CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFge | CFnge

and array_kind =
    Pgenarray | Paddrarray | Pintarray | Pfloatarray

and value_kind =
  (* CR mshinwell: Pfloatval should be renamed to Pboxedfloatval *)
    Pgenval | Pfloatval | Pboxedintval of boxed_integer | Pintval
  | Pnaked_intval

and block_shape =
  value_kind list option

and boxed_integer = Primitive.boxed_integer =
    Pnativeint | Pint32 | Pint64

and bigarray_kind =
    Pbigarray_unknown
  | Pbigarray_float32 | Pbigarray_float64
  | Pbigarray_sint8 | Pbigarray_uint8
  | Pbigarray_sint16 | Pbigarray_uint16
  | Pbigarray_int32 | Pbigarray_int64
  | Pbigarray_caml_int | Pbigarray_native_int
  | Pbigarray_complex32 | Pbigarray_complex64

and bigarray_layout =
    Pbigarray_unknown_layout
  | Pbigarray_c_layout
  | Pbigarray_fortran_layout

and raise_kind =
  | Raise_regular
  | Raise_reraise
  | Raise_notrace

val negate_integer_comparison : integer_comparison -> integer_comparison
val swap_integer_comparison : integer_comparison -> integer_comparison

val negate_float_comparison : float_comparison -> float_comparison
val swap_float_comparison : float_comparison -> float_comparison
