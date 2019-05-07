
val print : Format.formatter -> Flambda_types.t -> unit

val print_with_cache
   : cache:Printing_cache.t
  -> Format.formatter
  -> Flambda_types.t
  -> unit

val print_ty_value
   : Format.formatter
  -> Flambda_types.ty_value
  -> unit

val print_ty_naked_number
   : Format.formatter
  -> _ Flambda_types.ty_naked_number
  -> unit

val print_ty_naked_immediate
   : Format.formatter
  -> Immediate.Set.t Flambda_types.ty_naked_number
  -> unit

val print_ty_naked_float
   : Format.formatter
  -> Numbers.Float_by_bit_pattern.Set.t Flambda_types.ty_naked_number
  -> unit

val print_ty_naked_int32
   : Format.formatter
  -> Numbers.Int32.Set.t Flambda_types.ty_naked_number
  -> unit

val print_ty_naked_int64
   : Format.formatter
  -> Numbers.Int64.Set.t Flambda_types.ty_naked_number
  -> unit

val print_ty_naked_nativeint
   : Format.formatter
  -> Targetint.Set.t Flambda_types.ty_naked_number
  -> unit

val print_ty_fabricated
   : Format.formatter
  -> Flambda_types.ty_fabricated
  -> unit

val print_ty_value_with_cache
   : cache:Printing_cache.t
  -> Format.formatter
  -> Flambda_types.ty_value
  -> unit

val print_ty_naked_immediate_with_cache
   : cache:Printing_cache.t
  -> Format.formatter
  -> Immediate.Set.t Flambda_types.ty_naked_number
  -> unit

val print_ty_naked_float_with_cache
   : cache:Printing_cache.t
  -> Format.formatter
  -> Numbers.Float_by_bit_pattern.Set.t Flambda_types.ty_naked_number
  -> unit

val print_ty_naked_int32_with_cache
   : cache:Printing_cache.t
  -> Format.formatter
  -> Numbers.Int32.Set.t Flambda_types.ty_naked_number
  -> unit

val print_ty_naked_int64_with_cache
   : cache:Printing_cache.t
  -> Format.formatter
  -> Numbers.Int64.Set.t Flambda_types.ty_naked_number
  -> unit

val print_ty_naked_nativeint_with_cache
   : cache:Printing_cache.t
  -> Format.formatter
  -> Targetint.Set.t Flambda_types.ty_naked_number
  -> unit

val print_ty_fabricated_with_cache
   : cache:Printing_cache.t
  -> Format.formatter
  -> Flambda_types.ty_fabricated
  -> unit

val print_function_declaration_with_cache
   : cache:Printing_cache.t
  -> Format.formatter
  -> Flambda_types.function_declaration
  -> unit
