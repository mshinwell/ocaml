
val fast_equal
   : Typing_env.t
  -> Typing_env.t
  -> Flambda_types.t
  -> Flambda_types.t
  -> bool

val equal
   : bound_name:Name.t option
  -> Typing_env.t
  -> Typing_env.t
  -> Flambda_types.t
  -> Flambda_types.t
  -> bool

val equal_with_env
   : ?bound_name:Name.t
  -> Type_equality_env.t
  -> Type_equality_result.t
  -> Flambda_types.t
  -> Flambda_types.t
  -> Type_equality_result.t

val equal_closures_entry
   : Type_equality_env.t
  -> Type_equality_result.t
  -> Flambda_types.closures_entry
  -> Flambda_types.closures_entry
  -> Type_equality_result.t

val equal_set_of_closures_entry
   : Type_equality_env.t
  -> Type_equality_result.t
  -> Flambda_types.set_of_closures_entry
  -> Flambda_types.set_of_closures_entry
  -> Type_equality_result.t
