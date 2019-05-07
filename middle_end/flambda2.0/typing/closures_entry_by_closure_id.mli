type t

val print : cache:Printing_cache.t -> Format.formatter -> t -> unit

(** Describe one or more closures by giving for each one the closure ID
    and the set of variables in the closure. *)
val create_exactly_multiple
   : Flambda_types.closures_entry
       Closure_id_and_var_within_closure_set.Map.t
  -> t

(** Describe one or more closures that contain at least the given closure
    variables. *)
val create_at_least_multiple
   : Flambda_types.closures_entry Var_within_closure_set.Map.t
  -> t

val equal
   : Type_equality_env.t
  -> Type_equality_result.t
  -> t
  -> t
  -> Type_equality_result.t

(** Greatest lower bound of two values of type [t]. *)
val meet
   : Meet_env.t
  -> t
  -> t
  -> (t * Typing_env_extension.t) Or_bottom.t

(** Least upper bound of two values of type [t]. *)
val join
   : Join_env.t
  -> t
  -> t
  -> t

include Contains_names.S with type t := t
