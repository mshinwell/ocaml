
type t

val print : cache:Printing_cache.t -> Format.formatter -> t -> unit

val create : Flambda_types.t Closure_id.Map.t -> t

val create_bottom : unit -> t

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

val add_or_meet_equations
   : t
  -> Meet_env.t
  -> Typing_env_extension.t
  -> t

include Contains_names.S with type t := t
