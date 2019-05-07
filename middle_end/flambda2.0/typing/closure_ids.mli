type t

(*
val invariant : t -> unit
*)

type open_or_closed = Open | Closed

val create
   : Flambda_types.set_of_closures_entry Closure_id_set.Map.t
  -> open_or_closed
  -> t

val print : cache:Printing_cache.t -> Format.formatter -> t -> unit

val equal
   : Type_equality_env.t
  -> Type_equality_result.t
  -> t
  -> t
  -> Type_equality_result.t

val meet
   : Meet_env.t
  -> t
  -> t
  -> (t * Typing_env_extension.t) Or_bottom.t

val join
   : Join_env.t
  -> t
  -> t
  -> t

include Contains_names.S with type t := t
