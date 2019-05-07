type t 

include Contains_names.S with type t := t

val invariant : t -> unit

val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

val create
   : parameters:Flambda_types.t list
  -> results:Flambda_types.t list
  -> t

val create_unknown : unit -> t

val create_bottom : unit -> t

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

val join : Join_env.t -> t -> t -> t

val introduce : t -> Typing_env.t -> Typing_env.t
