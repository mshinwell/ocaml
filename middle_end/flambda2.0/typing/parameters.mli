include Contains_names.S

val print : Format.formatter -> t -> unit

val invariant : t -> unit

val create_bottom : unit -> t

val create : Flambda_types.t list -> t

(** Greatest lower bound. *)
val meet : Meet_env.t -> t -> t -> (t * Typing_env_extension.t) Or_bottom.t

(** Least upper bound. *)
val join : Join_env.t -> t -> t -> t
