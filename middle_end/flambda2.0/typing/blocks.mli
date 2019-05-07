type t

type open_or_closed = Open | Closed of Tag.t

(** Create a value which describes that there are exactly no blocks. *)
val create_bottom : unit -> t

val create : field_tys:Flambda_types.t list -> open_or_closed -> t

(*
val invariant : t -> unit
*)

val print_with_cache
   : cache:Printing_cache.t
  -> Format.formatter
  -> t
  -> unit

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

val is_empty : t -> bool

val classify : t -> unit Or_unknown_or_bottom.t

include Contains_names.S with type t := t
