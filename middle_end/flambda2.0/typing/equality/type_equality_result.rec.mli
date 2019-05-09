type t

val create : unit -> t

val (>>=) : t -> (t -> t) -> t

module Uses : sig
  type t

  val more_than_one_use_and_empty : t -> bool
end

val types_known_unequal : unit -> t

val are_types_known_equal : t -> bool
