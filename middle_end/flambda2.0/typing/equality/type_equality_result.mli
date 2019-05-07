type t

val create : unit -> t

val (>>=) : t -> (t -> t) -> t

val delay_existential : t -> Name.t -> must_equal_one_of:Name.Set.t -> t

module Uses : sig
  type t

  val more_than_one_use_and_empty : t -> bool
end

val leaving_scope_of_existential
   : t
  -> bound_names:Name.Set.t
  -> Uses.t Name.Map.t * t

val types_known_unequal : unit -> t

val are_types_known_equal : t -> bool
