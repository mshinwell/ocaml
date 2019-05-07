
module Make (Thing_without_names : Hashtbl.With_map) : sig
  type t

  val print : cache:Printing_cache.t -> Format.formatter -> t -> unit

  (** Create a value which describes the presence of exactly no things. *)
  val create_bottom : unit -> t

  (** Create a value which describes the presence of an unknown set of
      things. *)
  val create_unknown : unit -> t

  val create : Thing_without_names.Set.t -> t

  val create_with_equations
     : Typing_env_extension.t Thing_without_names.Map.t
    -> t

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

  val all : t -> Thing_without_names.Set.t Or_unknown.t

  val get_singleton
     : t
    -> (Thing_without_names.t * Typing_env_extension.t) option

  val classify : t -> unit Or_unknown_or_bottom.t

  include Contains_names.S with type t := t
end
