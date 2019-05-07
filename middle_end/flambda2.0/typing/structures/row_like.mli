module Make
  (Tag : Hashtbl.With_map)
  (Index : Hashtbl.With_map)
  (Tag_and_index : sig
    (** These values will not contain any names. *)
    type t = Tag.t * Index.t
    include Hashtbl.With_map with type t := t
  end)
  (Maps_to : sig
    type t

    val bottom : unit -> t

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

    val add_or_meet_equations
       : t
      -> Meet_env.t
      -> Typing_env_extension.t
      -> t

    val widen : t -> to_match:t -> t

    val meet
       : Meet_env.t
      -> t
      -> t
      -> (t * Typing_env_extension.t) Or_bottom.t

    val join : Join_env.t -> t -> t -> t

    include Contains_names.S with type t := t
  end) :
sig
  type t

  val print : cache:Printing_cache.t -> Format.formatter -> t -> unit

  val create_bottom : unit -> t

  val create_unknown : unit -> t

  val create_exactly : Tag.t -> Index.t -> Maps_to.t -> t

  val create_exactly_multiple : Maps_to.t Tag_and_index.Map.t -> t

  val create_at_least : Index.t -> Maps_to.t -> t

  val create_at_least_multiple : Maps_to.t Index.Map.t -> t

  val is_bottom : t -> bool

  val equal
     : Type_equality_env.t
    -> Type_equality_result.t
    -> t
    -> t
    -> Type_equality_result.t

  (** The [Maps_to] value which [meet] returns contains the join of all
      [Maps_to] values in the range of the row-like structure after the meet
      operation has been completed. *)
  val meet
     : Meet_env.t
    -> t
    -> t
    -> (t * Maps_to.t) Or_bottom.t

  val join : Join_env.t -> t -> t -> t

  val known : t -> Maps_to.t Tag_and_index.Map.t Or_unknown.t

  val at_least : t -> Maps_to.t Index.Map.t Or_unknown.t

  val get_singleton : t -> (Tag_and_index.t * Maps_to.t) option

  val classify : t -> unit Or_unknown_or_bottom.t

  include Contains_names.S with type t := t
end
