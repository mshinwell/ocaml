module Make
  (Index : Name_like_intf.S)
  (Component : sig
    include Name_like_intf.S
    val create : Flambda_kind.t -> t
    val equal
       : Type_equality_env.t
      -> t
      -> t
      -> bool
    val name : t -> Name.t
    val kind : t -> Flambda_kind.t
  end) =
struct
  module rec RP : sig
    (* CR mshinwell: This is the THIRD copy of this signature *)
    type t

    include Contains_names.S with type t := t

    val invariant : t -> unit

    val print : Format.formatter -> t -> unit

    val print_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> t
      -> unit

    val create
       : ?nested:RP.t
      -> Component.t Index.Map.t
      -> Typing_env_extension.t
      -> t

    val create_bottom : unit -> t

    val equal
       : Type_equality_env.t
      -> Type_equality_result.t
      -> t
      -> t
      -> Type_equality_result.t

    val widen : t -> to_match:t -> t

    val meet
       : Meet_env.t
      -> t
      -> t
      -> (t * Typing_env_extension.t) Or_bottom.t

    val join : Join_env.t -> t -> t -> t

    val standalone_extension : t -> Typing_env_extension.t

    val introduce : t -> Typing_env.t -> Typing_env.t

    val add_or_meet_equations
       : t
      -> Meet_env.t
      -> Typing_env_extension.t
      -> t
  end = Relational_product0.Make (Index) (Component) (RP)

  include RP
end
