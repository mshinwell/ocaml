(* CR mshinwell: See if we can simplify this.  Originally I wanted to
   recursively-define modules [T0] with another [T] inside
   [Relational_product.Make] such that [T0] contained
   "nested : T.t option" -- but this suffers from the double vision
   problem.  We can't use [module type of] on the result of the
   [Relational_product0] functor here as it produces an illegal recursive
   module reference error. *)
module Make
  (Index : Name_like_intf.S)
  (Component : sig
    include Name_like_intf.S
    val create : Flambda_kind.t -> t
    val equal : Type_equality_env.t -> t -> t -> bool
    val name : t -> Name.t
    val kind : t -> Flambda_kind.t
  end) :
sig
  type t

  include Contains_names.S with type t := t

  (** Perform invariant checks upon the given relational product. *)
  val invariant : t -> unit

  (** Format the given relational product value as an s-expression. *)
  val print : Format.formatter -> t -> unit

  val print_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> t
    -> unit

  (** Create a relational product value given:
      - the indexes (with associated components) for the product;
      - the equations that hold between the components in the product;
      - any other relational product over which the newly-created one is
        to be scoped.  The newly-created one will bind references to
        components in the nested one. *)
  val create
     : ?nested:t
    -> Component.t Index.Map.t
    -> Typing_env_extension.t
    -> t

  val create_bottom : unit -> t

  (** A conservative approximation to equality. *)
  val equal
     : Type_equality_env.t
    -> Type_equality_result.t
    -> t
    -> t
    -> Type_equality_result.t

  (** Ensure that the given relational product contains components for
      all indexes less than or equal to the given index. *)
  val widen : t -> to_match:t -> t

  (** Greatest lower bound of two relational products. *)
  val meet
     : Meet_env.t
    -> t
    -> t
    -> (t * Typing_env_extension.t) Or_bottom.t

  (** Least upper bound of two relational products. *)
  val join : Join_env.t -> t -> t -> t

  (** The environment extension associated with the given relational
      product, including at the start, existentially-bound definitions
      of each component to bottom (hence the name "standalone"). *)
  val standalone_extension : t -> Typing_env_extension.t

  (** Add or meet the definitions and equations from the given relational
      product value into the given typing environment. *)
  val introduce : t -> Typing_env.t -> Typing_env.t

  (** Add or meet the given equations into the environment extension held
      within the relational product. *)
  val add_or_meet_equations
     : t
    -> Meet_env.t
    -> Typing_env_extension.t
    -> t
end
