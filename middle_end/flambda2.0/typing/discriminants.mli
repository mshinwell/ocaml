type t

val print : cache:Printing_cache.t -> Format.formatter -> t -> unit

(*
(** Create a value which describes the presence of exactly no things. *)
val create_bottom : unit -> t

(** Create a value which describes the presence of an unknown set of
    things. *)
val create_unknown : unit -> t
*)

val create : Discriminant.Set.t -> t

val create_with_equations
   : Typing_env_extension.t Discriminant.Map.t
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

val get_singleton : t -> (Discriminant.t * Typing_env_extension.t) option

include Contains_names.S with type t := t
