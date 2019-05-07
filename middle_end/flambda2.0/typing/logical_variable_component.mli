type t = Logical_variable.t

val create : Flambda_kind.t -> t
val kind : t -> Flambda_kind.t
val rename : t -> t
(*
val in_compilation_unit : t -> Compilation_unit.t -> bool
*)
val equal : Type_equality_env.t -> t -> t -> bool
val name : t -> Name.t

include Map.With_set with type t := t
include Contains_names.S with type t := t
