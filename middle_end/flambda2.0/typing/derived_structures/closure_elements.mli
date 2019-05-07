type t

val create : Flambda_types.t Var_within_closure.Map.t -> t

val create_bottom : unit -> t

include Type_structure_intf.S with type t := t
