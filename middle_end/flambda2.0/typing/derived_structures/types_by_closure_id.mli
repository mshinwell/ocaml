
type t

val create : Flambda_types.t Closure_id.Map.t -> t

val create_bottom : unit -> t

val add_or_meet_equations
   : t
  -> Meet_env.t
  -> Typing_env_extension.t
  -> t

include Type_structure_intf.S with type t := t
