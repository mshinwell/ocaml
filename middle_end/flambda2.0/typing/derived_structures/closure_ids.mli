type t

(*
val invariant : t -> unit
*)

type open_or_closed = Open | Closed

val create
   : Flambda_types.set_of_closures_entry Closure_id_set.Map.t
  -> open_or_closed
  -> t

include Type_structure_intf.S with type t := t
