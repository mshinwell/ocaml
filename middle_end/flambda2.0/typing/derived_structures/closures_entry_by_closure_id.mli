type t

(** Describe one or more closures by giving for each one the closure ID
    and the set of variables in the closure. *)
val create_exactly_multiple
   : Flambda_types.closures_entry
       Closure_id_and_var_within_closure_set.Map.t
  -> t

(** Describe one or more closures that contain at least the given closure
    variables. *)
val create_at_least_multiple
   : Flambda_types.closures_entry Var_within_closure_set.Map.t
  -> t

include Type_structure_intf.S with type t := t
