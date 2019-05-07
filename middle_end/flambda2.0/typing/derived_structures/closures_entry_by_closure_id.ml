module RL =
  Row_like.Make (Closure_id) (Var_within_closure_set)
    (Closure_id_and_var_within_closure_set)
    (Flambda_type0_core.Closures_entry)

type t = RL.t

let create_exactly_multiple closure_id_and_vars_within_closure_map =
  RL.create_exactly_multiple closure_id_and_vars_within_closure_map

let create_at_least_multiple vars_within_closure_map =
  RL.create_at_least_multiple vars_within_closure_map

let print ~cache ppf t = RL.print ~cache ppf t

let meet env t1 t2 : _ Or_bottom.t =
  match RL.meet env t1 t2 with
  | Bottom -> Bottom
  | Ok (t, _closures_entry) -> Ok (t, Typing_env_extension.empty ())

let join = RL.join

let equal = RL.equal
let free_names = RL.free_names
let apply_name_permutation = RL.apply_name_permutation
