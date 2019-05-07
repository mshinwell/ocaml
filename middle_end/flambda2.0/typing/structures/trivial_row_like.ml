
module Make (Thing_without_names : Hashtbl.With_map) = struct
  module TEE = struct
    include Typing_env_extension

    let add_or_meet_equations t env t' =
      meet env t t'

    let widen t ~to_match:_ = t

    let meet env t1 t2 : _ Or_bottom.t =
      let t = meet env t1 t2 in
      if is_empty t then Bottom
      else Ok (t, empty ())

    let bottom () = empty ()
  end

  module Thing_without_names_and_unit =
    Hashtbl.Make_with_map_pair (Thing_without_names) (Unit)

  module RL =
    Row_like.Make (Thing_without_names) (Unit)
      (Thing_without_names_and_unit) (TEE)

  type t = RL.t

  let create_with_equations things_with_env_extensions =
    let things_with_env_extensions =
      Thing_without_names.Map.fold (fun thing extension result ->
          Thing_without_names_and_unit.Map.add (thing, ()) extension result)
        things_with_env_extensions
        Thing_without_names_and_unit.Map.empty
    in
    RL.create_exactly_multiple things_with_env_extensions

  let create things =
    let things_with_env_extensions =
      Thing_without_names.Map.of_set (fun _thing -> TEE.empty ()) things
    in
    create_with_equations things_with_env_extensions

  let create_bottom = RL.create_bottom
  let create_unknown = RL.create_unknown

  let print = RL.print
  let equal = RL.equal

  let meet = RL.meet
  let join = RL.join

  let free_names = RL.free_names
  let apply_name_permutation = RL.apply_name_permutation

  let all t : _ Or_unknown.t =
    match RL.at_least t, RL.known t with
    | Unknown, _ | _, Unknown -> Unknown
    | Known indexes, Known known ->
      if not (Unit.Map.is_empty indexes) then Unknown
      else
        let things =
          Thing_without_names_and_unit.Set.fold (fun (thing, ()) things ->
              Thing_without_names.Set.add thing things)
            (Thing_without_names_and_unit.Map.keys known)
            Thing_without_names.Set.empty
        in
        Known things

  let classify = RL.classify

  let get_singleton t =
    match RL.get_singleton t with
    | None -> None
    | Some ((thing, ()), env_extension) -> Some (thing, env_extension)
end
