(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module TEE = Typing_env_extension

module Make (Thing : Identifiable.S) = struct
  module Thing_and_unit = struct
    type t = Thing.t * unit
    include Identifiable.Make_pair (Thing) (Unit)
  end

  module Thing_or_unknown = Or_unknown.Lift (Thing)

  module Thing_or_unknown_and_unit = struct
    type t = Thing.t Or_unknown.t * unit
    include Identifiable.Make_pair (Thing_or_unknown) (Unit)
  end

  module Unit_maps_to = struct
    include Unit

    let print_with_cache ~cache:_ ppf () = print ppf ()
    let meet _env () () = Or_bottom.Ok ((), TEE.empty ())
    let join _env () () = ()
    let create_bottom () = ()
    let widen () ~to_match:() = ()
  end

  include Row_like.Make (Thing) (Unit) (Thing_and_unit)
    (Thing_or_unknown_and_unit) (Unit_maps_to)

  let create things =
    let things =
      Thing.Set.fold (fun thing result ->
          Thing_and_unit.Map.add (thing, ()) () result)
        things
        Thing_and_unit.Map.empty
    in
    create_exactly_multiple things

  let all t : _ Or_unknown.t =
    let indexes = at_least t in
    let known = known t in
    if not (Thing_or_unknown_and_unit.Map.is_empty indexes) then Unknown
    else
      let things =
        Thing_and_unit.Set.fold (fun (thing, ()) things ->
            Thing.Set.add thing things)
          (Thing_and_unit.Map.keys known)
          Thing.Set.empty
      in
      Known things

  let get_singleton t =
    match get_singleton t with
    | None -> None
    | Some ((thing, ()), ()) -> Some thing
end
