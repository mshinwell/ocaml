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

module Uses : sig
  type t

  val create : must_equal_one_of:Name.Set.t -> t

  val combine : t -> must_equal_one_of:Name.Set.t -> t

  val more_than_one_use_and_empty : t -> bool
end = struct
  type t =
    | One of { must_equal_one_of : Name.Set.t; }
    | Many of { must_equal_one_of : Name.Set.t; }

  let create ~must_equal_one_of =
    One { must_equal_one_of; }

  let combine t ~must_equal_one_of:must_equal_one_of' =
    match t with
    | One { must_equal_one_of; }
    | Many { must_equal_one_of; } ->
      let must_equal_one_of =
        Name.Set.inter must_equal_one_of must_equal_one_of'
      in
      Many { must_equal_one_of; }

  let more_than_one_use_and_empty = function
    | One _ -> false
    | Many { must_equal_one_of; } -> Name.Set.is_empty must_equal_one_of
end

type t =
  | Ok of { delayed_existentials : Uses.t Name.Map.t; }
  | Unequal

let create () =
  Ok { delayed_existentials = Name.Map.empty; }

let (>>=) result f =
  match result with
  | Unequal -> Unequal
  | Ok _ -> f result

let delay_existential t name ~must_equal_one_of =
  match t with
  | Unequal -> Unequal
  | Ok { delayed_existentials; } ->
    let delayed_existentials =
      Name.Map.update name (function
          | None -> Some (Uses.create ~must_equal_one_of)
          | Some uses ->
            Some (Uses.combine uses ~must_equal_one_of))
        delayed_existentials
    in
    Ok { delayed_existentials; }

let leaving_scope_of_existential t ~bound_names:names =
  match t with
  | Unequal -> Name.Map.empty, t
  | Ok { delayed_existentials; } ->
    let check_now, delayed_existentials =
      Name.Map.partition (fun name _must_equal_one_of ->
          Name.Set.mem name names)
        delayed_existentials
    in
    let t = Ok { delayed_existentials; } in
    check_now, t

let types_known_unequal () = Unequal

let are_types_known_equal t =
  match t with
  | Unequal -> false
  | Ok _ -> true
