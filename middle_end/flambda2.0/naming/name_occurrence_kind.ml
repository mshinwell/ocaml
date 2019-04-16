(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2019 OCamlPro SAS                                          *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | Normal
  | In_types
  | Phantom

type kind = t

let normal = Normal
let in_types = In_types
let phantom = Phantom

let is_normal t =
  match t with
  | Normal -> true
  | In_types | Phantom -> false

let is_phantom t =
  match t with
  | Phantom -> true
  | In_types | Normal -> false

let min = Phantom

include Identifiable.Make (struct
  type nonrec t = t

  let print ppf t =
    match t with
    | Normal -> Format.pp_print_string ppf "Normal"
    | In_types -> Format.pp_print_string ppf "In_types"
    | Phantom -> Format.pp_print_string ppf "Phantom"

  let output _ _ =
    Misc.fatal_error "Name_occurrence_kind.output not yet implemented"

  let hash _ = Misc.fatal_error "Name_occurrence_kind.hash not yet implemented"

  let number t =
    match t with
    | Normal -> 2
    | In_types -> 1
    | Phantom -> 0

  let compare t1 t2 =
    Stdlib.compare (number t1) (number t2)

  let equal t1 t2 =
    compare t1 t2 = 0
end)

let all =
  Set.of_list [Normal; In_types; Phantom]

let all_less_than_or_equal_to t =
  match t with
  | Normal -> Set.of_list [Normal; In_types; Phantom]
  | In_types -> Set.of_list [In_types; Phantom]
  | Phantom -> Set.of_list [Phantom]

module Or_absent = struct
  type t =
    | Absent
    | Present of kind

  let absent = Absent
  let present kind = Present kind

  let is_present = function
    | Absent -> false
    | Present _ -> true

  include Identifiable.Make (struct
    type nonrec t = t

    let print ppf t =
      match t with
      | Absent -> Format.pp_print_string ppf "Absent"
      | Present kind ->
        Format.fprintf ppf "@[<hov 1>(Present@ %a)@]" print kind

    let output _ _ = Misc.fatal_error "Not yet implemented"

    let hash _ = Misc.fatal_error "Not yet implemented"

    let compare t1 t2 =
      match t1, t2 with
      | Absent, Absent -> 0
      | Absent, Present _ -> -1
      | Present _, Absent -> 1
      | Present kind1, Present kind2 -> compare kind1 kind2

    let equal t1 t2 =
      compare t1 t2 = 0
  end)
end

type descr =
  | Normal
  | In_types
  | Phantom

let descr (t : t) : descr =
  match t with
  | Normal -> Normal
  | In_types -> In_types
  | Phantom -> Phantom
