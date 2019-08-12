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

[@@@ocaml.warning "+a-30-40-41-42"]

module Entry = struct
  type t =
    | Const of Rec_info.t
    | Name of Name.t

  include Identifiable.Make (struct
    type nonrec t = t

    let print ppf t =
      match t with
      | Const rec_info ->
        Format.fprintf ppf "@[(Const@ %a)@]" Rec_info.print rec_info
      | Name name ->
        Format.fprintf ppf "@[(Name@ %a)@]" Name.print name

    let output _ _ = Misc.fatal_error "Not yet implemented"

    let hash t =
      match t with
      | Const rec_info -> Hashtbl.hash (0, Rec_info.hash rec_info)
      | Name name -> Hashtbl.hash (1, Name.hash name)

    let compare t1 t2 =
      match t1, t2 with
      | Const rec_info1, Const rec_info2 ->
        Rec_info.compare rec_info1 rec_info2
      | Name name1, Name name2 ->
        Name.compare name1 name2
      | Const _, Name _ -> -1
      | Name _, Const _ -> 1

    let equal t1 t2 = (compare t1 t2 = 0)
  end)

  let const rec_info = Const rec_info

  let name name = Name name

  let free_names t =
    match t with
    | Const _ -> Name_occurrences.empty
    | Name name ->
      Name_occurrences.singleton_name name Name_occurrence_kind.normal

  let free_names_in_types t =
    match t with
    | Const _ -> Name_occurrences.empty
    | Name name ->
      Name_occurrences.singleton_name name Name_occurrence_kind.in_types

  let apply_name_permutation t perm =
    match t with
    | Const _ -> t
    | Name name -> Name (Name_permutation.apply_name perm name)
end

type t = Entry.t list

let empty = []

let add_newer_rec_info (t : t) (entry : Entry.t) =
  match t with
  | [] -> [entry]
  | head :: rest ->
    match entry, head with
    | Const _, Name _ | Name _, Const _ | Name _, Name _ -> entry :: t
    | Const newer_rec_info, Const older_rec_info ->
      let rec_info = Rec_info.merge older_rec_info ~newer:newer_rec_info in
      (Entry.Const rec_info) :: rest

let to_list_newest_first t = t

let free_names t =
  Name_occurrences.union_list (List.map Entry.free_names t)

let free_names_in_types t =
  Name_occurrences.union_list (List.map Entry.free_names_in_types t)

let apply_name_permutation t perm =
  List.map (fun entry -> Entry.apply_name_permutation entry perm) t

include Identifiable.Make (struct
  type nonrec t = t

  let print ppf t =
    Format.fprintf ppf "@[<hov 1>(%a)@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Entry.print)
      t

  let output _ _ = Misc.fatal_error "Not yet implemented"

  let hash t = Hashtbl.hash (List.map Entry.hash t)

  let compare t1 t2 = Misc.Stdlib.List.compare Entry.compare t1 t2

  let equal t1 t2 = (compare t1 t2 = 0)
end)
