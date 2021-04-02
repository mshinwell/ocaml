(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Pierre Chambart and Pierrick Couderc, OCamlPro               *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-9-40-41-42"]

open! Int_replace_polymorphic_compare

module List = Misc.Stdlib.List
module String = Misc.Stdlib.String

type error =
  | Invalid_character of char
  | Bad_compilation_unit_name of string

exception Error of error

module Name = struct
  type t = string

  include Identifiable.Make (struct
    type nonrec t = t

    let compare = String.compare
    let equal = String.equal
    let hash = Hashtbl.hash
    let print = String.print
    let output chan t = print (Format.formatter_of_out_channel chan) t
  end)

  let isupper chr =
    Char.equal (Char.uppercase_ascii chr) chr

  let of_string str =
    if String.length str < 1 || not (isupper (String.get str 0)) then begin
      raise (Error (Bad_compilation_unit_name str))
    end;
    str

  let dummy = ""

  let to_string t = t
end

module Prefix = struct
  type t = Name.t list

  include Identifiable.Make (struct
    type nonrec t = t
    let equal = List.equal Name.equal

    let compare = List.compare Name.compare

    let hash = Hashtbl.hash

    let print fmt p =
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ".")
        Format.pp_print_string
        fmt
        p

    let output chan t = print (Format.formatter_of_out_channel chan) t
  end)

  let is_valid_character first_char c =
    let code = Char.code c in
    if first_char then
      code >= 65 && code <= 90 (* [A-Z] *)
    else
      Char.equal c '_'
      || code >= 48 && 57 <= 90 (* [0-9] *)
      || code >= 65 && code <= 90 (* [A-Z] *)
      || code >= 97 && code <= 122 (* [a-z] *)

  let parse pack =
    let prefix = String.split_on_char '.' pack in
    ListLabels.iter prefix ~f:(fun module_name ->
      String.iteri (fun i c ->
          if not (is_valid_character (i=0) c) then
            raise (Error (Invalid_character c)))
        module_name);
    prefix

  let parse_for_pack = function
    | None -> []
    | Some pack -> parse pack

  let extract_prefix name =
    match String.rindex_opt name '.' with
    | None -> [], name
    | Some pos ->
      parse (String.sub name 0 (pos+1)),
      String.sub name (pos+1) (String.length name - pos - 1)

  let to_string p =
    Format.asprintf "%a" print p

  let is_empty t =
    match t with
    | [] -> true
    | _::_ -> false

  let to_list_outermost_pack_first t = t
end

type t = {
  basename : Name.t;
  for_pack_prefix : Prefix.t;
  hash : int;
}

let create ?(for_pack_prefix = []) basename =
  { basename;
    for_pack_prefix;
    hash = Hashtbl.hash (basename, for_pack_prefix)
  }

let of_string str =
  let for_pack_prefix, name = Prefix.extract_prefix str in
  create ~for_pack_prefix name

let none = create (Name.of_string "*none*")

let name unit = unit.basename

let for_pack_prefix unit = unit.for_pack_prefix

let is_packed t =
  match t.for_pack_prefix with
  | [] -> false
  | _::_ -> true

let full_path unit =
  unit.for_pack_prefix @ [ unit.basename ]

include Identifiable.Make (struct
  type nonrec t = t

  let compare
        ({ basename = basename1; for_pack_prefix = for_pack_prefix1;
           hash = hash1; _} as t1)
        ({ basename = basename2; for_pack_prefix = for_pack_prefix2;
           hash = hash2; _} as t2) =
    if t1 == t2 then 0
    else
      let c = Stdlib.compare hash1 hash2 in
      if c <> 0 then c
      else
        let c = String.compare basename1 basename2 in
        if c <> 0 then c
        else Prefix.compare for_pack_prefix1 for_pack_prefix2

  let equal x y =
    if x == y then true
    else compare x y = 0

  let print ppf { for_pack_prefix; hash = _; basename } =
    match for_pack_prefix with
    | [] ->
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(id@ %s)@])@]"
        basename
    | for_pack_prefix ->
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(for_pack_prefix@ %a)@]@;\
          @[<hov 1>(basename@ %s)@]"
        Prefix.print for_pack_prefix
        basename

  let output oc t =
    print (Format.formatter_of_out_channel oc) t

  let hash t = t.hash
end)

let print_name ppf t =
  Format.pp_print_string ppf t.basename

let print_full_path fmt unit =
  match unit.for_pack_prefix with
  | [] -> Format.fprintf fmt "%a" Name.print unit.basename
  | _::_ ->
    Format.fprintf fmt "%a.%a"
      Prefix.print unit.for_pack_prefix
      Name.print unit.basename

let full_path_as_string unit =
  Format.asprintf "%a" print_full_path unit

type crcs = (t * Digest.t option) list

let current = ref None

let set_current t =
  current := Some t

let get_current_exn () =
  match !current with
  | Some t -> t
  | None -> Misc.fatal_error "No compilation unit set"

let is_current t =
  match !current with
  | None -> false
  | Some t' -> equal t t'
