(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Int_replace_polymorphic_compare

module String = Misc.Stdlib.String

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
    Stdlib.(=) (Char.uppercase_ascii chr) chr

  let of_string str =
    if String.length str < 1 || not (isupper (String.get str 0)) then begin
      Misc.fatal_errorf "Bad compilation unit name: %s" str
    end;
    str

  let to_string t = t
end

type t = {
  for_pack_prefix : Name.t list;
  id : Ident.t;
  hash : int;
}

include Identifiable.Make (struct
  type nonrec t = t

  let compare
        ({ id = id1; for_pack_prefix = for_pack_prefix1; hash = hash1; } as t1)
        ({ id = id2; for_pack_prefix = for_pack_prefix2; hash = hash2; } as t2)
        =
    if t1 == t2 then 0
    else
      let c = Stdlib.compare hash1 hash2 in
      if c <> 0 then c
      else
        let c = Ident.compare id1 id2 in
        if c <> 0 then c
        else
          Misc.Stdlib.List.compare Name.compare
            for_pack_prefix1 for_pack_prefix2

  let equal x y =
    if x == y then true
    else compare x y = 0

  let print ppf { for_pack_prefix; id; hash = _; } =
    match for_pack_prefix with
    | [] ->
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(id@ %a)@])@]"
        Ident.print id
    | for_pack_prefix ->
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(for_pack_prefix@ %a)@]@;\
          @[<hov 1>(id@ %a)@])@]"
        (Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ".")
          Name.print)
        for_pack_prefix
        Ident.print id

  let output oc t =
    print (Format.formatter_of_out_channel oc) t

  let hash t = t.hash
end)

let print_name ppf t =
  Format.pp_print_string ppf (Ident.name t.id)

let create ?(for_pack_prefix = []) name =
  let id = Ident.create_persistent name in
  if not (Ident.persistent id) then begin
    Misc.fatal_error "Compilation_unit.create with non-persistent Ident.t"
  end;
  { id;
    for_pack_prefix;
    hash = Hashtbl.hash (Ident.name id, for_pack_prefix);
  }

let none = create (Name.of_string "*none*")

let name t = Name.of_string (Ident.name t.id)

let is_packed t =
  match t.for_pack_prefix with
  | [] -> false
  | _::_ -> true

let for_pack_prefix t = t.for_pack_prefix

let for_pack_prefix_as_string t =
  match for_pack_prefix t with
  | [] -> None
  | prefix -> Some (String.concat "." (List.map Name.to_string prefix))

let full_path t = t.for_pack_prefix @ [name t]

let full_path_as_string t =
  String.concat "." (List.map Name.to_string (full_path t))

let current = ref None

let is_current_exn arg =
  match !current with
  | None -> Misc.fatal_error "Current compilation unit is not set"
  | Some cur -> equal cur arg

let set_current t = current := Some t

let get_current () = !current

let get_current_exn () =
  match !current with
  | Some current -> current
  | None -> Misc.fatal_error "Current compilation unit is not set"

let get_current_id_exn () =
  let id = (get_current_exn ()).id in
  assert (Ident.persistent id);  (* see [create], above *)
  id

let path t =
  Path.Pident (Ident.create_persistent (Name.to_string (name t)))
