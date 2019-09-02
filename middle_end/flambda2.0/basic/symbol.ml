(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type t = {
  compilation_unit : Compilation_unit.t;
  linkage_name : Linkage_name.t;
  hash : int;
}

type symbol = t

module I = Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    (* Linkage names are unique across a whole project, so just comparing
       those is sufficient. *)
    if t1 == t2 then 0
    else
      let c = compare t1.hash t2.hash in
      if c <> 0 then c
      else Linkage_name.compare t1.linkage_name t2.linkage_name

  let equal t1 t2 = (compare t1 t2 = 0)

  let hash t = t.hash

  let print ppf t =
    Format.fprintf ppf "@<0>%s" (Flambda_colours.symbol ());
    Compilation_unit.print ppf t.compilation_unit;
    Format.pp_print_string ppf ".";
    Linkage_name.print ppf t.linkage_name;
    Format.fprintf ppf "@<0>%s" (Flambda_colours.normal ())

  let output chan t =
    print (Format.formatter_of_out_channel chan) t
end)

include I

let create compilation_unit linkage_name =
  let unit_linkage_name =
    Linkage_name.to_string
      (Compilation_unit.get_linkage_name compilation_unit)
  in
  let linkage_name =
    Linkage_name.create
      (unit_linkage_name ^ "__" ^ (Linkage_name.to_string linkage_name))
  in
  let hash = Linkage_name.hash linkage_name in
  { compilation_unit; linkage_name; hash; }

let unsafe_create compilation_unit linkage_name =
  let hash = Linkage_name.hash linkage_name in
  { compilation_unit; linkage_name; hash; }

let import_for_pack t ~pack:compilation_unit =
  let hash = Linkage_name.hash t.linkage_name in
  { compilation_unit;
    linkage_name = t.linkage_name; hash;
  }

let compilation_unit t = t.compilation_unit
let linkage_name t = t.linkage_name

let print_opt ppf = function
  | None -> Format.fprintf ppf "<no symbol>"
  | Some t -> print ppf t

let compare_lists l1 l2 =
  Misc.Stdlib.List.compare compare l1 l2

let in_compilation_unit t cu =
  Compilation_unit.equal t.compilation_unit cu

let is_predefined_exception t =
  Compilation_unit.is_predefined_exception t.compilation_unit

let rename t =
  let linkage_name = Linkage_name.rename t.linkage_name in
  create t.compilation_unit linkage_name
