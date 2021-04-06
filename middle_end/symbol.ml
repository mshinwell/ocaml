(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-9-30-40-41-42"]

module CU = Compilation_unit

type t = {
  compilation_unit : Compilation_unit.t;
  linkage_name : string;
  hash : int;
}

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    if t1 == t2 then 0
    else
      let c = compare t1.hash t2.hash in
      if c <> 0 then c
      else
        (* Linkage names are unique across a whole project, so just comparing
           those is sufficient. *)
        String.compare t1.linkage_name t2.linkage_name

  let equal t1 t2 = compare t1 t2 = 0
  let output chan t = output_string chan t.linkage_name
  let hash { hash; } = hash
  let print ppf t = Format.pp_print_string ppf t.linkage_name
end)

let linkage_name t = t.linkage_name
let compilation_unit t = t.compilation_unit

let caml_symbol_prefix = "caml"
let separator = "__"

let linkage_name_for_compilation_unit comp_unit =
  let name = CU.Name.to_string (CU.name comp_unit) in
  let for_pack_prefix = CU.for_pack_prefix comp_unit in
  let suffix =
    if CU.Prefix.is_empty for_pack_prefix then name
    else
      let pack_names =
        CU.Prefix.to_list_outermost_pack_first for_pack_prefix
        |> List.map CU.Name.to_string
      in
      String.concat separator (pack_names @ [name])
  in
  caml_symbol_prefix ^ suffix

let linkage_name_for_current_unit () =
  linkage_name_for_compilation_unit (CU.get_current_exn ())

let linkage_name_for_ident id =
  if Ident.is_predef id then
    "caml_exn_" ^ Ident.name id
  else if Ident.is_global id then
    Ident.compilation_unit_of_global_or_predef_ident id
    |> linkage_name_for_compilation_unit
  else begin
    assert (not (Ident.is_global id));
    (linkage_name_for_compilation_unit (CU.get_current_exn ()))
      ^ separator ^ Ident.unique_name id
  end

let for_ident id =
  let linkage_name = linkage_name_for_ident id in
  let compilation_unit =
    if Ident.is_global_or_predef id
    then Ident.compilation_unit_of_global_or_predef_ident id
    else Compilation_unit.get_current_exn ()
  in
  { compilation_unit;
    linkage_name;
    hash = Hashtbl.hash linkage_name;
  }

let for_compilation_unit compilation_unit =
  let linkage_name = linkage_name_for_current_unit () in
  { compilation_unit;
    linkage_name;
    hash = Hashtbl.hash linkage_name;
  }

let for_current_unit () =
  for_compilation_unit (Compilation_unit.get_current_exn ())

module Flambda = struct
  let for_variable var =
    let compilation_unit = Variable.get_compilation_unit var in
    let linkage_name =
      (linkage_name_for_compilation_unit compilation_unit)
        ^ separator ^ Variable.unique_name var
    in
    { compilation_unit;
      linkage_name;
      hash = Hashtbl.hash linkage_name;
    }

  let for_closure closure_id =
    let compilation_unit = Closure_id.get_compilation_unit closure_id in
    let linkage_name =
      (linkage_name_for_compilation_unit compilation_unit)
        ^ separator ^ Closure_id.unique_name closure_id ^ "_closure"
    in
    { compilation_unit;
      linkage_name;
      hash = Hashtbl.hash linkage_name;
    }

  let for_code_of_closure closure_id =
    let compilation_unit = Closure_id.get_compilation_unit closure_id in
    let linkage_name =
      (linkage_name_for_compilation_unit compilation_unit)
        ^ separator ^ Closure_id.unique_name closure_id
    in
    { compilation_unit;
      linkage_name;
      hash = Hashtbl.hash linkage_name;
    }
end

let const_label = ref 0

let for_new_const_in_current_unit () =
  incr const_label;
  let linkage_name =
    linkage_name_for_current_unit () ^ separator ^ (Int.to_string !const_label)
  in
  { compilation_unit = Compilation_unit.get_current_exn ();
    linkage_name;
    hash = Hashtbl.hash linkage_name;
  }

let is_predef_exn t =
  Compilation_unit.equal t.compilation_unit Compilation_unit.predef_exn
