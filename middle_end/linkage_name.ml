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

type t = string

include Identifiable.Make (struct
  include String
  let hash = Hashtbl.hash
  let print ppf t = Format.pp_print_string ppf t
  let output chan t = output_string chan t
end)

let create t = t
let to_string t = t

let caml_symbol_prefix = "caml"
let separator = "__"

let for_compilation_unit comp_unit =
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

let for_current_unit () =
  for_compilation_unit (CU.get_current_exn ())

let for_ident ?comp_unit id =
  if Ident.is_predef id then "caml_exn_" ^ Ident.name id
  else
    match Ident.compilation_unit_of_global_ident id with
    | Some comp_unit' ->
      begin match comp_unit with
      | None -> ()
      | Some comp_unit ->
        if not (CU.equal comp_unit comp_unit') then begin
          Misc.fatal_errorf "Compilation unit %a passed to \
              [Linkage_name.for_ident] does not match compilation unit on \
              the supplied global ident:@ %a"
            CU.print comp_unit
            Ident.print id
        end
      end;
      for_compilation_unit comp_unit'
    | None ->
      let comp_unit =
        match comp_unit with
        | None -> CU.get_current_exn ()
        | Some comp_unit -> comp_unit
      in
      assert (not (Ident.is_global id));
      (for_compilation_unit comp_unit) ^ separator ^ Ident.unique_name id

let for_variable var =
  (for_compilation_unit (Variable.get_compilation_unit var))
    ^ separator ^ Variable.unique_name var

let is_in_current_unit t =
  let prefix = for_current_unit () in
  String.equal t prefix
    || String.starts_with ~prefix:(prefix ^ separator) t
