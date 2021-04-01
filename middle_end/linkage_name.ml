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

open! Int_replace_polymorphic_compare

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

let for_compilation_unit comp_unit =
  let prefix = CU.for_pack_prefix comp_unit in
  let name = CU.Name.to_string (CU.name comp_unit) in
  if CU.Prefix.is_empty prefix then name
  else
    let p = CU.Prefix.to_string prefix in
    let b = Buffer.create 64 in
    for i = 0 to String.length p - 1 do
      match p.[i] with
      | '.' -> Buffer.add_string b "__"
      |  c  -> Buffer.add_char b c
    done;
    Buffer.add_string b "__";
    Buffer.add_string b name;
    Buffer.contents b

let for_current_unit () =
  for_compilation_unit (Compilation_unit.get_current_exn ())

let for_global_ident id =
  if Ident.is_predef id then "caml_exn_" ^ Ident.name id
  else for_compilation_unit (Ident.compilation_unit_of_global_ident id)
