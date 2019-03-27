(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2018-2019 Jane Street Group LLC                            *)
(*   Copyright 2019 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = {
  module_path : Path.t;
  location : Debuginfo.t;
  original_ident : Ident.t;
}

let print ppf { module_path; location; original_ident; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(module_path@ %a)@]@ \
      @[<hov 1>(location@ %a)@]@ \
      @[<hov 1>(original_ident@ %a)@]\
      )@]"
    Path.print module_path
    Debuginfo.print_compact location
    Ident.print original_ident

let create ~module_path ~location ~original_ident =
  { module_path;
    location;
    original_ident;
  }

let module_path t = t.module_path
let location t = t.location
let original_ident t = t.original_ident
