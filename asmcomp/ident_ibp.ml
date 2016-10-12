(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type provenance = {
  module_path : Path.t;
  location : Debuginfo.t;
  original_ident : Ident.t;
}

let print_provenance ppf (provenance : provenance) =
  Format.fprintf ppf "(from %a, %a)"
    Printtyp.path provenance.module_path
    Debuginfo.print_compact provenance.location

type t = {
  ident : Ident.t;
  provenance : provenance option;
}

let create ident provenance = { ident; provenance; }

let ident t = t.ident
let provenance t = t.provenance

let name t = Ident.name t.ident

let rename t = { t with ident = Ident.rename t.ident; }

let print ppf t =
  Format.fprintf ppf "%a%a"
    Ident.print t.ident
    (fun ppf provenance ->
      match provenance with
      | None -> ()
      | Some provenance -> print_provenance ppf provenance)
    t.provenance
