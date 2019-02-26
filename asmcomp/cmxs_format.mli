(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2010 Institut National de Recherche en Informatique et     *)
(*     en Automatique                                                     *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Format of .cmxs files *)

open Misc

(* Each .cmxs dynamically-loaded plugin contains a symbol
   "caml_plugin_header" containing the following info
   (as an externed record) *)

type dynunit = {
  dynu_name: modname;
  dynu_crc: Digest.t;
  dynu_imports_cmi: crcs;
  dynu_imports_cmx: crcs;
  dynu_defines: string list;
}

type dynheader = {
  dynu_magic: string;
  dynu_units: dynunit list;
}
