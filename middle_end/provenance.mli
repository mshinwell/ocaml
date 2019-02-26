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

(** Source level "provenance" to annotate variables and symbols,
    used for the emission of debugging information. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

val create
  : module_path:Path.t
  -> location:Debuginfo.t
  -> original_ident:Ident.t
  -> t

val module_path : t -> Path.t
val location : t -> Debuginfo.t
val original_ident : t -> Ident.t

val print : Format.formatter -> t -> unit
