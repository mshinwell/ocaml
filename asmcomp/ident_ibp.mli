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

(** Identifiers in binding position ("Ident_ibp"). *)

type provenance = {
  module_path : Path.t;
  location : Debuginfo.t;
  original_ident : Ident.t;
}

type t

val create : Ident.t -> provenance option -> t

val ident : t -> Ident.t
val provenance : t -> provenance option

val name : t -> string

val rename : t -> t

val print : Format.formatter -> t -> unit
