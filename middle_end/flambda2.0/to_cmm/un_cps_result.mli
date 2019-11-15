(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Result accumulator structure used during Flambda to Cmm translation. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

(* CR mshinwell: Put [t] arguments first. *)

val empty : t

val combine : t -> t -> t

val archive_data : t -> t

val add_data : Cmm.data_item list -> t -> t

val update_data : (Cmm.data_item list -> Cmm.data_item list) -> t -> t

val add_gc_roots : Symbol.t list -> t -> t

val add_function : Cmm.phrase -> t -> t

(* CR mshinwell: Use a "private" record for the return type of this. *)
val to_cmm
   : t
  -> Cmm.phrase list * (Symbol.t list) * (Cmm.phrase list)
