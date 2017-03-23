(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                     Mark Shinwell, Jane Street Europe                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Environments used during instruction selection. *)

type t

val empty : t

val add : t -> Ident.t -> Reg.t array -> Asttypes.mutable_flag -> t
val add_static_exception : t -> int -> Reg.t array list -> t
val add_simple_expression : t -> Ident.t -> Cmm.expression -> t

val find : t -> Ident.t -> Reg.t array
val find_with_mutability : t -> Ident.t -> Reg.t array * Asttypes.mutable_flag
val find_static_exception : t -> int -> Reg.t array list
val find_simple_expression : t -> Ident.t -> Cmm.expression

val mutability : t -> Ident.t -> Asttypes.mutable_flag
