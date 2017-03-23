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

type t = private {
  vars : (Ident.t, Reg.t array) Tbl.t;
  immutable_vars : Ident.Set.t;
  simple_expressions : Cmm.expression Ident.Map.t;
  static_exceptions : (int, Reg.t array list) Tbl.t;
  (** Which registers must be populated when jumping to the given
      handler. *)
}

val empty : t

val add : Ident.t -> Reg.t array -> immutable:bool -> t -> t

val add_static_exception : Ident.t -> int -> Reg.t array list -> t -> t

val add_simple_expression : t -> Ident.t -> Cmm.expression -> t

val find : Ident.t -> t -> Reg.t array

val find_static_exception : Ident.t -> int -> t -> Reg.t array list

val is_immutable : t -> Ident.t -> bool

val find_simple_expression : t -> Ident.t -> Cmm.expression
