(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2006 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type item = private {
  dinfo_file: string;
  dinfo_line: int;
  dinfo_char_start: int;
  dinfo_char_end: int
}

type t = item list

val none : t

val is_none : t -> bool

val to_string : t -> string

val from_location : Location.t -> t

val to_location : t -> Location.t

val concat: t -> t -> t

val inline: Location.t -> t -> t

val compare : t -> t -> int

val hash : t -> int

val print_compact : Format.formatter -> t -> unit

module Position : sig
  type nonrec t = t
end

module Expression : sig
  (** A value of type [t] represents debugging information attached to an
      expression (e.g. in Clambda or Cmm). *)
  type 'term t

  val create
     : Position.t
       (** Source code location of the expression. *)
    -> phantom_lets:(Ident.t * 'term) list
       (** List of phantom lets to surround the expression.  They must be in
           scope order, outermost first. *)
    -> 'term t

  val concat : outer:'term t -> inner:'term t -> 'term t

  val disjoint_union : 'term t -> 'term t -> 'term t
end
