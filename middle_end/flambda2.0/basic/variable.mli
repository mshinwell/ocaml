(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** [Variable.t] is the equivalent of a non-persistent [Ident.t] in
    the [Flambda] tree.  It wraps an [Ident.t] together with its source
    [compilation_unit].  As such, it is unique within a whole program,
    not just one compilation unit.

    Introducing a new type helps in tracing the source of identifiers
    when debugging the inliner.  It also avoids Ident renaming when
    importing cmx files.
*)

include Identifiable.S

val create
   : ?current_compilation_unit:Compilation_unit.t
  -> ?user_visible:unit
  -> string
  -> t

val create_with_same_name_as_ident : ?user_visible:unit -> Ident.t -> t

(* CR mshinwell: check on gdb branch if this preserves the "original ident".
   Sometimes it should and other times it should not (eg unboxing) *)
val rename
   : ?current_compilation_unit:Compilation_unit.t
  -> ?append:string
  -> t
  -> t

val fresh : unit -> t

val user_visible : t -> bool

val with_user_visible : t -> user_visible:bool -> t

val unique_name : t -> string

val print_list : Format.formatter -> t list -> unit
val print_opt : Format.formatter -> t option -> unit

val raw_name : t -> string
val raw_name_stamp : t -> int

(** If the given variable has the given stamp, call the user-supplied
    function.  For debugging purposes only. *)
val debug_when_stamp_matches : t -> stamp:int -> f:(unit -> unit) -> unit

type pair = t * t
module Pair : Identifiable.S with type t := pair

(* CR mshinwell: move to List.compare *)
val compare_lists : t list -> t list -> int

module List : sig
  type nonrec t = t list

  val rename
     : ?current_compilation_unit:Compilation_unit.t
    -> ?append:string
    -> t
    -> t
end
