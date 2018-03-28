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

(** Continuation variables. *)

include Hashtbl.With_map

val create : unit -> t

val to_int : t -> int

(* CR pchambart: moved here to avoid dependency problems with lambda.
   The right solution will be to replace every use of int as
   static exception identifiers by Continuation.t *)
val next_raise_count : unit -> int
val reset : unit -> unit

module With_args : sig
  type nonrec t = t * Variable.t list
  include Hashtbl.With_map with type t := t
end
