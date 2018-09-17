(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017--2018 OCamlPro SAS                                    *)
(*   Copyright 2017--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Numbering of the nesting depth of continuations and bindings. *)

(* CR mshinwell: To avoid confusion amongst variables called "level", maybe
   this module should just be [Scope], not [Scope_level]? *)

module type S = sig
  type t

  include Hashtbl.With_map with type t := t

  val initial : t

  val prev : t -> t
  val next : t -> t

  val (<=): t -> t -> bool
  val (<): t -> t -> bool
  val (>): t -> t -> bool
  val (>=): t -> t -> bool

  val to_int : t -> int
end

include S

val half_prev : t -> t
val half_next : t -> t
val for_symbols : t

module Sublevel : S

module With_sublevel : sig
  type with_sublevel

  val create : t -> Sublevel.t -> with_sublevel

  val level : with_sublevel -> t
  val sublevel : with_sublevel -> Sublevel.t

  type t = with_sublevel

  val (>): t -> t -> bool

  include Hashtbl.With_map with type t := t
end
