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

module type S = sig
  type t

  include Identifiable.S with type t := t

  val initial : t

  val prev : t -> t
  val next : t -> t

  val (>=): t -> t -> bool

  val to_int : t -> int
end

include S

val for_symbols : t

module Sublevel : S

module With_sublevel : sig
  type with_sublevel

  val create : t -> Sublevel.t -> with_sublevel

  val level : with_sublevel -> t
  val sublevel : with_sublevel -> Sublevel.t

  type t = with_sublevel
  include Identifiable.S with type t := t
end
