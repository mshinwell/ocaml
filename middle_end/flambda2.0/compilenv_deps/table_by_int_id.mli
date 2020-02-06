(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module Id : sig
  type t = int

  val flags_size_in_bits : int

  val flags : t -> int

  val without_flags : t -> int

  val with_flags : t -> int -> t

  include Identifiable.S with type t := t
end

module Make (E : sig
  type t

  val print : Format.formatter -> t -> unit
  val hash : t -> int
  val equal : t -> t -> bool
end) : sig
  type t

  val create : unit -> t

  val add : t -> E.t -> Id.t

  val find : t -> Id.t -> E.t
end
