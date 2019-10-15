(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type 'a t =
  | Known of 'a
  | Unknown

val print
   : (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a t
  -> unit

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val map : 'a t -> f:('a -> 'b) -> 'b t

val free_names : ('a -> Name_occurrences.t) -> 'a t -> Name_occurrences.t

module Lift (I : Identifiable.S) : sig
  type nonrec t = I.t t

  include Identifiable.S with type t := t
end
