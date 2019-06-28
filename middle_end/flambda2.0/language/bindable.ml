(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The signature of "name-like things" that may occur in binding position
    inside [Name_abstraction] constructs. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type S = sig
  type t

  include Identifiable.S with type := t
  include Contains_names.S with type t := t

  val print : Format.formatter -> t -> unit
  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

  val rename : t -> t

  val singleton_occurrence_in_terms : t -> Name_occurrences.t
  val add_occurrence_in_terms : t -> Name_occurrences.t -> Name_occurrences.t
end
