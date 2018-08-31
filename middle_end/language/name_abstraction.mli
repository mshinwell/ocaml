(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type Name = sig
  include Contains_names.S
  val create : unit -> t
  val permutation_to_swap : t -> t -> Name_permutation.t
end

module Make (Name : Name) (Term : Contains_names.S) : sig
  (** The type [t] is the equivalent of an atom-abstraction construction
      "[--]--" in nominal sets. *)
  include Contains_names.S

  val create : Name.t -> Term.t -> t

  val pattern_match : t -> f:(Name.t -> Term.t -> 'a) -> 'a
end

module Make2 (Name0 : Name) (Name1 : Name) (Term : Contains_names.S) : sig
  include Contains_names.S

  val create : Name0.t -> Name1.t -> Term.t -> t

  val pattern_match : t -> f:(Name0.t -> Name1.t -> Term.t -> 'a) -> 'a
end
