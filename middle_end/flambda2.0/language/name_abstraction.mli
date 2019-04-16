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

module type Term = sig
  include Contains_names.S
  val print : Format.formatter -> t -> unit
  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit
end

type printing_style =
  | Normal
  | Brackets
  | Existential

module type Common = sig
  type t

  val print : ?style:printing_style -> Format.formatter -> t -> unit

  val print_with_cache
     : ?style:printing_style
    -> cache:Printing_cache.t
    -> Format.formatter
    -> t
    -> unit
end

module Make (Term : Term) : sig
  (** The type [t] is the equivalent of an atom abstraction construction
      "[--]--" in nominal sets. *)

  include Contains_names.S
  include Common with type t := t

  (* CR mshinwell: Always using [Bindable_name] isn't ideal---it means that
     continuations are admitted statically when only variables are allowed,
     for example.  We should reintroduce a signature. *)
  val create : Bindable_name.t -> Term.t -> t

  (** Concretion of an abstraction at a fresh name. *)
  val pattern_match : t -> f:(Bindable_name.t -> Term.t -> 'a) -> 'a

  (** Concretion of an abstraction at a fresh name followed by reconstruction of
      the abstraction. *)
  val pattern_match_map : t -> f:(Term.t -> Term.t) -> t

  (** Like [pattern_match_map] but also provides the fresh name to [f]. *)
  val pattern_match_mapi : t -> f:(Bindable_name.t -> Term.t -> Term.t) -> t

  (** Concretion of a pair of abstractions at the same fresh name. *)
  val pattern_match_pair
     : t
    -> t
    -> f:(Bindable_name.t -> Term.t -> Term.t -> 'a)
    -> 'a
end

module Make_list (Term : Term) : sig
  (** The type [t] is the equivalent of a "generalised abstraction" construction
      with an ordered list containing disjoint elements, represented as a
      separated product, in binding position. *)

  include Contains_names.S
  include Common with type t := t

  val create : Bindable_name.t list -> Term.t -> t

  (** Concretion of an abstraction at a fresh name. *)
  val pattern_match : t -> f:(Bindable_name.t list -> Term.t -> 'a) -> 'a

  (** Concretion of an abstraction at a fresh name followed by reconstruction of
      the abstraction. *)
  val pattern_match_map : t -> f:(Term.t -> Term.t) -> t

  (** Like [pattern_match_map] but also provides the fresh names to [f]. *)
  val pattern_match_mapi
     : t
    -> f:(Bindable_name.t list -> Term.t -> Term.t)
    -> t

  (** Concretion of a pair of abstractions at the same fresh [Bindable_name]. *)
  val pattern_match_pair
     : t
    -> t
    -> f:(Bindable_name.t list -> Term.t -> Term.t -> 'a)
    -> 'a
end
