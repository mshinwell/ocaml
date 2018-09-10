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
  val print : Format.formatter -> t -> unit
  val rename : t -> t
  val free_name : t -> Name.t
  val apply_name_permutation : t -> Name_permutation.t -> t
end

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

module Make (Name : Name) (Term : Term) : sig
  (** The type [t] is the equivalent of an atom abstraction construction
      "[--]--" in nominal sets. *)

  include Contains_names.S
  include Common with type t := t

  val create : Name.t -> Term.t -> t

  (** Concretion of an abstraction at a fresh [Name]. *)
  val pattern_match : t -> f:(Name.t -> Term.t -> 'a) -> 'a

  (** Concretion of an abstraction at a fresh [Name] followed by reconstruction
      of the abstraction. *)
  val pattern_match_map : t -> f:(Term.t -> Term.t) -> t

  (** Like [pattern_match_map] but also provides the fresh [Name] to [f]. *)
  val pattern_match_mapi : t -> f:(Name.t -> Term.t -> Term.t) -> t

  (** Concretion of a pair of abstractions at the same fresh [Name]. *)
  val pattern_match_pair : t -> t -> f:(Name.t -> Term.t -> Term.t -> 'a) -> 'a
end

module Make_pair (Name0 : Name) (Name1 : Name) (Term : Term) : sig
  (** The type [t] is the equivalent of a "generalised abstraction"
      construction with a separated product "[-- (X) --]--" in binding
      position. *)

  include Contains_names.S
  include Common with type t := t

  val create : Name0.t -> Name1.t -> Term.t -> t

  val pattern_match : t -> f:(Name0.t -> Name1.t -> Term.t -> 'a) -> 'a

  val pattern_match_pair
     : t
    -> t
    -> f:(Name0.t -> Name1.t -> Term.t -> Term.t -> 'a)
    -> 'a
end

module Make_list (Name : Name) (Term : Term) : sig
  (** The type [t] is the equivalent of a "generalised abstraction"
      construction with an ordered list containing disjoint elements,
      represented as a separated product, in binding position. *)

  include Contains_names.S
  include Common with type t := t

  val create : Name.t list -> Term.t -> t

  val pattern_match : t -> f:(Name.t list -> Term.t -> 'a) -> 'a

  val pattern_match_pair
     : t
    -> t
    -> f:(Name.t list -> Term.t -> Term.t -> 'a)
    -> 'a
end

module Make_set (Name : Name) (Term : Term) : sig
  (** The type [t] is the equivalent of a "generalised abstraction"
      construction with an ordered set, represented as a separated product,
      in binding position. *)

  include Contains_names.S
  include Common with type t := t

  val create : Name.Set.t -> Term.t -> t

  val pattern_match : t -> f:(Name.Set.t -> Term.t -> 'a) -> 'a

  val pattern_match_pair
     : t
    -> t
    -> f:(Name.Set.t -> Term.t -> Term.t -> 'a)
    -> 'a
end
