(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2019 OCamlPro SAS                                          *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** A structure for counting name-like entities that occur free in terms
    or types. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Kind : sig
  type t
  type kind = t

  val normal : t

  (** A name that does not occur in terms (but may be required for the
      generation of debugging information), but does occur in types. *)
  val in_types : t

  (** A name that neither occurs in names nor types, but is required for the
      generation of debugging information. *)
  val phantom : t

  include Identifiable.S with type t := t

  module Or_absent : sig
    type t = private
      | Absent
      | Present of kind

    val absent : t
    val present : kind -> t

    include Identifiable.S with type t := t
  end
end

type t

val empty : t

val print : Format.formatter -> t -> unit

val apply_name_permutation : t -> Name_permutation.t -> t

val singleton_continuation : Continuation.t -> t

val add_continuation : t -> Continuation.t -> t

val count_continuation : t -> Continuation.t -> int

val singleton_variable : Variable.t -> Kind.t -> t

val add_variable : t -> Variable.t -> Kind.t -> t

val singleton_name : Name.t -> Kind.t -> t

val singleton_symbol : Symbol.t -> Kind.t -> t

val create_names : Name.Set.t -> Kind.t -> t

val diff : t -> t -> t

val union : t -> t -> t

val union_list : t list -> t

val subset : t -> t -> bool

val variables : t -> Variable.Set.t

val symbols : t -> Symbol.Set.t

val mem_var : t -> Variable.t -> bool

val mem_name : t -> Name.t -> bool

val remove_var : t -> Variable.t -> t

val only_contains_symbols : t -> bool

val greatest_occurrence_kind_var : t -> Variable.t -> Kind.Or_absent.t
