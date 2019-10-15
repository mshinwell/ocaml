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

module Num_occurrences : sig
  type t = private
    | Zero
    | One
    | More_than_one

  val print : Format.formatter -> t -> unit
end

type t

val empty : t

val print : Format.formatter -> t -> unit

val apply_name_permutation : t -> Name_permutation.t -> t

val singleton_continuation : Continuation.t -> t

val add_continuation : t -> Continuation.t -> t

val count_continuation : t -> Continuation.t -> Num_occurrences.t

val singleton_variable : Variable.t -> Name_occurrence_kind.t -> t

val add_variable : t -> Variable.t -> Name_occurrence_kind.t -> t

val add_name : t -> Name.t -> Name_occurrence_kind.t -> t

val singleton_name : Name.t -> Name_occurrence_kind.t -> t

val singleton_symbol : Symbol.t -> Name_occurrence_kind.t -> t

val create_variables : Variable.Set.t -> Name_occurrence_kind.t -> t

val create_names : Name.Set.t -> Name_occurrence_kind.t -> t

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

val greatest_occurrence_kind_var
   : t
  -> Variable.t
  -> Name_occurrence_kind.Or_absent.t

val downgrade_occurrences_at_strictly_greater_kind
   : t
  -> Name_occurrence_kind.t
  -> t
