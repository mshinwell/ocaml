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

(** A structure for keeping track of a set of bindable names together with a
    form of modal type that describes where particular names occur. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

type occurrence_kind =
  | In_terms
  | In_types
  | Debug_only

val create : unit -> t

val create_from_set_in_terms : Bindable_name.Set.t -> t

val create_from_name_set_in_terms : Name.Set.t -> t

val create_from_set_in_types : Bindable_name.Set.t -> t

val singleton_in_terms : Bindable_name.t -> t

val singleton_in_types : Bindable_name.t -> t

val of_list_in_terms : Bindable_name.t list -> t

val add : t -> Bindable_name.t -> occurrence_kind -> t

val add_set : t -> Bindable_name.Set.t -> occurrence_kind -> t

val remove : t -> Bindable_name.t -> t

val mem : t -> Bindable_name.t -> bool

val mem_in_terms : t -> Bindable_name.t -> bool

val mem_in_types : t -> Bindable_name.t -> bool

val mem_in_debug_only : t -> Bindable_name.t -> bool

val in_terms : t -> Bindable_name.Set.t

val in_types : t -> Bindable_name.Set.t

val in_debug_only : t -> Bindable_name.Set.t

val everything : t -> Bindable_name.Set.t

(* CR mshinwell: rename to [everything_must_be_a_name] *)
val everything_must_only_be_names : t -> Name.Set.t

val diff : t -> t -> t

val union : t -> t -> t

val union_list : t list -> t

val subset : t -> t -> bool

val promote_to_in_types : t -> t

val promote_to_debug_only : t -> t

val variables_only : t -> t

val fold_everything : t -> init:'a -> f:('a -> Bindable_name.t -> 'a) -> 'a

val choose_and_remove_amongst_everything : t -> (Bindable_name.t * t) option

val equal : t -> t -> bool

val print : Format.formatter -> t -> unit

val apply_name_permutation : t -> Name_permutation.t -> t
