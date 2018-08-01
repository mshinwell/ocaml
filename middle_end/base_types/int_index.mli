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

(** Target integers equipped with various operations relating to names.
    The intended use is indexes into algebraic structures such as
    [Relational_product]. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

include module type of struct include Targetint.OCaml end

val free_names : t -> Name_occurrences.t
val bound_names : t -> Name_occurrences.t
val apply_name_permutation : t -> Name_permutation.t -> t
val freshen : t -> t
val kind : t -> Flambda_kind.t
val name : t -> Name.t
