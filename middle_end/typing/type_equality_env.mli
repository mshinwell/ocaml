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

(* CR mshinwell: We could call this [Type_traversal_env] and then
   introduce it as a component in [Meet_env], replacing the existing
   permutation-handling code there. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

val create
   : perm_left:Name_permutation.t
  -> perm_right:Name_permutation.t
  -> t

val empty : t

val print : Format.formatter -> t -> unit

val perm_left : t -> Name_permutation.t

val perm_right : t -> Name_permutation.t

val shortcut_precondition : t -> bool

val compose_name_permutations
   : t
  -> perm_left:Name_permutation.t
  -> perm_right:Name_permutation.t
  -> t
