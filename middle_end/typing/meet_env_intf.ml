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

module type S = sig
  module Typing_env : sig type t end

  type t

  val create
     : Typing_env.t
    -> perm_left:Name_permutation.t
    -> perm_right:Name_permutation.t
    -> t

  val print : Format.formatter -> t -> unit

  val env : t -> Typing_env.t

  val perm_left : t -> Name_permutation.t

  val perm_right : t -> Name_permutation.t

  (** Note that we are now in the process of meeting the given two
      [Simple]s. *)
  val now_meeting : t -> Simple.t -> Simple.t -> t

  (** Determine whether we are now in the process of meeting the given two
      [Simple]s.  The arguments do not have to be provided in the same order
      as when [now_meeting] was called. *)
  val already_meeting : t -> Simple.t -> Simple.t -> bool

  val shortcut_precondition : t -> bool

  val with_env : t -> (Typing_env.t -> Typing_env.t) -> t

  val clear_name_permutations : t -> t

  val compose_name_permutations
     : t
    -> perm_left:Name_permutation.t
    -> perm_right:Name_permutation.t
    -> t
end
