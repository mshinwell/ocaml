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

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

(*
val create
   : typing_env_left:Typing_env.t
  -> typing_env_right:Typing_env.t
  -> perm_left:Name_permutation.t
  -> perm_right:Name_permutation.t
  -> t
*)

val empty
   : typing_env_left:Typing_env.t
  -> typing_env_right:Typing_env.t
  -> t

(*
val print : Format.formatter -> t -> unit
*)

val typing_env_left : t -> Typing_env.t

val typing_env_right : t -> Typing_env.t

val replace_typing_environments
   : t
  -> left:Typing_env.t
  -> right:Typing_env.t
  -> t

val add_definition_typing_env_left
   : t
  -> Name.t
  -> Flambda_kind.t
  -> t

val add_definition_typing_env_right
   : t
  -> Name.t
  -> Flambda_kind.t
  -> t

val perm_left : t -> Name_permutation.t

val perm_right : t -> Name_permutation.t

(*
val shortcut_precondition : t -> bool
*)

val compose_name_permutations
   : t
  -> perm_left:Name_permutation.t
  -> perm_right:Name_permutation.t
  -> t
