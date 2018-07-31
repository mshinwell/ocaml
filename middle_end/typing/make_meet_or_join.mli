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

(** Definitions of those parts of the meet and join operations on types
    which are common across all of the different kinds of types. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make (T : Typing_world.S)
    (E : Either_meet_or_join_intf.S with module T := T)
    (S : Meet_and_join_spec_intf.S with module T := T) : sig

end
