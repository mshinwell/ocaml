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

module Make
    (T : Typing_world.S)
    (Tag : sig
      type t
      include Contains_names.S with type t := t
    end)
    (Index : sig
      type t

      val equal : t -> t -> bool
      val compare : t -> t -> int

      include Map.With_set with type t := t
      include Contains_names.S with type t := t
    end)
    (Maps_to : sig
      type t

      val add_or_meet_equations
        : t
        -> Typing_env.t
        -> Typing_env_extension.t
        -> t

      include Contains_names.S with type t := t
    end) :
  Row_like_intf.S
    with module Flambda_type := T.Flambda_type
    with module Typing_env := T.Typing_env
    with module Join_env := T.Join_env
