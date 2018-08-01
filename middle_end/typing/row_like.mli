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
    (W : Typing_world.S)
    (Tag : sig
      type t
      include Hashtbl.With_map with type t := t
      include Contains_names.S with type t := t
    end)
    (Index : sig
      type t

      include Hashtbl.With_map with type t := t
      include Contains_names.S with type t := t
    end)
    (Maps_to : sig
      type t

      val print_with_cache
         : cache:Printing_cache.t
        -> Format.formatter
        -> t
        -> unit

      val add_or_meet_equations
        : t
        -> W.Typing_env.t
        -> W.Typing_env_extension.t
        -> t

      include Contains_names.S with type t := t
    end) :
  Row_like_intf.S
    with module Join_env := W.Join_env
    with module Typing_env := W.Typing_env
    with module Typing_env_extension := W.Typing_env_extension
