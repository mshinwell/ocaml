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

val print_with_cache
   : cache:Printing_cache.t
  -> Format.formatter
  -> t
  -> unit

val print : Format.formatter -> t -> unit

val invariant : t -> unit

val empty : t

val is_empty : t -> bool

val create : Typing_env_level.t -> t

val to_level : t -> Typing_env_level.t

val one_equation : Name.t -> Flambda_types.t -> t

val add_or_replace_equation : t -> Name.t -> Flambda_types.t -> t

val meet : Meet_env.t -> t -> t -> t

val join : Typing_env.t -> t -> t -> t

val mem : t -> Name.t -> bool
