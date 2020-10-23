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

open! Flambda.Import

type t = private
  | Reachable of {
      named : Named.t;
      free_names : Name_occurrences.t;
    }
  | Invalid of Invalid_term_semantics.t

val reachable : Named.t -> t

val reachable_with_known_free_names
   : Named.t
  -> free_names:Name_occurrences.t
  -> t

val invalid : unit -> t

val is_invalid : t -> bool

val print : Format.formatter -> t -> unit
