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

(** Union-find-like structure for keeping track of equivalence classes of
    names related by [Equals] types. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

val print : Format.formatter -> t -> unit

val invariant : t -> unit

val empty : t

val add_canonical_name : t -> Name.t -> t

type add_result = private {
  canonical_name : Name.t;
  alias_of : Name.t;
}

val add
   : t
  -> Simple.t
  -> Simple.t
  -> defined_earlier:(Simple.t -> than:Simple.t -> bool)
  -> add_result option * t

val get_canonical_name : t -> Name.t -> Name.t option

val aliases_of_simple : t -> Simple.t -> Name.Set.t

val canonical_names : t -> Name.Set.t
