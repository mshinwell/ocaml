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

(** Discriminants: values that are analysed by Flambda [Switch] statements
    in order to perform conditional control flow.  They form an abstraction
    around constant constructor indexes and block tags. *)

include Hashtbl.With_map

val create : Targetint.OCaml.t -> t option
val create_exn : Targetint.OCaml.t -> t

val of_int_exn : int -> t

val of_tag : Tag.t -> t
val to_tag : t -> Tag.t option

val to_int : t -> Targetint.OCaml.t

val zero : t

val bool_false : t
val bool_true : t
