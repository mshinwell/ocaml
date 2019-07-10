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
    in order to perform conditional control flow. *)

module Kind : sig
  type t =
    | Int
    | Tag
    | Is_int

  include Identifiable.S with type t := t

  val to_lowercase_string : t -> string
end

include Identifiable.S

val create : Kind.t -> Targetint.OCaml.t -> t option
val create_exn : Kind.t -> Targetint.OCaml.t -> t

val of_int_exn : Kind.t -> int -> t

val of_tag : Tag.t -> t
val to_tag : t -> Tag.t option

val to_int : t -> Targetint.OCaml.t

val zero : t

val kind : t -> Kind.t

val bool_false : t
val bool_true : t

val all_bools_set : Set.t
