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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** Continuation variables. *)

include Identifiable.S

type sort =
  | Normal
  | Return
  | Toplevel_return
  | Exn

val create : ?sort:sort -> unit -> t

val to_int : t -> int

val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

val sort : t -> sort

val is_exn : t -> bool

module With_args : sig
  type nonrec t = t * Variable.t list
  include Identifiable.S with type t := t
end
