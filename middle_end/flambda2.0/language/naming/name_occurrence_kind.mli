(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2019 OCamlPro SAS                                          *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** A description of the different contexts in which names may occur. *)

type t
type kind = t

val normal : t

(** A name that does not occur in terms (but may be required for the
    generation of debugging information), but does occur in types. *)
val in_types : t

(** A name that neither occurs in names nor types, but is required for the
    generation of debugging information. *)
val phantom : t

include Identifiable.S with type t := t

module Or_absent : sig
  type t = private
    | Absent
    | Present of kind

  val absent : t
  val present : kind -> t

  include Identifiable.S with type t := t
end
