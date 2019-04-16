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

module type S = sig
  module Thing_without_names : Identifiable.S

  type t

  (** Create a value which describes the presence of exactly no things. *)
  val create_bottom : unit -> t

  (** Create a value which describes the presence of exactly the given
      things. *)
  val create : Thing_without_names.Set.t -> t

  val is_bottom : t -> bool

  val all : t -> Thing_without_names.Set.t Or_unknown.t

  val get_singleton : t -> Thing_without_names.t option

  include Type_structure_intf.S with type t := t
end
