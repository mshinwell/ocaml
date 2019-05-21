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

module Make
  (Tag : Identifiable.S)
  (Index : Identifiable.S)
  (Tag_and_index : sig
    (** These values will not contain any names. *)
    type t = Tag.t * Index.t
    include Identifiable.S with type t := t
  end)
  (Maps_to : Row_like_maps_to_intf.S
    with type flambda_type := Flambda_types.t
    with type typing_env := Typing_env.t
    with type meet_env := Meet_env.t
    with type type_equality_env := Type_equality_env.t
    with type typing_env_extension := Typing_env_extension.t) :
sig
  type t

  val create_bottom : unit -> t

  val create_unknown : unit -> t

  val create_exactly : Tag.t -> Index.t -> Maps_to.t -> t

  val create_exactly_multiple : Maps_to.t Tag_and_index.Map.t -> t

  val create_at_least : Index.t -> Maps_to.t -> t

  val create_at_least_multiple : Maps_to.t Index.Map.t -> t

  val is_bottom : t -> bool

  val known : t -> Maps_to.t Tag_and_index.Map.t Or_unknown.t

  val at_least : t -> Maps_to.t Index.Map.t Or_unknown.t

  val get_singleton : t -> (Tag_and_index.t * Maps_to.t) option

  val classify : t -> unit Or_unknown_or_bottom.t

  (** The [Maps_to] value which [meet] returns contains the join of all
      [Maps_to] values in the range of the row-like structure after the meet
      operation has been completed. *)
  include Type_structure_intf.S
    with type t := t
    with type flambda_type := Flambda_types.t
    with type typing_env := Typing_env.t
    with type meet_env := Meet_env.t
    with type type_equality_env := Type_equality_env.t
    with type typing_env_extension := Typing_env_extension.t
end
