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

[@@@ocaml.warning "+a-30-40-41-42"]

(** Descriptions of types of a particular kind. *)

module Make (Head : sig
  include Contains_names.S

  module Make_meet_or_join (E : Lattice_ops_intf.S
    with type meet_env = Meet_env.t
    with type typing_env_extension = Typing_env_extension.t)
  : sig
    val meet_or_join
       : Meet_env.t
      -> t
      -> t
      -> (t * Typing_env_extension.t) Or_bottom_or_absorbing.t
  end

  val force_to_kind : Type_grammar.t -> t
  val erase_aliases : t -> allowed:Variable.Set.t -> t
  val apply_rec_info : t -> Rec_info.t -> t Or_bottom.t
end) : sig
  module Descr : sig
    type t = private
      | No_alias of Head.t Or_unknown_or_bottom.t
        (** For each kind there is a lattice of types.
            Unknown = "Any value can flow to this point": the top element.
            Bottom = "No value can flow to this point": the least element.
        *)
      | Equals of Simple.t
      | Type of Export_id.t
  end

  type t

  val create_no_alias : Head.t Or_unknown_or_bottom.t -> t
  val create_equals : Simple.t -> t
  val create_type : Export_id.t -> t

  val create : Head.t -> t

  val unknown : t
  val bottom : t

  val descr : t -> Descr.t

  val get_alias : t -> Simple.t option

  (* CR mshinwell: Try to use [Type_structure_intf] or similar *)

  include Contains_names.S with type t := t

  val erase_aliases : t -> allowed:Variable.Set.t -> t

  val apply_rec_info : t -> Rec_info.t -> t Or_bottom.t

  val is_obviously_bottom : t -> bool

  val meet : Meet_env.t -> t -> t -> t Or_bottom.t

  val join : ?bound_name:Name.t -> Meet_env.t -> t -> t -> t Or_bottom.t

  val make_suitable_for_environment
     : t
    -> Typing_env.t
    -> suitable_for:Typing_env.t
    -> t * Typing_env.t

  val expand_head : t -> Typing_env.t -> ???
end
