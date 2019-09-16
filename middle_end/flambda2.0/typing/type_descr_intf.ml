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

module type S = sig
  type flambda_type
  type typing_env
  type typing_env_extension
  type meet_env
  type head

  module Descr : sig
    type t = private
      | No_alias of head Or_unknown_or_bottom.t
        (** For each kind there is a lattice of types.
            Unknown = "Any value can flow to this point": the top element.
            Bottom = "No value can flow to this point": the least element.
        *)
      | Equals of Simple.t
      | Type of Export_id.t
  end

  type t

  val create_no_alias : head Or_unknown_or_bottom.t -> t
  val create_equals : Simple.t -> t
  val create_type : Export_id.t -> t

  val create : head -> t

  val unknown : t
  val bottom : t

  val descr : t -> Descr.t

  val get_alias : t -> Simple.t option

  val is_obviously_bottom : t -> bool

  (* CR mshinwell: Try to use [Type_structure_intf] or similar *)

  include Contains_names.S with type t := t

  val apply_rec_info : t -> Rec_info.t -> t Or_bottom.t

  val make_suitable_for_environment
     : t
    -> typing_env
    -> suitable_for:typing_env
    -> t * typing_env

  module Make_operations (S : sig
    val force_to_kind : flambda_type -> t
    val to_type : t -> flambda_type
  end) : sig
    val expand_head : t -> typing_env -> head Or_unknown_or_bottom.t

    module Make_meet_or_join (E : Lattice_ops_intf.S
      with type meet_env = meet_env
      with type typing_env_extension = typing_env_extension)
    : sig
      val meet_or_join
         : meet_env
        -> t
        -> t
        -> (t * typing_env_extension) Or_bottom_or_absorbing.t
    end
  end
end
