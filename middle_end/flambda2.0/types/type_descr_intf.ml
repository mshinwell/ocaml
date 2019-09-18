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

module type S_ops = sig
  type head
  type descr
  type typing_env
  type typing_env_extension
  type meet_env

  val expand_head : descr -> typing_env -> head Or_unknown_or_bottom.t

  module Make_meet_or_join (E : Lattice_ops_intf.S
    with type meet_env := meet_env
    with type typing_env := typing_env
    with type typing_env_extension := typing_env_extension)
  : sig
    val meet_or_join
       : meet_env
      -> descr
      -> descr
      -> (descr * typing_env_extension) Or_bottom.t
  end
end

module type S0 = sig
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

  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

  val print : Format.formatter -> t -> unit

  val create_no_alias : head Or_unknown_or_bottom.t -> t
  val create_equals : Simple.t -> t
  val create_type : Export_id.t -> t

  val create : head -> t

  val unknown : unit -> t
  val bottom : unit -> t

  val descr : t -> Descr.t

  (* CR mshinwell: Try to use [Type_structure_intf] or similar *)

  include Contains_names.S with type t := t

  val apply_rec_info : t -> Rec_info.t -> t Or_bottom.t
end

module type S = sig
  type flambda_type
  type typing_env
  type typing_env_extension
  type typing_env_level
  type meet_env
  type head

  include S0 with type head = head

  val get_alias : t -> Simple.t option

  val is_obviously_bottom : t -> bool

  val make_suitable_for_environment0
     : t
    -> typing_env
    -> suitable_for:typing_env
    -> typing_env_level
    -> typing_env_level * t

  val make_suitable_for_environment
     : t
    -> typing_env
    -> suitable_for:typing_env
    -> typing_env_extension * t

  module Make_operations (S : sig
    val print : Format.formatter -> flambda_type -> unit
    val force_to_kind : flambda_type -> t
    val to_type : t -> flambda_type
  end) : S_ops
    with type head := head
    with type descr := t
    with type typing_env := typing_env
    with type typing_env_extension := typing_env_extension
    with type meet_env := meet_env
end
