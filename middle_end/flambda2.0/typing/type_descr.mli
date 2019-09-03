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

(* Descriptions of types of a particular kind. *)

(*
  spec_intf:
  type flambda_type
  type 'a ty
  type meet_env
  type typing_env_extension
  type of_kind_foo

  val kind : Flambda_kind.t

  val to_type : of_kind_foo ty -> flambda_type

  val force_to_kind : flambda_type -> of_kind_foo ty

  (* CR mshinwell: Rename to [print_ty_with_cache]. *)
  val print_ty
     : cache:Printing_cache.t
    -> Format.formatter
    -> of_kind_foo ty
    -> unit

  val apply_rec_info : of_kind_foo -> Rec_info.t -> of_kind_foo Or_bottom.t

  val meet_or_join_of_kind_foo
     : meet_env
    -> meet_or_join_ty:
         (?bound_name:Name.t
      -> meet_env
      -> of_kind_foo ty
      -> of_kind_foo ty
      -> (of_kind_foo ty * typing_env_extension) Or_bottom.t)
    -> of_kind_foo
    -> of_kind_foo
    -> (of_kind_foo * typing_env_extension) Or_bottom_or_absorbing.t
*)

module Make (Head : sig
  include Contains_names.S

  module Make_meet_or_join (E : Lattice_ops_intf.S) : sig
    val meet_or_join
       : E.meet_env
      -> t
      -> t
      -> (t * E.typing_env_extension) Or_bottom_or_absorbing.t
  end

  val erase_aliases : t -> allowed:Variable.Set.t -> t

  val apply_rec_info : t -> Rec_info.t -> t Or_bottom.t
end) : sig
  type descr = private
    | No_alias of Head.t Or_unknown_or_bottom.t
      (** For each kind there is a lattice of types.
          Unknown = "Any value can flow to this point": the top element.
          Bottom = "No value can flow to this point": the least element.
      *)
    | Equals of Simple.t
    | Type of Export_id.t

  type t

  val create_no_alias : Head.t Or_unknown_or_bottom.t -> t
  val create_equals : Simple.t -> t
  val create_type : Export_id.t -> t

  val descr : t -> descr

  (* CR mshinwell: Try to use [Type_structure_intf] or similar *)

  include Contains_names.S with type t := t

  val erase_aliases : t -> allowed:Variable.Set.t -> t

  val apply_rec_info : t -> Rec_info.t -> t Or_bottom.t
end
