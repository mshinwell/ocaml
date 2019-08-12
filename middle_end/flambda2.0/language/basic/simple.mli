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

(** A value that is known to fit into a register (of the appropriate kind)
    on the target machine.  We do not require such values to be [Let]-bound. *)

[@@@ocaml.warning "+a-30-40-41-42"]

type t

val name : Name.t -> t

val var : Variable.t -> t

val vars : Variable.t list -> t list

val symbol : Symbol.t -> t

val const : Reg_width_const.t -> t

(** The constant representating the given number of type "int". *)
val const_int : Targetint.OCaml.t -> t

(** The constant representating the given boolean value. *)
val const_bool : bool -> t

(** The constant representating boolean true. *)
val const_true : t

(** The constant representating boolean false. *)
val const_false : t

(** The constant representating the number zero of type "int". *)
val const_zero : t

(** The constant representing the unit value. *)
val const_unit : t

(** The given switch discriminant as a simple. *)
val discriminant : Discriminant.t -> t

val add_rec_info : t -> newer_rec_info:Rec_info_sequence.Entry.t -> t

val rec_info : t -> Rec_info_sequence.t

val without_rec_info : t -> t

(* CR mshinwell: inconsistent naming *)
val must_be_var : t -> Variable.t option

val map_name : t -> f:(Name.t -> Name.t) -> t

val to_name : t -> Name.t option

(* CR mshinwell: remove these next two? *)
val map_var : t -> f:(Variable.t -> Variable.t) -> t
val map_symbol : t -> f:(Symbol.t -> Symbol.t) -> t

val is_symbol : t -> bool

val is_var : t -> bool

include Contains_names.S with type t := t

val free_names_in_types : t -> Name_occurrences.t

val allowed : t -> allowed:Variable.Set.t -> bool 

type descr = private
  | Name of Name.t
  | Const of Reg_width_const.t
  | Discriminant of Discriminant.t

val descr : t -> descr

val of_descr : descr -> t

include Identifiable.S with type t := t

module List : sig
  type nonrec t = t list

  include Contains_names.S with type t := t
  include Identifiable.S with type t := t
end

module Pair : sig
  type nonrec t = t * t

  include Identifiable.S with type t := t
end

module With_kind : sig
  type nonrec t = t * Flambda_kind.t

  include Contains_names.S with type t := t
  include Identifiable.S with type t := t
end
