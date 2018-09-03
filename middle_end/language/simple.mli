(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** A value that is known to fit into a register (of the appropriate kind)
    on the target machine.  We do not require such values to be [Let]-bound. *)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Const : sig
  type t =
    | Untagged_immediate of Immediate.t
    | Tagged_immediate of Immediate.t
    | Naked_float of Numbers.Float_by_bit_pattern.t
    | Naked_int32 of Int32.t
    | Naked_int64 of Int64.t
    | Naked_nativeint of Targetint.t

  include Hashtbl.With_map with type t := t

  val kind : t -> Flambda_kind.t
end

(* CR-someday mshinwell: Consider putting [Var] and [Symbol] directly
   in here. *)
type t = private
  | Name of Name.t
  | Const of Const.t
  | Discriminant of Discriminant.t

val name : Name.t -> t

val var : Variable.t -> t

val symbol : Symbol.t -> t

val const : Const.t -> t

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
val unit : t

(** The given switch discriminant as a simple. *)
val discriminant : Discriminant.t -> t

val map_name : t -> f:(Name.t -> Name.t) -> t

(* CR mshinwell: remove these next two? *)
val map_var : t -> f:(Variable.t -> Variable.t) -> t
val map_symbol : t -> f:(Symbol.t -> Symbol.t) -> t

include Contains_names.S with type t := t
include Hashtbl.With_map with type t := t

module List : sig
  type nonrec t = t list

  include Contains_names.S with type t := t
  include Hashtbl.With_map with type t := t
end

module With_kind : sig
  type nonrec t = t * Flambda_kind.t

  include Contains_names.S with type t := t
  include Hashtbl.With_map with type t := t
end
