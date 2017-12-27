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

  include Identifiable.S with type t := t

  val kind : t -> Flambda_kind.t
end

(* CR mshinwell: Consider putting [Var] and [Symbol] directly in here. *)
type t = private
  | Name of Name.t
  | Const of Const.t

val name : Name.t -> t

val var : Variable.t -> t

val symbol : Symbol.t -> t

val const : Const.t -> t

val const_bool : bool -> t

val const_true : t

val const_false : t

val free_names : t -> Name.Set.t

val map_var : t -> f:(Variable.t -> Variable.t) -> t

val map_symbol : t -> f:(Symbol.t -> Symbol.t) -> t

include Identifiable.S with type t := t

module List : sig
  type nonrec t = t list

  val free_names : t -> Name.Set.t

  include Identifiable.S with type t := t
end
