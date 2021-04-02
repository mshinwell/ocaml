(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

(** Symbols that identify statically-allocated code or data. *)

type t

(** It is assumed that the provided [Ident.t] is in the current unit unless
    it is [Global] or [Predef]. *)
val for_ident : Ident.t -> t

val for_current_unit : unit -> t
val for_new_const_in_current_unit : unit -> t

(** To be used for "entry" functions, etc. *)
val for_fixed_name : Compilation_unit.t -> name:string -> t

module Flambda : sig
  val for_variable : Variable.t -> t
  val for_closure : Closure_id.t -> t
  val for_code_of_closure : Closure_id.t -> t
end

val compilation_unit : t -> Compilation_unit.t
val linkage_name : t -> string

include Identifiable.S with type t := t

val is_predef_exn : t -> bool
