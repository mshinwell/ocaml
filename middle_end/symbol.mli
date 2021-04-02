(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** A symbol identifies a constant provided by either:
    - another compilation unit; or
    - a top-level module.

    * [sym_unit] is the compilation unit containing the value.
    * [sym_label] is the linkage name of the variable.

    The label must be globally unique: two compilation units linked in the
    same program must not share labels. *)

include Identifiable.S

(** It is assumed that the provided [Ident.t] is in the current unit unless
    it is [Global] or [Predef]. *)
val for_ident : Ident.t -> t

val for_variable : Variable.t -> t
val for_closure : Closure_id.t -> t
val for_code_of_closure : Closure_id.t -> t

(* Create the symbol without prefixing with the compilation unit.
   Used for global symbols like predefined exceptions *)
val of_global_linkage : Compilation_unit.t -> Linkage_name.t -> t

val compilation_unit : t -> Compilation_unit.t
val linkage_name : t -> Linkage_name.t

val print_opt : Format.formatter -> t option -> unit

val compare_lists : t list -> t list -> int
