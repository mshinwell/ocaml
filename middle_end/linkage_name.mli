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

include Identifiable.S

val to_string : t -> string

val create : string -> t

val for_compilation_unit : Compilation_unit.t -> t
val for_current_unit : unit -> t

val for_new_const_in_current_unit : unit -> t

val for_ident : ?comp_unit:Compilation_unit.t -> Ident.t -> t

val is_in_current_unit : t -> bool

val caml_symbol_prefix : string
