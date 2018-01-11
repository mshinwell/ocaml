(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** A "name" in Flambda identifies either a variable, symbol or field of a
    symbol.  Much of the same logic applies to all of these concepts. *)

type t = private
  | Var of Variable.t
  | Symbol of Symbol.t
  | Symbol_field of Symbol.t * Targetint.OCaml.t

val var : Variable.t -> t
val symbol : Symbol.t -> t
val symbol_field : Symbol.t -> field:Targetint.OCaml.t -> t

val map_var : t -> f:(Variable.t -> Variable.t) -> t

val map_symbol : t -> f:(Symbol.t -> Symbol.t) -> t

val to_var : t -> Variable.t option

include Identifiable.S with type t := t

val set_to_var_set : Set.t -> Variable.Set.t

val set_to_symbol_set : Set.t -> Symbol.Set.t
