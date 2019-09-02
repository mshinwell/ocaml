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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type t = private
  | Var of Variable.t
  | Symbol of Symbol.t
(* CR mshinwell: Phantom variables should be in here now.
  | Phantom_var of Variable.t
*)

val var : Variable.t -> t
val symbol : Symbol.t -> t

val map_var : t -> f:(Variable.t -> Variable.t) -> t

val map_symbol : t -> f:(Symbol.t -> Symbol.t) -> t

val to_var : t -> Variable.t option

include Identifiable.S with type t := t

val print_sexp : Format.formatter -> t -> unit

val variables_only : Set.t -> Set.t

val symbols_only_map : 'a Map.t -> 'a Map.t

val set_of_var_set : Variable.Set.t -> Set.t

val set_of_symbol_set : Symbol.Set.t -> Set.t

val set_to_var_set : Set.t -> Variable.Set.t

val set_to_symbol_set : Set.t -> Symbol.Set.t

val is_predefined_exception : t -> bool

val rename : t -> t

val in_compilation_unit : t -> Compilation_unit.t -> bool

module Pair : sig
  type nonrec t = t * t

  include Identifiable.S with type t := t
end
