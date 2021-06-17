(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2020 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Calling_convention : sig
  type t

  val print : Format.formatter -> t -> unit

  val needs_closure_arg : t -> bool
  val params_arity : t -> Flambda_arity.t
end

type t

include Contains_ids.S with type t := t

val apply_renaming
   : Code_id.t Code_id.Map.t
  -> Renaming.t
  -> t
  -> t

val print : Format.formatter -> t -> unit

val empty : t

val add_code : Flambda.Code.t Code_id.Map.t -> t -> t

val mark_as_imported : t -> t

val merge : t -> t -> t

val mem : Code_id.t -> t -> bool

val find_code : t -> Code_id.t -> Flambda.Code.t

val find_code_if_not_imported : t -> Code_id.t -> Flambda.Code.t option

val find_calling_convention : t -> Code_id.t -> Calling_convention.t

val remove_unreachable : t -> reachable_names:Name_occurrences.t -> t

val iter : t -> (Code_id.t -> Flambda.Code.t -> unit) -> unit

val fold
   : t
  -> init:'a
  -> f:('a -> Code_id.t -> Flambda.Code.t -> 'a)
  -> 'a

val fold_for_cmx
   : t
  -> init:'a
  -> f:('a -> Code_id.t -> Obj.t -> 'a)
  -> 'a

(** This function must be called _after_ all code from the value of type [t]
    has been marshalled to the .cmx file (e.g. by using [fold]).
    The returned value of type [t] should not be altered before it is
    marshalled to the .cmx file. *)
val prepare_for_cmx_header_section : t -> t

val associate_with_loaded_cmx_file
   : t
  -> read_flambda_section_from_cmx_file:(index:int -> Obj.t)
  -> code_sections_map:int Code_id.Map.t
  -> used_closure_vars:Var_within_closure.Set.t
  -> t
