(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Like [Static_const.t], but also keeps track of free names, as required
    at various points during simplification. *)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Flambda.Import

type t
type const_wfn = t

val print : Format.formatter -> t -> unit

(** Free names must be provided except for [Code] constants, which already
    hold the necessary information. *)
(* CR mshinwell: Maybe instead of [Or_unknown] we should use
   | Code
   | Known of Name_occurrences.t
   | Compute_now
   or something *)
val create : Static_const.t -> free_names:Name_occurrences.t Or_unknown.t -> t

val const : t -> Static_const.t

val free_names : t -> Name_occurrences.t

val is_fully_static : t -> bool

module Group : sig
  type t

  val print : Format.formatter -> t -> unit

  val empty : t

  val create : const_wfn list -> t

  val free_names : t -> Name_occurrences.t

  val consts : t -> Static_const.Group.t

  val to_list : t -> const_wfn list

  (** This function ignores [Deleted] code. *)
  val pieces_of_code : t -> Code.t Code_id.Map.t

  val match_against_bound_symbols
     : t
    -> Bound_symbols.t
    -> init:'a
    -> code:('a -> Code_id.t -> Code.t -> 'a)
    -> set_of_closures:(
         'a
      -> closure_symbols:Symbol.t Closure_id.Lmap.t
      -> Set_of_closures.t
      -> 'a)
    -> block_like:('a -> Symbol.t -> Static_const.t -> 'a)
    -> 'a

  val map : t -> f:(const_wfn -> const_wfn) -> t

  val concat : t -> t -> t
end
