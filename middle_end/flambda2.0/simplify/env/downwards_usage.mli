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

(** Tracking of variable usage during the downwards pass of simplification.
    This information can be used e.g. to remove unused parameters of recursive
    continuations. *)

[@@@ocaml.warning "+a-30-40-41-42"]

type t

val empty : t

val print : Format.formatter -> t -> unit

val record_use_of_variable
   : t
  -> ?var_being_defined:Variable.t
  -> Variable.Set.t
  -> t

val record_uses_of_variables
   : t
  -> ?var_being_defined:Variable.t
  -> Variable.Set.t
  -> t

val used_variables : t -> Variable.Set.t
