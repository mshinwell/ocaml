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

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

val create : unit -> t

val freshen_continuation : t -> Continuation.t -> Continuation.t * t

val apply_continuation : t -> Continuation.t -> Continuation.t

val freshen_kinded_parameter
   : t
  -> Kinded_parameter.t
  -> Kinded_parameter.t * t

val freshen_kinded_parameters
   : t
  -> Kinded_parameter.t list
  -> Kinded_parameter.t list * t

val apply_kinded_parameter
   : t
  -> Kinded_parameter.t
  -> Kinded_parameter.t

val apply_kinded_parameters
   : t
  -> Kinded_parameter.t list
  -> Kinded_parameter.t list

val freshen_name
   : t
  -> Name.t
  -> Name.t * t

val apply_name : t -> Name.t -> Name.t

val apply_simple : t -> Simple.t -> Simple.t

val freshen_symbol
   : t
  -> Symbol.t
  -> Symbol.t * t

val apply_symbol : t -> Symbol.t -> Symbol.t

val freshen_variable
   : t
  -> Variable.t
  -> Variable.t * t

val apply_variable : t -> Variable.t -> Variable.t

val name_permutation : t -> Name_permutation.t
