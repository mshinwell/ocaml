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

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

(** Describe one or more closures by giving for each one the closure ID
    and the set of variables in the closure. *)
val create_exactly_multiple
   : Flambda_types.closures_entry
       Closure_id_and_var_within_closure_set.Map.t
  -> t

(** Describe one or more closures that contain at least the given closure
    variables. *)
val create_at_least_multiple
   : Flambda_types.closures_entry Var_within_closure_set.Map.t
  -> t

include Type_structure_intf.S with type t := t
