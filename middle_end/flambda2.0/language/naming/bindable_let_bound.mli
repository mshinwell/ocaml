(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Things that a [Let]-expression binds. *)

type t

include Bindable.S with type t := t

val singleton : Var_in_binding_pos.t -> t

val set_of_closures : closure_vars:Var_in_binding_pos.t Closure_id.Map.t -> t

val must_be_singleton : t -> Var_in_binding_pos.t

val must_be_set_of_closures : t -> Var_in_binding_pos.t Closure_id.Map.t

val name_occurrence_kind : t -> Name_occurrence_kind.t

val all_bound_vars : t -> Var_in_binding_pos.Set.t
