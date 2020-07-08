(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2019--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

(** Things that a [Let]-expression binds. *)

module Symbol_scoping_rule : sig
  type t =
    | Syntactic
    | Dominator
end

type t = private
  | Singleton of Var_in_binding_pos.t
    (** The binding of a single variable, which is statically scoped. *)
  | Set_of_closures of {
      name_mode : Name_mode.t;
      closure_vars : Var_in_binding_pos.t Closure_id.Map.t;
    }
    (** The binding of one or more variables to the individual closures in a
        set of closures.  The variables are statically scoped. *)
  | Symbols of {
      scoping_rule : Symbol_scoping_rule.t;
      bound_symbols : Bound_symbols.t;
    }
    (** The binding of one or more symbols to a statically-allocated constant.
        The scoping of the symbols may either be syntactic, or follow the
        dominator tree. *)

include Bindable.S with type t := t

include Contains_ids.S with type t := t

val singleton : Var_in_binding_pos.t -> t

val set_of_closures : closure_vars:Var_in_binding_pos.t Closure_id.Map.t -> t

val must_be_singleton : t -> Var_in_binding_pos.t

val must_be_set_of_closures : t -> Var_in_binding_pos.t Closure_id.Map.t

val name_mode : t -> Name_mode.t

val all_bound_vars : t -> Var_in_binding_pos.Set.t

val all_bound_vars' : t -> Variable.Set.t
