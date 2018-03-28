(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017 OCamlPro SAS                                          *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Placement : sig
  type t =
    | After_let of Variable.t
    | After_let_cont of Continuation.Set.t
    | Just_inside_continuation of Continuation.t

  include Hashtbl.With_map with type t := t
end

(** This function accepts an expression and a map [new_conts] associating sets
    of new continuation handlers (which do not appear in the expression) with
    definitions of continuations that do appear in the expression.  The sets of
    new continuations mapped to by a given continuation [k] in [new_conts] may
    reference free variables that are not in scope at the point of definition
    of [k].  The function identifies, for each new set of continuation handlers,
    the earliest point in the expression (but not earlier than the definition
    of each [k]) at which a [Let_cont] defining such continuations may be
    inserted without its free variables being out of scope.

    This function is used when specialising continuations: a specialised
    continuation may reference variables not in scope at the time of definition
    of the continuation; and there may be multiple places from which a
    given specialised continuation must be referenced.

    [vars_in_scope] must specify all variables in scope around [expr].
*)
val find_insertion_points
   : Flambda.Expr.t
  -> vars_in_scope:Variable.Set.t
  -> new_conts:Flambda.Let_cont_handlers.t list Continuation.Map.t
  -> Flambda.Let_cont_handlers.t list Placement.Map.t
