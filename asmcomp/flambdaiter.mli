(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Abstract_identifiers

val apply_on_subexpressions : ('a Flambda.t -> unit) ->
  'a Flambda.t -> unit

val subexpressions : 'a Flambda.t -> 'a Flambda.t list

val iter : f:('a Flambda.t -> unit) -> 'a Flambda.t -> unit

val iter_toplevel : ('a Flambda.t -> unit) -> 'a Flambda.t -> unit
(** [iter_toplevel f t] Apply f on every toplevel subexpression of t,
    i.e. does not apply it on functions body *)

val iter_on_closures :
  ('a fset_of_closures -> 'a -> unit) -> 'a Flambda.t -> unit

val map : ('a Flambda.t -> 'a Flambda.t) ->
  'a Flambda.t -> 'a Flambda.t

val map_toplevel : ('a Flambda.t -> 'a Flambda.t) ->
  'a Flambda.t -> 'a Flambda.t

val free_variables : 'a Flambda.t -> Variable.Set.t

val fold_subexpressions :
  ('acc -> Variable.Set.t -> 'a Flambda.t -> 'acc * 'a Flambda.t) -> 'acc -> 'a Flambda.t ->
  'acc * 'a Flambda.t

val expression_free_variables : 'a Flambda.t -> Variable.Set.t

val subexpression_bound_variables : 'a Flambda.t -> (Variable.Set.t*'a Flambda.t) list

val map_data : ('a -> 'b) -> 'a Flambda.t -> 'b Flambda.t
