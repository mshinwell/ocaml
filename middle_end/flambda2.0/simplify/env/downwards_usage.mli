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

val record_use_of_variable : t -> Variable.t -> t

val record_definition
   : t
  -> var_being_defined:Variable.t
  -> uses_in_defining_expr:Variable.Set.t
  -> t

val used_variables : t -> Variable.Set.t

(* Notes on unboxing dataflow analysis

0. Split critical edges
- When recording a continuation's use, record which continuation the use
  lies in.
- When ready to simplify continuation handler [k1], look at the uses.
- Stop if there are <=1 use(s).
- Otherwise (>1 uses), find which continuation [k2] each use lies in,
  and then find out how many free continuations are in its body (beware of
  recursive ones). The continuation(s) [k2] must always be in scope.
- For each [k2], if [k2] has more than one free continuation in its body,
  then the Apply_cont (or equivalent) of [k1] in [k2] must instead be
  redirected to a new continuation that starts out just calling [k1].

1. Anticipated expressions

Boxed versions that are required on all paths from current point to return cont.

Domain: Set of (Flambda_kind.Boxable_number.t * Simple.t)

Direction: backwards

Function: Uses of boxed versions in current continuation (since expressions can
never be killed) minus uses of parameters of the continuation. Presumably when
we record a use, we should also record a summary of the type, in particular
whether it's a boxed number.

Meet operator: set intersection

Initial value: empty set

2. Early placement

Place boxing operations at the earliest point anticipated (needed on all
subsequent paths) but not already available.

Domain: Set of (Variable.t * Flambda_kind.Boxable_number.t * Simple.t)
Unclear -- need to distinguish:
- boxing operations added by this analysis
- pre-existing boxed values
- uses of boxed values

Direction:

Function:

Meet operator:

3. Postponement

Find which inserted boxing operations can be moved later.

Domain:

Direction:

Function:

Meet operator:

4. Calculation of latest position

Move the inserted boxing operations down until the latest possible point.

Domain:

Direction:

Function:

Meet operator:

The existing continuation unboxing pass will provide the unboxed arguments
down the control flow.

We could consider tracking of the unboxed uses as well in the future.

*)
