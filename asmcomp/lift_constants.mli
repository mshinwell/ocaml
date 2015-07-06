(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(** Determine which variables and set-of-closures IDs in the given
    expression are known to be constant.  Then lift them, along with
    certain varieties of [Fconst] nodes, as defining expressions into a
    table indexed by freshly-generated symbols.  The returned Flambda
    expression contains [Fsymbol] references to those symbols instead of
    the original defining expressions. *)
val lift_constants
   : _ Flambda.t
  -> unit Flambda.t * (unit Flambda.t Symbol.Table.t)
