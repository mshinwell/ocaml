(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016--2018 OCamlPro SAS                                    *)
(*   Copyright 2016--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Conversion from Lambda to Ilambda: halfway to Flambda. *)

(** The set of integers identifies those [Lstaticcatch] constructs to be
    treated as recursive. *)
val lambda_to_ilambda : Lambda.lambda * Numbers.Int.Set.t -> Ilambda.program
