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

(** Simplification of Flambda programs: inlining, specialisation,
    unboxing and so forth.

    Readers interested in the function inlining strategy should read the
    [Inlining_decision] module first.
*)
val run
   : never_inline:bool
  -> allow_continuation_inlining:bool
  -> allow_continuation_specialisation:bool
  -> backend:(module Backend_intf.S)
  -> prefixname:string
  -> round:int
  -> Flambda_static.Program.t
  -> Flambda_static.Program.t
