(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(* CR lwhite: "Closure_id" is quite a generic name.  I wonder
   whether something like "Closure_label" would better capture that it is
   the label of a projection. *)

(* CR mshinwell: update comment *)
(** An identifier, unique across the whole program (not just one compilation
    unit), that identifies a closure within a particular set of closures
    (viz. [Project_closure]). *)

include module type of Closure_element

(* CR mshinwell: Fix the problem that Closure_id and Var_within_closure seem
   to be being used interchangeably by the type checker---presumably they are
   actually equal?? *)
