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

(** Simplification of the contents of entire compilation units.  These are
    known (maybe somewhat misleadingly) as "programs". *)
(* CR mshinwell: Rename to [Simplify_unit]? *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make
    (Simplify_named : Simplify_named_intf.S)
    (Simplify_toplevel : Simplify_toplevel_intf.S)
  : Simplify_program_intf.S
