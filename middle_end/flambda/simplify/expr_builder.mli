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

(** Functions for rebuilding expressions that are used during
    simplification.  Unlike the basic creation functions in [Expr] these
    functions do things such as keeping track of free names and
    avoiding generation of unused bindings. *)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Flambda.Import

include Expr_builder_intf.S with type expr = Expr.t
