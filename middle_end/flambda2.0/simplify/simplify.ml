(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Flambda.Import

module E = Simplify_env_and_result.Env
module R = Simplify_env_and_result.Result

module Outer_namespace = struct
  module Simplify_expr = Simplify_expr
  module Simplify_named = Simplify_named
  module Simplify_program = Simplify_program
  module Simplify_toplevel = Simplify_toplevel
end

module rec Simplify_expr : Simplify_expr_intf.S =
  Outer_namespace.Simplify_expr.Make (Simplify_named)
and Simplify_named : Simplify_named_intf.S =
  Outer_namespace.Simplify_named.Make (Simplify_toplevel)
and Simplify_program : Simplify_program_intf.S =
  Outer_namespace.Simplify_program.Make (Simplify_named) (Simplify_toplevel)
and Simplify_toplevel : Simplify_toplevel_intf.S =
  Outer_namespace.Simplify_toplevel.Make (Simplify_expr)

let run ~backend ~round program =
  let env =
    E.create ~round ~backend
      ~scope_level_for_lifted_constants:Scope_level.initial
  in
  Simplify_program.simplify_program env program
