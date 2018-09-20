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

module E = Simplify_env_and_result.Env
module R = Simplify_env_and_result.Result

module Expr = Flambda.Expr

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

let run ~never_inline ~allow_continuation_inlining
      ~allow_continuation_specialisation ~backend ~prefixname ~round program =
  let report = !Clflags.inlining_report in
  if never_inline then begin
    Clflags.inlining_report := false
  end;
  let initial_env =
    E.create ~never_inline ~allow_continuation_inlining
      ~allow_continuation_specialisation ~backend
      ~scope_level_for_lifted_constants:Scope_level.initial
      ~round
  in
  let program, newly_imported_symbols =
    Simplify_program.simplify_program initial_env program
  in
  let imported_symbols =
    (* CR mshinwell: Here and elsewhere, these [disjoint_union] calls should
       raise proper messages *)
    Symbol.Map.disjoint_union program.imported_symbols
      newly_imported_symbols
  in
  let program : Flambda_static.Program.t =
    { program with
      imported_symbols;
    }
  in
  if !Clflags.inlining_report then begin
    let output_prefix = Printf.sprintf "%s.%d" prefixname round in
    Inlining_stats.save_then_forget_decisions ~output_prefix
  end;
  Clflags.inlining_report := report;
  program
