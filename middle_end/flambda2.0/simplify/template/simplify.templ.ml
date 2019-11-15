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

[@@@ocaml.warning "+a-30-40-41-42"]
(* CR mshinwell: Fix warning 60 *)
[@@@ocaml.warning "-60"]

open! Simplify_import

(* -- module rec binding here -- *)

let check_imported_symbols_don't_overlap_predef_exns
      ~imported_symbols ~predef_exn_symbols ~descr =
  let wrong_symbols =
    Symbol.Set.inter (Symbol.Map.keys imported_symbols)
      (Symbol.Map.keys predef_exn_symbols)
  in
  if not (Symbol.Set.is_empty wrong_symbols) then begin
    Misc.fatal_errorf "Program's [imported_symbols] (%s) must not contain \
        predefined exception symbols"
      descr
  end

let run ~backend ~round unit =
  let module Backend = (val backend : Flambda2_backend_intf.S) in
  let predef_exn_symbols =
    Symbol.Set.fold (fun symbol predef_exn_symbols ->
        Symbol.Map.add symbol K.value predef_exn_symbols)
      Backend.all_predefined_exception_symbols
      Symbol.Map.empty
  in
  let imported_symbols = FU.imported_symbols unit in
  check_imported_symbols_don't_overlap_predef_exns
    ~imported_symbols:imported_symbols ~predef_exn_symbols
    ~descr:"before simplification";
  let return_continuation = FU.return_continuation unit in
  let exn_continuation = FU.exn_continuation unit in
  let denv =
    DE.create ~round
      ~backend
      ~float_const_prop:!Clflags.float_const_prop
      ~unit_toplevel_exn_continuation:exn_continuation
  in
  let denv =
    Symbol.Map.fold (fun symbol kind denv ->
        DE.add_symbol denv symbol (T.unknown kind))
      (Symbol.Map.disjoint_union imported_symbols predef_exn_symbols)
      denv
  in
  let return_cont_scope = DE.get_continuation_scope_level denv in
  let denv = DE.increment_continuation_scope_level denv in
  let exn_cont_scope = DE.get_continuation_scope_level denv in
  let denv = DE.increment_continuation_scope_level denv in
  let r = R.create ~resolver:(DE.resolver denv) in
  let dacc = DA.create denv Continuation_uses_env.empty r in
  let body, _cont_uses_env, r =
    let exn_continuation =
      Exn_continuation.create ~exn_handler:exn_continuation ~extra_args:[]
    in
    Simplify_toplevel.simplify_toplevel dacc (FU.body unit) ~return_continuation
      ~return_arity:[K.value] exn_continuation ~return_cont_scope
      ~exn_cont_scope
  in
  let imported_symbols = R.imported_symbols r in
  check_imported_symbols_don't_overlap_predef_exns
    ~imported_symbols:imported_symbols ~predef_exn_symbols
    ~descr:"after simplification";
  FU.create ~imported_symbols
    ~return_continuation
    ~exn_continuation
    ~body
