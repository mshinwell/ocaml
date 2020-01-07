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

type simplify_result = {
  cmx : Flambda_cmx_format.t option;
  unit : Flambda_unit.t;
}

let run ~backend ~round unit =
  let module Backend = (val backend : Flambda2_backend_intf.S) in
  let return_continuation = FU.return_continuation unit in
  let exn_continuation = FU.exn_continuation unit in
  let imported_names = ref Name.Set.empty in
  let imported_code = ref Code_id.Map.empty in
  let resolver comp_unit =
    Flambda_cmx.load_cmx_file_contents backend comp_unit ~imported_names
      ~imported_code
  in
  let get_imported_names () = !imported_names in
  let get_imported_code () = !imported_code in
  let denv =
    DE.create ~round
      ~backend
      ~resolver
      ~get_imported_names
      ~get_imported_code
      ~float_const_prop:!Clflags.float_const_prop
      ~unit_toplevel_exn_continuation:exn_continuation
  in
  let return_cont_scope = DE.get_continuation_scope_level denv in
  let denv = DE.increment_continuation_scope_level denv in
  let exn_cont_scope = DE.get_continuation_scope_level denv in
  let denv = DE.increment_continuation_scope_level denv in
  let r = R.create ~resolver ~get_imported_names in
  let dacc = DA.create denv Continuation_uses_env.empty r in
  let body, return_cont_env, r =
    let exn_continuation =
      Exn_continuation.create ~exn_handler:exn_continuation ~extra_args:[]
    in
    Simplify_toplevel.simplify_toplevel dacc (FU.body unit) ~return_continuation
      ~return_arity:[K.value] exn_continuation ~return_cont_scope
      ~exn_cont_scope
  in
  let cmx =
    Flambda_cmx.prepare_cmx_file_contents ~return_cont_env
      ~return_continuation (R.all_code r)
  in
  let unit = FU.create ~return_continuation ~exn_continuation ~body in
  { cmx;
    unit;
  }
