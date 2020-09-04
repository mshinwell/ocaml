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

open! Simplify_import

let simplify_toplevel dacc expr ~return_continuation ~return_arity
      exn_continuation ~return_cont_scope ~exn_cont_scope =
  Simplify_expr.simplify_expr dacc expr ~down_to_up:(fun dacc ~rebuild ->
    let uenv =
      UE.add_continuation UE.empty return_continuation
        return_cont_scope return_arity
    in
    let uenv =
      UE.add_exn_continuation uenv exn_continuation exn_cont_scope
    in
    rebuild (UA.create uenv dacc) ~after_rebuild:(fun expr uacc -> expr, uacc))
