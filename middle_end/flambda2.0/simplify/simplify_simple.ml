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

module DA = Downwards_acc
module DE = Simplify_env_and_result.Downwards_env
module K = Flambda_kind
module T = Flambda_type
module TE = T.Typing_env

let simplify_simple dacc simple ~min_occurrence_kind =
  let typing_env = DE.typing_env (DA.denv dacc) in
  TE.get_canonical_simple typing_env simple ~min_occurrence_kind

let simplify_simples dacc simples ~min_occurrence_kind =
  Or_bottom.all (List.map (fun simple : _ Or_bottom.t ->
      match simplify_simple dacc simple ~min_occurrence_kind with
      | Ok (simple, ty) -> Ok (simple, ty)
      | Bottom _kind -> Bottom)
    simples)
