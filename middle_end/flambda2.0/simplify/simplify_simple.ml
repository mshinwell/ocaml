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

type result =
  | Ok of Simple.t * T.t
  | Bottom of K.t

let simplify_simple dacc simple ~min_occurrence_kind : result =
  let newer_rec_info = Simple.rec_info simple in
  let kind, simple =
    match Simple.descr simple with
    | Name name ->
      TE.get_canonical_simple (DE.typing_env (DA.denv dacc)) name
        ~min_occurrence_kind:result_occurrence_kind
    | Const const -> T.kind_for_const const, simple
    | Discriminant _ -> K.fabricated, simple
  in
  (* CR mshinwell: Should this look through the alias to determine if the
     type is [Bottom]? *)
  match Simple.merge_rec_info simple ~newer_rec_info with
  | Some simple -> Ok (simple, T.alias_type_of kind simple)
  | None -> Bottom kind

let simplify_simples dacc simples ~min_occurrence_kind =
  Or_bottom.all (List.map (fun simple : _ Or_bottom.t ->
      match simplify_simple dacc simple ~min_occurrence_kind with
      | Ok (simple, ty) -> Ok (simple, ty)
      | Bottom _kind -> Bottom)
    simples)
