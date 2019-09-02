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

let simplify_projection dacc ~original_term ~deconstructing ~shape ~result_var
      ~result_kind =
  let env = DE.typing_env (DA.denv dacc) in
  match T.meet_shape env deconstructing ~shape ~result_var ~result_kind with
  | Bottom -> Reachable.invalid (), TEE.empty (), dacc
  | Ok env_extension ->
    Reachable.reachable original_term, env_extension, dacc

type cse =
  | Invalid of T.t
  | Applied of (Reachable.t * TEE.t * DA.t)
  | Not_applied of DA.t

let apply_cse dacc ~original_prim ~min_occurrence_kind =
  match P.Eligible_for_cse.create original_prim with
  | None -> None
  | Some with_fixed_value ->
    let typing_env = DE.typing_env (DA.denv dacc) in
    match TE.find_cse typing_env with_fixed_value with
    | None ->
      None
    | Some simple ->
      match TE.get_canonical_simple typing_env ~min_occurrence_kind simple with
      | Bottom | Ok None -> None
      | Ok (Some simple) ->
        Some simple

let try_cse dacc ~original_prim ~result_kind ~min_occurrence_kind
      ~result_var : cse =
  (* CR mshinwell: Use [meet] and [reify] for CSE?  (discuss with lwhite) *)
  match apply_cse dacc ~original_prim ~min_occurrence_kind with
  | Some replace_with ->
    let named = Named.create_simple replace_with in
    let ty = T.alias_type_of result_kind replace_with in
    let env_extension = TEE.one_equation (Name.var result_var) ty in
    Applied (Reachable.reachable named, env_extension, dacc)
  | None ->
    let dacc =
      match P.Eligible_for_cse.create original_prim with
      | None -> dacc
      | Some eligible_prim ->
        let bound_to = Simple.var result_var in
        DA.map_denv dacc ~f:(fun denv ->
          DE.with_typing_env denv
           (TE.add_cse (DE.typing_env denv) eligible_prim ~bound_to))
    in
    Not_applied dacc
