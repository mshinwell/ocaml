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

open! Simplify_import

let simplify_projection denv ~original_term ~deconstructing ~shape ~result_var
      ~result_kind =
  let env = DE.typing_env denv in
  match T.meet_shape env deconstructing ~shape ~result_var ~result_kind with
  | Bottom -> Simplified_named.invalid (), TEE.empty (), denv
  | Ok env_extension ->
    Simplified_named.reachable original_term, env_extension, denv

type cse =
  | Invalid of T.t
  | Applied of (Simplified_named.t * TEE.t * Simple.t list * DE.t)
  | Not_applied of DE.t

let apply_cse denv ~original_prim =
  match P.Eligible_for_cse.create original_prim with
  | None -> None
  | Some with_fixed_value ->
    match DE.find_cse denv with_fixed_value with
    | None -> None
    | Some simple ->
      match TE.get_canonical_simple_exn (DE.typing_env denv) simple with
      | exception Not_found -> None
      | simple -> Some simple

let try_cse denv ~original_prim ~result_kind ~min_name_mode ~args
      ~result_var : cse =
  (* CR mshinwell: Use [meet] and [reify] for CSE?  (discuss with lwhite) *)
  if not (Name_mode.equal min_name_mode Name_mode.normal) then Not_applied denv
  else
    match apply_cse denv ~original_prim with
    | Some replace_with ->
      let named = Named.create_simple replace_with in
      let ty = T.alias_type_of result_kind replace_with in
      let env_extension = TEE.one_equation (Name.var result_var) ty in
      Applied (Simplified_named.reachable named, env_extension, args, denv)
    | None ->
      let denv =
        match P.Eligible_for_cse.create original_prim with
        | None -> denv
        | Some eligible_prim ->
          let bound_to = Simple.var result_var in
          DE.add_cse denv eligible_prim ~bound_to
      in
      Not_applied denv
