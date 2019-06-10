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
module T = Flambda_type
module TEE = Flambda_type.Typing_env_extension

let simplify_projection dacc ~original_term ~deconstructing ~shape ~result_var
      ~result_kind =
  let env = DE.typing_env (DA.denv dacc) in
(*
Format.eprintf "simplify_projection: original_term %a@ shape:@ %a@ deconstructing:@ %a@ env:@ %a\n%!"
  Flambda.Named.print original_term
  T.print shape
  T.print deconstructing
  DA.print dacc;
*)
  match T.meet_shape env deconstructing ~shape ~result_var ~result_kind with
  | Bottom -> Reachable.invalid (), TEE.empty, dacc
  | Ok env_extension ->
(*
Format.eprintf "Returned env extension:@ %a\n%!" TEE.print env_extension;
*)
    Reachable.reachable original_term, env_extension, dacc
