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

module JE = Flambda_type.Join_env
module T = Flambda_type

(* CR-soon mshinwell: Consider lwhite's suggestion of doing the existential
   introduction at [Switch] time *)

let parameters_and_body_env uses ~continuation_env_of_definition ~parameters =
  match Continuation_uses.uses uses with
  | [] -> parameters
  | first_use::remaining_uses ->
    let first_parameters = Continuation_uses.Use.parameters first_use in
    let env = JE.create continuation_env_of_definition in
    let joined_parameters =
      List.fold_left (fun joined_parameters use ->
          let parameters = Continuation_uses.Use.parameters use in
          T.Parameters.join env parameters joined_parameters)
        first_parameters
        remaining_uses
    in
    T.Parameters.meet ~fresh_name_semantics:Left
      env parameters joined_parameters

let parameters uses ~continuation_env_of_definition ~parameters =
  let _body_env, parameters =
    parameters_and_body_env uses ~continuation_env_of_definition ~parameters
  in
  parameters
