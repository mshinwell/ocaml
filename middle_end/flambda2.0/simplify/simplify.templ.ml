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

(* -- module rec binding here -- *)

let run ~backend ~round program =
  let dummy_toplevel_exn_cont = (* CR mshinwell: ugly? *)
    Exn_continuation.create ~exn_handler:(Continuation.create ~sort:Exn ())
      ~extra_args:[]
  in
  let denv =
    Simplify_env_and_result.Downwards_env.create ~round
      ~backend
      ~float_const_prop:!Clflags.float_const_prop
      ~toplevel_exn_cont:dummy_toplevel_exn_cont
  in
  Simplify_static.simplify_program denv program
