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

module T = Flambda_type

type t = {
  continuation_uses : Continuation_uses.t Continuation.Map.t;
}

let print ppf { continuation_uses; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(continuation_uses@ %a)@]\
      )@]"
    (Continuation.Map.print Continuation_uses.print) continuation_uses

let empty = {
  continuation_uses = Continuation.Map.empty;
}

let record_continuation_use t cont ~typing_env_at_use ~arg_types =
  (* XXX This needs to deal with exn continuation extra-args *)
  let continuation_uses =
    Continuation.Map.update cont (function
        | None ->
          let arity = T.arity_of_list arg_types in
          let uses = Continuation_uses.create cont arity in
          Some (Continuation_uses.add_use uses ~typing_env_at_use ~arg_types)
        | Some uses ->
          Some (Continuation_uses.add_use uses ~typing_env_at_use ~arg_types))
      t.continuation_uses
  in
  { continuation_uses;
  }

let continuation_env_and_param_types t ~definition_typing_env cont arity =
  match Continuation.Map.find cont t.continuation_uses with
  | exception Not_found ->
    let arg_types =
      (* CR mshinwell: Move to [Flambda_type] *)
      List.map (fun kind -> T.bottom kind) arity
    in
    definition_typing_env, arg_types
  | uses ->
    Continuation_uses.env_and_arg_types uses ~definition_typing_env

let num_continuation_uses t cont =
  match Continuation.Map.find cont t.continuation_uses with
  | exception Not_found -> 0
  | uses -> Continuation_uses.number_of_uses uses
