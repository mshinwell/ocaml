(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016--2017 OCamlPro SAS                                    *)
(*   Copyright 2016--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type continuation_handlers =
  | Non_recursive of Flambda.Continuation_handler.t
  | Recursive of Flambda.Continuation_handlers.t

type t = {
  name : Continuation.t;
  handlers : continuation_handlers option;
  arity : Flambda_arity.t;
}

let create ~name ~(handlers : continuation_handlers) ~arity =
  { name;
    handlers = Some handlers;
    arity;
  }

let create_unknown ~name ~arity =
  { name;
    handlers = None;
    arity;
  }

let name t = t.name
let arity t = t.arity
let handlers t = t.handlers

let is_alias t =
  match t.handlers with
  | None | Some (Recursive _) -> None
  | Some (Non_recursive handler) ->
    match handler.handler with
    | Apply_cont (cont, None, args) ->
      let args' =
        Misc.Stdlib.List.filter_map (fun (arg : Simple.t) ->
            match arg with
            | Name (Var var) -> Some var
            | Name (Symbol _) | Const _ -> None)
          args
      in
      if List.compare_lengths args args' = 0
         && Flambda.Typed_parameter.List.equal_vars handler.params args'
      then Some cont
      else None
    | _ -> None

let print ppf t =
  let print_handlers ppf = function
    | None -> Format.fprintf ppf "<handlers not known>"
    | Some handlers ->
      match handlers with
      | Non_recursive handler ->
        Flambda.Let_cont_handlers.print ppf
          (Non_recursive { name = t.name; handler; })
      | Recursive handlers ->
        Flambda.Let_cont_handlers.print ppf (Recursive handlers)
  in
  Format.fprintf ppf "@[((name %a) (arity %a) (handlers %a))@]"
    Continuation.print t.name
    Flambda_arity.print t.arity
    print_handlers t.handlers
