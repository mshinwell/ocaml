(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type t = {
  name : Continuation.t;
  handler : Flambda.continuation_handler option;
  num_params : int;
  specialised_args : Flambda.specialised_args;
}

let create ~name ~(handler : Flambda.continuation_handler) ~num_params
      ~specialised_args =
  { name;
    handler = Some handler;
    num_params;
    specialised_args;
  }

let create_unknown ~name ~num_params ~specialised_args =
  { name;
    handler = None;
    num_params;
    specialised_args;
  }

let name t = t.name
let num_params t = t.num_params
let handler t = t.handler
let specialised_args t = t.specialised_args

let print ppf t =
  let print_handler ppf = function
    | None -> Format.fprintf ppf "<handler not known>"
    | Some handler ->
      Flambda.print_let_cont_handler ppf (Handler handler)
  in
  Format.fprintf ppf "@[(%a with %d params %a, specialisation %a)@]"
    Continuation.print t.name
    t.num_params
    print_handler t.handler
    Flambda.print_specialised_args t.specialised_args
