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

type t =
  | Unknown of { arity : Flambda_arity.t; }
  | Unreachable of { arity : Flambda_arity.t; }
  | Apply_cont_with_constant_arg of {
      cont : Continuation.t;
      arg : Simple.Const.t;
      arity : Flambda_arity.t;
    }
  | Inline of {
      arity : Flambda_arity.t;
      handler : Flambda.Continuation_handler.t;
    }

(* CR mshinwell: Write a proper printer *)
let print ppf t =
  match t with
  | Unknown { arity = _; } -> Format.pp_print_string ppf "Unknown"
  | Unreachable { arity = _; } -> Format.pp_print_string ppf "Unreachable"
  | Apply_cont_with_constant_arg { cont = _; arg = _; arity = _; } ->
    Format.pp_print_string ppf "Apply_cont_with_constant_arg _"
  | Inline { arity = _; handler = _; } ->
    Format.pp_print_string ppf "Inline _"

let arity t =
  match t with
  | Unknown { arity; }
  | Unreachable { arity; }
  | Apply_cont_with_constant_arg { cont = _; arg = _; arity; }
  | Inline { arity; handler = _; } -> arity
