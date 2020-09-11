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
  | Linearly_used_and_inlinable of {
      arity : Flambda_arity.With_subkinds.t;
      params : Kinded_parameter.t list;
      handler : Flambda.Expr.t;
      free_names_of_handler : Name_occurrences.t;
    }
  | Other of {
      arity : Flambda_arity.With_subkinds.t;
      handler : Flambda.Continuation_handler.t option;
    }
  | Unreachable of { arity : Flambda_arity.With_subkinds.t; }

(* CR mshinwell: Write a proper printer *)
let print ppf t =
  match t with
  | Linearly_used_and_inlinable { arity = _; params = _; handler = _;
      free_names_of_handler = _; } ->
    Format.pp_print_string ppf "Linearly_used_and_inlinable _"
  | Other { arity = _; handler = _; } ->
    Format.pp_print_string ppf "Other"
  | Unreachable { arity = _; } -> Format.pp_print_string ppf "Unreachable"

let arity t =
  match t with
  | Linearly_used_and_inlinable { arity; params = _; handler = _;
      free_names_of_handler = _; }
  | Other { arity; handler = _; }
  | Unreachable { arity; } ->
    arity
