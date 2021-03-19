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
      params : Kinded_parameter.t list;
      handler : Flambda.Expr.t;
      free_names_of_handler : Name_occurrences.t;
      cost_metrics_of_handler : Flambda.Cost_metrics.t;
    }
  | Non_inlinable of {
      params : Kinded_parameter.t list;
      handler : Flambda.Expr.t;
    }
  | Unreachable
  (* CR mshinwell: improve "Other" naming now we have [Non_inlinable] *)
  | Other

(* CR mshinwell: Write a proper printer *)
let print ppf t =
  match t with
  | Linearly_used_and_inlinable { params = _; handler = _;
      free_names_of_handler = _; cost_metrics_of_handler = _ } ->
    Format.pp_print_string ppf "Linearly_used_and_inlinable _"
  | Non_inlinable { params = _; handler = _; } ->
    Format.pp_print_string ppf "Non_inlinable _"
  | Other -> Format.pp_print_string ppf "Other"
  | Unreachable -> Format.pp_print_string ppf "Unreachable"
