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

module RL =
  Row_like.Make (Closure_id) (Var_within_closure_set)
    (Closure_id_and_var_within_closure_set)
    (Flambda_type0_core.Closures_entry)

type t = RL.t

let create_exactly_multiple closure_id_and_vars_within_closure_map =
  RL.create_exactly_multiple closure_id_and_vars_within_closure_map

let create_at_least_multiple vars_within_closure_map =
  RL.create_at_least_multiple vars_within_closure_map

let print_with_cache = RL.print_with_cache
let print = RL.print
let equal = RL.equal

let meet env t1 t2 =
  Or_bottom.map (RL.meet env t1 t2)
    ~f:(fun (t, _closures_entry) -> t, Typing_env_extension.empty)

let join = RL.join
