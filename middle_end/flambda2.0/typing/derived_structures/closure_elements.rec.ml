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

type t = Flambda_types.t Var_within_closure.Map.t

let create closure_elements_to_tys = closure_elements_to_tys

let create_bottom () = Var_within_closure.Map.empty

let print ~cache:_ ppf t = RP.print ppf t

let meet = RP.meet
let join = RP.join

let equal = RP.equal
let free_names = RP.free_names
let apply_name_permutation = RP.apply_name_permutation
