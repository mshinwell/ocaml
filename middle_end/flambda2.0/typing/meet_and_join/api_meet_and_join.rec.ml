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

module Meet = Meet_and_join.Make (Either_meet_or_join.For_meet)
module Join = Meet_and_join.Make (Either_meet_or_join.For_join)

module Meet_value = Meet_and_join_value.Make (Either_meet_or_join.For_meet)
module Join_value = Meet_and_join_value.Make (Either_meet_or_join.For_join)

module Meet_fabricated =
  Meet_and_join_fabricated.Make (Either_meet_or_join.For_meet)
module Join_fabricated =
  Meet_and_join_fabricated.Make (Either_meet_or_join.For_join)

let meet env t1 t2 =
  Meet.meet_or_join (Join_env.create env) t1 t2

let join ?bound_name env t1 t2 =
  let join_ty, _env_extension = Join.meet_or_join ?bound_name env t1 t2 in
  join_ty

let meet_closures_entry env entry1 entry2 : _ Or_bottom.t =
  let env = Join_env.create env in
  match Meet_value.meet_or_join_closures_entry env entry1 entry2 with
  | Ok (entry, env_extension) -> Ok (entry, env_extension)
  | Absorbing -> Bottom

(* CR mshinwell: Sort out this nuisance Absorbing vs. Bottom thing.
   Also the fact that "join" is returning an env_extension *)
let join_closures_entry env entry1 entry2 =
  match Join_value.meet_or_join_closures_entry env entry1 entry2 with
  | Ok (entry, _env_extension) -> entry
  | Absorbing -> assert false

let meet_set_of_closures_entry env entry1 entry2 : _ Or_bottom.t =
  let env = Join_env.create env in
  match
    Meet_fabricated.meet_or_join_set_of_closures_entry env entry1 entry2
  with
  | Ok (entry, env_extension) -> Ok (entry, env_extension)
  | Absorbing | Bottom -> Bottom

let join_set_of_closures_entry env entry1 entry2 =
  match
    Join_fabricated.meet_or_join_set_of_closures_entry env entry1 entry2
  with
  | Ok (entry, _env_extension) -> entry
  | Absorbing -> assert false
