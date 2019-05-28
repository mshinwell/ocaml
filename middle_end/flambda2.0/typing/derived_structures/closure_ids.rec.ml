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

module Unit_and_closure_id_set = struct
  type t = unit * Closure_id.Set.t
  include Identifiable.Make_pair (Unit) (Closure_id_set)
end

module RL =
  Row_like.Make (Unit) (Closure_id_set) (Unit_and_closure_id_set)
    (Flambda_type0_core.Set_of_closures_entry)

type t = RL.t

type open_or_closed = Open | Closed

let create closure_ids_map open_or_closed : t =
  match open_or_closed with
  | Open -> RL.create_at_least_multiple closure_ids_map
  | Closed ->
    let closure_ids_map =
      Closure_id_set.Map.fold
        (fun closure_ids set_of_closures_entry result ->
          Unit_and_closure_id_set.Map.add ((), closure_ids)
            set_of_closures_entry result)
        closure_ids_map
        Unit_and_closure_id_set.Map.empty
    in
    RL.create_exactly_multiple closure_ids_map

let create_bottom = RL.create_bottom

let print = RL.print
let invariant _t = ()  (* CR mshinwell: RL.invariant *)

let equal = RL.equal

let meet env t1 t2 : _ Or_bottom.t =
  Or_bottom.map (RL.meet env t1 t2)
    ~f:(fun (t, _set_of_closures_entry) -> t, Typing_env_extension.empty)

let join = RL.join
