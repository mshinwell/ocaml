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

module A =
  Name_abstraction.Make_list (Bindable_variable_in_types) (Typing_env_level)

(* The record is here to avoid the double vision problem.  (Otherwise
   there would already be an equality
     t = Name_abstraction.Make_list ...
   meaning that the equality
     t = Typing_env_extension.t
   could not be added by the type checker.) *)
type t = {
  abst : A.t;
} [@@unboxed]

let print ppf { abst; } =
  Name_abstraction.with_printing_style Existential ~f:(fun () ->
    A.print ppf abst)

let invariant { abst; } =
  A.pattern_match abst ~f:(fun _ level -> Typing_env_level.invariant level)

let empty () =
  { abst = A.create [] (Typing_env_level.empty ()); }

let is_empty { abst; } =
  A.pattern_match abst ~f:(fun _ level -> Typing_env_level.is_empty level)

(* CR mshinwell: It might be worth adding a parameter here that asserts
   whether the [defined_vars] of the level are expected to be empty.  This
   should always be the case for extensions generated from [meet]. *)
let create level =
  let abst =
    A.create (Typing_env_level.defined_vars_in_order' level) level
  in
  { abst; }

let pattern_match { abst; } ~f =
  A.pattern_match abst ~f:(fun _ level -> f level)

let one_equation name ty =
  let abst = A.create [] (Typing_env_level.one_equation name ty) in
  { abst; }

let add_or_replace_equation { abst; } name ty =
  let abst =
    A.pattern_match abst ~f:(fun _defined_names level ->
      let level = Typing_env_level.add_or_replace_equation level name ty in
      A.create (Typing_env_level.defined_vars_in_order' level) level)
  in
  { abst; }

let add_cse { abst; } prim ~bound_to =
  let abst =
    A.pattern_match abst ~f:(fun _defined_names level ->
      let level = Typing_env_level.add_cse level prim ~bound_to in
      A.create (Typing_env_level.defined_vars_in_order' level) level)
  in
  { abst; }

let meet env (t1 : t) (t2 : t) : t =
  let abst =
    A.pattern_match t1.abst ~f:(fun _ level_1 ->
      A.pattern_match t2.abst ~f:(fun _ level_2 ->
        let level = Typing_env_level.meet env level_1 level_2 in
        A.create (Typing_env_level.defined_vars_in_order' level) level))
  in
  { abst; }

let n_way_join ~initial_env_at_join envs_with_extensions : t * _ =
  let abst, extra_cse_bindings =
    let rec open_binders envs_with_extensions envs_with_levels =
      match envs_with_extensions with
      | [] ->
        let level, extra_cse_bindings =
          Typing_env_level.n_way_join ~initial_env_at_join envs_with_levels
        in
        let abst =
          A.create (Typing_env_level.defined_vars_in_order' level) level
        in
        abst, extra_cse_bindings
      | (_env, id, interesting_vars, t)::envs_with_extensions ->
        A.pattern_match t.abst ~f:(fun _ level ->
          let env =
            Typing_env.add_env_extension_from_level initial_env_at_join level
          in
          (* It doesn't matter that the list gets reversed. *)
          let envs_with_levels =
            (env, id, interesting_vars, level) :: envs_with_levels
          in
          open_binders envs_with_extensions envs_with_levels)
    in
    open_binders envs_with_extensions []
  in
  { abst; }, extra_cse_bindings
