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

let print ppf { abst; } = A.print ~style:Existential ppf abst

let print_with_cache ~cache ppf { abst; } =
  A.print_with_cache ~style:Existential ~cache ppf abst

let free_names { abst; } = A.free_names abst

let apply_name_permutation ({ abst; } as t) perm =
  let abst' = A.apply_name_permutation abst perm in
  if abst == abst' then t
  else { abst = abst'; }

let invariant { abst; } =
  A.pattern_match abst ~f:(fun _ level -> Typing_env_level.invariant level)

let empty () =
  { abst = A.create [] (Typing_env_level.empty ()); }

let is_empty { abst; } =
  A.pattern_match abst ~f:(fun _ level -> Typing_env_level.is_empty level)

let create level =
  let abst =
    A.create (Typing_env_level.defined_names_in_order level) level
  in
  { abst; }

let is_bottom t = is_empty t

let meet (env : Meet_env.t) (t1 : t) (t2 : t) : t =
  if Meet_env.shortcut_precondition env && fast_equal t1 t2 then t1
  (* Care: the domains of [t1] and [t2] are treated as contravariant.
     So if one of them is bottom, the result of meeting it with any other
     level is that level, not bottom. *)
  else if is_bottom t1 then t2
  else if is_bottom t2 then t1
  else
    let t1 = apply_name_permutation t1 (Meet_env.perm_left env) in
    let t2 = apply_name_permutation t2 (Meet_env.perm_right env) in
    let env = Meet_env.clear_name_permutations env in
    let abst =
      A.pattern_match t1.abst ~f:(fun _ level_1 ->
        A.pattern_match t2.abst ~f:(fun _ level_2 ->
          let level = Typing_env_level.meet env level_1 level_2 in
          A.create (Typing_env_level.defined_names_in_order level) level))
    in
    { abst; }

let join (env : Join_env.t) (t1 : t) (t2 : t) : t =
  if Join_env.shortcut_precondition env && fast_equal t1 t2 then t1
  else if is_bottom t1 then t1
  else if is_bottom t2 then t2
  else
    let t1 = apply_name_permutation t1 (Join_env.perm_left env) in
    let t2 = apply_name_permutation t2 (Join_env.perm_right env) in
    let env = Join_env.clear_name_permutations env in
    let env =
      Join_env.add_extensions env ~holds_on_left:t1 ~holds_on_right:t2
    in
    let abst =
      A.pattern_match t1.abst ~f:(fun _ level_1 ->
        A.pattern_match t2.abst ~f:(fun _ level_2 ->
          let level = Typing_env_level.join env level_1 level_2 in
          A.create (Typing_env_level.defined_names_in_order level) level))
    in
    { abst; }

let add_definition { abst; } name kind =
  let abst =
    A.pattern_match abst ~f:(fun _defined_names level ->
      let level = Typing_env_level.add_definition level name kind in
      A.create (Typing_env_level.defined_names_in_order level) level)
  in
  { abst; }

let add_equation { abst; } name ty =
  let abst =
    A.pattern_match abst ~f:(fun _defined_names level ->
      let level = Typing_env_level.add_equation level name ty in
      A.create (Typing_env_level.defined_names_in_order level) level)
  in
  { abst; }

(* CR mshinwell: Consider an [A.pattern_match] variant that does not
   pass [defined_names] but where 'a is returned *)
let meet_equation { abst; } env name ty =
  let abst =
    A.pattern_match abst ~f:(fun _defined_names level ->
      let level = Typing_env_level.meet_equation level env name ty in
      A.create (Typing_env_level.defined_names_in_order level) level)
  in
  { abst; }

let add_cse { abst; } simple prim =
  let abst =
    A.pattern_match_map abst ~f:(fun level ->
      Typing_env_level.add_cse level simple prim)
  in
  { abst; }

let pattern_match { abst; } ~f =
  A.pattern_match abst ~f:(fun _ level -> f level)
