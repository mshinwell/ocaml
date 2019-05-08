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

module A = Name_abstraction.Make_list (Typing_env_level)

(* The record is here to avoid the double vision problem.  (Otherwise
   there would already be an equality
     t = Name_abstraction.Make_list (Typing_env_level)
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

let fast_equal t1 t2 = (t1 == t2)

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

(*
let restrict_to_definitions { abst; } =
  let abst =
    A.pattern_match_mapi abst ~f:(fun defined_names level ->
      (* CR mshinwell: Does "in terms" really make sense (or is it even
         correct?) here? *)
      Typing_env_level.restrict_to_names level
        (Name_occurrences.create_from_name_set_in_terms defined_names))
  in
  { abst; }

let restrict_names_to_those_occurring_in_types _t _env _env_allowed_names
      _tys =
  Misc.fatal_error "Not yet implemented"
*)
(*
  let free_names = free_names_transitive_list t env tys in
  let env_allowed_names = Typing_env.domain env_allowed_names in
  let allowed_names =
    Name_occurrences.union free_names env_allowed_names
  in
  pattern_match_map t ~f:(fun level ->
    Typing_env_level.restrict_to_names level allowed_names)
*)

let is_bottom t = is_empty t

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

(*
let diff { abst; } env =
  A.pattern_match abst ~f:(fun _ level ->
    let level = Typing_env_level.diff level env in
    let defined_names = Typing_env_level.defined_names level in
    { abst = A.create defined_names level; })
*)

let pattern_match { abst; } ~f =
  A.pattern_match abst ~f:(fun _ level -> f level)
