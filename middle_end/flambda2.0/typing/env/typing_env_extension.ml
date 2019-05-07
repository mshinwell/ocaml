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

let equal env result { abst = abst1; } { abst = abst2; } =
  A.pattern_match_pair abst1 abst2 ~f:(fun existentials level1 level2 ->
    let existentials =
      List.fold_left (fun result (bindable_name : Bindable_name.t) ->
          match bindable_name with
          | Name name -> Name.Set.add name result
          | Continuation _ ->
            Misc.fatal_error "[Continuation] not allowed here")
        Name.Set.empty
        existentials
    in
    let (>>=) = Type_equality_result.(>>=) in
    let env =
      Type_equality_env.entering_scope_of_existentials env existentials
    in
    Typing_env_level.equal env result level1 level2
    >>= fun result ->
    let check_now, result =
      Type_equality_result.leaving_scope_of_existential result
        ~bound_names:existentials
    in
    result
    >>= fun result ->
    Name.Map.fold (fun _name uses result ->
        result
        >>= fun result ->
        if Type_equality_result.Uses.more_than_one_use_and_empty uses
        then Type_equality_result.types_known_unequal ()
        else result)
      check_now
      result)

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

(* CR mshinwell: We should provide a termination proof for algorithms
   such as this. *)
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

(*
let diff { abst; } env =
  A.pattern_match abst ~f:(fun _ level ->
    let level = Typing_env_level.diff level env in
    let defined_names = Typing_env_level.defined_names level in
    { abst = A.create defined_names level; })
*)

let pattern_match { abst; } ~f =
  A.pattern_match abst ~f:(fun _ level -> f level)
