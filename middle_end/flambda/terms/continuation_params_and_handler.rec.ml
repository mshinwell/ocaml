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

module T0 = struct
  type t = {
    num_normal_occurrences_of_params : Num_occurrences.t Variable.Map.t;
    handler : Expr.t;
  }

  let print_with_cache ~cache ppf
        { handler; num_normal_occurrences_of_params = _; } =
    fprintf ppf "@[<hov 1>(\
        @[<hov 1>(handler@ %a)@]\
        )@]"
      (Expr.print_with_cache ~cache) handler

  let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let free_names { handler; num_normal_occurrences_of_params = _; } =
    Expr.free_names handler

  let apply_name_permutation
        ({ handler; num_normal_occurrences_of_params; } as t) perm =
    let handler' =
      Expr.apply_name_permutation handler perm
    in
    if handler == handler' then t
    else { handler = handler'; num_normal_occurrences_of_params; }

  let all_ids_for_export { handler; num_normal_occurrences_of_params = _; } =
    Expr.all_ids_for_export handler

  let import import_map { handler; num_normal_occurrences_of_params; } =
    let handler = Expr.import import_map handler in
    { handler; num_normal_occurrences_of_params; }
end

include Name_abstraction.Make_list (Kinded_parameter) (T0)

let invariant _env _t = ()

let print ppf t : unit = print ppf t

let print_with_cache ~cache ppf t : unit = print_with_cache ~cache ppf t

let create params ~handler ~(free_names_of_handler : _ Or_unknown.t) =
  let num_normal_occurrences_of_params =
    match free_names_of_handler with
    | Unknown -> Variable.Map.empty
    | Known free_names_of_handler ->
      ListLabels.fold_left params
        ~init:Variable.Map.empty
        ~f:(fun num_occurrences param ->
          let var = Kinded_parameter.var param in
          let num =
            Name_occurrences.count_variable_normal_mode
              free_names_of_handler var
          in
          Variable.Map.add var num num_occurrences)
  in
  let t0 : T0.t =
    { num_normal_occurrences_of_params;
      handler;
    }
  in
  create params t0

let pattern_match' t ~f =
  pattern_match t
    ~f:(fun params { handler; num_normal_occurrences_of_params; } ->
      f params ~num_normal_occurrences_of_params ~handler)

let pattern_match t ~f =
  pattern_match t
    ~f:(fun params { handler; num_normal_occurrences_of_params = _; } ->
      f params ~handler)

module Pattern_match_pair_error = struct
  type t = Parameter_lists_have_different_lengths

  let to_string = function
    | Parameter_lists_have_different_lengths ->
      "Parameter lists have different lengths"
end

let pattern_match_pair t1 t2 ~f =
  pattern_match t1 ~f:(fun params1 ~handler:_ ->
    pattern_match t2 ~f:(fun params2 ~handler:_ ->
      (* CR lmaurer: Should this check be done by
         [Name_abstraction.Make_list]? *)
      if List.compare_lengths params1 params2 = 0 then
        pattern_match_pair t1 t2 ~f:(
          fun params { handler = handler1; _ } { handler = handler2; _ } ->
            Ok (f params ~handler1 ~handler2))
      else
        Error Pattern_match_pair_error.Parameter_lists_have_different_lengths))
