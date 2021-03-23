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
  type t = Expr.continuation_handlers'

  let print_with_cache ~cache ppf
        { handler; num_normal_occurrences_of_params = _; } =
    fprintf ppf "@[<hov 1>(\
        @[<hov 1>(handler@ %a)@]\
        )@]"
      (Expr.print_with_cache ~cache) handler

  let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let free_names = Expr.free_names_continuation_handlers'
  let apply_renaming = Expr.apply_renaming_continuation_handlers'
  let all_ids_for_export = Expr.all_ids_for_export_continuation_handlers'
end

module A = Name_abstraction.Make_list (Kinded_parameter) (T0)

type t = Expr.continuation_handlers

let invariant _env _t = ()

let create params ~handler ~(free_names_of_handler : _ Or_unknown.t)
      ~is_exn_handler =
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
  let abst = A.create params t0 in
  { abst;
    is_exn_handler;
  }

let pattern_match' t ~f =
  A.pattern_match t.abst
    ~f:(fun params { handler; num_normal_occurrences_of_params; } ->
      f params ~num_normal_occurrences_of_params ~handler)

let pattern_match t ~f =
  A.pattern_match t.abst
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
        A.pattern_match_pair t1.abst t2.abst ~f:(
          fun params { handler = handler1; _ } { handler = handler2; _ } ->
            Ok (f params ~handler1 ~handler2))
      else
        Error Pattern_match_pair_error.Parameter_lists_have_different_lengths))

let print_using_where_with_cache (recursive : Recursive.t) ~cache ppf k
      ({ abst = _; is_exn_handler; } as t) ~first =
  let fprintf = Format.fprintf in
  if not first then begin
    fprintf ppf "@ "
  end;
  pattern_match t ~f:(fun params ~handler ->
    begin match Expr.descr handler with
    | Apply_cont _ | Invalid _ -> fprintf ppf "@[<hov 1>"
    | _ -> fprintf ppf "@[<v 1>"
    end;
    fprintf ppf "@<0>%s%a@<0>%s%s@<0>%s%s@<0>%s"
      (Flambda_colours.continuation_definition ())
      Continuation.print k
      (Flambda_colours.expr_keyword ())
      (match recursive with Non_recursive -> "" | Recursive -> " (rec)")
      (Flambda_colours.continuation_annotation ())
      (if is_exn_handler then "[eh]" else "")
      (Flambda_colours.normal ());
    if List.length params > 0 then begin
      fprintf ppf " %a" Kinded_parameter.List.print params
    end;
    fprintf ppf "@<0>%s:@<0>%s@ %a"
      (Flambda_colours.elide ())
      (Flambda_colours.normal ())
      (Expr.print_with_cache ~cache) handler;
    fprintf ppf "@]")

let print_with_cache ~cache ppf { abst; is_exn_handler; } =
  Format.fprintf ppf "@[<hov 1>\
      @[<hov 1>(params_and_handler@ %a)@]@ \
      @[<hov 1>(is_exn_handler@ %b)@]\
      @]"
    (A.print_with_cache ~cache) abst
    is_exn_handler

let print ppf t =
  print_with_cache ~cache:(Printing_cache.create ()) ppf t

let is_exn_handler t = t.is_exn_handler

let free_names t = A.free_names t.abst

let apply_renaming ({ abst; is_exn_handler; } as t) perm =
  let abst' = A.apply_renaming abst perm in
  if abst == abst' then t
  else
    { abst = abst';
      is_exn_handler;
    }

let all_ids_for_export { abst; is_exn_handler = _; } =
  A.all_ids_for_export abst
