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

module Behaviour = struct
  type t =
    | Unreachable of { arity : Flambda_arity.With_subkinds.t; }
    | Alias_for of {
        arity : Flambda_arity.With_subkinds.t;
        alias_for : Continuation.t;
      }
    | Unknown of { arity : Flambda_arity.With_subkinds.t; }

  let apply_renaming t perm =
    match t with
    | Unreachable { arity = _; }
    | Unknown { arity = _; } -> t
    | Alias_for { arity; alias_for; } ->
      let alias_for' = Renaming.apply_continuation perm alias_for in
      if alias_for == alias_for' then t
      else Alias_for { arity; alias_for = alias_for'; }

  let arity t =
    match t with
    | Unreachable { arity; }
    | Unknown { arity; }
    | Alias_for { arity; alias_for = _; } -> arity

  let all_ids_for_export t =
    match t with
    | Unreachable { arity = _; }
    | Unknown { arity = _; } -> Ids_for_export.empty
    | Alias_for { arity = _; alias_for; } ->
      Ids_for_export.singleton_continuation alias_for
end

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

  let apply_renaming
        ({ handler; num_normal_occurrences_of_params; } as t) perm =
    let handler' =
      Expr.apply_renaming handler perm
    in
    if handler == handler' then t
    else { handler = handler'; num_normal_occurrences_of_params; }

  let all_ids_for_export { handler; num_normal_occurrences_of_params = _; } =
    Expr.all_ids_for_export handler
end

module A = Name_abstraction.Make_list (Kinded_parameter) (T0)

type t = {
  abst : A.t;
  behaviour : Behaviour.t;
  is_exn_handler : bool;
}

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
  let behaviour : Behaviour.t =
    (* CR-someday mshinwell: This could be replaced by a more sophisticated
       analysis, but for the moment we just use a simple syntactic check. *)
    let arity = Kinded_parameter.List.arity_with_subkinds params in
    if is_exn_handler then
      Unknown { arity; }
    else
      match Expr.descr handler with
      | Apply_cont apply_cont ->
        begin match Apply_cont.trap_action apply_cont with
        | Some _ -> Unknown { arity; }
        | None ->
          let args = Apply_cont.args apply_cont in
          let params = List.map KP.simple params in
          if Misc.Stdlib.List.compare Simple.compare args params = 0 then
            Alias_for {
              arity;
              alias_for = Apply_cont.continuation apply_cont;
            }
          else
            Unknown { arity; }
        end
      | Invalid Treat_as_unreachable -> Unreachable { arity; }
      | _ -> Unknown { arity; }
  in
  let t0 : T0.t =
    { num_normal_occurrences_of_params;
      handler;
    }
  in
  let abst = A.create params t0 in
  { abst;
    behaviour;
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
      ({ abst = _; behaviour = _; is_exn_handler; } as t) ~first =
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

let print_with_cache ~cache ppf { abst; behaviour = _; is_exn_handler; } =
  Format.fprintf ppf "@[<hov 1>\
      @[<hov 1>(params_and_handler@ %a)@]@ \
      @[<hov 1>(is_exn_handler@ %b)@]\
      @]"
    (A.print_with_cache ~cache) abst
    is_exn_handler

let print ppf t =
  print_with_cache ~cache:(Printing_cache.create ()) ppf t

let is_exn_handler t = t.is_exn_handler

let arity t = Behaviour.arity t.behaviour

let behaviour t = t.behaviour

let free_names t = A.free_names t.abst

let apply_renaming ({ abst; behaviour; is_exn_handler; } as t) perm =
  let abst' = A.apply_renaming abst perm in
  let behaviour' = Behaviour.apply_renaming behaviour perm in
  if abst == abst' && behaviour == behaviour' then t
  else
    { abst = abst';
      behaviour = behaviour';
      is_exn_handler;
    }

let all_ids_for_export { abst; behaviour; is_exn_handler = _; } =
  Ids_for_export.union (A.all_ids_for_export abst)
    (Behaviour.all_ids_for_export behaviour)
