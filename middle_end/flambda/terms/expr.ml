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

module NA = Name_abstraction

type expr = {
  mutable descr : expr_descr;
  mutable delayed_permutation : Renaming.t;
}

and expr_descr =
  | Let of let_expr
  | Let_cont of let_cont_expr
  | Apply of Apply_expr.t
  | Apply_cont of Apply_cont_expr.t
  | Switch of Switch_expr.t
  | Invalid of Invalid_term_semantics.t

and let_expr = {
  name_abstraction : (Bindable_let_bound.t, let_expr') NA.t;
  defining_expr : Named.t;
}

and let_expr' = {
  num_normal_occurrences_of_bound_vars : Num_occurrences.t Variable.Map.t;
  body : expr;
}

and let_cont_expr =
  | Non_recursive of {
      handler : non_recursive_let_cont_handler;
      num_free_occurrences : Num_occurrences.t Or_unknown.t;
      is_applied_with_traps : bool;
    }
  | Recursive of recursive_let_cont_handler

and non_recursive_let_cont_handler = {
  continuation_and_body : (Bindable_continuation.t, expr) NA.t;
  handler : continuation_handler;
}

and recursive_let_cont_handlers =
  (Bindable_continuation.t, recursive_let_cont_handlers) NA.List.t

and recursive_let_cont_handlers' = {
  handlers : continuation_handlers;
  body : expr;
}

and continuation_handler = {
  abst : (Kinded_parameter.t, continuation_handler') NA.List.t;
  is_exn_handler : bool;
}

and continuation_handler' = {
  num_normal_occurrences_of_params : Num_occurrences.t Variable.Map.t;
  handler : expr;
}

and continuation_handlers = continuation_handler Continuation.Map.t

(* Everything below must use [descr t]; do not just project [t.descr]. *)
let descr expr =
  if Renaming.has_no_action expr.delayed_permutation then begin
    expr.descr
  end else begin
    let descr = Descr.apply_renaming expr.descr expr.delayed_permutation in
    expr.descr <- descr;
    expr.delayed_permutation <- Renaming.empty;
    descr
  end

let apply_renaming expr perm =
  { expr with
    delayed_permutation =
      Renaming.compose ~second:perm ~first:t.delayed_permutation;
  }

let rec apply_renaming_descr descr perm =
  match descr with
  | Let let_expr ->
    let let_expr' = Let_expr.apply_renaming let_expr perm in
    if let_expr == let_expr' then descr
    else Let let_expr'
  | Let_cont let_cont ->
    let let_cont' = Let_cont_expr.apply_renaming let_cont perm in
    if let_cont == let_cont' then descr
    else Let_cont let_cont'
  | Apply apply ->
    let apply' = Apply_expr.apply_renaming apply perm in
    if apply == apply' then descr
    else Apply apply'
  | Apply_cont apply_cont ->
    let apply_cont' = Apply_cont_expr.apply_renaming apply_cont perm in
    if apply_cont == apply_cont' then descr
    else Apply_cont apply_cont'
  | Switch switch ->
    let switch' = Switch_expr.apply_renaming switch perm in
    if switch == switch' then descr
    else Switch switch'
  | Invalid _ -> descr

and apply_renaming_recursive_let_cont_handlers' { handlers; body; } perm =
  { handlers = apply_renaming_continuation_handlers handlers perm;
    body = apply_renaming body perm;
  }

and apply_renaming_continuation_handler { abst; is_exn_handler; } perm =

and apply_renaming_continuation_handler'
      ({ handler; num_normal_occurrences_of_params; } as t) perm =
  let handler' = apply_renaming handler perm in
  if handler == handler' then t
  else { handler = handler'; num_normal_occurrences_of_params; }

and apply_renaming_continuation_handlers cont_handlers perm =
  Continuation.Map.fold (fun k handler result ->
      let k = Renaming.apply_continuation perm k in
      let handler = apply_renaming_continuation_handler handler perm in
      Continuation.Map.add k handler result)
    cont_handlers
    Continuation.Map.empty

and apply_renaming_continuation_handlers'
      ({ handler; num_normal_occurrences_of_params; } as t) perm =
  let handler' = apply_renaming handler perm in
  if handler == handler' then t
  else { handler = handler'; num_normal_occurrences_of_params; }

let rec free_names expr = free_names_descr (descr t)

and free_names_descr descr =
  match descr with
  | Let let_expr -> Let_expr.free_names let_expr
  | Let_cont let_cont -> Let_cont_expr.free_names let_cont
  | Apply apply -> Apply_expr.free_names apply
  | Apply_cont apply_cont -> Apply_cont_expr.free_names apply_cont
  | Switch switch -> Switch_expr.free_names switch
  | Invalid _ -> Name_occurrences.empty

and free_names_continuation_handler cont_handler =

and free_names_continuation_handlers cont_handlers =
  Continuation.Map.fold (fun _k handler free_names ->
      Name_occurrences.union free_names
        (free_names_continuation_handler handler))
    cont_handlers
    (Name_occurrences.empty)

and free_names_continuation_handlers'
      { handler; num_normal_occurrences_of_params = _; } =
  free_names handler

let rec all_ids_for_export_descr descr =

and all_ids_for_export_continuation_handler { abst; is_exn_handler = _; } =
  (* Maybe we should apply the NA functor in this file.
     Although then we need printing functions, which will mean we need
     the full expression printer here.
     Maybe this is ok?  Not that long. *)
  A.all_ids_for_export abst (* XXX *)

and all_ids_for_export_continuation_handlers cont_handlers =
  Continuation.Map.fold (fun k handler ids ->
      Ids_for_export.union ids
        (Ids_for_export.add_continuation
          (all_ids_for_export_continuation_handler handler)
          k))
    cont_handlers
    Ids_for_export.empty

and all_ids_for_export_continuation_handlers'
      { handler; num_normal_occurrences_of_params = _; } =
  all_ids_for_export handler

module Descr = struct
  type t = descr
  let free_names t = free_names_descr
  let apply_renaming = apply_renaming_descr
end

type t = expr

let create descr =
  { descr;
    delayed_permutation = Renaming.empty;
  }

let peek_descr t = t.descr

let all_ids_for_export t =
  match descr t with
  | Let let_expr -> Let_expr.all_ids_for_export let_expr
  | Let_cont let_cont -> Let_cont_expr.all_ids_for_export let_cont
  | Apply apply -> Apply.all_ids_for_export apply
  | Apply_cont apply_cont -> Apply_cont.all_ids_for_export apply_cont
  | Switch switch -> Switch.all_ids_for_export switch
  | Invalid _ -> Ids_for_export.empty

let invariant env t =
  match descr t with
  | Let let_expr -> Let_expr.invariant env let_expr
  | Let_cont let_cont -> Let_cont_expr.invariant env let_cont
  | Apply_cont apply_cont -> Apply_cont.invariant env apply_cont
  | Apply apply -> Apply.invariant env apply
  | Switch switch -> Switch.invariant env switch
  | Invalid _ -> ()

(* CR mshinwell: We might want printing functions that show the delayed
   permutation, etc. *)

let print_with_cache ~cache ppf (t : t) =
  match descr t with
  | Let let_expr -> Let_expr.print_with_cache ~cache ppf let_expr
  | Let_cont let_cont -> Let_cont_expr.print_with_cache ~cache ppf let_cont
  | Apply apply ->
    Format.fprintf ppf "@[<hov 1>(@<0>%sapply@<0>%s@ %a)@]"
      (Flambda_colours.expr_keyword ())
      (Flambda_colours.normal ())
      Apply.print apply
  | Apply_cont apply_cont -> Apply_cont.print ppf apply_cont
  | Switch switch -> Switch.print ppf switch
  | Invalid semantics ->
    fprintf ppf "@[@<0>%sInvalid %a@<0>%s@]"
      (Flambda_colours.expr_keyword ())
      Invalid_term_semantics.print semantics
      (Flambda_colours.normal ())

let print ppf (t : t) =
  print_with_cache ~cache:(Printing_cache.create ()) ppf t

let create_let let_expr = create (Let let_expr)
let create_let_cont let_cont = create (Let_cont let_cont)
let create_apply apply = create (Apply apply)
let create_apply_cont apply_cont = create (Apply_cont apply_cont)
let create_switch switch = create (Switch switch)

let create_invalid ?semantics () =
  let semantics : Invalid_term_semantics.t =
    match semantics with
    | Some semantics ->
      semantics
    | None ->
      if !Clflags.treat_invalid_code_as_unreachable then
        Treat_as_unreachable
      else
        Halt_and_catch_fire
  in
  create (Invalid semantics)

let create_if_then_else ~scrutinee ~if_true ~if_false =
  let arms =
    Target_imm.Map.of_list [
      Target_imm.bool_true, if_true;
      Target_imm.bool_false, if_false;
    ]
  in
  create_switch (Switch_expr.create ~scrutinee ~arms)

let bind_no_simplification ~bindings ~body ~cost_metrics_of_body ~free_names_of_body =
  ListLabels.fold_left (List.rev bindings)
    ~init:(body, cost_metrics_of_body, free_names_of_body)
    ~f:(fun (expr, size, free_names) (var, size_expr, defining_expr) ->
      let expr =
        Let_expr.create (Bindable_let_bound.singleton var)
          defining_expr
          ~body:expr
          ~free_names_of_body:(Known free_names)
        |> create_let
      in
      let free_names =
        Name_occurrences.union (Named.free_names defining_expr)
          (Name_occurrences.remove_var free_names (Var_in_binding_pos.var var))
      in
      let size = Cost_metrics.(+) size size_expr in
      expr, size, free_names)

let bind_parameters_to_args_no_simplification ~params ~args ~body =
  if List.compare_lengths params args <> 0 then begin
    Misc.fatal_errorf "Mismatching parameters and arguments: %a and %a"
      KP.List.print params
      Simple.List.print args
  end;
  ListLabels.fold_left2 (List.rev params) (List.rev args)
    ~init:body
    ~f:(fun expr param arg ->
      let var = Var_in_binding_pos.create (KP.var param) Name_mode.normal in
      Let_expr.create (Bindable_let_bound.singleton var)
        (Named.create_simple arg)
        ~body:expr
        ~free_names_of_body:Unknown
      |> create_let)
