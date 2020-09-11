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

module Descr = struct
  type t =
    | Let of Let_expr.t
    | Let_cont of Let_cont_expr.t
    | Apply of Apply.t
    | Apply_cont of Apply_cont.t
    | Switch of Switch.t
    | Invalid of Invalid_term_semantics.t

  let free_names t =
    match t with
    | Let let_expr -> Let_expr.free_names let_expr
    | Let_cont let_cont -> Let_cont_expr.free_names let_cont
    | Apply apply -> Apply.free_names apply
    | Apply_cont apply_cont -> Apply_cont.free_names apply_cont
    | Switch switch -> Switch.free_names switch
    | Invalid _ -> Name_occurrences.empty

  let apply_name_permutation t perm =
    match t with
    | Let let_expr ->
      let let_expr' = Let_expr.apply_name_permutation let_expr perm in
      if let_expr == let_expr' then t
      else Let let_expr'
    | Let_cont let_cont ->
      let let_cont' = Let_cont_expr.apply_name_permutation let_cont perm in
      if let_cont == let_cont' then t
      else Let_cont let_cont'
    | Apply apply ->
      let apply' = Apply.apply_name_permutation apply perm in
      if apply == apply' then t
      else Apply apply'
    | Apply_cont apply_cont ->
      let apply_cont' = Apply_cont.apply_name_permutation apply_cont perm in
      if apply_cont == apply_cont' then t
      else Apply_cont apply_cont'
    | Switch switch ->
      let switch' = Switch.apply_name_permutation switch perm in
      if switch == switch' then t
      else Switch switch'
    | Invalid _ -> t
end

(* CR mshinwell: Work out how to use [With_delayed_permutation] here.
   There were some problems with double vision etc. last time.  Although
   we don't want to cache free names here. *)

type t = {
  mutable descr : Descr.t;
  mutable delayed_permutation : Name_permutation.t;
}

type descr = Descr.t =
  | Let of Let_expr.t
  | Let_cont of Let_cont_expr.t
  | Apply of Apply.t
  | Apply_cont of Apply_cont.t
  | Switch of Switch.t
  | Invalid of Invalid_term_semantics.t

let create descr =
  { descr;
    delayed_permutation = Name_permutation.empty;
  }

let peek_descr t = t.descr

let descr t =
  if Name_permutation.is_empty t.delayed_permutation then begin
    t.descr
  end else begin
    let descr = Descr.apply_name_permutation t.descr t.delayed_permutation in
    t.descr <- descr;
    t.delayed_permutation <- Name_permutation.empty;
    descr
  end

let apply_name_permutation t perm =
  let delayed_permutation =
    Name_permutation.compose ~second:perm ~first:t.delayed_permutation
  in
  { t with
    delayed_permutation;
  }

let free_names t = Descr.free_names (descr t)

let all_ids_for_export t =
  match descr t with
  | Let let_expr -> Let_expr.all_ids_for_export let_expr
  | Let_cont let_cont -> Let_cont_expr.all_ids_for_export let_cont
  | Apply apply -> Apply.all_ids_for_export apply
  | Apply_cont apply_cont -> Apply_cont.all_ids_for_export apply_cont
  | Switch switch -> Switch.all_ids_for_export switch
  | Invalid _ -> Ids_for_export.empty

let import import_map t =
  let descr =
    match descr t with
    | Let let_expr -> Let (Let_expr.import import_map let_expr)
    | Let_cont let_cont -> Let_cont (Let_cont_expr.import import_map let_cont)
    | Apply apply -> Apply (Apply.import import_map apply)
    | Apply_cont apply_cont ->
      Apply_cont (Apply_cont.import import_map apply_cont)
    | Switch switch -> Switch (Switch.import import_map switch)
    | Invalid sem -> Invalid sem
  in
  create descr

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

type switch_creation_result =
  | Have_deleted_comparison_but_not_branch
  | Have_deleted_comparison_and_branch
  | Nothing_deleted

let create_switch0 ~scrutinee ~arms : t * switch_creation_result =
  if Target_imm.Map.cardinal arms < 1 then
    create_invalid (), Have_deleted_comparison_and_branch
  else
    let change_to_apply_cont action =
      create_apply_cont action, Have_deleted_comparison_but_not_branch
    in
    match Target_imm.Map.get_singleton arms with
    | Some (_discriminant, action) -> change_to_apply_cont action
    | None ->
      (* CR mshinwell: We should do a partial invariant check here (one
         which doesn't require [Invariant_env.t]. *)
      let actions =
        Apply_cont_expr.Set.of_list (Target_imm.Map.data arms)
      in
      match Apply_cont_expr.Set.get_singleton actions with
      | Some action -> change_to_apply_cont action
      | None ->
        let switch = Switch.create ~scrutinee ~arms in
        create (Switch switch), Nothing_deleted

let create_switch ~scrutinee ~arms =
  let expr, _ = create_switch0 ~scrutinee ~arms in
  expr

let create_if_then_else ~scrutinee ~if_true ~if_false =
  let arms =
    Target_imm.Map.of_list [
      Target_imm.bool_true, if_true;
      Target_imm.bool_false, if_false;
    ]
  in
  create_switch ~scrutinee ~arms

let bind_no_simplification ~bindings ~body =
  ListLabels.fold_left (List.rev bindings)
    ~init:body
    ~f:(fun expr (var, defining_expr) ->
      Let.create (Bindable_let_bound.singleton var)
        defining_expr
        ~body:expr
        ~free_names_of_body:Unknown
      |> create_let)

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
      Let.create (Bindable_let_bound.singleton var)
        (Named.create_simple arg)
        ~body:expr
        ~free_names_of_body:Unknown
      |> create_let)
