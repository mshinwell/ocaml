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

type descr =
  | Let of Let_expr.t
  | Let_cont of Let_cont_expr.t
  | Apply of Apply.t
  | Apply_cont of Apply_cont.t
  | Switch of Switch.t
  | Invalid of Invalid_term_semantics.t

type free_names =
  | Ok of Name_occurrences.t
  | Not_computed

type t = {
  descr : descr;
  free_names : free_names;
  delayed_permutation : Name_permutation.t;
  (* [delayed_permutation] must be applied to both [descr] and [free_names]
     before they are used. *)
  (* CR mshinwell: we should maybe try to statically enforce this *)
}

let apply_name_permutation t perm =
  let delayed_permutation =
    Name_permutation.compose ~second:perm ~first:t.delayed_permutation
  in
  { t with
    delayed_permutation;
  }

let descr t =
  let perm = t.delayed_permutation in
  if Name_permutation.is_empty perm then
    t.descr
  else
    match t.descr with
    | Let let_expr ->
      let let_expr' = Let_expr.apply_name_permutation let_expr perm in
      if let_expr == let_expr' then t.descr
      else Let let_expr'
    | Let_cont let_cont ->
      let let_cont' = Let_cont_expr.apply_name_permutation let_cont perm in
      if let_cont == let_cont' then t.descr
      else Let_cont let_cont'
    | Apply apply ->
      let apply' = Apply.apply_name_permutation apply perm in
      if apply == apply' then t.descr
      else Apply apply'
    | Apply_cont apply_cont ->
      let apply_cont' = Apply_cont.apply_name_permutation apply_cont perm in
      if apply_cont == apply_cont' then t.descr
      else Apply_cont apply_cont'
    | Switch switch ->
      let switch' = Switch.apply_name_permutation switch perm in
      if switch == switch' then t.descr
      else Switch switch'
    | Invalid _ -> t.descr

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

let free_names t =
  match t.free_names with
  | Ok free_names ->
    Name_occurrences.apply_name_permutation free_names t.delayed_permutation
  | Not_computed ->
    match descr t with
    | Let let_expr -> Let_expr.free_names let_expr
    | Let_cont let_cont -> Let_cont_expr.free_names let_cont
    | Apply apply -> Apply.free_names apply
    | Apply_cont apply_cont -> Apply_cont.free_names apply_cont
    | Switch switch -> Switch.free_names switch
    | Invalid _ -> Name_occurrences.empty

let create descr =
  { descr;
    delayed_permutation = Name_permutation.empty;
    free_names = Not_computed;
  }

type let_creation_result =
  | Have_deleted of Named.t
  | Nothing_deleted

let create_singleton_let (bound_var : Var_in_binding_pos.t) defining_expr body
      : t * let_creation_result =
  begin match !Clflags.dump_flambda_let with
  | None -> ()
  | Some stamp ->
    Variable.debug_when_stamp_matches (Var_in_binding_pos.var bound_var) ~stamp
      ~f:(fun () ->
        Printf.eprintf "Creation of [Let] with stamp %d:\n%s\n%!"
          stamp
          (Printexc.raw_backtrace_to_string (Printexc.get_callstack max_int)))
  end;
  let free_names_of_body = free_names body in
  (* CR mshinwell: [let_creation_result] should really be some kind of
     "benefit" type. *)
  let bound_var, keep_binding, let_creation_result =
    let greatest_occurrence_kind =
      Name_occurrences.greatest_occurrence_kind_var free_names_of_body
        (Var_in_binding_pos.var bound_var)
    in
    let declared_occurrence_kind =
      Var_in_binding_pos.occurrence_kind bound_var
    in
(*
Format.eprintf "%a: greatest mode %a, declared mode %a, free names %a, body:@ %a\n%!"
  Var_in_binding_pos.print bound_var
  Name_occurrence_kind.Or_absent.print greatest_occurrence_kind
  Name_occurrence_kind.print declared_occurrence_kind
  Name_occurrences.print free_names_of_body
  print body;
*)
    begin match
      Name_occurrence_kind.Or_absent.compare_partial_order
         greatest_occurrence_kind
         (Name_occurrence_kind.Or_absent.present declared_occurrence_kind)
    with
    | None -> ()
    | Some c ->
      if c <= 0 then ()
      else
        Misc.fatal_errorf "[Let]-binding declares variable %a (mode %a) to \
            be bound to@ %a,@ but this variable has occurrences at a higher \
            mode@ (>= %a)@ in the body (free names %a):@ %a"
          Var_in_binding_pos.print bound_var
          Name_occurrence_kind.print declared_occurrence_kind
          Named.print defining_expr
          Name_occurrence_kind.Or_absent.print greatest_occurrence_kind
          Name_occurrences.print free_names_of_body
          print body
    end;
    if not (Named.at_most_generative_effects defining_expr) then begin
      if not (Name_occurrence_kind.is_normal declared_occurrence_kind)
      then begin
        Misc.fatal_errorf "Cannot [Let]-bind non-normal variable to \
            a primitive that has more than generative effects:@ %a@ =@ %a"
          Var_in_binding_pos.print bound_var
          Named.print defining_expr
      end;
      bound_var, true, Nothing_deleted
    end else begin
      let has_uses =
        Name_occurrence_kind.Or_absent.is_present greatest_occurrence_kind
      in
      let uses_are_at_most_phantom = (* CR mshinwell: rename? *)
        (* CR mshinwell: This should detect whether there is any
           provenance info associated with the variable.  If there isn't, the
           [Let] can be deleted even if debugging information is being
           generated. *)
        match
          Name_occurrence_kind.Or_absent.compare_partial_order
            greatest_occurrence_kind
            (Name_occurrence_kind.Or_absent.present
              Name_occurrence_kind.normal)
        with
        | None -> assert false
        | Some c -> c < 0
      in
      let user_visible =
        Variable.user_visible (Var_in_binding_pos.var bound_var)
      in
      let will_delete_binding =
        (* CR mshinwell: This should detect whether there is any
           provenance info associated with the variable.  If there isn't, the
           [Let] can be deleted even if debugging information is being
           generated. *)
        uses_are_at_most_phantom
          && (not (!Clflags.debug && (has_uses || user_visible)))
      in
      if will_delete_binding then begin
(*
Format.eprintf "Deleting binding of %a; free names of body are:@ %a\n%!"
  Var_in_binding_pos.print bound_var
  Name_occurrences.print free_names_of_body;
*)
        bound_var, false, Have_deleted defining_expr
      end else
        let occurrence_kind =
          match greatest_occurrence_kind with
          | Absent -> Name_occurrence_kind.phantom
          | Present occurrence_kind -> occurrence_kind
        in
        assert (Name_occurrence_kind.can_be_in_terms occurrence_kind);
        let bound_var =
          Var_in_binding_pos.with_occurrence_kind bound_var occurrence_kind
        in
        if Name_occurrence_kind.is_normal occurrence_kind then
          bound_var, true, Nothing_deleted
        else
          bound_var, true, Have_deleted defining_expr
    end
  in
  (* CR mshinwell: When leaving behind phantom lets, maybe we should turn
     the defining expressions into simpler ones by using the type, if possible.
     For example an Unbox_naked_int64 or something could potentially turn
     into a variable.  This defining expression usually never exists as
     the types propagate the information forward. *)
  if not keep_binding then body, let_creation_result
  else
    let bound_vars = Bindable_let_bound.singleton bound_var in
    let let_expr = Let_expr.create ~bound_vars ~defining_expr ~body in
    let free_names =
      let from_defining_expr =
        Name_occurrences.downgrade_occurrences_at_strictly_greater_kind
          (Named.free_names defining_expr)
          (Var_in_binding_pos.occurrence_kind bound_var)
      in
      (* We avoid [Let_expr.free_names] since we already know the free names
         of [body] -- and calling that function would cause an abstraction
         to be opened. *)
      Name_occurrences.union from_defining_expr
        (Name_occurrences.remove_var free_names_of_body
          (Var_in_binding_pos.var bound_var))
    in
(*
Format.eprintf "Free names %a for new let expr:@ %a\n%!"
  Name_occurrences.print free_names
  Let_expr.print let_expr;
*)
    let t =
      { descr = Let let_expr;
        delayed_permutation = Name_permutation.empty;
        free_names = Ok free_names;
      }
    in
    t, Nothing_deleted

let create_set_of_closures_let ~closure_vars defining_expr body
      : t * let_creation_result =
  (* CR mshinwell: Think about when these kinds of [Let]s can be
     phantomised / removed *)
  let bound_vars = Bindable_let_bound.set_of_closures ~closure_vars in
  let let_expr = Let_expr.create ~bound_vars ~defining_expr ~body in
  let free_names = Let_expr.free_names let_expr in
  let t =
    { descr = Let let_expr;
      delayed_permutation = Name_permutation.empty;
      free_names = Ok free_names;
    }
  in
  t, Nothing_deleted

let create_pattern_let0 (bound_vars : Bindable_let_bound.t) defining_expr body
      : t * let_creation_result =
  match bound_vars with
  | Singleton bound_var -> create_singleton_let bound_var defining_expr body
  | Set_of_closures { closure_vars; _ } ->
    create_set_of_closures_let ~closure_vars defining_expr body

let create_let bound_var defining_expr body : t =
  let expr, _ = create_singleton_let bound_var defining_expr body in
  expr

let create_pattern_let bound_vars defining_expr body : t =
  let expr, _ = create_pattern_let0 bound_vars defining_expr body in
  expr

let create_let_cont let_cont = create (Let_cont let_cont)
let create_apply apply = create (Apply apply)
let create_apply_cont apply_cont = create (Apply_cont apply_cont)

let create_invalid () =
  if !Clflags.treat_invalid_code_as_unreachable then
    create (Invalid Treat_as_unreachable)
  else
    create (Invalid Halt_and_catch_fire)

type switch_creation_result =
  | Have_deleted_comparison_but_not_branch
  | Have_deleted_comparison_and_branch
  | Nothing_deleted

let create_switch0 sort ~scrutinee ~arms : t * switch_creation_result =
  if Discriminant.Map.cardinal arms < 1 then
    create_invalid (), Have_deleted_comparison_and_branch
  else
    let change_to_goto k =
      create_apply_cont (Apply_cont.goto k),
        Have_deleted_comparison_but_not_branch
    in
    match Discriminant.Map.get_singleton arms with
    | Some (_discriminant, k) -> change_to_goto k
    | None ->
      (* CR mshinwell: We should do a partial invariant check here (one
         which doesn't require [Invariant_env.t]. *)
      let destinations =
        Continuation.Set.of_list (Discriminant.Map.data arms)
      in
      match Continuation.Set.get_singleton destinations with
      | Some k -> change_to_goto k
      | None ->
        let switch = Switch.create sort ~scrutinee ~arms in
        create (Switch switch), Nothing_deleted

let create_switch sort ~scrutinee ~arms =
  let expr, _ = create_switch0 sort ~scrutinee ~arms in
  expr

let create_if_then_else ~scrutinee ~if_true ~if_false =
  let arms =
    Discriminant.Map.of_list [
      Discriminant.bool_true, if_true;
      Discriminant.bool_false, if_false;
    ]
  in
  create_switch Int ~scrutinee ~arms

let bind ~bindings ~body =
  List.fold_left (fun expr (var, (target : Named.t)) ->
(*
      match target with
      | Simple simple ->
        begin match Simple.descr simple with
        | Name (Var rhs_var) ->
          begin match Simple.rec_info simple with
          | None ->
            let perm =
              Can't do this unless the name modes match!
              Name_permutation.add_variable Name_permutation.empty
                (Var_in_binding_pos.var var) rhs_var
            in
            (* CR mshinwell: Think more about this.
               This is still leaving some let-bindings behind when it
               shouldn't need to, e.g.
                 let bar = foo in
                 switch foo ...
               seems to be turning into
                 let bar = foo in
                 switch bar *)
            create_let var target (apply_name_permutation expr perm)
          | Some _ -> create_let var target expr
          end
        | _ -> create_let var target expr
        end
      | _ -> *) create_let var target expr)
    body
    (List.rev bindings)

let bind_parameters ~bindings ~body =
  let bindings =
    List.map (fun (bind, target) ->
        let var =
          Var_in_binding_pos.create (KP.var bind) Name_occurrence_kind.normal
        in
        var, target)
      bindings
  in
  bind ~bindings ~body

let bind_parameters_to_simples ~bind ~target body =
  if List.compare_lengths bind target <> 0 then begin
    Misc.fatal_errorf "Mismatching parameters and arguments: %a and %a"
      KP.List.print bind
      Simple.List.print target
  end;
  let bindings =
    List.map (fun (bind, target) -> bind, Named.create_simple target)
      (List.combine bind target)
  in
  bind_parameters ~bindings ~body
