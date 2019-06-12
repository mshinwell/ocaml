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
    Format.fprintf ppf "@[<hov 1>(%sapply%s@ %a)@]"
      (Misc.Color.bold_cyan ())
      (Flambda_colours.normal ())
      Apply.print apply
  | Apply_cont apply_cont -> Apply_cont.print ppf apply_cont
  | Switch switch -> Switch.print ppf switch
  | Invalid semantics ->
    fprintf ppf "@[%sInvalid %a%s@]"
      (Misc.Color.bold_cyan ())
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

let create_let0 bound_var kind defining_expr body : t * let_creation_result =
  begin match !Clflags.dump_flambda_let with
  | None -> ()
  | Some stamp ->
    Variable.debug_when_stamp_matches bound_var ~stamp ~f:(fun () ->
      Printf.eprintf "Creation of [Let] with stamp %d:\n%s\n%!"
        stamp
        (Printexc.raw_backtrace_to_string (Printexc.get_callstack max_int)))
  end;
  let free_names_of_body = free_names body in
  (* If the [Let]-binding is redundant, don't even create it. *)
  if (not (Name_occurrences.mem_var free_names_of_body bound_var))
    && Named.at_most_generative_effects defining_expr
  then
    body, Have_deleted defining_expr
  else
    (* To save space, only keep free names on the outer term. *)
    let body =
      { body with
        free_names = Not_computed;
      }
    in
    let let_expr = Let_expr.create ~bound_var ~kind ~defining_expr ~body in
    let free_names = Let_expr.free_names let_expr in
    let t =
      { descr = Let let_expr;
        delayed_permutation = Name_permutation.empty;
        free_names = Ok free_names;
      }
    in
    t, Nothing_deleted

let create_let bound_var kind defining_expr body : t =
  let expr, _ = create_let0 bound_var kind defining_expr body in
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

let create_switch0 ~scrutinee ~arms : t * switch_creation_result =
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
        let switch = Switch.create ~scrutinee ~arms in
        create (Switch switch), Nothing_deleted

let create_switch ~scrutinee ~arms =
  let expr, _ = create_switch0 ~scrutinee ~arms in
  expr

let create_if_then_else ~scrutinee ~if_true ~if_false =
  let arms =
    Discriminant.Map.of_list [
      Discriminant.bool_true, if_true;
      Discriminant.bool_false, if_false;
    ]
  in
  create_switch ~scrutinee ~arms

let bind ~bindings ~body =
  List.fold_left (fun expr (bound_var, kind, defining_expr) ->
      create_let bound_var kind defining_expr expr)
    body bindings

let bind_parameters_to_simples ~bind ~target t =
  if List.compare_lengths bind target <> 0 then begin
    Misc.fatal_errorf "Lists of differing lengths: %a and %a"
      KP.List.print bind
      Simple.List.print target
  end;
  List.fold_left2 (fun expr bind target ->
      let var = KP.var bind in
      let kind = KP.kind bind in
      create_let var kind (Named.create_simple target) expr)
    t
    (List.rev bind) (List.rev target)

let link_continuations ~bind ~target ~arity t =
  let params =
    List.map (fun kind ->
        let param = Parameter.wrap (Variable.create "param") in
        KP.create param kind)
      arity
  in
  let params_and_handler =
    let apply_cont_target =
      let args = List.map (fun param -> KP.simple param) params in
      Apply_cont.create target ~args
    in
    Continuation_params_and_handler.create params
      ~handler:(create_apply_cont apply_cont_target)
  in
  let handler =
    Continuation_handler.create ~params_and_handler
      ~stub:true
      ~is_exn_handler:false
  in
  Let_cont_expr.create_non_recursive bind handler ~body:t
