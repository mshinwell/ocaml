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

(* CR mshinwell: Maybe make a version of [Var_in_binding_pos] that
   implements [Bindable]? *)
module Bound_var_and_body =
  Name_abstraction.Make (Bindable_variable_in_terms) (Expr)

type t = {
  bound_var_and_body : Bound_var_and_body.t;
  name_occurrence_kind : Name_occurrence_kind.t;
  defining_expr : Named.t;
}

let pattern_match t ~f =
  Bound_vars_and_body.pattern_match t.bound_vars_and_body
    ~f:(fun bound_vars body -> f ~bound_vars ~body)

let print_with_cache ~cache ppf
      ({ bound_var_and_body = _; name_occurrence_kind = _;
         defining_expr; } as t) =
  let let_bound_var_colour var =
    let kind = Var_in_binding_pos.occurrence_kind var in
    if Name_occurrence_kind.is_phantom kind then
      Flambda_colours.elide ()
    else
      Flambda_colours.let_bound_var ()
  in
  let rec let_body (expr : Expr.t) =
    match Expr.descr expr with
    | Let ({ bound_var_and_body = _; name_occurrence_kind = _;
             defining_expr; } as t) ->
      pattern_match t ~f:(fun ~bound_var ~body ->
        fprintf ppf
          "@ @[<hov 1>@<0>%s%a@<0>%s =@<0>%s@ %a@]"
          (let_bound_var_colour bound_var)
          Var_in_binding_pos.print bound_var
          (Flambda_colours.elide ())
          (Flambda_colours.normal ())
          (Named.print_with_cache ~cache) defining_expr;
        let_body body)
    | _ -> expr
  in
  pattern_match t ~f:(fun ~bound_var ~body ->
    fprintf ppf "@[<hov 1>(@<0>%slet@<0>%s@ @[<hov 1>(\
        @<0>%s%a@<0>%s =@<0>%s@ %a"
      (Flambda_colours.expr_keyword ())
      (Flambda_colours.normal ())
      (let_bound_var_colour bound_var)
      Var_in_binding_pos.print bound_var
      (Flambda_colours.elide ())
      (Flambda_colours.normal ())
      (Named.print_with_cache ~cache) defining_expr;
    let expr = let_body body in
    fprintf ppf ")@]@ %a)@]"
      (Expr.print_with_cache ~cache) expr)

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let create ~bound_vars ~defining_expr ~body =
  let bound_vars_and_body = Bound_vars_and_body.create bound_vars body in
  { bound_vars_and_body;
    defining_expr;
  }

let invariant env t =
  let module E = Invariant_env in
  Named.invariant env t.defining_expr;
  pattern_match t ~f:(fun ~bound_vars ~body ->
    let env =
      match t.defining_expr, bound_vars with
      | Set_of_closures _, Set_of_closures { closure_vars; } ->
        Closure_id.Map.fold (fun _closure_id closure_var env ->
            let closure_var = VB.var closure_var in
            E.add_variable env closure_var K.value)
          closure_vars
          env
      | Set_of_closures _, Singleton _ ->
        Misc.fatal_errorf "Cannot bind a [Set_of_closures] to a \
            [Singleton]:@ %a"
          print t
      | _, Set_of_closures _ ->
        Misc.fatal_errorf "Cannot bind a non-[Set_of_closures] to a \
            [Set_of_closures]:@ %a"
          print t
      | Prim (prim, _dbg), Singleton var ->
        let var = VB.var var in
        E.add_variable env var (Flambda_primitive.result_kind' prim)
      | Simple simple, Singleton var ->
        let var = VB.var var in
        match Simple.descr simple with
        | Const const -> E.add_variable env var (T.kind_for_const const)
        | Discriminant _ -> E.add_variable env var K.fabricated
        | Name name -> E.add_variable env var (E.kind_of_name env name)
    in
    Expr.invariant env body)

let defining_expr t = t.defining_expr

let free_names ({ bound_var_and_body = _; name_occurrence_kind;
                  defining_expr; } as t) =
  pattern_match t ~f:(fun ~bound_var ~body ->
    let bound_var = Var_in_binding_pos.var bound_var in
    let from_defining_expr =
      Name_occurrences.downgrade_occurrences_at_strictly_greater_kind
        (Named.free_names defining_expr)
        name_occurrence_kind
    in
    let from_body = Expr.free_names body in
    Name_occurrences.union from_defining_expr
      (Name_occurrences.remove_var from_body bound_var))

let apply_name_permutation ({ bound_vars_and_body; defining_expr; } as t)
      perm =
  let bound_vars_and_body' =
    Bound_vars_and_body.apply_name_permutation bound_vars_and_body perm
  in
  let defining_expr' =
    Named.apply_name_permutation defining_expr perm
  in
  if bound_vars_and_body == bound_vars_and_body'
    && defining_expr == defining_expr'
  then t
  else
    { bound_vars_and_body = bound_vars_and_body';
      defining_expr = defining_expr';
    }
