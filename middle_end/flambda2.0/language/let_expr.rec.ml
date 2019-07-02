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
  Bound_var_and_body.pattern_match t.bound_var_and_body
    ~f:(fun bound_var body ->
      let bound_var =
        Var_in_binding_pos.create bound_var t.name_occurrence_kind
      in
      f ~bound_var ~body)

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

let create ~bound_var ~defining_expr ~body =
  let name_occurrence_kind = Var_in_binding_pos.occurrence_kind bound_var in
  let bound_var = Var_in_binding_pos.var bound_var in
  let bound_var_and_body = Bound_var_and_body.create bound_var body in
  { bound_var_and_body;
    name_occurrence_kind;
    defining_expr;
  }

let invariant env t =
  let module E = Invariant_env in
  pattern_match t ~f:(fun ~bound_var:_ ~body -> Expr.invariant env body)

let defining_expr t = t.defining_expr

let free_names ({ bound_var_and_body = _; name_occurrence_kind = _;
                  defining_expr; } as t) =
  pattern_match t ~f:(fun ~bound_var ~body ->
    let bound_var = Var_in_binding_pos.var bound_var in
    let from_defining_expr = Named.free_names defining_expr in
    let from_body = Expr.free_names body in
    Name_occurrences.union from_defining_expr
      (Name_occurrences.remove_var from_body bound_var))

let apply_name_permutation ({ bound_var_and_body; name_occurrence_kind;
                              defining_expr; } as t) perm =
  let bound_var_and_body' =
    Bound_var_and_body.apply_name_permutation bound_var_and_body perm
  in
  let defining_expr' =
    Named.apply_name_permutation defining_expr perm
  in
  if bound_var_and_body == bound_var_and_body'
    && defining_expr == defining_expr'
  then t
  else
    { bound_var_and_body = bound_var_and_body';
      name_occurrence_kind;
      defining_expr = defining_expr';
    }
