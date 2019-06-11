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

module Bound_var_and_body =
  Name_abstraction.Make (Bindable_variable_in_terms) (Expr)

type t = {
  bound_var_and_body : Bound_var_and_body.t;
  kind : Flambda_kind.t;
  defining_expr : Named.t;
}

let pattern_match t ~f =
  Bound_var_and_body.pattern_match t.bound_var_and_body
    ~f:(fun bound_var body -> f ~bound_var ~body)

let print_with_cache ~cache ppf
      ({ bound_var_and_body = _; kind; defining_expr; } as t) =
  let rec let_body (expr : Expr.t) =
    match Expr.descr expr with
    | Let ({ bound_var_and_body = _; kind; defining_expr; } as t) ->
      pattern_match t ~f:(fun ~bound_var ~body ->
        fprintf ppf "@ @[<hov 1>%a@[@ %s\u{2237}@ %a%s@]@ %a@]"
          Variable.print bound_var
          (Misc.Color.bold_white ())
          Flambda_kind.print kind
          (Misc.Color.reset ())
          (Named.print_with_cache ~cache) defining_expr;
        let_body body)
    | _ -> expr
  in
  pattern_match t ~f:(fun ~bound_var ~body ->
    fprintf ppf "@[<hov 1>(%slet%s@ @[<hov 1>(\
        @[<hov 1>%a@[@ %s\u{2237}@ %a%s@]@ %a@]"
      (Misc.Color.bold_cyan ())
      (Misc.Color.reset ())
      Variable.print bound_var
      (Misc.Color.bold_white ())
      Flambda_kind.print kind
      (Misc.Color.reset ())
      (Named.print_with_cache ~cache) defining_expr;
    let expr = let_body body in
    fprintf ppf ")@]@ %a)@]"
      (Expr.print_with_cache ~cache) expr)

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let create ~bound_var ~kind ~defining_expr ~body =
  let bound_var_and_body = Bound_var_and_body.create bound_var body in
  { bound_var_and_body;
    kind;
    defining_expr;
  }

let invariant env t =
  let module E = Invariant_env in
  pattern_match t ~f:(fun ~bound_var ~body ->
    let named_kind =
      match Named.invariant_returning_kind env t.defining_expr with
      | Singleton kind -> Some kind
      | Unit -> Some K.value
    in
    begin match named_kind with
    | None -> ()
    | Some named_kind ->
      if not (K.equal named_kind t.kind) then begin
        Misc.fatal_errorf "[Let] expression inferred kind (%a)@ is not \
            equal to the annotated kind (%a);@ [Let] expression is:@ %a"
          K.print named_kind
          K.print t.kind
          print t
      end
    end;
    let env = E.add_variable env bound_var t.kind in
    Expr.invariant env body)

let kind t = t.kind
let defining_expr t = t.defining_expr

let free_names ({ bound_var_and_body = _; kind = _; defining_expr; } as t) =
  pattern_match t ~f:(fun ~bound_var ~body ->
    let from_defining_expr = Named.free_names defining_expr in
    let from_body = Expr.free_names body in
    Name_occurrences.union from_defining_expr
      (Name_occurrences.remove_var from_body bound_var))

let apply_name_permutation ({ bound_var_and_body; kind; defining_expr; } as t)
      perm =
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
      kind;
      defining_expr = defining_expr';
    }
