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

[@@@ocaml.warning "+a-30-40-41-42"]

open! Flambda.Import

type t = {
  imported_symbols : Flambda_kind.t Symbol.Map.t;
  root_symbol : Symbol.t;
  return_continuation : Continuation.t;
  exn_continuation : Continuation.t;
  body : Flambda.Expr.t;
}

let create ~imported_symbols ~root_symbol ~return_continuation
      ~exn_continuation ~body =
  { imported_symbols;
    root_symbol;
    return_continuation;
    exn_continuation;
    body;
  }

let imported_symbols t = t.imported_symbols
let root_symbol t = t.root_symbol
let return_continuation t = t.return_continuation
let exn_continuation t = t.exn_continuation
let body t = t.body

let print ppf
      { imported_symbols; root_symbol; return_continuation; exn_continuation;
        body;
      } =
  Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(imported_symbols@ %a)@]@ \
        @[<hov 1>(root_symbol@ %a)@]@ \
        @[<hov 1>(return_continuation@ %a)@]@ \
        @[<hov 1>(exn_continuation@ %a)@]@ \
        @[<hov 1>(body@ %a)@]\
      )@]"
    (Symbol.Map.print Flambda_kind.print) imported_symbols
    Symbol.print root_symbol
    Continuation.print return_continuation
    Continuation.print exn_continuation
    Flambda.Expr.print body

let invariant _t = ()

let used_closure_vars t =
  Name_occurrences.closure_vars (Flambda.Expr.free_names t.body)

(* Iter on all sets of closures of a given program. *)
(* CR mshinwell: These functions should be pushed directly into [Flambda] *)
module Iter_sets_of_closures = struct
  let rec expr f e =
    match (Expr.descr e : Expr.descr) with
    | Let e' -> let_expr f e'
    | Let_symbol e' -> let_symbol f e'
    | Let_cont e' -> let_cont f e'
    | Apply e' -> apply_expr f e'
    | Apply_cont e' -> apply_cont f e'
    | Switch e' -> switch f e'
    | Invalid e' -> invalid f e'

  and named f n =
    match (n : Named.t) with
    | Simple _ | Prim _ -> ()
    | Set_of_closures s ->
        f None s

  and let_expr f t =
    Let.pattern_match t ~f:(fun ~bound_vars:_ ~body ->
        let e = Let.defining_expr t in
        named f e;
        expr f body
      )

  and let_symbol_expr f let_sym =
    static_const f (Let_symbol.bound_symbols let_sym)
      (Let_symbol.defining_expr let_sym);
    expr f (Let_symbol.body)

  and let_cont f = function
    | Let_cont.Non_recursive { handler; _ } ->
        Non_recursive_let_cont_handler.pattern_match handler ~f:(fun k ~body ->
            let h = Non_recursive_let_cont_handler.handler handler in
            let_cont_aux f k h body
          )
    | Let_cont.Recursive handlers ->
        Recursive_let_cont_handlers.pattern_match handlers ~f:(fun ~body conts ->
            assert (not (Continuation_handlers.contains_exn_handler conts));
            let_cont_rec f conts body
          )

  and let_cont_aux f k h body =
    continuation_handler f k h;
    expr f body

  and let_cont_rec f conts body =
    let map = Continuation_handlers.to_map conts in
    Continuation.Map.iter (continuation_handler f) map;
    expr f body

  and continuation_handler f _ h =
    let h = Continuation_handler.params_and_handler h in
    Continuation_params_and_handler.pattern_match h ~f:(fun _ ~handler ->
        expr f handler
      )

  (* Expression application, continuation application and Switches
     only use single expressions and continuations, so no sets_of_closures
     can syntatically appear inside. *)
  and apply_expr _ _ = ()

  and apply_cont _ _ = ()

  and switch _ _ = ()

  and invalid _ _ = ()

  (* and fun_decl _f _ _decl = assert false  (\* FIXME Let code *\) *)
(*
    let t = Function_declaration.params_and_body decl in
    Function_params_and_body.pattern_match t
      ~f:(fun ~return_continuation:_ _exn_k _args ~body ~my_closure:_ ->
          expr f body
        )
*)

  and static_const f
        (bound_symbols : Let_symbol.Bound_symbols.t)
        (static_const : Static_const.t) =
    match bound_symbols, st with
    | Code_and_set_of_closures { code_ids = _; closure_symbols; },
      Code_and_set_of_closures { code; set_of_closures = s; } ->
        Option.iter (fun s -> f (Some closure_symbols) s) s;
        Code_id.Map.iter (fun _ { Flambda_static.Static_part.params_and_body;
                                  newer_version_of = _; } ->
            match params_and_body with
            | Deleted -> ()
            | Present params_and_body ->
                Function_params_and_body.pattern_match params_and_body
                  ~f:(fun ~return_continuation:_ _ _ ~body ~my_closure:_ ->
                      expr f body))
          code
    | _ ->
      (* CR mshinwell: Make exhaustive and cause errors on wrong cases *)
      ()
end

let iter_sets_of_closures t ~f =
  Iter_sets_of_closures.expr f t.body
