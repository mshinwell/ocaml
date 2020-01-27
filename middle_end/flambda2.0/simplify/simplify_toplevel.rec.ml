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

open! Simplify_import

let simplify_toplevel dacc expr ~return_continuation ~return_arity
      exn_continuation ~return_cont_scope ~exn_cont_scope =
  let expr, dacc, uacc =
    try
      Simplify_expr.simplify_expr dacc expr
        (fun dacc ->
          let uenv =
            UE.add_continuation UE.empty return_continuation
              return_cont_scope return_arity
          in
          let uenv =
            UE.add_exn_continuation uenv exn_continuation
              exn_cont_scope
          in
          dacc, UA.create uenv (DA.code_age_relation dacc) (DA.r dacc))
    with Misc.Fatal_error -> begin
      if !Clflags.flambda2_context_on_error then begin
        Format.eprintf "\n%sContext is:%s simplifying toplevel \
            expression:@ %a@ in downwards accumulator:@ %a"
          (Flambda_colours.error ())
          (Flambda_colours.normal ())
          Expr.print expr
          DA.print dacc
      end;
      raise Misc.Fatal_error
    end
  in
  (* CR mshinwell: add comment about this next part (and also see if it could
     be done more efficiently and/or effectively).  See variant_unboxing.ml
     test case.   It's a shame [compute_handler_env] can't be used, but it
     can't, as the types of the parameters may involve symbols that are
     out of scope. *)
  let uses = Continuation_uses_env.get_uses (DA.continuation_uses_env dacc) in
  match Continuation.Map.find return_continuation uses with
  | exception Not_found -> expr, dacc, UA.r uacc
  | uses ->
    let simples_in_scope simples =
      List.for_all (fun simple ->
          Simple.pattern_match simple
            ~const:(fun _ -> true)
            ~name:(fun name -> TE.mem (DE.typing_env (DA.denv dacc)) name))
        simples
    in
    let simples =
      List.fold_left
        (fun (simples : Simple.t list option Or_bottom.t) use
             : Simple.t list option Or_bottom.t ->
          match simples with
          | Bottom -> Bottom
          | Ok simples ->
            let args = One_continuation_use.args use in
            match simples with
            | None ->
              if simples_in_scope args then Ok (Some args)
              else Bottom
            | Some simples ->
              assert (List.compare_lengths simples args = 0);
              if Misc.Stdlib.List.compare Simple.compare simples args = 0
                && simples_in_scope simples
              then
                Ok (Some simples)
              else
                Bottom)
        (Or_bottom.Ok None)
        (Continuation_uses.get_uses uses)
    in
    match simples with
    | Bottom | Ok None -> expr, dacc, UA.r uacc
    | Ok (Some simples) ->
      let expr =
        Apply_cont.create return_continuation ~args:simples
          ~dbg:Debuginfo.none
        |> Expr.create_apply_cont
      in
      expr, dacc, UA.r uacc
