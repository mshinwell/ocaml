(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017--2018 OCamlPro SAS                                    *)
(*   Copyright 2017--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR-someday mshinwell: If we can get the types in [Flambda] to be uniform
   enough between functions and continuations then we can probably share
   code for doing remove-unused-parameters. *)

(* CR mshinwell: Add support for phantom parameters *)

(* XXX This also needs to remove unused continuation params from the return
   continuations in toplevel symbol definitions.
*)

module T = Flambda_type

let remove_parameters ~(handler : Flambda.Continuation_handler.t)
        ~to_remove : Flambda.Expr.with_wrapper =
  let params_to_kinds =
    List.fold_left (fun params_to_kinds typed_param ->
        let param = Flambda.Typed_parameter.param typed_param in
        let ty = Flambda.Typed_parameter.ty typed_param in
        let kind = T.kind ty in
        Parameter.Map.add param kind params_to_kinds)
      Parameter.Map.empty
      handler.params
  in
  let freshened_params =
    List.map (fun param -> param, Flambda.Typed_parameter.rename param)
      handler.params
  in
  let wrapper_params =
    List.map (fun (_param, freshened_param) -> freshened_param)
      freshened_params
  in
  let wrapper_params_not_unused =
    Misc.Stdlib.List.filter_map (fun (param, freshened_param) ->
        if Parameter.Set.mem (Flambda.Typed_parameter.param param) to_remove
        then None
        else
          let var = Flambda.Typed_parameter.var freshened_param in
          Some (Simple.var var))
      freshened_params
  in
  let new_params =
    List.filter (fun param ->
        not (Parameter.Set.mem (Flambda.Typed_parameter.param param) to_remove))
      handler.params
  in
  let new_cont = Continuation.create () in
  let wrapper_handler : Flambda.Continuation_handler.t =
    { params = wrapper_params;
      stub = false; (*true; *) (* XXX *)
      is_exn_handler = false;
      handler = Apply_cont (new_cont, None, wrapper_params_not_unused);
    }
  in
  assert (not handler.is_exn_handler);
  let new_handler =
    Parameter.Set.fold (fun param body ->
        let kind = Parameter.Map.find param params_to_kinds in
        let dummy_value = Flambda.Named.dummy_value kind in
        Flambda.Expr.create_let (Parameter.var param) kind
          dummy_value body)
      to_remove
      handler.handler
  in
  let new_handler : Flambda.Continuation_handler.t =
    { params = new_params;
      stub = handler.stub;
      is_exn_handler = false;
      handler = new_handler;
    }
  in
  With_wrapper {
    new_cont;
    new_handler;
    wrapper_handler;
  }

let for_continuation ~body ~unused
      ~(handlers : Flambda.Continuation_handlers.t)
      ~original ~recursive : Flambda.Expr.t =
  if Parameter.Set.is_empty unused then
    original
  else
    let handlers =
      Continuation.Map.fold (fun cont
              (handler : Flambda.Continuation_handler.t) handlers ->
          let to_remove =
            Parameter.Set.inter unused
              (Flambda.Typed_parameter.List.param_set handler.params)
          in
          let with_wrapper : Flambda.Expr.with_wrapper =
            if handler.stub || Parameter.Set.is_empty to_remove then
              Unchanged { handler; }
            else
              remove_parameters ~handler ~to_remove
          in
          Continuation.Map.add cont with_wrapper handlers)
        handlers
        Continuation.Map.empty
    in
    Flambda.Expr.build_let_cont_with_wrappers ~body ~recursive
      ~with_wrappers:handlers

let run program ~backend:_ =
  (* CR mshinwell: This is very inefficient, given the deeply-nested
     continuation structures that are typical.  The continuation declarations
     should probably be added one by one as they are encountered inside
     [Invariant_params] itself. *)
  Flambda_static.Program.Mappers.map_toplevel_exprs program
    ~f:(fun ~continuation_arity:_ _cont expr ->
      Flambda.Expr.Mappers.map_expr (fun (expr : Flambda.Expr.t) ->
          match expr with
          | Let_cont { body = _; handlers = Non_recursive {
              name = _; handler = {is_exn_handler = true; _ }; }; } -> expr
          | Let_cont { body; handlers = Non_recursive { name; handler; } } ->
            let unused =
              let free_names = Flambda.Expr.free_names handler.handler in
              let params =
                Flambda.Typed_parameter.List.param_set handler.params
              in
              Parameter.Set.filter (fun param ->
                  let param = Name.var (Parameter.var param) in
                  not (Name_occurrences.mem free_names param))
                params
            in
            let handlers =
              Continuation.Map.add name handler Continuation.Map.empty
            in
            for_continuation ~body ~handlers ~unused ~original:expr
              ~recursive:Non_recursive
          | Let_cont { body; handlers = Recursive handlers; } ->
            ignore body;
            ignore handlers;
            (* XXX fix this *)
            expr
  (*
            let unused =
              Invariant_params.Continuations.unused_arguments handlers ~backend
            in
            let unused = Parameter.Set.wrap unused in
            for_continuation ~body ~handlers ~unused ~original:expr
              ~recursive:Flambda.Recursive
  *)
          | Let _ | Let_mutable _ | Apply _ | Apply_cont _ | Switch _
          | Invalid _ -> expr)
        expr)
