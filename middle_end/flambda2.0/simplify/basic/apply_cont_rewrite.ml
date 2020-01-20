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

module EA = Continuation_extra_params_and_args.Extra_arg
module KP = Kinded_parameter
module Id = Apply_cont_rewrite_id

type t = {
  original_params : KP.t list;
  used_params : KP.Set.t;
  used_extra_params : KP.t list;
  extra_args : EA.t list Id.Map.t;
  extra_args_recursive_uses : EA.t list;
}

let print ppf { original_params; used_params; used_extra_params;
                extra_args; extra_args_recursive_uses;
              } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(original_params@ (%a))@]@ \
      @[<hov 1>(used_params@ %a)@]@ \
      @[<hov 1>(used_extra_params@ (%a))@]@ \
      @[<hov 1>(extra_args@ %a)@]@ \
      @[<hov 1>(extra_args_recursive_uses@ %a)@]\
      )@]"
    KP.List.print original_params
    KP.Set.print used_params
    KP.List.print used_extra_params
    (Id.Map.print EA.List.print) extra_args
    EA.List.print extra_args_recursive_uses

let does_nothing t =
  List.length t.original_params = KP.Set.cardinal t.used_params
    && Id.Map.is_empty t.extra_args

let create ~original_params ~used_params ~extra_params ~extra_args
      ~extra_args_recursive_uses ~used_extra_params =
  (* CR mshinwell: check there weren't any duplicates in the param lists too *)
  if List.length original_params < KP.Set.cardinal used_params then begin
    Misc.fatal_errorf "Must have at least as many [original_params] (%a)@ as \
        [used_params] (%a)"
      KP.List.print original_params
      KP.Set.print used_params
  end;
  if List.length extra_params < KP.Set.cardinal used_extra_params then begin
    Misc.fatal_errorf "Must have at least as many [extra_params] (%a)@ as \
        [used_extra_params] (%a)"
      KP.List.print extra_params
      KP.Set.print used_extra_params
  end;
  let extra_args =
    Id.Map.map (fun extra_args ->
        if List.compare_lengths extra_params extra_args <> 0 then begin
          Misc.fatal_errorf "Lengths of [extra_params] (%a)@ and all \
              [extra_args] (e.g. %a) should be equal"
            KP.List.print extra_params
            Continuation_extra_params_and_args.Extra_arg.List.print extra_args
        end;
        let extra_params_and_args = List.combine extra_params extra_args in
        List.filter_map (fun (extra_param, extra_arg) ->
            if KP.Set.mem extra_param used_extra_params then Some extra_arg
            else None)
          extra_params_and_args)
      extra_args
  in
  if List.compare_lengths extra_params extra_args_recursive_uses <> 0
  then begin
    Misc.fatal_errorf "Lengths of [extra_params] (%a)@ and \
        [extra_args_recursive_uses] (%a) should be equal"
      KP.List.print extra_params
      Continuation_extra_params_and_args.Extra_arg.List.print
      extra_args_recursive_uses
  end;
  let used_extra_params =
    List.filter (fun extra_param -> KP.Set.mem extra_param used_extra_params)
      extra_params
  in
  { original_params;
    used_params;
    used_extra_params;
    extra_args;
    extra_args_recursive_uses;
  }

let extra_params t = t.used_extra_params

let extra_args t id =
  match Id.Map.find id t.extra_args with
  | exception Not_found ->
    (* CR mshinwell: This check should still go off for non-recursive
       continuations.  We'll probably need something in the environment that
       tells us which recursive continuations' handlers we are currently
       inside. *)
(*
    if List.length (extra_params t) <> 0 then begin
      Misc.fatal_errorf "This [Apply_cont_rewrite] does not have any@ \
          extra arguments for use ID %a, but it has@ >= 1 extra parameter:@ %a"
        Id.print id
        print t
    end;
*)
    t.extra_args_recursive_uses
  | extra_args -> extra_args

let rewrite_use t id apply_cont =
  let args = Flambda.Apply_cont.args apply_cont in
  if List.compare_lengths args t.original_params <> 0 then begin
    Misc.fatal_errorf "Arguments to this [Apply_cont]@ (%a)@ do not match@ \
        [original_params] (%a):@ %a"
      Flambda.Apply_cont.print apply_cont
      KP.List.print t.original_params
      Simple.List.print args
  end;
  let original_params_with_args = List.combine t.original_params args in
  let args =
    List.filter_map (fun (original_param, arg) ->
        if KP.Set.mem original_param t.used_params then Some arg
        else None)
      original_params_with_args
  in
  let extra_args_list = extra_args t id in
  let new_vars =
    List.mapi (fun index _ ->
        let name = Printf.sprintf "extra_arg%d" index in
        Variable.create name)
      extra_args_list
  in
  let extra_args =
    List.map2
      (fun (arg : Continuation_extra_params_and_args.Extra_arg.t) new_var ->
        match arg with
        | Already_in_scope simple -> simple
        | New_binding _prim -> Simple.var new_var)
      extra_args_list
      new_vars
  in
  let args = args @ extra_args in
  let apply_cont =
    Flambda.Apply_cont.update_args apply_cont ~args
  in
  let expr = Flambda.Expr.create_apply_cont apply_cont in
  let expr =
    List.fold_left2
      (fun expr
           (arg : Continuation_extra_params_and_args.Extra_arg.t) new_var ->
        match arg with
        | Already_in_scope _ -> expr
        | New_binding prim ->
          Flambda.Expr.create_let
            (Var_in_binding_pos.create new_var Name_mode.normal)
            (Flambda.Named.create_prim prim Debuginfo.none)
            expr)
      expr
      extra_args_list new_vars
  in
  expr, apply_cont, args

(* CR mshinwell: tidy up.
   Also remove confusion between "extra args" as added by e.g. unboxing and
   "extra args" as in [Exn_continuation]. *)
let rewrite_exn_continuation t id exn_cont =
  let exn_cont_arity = Exn_continuation.arity exn_cont in
  let original_params_arity = List.map KP.kind t.original_params in
  if not (Flambda_arity.equal exn_cont_arity original_params_arity) then begin
    Misc.fatal_errorf "Arity of exception continuation %a does not \
        match@ [original_params] (%a)"
      Exn_continuation.print exn_cont
      KP.List.print t.original_params
  end;
  assert (List.length exn_cont_arity >= 1);
  let pre_existing_extra_params_with_args =
    List.combine (List.tl t.original_params)
      (Exn_continuation.extra_args exn_cont)
  in
  let extra_args0 =
    List.filter_map (fun (pre_existing_extra_param, arg) ->
        if KP.Set.mem pre_existing_extra_param t.used_params then Some arg
        else None)
      pre_existing_extra_params_with_args
  in
  let extra_args1 =
    let extra_args_list = extra_args t id in
    assert (List.compare_lengths t.used_extra_params extra_args_list = 0);
    List.map2
      (fun param (arg : Continuation_extra_params_and_args.Extra_arg.t) ->
        match arg with
        | Already_in_scope simple -> simple, KP.kind param
        | New_binding _ ->
          (* CR mshinwell: Do we need to support this yet? *)
          Misc.fatal_errorf "Not yet implemented:@ %a" print t)
      t.used_extra_params extra_args_list
  in
  let extra_args = extra_args0 @ extra_args1 in
  Exn_continuation.create ~exn_handler:(Exn_continuation.exn_handler exn_cont)
    ~extra_args

let original_params_arity t =
  KP.List.arity t.original_params
