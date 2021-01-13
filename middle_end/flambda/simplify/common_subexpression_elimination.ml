(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* [Simplify_import] cannot be used owing to a circular dependency. *)
module EA = Continuation_extra_params_and_args.Extra_arg
module EP = Flambda_primitive.Eligible_for_cse
module EPA = Continuation_extra_params_and_args
module K = Flambda_kind
module KP = Kinded_parameter
module NM = Name_mode
module P = Flambda_primitive
module RI = Apply_cont_rewrite_id
module T = Flambda_type
module TE = Flambda_type.Typing_env

module List = ListLabels

(* For the moment we don't index by scope level, unlike for [Typing_env],
   and use a diff operation to cut the CSE environment.  This is probably
   fine as there should be many fewer CSE equations than type equations. *)
type t = {
  cse : Simple.t EP.Map.t;
}

let print ppf { cse; } =
  Format.fprintf ppf "@[%a@]" (EP.Map.print Simple.print) cse

let empty =
  { cse = EP.Map.empty;
  }

let add t prim ~bound_to =
  match EP.Map.find prim t.cse with
  | exception Not_found ->
    let cse = EP.Map.add prim bound_to t.cse in
    { cse; }
  | _bound_to -> t

let find t prim =
  match EP.Map.find prim t.cse with
  | exception Not_found -> None
  | bound_to -> Some bound_to

let concat { cse = cse1; } { cse = cse2; } =
  let cse =
    (* CR mshinwell: I think elsewhere we are preferring the earlier binding *)
    EP.Map.union (fun _prim _t1 t2 -> Some t2) cse1 cse2
  in
  { cse; }

let meet { cse = cse1; } { cse = cse2; } =
  let cse =
    EP.Map.merge (fun _ simple1 simple2 ->
        match simple1, simple2 with
        | None, None | None, Some _ | Some _, None -> None
        | Some simple1, Some simple2 ->
          if Simple.equal simple1 simple2 then Some simple1
          else None)
      cse1 cse2
  in
  { cse; }

module Rhs_kind : sig
  type t =
    | Needs_extra_binding of { bound_to : Simple.t; }
    | Rhs_in_scope of { bound_to : Simple.t; }

  val bound_to : t -> Simple.t

  include Identifiable.S with type t := t
end = struct
  type t =
    | Needs_extra_binding of { bound_to : Simple.t; }
    | Rhs_in_scope of { bound_to : Simple.t; }

  let bound_to t =
    match t with
    | Needs_extra_binding { bound_to; }
    | Rhs_in_scope { bound_to; } -> bound_to

  include Identifiable.Make (struct
    type nonrec t = t

    let print ppf t =
      match t with
      | Needs_extra_binding { bound_to; } ->
        Format.fprintf ppf "@[<hov 1>(Needs_extra_binding@ %a)@]"
          Simple.print bound_to
      | Rhs_in_scope { bound_to; } ->
        Format.fprintf ppf "@[<hov 1>(Rhs_in_scope@ %a)@]"
          Simple.print bound_to

    let output _ _ = Misc.fatal_error "Rhs_kind.output not yet implemented"
    let hash _ = Misc.fatal_error "Rhs_kind.hash not yet implemented"
    let equal _ = Misc.fatal_error "Rhs_kind.equal not yet implemented"

    let compare t1 t2 =
      match t1, t2 with
      | Needs_extra_binding { bound_to = bound_to1; },
          Needs_extra_binding { bound_to = bound_to2; } ->
        Simple.compare bound_to1 bound_to2
      | Rhs_in_scope { bound_to = bound_to1; },
          Rhs_in_scope { bound_to = bound_to2; } ->
        Simple.compare bound_to1 bound_to2
      | Needs_extra_binding _, _ -> -1
      | Rhs_in_scope _, _ -> 1
  end)
end

let cse_with_eligible_lhs ~typing_env_at_fork ~cse_at_each_use ~params prev_cse
      (extra_bindings: EPA.t) extra_equations =
  let params = KP.List.simple_set params in
  List.fold_left cse_at_each_use ~init:EP.Map.empty
    ~f:(fun eligible (env_at_use, id, t) ->
      let find_new_name =
        if EPA.is_empty extra_bindings
        then (fun _arg -> None)
        else begin
          let extra_args = RI.Map.find id extra_bindings.extra_args in
          let rec find_name simple params args =
            match args, params with
            | [], [] -> None
            | [], _ | _, [] ->
              Misc.fatal_error "Mismatching params and args arity"
            | arg :: args, param :: params ->
              begin
              match (arg : EA.t) with
              | Already_in_scope arg when Simple.equal arg simple ->
                (* If [param] has an extra equation associated to it,
                   we shouldn't propagate equations on it as it will mess
                   with the application of constraints later *)
                if Name.Map.mem (KP.name param) extra_equations
                then None
                else Some (KP.simple param)
              | Already_in_scope _ | New_let_binding _ ->
                find_name simple params args
              end
          in
          (fun arg -> find_name arg extra_bindings.extra_params extra_args)
        end
      in
      EP.Map.fold (fun prim bound_to eligible ->
        let prim =
          EP.filter_map_args prim ~f:(fun arg ->
            match
              TE.get_canonical_simple_exn env_at_use arg
                ~min_name_mode:NM.normal
            with
            | exception Not_found -> None
            | arg ->
              begin match find_new_name arg with
              | None ->
                if TE.mem_simple typing_env_at_fork arg
                then Some arg
                else None
              | Some _ as arg_opt -> arg_opt
              end)
        in
        match prim with
        | None -> eligible
        | Some prim when EP.Map.mem prim prev_cse ->
          (* We've already got it from a previous round *)
          eligible
        | Some prim ->
          match
            TE.get_canonical_simple_exn env_at_use bound_to
              ~min_name_mode:NM.normal
          with
          | exception Not_found -> eligible
          | bound_to ->
            let bound_to =
              (* CR mshinwell: Think about whether this is the best fix.
                 The canonical simple might end up being one of the [params]
                 since they are defined in [env_at_fork].  However these
                 aren't bound at the use sites, so we must choose another
                 alias that is. *)
              if not (Simple.Set.mem bound_to params) then Some bound_to
              else
                let aliases =
                  TE.aliases_of_simple env_at_use
                    ~min_name_mode:NM.normal bound_to
                  |> Simple.Set.filter (fun simple ->
                    not (Simple.Set.mem simple params))
                in
                Simple.Set.get_singleton aliases
            in
            match bound_to with
            | None -> eligible
            | Some bound_to ->
              let bound_to : Rhs_kind.t =
                if TE.mem_simple typing_env_at_fork bound_to then
                  Rhs_in_scope { bound_to; }
                else
                  Needs_extra_binding { bound_to; }
              in
              (* CR mshinwell: Add [Map.add_or_replace]. *)
              match EP.Map.find prim eligible with
              | exception Not_found ->
                EP.Map.add prim (RI.Map.singleton id bound_to) eligible
              | from_prev_levels ->
                let map = RI.Map.add id bound_to from_prev_levels in
                EP.Map.add prim map eligible)
      t.cse
      eligible)

let join_one_cse_equation ~cse_at_each_use prim bound_to_map
      (cse, extra_bindings, extra_equations, allowed) =
  let has_value_on_all_paths =
    List.for_all cse_at_each_use ~f:(fun (_, id, _) ->
      RI.Map.mem id bound_to_map)
  in
  if not has_value_on_all_paths then
    cse, extra_bindings, extra_equations, allowed
  else
    let bound_to_set =
      RI.Map.data bound_to_map
      |> Rhs_kind.Set.of_list
    in
    match Rhs_kind.Set.get_singleton bound_to_set with
    | Some (Rhs_kind.Rhs_in_scope { bound_to; }) ->
      EP.Map.add prim bound_to cse, extra_bindings, extra_equations, allowed
    | None | Some (Rhs_kind.Needs_extra_binding { bound_to = _; }) ->
      let prim_result_kind = P.result_kind' (EP.to_primitive prim) in
      let var = Variable.create "cse_param" in
      let extra_param =
        KP.create var (K.With_subkind.create prim_result_kind Anything)
      in
      let bound_to = RI.Map.map Rhs_kind.bound_to bound_to_map in
      let cse = EP.Map.add prim (Simple.var var) cse in
      let extra_args =
        RI.Map.map (fun simple : EA.t -> Already_in_scope simple) bound_to
      in
      let extra_bindings = EPA.add extra_bindings ~extra_param ~extra_args in
      let extra_equations =
        (* For the primitives Is_int and Get_tag, they're strongly linked
           to their argument: additional information on the cse parameter
           should translate into additional information on the argument.
           This can be done by giving them the appropriate type. *)
        match EP.to_primitive prim with
        | Unary (Is_int, scrutinee) ->
          Name.Map.add (Name.var var) (T.is_int_for_scrutinee ~scrutinee)
            extra_equations
        | Unary (Get_tag, block) ->
          Name.Map.add (Name.var var) (T.get_tag_for_block ~block)
            extra_equations
        | _ -> extra_equations
      in
      let allowed =
        Name_occurrences.add_name allowed (Name.var var) NM.normal
      in
      cse, extra_bindings, extra_equations, allowed

let cut_cse_environment { cse; } ~cse_at_fork =
  let { cse = cse_at_fork; } = cse_at_fork in
  let cse =
    (* Remove all equations present at the fork from the use environment.
       (See comment earlier in this file about speed.) *)
    if cse == cse_at_fork then EP.Map.empty
    else
      EP.Map.fold (fun prim _simple t -> EP.Map.remove prim t) cse_at_fork cse
  in
  { cse; }

module Join_result = struct
  type nonrec t =
    { cse_at_join_point : t;
      extra_params : EPA.t;
      (* CR mshinwell: [extra_equations] should be something like [TEE.t] but
         that is kind of slow at present. *)
      extra_equations : T.t Name.Map.t;
      extra_allowed_names : Name_occurrences.t;
    }
end

let join ~typing_env_at_fork ~cse_at_fork ~cse_at_each_use ~params =
  let cse_at_each_use =
    List.map cse_at_each_use ~f:(fun (env_at_use, id, t) ->
      let t = cut_cse_environment t ~cse_at_fork in
      env_at_use, id, t)
  in
  let compute_cse_one_round prev_cse extra_params extra_equations ~allowed =
  (* CSE equations have a left-hand side specifying a primitive and a
     right-hand side specifying a [Simple].  The left-hand side is matched
     against portions of terms.  As such, the [Simple]s therein must have
     name mode [Normal], since we do not do CSE for phantom bindings (see
     [Simplify_common]).  It follows that any CSE equation whose left-hand side
     involves a name not defined at the fork point, having canonicalised such
     name, cannot be propagated.  This step also canonicalises the right-hand
     sides of the CSE equations. *)
    let new_t =
      cse_with_eligible_lhs ~typing_env_at_fork ~cse_at_each_use ~params
        prev_cse extra_params extra_equations
    in
  (* To make use of a CSE equation at or after the join point, its right-hand
     side must have the same value, no matter which path is taken from the
     fork point to the join point.  We filter out equations that do not
     satisfy this.  Sometimes we can force an equation to satisfy the
     property by explicitly passing the value of the right-hand side as an
     extra parameter to the continuation at the join point. *)
    let cse', extra_params', extra_equations', allowed =
      EP.Map.fold (join_one_cse_equation ~cse_at_each_use)
        new_t (EP.Map.empty, EPA.empty, Name.Map.empty, allowed)
    in
    let need_other_round =
      (* If we introduce new parameters, then CSE equations involving the
         corresponding arguments can be considered again, so we need
         another round. *)
      not (EPA.is_empty extra_params')
    in
    let cse = EP.Map.disjoint_union prev_cse cse' in
    let extra_params = EPA.concat extra_params' extra_params in
    let extra_equations =
      Name.Map.disjoint_union extra_equations extra_equations'
    in
    cse, extra_params, extra_equations, allowed, need_other_round
  in
  let cse, extra_params, extra_equations, allowed =
    let rec do_rounds current_round cse extra_params extra_equations allowed =
      let cse, extra_params, extra_equations, allowed, need_other_round =
        compute_cse_one_round cse extra_params extra_equations ~allowed
      in
      if need_other_round && current_round < Flambda_features.cse_depth ()
      then begin
        do_rounds (succ current_round) cse extra_params extra_equations allowed
      end else begin
        (* Either a fixpoint has been reached or we've already explored far
           enough *)
        cse, extra_params, extra_equations, allowed
      end
    in
    do_rounds 1 EP.Map.empty EPA.empty Name.Map.empty Name_occurrences.empty
  in
  let cse =
    (* Any CSE equation whose right-hand side identifies a name in the [allowed]
       set is propagated.  We don't need to check the left-hand sides because
       we know all of those names are in [typing_env_at_fork]. *)
    EP.Map.filter (fun _prim bound_to ->
        Simple.pattern_match bound_to
          ~const:(fun _ -> true)
          ~name:(fun name -> Name_occurrences.mem_name allowed name))
      cse
  in
  { Join_result.
    cse_at_join_point = concat cse_at_fork { cse; };
    extra_params;
    extra_equations;
    extra_allowed_names = allowed;
  }
