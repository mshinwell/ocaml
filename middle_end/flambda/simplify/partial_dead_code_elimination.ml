(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

(* [Simplify_import] cannot be used owing to a circular dependency. *)
module EA = Continuation_extra_params_and_args.Extra_arg
module EP = Flambda_primitive.Eligible_for_pdce
module EPA = Continuation_extra_params_and_args
module K = Flambda_kind
module KP = Kinded_parameter
module NM = Name_mode
module P = Flambda_primitive
module RI = Apply_cont_rewrite_id
module T = Flambda_type
module TE = Flambda_type.Typing_env

module List = ListLabels

module For_downwards_env = struct
  type t = {
    by_scope : EP.t Variable.Map.t Scope.Map.t;
    combined : EP.t Variable.Map.t;
  }

  let print ppf { by_scope; combined; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(by_scope@ %a)@]@ \
        @[<hov 1>(combined@ %a)@]\
        @]"
      (Scope.Map.print (Variable.Map.print EP.print)) by_scope
      (Variable.Map.print EP.print) combined
  let empty =
    { by_scope = Scope.Map.empty;
      combined = Variable.Map.empty;
    }

  let add t scope ~bound_to prim =
    let level =
      match Scope.Map.find scope t.by_scope with
      | exception Not_found -> Variable.Map.singleton bound_to prim
      | level -> Variable.Map.add bound_to prim level
    in
    let by_scope = Scope.Map.add (* replace *) scope level t.by_scope in
    let combined = Variable.Map.add bound_to prim t.combined in
    (* XXX What about [lhs_with_aliases]?
    let lhs_with_aliases = Variable.Set.add bound_to t.lhs_with_aliases in
    *)
    { by_scope;
      combined;
    }

  let consider_primitive t typing_env ~bound_to prim =
    match P.Eligible_for_pdce.create prim with
    | None -> t
    | Some prim ->
      (* We canonicalise the arguments of the primitive to maximise the
         chance, at a join point, of finding the variables involved in the
         fork environment (see below).
         If the primitive doesn't involve variables then it's not eligible
         for PDCE; it should get lifted instead. *)
      let free_names = P.Eligible_for_pdce.free_names prim in
      if Name_occurrences.no_variables free_names then t
      else
        let prim =
          P.Eligible_for_pdce.map_args prim ~f:(fun simple ->
            if not (Simple.is_var simple) then simple
            else
              match
                TE.get_canonical_simple_exn typing_env
                  simple ~min_name_mode:NM.normal
              with
              | exception Not_found ->
                Misc.fatal_errorf "Couldn't find canonical variable for %a \
                    occurring in PDCE primitive %a = %a@ Typing env:@ %a"
                  Simple.print simple
                  Variable.print bound_to
                  P.Eligible_for_pdce.print prim
                  TE.print typing_env
              | canonical -> canonical)
        in
        let scope = TE.current_scope typing_env in
        add t scope ~bound_to prim

  let cut_pdce_environment { by_scope; _ } ~scope_at_fork =
    (* This extracts those primitive bindings eligible for PDCE that arose
       between the fork point and each use of the continuation in question. *)
    let _, _, levels = Scope.Map.split scope_at_fork by_scope in
    Scope.Map.fold (fun _scope equations result ->
        Variable.Map.disjoint_union equations result)
      levels
      Variable.Map.empty

  module Join_result = struct
    type nonrec t =
      { pdce_at_join_point : t;
        extra_params : EPA.t;
        extra_equations : T.t Name.Map.t;
        extra_allowed_names : Name_occurrences.t;
      }
  end

  let join0 ~typing_env_at_fork ~pdce_at_fork ~pdce_at_each_use ~scope_at_fork =
    (* We assume that all of the equations seen thus far, which are available
       at all uses of the continuation (call it [k]) currently being
       considered, might be able to be sunk further down the control flow.
       To allow this we force all variables required for computation of the
       relevant primitives to be available in all successor continuations.
       In practice, because we only propagate equations beyond a join point
       if they are available on all incoming paths, this turns out to mean
       forcing availability in all continuations that [k] dominates. *)
    let equations =
      List.fold_left pdce_at_each_use
        ~init:Variable.Map.empty
        ~f:(fun result (typing_env, _id, equations) ->
          Variable.Map.inter (fun bound_to prim1 prim2 ->
              if not (P.Eligible_for_pdce.equal prim1 prim2) then begin
                Misc.fatal_errorf "PDCE equations across uses do not agree \
                    (%a = %a versus %a = %a)"
                  Variable.print bound_to
                  P.Eligible_for_pdce.print prim1
                  Variable.print bound_to
                  P.Eligible_for_pdce.print prim2
              end;
              prim1)
            result equations)
    in
    (* For every variable required for the propagated equations, determine
       whether it is already in scope in the environment at the fork point,
       and if not create a continuation parameter to pass it along.  The
       map between the original variables and these fresh parameters forms
       a name permutation. *)

    (* XXX What happens if [bound_to] is passed as a parameter?  We need to
       track that...

       We could try to check not only whether the variable is at the fork
       point but also whether it's passed as an argument.  We would want to
       canonicalise the arguments (Simple.t values) first before checking.
       If it's passed as an argument, no need for a new parameter, but the
       permutation still needs augmenting to swap over to the existing
       parameter. *)

    let perm =
      Variable.Map.fold (fun bound_to prim perm ->
          let names =
            Name_occurrences.add_variable (P.Eligible_for_pdce.free_names prim)
              bound_to NM.normal
          in
          Name_occurrences.fold_variables names ~init:perm
            ~f:(fun perm var ->
              let in_scope_at_fork =
                TE.mem typing_env_at_fork (Name.var var)
                  ~min_name_mode:NM.normal
              in
              if in_scope_at_fork then perm
              else
                let param_var = Variable.rename var in
                Name_permutation.add_fresh_variable perm
                  var ~guaranteed_fresh:param_var))
        equations
        Name_permutation.empty
    in
    let pdce_at_join_point, extra_allowed_names =
      Variable.Map.fold (fun bound_to prim (t, extra_allowed_names) ->
          let bound_to = Name_permutation.apply_variable perm bound_to in
          let prim = P.Eligible_for_pdce.apply_name_permutation prim perm in
          let t = add t (Scope.next scope_at_fork) ~bound_to prim in
          let extra_allowed_names =
            Name_occurrences.union extra_allowed_names
              (P.Eligible_for_pdce.free_names prim)
          in
          t, extra_allowed_names)
        equations
        (pdce_at_fork, Name_occurrences.empty)
    in
    (* what types will we give the new params?  Some of them could have
       proper types, e.g. "boxed_float(f)". *)
    let extra_params =
      (* need to form EPA.t *)
      ()
    in
    let extra_equations =
      ()
    in
    Some { Join_result.
      pdce_at_join_point;
      extra_params;
      extra_equations;
      extra_allowed_names;
    }

  let join ~typing_env_at_fork ~pdce_at_fork ~use_info ~get_pdce =
    let scope_at_fork = TE.current_scope typing_env_at_fork in
    let seen_equations = ref false in
    let pdce_at_each_use =
      List.map use_info ~f:(fun use ->
        let t = get_pdce use in
        let pdce_between_fork_and_use = cut_pdce_environment t ~scope_at_fork in
        if not (Variable.Map.is_empty pdce_between_fork_and_use) then begin
          seen_equations := true
        end;
        get_typing_env use, get_rewrite_id use, pdce_between_fork_and_use)
    in
    (* XXX even if no new equations, need to propagate & update acc *)
    if not !seen_equations then None
    else
      join0 ~typing_env_at_fork ~pdce_at_fork ~pdce_at_each_use ~scope_at_fork
end

module For_downwards_acc = struct
  type t = {
    lhs_with_aliases : Variable.Set.t;
  }

  let print ppf { lhs_with_aliases; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(lhs_with_aliases@ %a)@]\
        @]"
      Variable.Set.print lhs_with_aliases
end

module For_upwards_acc = struct
  type t = {
    still_to_be_placed : EP.t Variable.Map.t;
    lhs_with_aliases : Variable.Set.t;
  }
end

module For_upwards_env = struct
  type t = {
    uses_of_lhs_or_aliases : Variable.Set.t Continuation.Map.t;
  }
end
