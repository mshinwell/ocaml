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
module KP = Kinded_parameter
module NM = Name_mode
module P = Flambda_primitive
module RI = Apply_cont_rewrite_id
module T = Flambda_type
module TE = Flambda_type.Typing_env

module List = ListLabels

module For_downwards_acc = struct
  type t = {
    aliases : Variable.Set.t Variable.Map.t;
    definitions : EP.t Variable.Map.t;
  }

  let print ppf { aliases; definitions; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(aliases@ %a)@]@ \
        @[<hov 1>(definitions@ %a)@]\
        @]"
        (Variable.Map.print Variable.Set.print) aliases
        (Variable.Map.print EP.print) definitions

  let empty =
    { aliases = Variable.Map.empty;
      definitions = Variable.Map.empty;
    }

  let add_primitive t ~bound_to prim =
    let definitions = Variable.Map.add bound_to prim t.definitions in
    { aliases = t.aliases;
      definitions;
    }

  let add_aliases t ~bound_to new_aliases =
    let aliases =
      match Variable.Map.find bound_to t.aliases with
      | exception Not_found ->
        Variable.Map.add bound_to new_aliases t.aliases
      | aliases ->
        Variable.Map.add bound_to (Variable.Set.union aliases new_aliases)
          t.aliases
    in
    { aliases;
      definitions = t.definitions;
    }
end

module For_downwards_env = struct
  type t = {
    by_scope : EP.t Variable.Map.t Scope.Map.t;
    combined : EP.t Variable.Map.t;
  }

  type for_downwards_env = t

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
    { by_scope;
      combined;
    }

  let rec remove t ~bound_to =
    match Variable.Map.find bound_to t.combined with
    | exception Not_found -> t
    | prim ->
      let by_scope =
        Scope.Map.filter_map (fun _scope definitions ->
            let definitions = Variable.Map.remove bound_to definitions in
            if Variable.Map.is_empty definitions then None
            else Some definitions)
          t.by_scope
      in
      let combined = Variable.Map.remove bound_to t.combined in
      let t =
        { by_scope;
          combined;
        }
      in
      Name_occurrences.fold_variables (EP.free_names prim) ~init:t
        ~f:(fun t var -> remove t ~bound_to:var)

  let consider_simplified_var t var ~in_apply_cont =
    if in_apply_cont then t  (* see comment below *)
    else
      match Variable.Map.find var t.combined with
      | exception Not_found -> t
      | _prim ->
        (* We've seen a use of a variable bound to a primitive eligible for
           PDCE.  This use will block the primitive (and any others that depend
           on the variable in their definitions) from being sunk down any
           further -- unless the use is in an [Apply_cont] (or [Switch] arm).
           In these latter cases we track what's going on via [join]. *)
        remove t ~bound_to:var

  let consider_simplified_simple t simple ~in_apply_cont =
    Simple.pattern_match' simple
      ~const:(fun _ -> t)
      ~symbol:(fun _ -> t)
      ~var:(fun var -> consider_simplified_var t var ~in_apply_cont)

  let consider_simplified_primitive t acc typing_env ~bound_to prim =
    match P.Eligible_for_pdce.create prim with
    | None -> t, acc
    | Some prim ->
      (* This function is intended to be called after the primitive has been
         simplified, so its arguments will already have been canonicalised. *)
      let scope = TE.current_scope typing_env in
      let t = add t scope ~bound_to prim in
      let acc = For_downwards_acc.add_primitive acc ~bound_to prim in
      t, acc

  module Join_result = struct
    type nonrec t =
      { pdce : t;
        pdce_acc : For_downwards_acc.t;
        extra_params : EPA.t;
        extra_allowed_names : Name_occurrences.t;
        extra_equations : T.t Name.Map.t;
      }

    let extra_params t = t.extra_params
    let extra_allowed_names t = t.extra_allowed_names
    let extra_equations t = t.extra_equations
  end

  let join0 ~typing_env_at_fork ~pdce_at_fork pdce_acc ~pdce_at_each_use
        ~scope_at_fork ~params =
    (* We assume all of the equations seen thus far, that are available
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
        ~f:(fun result (_typing_env, _id, equations, _args) ->
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
    (* For every variable required for (the right-hand sides of) the
       propagated equations, determine whether it is already in scope in the
       environment at the fork point, and if not create a continuation parameter
       to pass it along. The map between the original variables and these fresh
       parameters forms a name permutation. *)
    let perm =
      Variable.Map.fold (fun bound_to prim perm ->
          (* [bound_to] should not be in scope at the join point, since it is
             bound to an equation defined on some path between the fork point
             and the join point. *)
          if TE.mem typing_env_at_fork (Name.var bound_to) then begin
            Misc.fatal_errorf "Didn't expect PDCE bound variable %a \
                (bound to %a) to be in scope in fork environment:@ %a"
              Variable.print bound_to
              EP.print prim
              TE.print typing_env_at_fork
          end;
          let names = P.Eligible_for_pdce.free_names prim in
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
    (* We look at the continuation's arguments (i.e. at the uses) to work out
       whether the left-hand sides of the equations were propagated past the
       join point -- and, if so, what names now correspond to the original
       left-hand sides of the equations.  Any left-hand side that isn't
       propagated can have its corresponding PDCE equation dropped, since
       there shouldn't be any further uses. *)
    let aliases_via_args =
      let by_param =
        List.fold_left pdce_at_each_use
          ~init:(List.init ~len:(List.length params) ~f:(fun _ -> None))
          ~f:(fun by_param (typing_env, _id, _equations, args) ->
            assert (List.compare_lengths params args = 0);
            (* We check that, up to canonicalisation of names, for every
               parameter of the continuation the same argument was supplied
               at all use sites. *)
            List.map2 by_param args ~f:(fun existing_alias arg_ty ->
              match T.get_alias_exn arg_ty with
              | exception Not_found -> None
              | alias ->
                match
                  TE.get_canonical_simple_exn typing_env alias
                    ~min_name_mode:NM.normal
                with
                | exception Not_found -> None
                | alias ->
                  Simple.pattern_match' alias
                    ~const:(fun _ -> None)
                    ~symbol:(fun _ -> None)
                    ~var:(fun var ->
                      match existing_alias with
                      | None -> Some var
                      | Some existing_alias' ->
                        if Variable.equal var existing_alias' then
                          existing_alias
                        else None)))
      in
      List.filter_map by_param ~f:Fun.id
      |> Variable.Set.of_list
    in
    let were_not_propagated, pdce_acc =
      Variable.Map.fold
        (fun bound_to _prim (were_not_propagated, pdce_acc) ->
          match Variable.Map.find bound_to aliases_via_args with
          | exception Not_found ->
            let were_not_propagated =
              Variable.Set.add bound_to were_not_propagated
            in
            were_not_propagated, pdce_acc
          | aliases ->
            let pdce_acc =
              For_downwards_acc.add_aliases pdce_acc ~bound_to aliases
            in
            were_not_propagated, pdce_acc)
        equations
        (Variable.Set.empty, pdce_acc)
    in
    let t =
      Variable.Map.fold (fun bound_to prim t ->
          if Variable.Set.mem bound_to were_not_propagated then t
          else
            let bound_to = Name_permutation.apply_variable perm bound_to in
            let prim = P.Eligible_for_pdce.apply_name_permutation prim perm in
            add t (Scope.next scope_at_fork) ~bound_to prim)
        equations
        pdce_at_fork
    in
    let extra_params =
      (* need to form EPA.t -- take into account were_not_prop. *)
      ()
    in
    (* We don't generate any new equations at present; this could be done in
       the future if worthwhile.  There also aren't any extra allowed names
       for the typing environment join, since we haven't introduced any
       equations (involving names that might otherwise not be propagated). *)
    Some { Join_result.
      pdce = t;
      pdce_acc;
      extra_params;
      extra_allowed_names = Name_occurrences.empty;
      extra_equations = Name.Map.empty;
    }

  let cut_pdce_environment { by_scope; _ } ~scope_at_fork =
    (* This extracts those primitive bindings eligible for PDCE that arose
       between the fork point and each use of the continuation in question. *)
    let _, _, levels = Scope.Map.split scope_at_fork by_scope in
    Scope.Map.fold (fun _scope equations result ->
        Variable.Map.disjoint_union equations result)
      levels
      Variable.Map.empty

  let join ~typing_env_at_fork ~pdce_at_fork pdce_acc ~use_info ~get_typing_env
        ~get_rewrite_id ~get_arg_types ~get_pdce ~params =
    (* We need to split the PDCE computation around the typing environment
       join.  The latter needs the [extra_allowed_names] information, whereas
       our join computations below need the joined types of the continuation's
       parameters. *)
    let scope_at_fork = TE.current_scope typing_env_at_fork in
    let seen_equations = ref false in
    let pdce_at_each_use =
      List.map use_info ~f:(fun use ->
        let t = get_pdce use in
        let pdce_between_fork_and_use = cut_pdce_environment t ~scope_at_fork in
        if not (Variable.Map.is_empty pdce_between_fork_and_use) then begin
          seen_equations := true
        end;
        get_typing_env use, get_rewrite_id use, pdce_between_fork_and_use,
          get_arg_types use)
    in
    if not !seen_equations then None
    else
      join0 ~typing_env_at_fork ~pdce_at_fork pdce_acc ~pdce_at_each_use
        ~scope_at_fork ~params
end

(*
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
*)
