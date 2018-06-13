(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module K = Flambda_kind

module Make
    (T : Flambda_type0_internal_intf.S)
    (Make_meet_and_join : functor
      (S : Meet_and_join_spec_intf.S with module T := T)
        -> Meet_and_join_intf.S
             with module T := T
             with type of_kind_foo = S.of_kind_foo)
    (Meet_and_join_value : Meet_and_join_intf.S with module T := T)
    (Meet_and_join : Meet_and_join_intf.S_for_types with module T := T)
    (Typing_env : Typing_env_intf.S with module T := T)
    (Typing_env_extension : Typing_env_extension_intf.S with module T := T)
    (Join_env : Join_env_intf.S with module T := T)
    (E : Either_meet_or_join_intf.S with module T := T) =
struct
  module rec Meet_and_join_fabricated : sig
    include Meet_and_join_intf.S
      with module T := T
      with type of_kind_foo = T.of_kind_fabricated
  end = Make_meet_and_join (struct
    open T

    module JE = Join_env

    type env_extension = T.env_extension

    type of_kind_foo = of_kind_fabricated

    let kind = K.fabricated ()

    let to_type ty : t =
      { descr = Fabricated ty;
        phantom = None;
      }

    let force_to_kind = force_to_kind_fabricated
    let print_ty = print_ty_fabricated

    let meet_closure env
          (closure1 : closure) (closure2 : closure)
          : (closure * env_extension) Or_absorbing.t =
      if closure1 == closure2 then
        Ok (closure1, Typing_env_extension.empty)
      else
        Misc.fatal_error "meet_closure: not yet implemented"

(*
    (* CR mshinwell: We need to work out how to stop direct call
       surrogates from being dropped e.g. when in a second round, a
       function type (with a surrogate) propagated from the first round is
       put into a meet with a type for the same function, but a new
       surrogate. *)
    let meet_closure env
          (closure1 : closure) (closure2 : closure)
          : (closure * env_extension) Or_bottom.t =
      if closure1 == closure2 then begin
        Ok (closure1, Typing_env_extension.empty)
      end else begin
        let cannot_prove_different ~params1 ~params2
              ~param_names1 ~param_names2 ~result1 ~result2
              ~result_env_extension1 ~result_env_extension2 : _ Or_bottom.t =
          let same_arity = List.compare_lengths params1 params2 = 0 in
          let same_num_results = List.compare_lengths result1 result2 = 0 in
          let equations = ref (Typing_env_extension.empty) in
          let has_bottom params = List.exists is_obviously_bottom params in
          let params_changed = ref Neither in
          let params : _ Or_bottom.t =
            if not same_arity then Bottom
            else
              let variable_list_to_name_list vars =
                List.map (fun var -> Name.var var) vars
              in
              let params2_to_params1_freshening, param_names =
                match param_names1, param_names2 with
                | None, None -> Name.Map.empty, []
                | None, Some param_names2 ->
                  let param_names2 = variable_list_to_name_list param_names2 in
                  Name.Map.empty, param_names2
                | Some param_names1, None ->
                  let param_names1 = variable_list_to_name_list param_names1 in
                  Name.Map.empty, param_names1
                | Some param_names1, Some param_names2 ->
                  (* This must match up with the code some distance below,
                     where the left-hand function declarations (the ones
                     whose parameter names are "param_names1") are chosen. *)
                  let param_names1 = variable_list_to_name_list param_names1 in
                  let param_names2 = variable_list_to_name_list param_names2 in
                  Name.Map.of_list (List.combine param_names2 param_names1),
                    param_names1
              in
              let params =
                List.map2 (fun t1 t2 ->
                    let t, new_equations =
                      Meet_and_join.meet env t1 t2
                    in
                    if not (t == t1) then begin
                      params_changed := join_changes !params_changed Left
                    end;
                    if not (t == t2) then begin
                      params_changed := join_changes !params_changed Right
                    end;
                    equations :=
                      Typing_env_extension.meet env
                        new_equations !equations;
                    t)
                  params1
                  params2
              in
              if has_bottom params then Bottom
              else Ok (params, param_names, params2_to_params1_freshening)
          in
          match params with
          | Bottom -> Bottom
          | Ok (param_tys, param_names, params2_to_params1_freshening) ->
            let result_changed = ref Neither in
            let result : _ Or_bottom.t =
              if not same_num_results then Bottom
              else
                let scope_level = Typing_env.max_level env in
                let env_with_params =
                  List.fold_left2 (fun env param param_ty ->
                      Typing_env.add env param scope_level
                        (Definition param_ty))
                    env
                    param_names param_tys
                in
                let result_env_extension2 =
                  (* XXX Check that [rename_names] has the correct
                     semantics *)
                  Typing_env_extension.rename_names
                    result_env_extension2 params2_to_params1_freshening
                in
                let result_env_extension =
                  Typing_env_extension.meet env_with_params
                    result_env_extension1 result_env_extension2
                in
                let result_env_extension_changed : changes =
                  let changed1 =
                    not (Typing_env_extension.fast_equal
                      result_env_extension1 result_env_extension)
                  in
                  let changed2 =
                    not (Typing_env_extension.fast_equal
                      result_env_extension2 result_env_extension)
                  in
                  match changed1, changed2 with
                  | false, false -> Neither
                  | true, false -> Left
                  | false, true -> Right
                  | true, true -> Both
                in
                let result_env =
                  Typing_env.add_or_meet_env_extension env_with_params
                    result_env_extension scope_level;
                in
                let result =
                  List.map2 (fun t1 t2 ->
                      let t, new_equations =
                        Meet_and_join.meet result_env t1 t2
                      in
                      if not (t == t1) then begin
                        result_changed := join_changes !result_changed Left
                      end;
                      if not (t == t2) then begin
                        result_changed := join_changes !result_changed Right
                      end;
                      equations :=
                        Typing_env_extension.meet result_env
                          new_equations !equations;
                      t)
                    result1
                    result2
                in
                if has_bottom result then Bottom
                else
                  Ok (result, result_env_extension,
                      result_env_extension_changed)
            in
            match result with
            | Ok (result, result_env_extension, result_env_extension_changed) ->
              let changed =
                join_changes !params_changed
                  (join_changes !result_changed result_env_extension_changed)
              in
              Ok (param_tys, changed, result, result_env_extension,
                !equations)
            | Bottom -> Bottom
        in
        let function_decls : _ Or_bottom.t =
          match closure1.function_decls, closure2.function_decls with
          | Inlinable inlinable1, Inlinable inlinable2 ->
            let params1 = List.map snd inlinable1.params in
            let params2 = List.map snd inlinable2.params in
            let param_names1 =
              List.map (fun (param, _ty) -> Parameter.var param)
                inlinable1.params
            in
            let param_names2 =
              List.map (fun (param, _ty) -> Parameter.var param)
                inlinable2.params
            in
            let result =
              cannot_prove_different ~params1 ~params2
                ~param_names1:(Some param_names1)
                ~param_names2:(Some param_names2)
                ~result1:inlinable1.result
                ~result2:inlinable2.result
                ~result_env_extension1:inlinable1.result_env_extension
                ~result_env_extension2:inlinable2.result_env_extension
            in
            begin match result with
            | Ok (params, changed, result, result_env_extension,
                  equations) ->
              (* [closure1.function_decls] and [closure2.function_decls] may be
                 different, but we cannot prove it.  We arbitrarily pick
                 [closure1.function_decls] to return, with parameter and result
                 types refined.  (Note that this decision needs to line up
                 with the code above; see comment above.) *)
              let params =
                List.map2 (fun (param, _old_ty) new_ty ->
                    param, new_ty)
                  inlinable1.params
                  params
              in
              begin match changed with
              | Neither -> Ok (closure1.function_decls, equations)
              | Left -> Ok (closure2.function_decls, equations)
              | Right -> Ok (closure1.function_decls, equations)
              | Both ->
                Ok (Inlinable { inlinable1 with
                  params;
                  result;
                  result_env_extension;
                }, equations)
              end
            | Bottom ->
              (* [closure1] and [closure2] are definitely different. *)
              Bottom
            end
          | Non_inlinable None, Non_inlinable None ->
            Ok (Non_inlinable None, Typing_env_extension.empty)
          | Non_inlinable (Some non_inlinable), Non_inlinable None
          | Non_inlinable None, Non_inlinable (Some non_inlinable) ->
            (* We can arbitrarily pick one side or the other: we choose the
               side which gives a more precise type. *)
            Ok (Non_inlinable (Some non_inlinable),
              Typing_env_extension.empty)
          | Non_inlinable None, Inlinable inlinable
          | Inlinable inlinable, Non_inlinable None ->
            (* Likewise. *)
            Ok (Inlinable inlinable, Typing_env_extension.empty)
          | Non_inlinable (Some non_inlinable1),
              Non_inlinable (Some non_inlinable2) ->
            let result =
              cannot_prove_different
                ~params1:non_inlinable1.params
                ~params2:non_inlinable2.params
                ~results1:non_inlinable1.results
                ~results2:non_inlinable2.results
            in
            begin match result with
            | Ok (params, _params_changed, results, equations) ->
              let non_inlinable_function_decl =
                { non_inlinable1 with
                  params;
                  results;
                }
              in
              Ok (Non_inlinable (Some non_inlinable_function_decl),
                equations)
            | Bottom ->
              Bottom
            end
          | Non_inlinable (Some non_inlinable), Inlinable inlinable
          | Inlinable inlinable, Non_inlinable (Some non_inlinable) ->
            let params1 = List.map snd inlinable.params in
            let param_names1 =
              List.map (fun (param, _ty) -> Parameter.var param)
                inlinable.params
            in
            let result =
              cannot_prove_different
                ~params1
                ~params2:non_inlinable.params
                ~param_names1:(Some param_names1)
                ~param_names2:None
                ~result1:inlinable.result
                ~result2:non_inlinable.result
                ~result_env_extension1:inlinable.result_env_extension
                ~result_env_extension2:non_inlinable.result_env_extension
            in
            begin match result with
            | Ok (params, _params_changed, result, result_env_extension,
                  equations) ->
              (* We pick the inlinable declaration, since it gives more
                 information.  (Note that this matches up with the fact
                 that the parameter names passed to [cannot_prove_different]
                 are for the inlinable declaration.) *)
              let params =
                List.map2 (fun (param, _old_ty) new_ty -> param, new_ty)
                  inlinable.params
                  params
              in
              let inlinable_function_decl =
                { inlinable with
                  params;
                  result;
                  result_env_extension;
                }
              in
              Ok (Inlinable inlinable_function_decl, equations)
            | Bottom ->
              Bottom
            end
        in
        match function_decls with
        | Bottom -> Bottom
        | Ok (function_decls, equations) ->
          if function_decls == closure1.function_decls then
            Ok (closure1, equations)
          else if function_decls == closure2.function_decls then
            Ok (closure2, equations)
          else
            Ok (({ function_decls; } : closure), equations)
      end

    let meet_or_join_closure env
          (closure1 : closure) (closure2 : closure)
          : closure E.Or_absorbing.t =
      if Join_env.fast_check_extensions_same_both_sides join_env
        && closure1 == closure2
      then begin
        closure1
      end else begin
        let arities_ok =
          let all_function_decls =
            (Closure_id.Map.data closure1.function_decls)
              @ (Closure_id.Map.data closure2.function_decls)
          in
          match all_function_decls with
          | [] -> true
          | function_decl::function_decls ->
            (* We rely on [function_declarations_compatible] being an
               equivalence relation. *)
            List.for_all (fun function_decl' ->
                T.function_declarations_compatible function_decl function_decl')
              function_decls
        in
        if not arities_ok then Absorbing
        else
          let function_decls =
            E.Closure_id.Map.union_or_inter_merge (fun decl1 decl2 ->
                meet_or_join_function_declaration join_env decl1 decl2)
              closure1.function_decls closure2.function_decls
          in
          { function_decls; }
      end



    let join_closure join_env
          (closure1 : closure) (closure2 : closure)
          : closure =
      if Join_env.fast_check_extensions_same_both_sides join_env
        && closure1 == closure2
      then begin
        closure1
      end else begin
        let produce_non_inlinable ~params1 ~params2 ~result1 ~result2
              ~direct_call_surrogate1 ~direct_call_surrogate2 =
          match Parameters.join_and_introduce join_env params1 params2 with
          | None -> Non_inlinable None
          | Some (params, join_env) ->
            match Parameters.join join_env results1 results2 with
            | None -> Non_inlinable None
            | Some results ->
              let direct_call_surrogate =
                match direct_call_surrogate1, direct_call_surrogate2 with
                | Some closure_id1, Some closure_id2
                    when Closure_id.equal closure_id1 closure_id2 ->
                  Some closure_id1
                | _, _ -> None
              in
              let non_inlinable : non_inlinable_function_declarations =
                { params;
                  results;
                  direct_call_surrogate;
                }
              in
              Non_inlinable (Some non_inlinable)
        in
        let function_decls : function_declarations =
          match closure1.function_decls, closure2.function_decls with
          | Non_inlinable None, _ | _, Non_inlinable None -> Non_inlinable None
          | Non_inlinable (Some non_inlinable1),
              Non_inlinable (Some non_inlinable2) ->
            produce_non_inlinable
              ~params1:non_inlinable1.params
              ~params2:non_inlinable2.params
              ~results1:non_inlinable1.results
              ~results2:non_inlinable2.results
              ~direct_call_surrogate1:non_inlinable1.direct_call_surrogate
              ~direct_call_surrogate2:non_inlinable2.direct_call_surrogate
          | Non_inlinable (Some non_inlinable), Inlinable inlinable
          (* CR mshinwell: This should use a record pattern match rather than
             field projection *)
          | Inlinable inlinable, Non_inlinable (Some non_inlinable) ->
            produce_non_inlinable
              ~params1:inlinable.params
              ~params2:non_inlinable.params
              ~results1:inlinable.results
              ~results2:non_inlinable.results
              ~direct_call_surrogate1:inlinable.direct_call_surrogate
              ~direct_call_surrogate2:non_inlinable.direct_call_surrogate
          | Inlinable inlinable1, Inlinable inlinable2 ->
            if not (Code_id.equal inlinable1.code_id inlinable2.code_id)
            then begin
              (* XXX check lengths *)
              let params1 = List.map snd inlinable1.params in
              let params2 = List.map snd inlinable2.params in
              let param_names1 = List.map fst inlinable1.params in
              let param_names2 = List.map fst inlinable2.params in
              produce_non_inlinable
                ~params1
                ~params2
                ~results1:inlinable1.results
                ~results2:inlinable2.results
                ~direct_call_surrogate1:inlinable1.direct_call_surrogate
                ~direct_call_surrogate2:inlinable2.direct_call_surrogate
            end else begin
              if !Clflags.flambda_invariant_checks then begin
                assert (Closure_origin.equal inlinable1.closure_origin
                  inlinable2.closure_origin);
                assert (Continuation.equal inlinable1.continuation_param
                  inlinable2.continuation_param);
                assert (Continuation.equal inlinable1.exn_continuation_param
                  inlinable2.exn_continuation_param);
                assert (Pervasives.(=) inlinable1.is_classic_mode
                  inlinable2.is_classic_mode);
                assert (List.compare_lengths inlinable1.params inlinable2.params
                  = 0);
                assert (List.compare_lengths inlinable1.result inlinable2.result
                  = 0);
                assert (Name_occurrences.equal inlinable1.free_names_in_body
                  inlinable2.free_names_in_body);
                assert (Pervasives.(=) inlinable1.stub inlinable2.stub);
                assert (Debuginfo.equal inlinable1.dbg inlinable2.dbg);
                assert (Pervasives.(=) inlinable1.inline inlinable2.inline);
                assert (Pervasives.(=) inlinable1.specialise
                  inlinable2.specialise);
                assert (Pervasives.(=) inlinable1.is_a_functor
                  inlinable2.is_a_functor);
                assert (Variable.Set.equal
                  (Lazy.force inlinable1.invariant_params)
                  (Lazy.force inlinable2.invariant_params));
                assert (Pervasives.(=)
                  (Lazy.force inlinable1.size)
                  (Lazy.force inlinable2.size));
                assert (Variable.equal inlinable1.my_closure
                  inlinable2.my_closure)
              end;
              (* Parameter types are treated covariantly. *)
              (* CR mshinwell: Add documentation for this -- the types provide
                 information about the calling context rather than the code of
                 the function. *)
              let params =
                List.map2 (fun (param1, t1) (param2, t2) ->
                    assert (Parameter.equal param1 param2);
                    let t =
                      Meet_and_join.join env env_extension1 env_extension2
                        t1 t2
                    in
                    param1, t)
                  inlinable1.params
                  inlinable2.params
              in
              let scope_level =
                Scope_level.next (Typing_env.max_level env)
              in
              let env_with_params =
                List.fold_left (fun env (param, ty) ->
                    let param_name = Parameter.name param in
                    Typing_env.add env param_name scope_level
                      (Definition ty))
                  env
                  params
              in
              let result_env_extension =
                Typing_env_extension.join env_with_params
                  env_extension1
                  env_extension2
                  inlinable1.result_env_extension
                  inlinable2.result_env_extension
              in
              let result =
                let result_env =
                  Typing_env.add_or_meet_env_extension env_with_params
                    result_env_extension
                    scope_level
                in
                let result_env_extension1 =
                  Typing_env_extension.diff inlinable1.result_env_extension
                    result_env
                in
                let result_env_extension2 =
                  Typing_env_extension.diff inlinable2.result_env_extension
                    result_env
                in
                List.map2 (fun t1 t2 ->
                    Meet_and_join.join result_env
                      result_env_extension1 result_env_extension2
                      t1 t2)
                  inlinable1.result
                  inlinable2.result
              in
              let direct_call_surrogate =
                match inlinable1.direct_call_surrogate,
                      inlinable2.direct_call_surrogate
                with
                | Some closure_id1, Some closure_id2
                    when Closure_id.equal closure_id1 closure_id2 ->
                  Some closure_id1
                | _, _ -> None
              in
              Inlinable {
                closure_origin = inlinable1.closure_origin;
                continuation_param = inlinable1.continuation_param;
                exn_continuation_param = inlinable1.exn_continuation_param;
                is_classic_mode = inlinable1.is_classic_mode;
                params;
                code_id = inlinable1.code_id;
                body = inlinable1.body;
                free_names_in_body = inlinable1.free_names_in_body;
                result;
                result_env_extension;
                stub = inlinable1.stub;
                dbg = inlinable1.dbg;
                inline = inlinable1.inline;
                specialise = inlinable1.specialise;
                is_a_functor = inlinable1.is_a_functor;
                invariant_params = inlinable1.invariant_params;
                size = inlinable1.size;
                direct_call_surrogate;
                my_closure = inlinable1.my_closure;
              }
            end
        in
        { function_decls; }
      end
*)

    let meet_or_join_set_of_closures env
          (set1 : set_of_closures) (set2 : set_of_closures)
          : (set_of_closures * env_extension) Or_absorbing.t =
      let equations = ref (Typing_env_extension.empty) in
      (* CR mshinwell: Try to refactor this code to shorten it. *)
      let closures : _ extensibility =
        match set1.closures, set2.closures with
        | Exactly closures1, Exactly closures2 ->
          let closures =
            E.Closure_id.Map.union_or_inter
              (fun ty_fabricated1 ty_fabricated2 ->
                let ty_fabricated, new_equations =
                  Meet_and_join_fabricated.meet_or_join_ty env
                    ty_fabricated1 ty_fabricated2
                in
                if ty_is_obviously_bottom ty_fabricated then begin
                  None
                end else begin
                  equations :=
                    Typing_env_extension.meet env
                      new_equations !equations;
                  Some ty_fabricated
                end)
              closures1
              closures2
          in
          (* CR mshinwell: Try to move this check into the intersection
             operation above (although note we still need to check the
             cardinality) *)
          let same_as_closures old_closures =
            match
              Closure_id.Map.for_all2_opt (fun ty_fabricated1 ty_fabricated2 ->
                  ty_fabricated1 == ty_fabricated2)
                old_closures closures
            with
            | None -> false
            | Some same -> same
          in
          if same_as_closures closures1 then set1.closures
          else if same_as_closures closures2 then set2.closures
          else Exactly closures
        | Exactly closures1, Open closures2
        | Open closures2, Exactly closures1 ->
          let closures =
            E.Closure_id.Map.union_or_inter_and_left (fun ty1 ty2 ->
                let ty_fabricated, new_equations =
                  Meet_and_join_fabricated.meet_or_join_ty env
                    ty1 ty2
                in
                if ty_is_obviously_bottom ty_fabricated then begin
                  None
                end else begin
                  equations :=
                    Typing_env_extension.meet env
                      new_equations !equations;
                  Some ty_fabricated
                end)
              closures1 closures2  (* N.B. the order matters on this line *)
          in
          Exactly closures
        | Open closures1, Open closures2 ->
          let closures =
            E.Closure_id.Map.union_or_inter
              (fun ty_fabricated1 ty_fabricated2 ->
                let ty_fabricated, new_equations =
                  Meet_and_join_fabricated.meet_or_join_ty env
                    ty_fabricated1 ty_fabricated2
                in
                if ty_is_obviously_bottom ty_fabricated then begin
                  None
                end else begin
                  equations :=
                    Typing_env_extension.meet env
                      new_equations !equations;
                  Some ty_fabricated
                end)
              closures1
              closures2
          in
          Open closures
      in
      let closure_elements =
        match set1.closure_elements, set2.closure_elements with
        | Exactly closure_elements1, Exactly closure_elements2 ->
          let closure_elements =
            E.Var_within_closure.Map.union_or_inter (fun ty_value1 ty_value2 ->
                let ty_value, new_equations =
                  Meet_and_join_value.meet_or_join_ty env
                    ty_value1 ty_value2
                in
                if ty_is_obviously_bottom ty_value then begin
                  None
                end else begin
                  equations :=
                    Typing_env_extension.meet env
                      new_equations !equations;
                  Some ty_value
                end)
              closure_elements1
              closure_elements2
          in
          let same_as_closure_elements old_closure_elements =
            match
              Var_within_closure.Map.for_all2_opt (fun ty_value1 ty_value2 ->
                  ty_value1 == ty_value2)
                old_closure_elements closure_elements
            with
            | None -> false
            | Some same -> same
          in
          if same_as_closure_elements closure_elements1 then
            set1.closure_elements
          else if same_as_closure_elements closure_elements2 then
            set2.closure_elements
          else
            Exactly closure_elements
        | Exactly closure_elements1, Open closure_elements2
        | Open closure_elements2, Exactly closure_elements1 ->
          let closures =
            E.Var_within_closure.Map.union_or_inter_and_left (fun ty1 ty2 ->
                let ty_value, new_equations =
                  Meet_and_join_value.meet_or_join_ty env
                    ty1 ty2
                in
                if ty_is_obviously_bottom ty_value then begin
                  None
                end else begin
                  equations :=
                    Typing_env_extension.meet env
                      new_equations !equations;
                  Some ty_value
                end)
              closures1 closures2  (* N.B. the order matters on this line *)
          in
          Exactly closures
        | Open closure_elements1, Open closure_elements2 ->
          let closure_elements =
            E.Var_within_closure.Map.union_or_inter
              (fun ty_value1 ty_value2 ->
                let ty_value, new_equations =
                  Meet_and_join_value.meet_or_join_ty env
                    ty_value1 ty_value2
                in
                if ty_is_obviously_bottom ty_value then begin
                  bottom_as_ty_value ()
                end else begin
                  equations :=
                    Typing_env_extension.meet env
                      new_equations !equations;
                  ty_value
                end)
              closure_elements1
              closure_elements2
          in
          Open closure_elements
      in
      match closures with
      | Exactly map when Closure_id.Map.is_empty map -> Absorbing
      | _ ->
        if closures == set1.closures
          && closure_elements == set1.closure_elements
        then Ok (set1, !equations)
        else if closures == set2.closures
          && closure_elements == set2.closure_elements
        then Ok (set2, !equations)
        else begin
          let set : set_of_closures =
            { closures;
              closure_elements;
            }
          in
          Ok (set, !equations)
        end

    let meet_or_join_of_kind_foo env
          (of_kind1 : of_kind_fabricated) (of_kind2 : of_kind_fabricated)
          : (of_kind_fabricated * env_extension) Or_absorbing.t =
      if
        env.fast_check_extensions_same_both_sides env
          && of_kind1 == of_kind2
      then
        Ok (of_kind1, Typing_env_extension.empty)
      else
        match of_kind1, of_kind2 with
        | Discriminant discriminants1, Discriminant discriminants2 ->
          let discriminants =
            E.Discriminant.Map.union_or_inter_both
              ~in_left_only:
                (fun ({ env_extension; } : discriminant_case)
                      : discriminant_case ->
                  let env_extension = JE.holds_on_left env in
                  { env_extension; })
              ~in_right_only:
                (fun ({ env_extension; } : discriminant_case)
                      : discriminant_case ->
                  let env_extension = JE.holds_on_right env in
                  { env_extension; })
              ~in_both:
                (fun ({ env_extension = env_extension1; } : discriminant_case)
                     ({ env_extension = env_extension2; } : discriminant_case)
                     : discriminant_case ->
                  let env_extension =
                    E.Typing_env_extension.meet_or_join env
                      env_extension1 env_extension2
                  in
                  { env_extension; })
          in
          begin match Discriminant.Map.get_singleton discriminants with
          | None ->
            if Discriminant.Map.is_empty discriminants then
              Absorbing
            else
              Ok (Discriminant discriminants, Typing_env_extension.empty)
          | Some (discriminant, discriminant_case) ->
            let discriminants =
              Discriminant.Map.singleton discriminant
                ({ env_extension = Typing_env_extension.empty; }
                  : discriminant_case)
            in
            Ok (Discriminant discriminants, discriminant_case.env_extension)
          end
        | Set_of_closures set1, Set_of_closures set2 ->
          begin match meet_set_of_closures env set1 set2 with
          | Ok (set_of_closures, equations) ->
            if set_of_closures == set1 then
              Ok (of_kind1, equations)
            else if set_of_closures == set2 then
              Ok (of_kind2, equations)
            else
              Ok (Set_of_closures set_of_closures, equations)
          | Absorbing ->
            (* CR mshinwell: Here and elsewhere we are relying on this case
               never been taken for [join] *)
            Absorbing
          end
        | Closure closure1, Closure closure2 ->
          begin match meet_closure env closure1 closure2 with
          | Ok (closure, equations) ->
            if closure == closure1 then
              Ok (of_kind1, equations)
            else if closure == closure2 then
              Ok (of_kind2, equations)
            else
              Ok (Closure closure, equations)
          | Absorbing -> Absorbing
          end
        | (Discriminant _ | Set_of_closures _ | Closure _), _ -> Absorbing
  end)

  include Meet_and_join_fabricated
end
