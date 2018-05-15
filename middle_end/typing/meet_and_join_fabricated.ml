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

module Make (T : sig
  include Flambda_type0_internal_intf.S

  val print_ty_fabricated
     : Format.formatter
    -> ty_fabricated
    -> unit

  val is_obviously_bottom : flambda_type -> bool

  val ty_is_obviously_bottom : 'a ty -> bool

  val force_to_kind_fabricated : t -> of_kind_fabricated ty

  val bottom_as_ty_fabricated : unit -> of_kind_fabricated ty

  val bottom_as_ty_value : unit -> of_kind_value ty

  val any_fabricated_as_ty_fabricated : unit -> of_kind_fabricated ty

  val any_value_as_ty_value : unit -> of_kind_value ty
end) (Make_meet_and_join : functor
    (S : sig
      include Meet_and_join_spec_intf.S
        with type flambda_type := T.flambda_type
        with type typing_environment := T.typing_environment
        with type env_extension := T.env_extension
        with type 'a ty := 'a T.ty
     end)
  -> sig
       include Meet_and_join_intf.S
         with type of_kind_foo := S.of_kind_foo
         with type typing_environment := T.typing_environment
         with type env_extension := T.env_extension
         with type 'a ty := 'a T.ty
    end) (Meet_and_join_value : sig
      include Meet_and_join_intf.S
        with type of_kind_foo := T.of_kind_value
        with type typing_environment := T.typing_environment
        with type env_extension := T.env_extension
        with type 'a ty := 'a T.ty
    end) (Meet_and_join : sig
      include Meet_and_join_intf.S_for_types
        with type typing_environment := T.typing_environment
        with type env_extension := T.env_extension
        with type flambda_type := T.flambda_type
    end) (Typing_env : sig
      include Typing_env_intf.S
        with type typing_environment := T.typing_environment
        with type typing_environment_entry := T.typing_environment_entry
        with type env_extension := T.env_extension
        with type flambda_type := T.flambda_type
        with type t_in_context := T.t_in_context
        with type 'a ty := 'a T.ty
        with type 'a unknown_or_join := 'a T.unknown_or_join
    end) (Typing_env_extension : sig
      include Typing_env_extension_intf.S
        with type env_extension := T.env_extension
        with type typing_environment := T.typing_environment
        with type flambda_type := T.flambda_type
    end) =
struct
  module rec Meet_and_join_fabricated : sig
    include Meet_and_join_intf.S
      with type of_kind_foo := T.of_kind_fabricated
      with type typing_environment := T.typing_environment
      with type env_extension := T.env_extension
      with type 'a ty := 'a T.ty
  end = Make_meet_and_join (struct
    open T

    type env_extension = T.env_extension

    type of_kind_foo = of_kind_fabricated

    let kind = K.fabricated ()

    let to_type ty : t =
      { descr = Fabricated ty;
        phantom = None;
      }

    let force_to_kind = force_to_kind_fabricated
    let print_ty = print_ty_fabricated

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
          let env_extension_from_meet = ref (Typing_env_extension.empty) in
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
                    let t, new_env_extension_from_meet =
                      Meet_and_join.meet env t1 t2
                    in
                    if not (t == t1) then begin
                      params_changed := join_changes !params_changed Left
                    end;
                    if not (t == t2) then begin
                      params_changed := join_changes !params_changed Right
                    end;
                    env_extension_from_meet :=
                      Typing_env_extension.meet env
                        new_env_extension_from_meet !env_extension_from_meet;
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
                      let t, new_env_extension_from_meet =
                        Meet_and_join.meet result_env t1 t2
                      in
                      if not (t == t1) then begin
                        result_changed := join_changes !result_changed Left
                      end;
                      if not (t == t2) then begin
                        result_changed := join_changes !result_changed Right
                      end;
                      env_extension_from_meet :=
                        Typing_env_extension.meet result_env
                          new_env_extension_from_meet !env_extension_from_meet;
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
                !env_extension_from_meet)
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
                  env_extension_from_meet) ->
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
              | Neither -> Ok (closure1.function_decls, env_extension_from_meet)
              | Left -> Ok (closure2.function_decls, env_extension_from_meet)
              | Right -> Ok (closure1.function_decls, env_extension_from_meet)
              | Both ->
                Ok (Inlinable { inlinable1 with
                  params;
                  result;
                  result_env_extension;
                }, env_extension_from_meet)
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
                ~param_names1:None
                ~param_names2:None
                ~result1:non_inlinable1.result
                ~result2:non_inlinable2.result
                ~result_env_extension1:non_inlinable1.result_env_extension
                ~result_env_extension2:non_inlinable2.result_env_extension
            in
            begin match result with
            | Ok (params, _params_changed, result, result_env_extension,
                  env_extension_from_meet) ->
              let non_inlinable_function_decl =
                { non_inlinable1 with
                  params;
                  result;
                  result_env_extension;
                }
              in
              Ok (Non_inlinable (Some non_inlinable_function_decl),
                env_extension_from_meet)
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
                  env_extension_from_meet) ->
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
              Ok (Inlinable inlinable_function_decl, env_extension_from_meet)
            | Bottom ->
              Bottom
            end
        in
        match function_decls with
        | Bottom -> Bottom
        | Ok (function_decls, env_extension_from_meet) ->
          if function_decls == closure1.function_decls then
            Ok (closure1, env_extension_from_meet)
          else if function_decls == closure2.function_decls then
            Ok (closure2, env_extension_from_meet)
          else
            Ok (({ function_decls; } : closure), env_extension_from_meet)
      end

    let join_closure env env_extension1 env_extension2
          (closure1 : closure) (closure2 : closure)
          : closure =
      if env_extension1 == env_extension2 && closure1 == closure2 then begin
        closure1
      end else begin
        let produce_non_inlinable ~params1 ~params2
              ~param_names1 ~param_names2 ~result1 ~result2
              ~result_env_extension1 ~result_env_extension2
              ~direct_call_surrogate1 ~direct_call_surrogate2 =
          let same_arity = List.compare_lengths params1 params2 = 0 in
          let same_num_results = List.compare_lengths result1 result2 = 0 in
          if same_arity && same_num_results then
            let params =
              List.map2 (fun t1 t2 ->
                  Meet_and_join.join env env_extension1 env_extension2 t1 t2)
                params1
                params2
            in
            let add_params_to_env_extension param_names env_extension =
              match param_names with
              | None -> env_extension
              | Some param_names ->
                List.fold_left2 (fun env_extension param param_name ->
                    Typing_env_extension.add_definition_at_beginning
                      env_extension (Parameter.name param_name) param)
                  env_extension
                  params param_names
            in
            let result_env_extension1 =
              add_params_to_env_extension param_names1 result_env_extension1
            in
            let result_env_extension2 =
              add_params_to_env_extension param_names2 result_env_extension2
            in
            let result_env_extension =
              Typing_env_extension.join env
                env_extension1
                env_extension2
                result_env_extension1
                result_env_extension2
            in
            let result =
              let result_env =
                Typing_env.add_or_meet_env_extension env
                  result_env_extension
                  (Typing_env.max_level env)
              in
              let result_env_extension1 =
                Typing_env_extension.diff result_env_extension1 result_env
              in
              let result_env_extension2 =
                Typing_env_extension.diff result_env_extension2 result_env
              in
              List.map2 (fun t1 t2 ->
                  Meet_and_join.join result_env
                    result_env_extension1 result_env_extension2
                    t1 t2)
                result1
                result2
            in
            let direct_call_surrogate =
              match direct_call_surrogate1, direct_call_surrogate2 with
              | Some closure_id1, Some closure_id2
                  when Closure_id.equal closure_id1 closure_id2 ->
                Some closure_id1
              | _, _ -> None
            in
            let non_inlinable : non_inlinable_function_declarations =
              { params;
                result;
                result_env_extension;
                direct_call_surrogate;
              }
            in
            Non_inlinable (Some non_inlinable)
          else
            Non_inlinable None
        in
        let function_decls : function_declarations =
          match closure1.function_decls, closure2.function_decls with
          | Non_inlinable None, _ | _, Non_inlinable None -> Non_inlinable None
          | Non_inlinable (Some non_inlinable1),
              Non_inlinable (Some non_inlinable2) ->
            produce_non_inlinable
              ~params1:non_inlinable1.params
              ~params2:non_inlinable2.params
              ~param_names1:None
              ~param_names2:None
              ~result1:non_inlinable1.result
              ~result2:non_inlinable2.result
              ~result_env_extension1:non_inlinable1.result_env_extension
              ~result_env_extension2:non_inlinable2.result_env_extension
              ~direct_call_surrogate1:non_inlinable1.direct_call_surrogate
              ~direct_call_surrogate2:non_inlinable2.direct_call_surrogate
          | Non_inlinable (Some non_inlinable), Inlinable inlinable
          (* CR mshinwell: This should use a record pattern match rather than
             field projection *)
          | Inlinable inlinable, Non_inlinable (Some non_inlinable) ->
            let params1 = List.map snd inlinable.params in
            let param_names1 = List.map fst inlinable.params in
            produce_non_inlinable
              ~params1
              ~params2:non_inlinable.params
              ~param_names1:(Some param_names1)
              ~param_names2:None
              ~result1:inlinable.result
              ~result2:non_inlinable.result
              ~result_env_extension1:inlinable.result_env_extension
              ~result_env_extension2:non_inlinable.result_env_extension
              ~direct_call_surrogate1:inlinable.direct_call_surrogate
              ~direct_call_surrogate2:non_inlinable.direct_call_surrogate
          | Inlinable inlinable1, Inlinable inlinable2 ->
            if not (Code_id.equal inlinable1.code_id inlinable2.code_id)
            then begin
              let params1 = List.map snd inlinable1.params in
              let params2 = List.map snd inlinable2.params in
              let param_names1 = List.map fst inlinable1.params in
              let param_names2 = List.map fst inlinable2.params in
              produce_non_inlinable
                ~params1
                ~params2
                ~param_names1:(Some param_names1)
                ~param_names2:(Some param_names2)
                ~result1:inlinable1.result
                ~result2:inlinable2.result
                ~result_env_extension1:inlinable1.result_env_extension
                ~result_env_extension2:inlinable2.result_env_extension
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

    let meet_set_of_closures env
          (set1 : set_of_closures) (set2 : set_of_closures)
          : (set_of_closures * env_extension) Or_bottom.t =
      let env_extension_from_meet = ref (Typing_env_extension.empty) in
      (* CR mshinwell: Try to refactor this code to shorten it. *)
      let closures : _ extensibility =
        match set1.closures, set2.closures with
        | Exactly closures1, Exactly closures2 ->
          let closures =
            Closure_id.Map.inter (fun ty_fabricated1 ty_fabricated2 ->
                let ty_fabricated, new_env_extension_from_meet =
                  Meet_and_join_fabricated.meet_ty env
                    ty_fabricated1 ty_fabricated2
                in
                if ty_is_obviously_bottom ty_fabricated then begin
                  None
                end else begin
                  env_extension_from_meet :=
                    Typing_env_extension.meet env
                      new_env_extension_from_meet !env_extension_from_meet;
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
            Closure_id.Map.filter_map (fun closure_id ty1 ->
                match Closure_id.Map.find closure_id closures2 with
                | exception Not_found -> Some ty1
                | ty2 ->
                  let ty_fabricated, new_env_extension_from_meet =
                    Meet_and_join_fabricated.meet_ty env ty1 ty2
                  in
                  if ty_is_obviously_bottom ty_fabricated then begin
                    None
                  end else begin
                    env_extension_from_meet :=
                      Typing_env_extension.meet env
                        new_env_extension_from_meet !env_extension_from_meet;
                    Some ty_fabricated
                  end)
              closures1
          in
          Exactly closures
        | Open closures1, Open closures2 ->
          let closures =
            Closure_id.Map.union_merge (fun ty_fabricated1 ty_fabricated2 ->
                let ty_fabricated, new_env_extension_from_meet =
                  Meet_and_join_fabricated.meet_ty env
                    ty_fabricated1 ty_fabricated2
                in
                if ty_is_obviously_bottom ty_fabricated then begin
                  bottom_as_ty_fabricated ()
                end else begin
                  env_extension_from_meet :=
                    Typing_env_extension.meet env
                      new_env_extension_from_meet !env_extension_from_meet;
                  ty_fabricated
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
            Var_within_closure.Map.inter (fun ty_value1 ty_value2 ->
                let ty_value, new_env_extension_from_meet =
                  Meet_and_join_value.meet_ty env ty_value1 ty_value2
                in
                if ty_is_obviously_bottom ty_value then begin
                  None
                end else begin
                  env_extension_from_meet :=
                    Typing_env_extension.meet env
                      new_env_extension_from_meet !env_extension_from_meet;
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
          let closure_elements =
            Var_within_closure.Map.filter_map (fun closure_id ty1 ->
                match
                  Var_within_closure.Map.find closure_id closure_elements2
                with
                | exception Not_found -> Some ty1
                | ty2 ->
                  let ty_value, new_env_extension_from_meet =
                    Meet_and_join_value.meet_ty env ty1 ty2
                  in
                  if ty_is_obviously_bottom ty_value then begin
                    None
                  end else begin
                    env_extension_from_meet :=
                      Typing_env_extension.meet env
                        new_env_extension_from_meet !env_extension_from_meet;
                    Some ty_value
                  end)
              closure_elements1
          in
          Exactly closure_elements
        | Open closure_elements1, Open closure_elements2 ->
          let closure_elements =
            Var_within_closure.Map.union_merge (fun ty_value1 ty_value2 ->
                let ty_value, new_env_extension_from_meet =
                  Meet_and_join_value.meet_ty env ty_value1 ty_value2
                in
                if ty_is_obviously_bottom ty_value then begin
                  bottom_as_ty_value ()
                end else begin
                  env_extension_from_meet :=
                    Typing_env_extension.meet env new_env_extension_from_meet
                      !env_extension_from_meet;
                  ty_value
                end)
              closure_elements1
              closure_elements2
          in
          Open closure_elements
      in
      match closures with
      | Exactly map when Closure_id.Map.is_empty map -> Bottom
      | _ ->
        if closures == set1.closures
          && closure_elements == set1.closure_elements
        then Ok (set1, !env_extension_from_meet)
        else if closures == set2.closures
          && closure_elements == set2.closure_elements
        then Ok (set2, !env_extension_from_meet)
        else begin
          let set : set_of_closures =
            { closures;
              closure_elements;
            }
          in
          Ok (set, !env_extension_from_meet)
        end

    let join_set_of_closures env env_extension1 env_extension2
          (set1 : set_of_closures) (set2 : set_of_closures)
          : set_of_closures =
      let closures : _ extensibility =
        match set1.closures, set2.closures with
        | Exactly closures1, Exactly closures2 ->
          let closures =
            Closure_id.Map.union_merge
              (fun ty_fabricated1 ty_fabricated2 ->
                Meet_and_join_fabricated.join_ty env
                  env_extension1 env_extension2
                  ty_fabricated1 ty_fabricated2)
              closures1
              closures2
          in
          Exactly closures
        | Exactly closures1, Open closures2
        | Open closures1, Exactly closures2 ->
          let closures =
            Closure_id.Map.union_merge
              (fun ty_fabricated1 ty_fabricated2 ->
                Meet_and_join_fabricated.join_ty env
                  env_extension1 env_extension2
                  ty_fabricated1 ty_fabricated2)
              closures1
              closures2
          in
          Open closures
        | Open closures1, Open closures2 ->
          let closures =
            Closure_id.Map.union_both
              (fun _ty_fabricated ->
                any_fabricated_as_ty_fabricated ())
              (fun ty_fabricated1 ty_fabricated2 ->
                Meet_and_join_fabricated.join_ty env
                  env_extension1 env_extension2
                  ty_fabricated1 ty_fabricated2)
              closures1
              closures2
          in
          Open closures
      in
      let closure_elements : _ extensibility =
        match set1.closure_elements, set2.closure_elements with
        | Exactly closure_elements1, Exactly closure_elements2 ->
          let closure_elements =
            Var_within_closure.Map.union_merge
              (fun ty_value1 ty_value2 ->
                Meet_and_join_value.join_ty env env_extension1 env_extension2
                  ty_value1 ty_value2)
              closure_elements1
              closure_elements2
          in
          Exactly closure_elements
        | Exactly closure_elements1, Open closure_elements2
        | Open closure_elements1, Exactly closure_elements2 ->
          let closure_elements =
            Var_within_closure.Map.union_merge
              (fun ty_value1 ty_value2 ->
                Meet_and_join_value.join_ty env env_extension1 env_extension2
                  ty_value1 ty_value2)
              closure_elements1
              closure_elements2
          in
          Open closure_elements
        | Open closure_elements1, Open closure_elements2 ->
          let closure_elements =
            Var_within_closure.Map.union_both
              (fun _ty_value ->
                any_value_as_ty_value ())
              (fun ty_value1 ty_value2 ->
                Meet_and_join_value.join_ty env env_extension1 env_extension2
                  ty_value1 ty_value2)
              closure_elements1
              closure_elements2
          in
          Open closure_elements
      in
      if closures == set1.closures
        && closure_elements == set1.closure_elements
      then
        set1
      else if closures == set2.closures
        && closure_elements == set2.closure_elements
      then
        set2
      else
        { closures;
          closure_elements;
        }

    let meet_of_kind_foo env
          (of_kind1 : of_kind_fabricated) (of_kind2 : of_kind_fabricated)
          : (of_kind_fabricated * env_extension) Or_bottom.t =
      match of_kind1, of_kind2 with
      | Discriminant discriminants1, Discriminant discriminants2 ->
        let discriminants =
          Discriminant.Map.inter_merge
            (fun ({ env_extension = env_extension1; } : discriminant_case)
                  ({ env_extension = env_extension2; } : discriminant_case)
                  : discriminant_case ->
              let env_extension =
                Typing_env_extension.meet env
                  env_extension1 env_extension2
              in
              (* CR mshinwell: Do we ever flip back to [Bottom] here? *)
              { env_extension; })
            discriminants1
            discriminants2
        in
        begin match Discriminant.Map.get_singleton discriminants with
        | None ->
          Ok (Discriminant discriminants, Typing_env_extension.empty)
        | Some (discriminant, discriminant_case) ->
          let env_extension_from_meet = discriminant_case.env_extension in
          let discriminants =
            Discriminant.Map.singleton discriminant
              ({ env_extension = Typing_env_extension.empty; }
                : discriminant_case)
          in
          Ok (Discriminant discriminants, env_extension_from_meet)
        end
      | Set_of_closures set1, Set_of_closures set2 ->
        begin match meet_set_of_closures env set1 set2 with
        | Ok (set_of_closures, env_extension_from_meet) ->
          if set_of_closures == set1 then
            Ok (of_kind1, env_extension_from_meet)
          else if set_of_closures == set2 then
            Ok (of_kind2, env_extension_from_meet)
          else
            Ok (Set_of_closures set_of_closures, env_extension_from_meet)
        | Bottom -> Bottom
        end
      | Closure closure1, Closure closure2 ->
        begin match meet_closure env closure1 closure2 with
        | Ok (closure, env_extension_from_meet) ->
          if closure == closure1 then
            Ok (of_kind1, env_extension_from_meet)
          else if closure == closure2 then
            Ok (of_kind2, env_extension_from_meet)
          else
            Ok (Closure closure, env_extension_from_meet)
        | Bottom -> Bottom
        end
      | (Discriminant _ | Set_of_closures _ | Closure _), _ -> Bottom

    let join_of_kind_foo env env_extension1 env_extension2
          (of_kind1 : of_kind_fabricated) (of_kind2 : of_kind_fabricated)
          : of_kind_fabricated Or_unknown.t =
      match of_kind1, of_kind2 with
      | Discriminant discriminants1, Discriminant discriminants2 ->
        let discriminants =
          Discriminant.Map.union_merge
            (fun ({ env_extension = env_extension1'; } : discriminant_case)
                  ({ env_extension = env_extension2'; } : discriminant_case)
                  : discriminant_case ->
              let env_extension =
                Typing_env_extension.join env
                  env_extension1 env_extension2
                  env_extension1' env_extension2'
              in
              { env_extension; })
            discriminants1
            discriminants2
        in
        Known (Discriminant discriminants)
      | Set_of_closures set1, Set_of_closures set2 ->
        let set_of_closures =
          join_set_of_closures env env_extension1 env_extension2 set1 set2
        in
        Known (Set_of_closures set_of_closures)
      | Closure closure1, Closure closure2 ->
        let closure =
          join_closure env env_extension1 env_extension2 closure1 closure2
        in
        Known (Closure closure)
      | (Discriminant _ | Set_of_closures _ | Closure _), _ -> Unknown
  end)

  include Meet_and_join_fabricated
end
