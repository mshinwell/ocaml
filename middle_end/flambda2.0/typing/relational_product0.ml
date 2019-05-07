
module Make
  (Index : Name_like_intf.S)
  (Component : sig
    include Name_like_intf.S
    val create : Flambda_kind.t -> t
    val equal
       : Type_equality_env.t
      -> t
      -> t
      -> bool
    val name : t -> Name.t
    val kind : t -> Flambda_kind.t
  end)
  (Nested : sig
    include Contains_names.S
(*
    val invariant : t -> unit
*)
    val print : Format.formatter -> t -> unit
    val print_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> t
      -> unit
    val equal
       : Type_equality_env.t
      -> Type_equality_result.t
      -> t
      -> t
      -> Type_equality_result.t
    val meet
       : Meet_env.t
      -> t
      -> t
      -> (t * Typing_env_extension.t) Or_bottom.t
    val join : Join_env.t -> t -> t -> t
  end) =
struct
  module T0 : sig
    type t = {
      components_by_index : Component.t Index.Map.t;
      env_extension : Typing_env_extension.t;
      nested : Nested.t option;
    }

    include Contains_names.S with type t := t

    val invariant : t -> unit
    val print : Format.formatter -> t -> unit
    val print_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> t
      -> unit
    val equal
       : Type_equality_env.t
      -> Type_equality_result.t
      -> t
      -> t
      -> Type_equality_result.t
    val widen : t -> to_match:t -> t
    val meet
       : Meet_env.t
      -> t
      -> t
      -> (t * Typing_env_extension.t * Component.t list) option Or_bottom.t
    val join : Join_env.t -> t -> t -> (t * Component.t list) option
    val standalone_extension : t -> Typing_env_extension.t
    val introduce : t -> Typing_env.t -> Typing_env.t
    val add_or_meet_equations
       : t
      -> Meet_env.t
      -> Typing_env_extension.t
      -> t
  end = struct
    type t = {
      components_by_index : Component.t Index.Map.t;
      env_extension : Typing_env_extension.t;
      nested : Nested.t option;
    }

    let invariant _t =
      (* CR mshinwell: This should check that the [env_extension] never
         contains [Definition]s for [Name]s occurring in the indexes. *)
      ()

    let print ppf { components_by_index; env_extension; nested; } =
      Format.fprintf ppf
        "@[<hov 1>(\
          @[<hov 1>(components_by_index@ %a)@]@ \
          @[<hov 1>(env_extension@ %a)@]@ \
          @[<hov 1>(nested@ %a)@])@]"
        (Index.Map.print Component.print) components_by_index
        Typing_env_extension.print env_extension
        (Misc.Stdlib.Option.print Nested.print) nested

    let print_with_cache ~cache ppf
          { components_by_index; env_extension; nested; } =
      Format.fprintf ppf
        "@[<hov 1>(\
          @[<hov 1>(components_by_index@ %a)@]@ \
          @[<hov 1>(env_extension@ %a)@]@ \
          @[<hov 1>(nested@ %a)@])@]"
        (Index.Map.print Component.print) components_by_index
        (Typing_env_extension.print_with_cache ~cache) env_extension
        (Misc.Stdlib.Option.print (Nested.print_with_cache ~cache)) nested

    let equal env result
          { components_by_index = components_by_index1;
            env_extension = env_extension1;
            nested = nested1;
          }
          { components_by_index = components_by_index2;
            env_extension = env_extension2;
            nested = nested2;
          } =
      let (>>=) = Type_equality_result.(>>=) in
      result
      >>= fun result ->
      let equal =
        Index.Map.fold2_stop_on_key_mismatch
          (fun _index component1 component2 equal ->
            if not equal then false
            else Component.equal env component1 component2)
          components_by_index1 components_by_index2
          true
      in
      let result =
        match equal with
        | None | Some false ->
          Type_equality_result.types_known_unequal ()
        | Some true -> result
      in
      result
      >>= function result ->
      let env =
        Index.Map.fold (fun _index component env ->
            Type_equality_env.add_definition_typing_env_left env
              (Component.name component)
              (Component.kind component))
          components_by_index1
          env
      in
      let env =
        Index.Map.fold (fun _index component env ->
            Type_equality_env.add_definition_typing_env_right env
              (Component.name component)
              (Component.kind component))
          components_by_index2
          env
      in
      Typing_env_extension.equal env result env_extension1 env_extension2
      >>= fun result ->
      match nested1, nested2 with
      | None, None -> result
      | None, Some _ | Some _, None ->
        Type_equality_result.types_known_unequal ()
      | Some nested1, Some nested2 ->
        Nested.equal env result nested1 nested2

    let free_names
          { components_by_index; env_extension; nested; } =
      let free_names_in_indexes =
        Index.Set.fold (fun index free_names ->
            Name_occurrences.union (Index.free_names index) free_names)
          (Index.Map.keys components_by_index)
          (Name_occurrences.create ())
      in
      let free_names_in_components =
        List.fold_left (fun free_names component ->
            Name_occurrences.union (Component.free_names component)
              free_names)
          (Name_occurrences.create ())
          (Index.Map.data components_by_index)
      in
      let free_names_in_nested =
        match nested with
        | None -> Name_occurrences.create ()
        | Some nested -> Nested.free_names nested
      in
      Name_occurrences.union_list [
        free_names_in_indexes;
        free_names_in_components;
        Typing_env_extension.free_names env_extension;
        free_names_in_nested;
      ]

    let apply_name_permutation
          { components_by_index; env_extension; nested; } perm =
      let components_by_index =
        Index.Map.fold (fun index component components_by_index ->
            let index = Index.apply_name_permutation index perm in
            let component =
              Component.apply_name_permutation component perm
            in
            Index.Map.add index component components_by_index)
          components_by_index
          Index.Map.empty
      in
      let env_extension =
        Typing_env_extension.apply_name_permutation env_extension perm
      in
      let nested =
        match nested with
        | None -> None
        | Some nested -> Some (Nested.apply_name_permutation nested perm)
      in
      { components_by_index;
        env_extension;
        nested;
      }

    let indexes t = Index.Map.keys t.components_by_index

    (* CR mshinwell: The [kind] may not be needed in [Component] but it
       isn't clear yet. We can sort this out later. At present all
       relational products map to components of kind [Value]. *)
    (* XXX Isn't this wrong?  Float arrays... *)
    let kind = Flambda_kind.value ()

    let kind_of_index _t _index = kind (* XXX *)

    let widen t ~to_match =
      let missing_indexes =
        Index.Set.diff (indexes to_match) (indexes t)
      in
      let components_by_index =
        Index.Set.fold (fun index components_by_index ->
            assert (not (Index.Map.mem index components_by_index));
            let kind = kind_of_index to_match index in
            let component = Component.create kind in
            Index.Map.add index component components_by_index)
          missing_indexes
          t.components_by_index
      in
      { t with components_by_index; }

    let environment_for_meet_or_join env (t1 : t) (t2 : t)
          ~indexes =
      let components_by_index_in_result, env =
        Index.Set.fold (fun index (components_by_index_in_result, env) ->
            let component = Component.create kind in
Format.eprintf "Made fresh component %a for RP meet/join\n%!"
Component.print component;
            let components_by_index_in_result =
              Index.Map.add index component components_by_index_in_result
            in
            let env =
              Join_env.add_definition_central_environment env
                (Component.name component) (Flambda_type0_core.unknown kind)
            in
            components_by_index_in_result, env)
          indexes
          (Index.Map.empty, env)
      in
      let result_components =
        Index.Map.data components_by_index_in_result
      in
      let add_equalities_to_extension t =
        Index.Map.fold (fun index component env_extension ->
            let name = Component.name component in
            let kind = Component.kind component in
            match Index.Map.find index components_by_index_in_result with
            | exception Not_found -> env_extension
            | result_component ->
              let result_name = Component.name result_component in
              let name_ty =
                Flambda_type0_core.alias_type_of kind
                  (Simple.name name)
              in
              Typing_env_extension.add_equation env_extension
                result_name name_ty)
          t.components_by_index
          t.env_extension
      in
      let env_extension1 = add_equalities_to_extension t1 in
      let env_extension2 = add_equalities_to_extension t2 in
      let add_definitions_to_extension t env_extension =
        Index.Map.fold (fun _index component env_extension ->
(*
            if not (Index.Set.mem index indexes) then
              env_extension
            else
*)
              let name = Component.name component in
              let kind = Component.kind component in
              Typing_env_extension.add_definition env_extension
                name kind)
          t.components_by_index
          env_extension
      in
      let env_extension1 = add_definitions_to_extension t1 env_extension1 in
      let env_extension2 = add_definitions_to_extension t2 env_extension2 in
      env, env_extension1, env_extension2, components_by_index_in_result,
        result_components

    let meet env t1 t2 : _ Or_bottom.t =
      (* CR mshinwell: Use a proper type instead of "option" for the return
         value? *)
      if Meet_env.shortcut_precondition env && t1 == t2 then
        Ok None
      else
        let indexes = Index.Set.inter (indexes t1) (indexes t2) in
        if Index.Set.is_empty indexes then Bottom
        else
          let env = Join_env.create env in
Format.eprintf "For RP meet:@ t1: %a@;t2: %a\n%!"
print t1 print t2;
          let env, env_extension1, env_extension2, components_by_index,
              result_components =
            environment_for_meet_or_join env t1 t2 ~indexes
          in
Format.eprintf "Env for RP meet:@ env: %a@;env_extension1: %a@;env_extension2: %a\n%!"
Typing_env.print (Meet_env.env (Join_env.central_environment env))
Typing_env_extension.print env_extension1
Typing_env_extension.print env_extension2;
          let env = Join_env.central_environment env in
          let env_extension =
            Typing_env_extension.meet env env_extension1 env_extension2
          in
          let nested : _ Or_bottom.t =
            match t1.nested, t2.nested with
            | None, None -> Ok None
            | Some nested1, Some nested2 ->
              begin match Nested.meet env nested1 nested2 with
              | Ok (nested, env_extension) ->
                Ok (Some (nested, env_extension))
              | Bottom -> Bottom
              end
          | None, Some _ | Some _, None ->
            Misc.fatal_errorf "Cannot meet relational products with \
                different nesting structures:@ %a@ and@ %a"
              print t1
              print t2
          in
          match nested with
          | Bottom -> Bottom
          | Ok None ->
            let t =
              { components_by_index;
                env_extension;
                nested = None;
              }
            in
            let env_extension = Typing_env_extension.empty () in
            Ok (Some (t, env_extension, result_components))
          | Ok (Some (nested, _env_extension)) ->
            (* CR mshinwell: _env_extension should always be empty, but
               this seems like a wart *)
            let t =
              { components_by_index;
                env_extension;
                nested = Some nested;
              }
            in
            let env_extension = Typing_env_extension.empty () in
            Ok (Some (t, env_extension, result_components))

    let join env t1 t2 =
      (* CR mshinwell: Same comment as above re. return value.
         Or alternatively, maybe pass the components set into this
         function? *)
      if Join_env.shortcut_precondition env && t1 == t2 then
        None
      else
        let indexes = Index.Set.union (indexes t1) (indexes t2) in
        let env, env_extension1, env_extension2, components_by_index,
            result_components =
          environment_for_meet_or_join env t1 t2 ~indexes
        in
        let env_extension =
          Typing_env_extension.join env env_extension1 env_extension2
        in
        let nested =
          match t1.nested, t2.nested with
          | None, None -> None
          | Some nested1, Some nested2 ->
            Some (Nested.join env nested1 nested2)
          | None, Some _ | Some _, None ->
            Misc.fatal_errorf "Cannot join relational products with \
                different nesting structures:@ %a@ and@ %a"
              print t1
              print t2
        in
        let t =
          { components_by_index;
            env_extension;
            nested;
          }
        in
        Some (t, result_components)

    let standalone_extension t =
      Index.Map.fold (fun _index component env_extension ->
          let name = Component.name component in
          let kind = Component.kind component in
          Typing_env_extension.add_definition env_extension
            name kind)
        t.components_by_index
        t.env_extension

    let introduce t env =
      Typing_env.add_env_extension env (standalone_extension t)

    let add_or_meet_equations t env new_equations =
Format.eprintf "AOME %a, new equations %a\n%!" print t
Typing_env_extension.print new_equations;
      (* XXX mshinwell: check again *)
      let env = Meet_env.with_env env (fun env -> introduce t env) in
      let env_extension =
        Typing_env_extension.meet env t.env_extension new_equations
      in
      let t = { t with env_extension; } in
Format.eprintf "AOME %a\n%!" print t;
      t
  end

  include Name_abstraction.Make_list (T0)

  let create_abstraction = create

  let bindable_name_list_from_components components =
    List.map (fun component : Bindable_name.t ->
        Name (Component.name component))
      components

  let create ?nested components_by_index env_extension : t =
    let in_binding_position =
      let components = Index.Map.data components_by_index in
      bindable_name_list_from_components components
    in
    let t0 : T0.t =
      { components_by_index;
        env_extension;
        nested;
      }
    in
    T0.invariant t0;
    create in_binding_position t0

  let create_bottom () =
    create Index.Map.empty (Typing_env_extension.empty ())

  let invariant t =
    pattern_match t ~f:(fun _ t0 -> T0.invariant t0)

  let print ppf t = print ppf t

  let print_with_cache ~cache ppf t = print_with_cache ~cache ppf t

  let equal env result t1 t2 =
    pattern_match_pair t1 t2 ~f:(fun _ t0_1 t0_2 ->
      T0.equal env result t0_1 t0_2)

  let widen t ~to_match =
    pattern_match_map t ~f:(fun t0 ->
      pattern_match to_match ~f:(fun _ to_match ->
        T0.widen t0 ~to_match))

  let meet env t1 t2 =
    pattern_match t1 ~f:(fun _ t0_1 ->
      pattern_match t2 ~f:(fun _ t0_2 : _ Or_bottom.t ->
        match T0.meet env t0_1 t0_2 with
        | Bottom -> Bottom
        | Ok None -> Ok (t1, Typing_env_extension.empty ())
        | Ok (Some (t0, env_extension, components)) ->
          let in_binding_position =
            bindable_name_list_from_components components
          in
          Ok (create_abstraction in_binding_position t0, env_extension)))

  let join env t1 t2 =
    pattern_match t1 ~f:(fun components1 t0_1 ->
      pattern_match t2 ~f:(fun _ t0_2 ->
        match T0.join env t0_1 t0_2 with
        | None -> create_abstraction components1 t0_1
        | Some (t0, components) ->
          let in_binding_position =
            bindable_name_list_from_components components
          in
          create_abstraction in_binding_position t0))

  let standalone_extension t =
    pattern_match t ~f:(fun _ t0 -> T0.standalone_extension t0)

  let introduce t env =
    pattern_match t ~f:(fun _ t0 -> T0.introduce t0 env)

  let add_or_meet_equations t env new_equations =
    pattern_match_map t ~f:(fun t0 ->
      T0.add_or_meet_equations t0 env new_equations)
end
