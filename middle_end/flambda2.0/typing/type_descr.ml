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

module T = Type_grammar
module TE = Typing_env
module TEE = Typing_env_extension

module Make (Head : Type_head_intf.S
  with type meet_env := Meet_env.t
  with type typing_env_extension := Typing_env_extension.t
  with type type_grammar := Type_grammar.t)
= struct
  module Descr = struct
    type t =
      | No_alias of Head.t Or_unknown_or_bottom.t
      | Equals of Simple.t
      | Type of Export_id.t

    let print ppf t =
      match t with
      | No_alias Unknown ->
        if !Clflags.flambda2_unicode then
          Format.fprintf ppf "%s\u{22a4}%s" colour (Flambda_colours.normal ())
        else
          Format.fprintf ppf "%sT%s" colour (Flambda_colours.normal ())
      | No_alias Bottom ->
        if !Clflags.flambda2_unicode then
          Format.fprintf ppf "%s\u{22a5}%s" colour (Flambda_colours.normal ())
        else
          Format.fprintf ppf "%s_|_%s" colour (Flambda_colours.normal ())
      | No_alias (Ok head) -> Head.print ppf head
      | Equals simple ->
        Format.fprintf ppf "@[(%s=%s %a)@]"
          (Flambda_colours.error ())
          (Flambda_colours.normal ())
          Simple.print simple
      | Type export_id ->
        Format.fprintf ppf "@[(%s=export_id%s %a)@]"
          (Flambda_colours.error ())
          (Flambda_colours.normal ())
          Export_id.print export_id

    let apply_name_permutation t perm =
      if Name_permutation.is_empty perm then t
      else
        match t with
        | No_alias Bottom | No_alias Unknown -> t
        | No_alias (Ok head) ->
          let head' = Head.apply_name_permutation head perm in
          if head == head' then t
          else No_alias head'
        | Equals simple ->
          let simple' = Simple.apply_name_permutation simple perm in
          if simple == simple' then t
          else Equals simple'
        | Type _ -> t

    let free_names t =
      match t with
      | No_alias Bottom | No_alias Unknown -> Name_occurrences.empty
      | No_alias (Ok head) -> Head.free_names head
      | Equals simple -> Simple.free_names simple
      | Type _ -> Name_occurrences.empty
  end

  include With_delayed_permutation.Make (Descr)

  let print ppf t = Descr.print ppf (descr t)

  let create_no_alias head = create (No_alias head)
  let create_equals simple = create (Equals simple)
  let create_type export_id = create (Type export_id)

  let bottom = create (No_alias Bottom)
  let unknown = create (No_alias Unknown)

  let create head = create_no_alias (Ok head)

  let is_obviously_bottom t =
    match descr t with
    | No_alias Bottom -> true
    | No_alias (Ok _ | Unknown _)
    | Equals _ | Type _ -> false

  let get_alias t =
    match descr t with
    | Equals alias -> Some alias
    | No_alias _ | Type _ -> None
 
  let apply_rec_info t rec_info : _ Or_bottom.t =
    match descr t with
    | Equals simple ->
      let newer_rec_info = Some rec_info in
      begin match Simple.merge_rec_info simple ~newer_rec_info with
      | None -> Bottom
      | Some simple -> Ok (Equals simple)
      end
    | Type _ -> Misc.fatal_error "Not yet implemented"
    | No_alias Unknown -> Ok ty
    | No_alias Bottom -> Bottom
    | No_alias (Ok of_kind_foo) ->
      match apply_rec_info_of_kind_foo of_kind_foo rec_info with
      | Bottom -> Bottom
      | Ok of_kind_foo -> Ok (No_alias (Ok of_kind_foo))

  let make_suitable_for_environment t env ~suitable_for =
    let free_vars = Name_occurrences.variables (free_names t) in
    if Variable.Set.is_empty free_vars then t, suitable_for
    else
      let allowed = TE.var_domain suitable_for in
      let to_erase = Variable.Set.diff free_vars allowed in
      if Variable.Set.is_empty to_erase then t, suitable_for
      else
        let result_env, perm =
          Variable.Set.fold (fun to_erase (result_env, perm) ->
              let kind = TE.find_kind env to_erase in
              let fresh_var = Variable.rename to_erase in
              let fresh_var_name = Name.var fresh_var in
              let result_env =
                let name =
                  Name_in_binding_pos.create fresh_var_name
                    Name_occurrence_kind.in_types
                in
                TE.add_definition result_env name kind
              in
              let canonical_simple =
                TE.get_canonical_simple env
                  ~min_occurrence_kind:Name_occurrence_kind.in_types
                  (Simple.var to_erase)
              in
              let result_env =
                match TE.find_simple_opt env canonical_simple with
                | None -> result_env
                | Some simple ->
                  if not (Simple.allowed simple ~allowed) then result_env
                  else
                    let ty = T.alias_type_of simple kind in
                    TE.add_equation result_env fresh_var_name ty
              in
              let perm =
                Name_permutation.add_variable perm to_erase fresh_var
              in
              result_env, perm)
            to_erase
            (suitable_for, Name_permutation.empty)
        in
        result_env, apply_name_permutation t perm

  (* The [Make_operations] functor enables operations that involve
     [Type_grammar.t] values to be obtained. *)
  module Make_operations (S :
    val force_to_kind : Type_grammar.t -> t
    val to_type : t -> Type_grammar.t
  end) : sig
    let force_to_head t =
      match descr (S.force_to_kind t) with
      | No_alias head -> head
      | Type _ | Equals _ ->
        Misc.fatal_errorf "Expected [No_alias]:@ %a" print t

    let expand_head t env ~force_to_kind : _ Or_unknown_or_bottom.t =
      match descr t with
      | No_alias head -> head
      | Type _export_id -> Misc.fatal_error ".cmx loading not yet implemented"
      | Equals simple ->
        let min_occurrence_kind = Name_occurrence_kind.min in
        (* We must get the canonical simple with the least occurrence kind,
           since that's the one that is guaranteed not to have an [Equals]
           type. *)
        match TE.get_canonical_simple0 env simple ~min_occurrence_kind with
        | Bottom, _kind -> Bottom
        | Ok None, _kind ->
          Misc.fatal_errorf "There should always be a canonical simple for %a \
              in environment:@ %a"
            Simple.print simple
            TE.print env
        | Ok (Some (simple, rec_info)), _kind ->
          match Simple.descr simple with
          | Const const ->
            let typ =
              match const with
              | Naked_immediate i -> T.this_naked_immediate_without_alias i
              | Tagged_immediate i -> T.this_tagged_immediate_without_alias i
              | Naked_float f -> T.this_naked_float_without_alias f
              | Naked_int32 i -> T.this_naked_int32_without_alias i
              | Naked_int64 i -> T.this_naked_int64_without_alias i
              | Naked_nativeint i -> T.this_naked_nativeint_without_alias i
            in
            force_to_head typ ~force_to_kind
          | Discriminant discr ->
            let typ =
              match Discriminant.sort discr with
              | Int ->
                let imm = Immediate.int (Discriminant.to_int discr) in
                T.this_tagged_immediate_without_alias imm
              | Is_int | Tag -> T.this_discriminant_without_alias discr
            in
            force_to_head typ ~force_to_kind
          | Name name ->
            let t = S.force_to_kind (find t name) in
            match t with
            | No_alias Bottom -> Bottom
            | No_alias Unknown -> Unknown
            | No_alias (Ok head) ->
              begin match rec_info with
              | None -> Ok head
              | Some rec_info ->
                (* CR mshinwell: check rec_info handling is correct, after
                   recent changes in this area *)
                (* [simple] already has [rec_info] applied to it (see
                   [get_canonical_simple], above).  However we also need to
                   apply it to the expanded head of the type. *)
                match Head.apply_rec_info head rec_info with
                | Bottom -> Bottom
                | Ok head -> Ok head
              end
            | Type _export_id ->
              Misc.fatal_error ".cmx loading not yet implemented"
            | Equals _ ->
              Misc.fatal_errorf "Canonical alias %a should never have \
                  [Equals] type:%s@ %a"
                Simple.print simple
                TE.print env

    let add_equation _env (simple : Simple.t) ty env_extension =
      match Simple.descr simple with
      (* CR mshinwell: Does this need to use some kind of [meet_equation]? *)
      | Name name -> TEE.add_or_replace_equation env_extension name ty
      | Const _ | Discriminant _ -> env_extension

    let all_aliases_of env simple_opt =
      match simple_opt with
      | None -> Simple.Set.empty
      | Some simple ->
        Simple.Set.add simple (
          TE.aliases_of_simple_allowable_in_types env simple)

    let get_canonical_simples_and_expand_heads typing_env t1 t2 =
      let canonical_simple1 =
        TE.get_alias_ty_then_canonical_simple typing_env t1
      in
      let head1 = expand_head t1 typing_env in
      let canonical_simple2 =
        TE.get_alias_ty_then_canonical_simple typing_env t2
      in
      let head2 = expand_head t2 typing_env in
      canonical_simple1, head1, canonical_simple2, head2

    module Make_meet_and_join
      (E : Lattice_ops_intf.S
       with type meet_env := Meet_env.t
       with type typing_env := TE.t
       with type typing_env_extension := TEE.t) =
    struct
      module Head_meet_or_join = Head.Make_meet_or_join (E)

      let meet_head_or_unknown_or_bottom env
            (head1 : _ Or_unknown_or_bottom.t)
            (head2 : _ Or_unknown_or_bottom.t)
            : _ Or_unknown_or_bottom.t * TEE.t =
        match head1, head2 with
        | Unknown, head2 -> head2, TEE.empty ()
        | head1, Unknown -> head1, TEE.empty ()
        | Bottom, _ | _, Bottom -> Bottom, TEE.empty ()
        | Ok head1, Ok head2 ->
          match Head_meet_or_join.meet_or_join env head1 head2 with
          | Ok (head, env_extension) -> Ok head, env_extension
          | Absorbing | Bottom -> Bottom, TEE.empty ()

      let join_head_or_unknown_or_bottom env
            (head1 : _ Or_unknown_or_bottom.t)
            (head2 : _ Or_unknown_or_bottom.t)
            : _ Or_unknown_or_bottom.t * TEE.t =
        match head1, head2 with
        | Bottom, head2 -> head2
        | head1, Bottom -> head1
        | Unknown, _ | _, Unknown -> Unknown
        | Ok of_kind_foo1, Ok of_kind_foo2 ->
          let env = Meet_env.create env in
          match Head_meet_or_join.meet_or_join env head1 head2 with
          | Ok (head, env_extension) ->
            assert (TEE.is_empty env_extension);
            Ok head
          | Bottom -> Bottom
          | Absorbing -> Unknown

      let meet env t1 t2 =
        let canonical_simple1, head1, canonical_simple2, head2 =
          let typing_env = Meet_env.env env in
          get_canonical_simples_and_expand_heads typing_env or_alias1 or_alias2
        in
        match canonical_simple1, canonical_simple2 with
        | Bottom, _ | _, Bottom -> bottom, TEE.empty ()
        | Ok None, Ok None ->
          let head, env_extension =
            meet_head_or_unknown_or_bottom env head1 head2
          in
          create head, env_extension
        | Ok (Some simple1), Ok (Some simple2)
            when Simple.equal simple1 simple2
                   || Meet_env.already_meeting env simple1 simple2 ->
          create_equals simple1, TEE.empty ()
        | Ok (Some simple1), Ok (Some simple2) ->
          (* XXX Think about how to handle this properly. *)
          begin match Simple.descr simple1, Simple.descr simple2 with
          | Const const1, Const const2
              when not (Simple.Const.equal const1 const2) ->
            bottom, TEE.empty ()
          | Discriminant discriminant1, Discriminant discriminant2
              when not (Discriminant.equal discriminant1 discriminant2) ->
            bottom, TEE.empty ()
          | _, _ ->
            let head, env_extension =
              let env = Meet_env.now_meeting env simple1 simple2 in
              meet_ env head1 head2
            in
            let env_extension =
              if TE.defined_earlier (Meet_env.env env) simple1 ~than:simple2
              then
                env_extension
                |> add_equation env simple1 (S.to_type (create_no_alias head))
                |> add_equation env simple2 (S.to_type (create_equals simple1))
              else
                env_extension
                |> add_equation env simple2 (S.to_type (create_no_alias head))
                |> add_equation env simple1 (S.to_type (create_equals simple2))
            in
            (* It doesn't matter whether [simple1] or [simple2] is returned. *)
            create_equals simple1, env_extension
          end
        | Ok (Some simple), Ok None | Ok None, Ok (Some simple) ->
          let head, env_extension =
            meet_head_or_unknown_or_bottom env head1 head2
          in
          let env_extension =
            env_extension
            |> add_equation env simple (S.to_type (create_no_alias head))
          in
          (* XXX Not sure we want to return [Equals] when it's Bottom *)
          create_equals simple, env_extension

      let join ?bound_name typing_env t1 t2 =
        let canonical_simple1, head1, canonical_simple2, head2 =
          get_canonical_simples_and_expand_heads typing_env or_alias1 or_alias2
        in
        match canonical_simple1, canonical_simple2 with
        | Bottom, _ -> t2
        | _, Bottom -> t1
        | Ok canonical_simple1, Ok canonical_simple2 ->
          (* CR mshinwell: Think further about this "bound name" stuff. *)
          let shared_aliases_not_aliasing_bound_name =
            Simple.Set.diff
              (Simple.Set.inter (all_aliases_of typing_env canonical_simple1)
                (all_aliases_of typing_env canonical_simple2))
              (all_aliases_of typing_env (Option.map Simple.name bound_name))
          in
          match
            Simple.Set.choose_opt shared_aliases_not_aliasing_bound_name
          with
          | Some simple -> create_equals simple
          | None -> create (join_head_or_unknown_or_bottom env head1 head2)

      let meet_or_join ?bound_name env t1 t2 : _ Or_bottom.t =
        let t, env_extension =
          E.switch_no_bottom meet (join ?bound_name) env t1 t2
        in
        if is_obviously_bottom t then Bottom
        else Ok (t, env_extension)
    end
  end
end
