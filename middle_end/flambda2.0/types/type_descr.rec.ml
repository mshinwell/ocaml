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
module TEL = Typing_env_level

module Make (Head : Type_head_intf.S
  with type meet_env := Meet_env.t
  with type typing_env := Typing_env.t
  with type typing_env_extension := Typing_env_extension.t
  with type type_grammar := Type_grammar.t)
= struct
  module Descr = struct
    type t =
      | No_alias of Head.t Or_unknown_or_bottom.t
      | Equals of Simple.t
      | Type of Export_id.t

    let print_with_cache ~cache ppf t =
      let colour = Flambda_colours.top_or_bottom_type () in
      match t with
      | No_alias Unknown ->
        if !Clflags.flambda2_unicode then
          Format.fprintf ppf "@<0>%s@<1>\u{22a4}@<0>%s"
            colour (Flambda_colours.normal ())
        else
          Format.fprintf ppf "@<0>%sT@<0>%s" colour (Flambda_colours.normal ())
      | No_alias Bottom ->
        if !Clflags.flambda2_unicode then
          Format.fprintf ppf "@<0>%s@<1>\u{22a5}@<0>%s"
            colour (Flambda_colours.normal ())
        else
          Format.fprintf ppf "@<0>%s_|_@<0>%s" colour (Flambda_colours.normal ())
      | No_alias (Ok head) -> Head.print_with_cache ~cache ppf head
      | Equals simple ->
        Format.fprintf ppf "@[(@<0>%s=@<0>%s %a)@]"
          (Flambda_colours.error ())
          (Flambda_colours.normal ())
          Simple.print simple
      | Type export_id ->
        Format.fprintf ppf "@[(@<0>%s=export_id@<0>%s %a)@]"
          (Flambda_colours.error ())
          (Flambda_colours.normal ())
          Export_id.print export_id

    let print ppf t =
      print_with_cache ~cache:(Printing_cache.create ()) ppf t

    let apply_name_permutation t perm =
      if Name_permutation.is_empty perm then t
      else
        match t with
        | No_alias Bottom | No_alias Unknown -> t
        | No_alias (Ok head) ->
          let head' = Head.apply_name_permutation head perm in
          if head == head' then t
          else No_alias (Ok head')
        | Equals simple ->
          let simple' = Simple.apply_name_permutation simple perm in
          if simple == simple' then t
          else Equals simple'
        | Type _ -> t

    let free_names t =
      match t with
      | No_alias Bottom | No_alias Unknown -> Name_occurrences.empty
      | No_alias (Ok head) -> Head.free_names head
      | Equals simple ->
        Name_occurrences.downgrade_occurrences_at_strictly_greater_kind
          (Simple.free_names simple)
          Name_mode.in_types
      | Type _ -> Name_occurrences.empty
  end

  include With_delayed_permutation.Make (Descr)

  let print_with_cache ~cache ppf t =
    Descr.print_with_cache ~cache ppf (descr t)

  let print ppf t =
    print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let create_no_alias head = create (No_alias head)
  let create_equals simple = create (Equals simple)
  let create_type export_id = create (Type export_id)

  let bottom () = create (No_alias Bottom)
  let unknown () = create (No_alias Unknown)

  let create head = create_no_alias (Ok head)

  let is_obviously_bottom t =
    match descr t with
    | No_alias Bottom -> true
    | No_alias (Ok _ | Unknown)
    | Equals _ | Type _ -> false

  let is_obviously_unknown t =
    match descr t with
    | No_alias Unknown -> true
    | No_alias (Ok _ | Bottom)
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
      | Some simple -> Ok (create_equals simple)
      end
    | Type _ -> Misc.fatal_error "Not yet implemented"
    | No_alias Unknown -> Ok t
    | No_alias Bottom -> Bottom
    | No_alias (Ok head) ->
      Or_bottom.map (Head.apply_rec_info head rec_info)
        ~f:(fun head -> create head)

  let make_suitable_for_environment0 t env ~suitable_for level =
    let free_vars = Name_occurrences.variables (free_names t) in
    if Variable.Set.is_empty free_vars then level, t
    else
      let allowed = TE.var_domain suitable_for in
      let to_erase = Variable.Set.diff free_vars allowed in
      if Variable.Set.is_empty to_erase then level, t
      else
        let result_level, perm, _binding_time =
          (* To avoid writing an erasure operation, we define irrelevant fresh
             variables in the returned [Typing_env_level], and swap them with
             the variables that we wish to erase throughout the type. *)
          Variable.Set.fold (fun to_erase (result_level, perm, binding_time) ->
              let kind = T.kind (TE.find env (Name.var to_erase)) in
              let fresh_var = Variable.rename to_erase in
              let fresh_var_name = Name.var fresh_var in
              let result_level =
                TEL.add_definition result_level fresh_var kind binding_time
              in
              let canonical_simple =
                TE.get_canonical_simple env
                  ~min_name_mode:Name_mode.in_types
                  (Simple.var to_erase)
              in
              let result_level =
                let ty =
                  match canonical_simple with
                  | Bottom -> T.bottom kind
                  | Ok None -> T.unknown kind
                  | Ok (Some canonical_simple) ->
                    (* CR-someday mshinwell: If we can't find a [Simple] in
                       [suitable_for] then we could expand the head of [ty],
                       with a view to returning something better than
                       [Unknown]. *)
                    if TE.mem_simple suitable_for canonical_simple then
                      T.alias_type_of kind canonical_simple
                    else
                      T.unknown kind
                in
                TEL.add_or_replace_equation result_level fresh_var_name ty
              in
              let perm =
                Name_permutation.add_variable perm to_erase fresh_var
              in
              result_level, perm, Binding_time.succ binding_time)
            to_erase
            (level, Name_permutation.empty, Binding_time.earliest_var)
        in
        result_level, apply_name_permutation t perm

  let make_suitable_for_environment t env ~suitable_for ~bind_to ~to_type =
    let bind_to = Name.var bind_to in
    if TE.mem env bind_to then begin
      Misc.fatal_errorf "[bind_to] %a must not be bound in the \
          source environment:@ %a"
        Name.print bind_to
        TE.print env
    end;
    if not (TE.mem suitable_for bind_to) then begin
      Misc.fatal_errorf "[bind_to] %a is expected to be bound in the \
          [suitable_for] environment:@ %a"
        Name.print bind_to
        TE.print suitable_for
    end;
    let level, ty =
      make_suitable_for_environment0 t env ~suitable_for (TEL.empty ())
    in
    let level = TEL.add_or_replace_equation level bind_to (to_type ty) in
    TEE.create level

  let force_to_head ~force_to_kind t =
    match descr (force_to_kind t) with
    | No_alias head -> head
    | Type _ | Equals _ ->
      Misc.fatal_errorf "Expected [No_alias]:@ %a" T.print t

  let expand_head ~force_to_kind t env : _ Or_unknown_or_bottom.t =
    match descr t with
    | No_alias head -> head
    | Type _export_id -> Misc.fatal_error ".cmx loading not yet implemented"
    | Equals simple ->
      let min_name_mode = Name_mode.min_in_types in
      (* We must get the canonical simple with the least occurrence kind,
         since that's the one that is guaranteed not to have an [Equals]
         type. *)
      match TE.get_canonical_simple env simple ~min_name_mode with
      | Bottom -> Bottom
      | Ok None ->
        Misc.fatal_errorf "There should always be a canonical simple for %a \
            in environment:@ %a"
          Simple.print simple
          TE.print env
      | Ok (Some simple) ->
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
          force_to_head ~force_to_kind typ
        | Name name ->
          let t = force_to_kind (TE.find env name) in
          match descr t with
          | No_alias Bottom -> Bottom
          | No_alias Unknown -> Unknown
          | No_alias (Ok head) -> Ok head
            (* CR mshinwell: Fix Rec_info
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
            *)
          | Type _export_id ->
            Misc.fatal_error ".cmx loading not yet implemented"
          | Equals _ ->
            Misc.fatal_errorf "Canonical alias %a should never have \
                [Equals] type:@ %a"
              Simple.print simple
              TE.print env

  let add_equation _env (simple : Simple.t) ty env_extension =
    match Simple.descr simple with
    (* CR mshinwell: Does this need to use some kind of [meet_equation]? *)
    | Name name -> TEE.add_or_replace_equation env_extension name ty
    | Const _ -> env_extension

  let all_aliases_of env simple_opt =
    match simple_opt with
    | None -> Simple.Set.empty
    | Some simple ->
      Simple.Set.add simple (
        TE.aliases_of_simple_allowable_in_types env simple)

  let get_canonical_simples_and_expand_heads ~force_to_kind ~to_type
      typing_env t1 t2 =
    let extract_descr simple : (Simple.t * Simple.descr) option Or_bottom.t =
      Or_bottom.map simple ~f:(fun simple ->
        Option.map (fun simple -> simple, Simple.descr simple) simple)
    in
    let canonical_simple1 =
      TE.get_alias_then_canonical_simple typing_env (to_type t1)
      |> extract_descr
    in
    let head1 = expand_head ~force_to_kind t1 typing_env in
    let canonical_simple2 =
      TE.get_alias_then_canonical_simple typing_env (to_type t2)
      |> extract_descr
    in
    let head2 = expand_head ~force_to_kind t2 typing_env in
    canonical_simple1, head1, canonical_simple2, head2

  module Make_meet_or_join
    (E : Lattice_ops_intf.S
     with type meet_env := Meet_env.t
     with type typing_env := TE.t
     with type typing_env_extension := TEE.t) =
  struct
    module Head_meet_or_join = Head.Make_meet_or_join (E)

    type meet_or_join_head_or_unknown_or_bottom_result =
      | Left_head_unchanged
      | Right_head_unchanged
      | New_head of Head.t Or_unknown_or_bottom.t * TEE.t

    let meet_head_or_unknown_or_bottom env
          (head1 : _ Or_unknown_or_bottom.t)
          (head2 : _ Or_unknown_or_bottom.t)
          : meet_or_join_head_or_unknown_or_bottom_result =
      match head1, head2 with
      | _, Unknown -> Left_head_unchanged
      | Unknown, _ -> Right_head_unchanged
      | Bottom, _ -> Left_head_unchanged
      | _, Bottom -> Right_head_unchanged
      | Ok head1, Ok head2 ->
        match Head_meet_or_join.meet_or_join env head1 head2 with
        | Ok (head, env_extension) -> New_head (Ok head, env_extension)
        | Absorbing | Bottom -> New_head (Bottom, TEE.empty ())

    let join_head_or_unknown_or_bottom env
          (head1 : _ Or_unknown_or_bottom.t)
          (head2 : _ Or_unknown_or_bottom.t)
          : _ Or_unknown_or_bottom.t =
      match head1, head2 with
      | _, Bottom -> head1
      | Bottom, _ -> head2
      | Unknown, _ -> Unknown
      | _, Unknown -> Unknown
      | Ok head1, Ok head2 ->
        let env = Meet_env.create env in
        match Head_meet_or_join.meet_or_join env head1 head2 with
        | Ok (head, env_extension) ->
          assert (TEE.is_empty env_extension);
          Ok head
        | Bottom -> Bottom
        | Absorbing -> Unknown

    let meet ~force_to_kind ~to_type env t1 t2 =
      let canonical_simple1, head1, canonical_simple2, head2 =
        let typing_env = Meet_env.env env in
        get_canonical_simples_and_expand_heads ~force_to_kind ~to_type
          typing_env t1 t2
      in
      match canonical_simple1, canonical_simple2 with
      | Bottom, _ | _, Bottom -> bottom (), TEE.empty ()
      | Ok (Some (simple1, _)), Ok (Some (simple2, _))
          when Simple.equal simple1 simple2
            || Meet_env.already_meeting env simple1 simple2 ->
        (* This produces "=simple" for the output rather than a type that
           might need transformation back from an expanded head (as would
           happen if we used the next case). *)
        create_equals simple1, TEE.empty ()
      | Ok None, Ok None
      | Ok None, Ok (Some (_, Const _))
      | Ok (Some (_, Const _)), Ok None
      | Ok (Some (_, Const _)), Ok (Some (_, Const _)) ->
        (* [Simple]s that are constants must be treated differently during
           [meet] from those that are e.g. variables. The reason is that the
           latter encode constraints, which should not be forgotten, whereas the
           former are treated just like they were part of the "head". *)
        begin match meet_head_or_unknown_or_bottom env head1 head2 with
        | Left_head_unchanged -> t1, TEE.empty ()
        | Right_head_unchanged -> t2, TEE.empty ()
        | New_head (head, env_extension) ->
          match head with
          | Bottom -> bottom (), env_extension
          | Unknown -> unknown (), env_extension
          | Ok head -> create head, env_extension
        end
      | Ok (Some (simple1, Name _)), Ok (Some (simple2, Name _)) ->
        let env = Meet_env.now_meeting env simple1 simple2 in
        (* In the following cases we may generate equations "pointing the
           wrong way", for example "y : =x" when [y] is the canonical element.
           This doesn't matter, however, because [Typing_env] sorts this out
           when adding equations into an environment. *)
        begin match meet_head_or_unknown_or_bottom env head1 head2 with
        | Left_head_unchanged ->
          let env_extension =
            TEE.empty ()
            |> add_equation env simple2 (to_type (create_equals simple1))
          in
          create_equals simple1, env_extension
        | Right_head_unchanged ->
          let env_extension =
            TEE.empty ()
            |> add_equation env simple1 (to_type (create_equals simple2))
          in
          create_equals simple2, env_extension
        | New_head (head, env_extension) ->
          let env_extension =
            env_extension
            |> add_equation env simple1 (to_type (create_no_alias head))
            |> add_equation env simple2 (to_type (create_equals simple1))
          in
          (* It makes things easier (to check if the result of [meet] was
             bottom) to not return "=simple" in the bottom case.  This is ok
             because no constraint is being dropped; the type cannot be
             refined any further. *)
          match head with
          | Bottom -> bottom (), env_extension
          | Unknown | Ok _ -> create_equals simple1, env_extension
        end
      | Ok (Some (simple, Name _)), Ok (None | Some (_, Const _)) ->
        begin match meet_head_or_unknown_or_bottom env head1 head2 with
        | Left_head_unchanged -> create_equals simple, TEE.empty ()
        | Right_head_unchanged ->
          let env_extension =
            TEE.empty ()
            |> add_equation env simple (to_type (create_no_alias head2))
          in
          create_equals simple, env_extension
        | New_head (head, env_extension) ->
          let env_extension =
            env_extension
            |> add_equation env simple (to_type (create_no_alias head))
          in
          match head with
          | Bottom -> bottom (), env_extension
          | Unknown | Ok _ -> create_equals simple, env_extension
        end
      | Ok (None | Some (_, Const _)), Ok (Some (simple, Name _)) ->
        begin match meet_head_or_unknown_or_bottom env head1 head2 with
        | Left_head_unchanged ->
          let env_extension =
            TEE.empty ()
            |> add_equation env simple (to_type (create_no_alias head1))
          in
          create_equals simple, env_extension
        | Right_head_unchanged -> create_equals simple, TEE.empty ()
        | New_head (head, env_extension) ->
          let env_extension =
            env_extension
            |> add_equation env simple (to_type (create_no_alias head))
          in
          match head with
          | Bottom -> bottom (), env_extension
          | Unknown | Ok _ -> create_equals simple, env_extension
        end

    let join ~force_to_kind ~to_type typing_env t1 t2 =
      let canonical_simple1, head1, canonical_simple2, head2 =
        get_canonical_simples_and_expand_heads ~force_to_kind ~to_type
          typing_env t1 t2
      in
      match canonical_simple1, canonical_simple2 with
      | Bottom, _ -> t2
      | _, Bottom -> t1
      | Ok canonical_simple1, Ok canonical_simple2 ->
        let canonical_simple1 =
          Option.map (fun (simple, _) -> simple) canonical_simple1
        in
        let canonical_simple2 =
          Option.map (fun (simple, _) -> simple) canonical_simple2
        in
        let shared_aliases =
          Simple.Set.inter (all_aliases_of typing_env canonical_simple1)
            (all_aliases_of typing_env canonical_simple2)
        in
        match Simple.Set.choose_opt shared_aliases with
        | Some simple -> create_equals simple
        | None ->
          match join_head_or_unknown_or_bottom typing_env head1 head2 with
          | Bottom -> bottom ()
          | Unknown -> unknown ()
          | Ok head -> create head

    let meet_or_join ~force_to_kind ~to_type env t1 t2 : _ Or_bottom.t =
      let t, env_extension =
        E.switch_no_bottom (meet ~force_to_kind ~to_type)
          (join ~force_to_kind ~to_type) env t1 t2
      in
      if is_obviously_bottom t then Bottom
      else Ok (t, env_extension)
  end
end
