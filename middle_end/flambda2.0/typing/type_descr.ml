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

module Make (Head : sig
  include Contains_names.S

  module Make_meet_or_join (E : Lattice_ops_intf.S
    with type meet_env = Meet_env.t
    with type typing_env_extension = Typing_env_extension.t)
  : sig
    val meet_or_join
       : Meet_env.t
      -> t
      -> t
      -> (t * Typing_env_extension.t) Or_bottom_or_absorbing.t
  end

  val force_to_kind : Type_grammar.t -> t
  val erase_aliases : t -> Typing_env.t -> allowed:Variable.Set.t -> t
  val apply_rec_info : t -> Rec_info.t -> t Or_bottom.t
end) = struct
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
  end

  module T : sig
    (* This signature ensures that we don't accidentally fail to apply the
       delayed permutation and/or allowed variables set (or break their
       invariants). *)
    (* CR mshinwell: Do the same in [Expr]. *)

    type t

    val create_no_alias : Head.t Or_unknown_or_bottom.t -> t
    val create_equals : Simple.t -> t
    val create_type : Export_id.t -> t

    val descr : t -> Descr.t

    include Contains_names.S with type t := t
  end = struct
    type allowed = {
      env : Typing_env.t;
      allowed : Variable.Set.t;
    }

    type t = {
      descr : Descr.t;
      delayed_permutation : Name_permutation.t;
      (* To remove allowed_vars:

         - Add free_names here so it's quick to calculate
         - Add a function that takes a type and the allowed variables.
           It should return:
           (a) a new type
           (b) a list of irrelevant variables to be bound to Unknown
           such that the free varibles in the returned type do not exceed
           the allowed set.  The permutations to the irrelevant vars will be
           applied by this function.
      *)
      delayed_allowed_vars : allowed list;
      (* [delayed_allowed_vars] entries are always applied after the
         [delayed_permutation].  The entry at the start of the list is
         applied last. *)
    }

    let create descr =
      { descr;
        delayed_permutation = Name_permutation.empty;
        delayed_allowed_vars = [];
      }

    let create_no_alias head = create (No_alias head)
    let create_equals simple = create (Equals simple)
    let create_type export_id = create (Type export_id)

    let descr t =
      let descr =
        let perm = t.delayed_permutation in
        if Name_permutation.is_empty perm then t.descr
        else
          match t.descr with
          | No_alias head ->
            let head' =
              Or_unknown_or_bottom.map_sharing head
                ~f:(fun head -> Head.apply_name_permutation head perm)
            in
            if head == head' then t.descr
            else No_alias head'
          | Equals simple ->
            let simple' = Simple.apply_name_permutation simple perm in
            if simple == simple' then t.descr
            else Equals simple'
          | Type _ -> t.descr
      in
      match t.delayed_allowed_vars with
      | [] -> descr
      | allowed ->
        List.fold_left (fun { env; allowed; } descr ->
            match descr with
            | No_alias head ->
              let head' =
                Or_unknown_or_bottom.map_sharing head
                  ~f:(fun head -> Head.erase_aliases head env ~allowed)
              in
              if head == head' then descr
              else No_alias head'
            | Type _ -> descr
            | Equals simple ->
              let canonical_simple =
                Typing_env.get_canonical_simple env
                  ~min_occurrence_kind:Name_occurrence_kind.in_types
                  simple
              in
              match canonical_simple with
              | Bottom -> No_alias Bottom
              | Ok None -> (* CR mshinwell: Can this happen? *)
                Misc.fatal_errorf "No canonical simple for %a"
                  Simple.print simple
              | Ok (Some simple) ->
                if Simple.allowed simple ~allowed then Equals simple
                else
                  let head =
                    Typing_env.expand_head_from_descr env
                      ~force_to_kind:Head.force_to_kind
                      ~print:Head.print
                      ~apply_rec_info:Head.apply_rec_info
                      descr
                  in
                  No_alias (Head.erase_aliases head env ~allowed))
          descr
          (List.rev allowed)

    let apply_name_permutation t perm =
      let delayed_permutation =
        Name_permutation.compose ~second:perm ~first:t.delayed_permutation
      in
      let delayed_allowed_vars =
        List.map (fun { env; allowed; } ->
            let allowed = Name_permutation.apply_variable_set perm allowed in
            { env; allowed; })
          t.delayed_allowed_vars
      in
      { t with
        delayed_permutation;
        delayed_allowed_vars;
      }

    let erase_aliases t env ~allowed =
      let delayed_allowed_vars = { env; allowed; } :: t.delayed_allowed_vars in
      { t with delayed_allowed_vars; }
  end

  include T

  let print ppf t = Descr.print ppf (descr t)

  let create head = create_no_alias (Ok head)

  let free_names t =
    match descr t with
    | No_alias Bottom | No_alias Unknown -> Name_occurrences.empty
    | No_alias ok -> Head.free_names ok
    | Equals simple -> Simple.free_names simple
    | Type _ -> Name_occurrences.empty

 

 
  let erase_aliases_ty env ~bound_name ~already_seen
        ~allowed erase_aliases_of_kind_foo
        ~force_to_kind ~print_ty ~apply_rec_info
        (ty : _ Type_grammar.ty) : _ Type_grammar.ty =
    match ty with
    | No_alias unknown_or_join ->
      let unknown_or_join' =
        erase_aliases_unknown_or_join erase_aliases_of_kind_foo env
          ~bound_name ~already_seen ~allowed unknown_or_join
      in
      if unknown_or_join == unknown_or_join' then ty
      else No_alias unknown_or_join'
    | Type _export_id -> ty
    | Equals simple ->
  
  let apply_rec_info_ty (type of_kind_foo)
        (apply_rec_info_of_kind_foo :
          (of_kind_foo -> Rec_info.t -> of_kind_foo Or_bottom.t))
        (ty : of_kind_foo Type_grammar.ty)
        rec_info : of_kind_foo Type_grammar.ty Or_bottom.t =
    match ty with
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

  let meet_unknown meet_contents env
      (or_unknown1 : _ Or_unknown.t) (or_unknown2 : _ Or_unknown.t)
      : ((_ Or_unknown.t) * TEE.t) Or_bottom.t =
    match or_unknown1, or_unknown2 with
    | Unknown, Unknown -> Ok (Unknown, TEE.empty ())
    | _, Unknown -> Ok (or_unknown1, TEE.empty ())
    | Unknown, _ -> Ok (or_unknown2, TEE.empty ())
    | Known contents1, Known contents2 ->
      Or_bottom.map (meet_contents env contents1 contents2)
        ~f:(fun (contents, env_extension) ->
          Or_unknown.Known contents, env_extension)

  let join_unknown join_contents env
      (or_unknown1 : _ Or_unknown.t) (or_unknown2 : _ Or_unknown.t)
      : _ Or_unknown.t =
    match or_unknown1, or_unknown2 with
    | Unknown, Unknown
    | _, Unknown
    | Unknown, _ -> Unknown
    | Known contents1, Known contents2 ->
      Known (join_contents env contents1 contents2)

  let add_equation _env (simple : Simple.t) ty env_extension =
    match Simple.descr simple with
    (* CR mshinwell: Does this need to use some kind of [meet_equation]? *)
    | Name name -> TEE.add_or_replace_equation env_extension name ty
    | Const _ | Discriminant _ -> env_extension

  let all_aliases_of env simple_opt =
    match simple_opt with
    | None -> Simple.Set.empty
    | Some simple ->
      Simple.Set.add simple
        (Typing_env.aliases_of_simple_allowable_in_types env simple)

  let get_canonical_simples_and_expand_heads typing_env or_alias1 or_alias2 =
    let canonical_simple1 =
      Typing_env.get_alias_ty_then_canonical_simple typing_env or_alias1
    in
    let unknown_or_join1 =
      Typing_env.expand_head_ty typing_env
        ~force_to_kind:S.force_to_kind ~print_ty
        ~apply_rec_info:S.apply_rec_info
        or_alias1
    in
    let canonical_simple2 =
      Typing_env.get_alias_ty_then_canonical_simple typing_env or_alias2
    in
    let unknown_or_join2 =
      Typing_env.expand_head_ty typing_env
        ~force_to_kind:S.force_to_kind
        ~apply_rec_info:S.apply_rec_info
        ~print_ty or_alias2
    in
    canonical_simple1, unknown_or_join1, canonical_simple2, unknown_or_join2

  let meet_on_unknown_or_join env ~meet_or_join_ty
        (ou1 : S.of_kind_foo T.unknown_or_join)
        (ou2 : S.of_kind_foo T.unknown_or_join)
        : S.of_kind_foo T.unknown_or_join * TEE.t =
    match ou1, ou2 with
    | Unknown, ou2 -> ou2, TEE.empty ()
    | ou1, Unknown -> ou1, TEE.empty ()
    | Bottom, _ | _, Bottom -> Bottom, TEE.empty ()
    | Ok of_kind_foo1, Ok of_kind_foo2 ->
      match
        S.meet_or_join_of_kind_foo env ~meet_or_join_ty
          of_kind_foo1 of_kind_foo2
      with
      | Ok (of_kind_foo, env_extension) -> Ok of_kind_foo, env_extension
      | Absorbing | Bottom -> Bottom, TEE.empty ()

  let join_on_unknown_or_join env ~meet_or_join_ty
        (uj1 : S.of_kind_foo T.unknown_or_join)
        (uj2 : S.of_kind_foo T.unknown_or_join)
        : S.of_kind_foo T.unknown_or_join =
    match uj1, uj2 with
    | Bottom, _ -> uj2
    | _, Bottom -> uj1
    | Unknown, _ | _, Unknown -> Unknown
    | Ok of_kind_foo1, Ok of_kind_foo2 ->
      let env = Meet_env.create env in
      match
        S.meet_or_join_of_kind_foo env ~meet_or_join_ty
          of_kind_foo1 of_kind_foo2
      with
      | Ok (of_kind_foo, _env_extension) -> Ok of_kind_foo
      | Bottom -> Bottom
      | Absorbing -> Unknown

  let rec meet_ty env
        (or_alias1 : S.of_kind_foo T.ty) (or_alias2 : S.of_kind_foo T.ty)
        : S.of_kind_foo T.ty * TEE.t =
    let canonical_simple1, unknown_or_join1,
        canonical_simple2, unknown_or_join2 =
      let typing_env = Meet_env.env env in
      get_canonical_simples_and_expand_heads typing_env or_alias1 or_alias2
    in
    match canonical_simple1, canonical_simple2 with
    | Bottom, _ | _, Bottom -> No_alias Bottom, TEE.empty ()
    | Ok None, Ok None ->
      let unknown_or_join, env_extension =
        meet_on_unknown_or_join env ~meet_or_join_ty
          unknown_or_join1 unknown_or_join2
      in
      No_alias unknown_or_join, env_extension
    | Ok (Some simple1), Ok (Some simple2)
        when Simple.equal simple1 simple2
               || Meet_env.already_meeting env simple1 simple2 ->
      Equals simple1, TEE.empty ()
    | Ok (Some simple1), Ok (Some simple2) ->
      (* XXX Think about how to handle this properly. *)
      begin match Simple.descr simple1, Simple.descr simple2 with
      | Const const1, Const const2
          when not (Simple.Const.equal const1 const2) ->
        No_alias Bottom, TEE.empty ()
      | Discriminant discriminant1, Discriminant discriminant2
          when not (Discriminant.equal discriminant1 discriminant2) ->
        No_alias Bottom, TEE.empty ()
      | _, _ ->
(*
Format.eprintf "Meeting simples: %a and %a\n%!"
  Simple.print simple1
  Simple.print simple2;
*)
        let unknown_or_join, env_extension =
          let env = Meet_env.now_meeting env simple1 simple2 in
          meet_on_unknown_or_join env ~meet_or_join_ty
            unknown_or_join1 unknown_or_join2
        in
(*
Format.eprintf "TEE from meeting simples (1): %a\n%!"
  Typing_env_extension.print env_extension;
*)
        let env_extension =
          if Typing_env.defined_earlier (Meet_env.env env) simple1 ~than:simple2
          then
            env_extension
            |> add_equation env simple1 (S.to_type (No_alias unknown_or_join))
            |> add_equation env simple2 (S.to_type (Equals simple1))
          else
            env_extension
            |> add_equation env simple2 (S.to_type (No_alias unknown_or_join))
            |> add_equation env simple1 (S.to_type (Equals simple2))
        in
(*
Format.eprintf "TEE from meeting simples (2): %a\n%!"
  Typing_env_extension.print env_extension;
*)
      (* It doesn't matter whether [simple1] or [simple2] is returned here. *)
(*
Format.eprintf "Returning =%a\n%!" Simple.print simple1;
*)
        Equals simple1, env_extension
      end
    | Ok (Some simple), Ok None | Ok None, Ok (Some simple) ->
      let unknown_or_join, env_extension =
        meet_on_unknown_or_join env ~meet_or_join_ty
          unknown_or_join1 unknown_or_join2
      in
      let env_extension =
        env_extension
        |> add_equation env simple (S.to_type (No_alias unknown_or_join))
      in
      (* XXX Not sure we want to return [Equals] when it's Bottom *)
      Equals simple, env_extension

  and join_ty ?bound_name typing_env
        (or_alias1 : S.of_kind_foo T.ty) (or_alias2 : S.of_kind_foo T.ty)
        : S.of_kind_foo T.ty =
    let canonical_simple1, unknown_or_join1,
        canonical_simple2, unknown_or_join2 =
      get_canonical_simples_and_expand_heads typing_env or_alias1 or_alias2
    in
    match canonical_simple1, canonical_simple2 with
    | Bottom, _ -> or_alias2
    | _, Bottom -> or_alias1
    | Ok canonical_simple1, Ok canonical_simple2 ->
      (* CR mshinwell: Think further about this "bound name" stuff. *)
      let shared_aliases_not_aliasing_bound_name =
        Simple.Set.diff
          (Simple.Set.inter (all_aliases_of typing_env canonical_simple1)
            (all_aliases_of typing_env canonical_simple2))
          (all_aliases_of typing_env (Option.map Simple.name bound_name))
      in
      match Simple.Set.choose_opt shared_aliases_not_aliasing_bound_name with
      | Some simple -> Equals simple
      | None ->
        No_alias (join_on_unknown_or_join typing_env ~meet_or_join_ty
          unknown_or_join1 unknown_or_join2)

  and meet_or_join_ty ?bound_name env
        (or_alias1 : S.of_kind_foo T.ty)
        (or_alias2 : S.of_kind_foo T.ty) : _ Or_bottom.t =
    let ty, env_extension =
      E.switch_no_bottom meet_ty (join_ty ?bound_name) env or_alias1 or_alias2
    in
    if Basic_type_ops.ty_is_obviously_bottom ty then Bottom
    else Ok (ty, env_extension)

  let is_obviously_bottom t =
    match descr t with
    | No_alias Bottom -> true
    | No_alias (Ok _ | Unknown _)
    | Equals _ | Type _ -> false

  let get_alias t =
    match descr t with
    | Equals alias -> Some alias
    | No_alias _ | Type _ -> None
end
