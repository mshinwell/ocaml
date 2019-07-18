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

(* CR mshinwell: Consider packaging up [env], [bound_name], [already_seen]
   and [allowed] into a single environment. *)

let erase_aliases_unknown_or_join erase_aliases_contents env ~bound_name
      ~already_seen ~allowed (o : _ Flambda_types.unknown_or_join)
      : _ Flambda_types.unknown_or_join =
  match o with
  | Unknown | Bottom -> o
  | Ok contents ->
    Ok (erase_aliases_contents env ~bound_name ~already_seen
      ~allowed contents)

let erase_aliases_ty env ~bound_name ~already_seen
      ~allowed erase_aliases_of_kind_foo
      ~force_to_kind ~print_ty ~apply_rec_info
      (ty : _ Flambda_types.ty) : _ Flambda_types.ty =
  match ty with
  | No_alias unknown_or_join ->
    No_alias (erase_aliases_unknown_or_join erase_aliases_of_kind_foo env
      ~bound_name ~already_seen ~allowed unknown_or_join)
  | Type _export_id -> ty
  | Equals simple ->
    (* First of all, make sure we're not going around in a loop, which can
       happen for closure or set-of-closures recursive types.
       Then try to find an alias that's in the [allowed] set and is eligible
       to appear in types.
       If that fails, expand the head of the type, and then recursively erase
       aliases on the result (returning a non-alias type). *)
    let canonical_simple =
      Typing_env.get_canonical_simple env
        ~min_occurrence_kind:Name_occurrence_kind.in_types
        simple
    in
    match canonical_simple with
    | Bottom -> No_alias Bottom
    | Ok None -> (* CR mshinwell: Can this happen? *)
      Misc.fatal_errorf "No canonical simple for %a" Simple.print simple
    | Ok (Some simple) ->
      if Simple.allowed simple ~allowed then Equals simple
      else
        let unknown_or_join =
          Typing_env.expand_head_ty env ~force_to_kind ~print_ty ~apply_rec_info
            ty
        in
        let already_seen = Simple.Set.add simple already_seen in
        No_alias (erase_aliases_unknown_or_join erase_aliases_of_kind_foo env
          ~bound_name ~already_seen ~allowed unknown_or_join)

let erase_aliases_of_kind_naked_number (type n) _env ~bound_name:_
      ~already_seen:_ ~allowed:_
      (ty : n Flambda_types.of_kind_naked_number) = ty

let rec erase_aliases env ~bound_name ~already_seen ~allowed
      (t : Flambda_types.t) : Flambda_types.t =
  match t with
  | Value ty ->
    Value (
      erase_aliases_ty env ~bound_name ~already_seen ~allowed
        ~force_to_kind:Flambda_type0_core.force_to_kind_value
        ~print_ty:Type_printers.print_ty_value
        ~apply_rec_info:Flambda_type0_core.apply_rec_info_of_kind_value
        erase_aliases_of_kind_value
        ty)
  | Naked_number (ty, kind) ->
    Naked_number (
      erase_aliases_ty env ~bound_name ~already_seen ~allowed
        ~force_to_kind:(Flambda_type0_core.force_to_kind_naked_number kind)
        ~print_ty:Type_printers.print_ty_naked_number
        ~apply_rec_info:Flambda_type0_core.apply_rec_info_of_kind_naked_number
        erase_aliases_of_kind_naked_number
        ty,
      kind)
  | Fabricated ty ->
    Fabricated (
      erase_aliases_ty env ~bound_name ~already_seen ~allowed
        ~force_to_kind:Flambda_type0_core.force_to_kind_fabricated
        ~print_ty:Type_printers.print_ty_fabricated
        ~apply_rec_info:Flambda_type0_core.apply_rec_info_of_kind_fabricated
        erase_aliases_of_kind_fabricated
        ty)

and erase_aliases_of_kind_value env ~bound_name ~already_seen ~allowed
      (of_kind : Flambda_types.of_kind_value)
      : Flambda_types.of_kind_value =
  match of_kind with
  | Blocks_and_tagged_immediates { blocks; immediates; } ->
    let blocks =
      Or_unknown.map blocks ~f:(fun blocks ->
        Blocks.erase_aliases blocks env ~already_seen ~allowed)
    in
    let immediates =
      Or_unknown.map immediates ~f:(fun immediates ->
        Immediates.erase_aliases immediates env ~already_seen ~allowed);
    in
    Blocks_and_tagged_immediates {
      blocks;
      immediates;
    }
  | Boxed_number (Boxed_float ty) ->
    Boxed_number (Boxed_float (
      erase_aliases_ty env ~bound_name ~already_seen ~allowed
        erase_aliases_of_kind_naked_number
        ~force_to_kind:Flambda_type0_core.force_to_kind_naked_float
        ~print_ty:Type_printers.print_ty_naked_float
        ~apply_rec_info:Flambda_type0_core.apply_rec_info_of_kind_naked_number
        ty))
  | Boxed_number (Boxed_int32 ty) ->
    Boxed_number (Boxed_int32 (
      erase_aliases_ty env ~bound_name ~already_seen ~allowed
        erase_aliases_of_kind_naked_number
        ~force_to_kind:Flambda_type0_core.force_to_kind_naked_int32
        ~print_ty:Type_printers.print_ty_naked_int32
        ~apply_rec_info:Flambda_type0_core.apply_rec_info_of_kind_naked_number
        ty))
  | Boxed_number (Boxed_int64 ty) ->
    Boxed_number (Boxed_int64 (
      erase_aliases_ty env ~bound_name ~already_seen ~allowed
        erase_aliases_of_kind_naked_number
        ~force_to_kind:Flambda_type0_core.force_to_kind_naked_int64
        ~print_ty:Type_printers.print_ty_naked_int64
        ~apply_rec_info:Flambda_type0_core.apply_rec_info_of_kind_naked_number
        ty))
  | Boxed_number (Boxed_nativeint ty) ->
    Boxed_number (Boxed_nativeint (
      erase_aliases_ty env ~bound_name ~already_seen ~allowed
        erase_aliases_of_kind_naked_number
        ~force_to_kind:Flambda_type0_core.force_to_kind_naked_nativeint
        ~print_ty:Type_printers.print_ty_naked_nativeint
        ~apply_rec_info:Flambda_type0_core.apply_rec_info_of_kind_naked_number
        ty))
  | Closures { by_closure_id; } ->
    Closures {
      by_closure_id =
        Closures_entry_by_closure_id.erase_aliases by_closure_id env
          ~already_seen ~allowed;
    }
  | String _ -> of_kind
  | Array { length; } ->
    let length =
      erase_aliases_ty_value env ~bound_name ~already_seen ~allowed length
    in
    Array { length; }

and erase_aliases_of_kind_fabricated env ~bound_name:_ ~already_seen ~allowed
      (of_kind : Flambda_types.of_kind_fabricated)
      : Flambda_types.of_kind_fabricated =
  match of_kind with
  | Discriminants discrs ->
    Discriminants (
      Discriminants.erase_aliases discrs env ~already_seen ~allowed)
  | Set_of_closures { closures; } ->
    Set_of_closures {
      closures = Closure_ids.erase_aliases closures env ~already_seen ~allowed;
    }

and erase_aliases_ty_value env ~bound_name ~already_seen ~allowed ty =
  erase_aliases_ty env ~bound_name ~already_seen ~allowed
    erase_aliases_of_kind_value
    ~force_to_kind:Flambda_type0_core.force_to_kind_value
    ~print_ty:Type_printers.print_ty_value
    ~apply_rec_info:Flambda_type0_core.apply_rec_info_of_kind_value
    ty

let erase_aliases_ty_fabricated env ~bound_name ~already_seen ~allowed ty =
  erase_aliases_ty env ~bound_name ~already_seen ~allowed
    erase_aliases_of_kind_fabricated
    ~force_to_kind:Flambda_type0_core.force_to_kind_fabricated
    ~print_ty:Type_printers.print_ty_fabricated
    ~apply_rec_info:Flambda_type0_core.apply_rec_info_of_kind_fabricated
    ty
