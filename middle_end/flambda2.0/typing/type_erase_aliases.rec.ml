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

let simple_is_eligible ~allowed simple =
  match Simple.descr simple with
  | Const _ | Discriminant _ | Name (Symbol _) -> true
  | Name (Var var) -> Variable.Set.mem var allowed

let erase_aliases_unknown_or_join erase_aliases_contents env ~allowed
      (o : _ Flambda_types.unknown_or_join) : _ Flambda_types.unknown_or_join =
  match o with
  | Unknown | Bottom -> o
  | Ok contents -> Ok (erase_aliases_contents env ~allowed contents)

let erase_aliases_ty env ~allowed erase_aliases_of_kind_foo
      ~force_to_kind ~print_ty (ty : _ Flambda_types.ty) : _ Flambda_types.ty =
  match ty with
  | No_alias unknown_or_join ->
    No_alias (erase_aliases_unknown_or_join erase_aliases_of_kind_foo env
      ~allowed unknown_or_join)
  | Type _export_id -> ty
  | Equals simple ->
    (* First of all try to find an alias that's in the [allowed] set and
       is eligible to appear in types.
       If that fails, expand the head of the type, and then recursively erase
       aliases on the result (returning a non-alias type). *)
    let all_aliases =
      Typing_env.aliases_of_simple_allowable_in_types env simple
    in
    let eligible_aliases =
      Simple.Set.filter (fun simple -> simple_is_eligible ~allowed simple)
        all_aliases
    in
    match Simple.Set.choose_opt eligible_aliases with
    | Some alias -> Equals alias
    | None ->
      let unknown_or_join, _ =
        Typing_env.resolve_ty env ~force_to_kind ~print_ty ty
      in
      No_alias (erase_aliases_unknown_or_join erase_aliases_of_kind_foo env
        ~allowed unknown_or_join)

let erase_aliases_of_kind_naked_number (type n) _env ~allowed:_
      (ty : n Flambda_types.of_kind_naked_number) = ty

let rec erase_aliases env ~allowed (t : Flambda_types.t) : Flambda_types.t =
  match t with
  | Value ty ->
    Value (
      erase_aliases_ty env ~allowed
        ~force_to_kind:Flambda_type0_core.force_to_kind_value
        ~print_ty:Type_printers.print_ty_value
        erase_aliases_of_kind_value
        ty)
  | Naked_number (ty, kind) ->
    Naked_number (
      erase_aliases_ty env ~allowed
        ~force_to_kind:(Flambda_type0_core.force_to_kind_naked_number kind)
        ~print_ty:Type_printers.print_ty_naked_number
        erase_aliases_of_kind_naked_number
        ty,
      kind)
  | Fabricated ty ->
    Fabricated (
      erase_aliases_ty env ~allowed
        ~force_to_kind:Flambda_type0_core.force_to_kind_fabricated
        ~print_ty:Type_printers.print_ty_fabricated
        erase_aliases_of_kind_fabricated
        ty)

and erase_aliases_of_kind_value env ~allowed
      (of_kind : Flambda_types.of_kind_value)
      : Flambda_types.of_kind_value =
  match of_kind with
  | Blocks_and_tagged_immediates { blocks; immediates; } ->
    let blocks =
      Or_unknown.map blocks ~f:(fun blocks ->
        Blocks.erase_aliases blocks env ~allowed)
    in
    let immediates =
      Or_unknown.map immediates ~f:(fun immediates ->
        Immediates.erase_aliases immediates env ~allowed);
    in
    Blocks_and_tagged_immediates {
      blocks;
      immediates;
    }
  | Boxed_number (Boxed_float ty) ->
    Boxed_number (Boxed_float (
      erase_aliases_ty env ~allowed erase_aliases_of_kind_naked_number
        ~force_to_kind:Flambda_type0_core.force_to_kind_naked_float
        ~print_ty:Type_printers.print_ty_naked_float
        ty))
  | Boxed_number (Boxed_int32 ty) ->
    Boxed_number (Boxed_int32 (
      erase_aliases_ty env ~allowed erase_aliases_of_kind_naked_number
        ~force_to_kind:Flambda_type0_core.force_to_kind_naked_int32
        ~print_ty:Type_printers.print_ty_naked_int32
        ty))
  | Boxed_number (Boxed_int64 ty) ->
    Boxed_number (Boxed_int64 (
      erase_aliases_ty env ~allowed erase_aliases_of_kind_naked_number
        ~force_to_kind:Flambda_type0_core.force_to_kind_naked_int64
        ~print_ty:Type_printers.print_ty_naked_int64
        ty))
  | Boxed_number (Boxed_nativeint ty) ->
    Boxed_number (Boxed_nativeint (
      erase_aliases_ty env ~allowed erase_aliases_of_kind_naked_number
        ~force_to_kind:Flambda_type0_core.force_to_kind_naked_nativeint
        ~print_ty:Type_printers.print_ty_naked_nativeint
        ty))
  | Closures { by_closure_id; } ->
    Closures {
      by_closure_id =
        Closures_entry_by_closure_id.erase_aliases by_closure_id env ~allowed;
    }
  | String _ -> of_kind

and erase_aliases_of_kind_fabricated env ~allowed
      (of_kind : Flambda_types.of_kind_fabricated)
      : Flambda_types.of_kind_fabricated =
  match of_kind with
  | Discriminants discrs ->
    Discriminants (Discriminants.erase_aliases discrs env ~allowed)
  | Set_of_closures { closures; } ->
    Set_of_closures {
      closures = Closure_ids.erase_aliases closures env ~allowed;
    }

let erase_aliases_ty_value env ~allowed ty =
  erase_aliases_ty env ~allowed erase_aliases_of_kind_value ty

let erase_aliases_ty_fabricated env ~allowed ty =
  erase_aliases_ty env ~allowed erase_aliases_of_kind_fabricated ty
