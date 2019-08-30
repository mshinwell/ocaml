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
      ~already_seen ~allowed (o : _ Type_grammar.unknown_or_join)
      : _ Type_grammar.unknown_or_join =
  match o with
  | Unknown | Bottom -> o
  | Ok contents ->
    let contents' =
      erase_aliases_contents env ~bound_name ~already_seen
        ~allowed contents
    in
    if contents == contents' then o
    else Ok contents'

(* CR mshinwell: [bound_name] may be unused *)

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
(*
Format.eprintf "checking canonical simple %a.  allowed? %b  mem? %b\n%!"
  Simple.print simple
  (Simple.allowed simple ~allowed)
  (Simple.Set.mem simple already_seen);
*)
      if Simple.allowed simple ~allowed then Equals simple
      else if Simple.Set.mem simple already_seen then No_alias Unknown
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
      (ty : n Type_grammar.of_kind_naked_number) = ty

let erase_aliases env ~allowed (t : Type_grammar.t) : Type_grammar.t =
  let delayed_allowed_vars =
    Variable.Set.inter allowed t.delayed_allowed_vars
  in
  { t with
    delayed_allowed_vars;
  }

let erase_aliases env ~allowed (t : Type_grammar.t) : Type_grammar.t =
  match t with
  | Value ty ->
    let ty' =
      erase_aliases_ty env ~allowed
        ~force_to_kind:Basic_type_ops.force_to_kind_value
        ~print_ty:Type_printers.print_ty_value
        ~apply_rec_info:Basic_type_ops.apply_rec_info_of_kind_value
        erase_aliases_of_kind_value
        ty
    in
    if ty == ty' then t
    else Value ty'
  | Naked_number (ty, kind) ->
    let ty' =
      erase_aliases_ty env ~allowed
        ~force_to_kind:(Basic_type_ops.force_to_kind_naked_number kind)
        ~print_ty:Type_printers.print_ty_naked_number
        ~apply_rec_info:Basic_type_ops.apply_rec_info_of_kind_naked_number
        erase_aliases_of_kind_naked_number
        ty
    in
    if ty == ty' then t
    else Naked_number (ty', kind)
  | Fabricated ty ->
    let ty' =
      erase_aliases_ty env ~allowed
        ~force_to_kind:Basic_type_ops.force_to_kind_fabricated
        ~print_ty:Type_printers.print_ty_fabricated
        ~apply_rec_info:Basic_type_ops.apply_rec_info_of_kind_fabricated
        erase_aliases_of_kind_fabricated
        ty
    in
    if ty == ty' then t
    else Fabricated ty'

(*
let rec erase_aliases env ~bound_name ~already_seen ~allowed
      (t : Type_grammar.t) : Type_grammar.t =
(*
Format.eprintf "erase_aliases (allowed %a) %a\n%!"
  Variable.Set.print allowed
  Type_printers.print t;
*)
  match t with
  | Value ty ->
    let ty' =
      erase_aliases_ty env ~bound_name ~already_seen ~allowed
        ~force_to_kind:Basic_type_ops.force_to_kind_value
        ~print_ty:Type_printers.print_ty_value
        ~apply_rec_info:Basic_type_ops.apply_rec_info_of_kind_value
        erase_aliases_of_kind_value
        ty
    in
    if ty == ty' then t
    else Value ty'
  | Naked_number (ty, kind) ->
    let ty' =
      erase_aliases_ty env ~bound_name ~already_seen ~allowed
        ~force_to_kind:(Basic_type_ops.force_to_kind_naked_number kind)
        ~print_ty:Type_printers.print_ty_naked_number
        ~apply_rec_info:Basic_type_ops.apply_rec_info_of_kind_naked_number
        erase_aliases_of_kind_naked_number
        ty
    in
    if ty == ty' then t
    else Naked_number (ty', kind)
  | Fabricated ty ->
    let ty' =
      erase_aliases_ty env ~bound_name ~already_seen ~allowed
        ~force_to_kind:Basic_type_ops.force_to_kind_fabricated
        ~print_ty:Type_printers.print_ty_fabricated
        ~apply_rec_info:Basic_type_ops.apply_rec_info_of_kind_fabricated
        erase_aliases_of_kind_fabricated
        ty
    in
    if ty == ty' then t
    else Fabricated ty'
*)

and erase_aliases_of_kind_value env ~bound_name ~already_seen ~allowed
      (of_kind : Type_grammar.of_kind_value)
      : Type_grammar.of_kind_value =
  match of_kind with
  | Blocks_and_tagged_immediates { blocks; immediates; } ->
    let blocks' =
      Or_unknown.map_sharing blocks ~f:(fun blocks ->
        Blocks.erase_aliases blocks env ~already_seen ~allowed)
    in
    let immediates' =
      Or_unknown.map_sharing immediates ~f:(fun immediates ->
        Immediates.erase_aliases immediates env ~already_seen ~allowed);
    in
    if blocks == blocks' && immediates == immediates' then of_kind
    else
      Blocks_and_tagged_immediates {
        blocks = blocks';
        immediates = immediates';
      }
  | Boxed_number (Boxed_float ty) ->
    let ty' =
      erase_aliases_ty env ~bound_name ~already_seen ~allowed
        erase_aliases_of_kind_naked_number
        ~force_to_kind:Basic_type_ops.force_to_kind_naked_float
        ~print_ty:Type_printers.print_ty_naked_float
        ~apply_rec_info:Basic_type_ops.apply_rec_info_of_kind_naked_number
        ty
    in
    if ty == ty' then of_kind
    else Boxed_number (Boxed_float ty')
  | Boxed_number (Boxed_int32 ty) ->
    let ty' =
      erase_aliases_ty env ~bound_name ~already_seen ~allowed
        erase_aliases_of_kind_naked_number
        ~force_to_kind:Basic_type_ops.force_to_kind_naked_int32
        ~print_ty:Type_printers.print_ty_naked_int32
        ~apply_rec_info:Basic_type_ops.apply_rec_info_of_kind_naked_number
        ty
    in
    if ty == ty' then of_kind
    else Boxed_number (Boxed_int32 ty')
  | Boxed_number (Boxed_int64 ty) ->
    let ty' =
      erase_aliases_ty env ~bound_name ~already_seen ~allowed
        erase_aliases_of_kind_naked_number
        ~force_to_kind:Basic_type_ops.force_to_kind_naked_int64
        ~print_ty:Type_printers.print_ty_naked_int64
        ~apply_rec_info:Basic_type_ops.apply_rec_info_of_kind_naked_number
        ty
    in
    if ty == ty' then of_kind
    else Boxed_number (Boxed_int64 ty')
  | Boxed_number (Boxed_nativeint ty) ->
    let ty' =
      erase_aliases_ty env ~bound_name ~already_seen ~allowed
        erase_aliases_of_kind_naked_number
        ~force_to_kind:Basic_type_ops.force_to_kind_naked_nativeint
        ~print_ty:Type_printers.print_ty_naked_nativeint
        ~apply_rec_info:Basic_type_ops.apply_rec_info_of_kind_naked_number
        ty
    in
    if ty == ty' then of_kind
    else Boxed_number (Boxed_nativeint ty')
  | Closures { by_closure_id; } ->
    let by_closure_id' =
      Closures_entry_by_set_of_closures_contents.erase_aliases by_closure_id
        env ~already_seen ~allowed
    in
    if by_closure_id == by_closure_id' then of_kind
    else Closures { by_closure_id = by_closure_id'; }
  | String _ -> of_kind
  | Array { length; } ->
    let length' =
      erase_aliases_ty_value env ~bound_name ~already_seen ~allowed length
    in
    if length == length' then of_kind
    else Array { length = length'; }

and erase_aliases_of_kind_fabricated env ~bound_name:_ ~already_seen ~allowed
      (of_kind : Type_grammar.of_kind_fabricated)
      : Type_grammar.of_kind_fabricated =
  match of_kind with
  | Discriminants discrs ->
    let discrs' =
      Discriminants.erase_aliases discrs env ~already_seen ~allowed
    in
    if discrs == discrs' then of_kind
    else Discriminants discrs'

and erase_aliases_ty_value env ~bound_name ~already_seen ~allowed ty =
  erase_aliases_ty env ~bound_name ~already_seen ~allowed
    erase_aliases_of_kind_value
    ~force_to_kind:Basic_type_ops.force_to_kind_value
    ~print_ty:Type_printers.print_ty_value
    ~apply_rec_info:Basic_type_ops.apply_rec_info_of_kind_value
    ty
