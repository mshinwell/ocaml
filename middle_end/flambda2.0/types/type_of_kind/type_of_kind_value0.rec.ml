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

type t =
  | Blocks_and_tagged_immediates of {
      immediates : Immediates.t Or_unknown.t;
      blocks : Blocks.t Or_unknown.t;
    }
  | Boxed_number of Type_of_kind_naked_number.t
  | Closures of {
      by_closure_id : Closures_entry_by_set_of_closures_contents.t;
    }
  | String of String_info.Set.t
  | Array of { length : Type_of_kind_value.t; }

let print_with_cache ~cache ppf t =
  match t with
  | Blocks_and_tagged_immediates { blocks; immediates; } ->
    (* CR mshinwell: Improve so that we elide blocks and/or immediates when
       they're empty. *)
    Format.fprintf ppf
      "@[<hov 1>(Blocks_and_immediates@ \
        @[<hov 1>(blocks@ %a)@]@ \
        @[<hov 1>(immediates@ %a)@]\
        )@]"
      (Or_unknown.print (Blocks.print_with_cache ~cache)) blocks
      (Or_unknown.print (Immediates.print_with_cache ~cache)) immediates
  | Boxed_number naked_ty ->
    Format.fprintf ppf "@[<hov 1>(Boxed_number@ %a)@]"
      (Type_of_kind_naked_number.print_with_cache ~cache) naked_ty
  | Closures { by_closure_id; } ->
    Closures_entry_by_set_of_closures_contents.print_with_cache ~cache
      ppf by_closure_id
  | String str_infos ->
    Format.fprintf ppf "@[<hov 1>(Strings@ (%a))@]"
      String_info.Set.print str_infos
  | Array { length; } ->
    Format.fprintf ppf "@[<hov 1>(Array@ (length@ %a))@]"
      (Type_of_kind_value.print_with_cache ~cache) length

let apply_name_permutation_blocks_and_tagged_immediates blocks immediates perm =
  let immediates' =
    Or_unknown.map immediates ~f:(fun immediates ->
      Immediates.apply_name_permutation immediates perm)
  in
  let blocks' =
    Or_unknown.map blocks ~f:(fun blocks ->
      Blocks.apply_name_permutation blocks perm)
  in
  if immediates == immediates' && blocks == blocks' then
    blocks_and_tagged_immediates
  else
    { immediates = immediates'; blocks = blocks'; }

let apply_name_permutation t perm =
  match t with
  | Blocks_and_tagged_immediates { blocks; immediates; } ->
    let blocks_and_tagged_immediates' =
      apply_name_permutation_blocks_and_tagged_immediates
        blocks_and_tagged_immediates perm
    in
    if blocks_and_tagged_immediates == blocks_and_tagged_immediates' then
      of_kind_value
    else
      Blocks_and_tagged_immediates blocks_and_tagged_immediates'
  | Boxed_number (Boxed_float ty_naked_number) ->
    let ty_naked_number' =
      apply_name_permutation_ty apply_name_permutation_of_kind_naked_number
        ty_naked_number perm
    in
    if ty_naked_number == ty_naked_number' then of_kind_value
    else Boxed_number (Boxed_float ty_naked_number')
  | Boxed_number (Boxed_int32 ty_naked_number) ->
    let ty_naked_number' =
      apply_name_permutation_ty apply_name_permutation_of_kind_naked_number
        ty_naked_number perm
    in
    if ty_naked_number == ty_naked_number' then of_kind_value
    else Boxed_number (Boxed_int32 ty_naked_number')
  | Boxed_number (Boxed_int64 ty_naked_number) ->
    let ty_naked_number' =
      apply_name_permutation_ty apply_name_permutation_of_kind_naked_number
        ty_naked_number perm
    in
    if ty_naked_number == ty_naked_number' then of_kind_value
    else Boxed_number (Boxed_int64 ty_naked_number')
  | Boxed_number (Boxed_nativeint ty_naked_number) ->
    let ty_naked_number' =
      apply_name_permutation_ty apply_name_permutation_of_kind_naked_number
        ty_naked_number perm
    in
    if ty_naked_number == ty_naked_number' then of_kind_value
    else Boxed_number (Boxed_nativeint ty_naked_number')
  | Closures { by_closure_id; } ->
    let by_closure_id' =
      Closures_entry_by_set_of_closures_contents.apply_name_permutation
        by_closure_id perm
    in
    if by_closure_id == by_closure_id' then of_kind_value
    else Closures { by_closure_id = by_closure_id'; }
  | String _ -> of_kind_value
  | Array { length; } ->
    let length' = Type_of_kind_value.apply_name_permutation length perm in
    if length == length' then of_kind_value
    else Array { length = length'; }

let free_names t =
  match of_kind with
  | Blocks_and_tagged_immediates { blocks; immediates; } ->
    Name_occurrences.union
      (Or_unknown.free_names Blocks.free_names blocks)
      (Or_unknown.free_names Immediates.free_names immediates)
  | Boxed_number (Boxed_float n) ->
    free_names_ty free_names_of_kind_naked_number n
  | Boxed_number (Boxed_int32 n) ->
    free_names_ty free_names_of_kind_naked_number n
  | Boxed_number (Boxed_int64 n) ->
    free_names_ty free_names_of_kind_naked_number n
  | Boxed_number (Boxed_nativeint n) ->
    free_names_ty free_names_of_kind_naked_number n
  | Closures { by_closure_id; } ->
    Closures_entry_by_set_of_closures_contents.free_names by_closure_id
  | String _ -> Name_occurrences.empty
  | Array { length; } -> Type_of_kind_value.free_names length

let apply_rec_info t rec_info : _ Or_bottom.t =
  match t with
  | Closures { by_closure_id; } ->
    Or_bottom.map
      (Closures_entry_by_set_of_closures_contents.map_function_decl_types
        by_closure_id
        ~f:(fun (decl : Type_grammar.function_declaration)
              : Type_grammar.function_declaration Or_bottom.t ->
          match decl with
          | Non_inlinable _ -> Ok decl
          | Inlinable { function_decl; rec_info = old_rec_info; } ->
            let rec_info = Rec_info.merge old_rec_info ~newer:rec_info in
            Ok (Inlinable { function_decl; rec_info; })))
      ~f:(fun by_closure_id -> Closures { by_closure_id; })
  | Blocks_and_tagged_immediates _
  | Boxed_number _
  | String _
  | Array _ ->
    if Rec_info.is_initial rec_info then Ok of_kind_value
    else Bottom

module Make_meet_and_join
  (E : Lattice_ops_intf.S
   with type meet_env := Meet_env.t
   with type typing_env := Typing_env.t
   with type typing_env_extension := Typing_env_extension.t) =
struct
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

let meet_or_join_blocks_and_tagged_immediates env
      ~blocks1 ~imms1 ~blocks2 ~imms2 : _ Or_bottom.t =
  let blocks =
    E.switch (meet_unknown Blocks.meet) (join_unknown Blocks.join)
      env blocks1 blocks2
  in
  let imms =
    E.switch (meet_unknown Immediates.meet) (join_unknown Immediates.join)
      env imms1 imms2
  in
  match blocks, imms with
  | Bottom, Bottom -> Bottom
  | Ok (blocks, env_extension), Bottom ->
    let immediates : _ Or_unknown.t = Known (Immediates.create_bottom ()) in
    Ok (blocks, immediates, env_extension)
  | Bottom, Ok (immediates, env_extension) ->
    let blocks : _ Or_unknown.t = Known (Blocks.create_bottom ()) in
    Ok (blocks, immediates, env_extension)
  | Ok (blocks, env_extension1), Ok (immediates, env_extension2) ->
    let env_extension =
      (* XXX *)
      let left_env = Meet_env.env env in
      let right_env = Meet_env.env env in
      (* CR mshinwell: Move to [TEE] *)
      let join_extensions env ext1 ext2 =
        let env_extension, _ =
          TEE.n_way_join env [
            left_env, Apply_cont_rewrite_id.create (), ext1;
            right_env, Apply_cont_rewrite_id.create (), ext2;
          ]
        in
        env_extension
      in
      E.switch0 TEE.meet join_extensions env
        env_extension1 env_extension2
    in
    Ok (blocks, immediates, env_extension)

let meet_or_join_naked_number env n1 n2 meet_or_join_ty box =
  Or_bottom_or_absorbing.of_or_bottom (meet_or_join_ty env n1 n2)
    ~f:(fun (n, env_extension) -> T.Boxed_number (box n), env_extension)

let meet_or_join env t1 t2 : _ Or_bottom_or_absorbing.t =
  match of_kind1, of_kind2 with
  | Blocks_and_tagged_immediates { blocks = blocks1; immediates = imms1; },
    Blocks_and_tagged_immediates { blocks = blocks2; immediates = imms2; } ->
    Or_bottom_or_absorbing.of_or_bottom
      (meet_or_join_blocks_and_tagged_immediates env
        ~blocks1 ~imms1 ~blocks2 ~imms2)
      ~f:(fun (blocks, immediates, env_extension) ->
        T.Blocks_and_tagged_immediates { blocks; immediates; }, env_extension)
  | Boxed_number (Boxed_float n1),
    Boxed_number (Boxed_float n2) ->
    meet_or_join_naked_number env n1 n2
      (* CR mshinwell: Sort out this [bound_name] stuff *)
      (Naked_float.meet_or_join_ty ?bound_name:None)
      (fun n -> T.Boxed_float n)
  | Boxed_number (Boxed_int32 n1), Boxed_number (Boxed_int32 n2) ->
    meet_or_join_naked_number env n1 n2
      (Naked_int32.meet_or_join_ty ?bound_name:None)
      (fun n -> T.Boxed_int32 n)
  | Boxed_number (Boxed_int64 n1), Boxed_number (Boxed_int64 n2) ->
    meet_or_join_naked_number env n1 n2
      (Naked_int64.meet_or_join_ty ?bound_name:None)
      (fun n -> T.Boxed_int64 n)
  | Boxed_number (Boxed_nativeint n1), Boxed_number (Boxed_nativeint n2) ->
    meet_or_join_naked_number env n1 n2
      (Naked_nativeint.meet_or_join_ty ?bound_name:None)
      (fun n -> T.Boxed_nativeint n)
  | Closures { by_closure_id = by_closure_id1; },
      Closures { by_closure_id = by_closure_id2; } ->
    let module C = Closures_entry_by_set_of_closures_contents in
    Or_bottom_or_absorbing.of_or_bottom
      (E.switch C.meet C.join env by_closure_id1 by_closure_id2)
      ~f:(fun (by_closure_id, env_extension) ->
        T.Closures { by_closure_id; }, env_extension)
  | String strs1, String strs2 ->
    let strs = E.String_info.Set.union_or_inter strs1 strs2 in
    if String_info.Set.is_empty strs then Bottom
    else Or_bottom_or_absorbing.Ok (String strs, TEE.empty ())
  | Array { length = length1; }, Array { length = length2; } ->
    Or_bottom_or_absorbing.of_or_bottom
      (meet_or_join_ty ?bound_name:None env length1 length2)
      ~f:(fun (length, env_extension) -> T.Array { length }, env_extension)
  | (Blocks_and_tagged_immediates _
      | Boxed_number _
      | Closures _
      | String _
      | Array _), _ -> Absorbing
end
