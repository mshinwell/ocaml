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
module TEE = Typing_env_extension

module Blocks = Row_like.For_blocks

type t =
  | Variant of {
      is_int : T.t;
      immediates : T.t;
      tag : T.t;
      blocks : Blocks.t Or_unknown.t;
    }
  | Boxed_float of T.t
  | Boxed_int32 of T.t
  | Boxed_int64 of T.t
  | Boxed_nativeint of T.t
  | Closures of {
      by_closure_id : Row_like.For_closures_entry_by_set_of_closures_contents.t;
    }
  | String of String_info.Set.t
  | Array of { length : T.t; }

let print_with_cache ~cache ppf t =
  match t with
  | Variant { is_int; immediates; tag; blocks; } ->
    (* CR mshinwell: Improve so that we elide blocks and/or immediates when
       they're empty. *)
    Format.fprintf ppf
      "@[<hov 1>(Blocks_and_immediates@ \
        @[<hov 1>(is_int@ %a)@]@ \
        @[<hov 1>(immediates@ %a)@]@ \
        @[<hov 1>(tag@ %a)@]@ \
        @[<hov 1>(blocks@ %a)@]\
        )@]"
      (T.print_with_cache ~cache) is_int
      (T.print_with_cache ~cache) immediates
      (T.print_with_cache ~cache) tag
      (Or_unknown.print (Blocks.print_with_cache ~cache)) blocks
  | Boxed_float naked_ty ->
    Format.fprintf ppf "@[<hov 1>(Boxed_float@ %a)@]"
      (T.print_with_cache ~cache) naked_ty
  | Boxed_int32 naked_ty ->
    Format.fprintf ppf "@[<hov 1>(Boxed_int32@ %a)@]"
      (T.print_with_cache ~cache) naked_ty
  | Boxed_int64 naked_ty ->
    Format.fprintf ppf "@[<hov 1>(Boxed_int64@ %a)@]"
      (T.print_with_cache ~cache) naked_ty
  | Boxed_nativeint naked_ty ->
    Format.fprintf ppf "@[<hov 1>(Boxed_nativeint@ %a)@]"
      (T.print_with_cache ~cache) naked_ty
  | Closures { by_closure_id; } ->
    Row_like.For_closures_entry_by_set_of_closures_contents.print_with_cache ~cache
      ppf by_closure_id
  | String str_infos ->
    Format.fprintf ppf "@[<hov 1>(Strings@ (%a))@]"
      String_info.Set.print str_infos
  | Array { length; } ->
    Format.fprintf ppf "@[<hov 1>(Array@ (length@ %a))@]"
      (T.print_with_cache ~cache) length

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let apply_name_permutation_variant
      ~is_int ~immediates ~tag blocks perm =
  let is_int' = T.apply_name_permutation is_int perm in
  let immediates' = T.apply_name_permutation immediates perm in
  let tag' = T.apply_name_permutation tag perm in
  let blocks' =
    Or_unknown.map blocks ~f:(fun blocks ->
      Blocks.apply_name_permutation blocks perm)
  in
  if is_int == is_int' && immediates == immediates' && tag == tag'
       && blocks == blocks' then
    None
  else
    Some (is_int', immediates', tag', blocks')

let apply_name_permutation t perm =
  match t with
  | Variant { is_int; immediates; tag; blocks; } ->
    begin match
      apply_name_permutation_variant ~is_int ~immediates ~tag blocks perm
    with
    | None -> t
    | Some (is_int, immediates, tag, blocks) ->
      Variant { is_int; immediates; tag; blocks; }
    end
  | Boxed_float ty ->
    let ty' = T.apply_name_permutation ty perm in
    if ty == ty' then t
    else Boxed_float ty'
  | Boxed_int32 ty ->
    let ty' = T.apply_name_permutation ty perm in
    if ty == ty' then t
    else Boxed_int32 ty'
  | Boxed_int64 ty ->
    let ty' = T.apply_name_permutation ty perm in
    if ty == ty' then t
    else Boxed_int64 ty'
  | Boxed_nativeint ty ->
    let ty' = T.apply_name_permutation ty perm in
    if ty == ty' then t
    else Boxed_nativeint ty'
  | Closures { by_closure_id; } ->
    let by_closure_id' =
      Row_like.For_closures_entry_by_set_of_closures_contents.apply_name_permutation
        by_closure_id perm
    in
    if by_closure_id == by_closure_id' then t
    else Closures { by_closure_id = by_closure_id'; }
  | String _ -> t
  | Array { length; } ->
    let length' = T.apply_name_permutation length perm in
    if length == length' then t
    else Array { length = length'; }

let free_names t =
  match t with
  | Variant { is_int; immediates; tag; blocks; } ->
    Name_occurrences.union_list [
      T.free_names is_int;
      T.free_names immediates;
      T.free_names tag;
      Or_unknown.free_names Blocks.free_names blocks;
    ]
  | Boxed_float ty -> T.free_names ty
  | Boxed_int32 ty -> T.free_names ty
  | Boxed_int64 ty -> T.free_names ty
  | Boxed_nativeint ty -> T.free_names ty
  | Closures { by_closure_id; } ->
    Row_like.For_closures_entry_by_set_of_closures_contents.free_names by_closure_id
  | String _ -> Name_occurrences.empty
  | Array { length; } -> T.free_names length

let apply_rec_info t rec_info : _ Or_bottom.t =
  match t with
  | Closures { by_closure_id; } ->
    Or_bottom.map
      (Row_like.For_closures_entry_by_set_of_closures_contents.map_function_decl_types
        by_closure_id
        ~f:(fun (decl : Function_declaration_type.t)
              : Function_declaration_type.t Or_bottom.t ->
          match decl with
          | Non_inlinable _ -> Ok decl
          | Inlinable { function_decl; rec_info = old_rec_info; } ->
            let rec_info = Rec_info.merge old_rec_info ~newer:rec_info in
            Ok (Inlinable { function_decl; rec_info; })))
      ~f:(fun by_closure_id -> Closures { by_closure_id; })
  | Variant _
  | Boxed_float _
  | Boxed_int32 _
  | Boxed_int64 _
  | Boxed_nativeint _
  | String _
  | Array _ ->
    if Rec_info.is_initial rec_info then Ok t
    else Bottom

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

module Make_meet_or_join
  (E : Lattice_ops_intf.S
   with type meet_env := Meet_env.t
   with type typing_env := Typing_env.t
   with type typing_env_extension := Typing_env_extension.t) =
struct
  let meet_or_join_variant env
        ~is_int1 ~immediates1 ~tag1 ~blocks1
        ~is_int2 ~immediates2 ~tag2 ~blocks2
        : _ Or_bottom.t =
    let is_int = E.switch T.meet T.join env is_int1 is_int2 in
    let immediates = E.switch T.meet T.join env immediates1 immediates2 in
    let tag = E.switch T.meet T.join env tag1 tag2 in
    let blocks =
      E.switch (meet_unknown Blocks.meet) (join_unknown Blocks.join)
        env blocks1 blocks2
    in
    match is_int with
    | Bottom -> Bottom
    | Ok (is_int, env_extension) ->
      let meet_or_join_env_extensions env_extension1 env_extension2 =
        (* XXX *)
        let left_env = Meet_env.env env in
        let right_env = Meet_env.env env in
        (* CR mshinwell: Move to [TEE] *)
        let join_extensions env ext1 ext2 =
          let env_extension, _ =
            TEE.n_way_join ~initial_env_at_join:env [
              left_env, Apply_cont_rewrite_id.create (),
                Variable.Set.empty, ext1;
              right_env, Apply_cont_rewrite_id.create (),
                Variable.Set.empty, ext2;
            ]
          in
          env_extension
        in
        E.switch0 TEE.meet join_extensions env
          env_extension1 env_extension2
      in
      let immediates, env_extension =
        match immediates with
        | Bottom -> T.bottom K.fabricated, env_extension
        | Ok (immediates, env_extension') ->
          immediates, meet_or_join_extensions env_extension env_extension'
      in
      let tag, env_extension =
        match tag with
        | Bottom -> T.bottom K.fabricated, env_extension
        | Ok (tag, env_extension') ->
          tag, meet_or_join_extensions env_extension env_extension'
      in
      let blocks, env_extension =
        match blocks with
        | Bottom -> Blocks.create_bottom ()
        | Ok (blocks, env_extension') ->
          blocks, meet_or_join_extensions env_extension env_extension'
      in
      Ok (is_int, immediates, tag, blocks env_extension)

  let meet_or_join env t1 t2 : _ Or_bottom_or_absorbing.t =
    match t1, t2 with
    | Variant
        { is_int = is_int1; immediates = immediates1; tag = tag1;
          blocks = blocks1; },
      Variant
        { is_int = is_int2; immediates = immediates2; tag = tag2;
          blocks = blocks2; } ->
      Or_bottom_or_absorbing.of_or_bottom
        (meet_or_join_variant env
          ~is_int1 ~immediates1 ~tag1 ~blocks1
          ~is_int2 ~immediates2 ~tag2 ~blocks2)
        ~f:(fun (is_int, immediates, tag, blocks, env_extension) ->
          Variant { is_int; immediates; tag; blocks; }, env_extension)
    | Boxed_float n1, Boxed_float n2 ->
      Or_bottom_or_absorbing.of_or_bottom
        (E.switch T.meet T.join env n1 n2)
        ~f:(fun (n, env_extension) -> Boxed_float n, env_extension)
    | Boxed_int32 n1, Boxed_int32 n2 ->
      Or_bottom_or_absorbing.of_or_bottom
        (E.switch T.meet T.join env n1 n2)
        ~f:(fun (n, env_extension) -> Boxed_int32 n, env_extension)
    | Boxed_int64 n1, Boxed_int64 n2 ->
      Or_bottom_or_absorbing.of_or_bottom
        (E.switch T.meet T.join env n1 n2)
        ~f:(fun (n, env_extension) -> Boxed_int64 n, env_extension)
    | Boxed_nativeint n1, Boxed_nativeint n2 ->
      Or_bottom_or_absorbing.of_or_bottom
        (E.switch T.meet T.join env n1 n2)
        ~f:(fun (n, env_extension) -> Boxed_nativeint n, env_extension)
    | Closures { by_closure_id = by_closure_id1; },
        Closures { by_closure_id = by_closure_id2; } ->
      let module C = Row_like.For_closures_entry_by_set_of_closures_contents in
      Or_bottom_or_absorbing.of_or_bottom
        (E.switch C.meet C.join env by_closure_id1 by_closure_id2)
        ~f:(fun (by_closure_id, env_extension) ->
          Closures { by_closure_id; }, env_extension)
    | String strs1, String strs2 ->
      let strs = E.String_info.Set.union_or_inter strs1 strs2 in
      if String_info.Set.is_empty strs then Bottom
      else Or_bottom_or_absorbing.Ok (String strs, TEE.empty ())
    | Array { length = length1; }, Array { length = length2; } ->
      Or_bottom_or_absorbing.of_or_bottom
        (E.switch T.meet T.join env length1 length2)
        ~f:(fun (length, env_extension) -> Array { length; }, env_extension)
    | (Variant _
        | Boxed_float _
        | Boxed_int32 _
        | Boxed_int64 _
        | Boxed_nativeint _
        | Closures _
        | String _
        | Array _), _ -> Absorbing
end
