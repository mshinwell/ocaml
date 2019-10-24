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
module TI = Targetint
module TEE = Typing_env_extension

type t =
  | Ints of TI.Set.t
  | Is_int of T.t
  | Get_tag of T.t

let print_with_cache ~cache ppf t =
  match t with
  | Ints is ->
    Format.fprintf ppf "@[<hov 1>(Ints@ (%a))@]" TI.Set.print is
  | Is_int ty ->
    Format.fprintf ppf "@[<hov 1>(Is_int@ %a)@]"
      (T.print_with_cache ~cache) ty
  | Get_tag ty ->
    Format.fprintf ppf "@[<hov 1>(Get_tag@ %a)@]"
      (T.print_with_cache ~cache) ty

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let apply_name_permutation t perm =
  match t with
  | Ints _ -> t
  | Is_int ty ->
    let ty' = T.apply_name_permutation ty perm in
    if ty == ty' then t
    else Is_int ty'
  | Get_tag ty ->
    let ty' = T.apply_name_permutation ty perm in
    if ty == ty' then t
    else Get_tag ty'

let free_names t =
  match t with
  | Ints _ -> Name_occurrences.empty
  | Is_int ty | Get_tag ty -> T.free_names ty

let apply_rec_info t rec_info : _ Or_bottom.t =
  if Rec_info.is_initial rec_info then Ok t
  else Bottom

module Make_meet_or_join
  (E : Lattice_ops_intf.S
    with type meet_env := Meet_env.t
    with type typing_env := Typing_env.t
    with type typing_env_extension := Typing_env_extension.t) =
struct
  let bad_meet_or_join env t1 t2 =
    Misc.fatal_errorf "Bad naked number meet/join with %a and %a in env:@ %a"
      print t1
      print t2
      TE.print (Meet_env.env env)

  let meet_or_join env t1 t2 : _ Or_bottom_or_absorbing.t =
  (*
    Format.eprintf "NN meet_or_join %a and %a\n%!"
      print t1 print t2 (*Typing_env.print (Meet_env.env env)*);
      *)
    match t1, t2 with
    | Ints is1, Ints is2 ->
      let is = E.Targetint.Set.union_or_inter is1 is2 in
      if TI.Set.is_empty is then Bottom
      else Ok (Ints is, TEE.empty ())
    | Is_int ty1, Is_int ty2 ->
      Or_bottom_or_absorbing.of_or_bottom (E.switch T.meet T.join env ty1 ty2)
        ~f:(fun (ty, env_extension) -> Is_int ty, env_extension)
    | Get_tag ty1, Get_tag ty2 ->
      Or_bottom_or_absorbing.of_or_bottom (E.switch T.meet T.join env ty1 ty2)
        ~f:(fun (ty, env_extension) -> Get_tag ty, env_extension)
    | Ints is_int, Is_int ty | Is_int ty, Ints is_int ->
      begin match TI.Set.elements is_int with
      | [] -> Bottom
      | [is_int] ->
        let shape =
          if TI.equal is_int TI.zero then T.any_block ()
          else if TI.equal is_int TI.one then T.any_tagged_immediate ()
          else bad_meet_or_join env t1 t2
        in
        Or_bottom_or_absorbing.of_or_bottom
          (E.switch T.meet T.join env ty shape)
          ~f:(fun (ty, env_extension) ->
            Is_int ty, env_extension)
      | _::_ -> bad_meet_or_join env t1 t2
      end
    | Ints tags, Get_tag ty | Get_tag ty, Ints tags ->
      let tags =
        Targetint.Set.fold (fun tag tags ->
            match Tag.create_from_targetint (TI.OCaml.of_targetint tag) with
            | Some tag -> Tag.Set.add tag tags
            | None -> bad_meet_or_join env t1 t2)
          tags
          Tag.Set.empty
      in
      let shape = T.blocks_with_these_tags tags in
      Or_bottom_or_absorbing.of_or_bottom
        (E.switch T.meet T.join env ty shape)
        ~f:(fun (ty, env_extension) -> Is_int ty, env_extension)
    | (Is_int _ | Get_tag _), (Is_int _ | Get_tag _) -> Absorbing
end
