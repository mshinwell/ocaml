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
  | Discriminants of Row_like.For_discriminants.t

let print_with_cache ~cache ppf t =
  match t with
  | Discriminants discriminants ->
    Format.fprintf ppf "@[<hov 1>(Discriminants@ %a)@]"
      (Row_like.For_discriminants.print_with_cache ~cache) discriminants

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let apply_name_permutation t perm =
  match t with
  | Discriminants discrs ->
    let discrs' = Row_like.For_discriminants.apply_name_permutation discrs perm in
    if discrs == discrs' then t
    else Discriminants discrs'

let free_names t =
  match t with
  | Discriminants discrs -> Row_like.For_discriminants.free_names discrs

let apply_rec_info t rec_info : _ Or_bottom.t =
  if Rec_info.is_initial rec_info then Ok t
  else Bottom

module Make_meet_or_join
  (E : Lattice_ops_intf.S
   with type meet_env := Meet_env.t
   with type typing_env := Typing_env.t
   with type typing_env_extension := Typing_env_extension.t) =
struct
  let meet_or_join env t1 t2 =
    match t1, t2 with
    | Discriminants discrs1, Discriminants discrs2 ->
      Or_bottom_or_absorbing.of_or_bottom
        (E.switch Row_like.For_discriminants.meet
          Row_like.For_discriminants.join env discrs1 discrs2)
        ~f:(fun (discrs, env_extension) -> Discriminants discrs, env_extension)
end
