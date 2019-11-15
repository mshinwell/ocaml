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

type t =
  | Code of {
      code : Term_language_code.t;
    }

let print_with_cache ~cache ppf t =
  match t with
  | Code { code; } ->
    (* CR mshinwell: Improve so that we elide blocks and/or immediates when
       they're empty. *)
    Format.fprintf ppf
      "@[<hov 1>(Code@ \
        @[<hov 1>(code@ %a)@]\
        )@]"
      Term_language_code.print code

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let apply_name_permutation t perm =
  match t with
  | Code { code; } ->
    let code' = Term_language_code.apply_name_permutation perm code in
    if code == code' then t
    else Code { code = code'; }

let free_names t =
  match t with
  | Code { code; } -> Term_language_code.free_names code

let apply_rec_info t _rec_info : _ Or_bottom.t = t

module Make_meet_or_join
  (E : Lattice_ops_intf.S
   with type meet_env := Meet_env.t
   with type typing_env := Typing_env.t
   with type typing_env_extension := Typing_env_extension.t) =
struct
  let meet_or_join env t1 t2 : _ Or_bottom_or_absorbing.t =
    match t1, t2 with
    | Code { code = code1; }, Code { code = code2; } ->
      if code1 == code2 then Ok code1
      else Absorbing
end
