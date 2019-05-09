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

type t =
  | Regular
  | Reraise
  | No_trace

let invariant _env _t = ()

let print ppf t =
  match t with
  | Regular -> Format.pp_print_string ppf "Regular"
  | Reraise -> Format.pp_print_string ppf "Reraise"
  | No_trace -> Format.pp_print_string ppf "No_trace"

let print_with_cache ~cache:_ ppf t = print ppf t

let free_names _t = Name_occurrences.empty

let apply_name_permutation t _perm = t

module Option = struct
  type nonrec t = t option

  let invariant _env _t = ()

  let print ppf = function
    | None -> ()
    | Some t -> print ppf t

  let free_names _t = Name_occurrences.empty

  let apply_name_permutation t _perm = t
end
