(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = Targetint.OCaml.t

let create i =
  if Targetint.OCaml.compare i Targetint.OCaml.zero < 0 then None
  else Some i

let create_exn i =
  match create i with
  | Some t -> t
  | None ->
    Misc.fatal_errorf "Discriminant.of_int_exn: invalid discriminant %a"
      Targetint.OCaml.print i

let of_int_exn i =
  let ti = Targetint.OCaml.of_int i in
  match create ti with
  | Some t -> t
  | None ->
    Misc.fatal_errorf "Discriminant.of_int_exn: invalid discriminant %d" i

let of_tag t =
  let tag = Tag.to_int t in
  of_int_exn tag

let to_tag t = Tag.create_from_targetint t

let to_int t = t

let zero = Targetint.OCaml.zero

let bool_false = zero
let bool_true = Targetint.OCaml.one

include Hashtbl.Make_with_map (struct
  type nonrec t = t

  let compare = Targetint.OCaml.compare
  let hash = Targetint.OCaml.hash

  let print ppf t =
    Format.fprintf ppf "@[discr_%a@]" Targetint.OCaml.print t
end)
