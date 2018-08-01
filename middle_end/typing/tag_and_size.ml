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

type t = Tag.t * Targetint.OCaml.t

include Hashtbl.Make_with_map (struct
  type nonrec t = t

  let compare (tag1, size1) (tag2, size2) =
    let c = Tag.compare tag1 tag2 in
    if c <> 0 then c
    else Targetint.OCaml.compare size1 size2

  let hash (tag, size) =
    Hashtbl.hash (Tag.hash tag, Targetint.OCaml.hash size)

  let print ppf (tag, size) =
    Format.fprintf ppf "@[((tag %a) (size %a))@]"
      Tag.print tag
      Targetint.OCaml.print size
end)

let create tag size = tag, size

let tag (tag, _size) = tag
let size (_tag, size) = size
