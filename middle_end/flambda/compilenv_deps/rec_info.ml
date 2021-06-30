(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]
type t = unit

include Container_types.Make (struct
  type nonrec t = t

  let print ppf () =
    Format.fprintf ppf "@<0>%s()@<0>%s"
    (Flambda_colours.rec_info ())
    (Flambda_colours.normal ())

  let compare () () = 0

  let equal () () = true

  let hash () = Hashtbl.hash ()

  let output _ _ = Misc.fatal_error "Not yet implemented"
end)

let initial = ()

let unknown = ()

let is_initial () = true
