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

type t = {
  depth : int;
  unroll_to : int option;
}

include Identifiable.Make (struct
  type nonrec t = t

  let print ppf { depth; unroll_to; } =
    Format.fprintf ppf "%s@[<hov 1>(\
        @[<hov 1>(depth@ %d)@]@ \
        @[<hov 1>(unroll_to@ %a)@]\
        )@]%s"
      (Flambda_colours.rec_info ())
      depth
      (Misc.Stdlib.Option.print Numbers.Int.print) unroll_to
      (Flambda_colours.normal ())

  let compare t1 t2 = Stdlib.compare t1 t2

  let equal t1 t2 = (compare t1 t2 = 0)

  let hash t = Hashtbl.hash t

  let output _ _ = Misc.fatal_error "Not yet implemented"
end)

let create ~depth ~unroll_to =
  { depth;
    unroll_to;
  }

let depth t = t.depth
let unroll_to t = t.unroll_to

let join { depth = depth1; unroll_to = unroll_to1; }
         { depth = depth2; unroll_to = unroll_to2; } =
  let depth = depth1 + depth2 in
  let unroll_to =
    match unroll_to1, unroll_to2 with
    | None, None -> None
    | Some unroll_to, None | None, Some unroll_to -> Some unroll_to
    | Some unroll_to1, Some unroll_to2 -> Some (unroll_to1 + unroll_to2)
  in
  { depth;
    unroll_to;
  }
