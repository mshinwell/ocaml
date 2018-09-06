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

type t = {
  perm_left : Name_permutation.t;
  perm_right : Name_permutation.t;
}

let print ppf { perm_left; perm_right; } =
  Format.fprintf ppf
    "@[<hov 1>(\
      @[<hov 1>(perm_left@ %a)@]@ \
      @[<hov 1>(perm_right@ %a)@])@]"
    Name_permutation.print perm_left
    Name_permutation.print perm_right

let create ~perm_left ~perm_right =
  { perm_left;
    perm_right;
  }

let empty () =
  create ~perm_left:(Name_permutation.create ())
    ~perm_right:(Name_permutation.create ())

let perm_left t = t.perm_left
let perm_right t = t.perm_right

let shortcut_precondition t =
  t.perm_left == t.perm_right

let compose_name_permutations t ~perm_left ~perm_right =
  { perm_left =
      Name_permutation.compose ~first:t.perm_left ~second:perm_left;
    perm_right =
      Name_permutation.compose ~first:t.perm_right ~second:perm_right;
  }
