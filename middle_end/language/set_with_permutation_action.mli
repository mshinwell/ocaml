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

module Make (N : Name_like_intf.S) : sig
  type t = N.Set.t

  include Contains_names.S with type t := t

  include Set.S_printable
    with type elt := N.t
    with type t := t

  val rename : t -> t

  val permutation_to_swap : t -> t -> Name_permutation.t
end
