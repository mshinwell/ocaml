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

(* CR mshinwell: Use this module in [Expr] *)

module Make (Descr : Contains_names.S) = struct
  type t = {
    descr : Descr.t;
    delayed_permutation : Name_permutation.t;
    free_names : Name_occurrences.t;
  }

  let create descr =
    { descr;
      delayed_permutation = Name_permutation.empty;
      free_names = Descr.free_names descr;
    }

  let descr t =
    Descr.apply_name_permutation t.descr t.delayed_permutation

  let apply_name_permutation t perm =
    let delayed_permutation =
      Name_permutation.compose ~second:perm ~first:t.delayed_permutation
    in
    let free_names =
      Name_occurrences.apply_name_permutation t.free_names perm
    in
    { t with
      delayed_permutation;
      free_names;
    }

  let free_names t = t.free_names
end
