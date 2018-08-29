(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make (Name : sig
  include Contains_names.S
  val rename : unit -> t
  val permutation_to_swap : t -> t -> Name_permutation.t
end) (Term : Contains_names.S) = struct
  type t = Name.t * Term.t

  let create name term = name, term

  let pattern_match (name, term) ~f =
    let fresh_name = Name.rename name in
    let perm = Name.permutation_to_swap name fresh_name in
    let fresh_term = Term.apply_name_permutation term perm in
    f fresh_name fresh_term

  let apply_name_permutation (name, term) perm =
    let name = Name.apply_name_permutation name perm in
    let term = Term.apply_name_permutation term perm in
    name, term

  let free_names (name, term) =
    let bound = Name.free_names name in
    let free_in_term = Term.free_names term in
    Name_occurrences.diff free_in_term bound
end
