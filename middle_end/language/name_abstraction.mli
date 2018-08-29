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
  val create : unit -> t
  val permutation_to_swap : t -> t -> Name_permutation.t
end) (Term : Contains_names.S) : sig
  include Contains_names.S

  val create : Name.t -> Term.t -> t

  val pattern_match : t -> f:(Name.t -> Term.t -> 'a) -> 'a
end
