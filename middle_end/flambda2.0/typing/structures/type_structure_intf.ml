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

module type S = sig
  type join_env
  type meet_env
  type type_equality_env
  type type_equality_result
  type typing_env_extension
  type flambda_type

  type t

  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

  val print : Format.formatter -> t -> unit

  val equal
     : type_equality_env
    -> type_equality_result
    -> t
    -> t
    -> type_equality_result

  val meet : meet_env -> t -> t -> (t * typing_env_extension) Or_bottom.t

  (* CR mshinwell: The signature of [join] implies that each [t] must have
     a bottom element in itself.  How do we reconcile that against the fact
     that we're trying to propagate bottom upwards? *)
  val join : join_env -> t -> t -> t

  val map_types : t -> f:(flambda_type -> flambda_type) -> t

  (** Type algebraic structures are never kept underneath object-language
      binders at present, so we don't include [Contains_names.S]. *)
end
