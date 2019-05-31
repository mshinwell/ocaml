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

(** Description of a lifted constant emitted from [Simplify_named].  This
    comprises not only the definition of the constant but also its type, to
    avoid having to re-infer the type once the constant is added to some
    outer environmnent. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

val print : Format.formatter -> t -> unit

val create
   : Flambda_type.t Symbol.Map.t
  -> Flambda_static.Bound_symbols.t
  -> Flambda_static.Static_part.t
  -> t

val introduce : t -> Flambda_type.Typing_env.t -> t

val static_structure : t -> Flambda_static.Static_structure.t
