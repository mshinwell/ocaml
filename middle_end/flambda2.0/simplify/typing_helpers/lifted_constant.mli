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

(* CR mshinwell: Rename file to indicate plurality *)

(** Description of a lifted constant emitted from [Simplify_named].  This
    comprises not only the definition of the constant but also its type, to
    avoid having to re-infer the type once the constant is added to some
    outer environmnent. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

val print : Format.formatter -> t -> unit

val create
   : ?computation:Flambda_static.Program_body.Computation.t
  -> Flambda_type.Typing_env.t
  -> Flambda_type.t Symbol.Map.t
  -> 'k Flambda_static.Program_body.Bound_symbols.t
  -> 'k Flambda_static.Static_part.t
  -> t

val create_from_static_structure
   : Flambda_type.Typing_env.t
  -> Flambda_type.t Symbol.Map.t
  -> Flambda_static.Program_body.Static_structure.t
  -> t list

val create_from_definition
   : Flambda_type.Typing_env.t
  -> Flambda_type.t Symbol.Map.t
  -> Flambda_static.Program_body.Definition.t
  -> t

(* CR mshinwell: Add comment that this doesn't introduce anything if the
   symbols are defined.  Is this the best semantics?  It comes from not wanting
   to diff lifted constants in [r] *)
val introduce
   : t
  -> Flambda_type.Typing_env.t
  -> Flambda_type.Typing_env.t

val definition : t -> Flambda_static.Program_body.Definition.t
