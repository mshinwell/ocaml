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

type inlinable = {
  code : Type_grammar.t;
  rec_info : Rec_info.t;
}

type t =
  | Non_inlinable of {
      param_arity : Flambda_arity.t;
      result_arity : Flambda_arity.t;
      recursive : Recursive.t;
    }
  | Inlinable of inlinable

(* CR mshinwell: Add [create] and make [private]. *)

val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

val meet_or_join_or_unknown
   : t Or_unknown.t
  -> t Or_unknown.t
  -> t Or_unknown.t * Typing_env_extension.t