(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Basic definitions and constructors for the type system of Flambda. The types
    give approximations to the values obtained by evaluating Flambda terms at
    runtime.  Each type has a kind, as per [Flambda_kind].

    Normal Flambda passes should use the interface provided in [Flambda_types]
    rather than this one. *)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** The type system is parameterised over the expression language. *)
module Make (Expr : sig
  type t
  val print : Format.formatter -> t -> unit
  val free_names : t -> Name_occurrences.t
end) : Flambda_type0_intf.S with type expr = Expr.t
