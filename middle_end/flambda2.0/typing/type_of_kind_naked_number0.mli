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

(** Unboxed ("naked") integer and floating-point numbers. *)

type 'k t0 =
  | Immediate : Immediate.Set.t -> Flambda_kind.naked_immediate t0
  | Float : Float.Set.t -> Flambda_kind.naked_float t0
  | Int32 : Int32.Set.t -> Flambda_kind.naked_int32 t0
  | Int64 : Int64.Set.t -> Flambda_kind.naked_int64 t0
  | Nativeint : Targetint.Set.t -> Flambda_kind.naked_nativeint t0

type t = N : 'k t0 * 'k Flambda_kind.Naked_number.t -> t
