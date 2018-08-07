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

type t = Logical_variable.t

val create : Flambda_kind.t -> t
val kind : t -> Flambda_kind.t
val rename : t -> t
val in_compilation_unit : t -> Compilation_unit.t -> bool
val equal : Type_equality_env.t -> t -> t -> bool
val name : t -> Name.t

include Map.With_set with type t := t
include Contains_names.S with type t := t
