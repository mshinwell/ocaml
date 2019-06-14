(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

val create : Flambda_kind.t -> Simple.t -> Binding_time.t -> t

val create_name : Flambda_kind.t -> Name.t -> Binding_time.t -> t

include Identifiable.S with type t := t

val defined_earlier : t -> than:t -> bool

val simple : t -> Simple.t

val kind : t -> Flambda_kind.t
