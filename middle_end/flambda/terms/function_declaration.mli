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

type t

(** Printing, invariant checks, name manipulation, etc. *)
include Expr_std.S with type t := t

include Contains_ids.S with type t := t

(** Create a function declaration. *)
val create
   : code_id:Code_id.t
  -> t

(** The identifier of the code of the function (which must be bound using
    [Define_symbol]). *)
val code_id : t -> Code_id.t

(** Return a function declaration that is like the supplied one except
    that it has a new code ID. *)
val update_code_id : t -> Code_id.t -> t

val compare : t -> t -> int

val equal : t -> t -> bool
