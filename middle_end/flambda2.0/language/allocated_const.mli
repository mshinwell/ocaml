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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** Constants that are always allocated (possibly statically).  Blocks
    are not included here since they are always encoded using
    [Prim (Pmakeblock, ...)]. *)

type t =
  | Boxed_float of float
  | Boxed_int32 of Int32.t
  | Boxed_int64 of Int64.t
  | Boxed_nativeint of Targetint.t
  (* CR-someday mshinwell: consider using "float array" *)
  | Mutable_float_array of { initial_value : float list; }
  | Immutable_float_array of float list
  | Mutable_string of { initial_value : string; }
  | Immutable_string of string

val tag : t -> Tag.t

val compare : t -> t -> int

val print : Format.formatter -> t -> unit
