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

(** Desired semantics of [Apply_cont] expressions in the event that they are
    compiled to "raise". *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | Regular
  | Reraise
  | No_trace

include Expr_std.S with type t := t

module Option : sig
  type nonrec t = t option

  include Expr_std.S with type t := t
end
