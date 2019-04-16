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

(** Translate Lambda code to Flambda 2.0 code and then optimize it. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val middle_end
   : ppf_dump:Format.formatter
  -> prefixname:string
  -> backend:(module Flambda2_backend_intf.S)
  -> size:int
  -> filename:string
  -> module_ident:Ident.t
  -> module_initializer:Lambda.lambda
  -> Flambda_static.Program.t
