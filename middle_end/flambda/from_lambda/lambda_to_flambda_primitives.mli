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

open Wrapper
open Closure_conversion_aux

val convert_and_bind
  : Acc.t
  -> backend:(module Flambda_backend_intf.S)
  -> Exn_continuation.t option
  -> register_const_string:(Acc.t -> string -> Acc.t * Symbol.t)
  -> Lambda.primitive
  -> args:Simple.t list
  -> Debuginfo.t
  -> (Acc.t -> Named_with_size.t option -> Acc.t * Expr_with_size.t)
  -> Acc.t * Expr_with_size.t
