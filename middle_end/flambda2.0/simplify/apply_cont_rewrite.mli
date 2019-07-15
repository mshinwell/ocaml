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

val create
   : original_params:Kinded_parameter.t list
  -> used_params:Kinded_parameter.t list
  -> used_extra_params:(Kinded_parameter.t * Simple.t) list
  -> t

val apply : t -> Apply_cont.t -> Apply_cont.t

val extra_params : t -> Kinded_parameter.t list

val extra_args : t -> Simple.t list
