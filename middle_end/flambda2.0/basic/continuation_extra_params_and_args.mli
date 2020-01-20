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

(* CR mshinwell: This probably doesn't belong in the basic/ directory. *)

module Extra_arg : sig
  type t =
    | Already_in_scope of Simple.t
    | New_binding of Flambda_primitive.t

  val print : Format.formatter -> t -> unit

  module List : sig
    type nonrec t = t list

    val print : Format.formatter -> t -> unit
  end
end

type t = {
  extra_params : Kinded_parameter.t list;
  extra_args : Extra_arg.t list Apply_cont_rewrite_id.Map.t;
  extra_args_recursive_uses : Extra_arg.t list;
}

val print : Format.formatter -> t -> unit

val empty : t

val is_empty : t -> bool

val add
   : t
  -> extra_param:Kinded_parameter.t
  -> extra_arg:Extra_arg.t Apply_cont_rewrite_id.Map.t
  -> extra_arg_recursive_uses:Extra_arg.t
  -> t
