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

val print_with_cache
   : cache:Printing_cache.t
  -> Format.formatter
  -> t
  -> unit

val print : Format.formatter -> t -> unit

val invariant : t -> unit

val empty : unit -> t

val is_empty : t -> bool

val defined_vars_in_order : t -> (Variable.t * Flambda_kind.t) list

val defined_vars_in_order' : t -> Variable.t list

val equations : t -> Flambda_types.t Name.Map.t

val one_equation : Name.t -> Flambda_types.t -> t

val add_definition : t -> Variable.t -> Flambda_kind.t -> Binding_time.t -> t

val add_or_replace_equation : t -> Name.t -> Flambda_types.t -> t

val add_cse
   : t
  -> Flambda_primitive.Eligible_for_cse.t
  -> bound_to:Simple.t
  -> t

val concat : t -> t -> t

val meet : Meet_env.t -> t -> t -> t

val n_way_join
   : Typing_env.t
  -> (Typing_env.t * Apply_cont_rewrite_id.t * t) list
  -> t * Continuation_extra_params_and_args.t

val mem : t -> Name.t -> bool

val cse : t -> Simple.t Flambda_primitive.Eligible_for_cse.Map.t

include Contains_names.S with type t := t
