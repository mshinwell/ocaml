(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

(** A piece of code, comprising of the parameters and body of a function,
    together with a field indicating whether the piece of code is a newer
    version of one that existed previously (and may still exist), for
    example after a round of simplification. *)
type t

val code_id : t -> Code_id.t

val params_and_body : t -> Function_params_and_body.t Or_deleted.t

val params_and_body_opt : t -> Function_params_and_body.t option

val params_and_body_must_be_present
   : error_context:string
  -> t
  -> Function_params_and_body.t

val newer_version_of : t -> Code_id.t option

val params_arity : t -> Flambda_arity.With_subkinds.t

val result_arity : t -> Flambda_arity.With_subkinds.t

val stub : t -> bool

val inline : t -> Inline_attribute.t

val is_a_functor : t -> bool

val recursive : t -> Recursive.t

val create
   : Code_id.t  (** needed for [compare], although useful otherwise too *)
  -> params_and_body:Function_params_and_body.t Or_deleted.t
  -> newer_version_of:Code_id.t option
  -> params_arity:Flambda_arity.With_subkinds.t
  -> result_arity:Flambda_arity.With_subkinds.t
  -> stub:bool
  -> inline:Inline_attribute.t
  -> is_a_functor:bool
  -> recursive:Recursive.t
  -> t

val with_code_id : Code_id.t -> t -> t

val with_params_and_body : Function_params_and_body.t Or_deleted.t -> t -> t

val with_newer_version_of : Code_id.t option -> t -> t

val make_deleted : t -> t

include Contains_names.S with type t := t

val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

val print : Format.formatter -> t -> unit

val all_ids_for_export : t -> Ids_for_export.t

val import : Ids_for_export.Import_map.t -> t -> t

val compare : t -> t -> int

(* CR mshinwell: Somewhere there should be an invariant check that
   code has no free names. *)

