(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The interface of typing environment extensions. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type S_types = sig
  type t
end

module type S = sig
  module Flambda_types : sig type t end
  module Meet_env : sig type t end
  module Typing_env : sig type t end
  module Join_env : sig type t end

  type t

  include Contains_names.S with type t := t

  (** Perform various invariant checks upon the given environment
      extension. *)
  val invariant : t -> unit

  (** Print the given typing environment to a formatter. *)
  val print : Format.formatter -> t -> unit

  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

  (** Equality on two environment extensions.
      Note that this doesn't do anything fancy such as making a canonical
      form of environment from the extensions; it's just a structural
      comparison. *)
  val equal : Type_equality_env.t -> t -> t -> bool

  (** A sound but not complete equality function which is much faster than
      [equal]. *)
  val fast_equal : t -> t -> bool

  (** The unique environment extension containing no information. *)
  val empty : t

  val is_empty : t -> bool

  (** Add a definition of an existentially-bound name prior to all
      other entries in the given extension. *)
  val add_definition_at_beginning : t -> Name.t -> Flambda_types.t -> t

  (** Add an equation (on a name that is either existentially bound in the
      given extension, or free in the extension) after all other entries in
      the given extension. *)
  val add_equation : t -> Name.t -> Flambda_types.t -> t

  val add_cse : t -> Simple.t -> Flambda_primitive.With_fixed_value.t -> t

  (** Least upper bound of two environment extensions. *)
  val meet
     : Meet_env.t
    -> t
    -> t
    -> t

  (** Greatest lower bound of two environment extensions. *)
  val join
     : Join_env.t
    -> t
    -> t
    -> t

  val restrict_to_definitions : t -> t

  val restrict_names_to_those_occurring_in_types
     : t
    -> Typing_env.t
    -> Typing_env.t
    -> Flambda_types.t list
    -> t

  (** [diff t env] computes the environment extension whose bindings are
      those in [t], when interpreted in the context of [env], that:
        - do not occur in [env]; or
        - do occur in [env] but where [t] contains a more precise type.
  *)
  val diff : t -> Typing_env.t -> t
end
