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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** The interface of typing environment extensions. *)

module type S = sig
  module T : Flambda_type0_internal_intf.S

  type t

  (** Perform various invariant checks upon the given environment
      extension. *)
  val invariant : t -> unit

  (** Print the given typing environment to a formatter. *)
  val print : Format.formatter -> t -> unit

  (** Equality on two environment extensions, given equality on types.
      Note that this doesn't do anything fancy such as making a canonical
      form of environment from the extensions; it's just a structural
      comparison. *)
  val equal
     : equal_type:(T.flambda_type -> T.flambda_type -> bool)
    -> t
    -> t
    -> bool

  (** A sound but not complete equality function which is much faster than
      [equal]. *)
  val fast_equal : t -> t -> bool

  (** The unique environment extension containing no information. *)
  val empty : t

  (** Add a definition of an existentially-bound name prior to all
      other entries in the given extension. *)
  val add_definition_at_beginning
     : t
    -> Name.t
    -> T.flambda_type
    -> t

  (** Add an equation (on a name that is either existentially bound in the
      given extension, or free in the extension) after all other entries in
      the given extension. *)
  val add_equation
     : t
    -> Name.t
    -> T.flambda_type
    -> t

  val add_cse
     : t
    -> Simple.t
    -> Flambda_primitive.With_fixed_value.t
    -> t

  (** Least upper bound of two environment extensions. *)
  val meet : T.typing_environment -> t -> t -> t

  (** Greatest lower bound of two environment extensions. *)
  val join : T.join_env -> t -> t -> t

  val restrict_to_definitions : t -> t

  val restrict_names_to_those_occurring_in_types
     : t
    -> T.typing_environment
    -> T.typing_environment
    -> T.flambda_type list
    -> t

  (** [diff t env] computes the environment extension whose bindings are
      those in [t], when interpreted in the context of [env], that:
        - do not occur in [env]; or
        - do occur in [env] but where [t] contains a more precise type.
  *)
  val diff : t -> T.typing_environment -> t

  (** Apply the given name permutation throughout the given environment
      extension. *)
  val apply_name_permutation : t -> Name_permutation.t -> t
end
