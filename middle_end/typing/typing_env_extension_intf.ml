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
  type env_extension
  type typing_environment
  type flambda_type

  type t = env_extension

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
     : equal_type:(flambda_type -> flambda_type -> bool)
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
    -> flambda_type
    -> t

  (** Add an equation (on a name that is either existentially bound in the
      given extension, or free in the extension) after all other entries in
      the given extension. *)
  val add_equation
     : t
    -> Name.t
    -> flambda_type
    -> t

  val add_cse
     : t
    -> Name.t
    -> Flambda_primitive.With_fixed_value.t
    -> t

  (** Least upper bound of two environment extensions in the given typing
      environment. *)
  val meet : typing_environment -> t -> t -> t

  (* CR mshinwell: This signature is confusing -- labels? *)
  (** Greatest lower bound of two environment extensions in the given typing
      environment.  [join env t1' t2' t1 t2] will take the join of [t1] and
      [t2] whilst pushing [t1'] and [t2'] down through the structure of
      the resulting joined types. *)
  val join
     : typing_environment
    -> t
    -> t
    -> t
    -> t
    -> t

  val restrict_to_definitions : t -> t

  val restrict_names_to_those_occurring_in_types
     : t
    -> typing_environment
    -> flambda_type list
    -> t

  val rename_variables_not_occurring_in_domain
     : t
    -> Variable.t Variable.Map.t
    -> t

  (** [diff t env] computes the environment extension whose bindings are
      those in [t], when interpreted in the context of [env], that:
        - do not occur in [env]; or
        - do occur in [env] but where [t] contains a more precise type.
  *)
  val diff : env_extension -> typing_environment -> env_extension
end
