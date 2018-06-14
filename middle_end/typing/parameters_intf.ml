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

(** The representation of an abstraction that binds a number of ordered
    parameters along with their kinds.  Equations may be supplied upon the
    parameters together with existential definitions.
*)

module type S = sig
  module T : sig
    type env_extension
    type typing_environment
    type join_env
    type flambda_type
    type parameters
  end

  type t = T.parameters

  (** Perform invariant checks upon the given parameters value. *)
  val invariant : t -> unit

  (** Format the given parameters value as an s-expression. *)
  val print : Format.formatter -> t -> unit

  (** As for [print], but uses a printing cache, and prints nothing if the
      supplied parameters value contains no parameters. *)
  val print_or_omit_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> t
    -> unit

  (** A parameters value, with bottom types for the parameters, using the
      given names and kinds (in the given order). *)
  val create : Kinded_parameter.t list -> t

  (** Like [create] but also accepts equations about the parameters (which
      may of course involve existentially-bound names in the supplied
      extension). *)
  val create_with_env_extension
     : Kinded_parameter.t list
    -> T.env_extension
    -> t

  (** A parameters value, with fresh names for the parameters, assigning the
      given types (in the given order) to such parameters. *)
  val create_from_types : T.flambda_type list -> t

  (** A conservative approximation to equality. *)
  val equal : t -> t -> bool

  (** The kinds of the parameters, in order. *)
  val arity : t -> Flambda_arity.t

  type fresh_name_semantics =
    | Fresh
      (** [meet] and [join] will generate fresh names and add equalities to
          make them equal to the names in the [t]s as appropriate.  These
          fresh names will be assigned to the [kinded_params] in the output
          of [meet] and [join]. *)
    | Left
      (** [meet] and [join] will use the names in the left-hand [t] instead
          of generating fresh names.  This means that the results of these
          functions will produce values of type [t] whose [kinded_params]
          correspond to those names. *)
    | Right
      (** As for [left], but uses names from the right-hand [t]. *)

  (** Greatest lower bound of two parameter bindings. *)
  val meet
     : ?fresh_name_semantics:fresh_name_semantics
    -> T.typing_environment
    -> t
    -> t
    -> t

  (** Least upper bound of two parameter bindings. *)
  val join
     : ?fresh_name_semantics:fresh_name_semantics
    -> T.join_env
    -> t
    -> t
    -> t

  (** As for [meet] with [Fresh] semantics, but without the optional argument,
      to avoid warning 48. *)
  val meet_fresh : T.typing_environment -> t -> t -> t

  (** Like [meet_fresh] but for [join]. *)
  val join_fresh : T.join_env -> t -> t -> t

  (** Add or meet more equations into the environment extension associated with
      the given parameters. *)
  val add_or_meet_equations : t -> T.typing_environment -> T.env_extension -> t

  (** All free names occurring in the given parameters value. *)
  val free_names : t -> Name_occurrences.t

  (** All bound names (that is to say, the kinded parameters) occurring in
      the given parameters value. *)
  val bound_names : t -> Name_occurrences.t

  (** The environment extension associated with the given parameters, including
      at the start, definitions of such parameters to bottom (hence the
      name "standalone"). *)
  val standalone_extension : t -> T.env_extension

  (** Add or meet the definitions and equations from the given parameters value
      into the given typing environment. *)
  val introduce : t -> T.typing_environment -> t
end
