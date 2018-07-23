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

(* CR mshinwell: This needs a more appropriate name than "Parameters". *)

(** The representation of an abstraction that existentially binds a number of
    logical variables whilst at the same time holding equations upon such
    variables.

    The external view of this structure is determined by the caller.  This is
    done by providing a type of "external variables", which will be in
    bijection with the logical variables, and an algebraic structure upon
    them that provides a container (typically lists or sets).  External
    variables are treated as bound names; they must be maintained fresh by
    the caller.
*)

module type S = sig
  module T : sig
    type env_extension
    type typing_environment
    type join_env
    type flambda_type
  end

  module EVS : sig
    (** The algebraic structure on external variables. *)
    type t
  end

  type t

  include Contains_names.S with type t := t

  (** Perform invariant checks upon the given parameters value. *)
  val invariant : t -> unit

  (** Format the given parameters value as an s-expression. *)
  val print : Format.formatter -> t -> unit

  (** Create a parameters value given the external structure. *)
  val create : EVS.t -> t

  (** Like [create] but also accepts equations on the logical variables. *)
  val create_with_env_extension : EVS.t -> T.env_extension -> t

  (** A conservative approximation to equality. *)
  val equal : t -> t -> bool

  (** The external variables in the appropriate algebraic structure. *)
  val external_structure : t -> EVS.t

  (** Greatest lower bound of two parameter bindings. *)
  val meet
     : T.typing_environment
    -> t
    -> t
    -> t Or_bottom.t

  (** Least upper bound of two parameter bindings. *)
  val join
     : T.join_env
    -> t
    -> t
    -> t Or_unknown.t

  (** Add or meet more equations into the environment extension associated with
      the given parameters. *)
  val add_or_meet_equations : t -> T.typing_environment -> T.env_extension -> t

  (** The environment extension associated with the given parameters, including
      at the start, definitions of such parameters to bottom (hence the
      name "standalone"). *)
  val standalone_extension : t -> T.env_extension

  (** Add or meet the definitions and equations from the given parameters value
      into the given typing environment. *)
  val introduce : t -> T.typing_environment -> T.typing_environment
end
