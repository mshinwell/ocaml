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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type Name_like = sig
  type t

  include Map.With_set with type t := t
  include Contains_names.S with type t := t
end

type fresh_component_semantics =
  | Fresh
    (** For [meet] and [join] always assign fresh components. *)
  | Left
    (** For [meet] and [join] use component names from the left-hand
        value of type [t] passed in.  (During [join], if a component does
        not occur on the left-hand side, a fresh component will be
        created. *)
  | Right
    (** Analogous to [Left]. *)

module type S = sig
  module Join_env : sig type t end
  module Typing_env : sig type t end
  module Typing_env_extension : sig type t end

  module Index : Name_like
  module Component : sig
    include Name_like
    val create : Flambda_kind.t -> t
    val equal : t -> t -> bool
    val name : t -> Name.t
    val kind : t -> Flambda_kind.t
  end

  type t

  include Contains_names.S with type t := t

  (** Perform invariant checks upon the given relational product. *)
  val invariant : t -> unit

  (** Format the given relational product value as an s-expression. *)
  val print : Format.formatter -> t -> unit

  (** Create a relational product value given:
      - the indexes (with associated components) for each of the indexed
        products;
      - the equations that hold between the components in each of the indexed
        products. *)
  val create : (Component.t Index.Map.t * Typing_env_extension.t) list -> t

  (** A conservative approximation to equality. *)
  val equal : t -> t -> bool

  (** The components of the relational product.  Each list of components
      is ordered according to [Index.compare]. *)
  val components : t -> Component.t list list

  (** Greatest lower bound of two parameter bindings. *)
  val meet
     : Typing_env.t
    -> Name_permutation.t
    -> Name_permutation.t
    -> fresh_component_semantics
    -> t
    -> t
    -> t Or_bottom.t * Typing_env_extension.t

  (** Least upper bound of two parameter bindings. *)
  val join
     : Join_env.t
    -> Name_permutation.t
    -> Name_permutation.t
    -> fresh_component_semantics
    -> t
    -> t
    -> t

  (** The environment extension associated with the given relational product,
      including at the start, definitions of each component to bottom
      (hence the name "standalone"). *)
  val standalone_extension : t -> Typing_env.t -> Typing_env_extension.t

  (** Add or meet the definitions and equations from the given relational
      product value into the given typing environment. *)
  val introduce : t -> Typing_env.t -> Typing_env.t

  (** Add or meet the given equations into the environment extension held
      within the relational product. *)
  val add_or_meet_equations
     : t
    -> Typing_env.t
    -> Typing_env_extension.t
    -> t
end
