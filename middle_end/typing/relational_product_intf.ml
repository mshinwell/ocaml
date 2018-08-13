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

module type S_types = sig
  module T : Typing_world_abstract.S
  module Make_types (Index : Map.With_set) (Component : Map.With_set) :
    Map.With_set
end

module type S = sig
  module T : Typing_world_abstract.S
  module Functor_T : Typing_world_abstract.Functor_S
  include module type of struct include Functor_T.Relational_product end

  module Make (Index : Map.With_set) (Component : Map.With_set) : sig
    type t = Make_types (Index) (Component).t

    include Contains_names.S with type t := t

    (** Perform invariant checks upon the given relational product. *)
    val invariant : t -> unit

    (** Format the given relational product value as an s-expression. *)
    val print : Format.formatter -> t -> unit

    val print_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> t
      -> unit

    (** Create a relational product value given:
        - the indexes (with associated components) for each of the indexed
          products;
        - the equations that hold between the components in each of the
          indexed products. *)
    val create
       : (Component.t Index.Map.t * T.Typing_env_extension.t) list
      -> t

    val create_bottom : arity:int -> t

    (** A conservative approximation to equality. *)
    val equal : Type_equality_env.t -> t -> t -> bool

    (** The components of the relational product.  Each list of components
        is ordered according to [Index.compare]. *)
    val components : t -> Component.t list list

    (** Greatest lower bound of two parameter bindings. *)
    (* CR mshinwell: I'm not sure [fresh_component_semantics] is needed now
       since we shouldn't be meeting relational products that occur in terms. *)
    val meet
       : T.Meet_env.t
      -> fresh_component_semantics
      -> t
      -> t
      -> (t * T.Typing_env_extension.t) Or_bottom.t

    (** Least upper bound of two parameter bindings. *)
    val join
       : T.Join_env.t
      -> fresh_component_semantics
      -> t
      -> t
      -> t

    (** The environment extension associated with the given relational
        product, including at the start, definitions of each component to
        bottom (hence the name "standalone"). *)
    val standalone_extension
       : t
      -> T.Typing_env.t
      -> T.Typing_env_extension.t

    (** Add or meet the definitions and equations from the given relational
        product value into the given typing environment. *)
    val introduce : t -> T.Typing_env.t -> T.Typing_env.t

    (** Add or meet the given equations into the environment extension held
        within the relational product. *)
    val add_or_meet_equations
       : t
      -> T.Meet_env.t
      -> T.Typing_env_extension.t
      -> t
  end
end
