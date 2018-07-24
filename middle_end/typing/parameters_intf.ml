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

(* CR mshinwell: Not sure this file is needed. *)

module type S = sig
  module type External_var_sig = sig
    type t

    include Map.With_set with type t := t
    include Contains_names.S with type t := t

    val kind : t -> Flambda_kind.t
  end

  module type Make_structure = functor
    (External_var : External_var_sig)
    ->
      sig
        type t

        val print : Format.formatter -> t -> unit
        val fold : ('a -> External_var.t -> 'a) -> 'a -> t -> 'a
        val to_set : t -> External_var.Set.t

        val meet : t -> t -> t Or_bottom.t
        val join : t -> t -> t Or_unknown.t
      end

  module Make
      (External_var : External_var_sig)
      (Make_structure : functor
        External_var_sig with type t = External_var.t
        ->
        sig
          type t

          val print : Format.formatter -> t -> unit
          val fold : ('a -> External_var.t -> 'a) -> 'a -> t -> 'a
          val to_set : t -> External_var.Set.t

          val meet : t -> t -> t Or_bottom.t
          val join : t -> t -> t Or_unknown.t
        end)
      (T : Flambda_type0_internal_intf.S)
      (Typing_env : Typing_env_intf.S with module T := T)
      (Typing_env_extension : Typing_env_extension_intf.S with module T := T)
      (Meet_and_join : Meet_and_join_intf.S_both with module T := T)
      (Join_env : Join_env_intf.S with module T := T) :
  sig
    module EVS = Make_structure (External_var)
    (** The algebraic structure on external variables. *)

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

    (** Add or meet more equations into the environment extension associated
        with the given parameters. *)
    val add_or_meet_equations
       : t
      -> T.typing_environment
      -> T.env_extension
      -> t

    (** The environment extension associated with the given parameters,
        including at the start, definitions of such parameters to bottom (hence
        the name "standalone"). *)
    val standalone_extension : t -> T.env_extension

    (** Add or meet the definitions and equations from the given parameters
        value into the given typing environment. *)
    val introduce : t -> T.typing_environment -> T.typing_environment
  end

  module List_structure : Make_structure
  module Set_structure : Make_structure
end
