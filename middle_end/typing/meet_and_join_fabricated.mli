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

module Make (T : sig
  include Flambda_type0_internal_intf.S

  val print_ty_fabricated
     : Format.formatter
    -> ty_fabricated
    -> unit

  val is_obviously_bottom : flambda_type -> bool

  val ty_is_obviously_bottom : 'a ty -> bool

  val force_to_kind_fabricated : t -> of_kind_fabricated ty

  val bottom_as_ty_fabricated : unit -> of_kind_fabricated ty

  val bottom_as_ty_value : unit -> of_kind_value ty

  val any_fabricated_as_ty_fabricated : unit -> of_kind_fabricated ty

  val any_value_as_ty_value : unit -> of_kind_value ty
end) (Make_meet_and_join : functor
    (S : sig
      include Meet_and_join_spec_intf.S
        with type flambda_type := T.flambda_type
        with type env_extension := T.env_extension
        with type 'a ty := 'a T.ty
     end)
  -> sig
       include Meet_and_join_intf.S
         with type of_kind_foo := S.of_kind_foo
         with type env_extension := T.env_extension
         with type join_env := T.join_env
         with type 'a ty := 'a T.ty
    end) (Meet_and_join_value : sig
      include Meet_and_join_intf.S
        with type of_kind_foo := T.of_kind_value
        with type env_extension := T.env_extension
        with type join_env := T.join_env
        with type 'a ty := 'a T.ty
    end) (Meet_and_join : sig
      include Meet_and_join_intf.S_for_types
        with type typing_environment := T.typing_environment
        with type env_extension := T.env_extension
        with type join_env := T.join_env
        with type flambda_type := T.flambda_type
    end)
  (Typing_env : Typing_env_intf.S with module T := T)
    (Typing_env_extension : sig
      include Typing_env_extension_intf.S
        with type env_extension := T.env_extension
        with type typing_environment := T.typing_environment
        with type flambda_type := T.flambda_type
    end) (E : sig
      include Either_meet_or_join_intf.S
    end)
: sig
  include Meet_and_join_intf.S
    with type of_kind_foo := T.of_kind_fabricated
    with type env_extension := T.env_extension
    with type join_env := T.join_env
    with type 'a ty := 'a T.ty
end
