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

  val print_ty_naked_number
     : Format.formatter
    -> 'a ty_naked_number
    -> unit

  val force_to_kind_naked_immediate
     : t
    -> Immediate.Set.t of_kind_naked_number ty

  val force_to_kind_naked_float
     : t
    -> Numbers.Float_by_bit_pattern.Set.t of_kind_naked_number ty

  val force_to_kind_naked_int32
     : t
    -> Numbers.Int32.Set.t of_kind_naked_number ty

  val force_to_kind_naked_int64
     : t
    -> Numbers.Int64.Set.t of_kind_naked_number ty

  val force_to_kind_naked_nativeint
     : t
    -> Targetint.Set.t of_kind_naked_number ty
end) (Make_meet_and_join : functor
    (S : sig
      include Meet_and_join_spec_intf.S
        with type flambda_type := T.flambda_type
        with type typing_environment := T.typing_environment
        with type env_extension := T.env_extension
        with type 'a ty := 'a T.ty
     end)
  -> sig
       include Meet_and_join_intf.S
         with type of_kind_foo := S.of_kind_foo
         with type typing_environment := T.typing_environment
         with type env_extension := T.env_extension
         with type 'a ty := 'a T.ty
     end) (Meet_and_join : sig
       include Meet_and_join_intf.S_for_types
         with type typing_environment := T.typing_environment
         with type env_extension := T.env_extension
         with type flambda_type := T.flambda_type
     end) (Typing_env0 : sig
       include Typing_env0_intf.S
         with type typing_environment := T.typing_environment
         with type env_extension := T.env_extension
         with type flambda_type := T.flambda_type
         with type t_in_context := T.t_in_context
         with type 'a ty := 'a T.ty
         with type 'a unknown_or_join := 'a T.unknown_or_join
     end) (Typing_env_extension : sig
       include Typing_env_extension_intf.S
         with type env_extension := T.env_extension
         with type typing_environment := T.typing_environment
         with type flambda_type := T.flambda_type
     end)
 : sig
  module Naked_immediate : sig
    include Meet_and_join_intf.S
      with type of_kind_foo := Immediate.Set.t T.of_kind_naked_number
      with type typing_environment := T.typing_environment
      with type env_extension := T.env_extension
      with type 'a ty := 'a T.ty
  end

  module Naked_float : sig
    include Meet_and_join_intf.S
      with type of_kind_foo :=
        Numbers.Float_by_bit_pattern.Set.t T.of_kind_naked_number
      with type typing_environment := T.typing_environment
      with type env_extension := T.env_extension
      with type 'a ty := 'a T.ty
  end

  module Naked_int32 : sig
    include Meet_and_join_intf.S
      with type of_kind_foo := Numbers.Int32.Set.t T.of_kind_naked_number
      with type typing_environment := T.typing_environment
      with type env_extension := T.env_extension
      with type 'a ty := 'a T.ty
  end

  module Naked_int64 : sig
    include Meet_and_join_intf.S
      with type of_kind_foo := Numbers.Int64.Set.t T.of_kind_naked_number
      with type typing_environment := T.typing_environment
      with type env_extension := T.env_extension
      with type 'a ty := 'a T.ty
  end

  module Naked_nativeint : sig
    include Meet_and_join_intf.S
      with type of_kind_foo := Targetint.Set.t T.of_kind_naked_number
      with type typing_environment := T.typing_environment
      with type env_extension := T.env_extension
      with type 'a ty := 'a T.ty
  end
end
