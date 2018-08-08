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

module type Functor_S = sig
  module Typing_world : Typing_world_types.S

  module Make_meet_or_join : Make_meet_or_join_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
  module Meet_and_join : Meet_and_join_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
  module Relational_product : Relational_product_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
  module Row_like : Row_like_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
end

module type S = sig
  module Typing_world : Typing_world_types.S

  module Blocks : Blocks_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
  module Both_meet_and_join : Both_meet_and_join_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
  module Closure_elements : Closure_elements_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
  module Closure_ids : Closure_ids_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
  module Closures_entry_by_closure_id : Closures_entry_by_closure_id_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
  module Discriminants : Discriminants_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
  module Expr : Expr_intf.S
  module Flambda_type0_core : Flambda_type0_core_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
  module Flambda_types :
    module type of struct include Typing_world.Flambda_types end
      with module T := Typing_world.Types
      and module Functor_T := Typing_world.Functor_types
  module Function_type : Function_type_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
  module Immediates : Immediates_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
  module Join_env : Join_env_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
  module Meet_and_join_value : Meet_and_join_value_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
  module Meet_and_join_naked_immediate : Meet_and_join_naked_immediate_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
  module Meet_and_join_naked_int32 : Meet_and_join_naked_int32_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
  module Meet_and_join_naked_int64 : Meet_and_join_naked_int64_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
  module Meet_and_join_naked_nativeint : Meet_and_join_naked_nativeint_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
  module Meet_and_join_naked_float : Meet_and_join_naked_float_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
  module Meet_and_join_fabricated : Meet_and_join_fabricated_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
  module Meet_env : Meet_env_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
  module Parameters : Parameters_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
  module Type_equality : Type_equality_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
  module Type_free_names : Type_free_names_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
  module Type_printers : Type_printers_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
  module Types_by_closure_id : Types_by_closure_id_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
  module Typing_env : Typing_env_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
  module Typing_env_extension : Typing_env_extension_intf.S
    with module T := Typing_world.Types
    and module Functor_T := Typing_world.Functor_types
end
