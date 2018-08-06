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

(** The common dependencies of individual typing/ files together with the
    recursive links between them. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type S = sig
  module rec Blocks : (Blocks_intf.S
    with module Flambda_type0_core := Flambda_type0_core
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Both_meet_and_join : (Both_meet_and_join_intf.S
    with module Flambda_type0_core := Flambda_type0_core
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Closure_elements : (Closure_elements_intf.S
    with module Flambda_type0_core := Flambda_type0_core
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Closure_ids : (Closure_ids_intf.S
    with module Flambda_type0_core := Flambda_type0_core
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env := Typing_env)
  and Closures_entry_by_closure_id : (Closures_entry_by_closure_id_intf.S
    with module Flambda_type0_core := Flambda_type0_core
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Discriminants : (Trivial_row_like_intf.S_applied
    with module Flambda_type0_core := Flambda_type0_core
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension
    with module Thing_without_names := Discriminant)
  and Expr : Expr_intf.S
  and Flambda_type0_core : (Flambda_type0_core_intf.S
    with module Both_meet_and_join := Both_meet_and_join
    with module Closure_elements := Closure_elements
    with module Expr := Expr
    with module Flambda_types := Flambda_types
    with module Function_type := Function_type
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Type_free_names := Type_free_names
    with module Type_printers := Type_printers
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Flambda_types : (Flambda_types_intf.S
    with module Blocks := Blocks
    with module Closure_elements := Closure_elements
    with module Closure_ids := Closure_ids
    with module Closures_entry_by_closure_id := Closures_entry_by_closure_id
    with module Discriminants := Discriminants
    with module Expr := Expr
    with module Function_type := Function_type
    with module Immediates := Immediates
    with module Types_by_closure_id := Types_by_closure_id)
  and Function_type : (Function_type_intf.S
    with module Flambda_type0_core := Flambda_type0_core
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Immediates : (Trivial_row_like_intf.S_applied
    with module Flambda_type0_core := Flambda_type0_core
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension
    with module Thing_without_names := Immediate)
  and Join_env : (Join_env_intf.S
    with module Flambda_type0_core := Flambda_type0_core
    with module Meet_env := Meet_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Make_meet_or_join : (Make_meet_or_join_intf.S
    with module Flambda_types := Flambda_types
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Meet_and_join : (Meet_and_join_intf.S
    with module Flambda_types := Flambda_types
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Meet_and_join_value : (Meet_and_join_value_intf.S
    with module Flambda_types := Flambda_types
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Meet_and_join_naked_immediate : (Meet_and_join_naked_immediate_intf.S
    with module Flambda_types := Flambda_types
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Meet_and_join_naked_int32 : (Meet_and_join_naked_int32_intf.S
    with module Flambda_types := Flambda_types
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Meet_and_join_naked_int64 : (Meet_and_join_naked_int64_intf.S
    with module Flambda_types := Flambda_types
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Meet_and_join_naked_nativeint : (Meet_and_join_naked_nativeint_intf.S
    with module Flambda_types := Flambda_types
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Meet_and_join_naked_float : (Meet_and_join_naked_float_intf.S
    with module Flambda_types := Flambda_types
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Meet_and_join_fabricated : (Meet_and_join_fabricated_intf.S
    with module Flambda_types := Flambda_types
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Meet_env : (Meet_env_intf.S
    with module Typing_env := Typing_env)
  and Parameters : (Parameters_intf.S
    with module Flambda_type0_core := Flambda_type0_core
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env := Typing_env)
  and Relational_product : (Relational_product_intf.S
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Row_like : (Row_like_intf.S
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Type_equality : (Type_equality_intf.S
    with module Flambda_types := Flambda_types)
  and Type_free_names : (Type_free_names_intf.S
    with module Flambda_types := Flambda_types)
  and Type_printers : (Type_printers_intf.S
    with module Flambda_types := Flambda_types)
  and Types_by_closure_id : (Types_by_closure_id_intf.S
    with module Flambda_type0_core := Flambda_type0_core
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Typing_env : (Typing_env_intf.S
    with module Flambda_type0_core := Flambda_type0_core
    with module Typing_env := Typing_env
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env_extension := Typing_env_extension)
  and Typing_env_extension : (Typing_env_extension_intf.S
    with module Flambda_type0_core := Flambda_type0_core
    with module Typing_env := Typing_env
    with module Join_env := Join_env
    with module Meet_env := Meet_env)
end
