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
    with module Flambda_type := Flambda_type
    with module Join_env := Join_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Both_meet_and_join : (Both_meet_and_join_intf.S
    with module Flambda_type0_core := Flambda_type0_core
    with module Join_env := Join_env
    with module Type_equality := Type_equality
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Closure_elements : (Closure_elements_intf.S
    with module Index := Var_within_closure.Set
    with module Component := Logical_variable)
  and Closure_ids : (Closure_ids_intf.S
    with module Tag := Unit
    with module Index := Closure_id.Set
    with module Maps_to := Flambda_type.Ty_value)
  and Closures_entry_by_closure_id : (Closures_entry_by_closure_id_intf.S
    with module Tag := Unit
    with module Index := Closure_id
    with module Maps_to := Flambda_type.Closures_entry)
  and Discriminants : (Trivial_row_like_intf.S
    with module Flambda_type := Flambda_type
    with module Join_env := Join_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension
    with module Thing_without_names := Discriminant)
  and Flambda_type0_core : (Flambda_type0_core_intf.S
    with module Blocks := Blocks
    with module Closure_elements := Closure_elements
    with module Closure_ids := Closure_ids
    with module Closures_entry_by_closure_id := Closures_entry_by_closure_id
    with module Discriminants := Discriminants
    with module Expr := Expr
    with module Function_type := Function_type
    with module Immediates := Immediates
    with module Join_env := Join_env
    with module Parameters := Parameters
    with module Types_by_closure_id := Types_by_closure_id
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Function_type : (Function_type_intf.S
    with module Flambda_type := Flambda_type
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Immediates : (Trivial_row_like_intf.S
    with module Flambda_type := Flambda_type
    with module Join_env := Join_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension
    with module Thing_without_names := Immediate)
  and Join_env : (Join_env_intf.S
    with module Flambda_type := Flambda_type
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Relational_product : (Relational_product_intf.S
    with module Join_env := Join_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Row_like : (Row_like_intf.S
    with module Join_env := Join_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Types_by_closure_id : (Types_by_closure_id_intf.S
    with module Flambda_type := Flambda_type
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Typing_env : (Typing_env_intf.S
    with module Flambda_type := Flambda_type)
  and Typing_env_extension : (Typing_env_extension_intf.S
    with module Flambda_type := Flambda_type)
  and Type_equality : (Type_equality_intf.S
    with module Flambda_type := Flambda_type
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Type_free_names : (Type_free_names_intf.S
    with module Flambda_type := Flambda_type
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
  and Type_printers : (Type_printers_intf.S
    with module Flambda_type := Flambda_type
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension)
end
