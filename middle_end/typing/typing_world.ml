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

(** The common dependencies of individual typing/ files. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type S = sig
  module rec Flambda_type : Flambda_type0_internal_intf.S
    with module Blocks := Blocks
    with module Discriminants := Discriminants
    with module Expr := Expr
    with module Function_parameters := Function_parameters
    with module Immediates := Immediates
    with module Join_env := Join_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension
  and Typing_env : Typing_env_intf.S
    with module Flambda_type := Flambda_type
  and Typing_env_extension : Typing_env_extension_intf.S
    with module Flambda_type := Flambda_type
  and Join_env : Join_env_intf.S
    with module Flambda_type := Flambda_type
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension
  and Function_parameters : Relational_product_intf.S
    with module Index := Targetint.OCaml
    with module Component := Logical_variable
  and Continuation_parameters : Relational_product_intf.S
    with module Index := Targetint.OCaml
    with module Component := Logical_variable
  and Block_fields : Relational_product_intf.S
    with module Index := Targetint.OCaml
    with module Component := Logical_variable
  and Blocks : Row_like_intf.S
    with module Tag := Tag
    with module Index := Targetint.OCaml
    with module Maps_to := Block_fields
  and Closure_elements : Relational_product_intf.S
    with module Index := Var_within_closure.Set
    with module Component := Logical_variable
  and Closure_ids_with_elements : Row_like_intf.S
    with module Tag := Closure_id
    with module Index := Var_within_closure.Set
    with module Maps_to := Flambda_type.closure
  and Closure_ids : Row_like_intf.S
    with module Tag := Unit
    with module Index := Closure_id.Set
    with module Maps_to := Flambda_type.ty_value
  and Immediates_relational_product : Relational_product_intf.S
    with module Index := Targetint.OCaml.Set
    with module Component := Logical_variable
  and Immediates : Row_like_intf.S
    with module Tag := Unit
    with module Index := Targetint.OCaml.Set
    with module Maps_to := Immediates_relational_product
  and Discriminants_relational_product : Relational_product_intf.S
    with module Index := Discriminant.Set
    with module Component := Logical_variable
  and Discriminants : Row_like_intf.S
    with module Tag := Unit
    with module Index := Discriminant.Set
    with module Maps_to := Discriminants_relational_product
end
