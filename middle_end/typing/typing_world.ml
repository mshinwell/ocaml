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
  module rec Flambda_type : Flambda_type0_core_intf.S
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
  and Blocks : Blocks_intf.S
    with module Flambda_type := T.Flambda_type
    with module Join_env := T.Join_env
    with module Typing_env := T.Typing_env
    with module Typing_env_extension := T.Typing_env_extension
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
  and Immediates : Trivial_row_like_intf.S
    with module Flambda_type := T.Flambda_type
    with module Join_env := T.Join_env
    with module Typing_env := T.Typing_env
    with module Typing_env_extension := T.Typing_env_extension
    with module Thing_without_names := Immediate
  and Discriminants : Trivial_row_like_intf.S
    with module Flambda_type := T.Flambda_type
    with module Join_env := T.Join_env
    with module Typing_env := T.Typing_env
    with module Typing_env_extension := T.Typing_env_extension
    with module Thing_without_names := Discriminant
end
