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

module type Types_nonrec = sig
  module Abstract_types : Typing_world_abstract.S
  module Abstract_functor_types : Typing_world_abstract.Functor_S

  module Blocks : Blocks_intf.S_types
    with module T := Abstract_types
    and module Functor_T := Abstract_functor_types
  module Closure_elements : Closure_elements_intf.S_types
    with module T := Abstract_types
  module Closure_ids : Closure_ids_intf.S_types
    with module T := Abstract_types
  module Closures_entry_by_closure_id :
    Closures_entry_by_closure_id_intf.S_types
      with module T := Abstract_types
  module Discriminants : Trivial_row_like_intf.S_types
    with module T := Abstract_types
  module Expr : Expr_intf.S
    with module T := Abstract_types
  module Flambda_types : Flambda_types_intf.S
    with module T := Abstract_types
  module Function_type : Function_type_intf.S_types
    with module T := Abstract_types
  module Immediates : Trivial_row_like_intf.S_types
    with module T := Abstract_types
  module Join_env : Join_env_intf.S_types
    with module T := Abstract_types
  module Meet_env : Meet_env_intf.S_types
    with module T := Abstract_types
  module Parameters : Parameters_intf.S_types
    with module T := Abstract_types
  module Types_by_closure_id : Types_by_closure_id_intf.S_types
    with module T := Abstract_types
  module Typing_env : Typing_env_intf.S_types
    with module T := Abstract_types
  module Typing_env_extension : Typing_env_extension_intf.S_types
    with module T := Abstract_types
end

module type Functor_types_nonrec = sig
  module Abstract_types : Typing_world_abstract.S
  (** Direct dependencies between the functors are not allowed (hence the
      absence of a module satisfying [Abstract_functor_types]). *)

  module Relational_product : Relational_product_intf.S_types
    with module T := Abstract_types
  module Row_like : Row_like_intf.S_types
    with module T := Abstract_types
end

module type S = sig
  module rec Types : (
    Types_nonrec
      with module Abstract_types := Nonrec_types
      and module Abstract_functor_types := Nonrec_functor_types)
  and Functor_types :
    Functor_types_nonrec
      with module Abstract_types := Nonrec_types
end
