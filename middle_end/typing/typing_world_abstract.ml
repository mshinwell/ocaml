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

module type Flambda_types = sig
  type 'a or_alias
  type t
  type ty_value
  type 'a ty_naked_number
  type ty_fabricated
  type 'a ty
  type 'a unknown_or_join
  type of_kind_value
  type blocks_and_tagged_immediates
  type 'a of_kind_value_boxed_number
  type inlinable_function_declaration
  type function_declaration
  type closures_entry
  type closures
  type 'a of_kind_naked_number
  type of_kind_naked_immediate
  type of_kind_naked_float
  type of_kind_naked_int32
  type of_kind_naked_int64
  type of_kind_naked_nativeint
  type of_kind_fabricated
  type set_of_closures_entry
  type set_of_closures
end

module type Typing_env = sig
  type t
  type binding_type
  type typing_environment_entry0
  type typing_environment_entry
  type levels_to_entries
end

module type S = sig
  module Blocks : sig type t end
  module Closure_elements : sig type t end
  module Closure_ids : sig type t end
  module Closures_entry_by_closure_id : sig type t end
  module Discriminants : sig type t end
  module Expr : sig type t end
  module Flambda_types : Flambda_types
  module Function_type : sig type t end
  module Immediates : sig type t end
  module Join_env : sig type t end
  module Meet_env : sig type t end
  module Parameters : sig type t end
  module Types_by_closure_id : sig type t end
  module Typing_env : Typing_env
  module Typing_env_extension : sig type t end
end

module type Functor_S = sig
  module Relational_product : sig
    module Make_types (Index : sig end) (Component : sig end)
      : sig type t end
  end

  module Row_like : sig
    module Make_types (Tag : sig end) (Index : sig end) (Maps_to : sig end)
      : sig type t end
  end

  module Trivial_row_like : sig
    module Make_types (Thing_without_names : sig end)
      : sig type t end
  end
end
