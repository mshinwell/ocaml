(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Outer_namespace = struct
  module Blocks = Blocks
  module Both_meet_and_join = Both_meet_and_join
  module Closure_elements = Closure_elements
  module Closure_ids = Closure_ids
  module Closures_entry_by_closures_id = Closures_entry_by_closures_id
  module Discriminants = Discriminants
  module Either_meet_or_join = Either_meet_or_join
  module Flambda_type0_core = Flambda_type0_core
  module Function_type = Function_type
  module Immediates = Immediates
  module Join_env = Join_env
  module Meet_and_join_value = Meet_and_join_value
  module Meet_and_join_naked_number = Meet_and_join_naked_number
  module Meet_and_join_fabricated = Meet_and_join_fabricated
  module Relational_product = Relational_product
  module Row_like = Row_like
  module Types_by_closure_id = Types_by_closure_id
  module Typing_env = Typing_env
  module Typing_env_extension = Typing_env_extension
  module Type_equality = Type_equality
  module Type_free_names = Type_free_names
  module Type_printers = Type_printers
end

module Make (Expr : Expr_intf.S) = struct
  module rec Blocks0
    : Blocks_intf.S

    = Outer_namespace.Blocks.Make (W)
  and Both_meet_and_join0
    : Both_meet_and_join_intf.S
        with module Flambda_type0_core := Flambda_type0_core0
        with module Join_env := Join_env0
        with module Type_equality := Type_equality0
        with module Typing_env := Typing_env0
        with module Typing_env_extension := Typing_env_extension0
    = Outer_namespace.Both_meet_and_join.Make (W)
  and Closure_elements0
    : Closure_elements_intf.S

    = Outer_namespace.Closure_elements.Make (W)
  and Closure_ids0
    : Closure_ids_intf.S

    = Outer_namespace.Closure_ids.Make (W)
  and Closures_entry_by_closure_id0
    : Closures_entry_by_closure_id_intf.S

    = Outer_namespace.Closures_entry_by_closure_id.Make (W)
  and Discriminants0
    : Trivial_row_like_intf.S
        with module Thing_without_names := Discriminant

    = Outer_namespace.Discriminants.Make (W)
  and Either_meet_or_join0
    : Either_meet_or_join_intf.S

    = Outer_namespace.Either_meet_or_join.Make (W)
  and Flambda_type0_core0
    : (Flambda_type0_core_intf.S
        with module Both_meet_and_join := Both_meet_and_join0
        with module Flambda_types := Flambda_types0
        with module Join_env := Join_env0
        with module Meet_env := Meet_env0
        with module Type_free_names := Type_free_names0
        with module Type_printers := Type_printers0
        with module Typing_env := Typing_env0
        with module Typing_env_extension := Typing_env_extension0)
    = Outer_namespace.Flambda_type0_core.Make (W)
  and Flambda_types0
    : (Flambda_types_intf.S
        with module Blocks := Blocks0
        with module Closure_elements := Closure_elements0
        with module Closure_ids := Closure_ids0
        with module Closures_entry_by_closure_id :=
          Closures_entry_by_closure_id0
        with module Discriminants := Discriminants0
        with module Expr := Expr0
        with module Function_type := Function_type0
        with module Immediates := Immediates0
        with module Types_by_closure_id := Types_by_closure_id0)
    = Flambda_types0
  and Function_type0
    : Function_type_intf.S

    = Outer_namespace.Function_type0.Make (W)
  and Immediates0
    : Trivial_row_like_intf.S
        with module Thing_without_names := Immediate

    = Outer_namespace.Immediates.Make (W)
  and Join_env0
    : Join_env_intf.S
        with module Flambda_type0_core := Flambda_type0_core0
        with module Typing_env := Typing_env0
        with module Typing_env_extension := Typing_env_extension0
    = Outer_namespace.Join_env.Make (W)
  and Meet_and_join_value0
    : Meet_and_join_intf.S
        with module Flambda_type0_core := Flambda_type0_core0
        with type of_kind_foo = Flambda_type0_core0.of_kind_value
    = Outer_namespace.Meet_and_join_value.Make (W)
  and Meet_and_join_naked_number : sig
      module Naked_immediate :
        Meet_and_join_intf.S
          with module Flambda_type0_core := Flambda_type0_core0
          with type of_kind_foo =
            Immediate.Set.t Flambda_type0_core0.of_kind_naked_number
      module Naked_float :
        Meet_and_join_intf.S
          with module Flambda_type0_core := Flambda_type0_core0
          with type of_kind_foo =
            Numbers.Float_by_bit_pattern.Set.t
              Flambda_type0_core0.of_kind_naked_number
      module Naked_int32 :
        Meet_and_join_intf.S
          with module Flambda_type0_core := Flambda_type0_core0
          with type of_kind_foo =
            Numbers.Int32.Set.t Flambda_type0_core0.of_kind_naked_number
      module Naked_int64 :
        Meet_and_join_intf.S
          with module Flambda_type0_core := Flambda_type0_core0
          with type of_kind_foo =
            Numbers.Int64.Set.t Flambda_type0_core0.of_kind_naked_number
      module Naked_nativeint :
        Meet_and_join_intf.S
          with module Flambda_type0_core := Flambda_type0_core0
          with type of_kind_foo =
            Targetint.Set.t Flambda_type0_core0.of_kind_naked_number
    end
    = Outer_namespace.Meet_and_join_naked_number.Make (W)
  and Meet_and_join_fabricated0
    : Meet_and_join_intf.S
        with module Flambda_type0_core := Flambda_type0_core0
        with type of_kind_foo = Flambda_type0_core0.of_kind_fabricated
    = Outer_namespace.Meet_and_join_fabricated.Make (W)
  and Relational_product0
    : Relational_product_intf.S
        with module Join_env := Join_env0
        with module Typing_env := Typing_env0
        with module Typing_env_extension := Typing_env_extension0
    = Outer_namespace.Relational_product.Make (W)
  and Row_like0
    : Row_like_intf.S
        with module Join_env := Join_env0
        with module Typing_env := Typing_env0
        with module Typing_env_extension := Typing_env_extension0
    = Outer_namespace.Row_like.Make (W)
  and Typing_env0
    : Typing_env_intf.S
        with module Flambda_type0_core := Flambda_type0_core0
        with module Typing_env_extension := Typing_env_extension0
    = Outer_namespace.Typing_env.Make (W)
  and Typing_env_extension0
    : Typing_env_extension_intf.S
        with module Flambda_type0_core := Flambda_type0_core0
        with module Typing_env := Typing_env0
    = Outer_namespace.Typing_env_extension.Make (W)
  and Type_equality0
    : Type_equality_intf.S
        with module Flambda_type0_core := Flambda_type0_core0
        with module Typing_env := Typing_env0
        with module Typing_env_extension := Typing_env_extension0
    = Outer_namespace.Type_equality.Make (W)
  and Type_free_names0
    : Type_free_names_intf.S
        with module Flambda_type0_core := Flambda_type0_core0
        with module Typing_env := Typing_env0
        with module Typing_env_extension := Typing_env_extension0
    = Outer_namespace.Type_free_names.Make (W)
  and Type_printers0
    : Type_printers_intf.S
        with module Flambda_type0_core := Flambda_type0_core0
        with module Typing_env := Typing_env0
        with module Typing_env_extension := Typing_env_extension0
    = Outer_namespace.Type_printers.Make (W)
  and W : sig
    include Typing_world.S
      with module Blocks = Blocks0
      with module Closure_elements = Closure_elements0
      with module Closure_ids = Closure_ids0
      with module Closures_entry_by_closure_id = Closures_entry_by_closures_id0
      with module Discriminants = Discriminants
      with module Flambda_type0_core = Flambda_type0_core0
      with module Function_type = Function_type
      with module Immediates = Immediates
      with module Join_env = Join_env0
      with module Relational_product = Relational_product0
      with module Row_like = Row_like0
      with module Typing_env = Typing_env0
      with module Typing_env_extension = Typing_env_extension0
      with module Type_equality = Type_equality0
      with module Type_free_names = Type_free_names0
      with module Type_printers = Type_printers0
  end = struct
    module rec Blocks
      : module type of struct include Blocks0 end
      = Blocks0
    and Closure_elements
      : module type of struct include Closure_elements0 end
      = Closure_elements0
    and Closure_ids
      : module type of struct include Closure_ids0 end
      = Closure_ids0
    and Closures_entry_by_closure_id
      : module type of struct include Closures_entry_by_closure_id0 end
      = Closures_entry_by_closure_id0
    and Discriminants
      : module type of struct include Discriminants0 end
      = Discriminants0
    and Flambda_type0_core
      : module type of struct include Flambda_type0_core0 end
      = Flambda_type0_core0
    and Flambda_types
      : module type of struct include Flambda_types0 end
      = Flambda_types0
    and Function_type
      : module type of struct include Function_type0 end
      = Function_type0
    and Immediates
      : module type of struct include Immediates0 end
      = Immediates0
    and Join_env
      : module type of struct include Join_env0 end
      = Join_env0
    and Meet_and_join_value
      : module type of struct include Meet_and_join_value0 end
      = Meet_and_join_value0
    and Meet_and_join_naked_number
      : module type of struct include Meet_and_join_naked_number0 end
      = Meet_and_join_naked_number0
    and Meet_and_join_fabricated
      : module type of struct include Meet_and_join_fabricated0 end
      = Meet_and_join_fabricated0
    and Relational_product
      : module type of struct include Relational_product0 end
      = Relational_product0
    and Row_like
      : module type of struct include Row_like0 end
      = Row_like0
    and Types_by_closure_id
      : module type of struct include Types_by_closure_id0 end
      = Types_by_closure_id0
    and Typing_env
      : module type of struct include Typing_env0 end
      = Typing_env0
    and Typing_env_extension
      : module type of struct include Typing_env_extension0 end
      = Typing_env_extension0
    and Type_equality
      : module type of struct include Type_equality0 end
      = Type_equality0
    and Type_free_names
      : module type of struct include Type_free_names0 end
      = Type_free_names0
    and Type_printers
      : module type of struct include Type_printers0 end
      = Type_printers0
  end

  module Blocks = Blocks0
  module Both_meet_and_join = Both_meet_and_join0
  module Closure_elements = Closure_elements0
  module Closure_ids = Closure_ids0
  module Closures_entry_by_closures_id = Closures_entry_by_closures_id0
  module Discriminants = Discriminants0
  module Either_meet_or_join = Either_meet_or_join0
  module Flambda_type0_core = Flambda_type0_core0
  module Flambda_types = Flambda_types0
  module Function_type = Function_type0
  module Immediates = Immediates0
  module Join_env = Join_env0
  module Meet_and_join_value = Meet_and_join_value0
  module Meet_and_join_naked_number = Meet_and_join_naked_number0
  module Meet_and_join_fabricated = Meet_and_join_fabricated0
  module Relational_product = Relational_product0
  module Row_like = Row_like0
  module Types_by_closure_id = Types_by_closure_id0
  module Typing_env = Typing_env0
  module Typing_env_extension = Typing_env_extension0
  module Type_equality = Type_equality0
  module Type_free_names = Type_free_names0
  module Type_printers = Type_printers0

  include Flambda_type0_core

  let meet = Both_meet_and_join.meet
  let join = Both_meet_and_join.join
  let as_or_more_precise = Both_meet_and_join.as_or_more_precise
  let strictly_more_precise = Both_meet_and_join.strictly_more_precise
  let fast_equal = Type_equality.fast_equal
  let equal = Type_equality.equal
end
