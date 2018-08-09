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

module F (Expr : Expr_intf.S) = struct
  module rec Types : sig
    module Flambda_types :
      Flambda_types_intf.S_types
        with module T := Types
        and module Functor_T := Functor_types

    include Typing_world_abstract.S
      with module Blocks = Blocks.Make_types (Types) (Functor_types)
      and module Closure_elements =
        Closure_elements.Make_types (Types) (Functor_types)
      and module Closure_ids = Closure_ids.Make_types (Types) (Functor_types)
      and module Closures_entry_by_closure_id =
        Closures_entry_by_closure_id.Make_types (Types) (Functor_types)
      and module Discriminants =
        Discriminants.Make_types (Types) (Functor_types)
      and module Expr = Expr
      and module Flambda_types := Flambda_types
      and module Function_type =
        Function_type.Make_types (Types) (Functor_types)
      and module Immediates = Immediates.Make_types (Types) (Functor_types)
      and module Join_env = Join_env.Make_types (Types) (Functor_types)
      and module Meet_env = Meet_env.Make_types (Types) (Functor_types)
      and module Parameters = Parameters.Make_types (Types) (Functor_types)
      and module Types_by_closure_id =
        Types_by_closure_id.Make_types (Types) (Functor_types)
      and module Typing_env = Typing_env.Make_types (Types) (Functor_types)
      and module Typing_env_extension =
        Typing_env_extension.Make_types (Types) (Functor_types)
  end = struct
    module Blocks = Blocks.Make_types (Types) (Functor_types)
    module Closure_elements =
      Closure_elements.Make_types (Types) (Functor_types)
    module Closure_ids = Closure_ids.Make_types (Types) (Functor_types)
    module Closures_entry_by_closure_id =
      Closures_entry_by_closure_id.Make_types (Types) (Functor_types)
    module Discriminants = Discriminants.Make_types (Types) (Functor_types)
    module Expr = Expr
    module Flambda_types = Types.Flambda_types
    module Function_type = Function_type.Make_types (Types) (Functor_types)
    module Immediates = Immediates.Make_types (Types) (Functor_types)
    module Join_env = Join_env.Make_types (Types) (Functor_types)
    module Meet_env = Meet_env.Make_types (Types) (Functor_types)
    module Parameters = Parameters.Make_types (Types) (Functor_types)
    module Types_by_closure_id =
      Types_by_closure_id.Make_types (Types) (Functor_types)
    module Typing_env = Typing_env.Make_types (Types) (Functor_types)
    module Typing_env_extension =
      Typing_env_extension.Make_types (Types) (Functor_types)
  end and Functor_types :
    Typing_world_abstract.Functor_S
      with module Relational_product = Relational_product.Make_types (Types)
      and module Row_like = Row_like.Make_types (Types)
      and module Trivial_row_like = Trivial_row_like.Make_types (Types) =
  struct
    module Relational_product = Relational_product.Make_types (Types)
    module Row_like = Row_like.Make_types (Types)
    module Trivial_row_like = Trivial_row_like.Make_types (Types)
  end

  module rec Functor_world : sig
    module Recursive_world : sig
      module Types = Types
      module Functor_types = Functor_types
    end
    include Typing_world.Functor_S
      with module Recursive_world := Recursive_world
  end = struct
    module Recursive_world = struct
      module Types = Types
      module Functor_types = Functor_types
    end
    module Make_meet_or_join = Make_meet_or_join.Make (World)
    module Meet_and_join = Meet_and_join.Make (World)
    module Meet_and_join_value = Meet_and_join_value.Make (World)
    module Meet_and_join_naked_immediate
      = Meet_and_join_naked_immediate.Make (World)
    module Meet_and_join_naked_int32 = Meet_and_join_naked_int32.Make (World)
    module Meet_and_join_naked_int64 = Meet_and_join_naked_int64.Make (World)
    module Meet_and_join_naked_nativeint
      = Meet_and_join_naked_nativeint.Make (World)
    module Meet_and_join_naked_float = Meet_and_join_naked_float.Make (World)
    module Meet_and_join_fabricated = Meet_and_join_fabricated.Make (World)
    module Relational_product = Relational_product.Make (World)
    module Row_like = Row_like.Make (World)
    module Trivial_row_like = Trivial_row_like.Make (World)
  end
  and World : sig
    module Recursive_world : sig
      module Types = Types
      module Functor_types = Functor_types
    end
    include Typing_world.S with module Recursive_world := Recursive_world
  end = struct
    module Recursive_world = struct
      module Types = Types
      module Functor_types = Functor_types
    end
    module Blocks = Blocks.Make (World) (Functor_world)
    module Both_meet_and_join =
      Both_meet_and_join.Make (World) (Functor_world)
    module Closure_elements = Closure_elements.Make (World) (Functor_world)
    module Closure_ids = Closure_ids.Make (World) (Functor_world)
    module Closures_entry_by_closure_id =
      Closures_entry_by_closure_id.Make (World) (Functor_world)
    module Discriminants = Discriminants.Make (World) (Functor_world)
    module Either_meet_or_join =
      Either_meet_or_join.Make (World) (Functor_world)
    module Expr = Expr
    module Flambda_type0_core = Flambda_type0_core.Make (World) (Functor_world)
    module Flambda_types = Recursive_world.Types.Flambda_types
    module Function_type = Function_type.Make (World) (Functor_world)
    module Immediates = Immediates.Make (World) (Functor_world)
    module Join_env = Join_env.Make (World) (Functor_world)
    module Meet_env = Meet_env.Make (World) (Functor_world)
    module Parameters = Parameters.Make (World) (Functor_world)
    module Type_equality = Type_equality.Make (World) (Functor_world)
    module Type_free_names = Type_free_names.Make (World) (Functor_world)
    module Type_printers = Type_printers.Make (World) (Functor_world)
    module Types_by_closure_id =
      Types_by_closure_id.Make (World) (Functor_world)
    module Typing_env = Typing_env.Make (World) (Functor_world)
    module Typing_env_extension
      = Typing_env_extension.Make (World) (Functor_world)
  end

  module Blocks = World.Blocks
  module Closure_elements = World.Closure_elements
  module Closure_ids = World.Closure_ids
  module Closures_entry_by_closure_id = World.Closures_entry_by_closure_id
  module Discriminants = World.Discriminants
  module Function_type = World.Function_type
  module Immediates = World.Immediates
  module Join_env = World.Join_env
  module Meet_env = World.Meet_env
  module Parameters = World.Parameters
  module Types_by_closure_id = World.Types_by_closure_id
  module Typing_env = World.Typing_env
  module Typing_env_extension = World.Typing_env_extension

  include World.Flambda_types
  include World.Flambda_type0_core

  let meet = World.Both_meet_and_join.meet
  let join = World.Both_meet_and_join.join

  let as_or_more_precise = World.Both_meet_and_join.as_or_more_precise
  let strictly_more_precise = World.Both_meet_and_join.strictly_more_precise

  let fast_equal = World.Type_equality.fast_equal
  let equal = World.Type_equality.equal

  let print = World.Type_printers.print
  let print_with_cache = World.Type_printers.print_with_cache

  let free_names = World.Type_free_names.free_names
end
