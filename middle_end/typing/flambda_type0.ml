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
  module Flambda_type0_core = Flambda_type0_core
  module Join_env = Join_env
  module Meet_and_join_value = Meet_and_join_value
  module Meet_and_join_naked_number = Meet_and_join_naked_number
  module Meet_and_join_fabricated = Meet_and_join_fabricated
  module Relational_product = Relational_product
  module Row_like = Row_like
  module Typing_env = Typing_env
  module Typing_env_extension = Typing_env_extension
  module Type_equality = Type_equality
  module Type_free_names = Type_free_names
  module Type_printers = Type_printers
end

module Make (Expr : Expr_intf.S) = struct
  module rec Flambda_type0_core0
    : Flambda_type0_core_intf.S
        with module Typing_env := Typing_env0
        with module Typing_env_extension := Typing_env_extension0
    = Outer_namespace.Flambda_type0_core.Make (T)
  and Join_env0
    : Join_env_intf.S
        with module Typing_env := Typing_env0
        with module Typing_env_extension := Typing_env_extension0
    = Outer_namespace.Join_env.Make (T)
  and ...
  and Relational_product0
    : Relational_product_intf.S
        with module Join_env := Join_env0
        with module Typing_env := Typing_env0
        with module Typing_env_extension := Typing_env_extension0
    = Outer_namespace.Relational_product.Make (T)
  and Row_like0
    : Row_like_intf.S
        with module Join_env := Join_env0
        with module Typing_env := Typing_env0
        with module Typing_env_extension := Typing_env_extension0
    = Outer_namespace.Row_like.Make (T)
  and Typing_env0
    : Typing_env_intf.S
        with module Flambda_type0_core := Flambda_type0_core0
        with module Typing_env_extension := Typing_env_extension0
    = Outer_namespace.Typing_env.Make (T)
  and Typing_env_extension0
    : Typing_env_extension_intf.S
        with module Flambda_type0_core := Flambda_type0_core0
        with module Typing_env := Typing_env0
    = Outer_namespace.Typing_env_extension.Make (T)
  and Type_equality0
    : Type_equality_intf.S
        with module Flambda_type0_core := Flambda_type0_core0
        with module Typing_env := Typing_env0
        with module Typing_env_extension := Typing_env_extension0
    = Outer_namespace.Type_equality.Make (T)
  and Type_free_names0
    : Type_free_names_intf.S
        with module Flambda_type0_core := Flambda_type0_core0
        with module Typing_env := Typing_env0
        with module Typing_env_extension := Typing_env_extension0
    = Outer_namespace.Type_free_names.Make (T)
  and Type_printers0
    : Type_printers_intf.S
        with module Flambda_type0_core := Flambda_type0_core0
        with module Typing_env := Typing_env0
        with module Typing_env_extension := Typing_env_extension0
    = Outer_namespace.Type_printers.Make (T)
  and T : sig
    include Typing_world.S
      with module Flambda_type0_core = Flambda_type0_core0
      with module Join_env = Join_env0
      with module Relational_product = Relational_product0
      with module Row_like = Row_like0
      with module Typing_env = Typing_env0
      with module Typing_env_extension = Typing_env_extension0
      with module Type_equality = Type_equality0
      with module Type_free_names = Type_free_names0
      with module Type_printers = Type_printers0
  end = struct
    module rec Flambda_type0_core
      : module type of struct include Flambda_type0_core0 end
      = Flambda_type0_core0
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

  module Flambda_type0_core = Flambda_type0_core0
  module Join_env = Join_env0
  module Meet_and_join_value = Meet_and_join_value0
  module Meet_and_join_naked_number = Meet_and_join_naked_number0
  module Meet_and_join_fabricated = Meet_and_join_fabricated0
  module Relational_product = Relational_product0
  module Row_like = Row_like0
  module Typing_env = Typing_env0
  module Typing_env_extension = Typing_env_extension0
  module Type_equality = Type_equality0
  module Type_free_names = Type_free_names0
  module Type_printers = Type_printers0
end

(*
  module rec T : Flambda_type0_internal_intf.S
    with type expr := Expr.t
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension
    with module Join_env := Join_env
    with module Parameters := Parameters
    with module Blocks := Blocks
    with module Closure_elements := Closure_elements
  = struct
    include Flambda_type0_internal_intf.S_impl (Expr) (Typing_env)
      (Typing_env_extension) (Join_env) (Parameters) (Blocks)
      (Closure_elements)

    end and Meet_and_join_value :
      Meet_and_join_intf.S
        with module T := T
        with type of_kind_foo = of_kind_value
      = Outer_namespace.Meet_and_join_value.Make (T1)
          (Make_meet_and_join) (Meet_and_join_naked_immediate)
          (Meet_and_join_naked_float) (Meet_and_join_naked_int32)
          (Meet_and_join_naked_int64) (Meet_and_join_naked_nativeint)
          (Meet_and_join_fabricated) (Both_meet_and_join)
          (Typing_env) (Typing_env_extension) (Join_env) (Parameters) (E)
    and Meet_and_join_naked_number : sig
      (* CR mshinwell: Deal with this signature somehow *)
      module Naked_immediate :
        Meet_and_join_intf.S
          with module T := T
          with type of_kind_foo = Immediate.Set.t of_kind_naked_number
      module Naked_float :
        Meet_and_join_intf.S
          with module T := T
          with type of_kind_foo =
            Numbers.Float_by_bit_pattern.Set.t of_kind_naked_number
      module Naked_int32 :
        Meet_and_join_intf.S
          with module T := T
          with type of_kind_foo = Numbers.Int32.Set.t of_kind_naked_number
      module Naked_int64 :
        Meet_and_join_intf.S
          with module T := T
          with type of_kind_foo = Numbers.Int64.Set.t of_kind_naked_number
      module Naked_nativeint :
        Meet_and_join_intf.S
          with module T := T
          with type of_kind_foo = Targetint.Set.t of_kind_naked_number
    end = Outer_namespace.Meet_and_join_naked_number.Make
      (T1) (Make_meet_and_join) (Typing_env) (Typing_env_extension) (E)
    and Meet_and_join_naked_immediate :
      Meet_and_join_intf.S
        with module T := T
        with type of_kind_foo = Immediate.Set.t of_kind_naked_number
      = Meet_and_join_naked_number.Naked_immediate
    and Meet_and_join_naked_float :
      (* CR mshinwell: See if we can abstract these naked number cases some
         more? *)
      Meet_and_join_intf.S
        with module T := T
        with type of_kind_foo = Float_by_bit_pattern.Set.t of_kind_naked_number
      = Meet_and_join_naked_number.Naked_float
    and Meet_and_join_naked_int32 :
      Meet_and_join_intf.S
        with module T := T
        with type of_kind_foo = Int32.Set.t of_kind_naked_number
      = Meet_and_join_naked_number.Naked_int32
    and Meet_and_join_naked_int64 :
      Meet_and_join_intf.S
        with module T := T
        with type of_kind_foo = Int64.Set.t of_kind_naked_number
      = Meet_and_join_naked_number.Naked_int64
    and Meet_and_join_naked_nativeint :
      Meet_and_join_intf.S
        with module T := T
        with type of_kind_foo = Targetint.Set.t of_kind_naked_number
      = Meet_and_join_naked_number.Naked_nativeint
    and Meet_and_join_fabricated :
      Meet_and_join_intf.S
        with module T := T
        with type of_kind_foo = of_kind_fabricated
      = Outer_namespace.Meet_and_join_fabricated.Make
          (T1) (Make_meet_and_join) (Meet_and_join_value) (Both_meet_and_join)
          (Typing_env) (Typing_env_extension) (Join_env) (E)
  end and Meet : sig
    module Meet_and_join : Meet_and_join_intf.S_for_types with module T := T
  end = Make_meet_or_join (For_meet)
  and Join : sig
    module Meet_and_join : Meet_and_join_intf.S_for_types with module T := T
  end = Make_meet_or_join (For_join)
  and Both_meet_and_join : Meet_and_join_intf.S_both with module T := T
    = struct
      module T = T

      let meet env t1 t2 =
        Meet.Meet_and_join.meet_or_join (Join_env.create env) t1 t2

      let join env t1 t2 =
        let join_ty, _env_extension =
          Join.Meet_and_join.meet_or_join env t1 t2
        in
        join_ty

      let as_or_more_precise env t1 ~than:t2 =
        if Type_equality.fast_equal t1 t2 then true
        else
          let meet_t, _env_extension = meet env t1 t2 in
          Type_equality.equal meet_t t1

      let strictly_more_precise env t1 ~than:t2 =
        if Type_equality.fast_equal t1 t2 then false
        else
          let meet_t, _env_extension = meet env t1 t2 in
          Type_equality.equal meet_t t1
            && not (Type_equality.equal meet_t t2)
    end
*)
