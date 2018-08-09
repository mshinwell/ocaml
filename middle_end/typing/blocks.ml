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

(* CR mshinwell: Delete >= 4.08 *)
[@@@ocaml.warning "-60"]
module Flambda_type0_core = struct end
module Join_env = struct end
module Meet_env = struct end
module Relational_product = struct end
module Row_like = struct end
module Typing_env = struct end
module Typing_env_extension = struct end

module Make_types
  (T : Typing_world_abstract.S)
  (Functor_T : Typing_world_abstract.Functor_S) =
struct
  open! T
  open! Functor_T

  module RP = struct
    include Relational_product.Make_types
      (Int_index) (Logical_variable_component)
  end

  module RL = Row_like.Make_types (Tag) (Targetint.OCaml) (RP)

  type t = RL.t
end

module type Strengthened_world = sig
  module Recursive_world : sig
    module rec Types : (Typing_world_types.Types_nonrec
      with module Abstract_types := Types
      and module Abstract_functor_types := Functor_types
      with module Blocks = Make_types (Types) (Functor_types))
    and Functor_types : Typing_world_types.Functor_types_nonrec
      with module Abstract_types := Types
  end
  include Typing_world.S with module Recursive_world := Recursive_world
end

module Make (W : Strengthened_world)
  (F : (Typing_world.Functor_S
    with Recursive_world = Recursive_world))
struct
  open! W

  include Recursive_world.Types.Blocks

  module RP = struct
    include Relational_product.Make (Int_index) (Logical_variable_component)

    let bottom () = create_bottom ~arity:1
  end

  module RL = Row_like.Make (Tag) (Targetint.OCaml) (RP)

  type t = RL.t

  type open_or_closed = Open | Closed of Tag.t

  let create ~field_tys open_or_closed : t =
    (* CR mshinwell: This code is very similar to some in [Function_type]. *)
    let indexes_to_vars =
      Targetint.OCaml.Map.of_list (
        List.mapi (fun index _field_ty ->
            let index = Targetint.OCaml.of_int index in
            let logical_var = Logical_variable.create (Flambda_kind.value ()) in
            index, logical_var)
          field_tys)
    in
    let env_extension, _index =
      List.fold_left (fun (env_extension, index) field_ty ->
          let logical_var = Targetint.OCaml.Map.find index indexes_to_vars in
          let env_extension =
            Typing_env_extension.add_equation env_extension
              (Name.logical_var logical_var) field_ty
          in
          let next_index = Targetint.OCaml.add index Targetint.OCaml.one in
          env_extension, next_index)
        (Typing_env_extension.empty, Targetint.OCaml.zero)
        field_tys
    in
    let product =
      RP.create [
        indexes_to_vars, env_extension;
      ]
    in
    let size = Targetint.OCaml.of_int (List.length field_tys) in
    match open_or_closed with
    | Open -> RL.create_at_least size product
    | Closed tag -> RL.create_exactly tag size product

  let create_bottom () =
    create ~field_tys:[] (Closed Tag.arbitrary)

  let invariant _t = () (* CR mshinwell: RL.invariant *)
  let print_with_cache = RL.print

  let equal = RL.equal

  let meet env t1 t2 : _ Or_bottom.t =
    match RL.meet env Fresh t1 t2 with
    | Bottom -> Bottom
    | Ok (t, product) ->
      Ok (t, RP.standalone_extension product (Meet_env.env env))

  let join env t1 t2 =
    RL.join env Fresh t1 t2

  let free_names = RL.free_names
  let bound_names = RL.bound_names
  let apply_name_permutation = RL.apply_name_permutation
  let freshen = RL.freshen
end
