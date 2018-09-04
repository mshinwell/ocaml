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

(* CR mshinwell: Remove when warning 60 fixed *)
[@@@ocaml.warning "-60"]

module Float = Numbers.Float_by_bit_pattern
module Int32 = Numbers.Int32
module Int64 = Numbers.Int64

module K = Flambda_kind

(* CR mshinwell: Should there be a different [Name_occurrences] used for
   types?  It would remove most of this "everything_must_only_be_names"
   stuff. *)

module type Either_meet_or_join_intf = sig
  module Join_env : sig type t end
  module Meet_env : sig type t end
  module Typing_env_extension : sig type t end

  val name : unit -> string

  type meet_or_join = private Meet | Join

  val op : unit -> meet_or_join

  val unknown_is_identity : unit -> bool
  val unknown_is_absorbing : unit -> bool

  module String_info : sig
    module Set : sig
      type t = String_info.Set.t

      val union_or_inter : t -> t -> t
    end
  end

  module Immediate : sig
    module Set : sig
      type t = Immediate.Set.t

      val union_or_inter : t -> t -> t
    end

    module Map : sig
      type 'a t = 'a Immediate.Map.t

      val union_or_inter
         : (Immediate.t -> 'a -> 'a -> 'a option)
        -> 'a t
        -> 'a t
        -> 'a t
    end
  end

  module Float : sig
    module Set : sig
      type t = Numbers.Float_by_bit_pattern.Set.t

      val union_or_inter : t -> t -> t
    end
  end

  module Int32 : sig
    module Set : sig
      type t = Numbers.Int32.Set.t

      val union_or_inter : t -> t -> t
    end
  end

  module Int64 : sig
    module Set : sig
      type t = Numbers.Int64.Set.t

      val union_or_inter : t -> t -> t
    end
  end

  module Targetint : sig
    module Set : sig
      type t = Targetint.Set.t

      val union_or_inter : t -> t -> t
    end

    module OCaml : sig
      module Map : sig
        type 'a t = 'a Targetint.OCaml.Map.t

        val union_or_inter_both
          : in_left_only:('a -> 'a)
         -> in_right_only:('a -> 'a)
         -> in_both:(Targetint.OCaml.t -> 'a -> 'a -> 'a option)
         -> 'a t
         -> 'a t
         -> 'a t
      end
    end
  end

  module Closure_id : sig
    module Map : sig
      type 'a t = 'a Closure_id.Map.t

      val union_or_inter
         : (Closure_id.t -> 'a -> 'a -> 'a option)
        -> 'a t
        -> 'a t
        -> 'a t

      (** When this operation is "intersection" it also copies through from
          the inputs [t1] and [t2] those bindings in [t1] that do not occur
          in [t2]. *)
      val union_or_inter_and_left
         : (Closure_id.t -> 'a -> 'a -> 'a option)
        -> 'a t
        -> 'a t
        -> 'a t
    end
  end

  module Var_within_closure : sig
    module Map : sig
      type 'a t = 'a Var_within_closure.Map.t

      val union_or_inter
         : (Var_within_closure.t -> 'a -> 'a -> 'a option)
        -> 'a t
        -> 'a t
        -> 'a t

      (** As for [Closure_id.union_or_inter_and_left], above. *)
      val union_or_inter_and_left
         : (Var_within_closure.t -> 'a -> 'a -> 'a option)
        -> 'a t
        -> 'a t
        -> 'a t
    end
  end

  module Tag : sig
    module Map : sig
      type 'a t = 'a Tag.Map.t

      val union_or_inter_both
        : in_left_only:('a -> 'a)
       -> in_right_only:('a -> 'a)
       -> in_both:(Tag.t -> 'a -> 'a -> 'a option)
       -> 'a t
       -> 'a t
       -> 'a t
    end
  end

  module Discriminant : sig
    module Map : sig
      type 'a t = 'a Discriminant.Map.t

      val union_or_inter_both
        : in_left_only:('a -> 'a)
       -> in_right_only:('a -> 'a)
       -> in_both:(Discriminant.t -> 'a -> 'a -> 'a option)
       -> 'a t
       -> 'a t
       -> 'a t
    end
  end

  val switch_no_bottom
     : (Meet_env.t
      -> 'a
      -> 'a
      -> 'a * Typing_env_extension.t)
    -> (Join_env.t
      -> 'a
      -> 'a
      -> 'a)
    -> Join_env.t
    -> 'a
    -> 'a
    -> 'a * Typing_env_extension.t

  val switch
     : (Meet_env.t
      -> 'a
      -> 'a
      -> ('a * Typing_env_extension.t) Or_bottom.t)
    -> (Join_env.t
      -> 'a
      -> 'a
      -> 'a)
    -> Join_env.t
    -> 'a
    -> 'a
    -> ('a * Typing_env_extension.t) Or_bottom.t

  val switch'
     : (Meet_env.t
      -> 'a
      -> 'a
      -> ('a * Typing_env_extension.t) Or_bottom.t)
    -> (Join_env.t
      -> 'a
      -> 'a
      -> 'a)
    -> Join_env.t
    -> 'a
    -> 'a
    -> 'a Or_bottom.t

  val switch'_with_param
     : (Meet_env.t
      -> 'b
      -> 'a
      -> 'a
      -> ('a * Typing_env_extension.t) Or_bottom.t)
    -> (Join_env.t
      -> 'b
      -> 'a
      -> 'a
      -> 'a)
    -> Join_env.t
    -> 'b
    -> 'a
    -> 'a
    -> 'a Or_bottom.t
end

module type Meet_and_join_spec_intf = sig
  module Flambda_types : sig
    type t
    type 'a ty
  end

  module Join_env : sig type t end
  module Typing_env_extension : sig type t end

  type of_kind_foo

  val kind : unit -> Flambda_kind.t

  val to_type : of_kind_foo Flambda_types.ty -> Flambda_types.t

  val force_to_kind : Flambda_types.t -> of_kind_foo Flambda_types.ty

  (* CR mshinwell: Rename to [print_ty_with_cache]. *)
  val print_ty
     : cache:Printing_cache.t
    -> Format.formatter
    -> of_kind_foo Flambda_types.ty
    -> unit

  val meet_or_join_of_kind_foo
     : Join_env.t
    -> of_kind_foo
    -> of_kind_foo
    -> (of_kind_foo * Typing_env_extension.t) Or_absorbing.t
end

module type Meet_and_join_naked_number_intf = sig
  module Flambda_types : sig
    type t
    type 'a ty
    type 'a of_kind_naked_number
  end

  module Join_env : sig type t end
  module Meet_env : sig type t end

  module Naked_number : sig
    type t
    module Set : Set.S with type elt = t
  end

  module Typing_env_extension : sig type t end

  module Make
    (E : Either_meet_or_join_intf
      with module Join_env := Join_env
      with module Meet_env := Meet_env
      with module Typing_env_extension := Typing_env_extension) :
  sig
    include Meet_and_join_spec_intf
      with module Flambda_types := Flambda_types
      with module Join_env := Join_env
      with module Typing_env_extension := Typing_env_extension
      with type of_kind_foo =
        Naked_number.Set.t Flambda_types.of_kind_naked_number
  end
end

module Make (Expr : Expr_intf.S) = struct
  module rec Blocks : sig
    type t

    type open_or_closed = Open | Closed of Tag.t

    (** Create a value which describes that there are exactly no blocks. *)
    val create_bottom : unit -> t

    val create : field_tys:Flambda_types.t list -> open_or_closed -> t

(*
    val invariant : t -> unit
*)

    val print_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> t
      -> unit

    val equal : Type_equality_env.t -> t -> t -> bool

    val meet
       : Meet_env.t
      -> t
      -> t
      -> (t * Typing_env_extension.t) Or_bottom.t

    val join
       : Join_env.t
      -> t
      -> t
      -> t

    val is_empty : t -> bool

    include Contains_names.S with type t := t
  end = struct
    module RP = struct
      include Relational_product.Make
        (Int_index) (Logical_variable_component)

      let bottom () = create_bottom ~arity:1
    end

    module Tag_and_targetint_ocaml =
      Hashtbl.Make_with_map_pair (Tag) (Targetint.OCaml)

    module RL =
      Row_like.Make (Tag) (Targetint.OCaml) (Tag_and_targetint_ocaml) (RP)

    type t = RL.t

    type open_or_closed = Open | Closed of Tag.t

    let create ~field_tys open_or_closed : t =
      (* CR mshinwell: This code is very similar to some in [Function_type]. *)
      let indexes_to_vars =
        Targetint.OCaml.Map.of_list (
          List.mapi (fun index _field_ty ->
              let index = Targetint.OCaml.of_int index in
              let logical_var =
                Logical_variable.create (Flambda_kind.value ())
              in
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
          (Typing_env_extension.empty (), Targetint.OCaml.zero)
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

    let _invariant _t = () (* CR mshinwell: RL.invariant *)
    let print_with_cache = RL.print

    let equal = RL.equal
    let is_empty = RL.is_bottom

    let meet env t1 t2 : _ Or_bottom.t =
      match RL.meet env Fresh t1 t2 with
      | Bottom -> Bottom
      | Ok (t, product) ->
        Ok (t, RP.standalone_extension product (Meet_env.env env))

    let join env t1 t2 =
      RL.join env Fresh t1 t2

    let free_names = RL.free_names
    let apply_name_permutation = RL.apply_name_permutation
  end and Both_meet_and_join : sig
    val meet
       : Meet_env.t
      -> Flambda_types.t
      -> Flambda_types.t
      -> Flambda_types.t * Typing_env_extension.t

    val join
       : Join_env.t
      -> Flambda_types.t
      -> Flambda_types.t
      -> Flambda_types.t

    val meet_closures_entry
       : Meet_env.t
      -> Flambda_types.closures_entry
      -> Flambda_types.closures_entry
      -> (Flambda_types.closures_entry * Typing_env_extension.t) Or_bottom.t

    val join_closures_entry
       : Join_env.t
      -> Flambda_types.closures_entry
      -> Flambda_types.closures_entry
      -> Flambda_types.closures_entry

    val meet_set_of_closures_entry
       : Meet_env.t
      -> Flambda_types.set_of_closures_entry
      -> Flambda_types.set_of_closures_entry
      -> (Flambda_types.set_of_closures_entry * Typing_env_extension.t)
           Or_bottom.t

    val join_set_of_closures_entry
       : Join_env.t
      -> Flambda_types.set_of_closures_entry
      -> Flambda_types.set_of_closures_entry
      -> Flambda_types.set_of_closures_entry

    val as_or_more_precise
       : Typing_env.t
      -> Flambda_types.t
      -> than:Flambda_types.t
      -> bool

    val strictly_more_precise
       : Typing_env.t
      -> Flambda_types.t
      -> than:Flambda_types.t
      -> bool
  end = struct
    module Meet = Meet_and_join.Make (Either_meet_or_join.For_meet)
    module Join = Meet_and_join.Make (Either_meet_or_join.For_join)

    let meet env t1 t2 =
      Meet.meet_or_join (Join_env.create env) t1 t2

    let join env t1 t2 =
      let join_ty, _env_extension = Join.meet_or_join env t1 t2 in
      join_ty

    module Meet_value = Meet_and_join_value.Make (Either_meet_or_join.For_meet)
    module Join_value = Meet_and_join_value.Make (Either_meet_or_join.For_join)

    let meet_closures_entry env entry1 entry2 : _ Or_bottom.t =
      let env = Join_env.create env in
      match Meet_value.meet_or_join_closures_entry env entry1 entry2 with
      | Ok (entry, env_extension) -> Ok (entry, env_extension)
      | Absorbing -> Bottom

    (* CR mshinwell: Sort out this nuisance Absorbing vs. Bottom thing.
       Also the fact that "join" is returning an env_extension *)
    let join_closures_entry env entry1 entry2 =
      match Join_value.meet_or_join_closures_entry env entry1 entry2 with
      | Ok (entry, _env_extension) -> entry
      | Absorbing -> assert false

    module Meet_fabricated =
      Meet_and_join_fabricated.Make (Either_meet_or_join.For_meet)
    module Join_fabricated =
      Meet_and_join_fabricated.Make (Either_meet_or_join.For_join)

    let meet_set_of_closures_entry env entry1 entry2 : _ Or_bottom.t =
      let env = Join_env.create env in
      match
        Meet_fabricated.meet_or_join_set_of_closures_entry env entry1 entry2
      with
      | Ok (entry, env_extension) -> Ok (entry, env_extension)
      | Absorbing -> Bottom

    let join_set_of_closures_entry env entry1 entry2 =
      match
        Join_fabricated.meet_or_join_set_of_closures_entry env entry1 entry2
      with
      | Ok (entry, _env_extension) -> entry
      | Absorbing -> assert false

    let as_or_more_precise env t1 ~than:t2 =
      if Type_equality.fast_equal t1 t2 then true
      else
        let env =
          (* CR mshinwell: We shouldn't have to write out these "empty
             permutation" arguments *)
          Meet_env.create env
            ~perm_left:(Name_permutation.create ())
            ~perm_right:(Name_permutation.create ())
        in
        let meet_t, _env_extension = meet env t1 t2 in
        Type_equality.equal meet_t t1

    let strictly_more_precise env t1 ~than:t2 =
      if Type_equality.fast_equal t1 t2 then false
      else
        let env =
          Meet_env.create env
            ~perm_left:(Name_permutation.create ())
            ~perm_right:(Name_permutation.create ())
        in
        let meet_t, _env_extension = meet env t1 t2 in
        Type_equality.equal meet_t t1
          && not (Type_equality.equal meet_t t2)
  end and Closure_elements : sig
    type t

    val print : cache:Printing_cache.t -> Format.formatter -> t -> unit

    val create : Flambda_types.t Var_within_closure.Map.t -> t

    val create_bottom : unit -> t

    val equal : Type_equality_env.t -> t -> t -> bool

    (** Greatest lower bound of two values of type [t]. *)
    val meet
       : Meet_env.t
      -> t
      -> t
      -> (t * Typing_env_extension.t) Or_bottom.t

    (** Least upper bound of two values of type [t]. *)
    val join
       : Join_env.t
      -> t
      -> t
      -> t

      include Contains_names.S with type t := t
  end = struct
    module Var_within_closure = struct
      include Var_within_closure

      let free_names _t = Name_occurrences.create ()

      let apply_name_permutation t _perm = t
    end

    module RP =
      Relational_product.Make (Var_within_closure)
        (Logical_variable_component)

    type t = RP.t

    let create closure_elements_to_tys =
      let closure_elements_to_logical_variables =
        Var_within_closure.Map.map (fun _ty ->
            Logical_variable.create (Flambda_kind.value ()))
          closure_elements_to_tys
      in
      let env_extension =
        Var_within_closure.Map.fold (fun var ty env_extension ->
            let logical_var =
              Var_within_closure.Map.find var
                closure_elements_to_logical_variables
            in
            Typing_env_extension.add_equation env_extension
              (Name.logical_var logical_var) ty)
          closure_elements_to_tys
          (Typing_env_extension.empty ())
      in
      RP.create [
        closure_elements_to_logical_variables, env_extension;
      ]

    let create_bottom () = RP.create_bottom ~arity:1

    let print ~cache:_ ppf t = RP.print ppf t

    let meet env t1 t2 = RP.meet env Fresh t1 t2
    let join env t1 t2 = RP.join env Fresh t1 t2

    let equal = RP.equal
    let free_names = RP.free_names
    let apply_name_permutation = RP.apply_name_permutation
  end and Closure_ids : sig
    type t

(*
    val invariant : t -> unit
*)

    type open_or_closed = Open | Closed

    val create
       : Flambda_types.set_of_closures_entry Closure_id_set.Map.t
      -> open_or_closed
      -> t

    val print : cache:Printing_cache.t -> Format.formatter -> t -> unit

    val equal : Type_equality_env.t -> t -> t -> bool

    val meet
       : Meet_env.t
      -> t
      -> t
      -> (t * Typing_env_extension.t) Or_bottom.t

    val join
       : Join_env.t
      -> t
      -> t
      -> t

    include Contains_names.S with type t := t
  end = struct
    module Unit_and_closure_id_set =
      Hashtbl.Make_with_map_pair (Unit) (Closure_id_set)

    module RL =
      Row_like.Make (Unit) (Closure_id_set) (Unit_and_closure_id_set)
        (Flambda_type0_core.Set_of_closures_entry)

    type t = RL.t

    type open_or_closed = Open | Closed

    let create closure_ids_map open_or_closed : t =
      match open_or_closed with
      | Open -> RL.create_at_least_multiple closure_ids_map
      | Closed ->
        let closure_ids_map =
          Closure_id_set.Map.fold
            (fun closure_ids set_of_closures_entry result ->
              Unit_and_closure_id_set.Map.add ((), closure_ids)
                set_of_closures_entry result)
            closure_ids_map
            Unit_and_closure_id_set.Map.empty
        in
        RL.create_exactly_multiple closure_ids_map

    let print = RL.print
    let _invariant _t = ()  (* CR mshinwell: RL.invariant *)

    let meet env t1 t2 : _ Or_bottom.t =
      match RL.meet env Fresh t1 t2 with
      | Bottom -> Bottom
      | Ok (t, _set_of_closures_entry) -> Ok (t, Typing_env_extension.empty ())

    let join env t1 t2 = RL.join env Fresh t1 t2

    let equal = RL.equal
    let free_names = RL.free_names
    let apply_name_permutation = RL.apply_name_permutation
  end and Closures_entry_by_closure_id : sig
    type t

    val print : cache:Printing_cache.t -> Format.formatter -> t -> unit

    (** Describe one or more closures by giving for each one the closure ID
        and the set of variables in the closure. *)
    val create_exactly_multiple
       : Flambda_types.closures_entry
           Closure_id_and_var_within_closure_set.Map.t
      -> t

    (** Describe one or more closures that contain at least the given closure
        variables. *)
    val create_at_least_multiple
       : Flambda_types.closures_entry Var_within_closure_set.Map.t
      -> t

    val equal : Type_equality_env.t -> t -> t -> bool

    (** Greatest lower bound of two values of type [t]. *)
    val meet
       : Meet_env.t
      -> t
      -> t
      -> (t * Typing_env_extension.t) Or_bottom.t

    (** Least upper bound of two values of type [t]. *)
    val join
       : Join_env.t
      -> t
      -> t
      -> t

    include Contains_names.S with type t := t
  end = struct
    module RL =
      Row_like.Make (Closure_id) (Var_within_closure_set)
        (Closure_id_and_var_within_closure_set)
        (Flambda_type0_core.Closures_entry)

    type t = RL.t

    let create_exactly_multiple closure_id_and_vars_within_closure_map =
      RL.create_exactly_multiple closure_id_and_vars_within_closure_map

    let create_at_least_multiple vars_within_closure_map =
      RL.create_at_least_multiple vars_within_closure_map

    let print ~cache ppf t = RL.print ~cache ppf t

    let meet env t1 t2 : _ Or_bottom.t =
      match RL.meet env Fresh t1 t2 with
      | Bottom -> Bottom
      | Ok (t, _closures_entry) -> Ok (t, Typing_env_extension.empty ())

    let join env t1 t2 = RL.join env Fresh t1 t2

    let equal = RL.equal
    let free_names = RL.free_names
    let apply_name_permutation = RL.apply_name_permutation
  end and Discriminants : sig
    type t

    val print : cache:Printing_cache.t -> Format.formatter -> t -> unit

(*
    (** Create a value which describes the presence of exactly no things. *)
    val create_bottom : unit -> t

    (** Create a value which describes the presence of an unknown set of
        things. *)
    val create_unknown : unit -> t
*)

    val create : Discriminant.Set.t -> t

    val create_with_equations
       : Typing_env_extension.t Discriminant.Map.t
      -> t

    val equal : Type_equality_env.t -> t -> t -> bool

    val meet
       : Meet_env.t
      -> t
      -> t
      -> (t * Typing_env_extension.t) Or_bottom.t

    val join
       : Join_env.t
      -> t
      -> t
      -> t

    val get_singleton : t -> (Discriminant.t * Typing_env_extension.t) option

    include Contains_names.S with type t := t
  end = struct
    include Trivial_row_like.Make (Discriminant)
  end and Either_meet_or_join : sig
    module For_meet : Either_meet_or_join_intf
      with module Join_env := Join_env
      with module Meet_env := Meet_env
      with module Typing_env_extension := Typing_env_extension
    module For_join : Either_meet_or_join_intf
      with module Join_env := Join_env
      with module Meet_env := Meet_env
      with module Typing_env_extension := Typing_env_extension
  end = struct
    module For_meet = struct
      let name () = "meet"

      type meet_or_join = Meet | Join

      let op () : meet_or_join = Meet
      let (_ : meet_or_join) = Join

      let unknown_is_identity () = true
      let unknown_is_absorbing () = false

      (* CR mshinwell: Write functors to generate these patterns *)
      module String_info = struct
        module Set = struct
          type t = String_info.Set.t
          let union_or_inter = String_info.Set.inter
        end
      end

      module Immediate = struct
        module Set = struct
          type t = Immediate.Set.t
          let union_or_inter = Immediate.Set.inter
        end

        module Map = struct
          type 'a t = 'a Immediate.Map.t

          let union_or_inter = Immediate.Map.inter
        end
      end

      module Float = struct
        module Set = struct
          type t = Numbers.Float_by_bit_pattern.Set.t
          let union_or_inter = Numbers.Float_by_bit_pattern.Set.inter
        end
      end

      module Int32 = struct
        module Set = struct
          type t = Numbers.Int32.Set.t
          let union_or_inter = Numbers.Int32.Set.inter
        end
      end

      module Int64 = struct
        module Set = struct
          type t = Numbers.Int64.Set.t
          let union_or_inter = Numbers.Int64.Set.inter
        end
      end

      module Targetint = struct
        module Set = struct
          type t = Targetint.Set.t
          let union_or_inter = Targetint.Set.inter
        end

        module OCaml = struct
          module Map = struct
            type 'a t = 'a Targetint.OCaml.Map.t

            let union_or_inter_both ~in_left_only:_ ~in_right_only:_ ~in_both
                  t1 t2 =
              Targetint.OCaml.Map.inter in_both t1 t2
          end
        end
      end

      module Closure_id = struct
        module Map = struct
          type 'a t = 'a Closure_id.Map.t

          let union_or_inter = Closure_id.Map.inter

          (* CR mshinwell: implement these *)
          let union_or_inter_and_left _f _t1 _t2 = assert false
        end
      end

      module Var_within_closure = struct
        module Map = struct
          type 'a t = 'a Var_within_closure.Map.t

          let union_or_inter = Var_within_closure.Map.inter

          let union_or_inter_and_left _f _t1 _t2 = assert false
        end
      end

      module Tag = struct
        module Map = struct
          type 'a t = 'a Tag.Map.t

          let union_or_inter_both ~in_left_only:_ ~in_right_only:_ ~in_both
                t1 t2 =
            Tag.Map.inter in_both t1 t2
        end
      end

      module Discriminant = struct
        module Map = struct
          type 'a t = 'a Discriminant.Map.t

          let union_or_inter_both ~in_left_only:_ ~in_right_only:_ ~in_both
                t1 t2 =
            Discriminant.Map.inter in_both t1 t2
        end
      end

      let switch_no_bottom meet _join join_env thing1 thing2 =
        meet (Join_env.central_environment join_env) thing1 thing2

      let switch meet _join join_env thing1 thing2 =
        meet (Join_env.central_environment join_env) thing1 thing2

      let switch' meet _join join_env thing1 thing2 : _ Or_bottom.t =
        let result : _ Or_bottom.t =
          meet (Join_env.central_environment join_env) thing1 thing2
        in
        match result with
        | Bottom -> Bottom
        | Ok (thing, _) -> Ok thing

      let switch'_with_param meet _join join_env param thing1 thing2
            : _ Or_bottom.t =
        let result : _ Or_bottom.t =
          meet (Join_env.central_environment join_env) param thing1 thing2
        in
        match result with
        | Bottom -> Bottom
        | Ok (thing, _) -> Ok thing
    end

    module For_join = struct
      let name () = "join"

      type meet_or_join = Meet | Join

      let op () : meet_or_join = Join
      let (_ : meet_or_join) = Meet

      let unknown_is_identity () = false
      let unknown_is_absorbing () = true

      module String_info = struct
        module Set = struct
          type t = String_info.Set.t
          let union_or_inter = String_info.Set.union
        end
      end

      module Immediate = struct
        module Set = struct
          type t = Immediate.Set.t
          let union_or_inter = Immediate.Set.union
        end

        module Map = struct
          type 'a t = 'a Immediate.Map.t

          let union_or_inter = Immediate.Map.union
        end
      end

      module Float = struct
        module Set = struct
          type t = Numbers.Float_by_bit_pattern.Set.t
          let union_or_inter = Numbers.Float_by_bit_pattern.Set.union
        end
      end

      module Int32 = struct
        module Set = struct
          type t = Numbers.Int32.Set.t
          let union_or_inter = Numbers.Int32.Set.union
        end
      end

      module Int64 = struct
        module Set = struct
          type t = Numbers.Int64.Set.t
          let union_or_inter = Numbers.Int64.Set.union
        end
      end

      module Targetint = struct
        module Set = struct
          type t = Targetint.Set.t
          let union_or_inter = Targetint.Set.union
        end

        module OCaml = struct
          module Map = struct
            type 'a t = 'a Targetint.OCaml.Map.t

            let union_or_inter_both = Targetint.OCaml.Map.union_both
          end
        end
      end

      module Closure_id = struct
        module Map = struct
          type 'a t = 'a Closure_id.Map.t

          let union_or_inter = Closure_id.Map.union

          let union_or_inter_and_left f t1 t2 =
            Closure_id.Map.union f t1 t2
        end
      end

      module Var_within_closure = struct
        module Map = struct
          type 'a t = 'a Var_within_closure.Map.t

          let union_or_inter = Var_within_closure.Map.union

          let union_or_inter_and_left f t1 t2 =
            Var_within_closure.Map.union f t1 t2
        end
      end

      module Tag = struct
        module Map = struct
          type 'a t = 'a Tag.Map.t

          let union_or_inter_both = Tag.Map.union_both
        end
      end

      module Discriminant = struct
        module Map = struct
          type 'a t = 'a Discriminant.Map.t

          let union_or_inter_both = Discriminant.Map.union_both
        end
      end

      let switch_no_bottom _meet join join_env thing1 thing2 =
        join join_env thing1 thing2, Typing_env_extension.empty ()

      let switch _meet join join_env thing1 thing2 : _ Or_bottom.t =
        Ok (join join_env thing1 thing2, Typing_env_extension.empty ())

      let switch' _meet join join_env thing1 thing2 : _ Or_bottom.t =
        Ok (join join_env thing1 thing2)

      let switch'_with_param _meet join join_env param thing1 thing2
            : _ Or_bottom.t =
        Ok (join join_env param thing1 thing2)
    end
  end and Flambda_type0_core : sig
    include Contains_names.S with type t := Flambda_types.t

    module Closures_entry : sig
      type t = Flambda_types.closures_entry

      val bottom : unit -> t

      val print_with_cache
         : cache:Printing_cache.t
        -> Format.formatter
        -> t
        -> unit

      val add_or_meet_equations
         : t
        -> Meet_env.t
        -> Typing_env_extension.t
        -> t

      val equal : Type_equality_env.t -> t -> t -> bool

      val meet
         : Meet_env.t
        -> Relational_product.fresh_component_semantics
        -> t
        -> t
        -> (t * Typing_env_extension.t) Or_bottom.t

      val join
         : Join_env.t
        -> Relational_product.fresh_component_semantics
        -> t
        -> t
        -> t

      include Contains_names.S with type t := t
    end

    module Set_of_closures_entry : sig
      type t = Flambda_types.set_of_closures_entry

      val bottom : unit -> t

      val print_with_cache
         : cache:Printing_cache.t
        -> Format.formatter
        -> t
        -> unit

      val add_or_meet_equations
         : t
        -> Meet_env.t
        -> Typing_env_extension.t
        -> t

      val equal : Type_equality_env.t -> t -> t -> bool

      val meet
         : Meet_env.t
        -> Relational_product.fresh_component_semantics
        -> t
        -> t
        -> (t * Typing_env_extension.t) Or_bottom.t

      val join
         : Join_env.t
        -> Relational_product.fresh_component_semantics
        -> t
        -> t
        -> t

      include Contains_names.S with type t := t
    end

    val get_alias : Flambda_types.t -> Simple.t option

    val is_obviously_bottom : Flambda_types.t -> bool

    val of_ty_value : Flambda_types.ty_value -> Flambda_types.t

    val of_ty_naked_number
       : 'kind Flambda_types.ty_naked_number
      -> 'kind Flambda_kind.Naked_number.t
      -> Flambda_types.t

    val of_ty_fabricated : Flambda_types.ty_fabricated -> Flambda_types.t

    (** Construction of top types. *)
    val unknown : Flambda_kind.t -> Flambda_types.t

    val any_value : unit -> Flambda_types.t
    val any_value_as_ty_value : unit -> Flambda_types.ty_value

    val any_fabricated : unit -> Flambda_types.t
    val any_fabricated_as_ty_fabricated : unit -> Flambda_types.ty_fabricated

    val any_tagged_immediate : unit -> Flambda_types.t
    val any_tagged_bool : unit -> Flambda_types.t

    val any_boxed_float : unit -> Flambda_types.t
    val any_boxed_int32 : unit -> Flambda_types.t
    val any_boxed_int64 : unit -> Flambda_types.t
    val any_boxed_nativeint : unit -> Flambda_types.t

    val any_naked_immediate : unit -> Flambda_types.t
    val any_naked_float : unit -> Flambda_types.t

    val any_naked_float_as_ty_naked_float
      : unit
     -> Numbers.Float_by_bit_pattern.Set.t Flambda_types.ty_naked_number

    (** The top type for unboxed 32-bit numbers. *)
    val any_naked_int32 : unit -> Flambda_types.t

    (** The top type for unboxed 64-bit numbers. *)
    val any_naked_int64 : unit -> Flambda_types.t

    (** The top type for unboxed "nativeint" numbers. *)
    val any_naked_nativeint : unit -> Flambda_types.t

    (** Building of types representing tagged / boxed values from specified
        constants. *)
    val this_tagged_immediate : Immediate.t -> Flambda_types.t
    val these_tagged_immediates : Immediate.Set.t -> Flambda_types.t
    val this_boxed_float : Numbers.Float_by_bit_pattern.t -> Flambda_types.t
    val these_boxed_floats
       : Numbers.Float_by_bit_pattern.Set.t
      -> Flambda_types.t
    val this_boxed_int32 : Int32.t -> Flambda_types.t
    val these_boxed_int32s : Numbers.Int32.Set.t -> Flambda_types.t
    val this_boxed_int64 : Int64.t -> Flambda_types.t
    val these_boxed_int64s : Numbers.Int64.Set.t -> Flambda_types.t
    val this_boxed_nativeint : Targetint.t -> Flambda_types.t
    val these_boxed_nativeints : Targetint.Set.t -> Flambda_types.t
    val this_immutable_string : string -> Flambda_types.t
    val this_immutable_float_array
       : Numbers.Float_by_bit_pattern.t array
      -> Flambda_types.t

    (** A type representing a set of tagged immediates combined with typing
        judgements that will be used if the set contains, or is subsequently
        refined to contain, only a unique element. *)
    val these_tagged_immediates_with_envs
       : Typing_env_extension.t Immediate.Map.t
      -> Flambda_types.t

    (** Building of types representing untagged / unboxed values from
        specified constants. *)
    val this_naked_immediate : Immediate.t -> Flambda_types.t
    val this_naked_float : Numbers.Float_by_bit_pattern.t -> Flambda_types.t
    val this_naked_float_as_ty_naked_float
       : Numbers.Float_by_bit_pattern.t
      -> Numbers.Float_by_bit_pattern.Set.t Flambda_types.ty_naked_number
    val these_naked_floats
       : Numbers.Float_by_bit_pattern.Set.t
      -> Flambda_types.t
    val this_naked_int32 : Int32.t -> Flambda_types.t
    val these_naked_int32s : Numbers.Int32.Set.t -> Flambda_types.t
    val this_naked_int64 : Int64.t -> Flambda_types.t
    val these_naked_int64s : Numbers.Int64.Set.t -> Flambda_types.t
    val this_naked_nativeint : Targetint.t -> Flambda_types.t
    val these_naked_nativeints : Targetint.Set.t -> Flambda_types.t

    (** Building of types corresponding to immutable values given only the
        size of such values. *)
    val immutable_string : size:Targetint.OCaml.t -> Flambda_types.t

    (** The type corresponding to a mutable string of length [size]. *)
    val mutable_string : size:Targetint.OCaml.t -> Flambda_types.t

    (** The type corresponding to a mutable float array holding [size]
        naked floats. *)
    val mutable_float_array : size:Targetint.OCaml.t -> Flambda_types.t

    (** Building of types corresponding to values that did not exist at
        source level. *)

    (** The given discriminant. *)
    val this_discriminant : Discriminant.t -> Flambda_types.t

    (** Like [this_discriminant], but returns the [ty_fabricated], rather than
        a type. *)
    val this_discriminant_as_ty_fabricated
       : Discriminant.t
      -> Flambda_types.ty_fabricated

    (** The given block discriminants coupled with the env_extensions that hold
        if the corresponding block can be shown to have one of the
        discriminants. *)
    val these_discriminants
       : Typing_env_extension.t Discriminant.Map.t
      -> Flambda_types.t

    (** Like [these_discriminants], but returns the [ty_fabricated], rather than
        a value of type [t]. *)
    val these_discriminants_as_ty_fabricated
       : Typing_env_extension.t Discriminant.Map.t
      -> Flambda_types.ty_fabricated

    (** Any discriminant. *)
    val any_discriminant_as_ty_fabricated : unit -> Flambda_types.ty_fabricated

    (** Given the type of a naked floating-point number, return the type of the
        corresponding boxed version. *)
    val box_float : Flambda_types.t -> Flambda_types.t

    (** Given the type of a naked int32 number, return the type of the
        corresponding boxed version. *)
    val box_int32 : Flambda_types.t -> Flambda_types.t

    (** Given the type of a naked int64 number, return the type of the
        corresponding boxed version. *)
    val box_int64 : Flambda_types.t -> Flambda_types.t

    (** Given the type of a naked nativeint number, return the type of the
        corresponding boxed version. *)
    val box_nativeint : Flambda_types.t -> Flambda_types.t

    (** The type of a float array containing the given floating-point
        numbers. *)
    val immutable_float_array
       : Numbers.Float_by_bit_pattern.Set.t Flambda_types.ty_naked_number array
      -> Flambda_types.t

    (** The type of a block with a known tag, size and field types. *)
    val block
       : Tag.t
      -> fields:Flambda_types.t list
      -> Flambda_types.t

    (** Like [block], except that the field types are statically known to be
        of kind [Value]). *)
    val block_of_values
       : Tag.t
      -> fields:Flambda_types.ty_value list
      -> Flambda_types.t

    (** The type of a block with a known tag and size but unknown content,
        save that the contents are all of kind [Value]. *)
    val block_of_unknown_values
       : Tag.t
      -> size:int
      -> Flambda_types.t

    (** The type of a block with at least [n] fields and an unknown tag.
        The type of the [n - 1]th field is taken to be an [Equals] to the
        given variable. *)
    val block_with_size_at_least
       : n:int
      -> field_n_minus_one:Variable.t
      -> Flambda_types.t

    (** The bottom type for the given kind ("no value can flow to this
        point"). *)
    val bottom : Flambda_kind.t -> Flambda_types.t

    (** The bottom type for kind [Value] expressed as a type whose kind is
        statically known. *)
    val bottom_as_ty_value : unit -> Flambda_types.ty_value

    (** The bottom type for kind [Fabricated] expressed as a type whose kind is
        statically known. *)
    val bottom_as_ty_fabricated : unit -> Flambda_types.ty_fabricated

    (** Create an "bottom" type with the same kind as the given type. *)
    val bottom_like : Flambda_types.t -> Flambda_types.t

    (** Create an "unknown" type with the same kind as the given type. *)
    val unknown_like : Flambda_types.t -> Flambda_types.t

    (** Create a description of a function declaration whose code is known. *)
    val create_inlinable_function_declaration
       : is_classic_mode:bool
      -> closure_origin:Closure_origin.t
      -> continuation_param:Continuation.t
      -> exn_continuation_param:Continuation.t
      -> params:Kinded_parameter.t list
      -> body:Expr.t
      -> code_id:Code_id.t
      -> result_arity:Flambda_arity.t
      -> stub:bool
      -> dbg:Debuginfo.t
      -> inline:Inline_attribute.t
      -> specialise:Specialise_attribute.t
      -> is_a_functor:bool
      -> invariant_params:Variable.Set.t lazy_t
      -> size:int option lazy_t
      -> direct_call_surrogate:Closure_id.t option
      -> my_closure:Variable.t
      -> Flambda_types.function_declaration

    (** Create a description of a function declaration whose code is unknown.
        Such declarations cannot be inlined. *)
    val create_non_inlinable_function_declaration
       : unit
      -> Flambda_types.function_declaration

    (** Create a closure type given full information about the closure. *)
    val closure
       : Closure_id.t
      -> Flambda_types.function_declaration
      -> Function_type.t
      -> Flambda_types.ty_value Var_within_closure.Map.t
      -> set_of_closures:Flambda_types.ty_fabricated
      -> Flambda_types.t

    (** The type of a closure (of kind [Value]) containing at least one
        closure that holds the given closure variable with the given type. *)
    val closure_containing_at_least
       : Var_within_closure.t
      -> Flambda_types.ty_value
      -> Flambda_types.t

    (** The type of a set of closures containing exactly those closure IDs
        with the given types. *)
    val set_of_closures
       : closures:Flambda_types.t Closure_id.Map.t
      -> Flambda_types.t

    (** The type of a set of closures containing at least one closure with
        the given closure ID. *)
    val set_of_closures_containing_at_least : Closure_id.t -> Flambda_types.t

    (** Construct a type equal to the type of the given name.  (The name
        must be present in the given environment when calling e.g. [join].) *)
    val alias_type_of : Flambda_kind.t -> Simple.t -> Flambda_types.t

    (** Like [alias_type_of], but for types of kind [Value], and returns the
        [ty] rather than a [t]. *)
    val alias_type_of_as_ty_value : Simple.t -> Flambda_types.ty_value

    (** Like [alias_type_of_as_ty_value] but for types of [Fabricated] kind. *)
    val alias_type_of_as_ty_fabricated : Simple.t -> Flambda_types.ty_fabricated

    (** The type that is equal to another type, found in a .cmx file, named
        by export identifier. *)
    val alias_type : Flambda_kind.t -> Export_id.t -> Flambda_types.t

    (** Determine the (unique) kind of a type. *)
    val kind : Flambda_types.t -> Flambda_kind.t

    (** Enforce that a type is of kind [Value], returning the corresponding
        [ty]. *)
    val force_to_kind_value : Flambda_types.t -> Flambda_types.ty_value

    (** Enforce that a type is of a naked number kind, returning the
        corresponding [ty]. *)
    val force_to_kind_naked_number
       : 'kind Flambda_kind.Naked_number.t
      -> Flambda_types.t
      -> 'kind Flambda_types.ty_naked_number

    (** Enforce that a type is of naked float kind, returning the corresponding
        [ty]. *)
    val force_to_kind_naked_float
       : Flambda_types.t
      -> Numbers.Float_by_bit_pattern.Set.t Flambda_types.ty_naked_number

    val force_to_kind_naked_int32
       : Flambda_types.t
      -> Numbers.Int32.Set.t Flambda_types.ty_naked_number

    val force_to_kind_naked_int64
       : Flambda_types.t
      -> Numbers.Int64.Set.t Flambda_types.ty_naked_number

    val force_to_kind_naked_nativeint
       : Flambda_types.t
      -> Targetint.Set.t Flambda_types.ty_naked_number

    val force_to_kind_naked_immediate
       : Flambda_types.t
      -> Immediate.Set.t Flambda_types.ty_naked_number

    (** Enforce that a type is of fabricated kind, returning the corresponding
        [ty]. *)
    val force_to_kind_fabricated
       : Flambda_types.t
      -> Flambda_types.of_kind_fabricated Flambda_types.ty

    (** Enforce that a type is of a given kind. *)
    val check_of_kind : Flambda_types.t -> Flambda_kind.t -> unit
  end = struct
    open Flambda_types

    let force_to_kind_value t =
      match t with
      | Value ty_value -> ty_value
      | Naked_number _
      | Fabricated _ ->
        Misc.fatal_errorf "Type has wrong kind (expected [Value]):@ %a"
          Type_printers.print t

    let force_to_kind_naked_immediate (t : t)
          : Immediate.Set.t ty_naked_number =
      match t with
      | Naked_number (ty_naked_number, K.Naked_number.Naked_immediate) ->
        ty_naked_number
      | Naked_number _
      | Fabricated _
      | Value _ ->
        Misc.fatal_errorf
          "Type has wrong kind (expected [Naked_number Immediate]):@ %a"
          Type_printers.print t

    let force_to_kind_naked_float (t : t)
          : Float.Set.t ty_naked_number =
      match t with
      | Naked_number (ty_naked_number, K.Naked_number.Naked_float) ->
        ty_naked_number
      | Naked_number _
      | Fabricated _
      | Value _ ->
        Misc.fatal_errorf
          "Type has wrong kind (expected [Naked_number Float]):@ %a"
          Type_printers.print t

    let force_to_kind_naked_int32 (t : t) : Int32.Set.t ty_naked_number =
      match t with
      | Naked_number (ty_naked_number, K.Naked_number.Naked_int32) ->
        ty_naked_number
      | Naked_number _
      | Fabricated _
      | Value _ ->
        Misc.fatal_errorf
          "Type has wrong kind (expected [Naked_number Int32]):@ %a"
          Type_printers.print t

    let force_to_kind_naked_int64 (t : t) : Int64.Set.t ty_naked_number =
      match t with
      | Naked_number (ty_naked_number, K.Naked_number.Naked_int64) ->
        ty_naked_number
      | Naked_number _
      | Fabricated _
      | Value _ ->
        Misc.fatal_errorf
          "Type has wrong kind (expected [Naked_number Int64]):@ %a"
          Type_printers.print t

    let force_to_kind_naked_nativeint (t : t)
          : Targetint.Set.t ty_naked_number =
      match t with
      | Naked_number (ty_naked_number, K.Naked_number.Naked_nativeint) ->
        ty_naked_number
      | Naked_number _
      | Fabricated _
      | Value _ ->
        Misc.fatal_errorf
          "Type has wrong kind (expected [Naked_number Nativeint]):@ %a"
          Type_printers.print t

    let force_to_kind_naked_number (type n) (kind : n K.Naked_number.t) (t : t)
          : n ty_naked_number =
      match t, kind with
      | Naked_number (ty_naked_number, K.Naked_number.Naked_immediate),
          K.Naked_number.Naked_immediate ->
        ty_naked_number
      | Naked_number (ty_naked_number, K.Naked_number.Naked_float),
          K.Naked_number.Naked_float ->
        ty_naked_number
      | Naked_number (ty_naked_number, K.Naked_number.Naked_int32),
          K.Naked_number.Naked_int32 ->
        ty_naked_number
      | Naked_number (ty_naked_number, K.Naked_number.Naked_int64),
          K.Naked_number.Naked_int64 ->
        ty_naked_number
      | Naked_number (ty_naked_number, K.Naked_number.Naked_nativeint),
          K.Naked_number.Naked_nativeint ->
        ty_naked_number
      | Naked_number _, _
      | Fabricated _, _
      | Value _, _ ->
        Misc.fatal_errorf "Type has wrong kind (expected \
            [Naked_number %a]):@ %a"
          K.Naked_number.print kind
          Type_printers.print t

    let force_to_kind_fabricated t =
      match t with
      | Fabricated ty_fabricated -> ty_fabricated
      | Value _
      | Naked_number _ ->
        Misc.fatal_errorf "Type has wrong kind (expected [Fabricated]):@ %a"
          Type_printers.print t

    let ty_is_obviously_bottom (ty : _ ty) =
      match ty with
      | No_alias (Join []) -> true
      | _ -> false

    let is_obviously_bottom (t : t) =
      match t with
      | Value ty -> ty_is_obviously_bottom ty
      | Naked_number (ty, _) -> ty_is_obviously_bottom ty
      | Fabricated ty -> ty_is_obviously_bottom ty

    let of_ty_value ty_value : t =
      Value ty_value

    let of_ty_naked_number (type n) (ty_naked_number : n ty_naked_number)
          (kind : n K.Naked_number.t) : t =
      Naked_number (ty_naked_number, kind)

    let of_ty_fabricated ty_fabricated : t =
      Fabricated ty_fabricated

    (* CR-someday mshinwell: Functions such as [alias] and [bottom] could be
       simplified if [K.t] were a GADT. *)

    let alias_type_of (kind : K.t) name : t =
      match kind with
      | Value ->
        Value (Equals name)
      | Naked_number Naked_immediate ->
        Naked_number (Equals name, K.Naked_number.Naked_immediate)
      | Naked_number Naked_float ->
        Naked_number (Equals name, K.Naked_number.Naked_float)
      | Naked_number Naked_int32 ->
        Naked_number (Equals name, K.Naked_number.Naked_int32)
      | Naked_number Naked_int64 ->
        Naked_number (Equals name, K.Naked_number.Naked_int64)
      | Naked_number Naked_nativeint ->
        Naked_number (Equals name, K.Naked_number.Naked_nativeint)
      | Fabricated ->
        Fabricated (Equals name)

    let alias_type_of_as_ty_value name : ty_value = Equals name

    let alias_type_of_as_ty_fabricated name : ty_fabricated = Equals name

    let alias_type (kind : K.t) export_id : t =
      match kind with
      | Value ->
        Value (Type export_id)
      | Naked_number Naked_immediate ->
        Naked_number (Type export_id, K.Naked_number.Naked_immediate)
      | Naked_number Naked_float ->
        Naked_number (Type export_id, K.Naked_number.Naked_float)
      | Naked_number Naked_int32 ->
        Naked_number (Type export_id, K.Naked_number.Naked_int32)
      | Naked_number Naked_int64 ->
        Naked_number (Type export_id, K.Naked_number.Naked_int64)
      | Naked_number Naked_nativeint ->
        Naked_number (Type export_id, K.Naked_number.Naked_nativeint)
      | Fabricated ->
        Fabricated (Type export_id)

    let bottom_as_ty_value () : ty_value =
      No_alias (Join [])

    let bottom_as_ty_fabricated () : ty_fabricated =
      No_alias (Join [])

    let bottom (kind : K.t) : t =
      match kind with
      | Value ->
        Value (No_alias (Join []))
      | Naked_number Naked_immediate ->
        Naked_number (No_alias (Join []), K.Naked_number.Naked_immediate)
      | Naked_number Naked_float ->
        Naked_number (No_alias (Join []), K.Naked_number.Naked_float)
      | Naked_number Naked_int32 ->
        Naked_number (No_alias (Join []), K.Naked_number.Naked_int32)
      | Naked_number Naked_int64 ->
        Naked_number (No_alias (Join []), K.Naked_number.Naked_int64)
      | Naked_number Naked_nativeint ->
        Naked_number (No_alias (Join []), K.Naked_number.Naked_nativeint)
      | Fabricated ->
        Fabricated (No_alias (Join []))

    let any_value_as_ty_value () : ty_value =
      No_alias Unknown

    let any_fabricated_as_ty_fabricated () : ty_fabricated =
      No_alias Unknown

    let any_naked_float_as_ty_naked_float () : _ ty_naked_number =
      No_alias Unknown

    let any_value () : t =
      Value (any_value_as_ty_value ())

    let any_tagged_immediate () : t =
      Value (No_alias (Join [Blocks_and_tagged_immediates {
        immediates = Immediates.create_unknown ();
        blocks = Blocks.create_bottom ();
      }, Name_permutation.create ()]))

    let any_naked_immediate () : t =
      Naked_number (No_alias Unknown, K.Naked_number.Naked_immediate)

    let any_naked_float () : t =
      Naked_number (No_alias Unknown, K.Naked_number.Naked_float)

    let any_naked_int32 () : t =
      Naked_number (No_alias Unknown, K.Naked_number.Naked_int32)

    let any_naked_int64 () : t =
      Naked_number (No_alias Unknown, K.Naked_number.Naked_int64)

    let any_naked_nativeint () : t =
      Naked_number (No_alias Unknown, K.Naked_number.Naked_nativeint)

    let any_fabricated () : t =
      Fabricated (No_alias Unknown)

    let unknown (kind : K.t) =
      match kind with
      | Value ->
        Value (No_alias Unknown)
      | Naked_number Naked_immediate ->
        Naked_number (No_alias Unknown, K.Naked_number.Naked_immediate)
      | Naked_number Naked_float ->
        Naked_number (No_alias Unknown, K.Naked_number.Naked_float)
      | Naked_number Naked_int32 ->
        Naked_number (No_alias Unknown, K.Naked_number.Naked_int32)
      | Naked_number Naked_int64 ->
        Naked_number (No_alias Unknown, K.Naked_number.Naked_int64)
      | Naked_number Naked_nativeint ->
        Naked_number (No_alias Unknown, K.Naked_number.Naked_nativeint)
      | Fabricated ->
        Fabricated (No_alias Unknown)

    let these_naked_immediates (is : Immediate.Set.t) : t =
      let of_kind : _ of_kind_naked_number = Immediate is in
      Naked_number (No_alias (Join [of_kind, Name_permutation.create ()]),
        K.Naked_number.Naked_immediate)

    let these_naked_floats (is : Float.Set.t) : t =
      let of_kind : _ of_kind_naked_number = Float is in
      Naked_number (No_alias (Join [of_kind, Name_permutation.create ()]),
        K.Naked_number.Naked_float)

    let these_naked_int32s (is : Int32.Set.t) : t =
      let of_kind : _ of_kind_naked_number = Int32 is in
      Naked_number (No_alias (Join [of_kind, Name_permutation.create ()]),
        K.Naked_number.Naked_int32)

    let these_naked_int64s (is : Int64.Set.t) : t =
      let of_kind : _ of_kind_naked_number = Int64 is in
      Naked_number (No_alias (Join [of_kind, Name_permutation.create ()]),
        K.Naked_number.Naked_int64)

    let these_naked_nativeints (is : Targetint.Set.t) : t =
      let of_kind : _ of_kind_naked_number = Nativeint is in
      Naked_number (No_alias (Join [of_kind, Name_permutation.create ()]),
        K.Naked_number.Naked_nativeint)

    let this_naked_immediate i =
      these_naked_immediates (Immediate.Set.singleton i)

    let this_naked_float f =
      these_naked_floats (Float.Set.singleton f)

    let this_naked_float_as_ty_naked_float f =
      let fs = Float.Set.singleton f in
      let of_kind : _ of_kind_naked_number = Float fs in
      No_alias (Join [of_kind, Name_permutation.create ()])

    let this_naked_int32 i =
      these_naked_int32s (Int32.Set.singleton i)

    let this_naked_int64 i =
      these_naked_int64s (Int64.Set.singleton i)

    let this_naked_nativeint i =
      these_naked_nativeints (Targetint.Set.singleton i)

    let box_float (t : t) : t =
      match t with
      | Naked_number (ty_naked_float, K.Naked_number.Naked_float) ->
        Value (No_alias (Join [
          Boxed_number (Boxed_float ty_naked_float),
            Name_permutation.create ()]))
      | Value _
      | Naked_number _
      | Fabricated _ ->
        Misc.fatal_errorf "Type of wrong kind for [box_float]: %a"
          Type_printers.print t

    let box_int32 (t : t) : t =
      match t with
      | Naked_number (ty_naked_int32, K.Naked_number.Naked_int32) ->
        Value (No_alias (Join [
          Boxed_number (Boxed_int32 ty_naked_int32),
            Name_permutation.create ()]))
      | Value _
      | Naked_number _
      | Fabricated _ ->
        Misc.fatal_errorf "Type of wrong kind for [box_int32]: %a"
          Type_printers.print t

    let box_int64 (t : t) : t =
      match t with
      | Naked_number (ty_naked_int64, K.Naked_number.Naked_int64) ->
        Value (No_alias (Join [
          Boxed_number (Boxed_int64 ty_naked_int64),
            Name_permutation.create ()]))
      | Value _
      | Naked_number _
      | Fabricated _ ->
        Misc.fatal_errorf "Type of wrong kind for [box_int64]: %a"
          Type_printers.print t

    let box_nativeint (t : t) : t =
      match t with
      | Naked_number (ty_naked_nativeint, K.Naked_number.Naked_nativeint) ->
        Value (No_alias (Join [
          Boxed_number (Boxed_nativeint ty_naked_nativeint),
            Name_permutation.create ()]))
      | Value _
      | Naked_number _
      | Fabricated _ ->
        Misc.fatal_errorf "Type of wrong kind for [box_nativeint]: %a"
          Type_printers.print t

    let these_tagged_immediates imms : t =
      if Immediate.Set.is_empty imms then
        bottom (K.value ())
      else
        let immediates =
          Immediates.create_with_equations (
            Immediate.Map.of_set (fun _imm ->
                Typing_env_extension.empty ())
              imms)
        in
        let blocks_and_tagged_immediates : blocks_and_tagged_immediates =
          { immediates;
            blocks = Blocks.create_bottom ();
          }
        in
        Value (No_alias (Join [
          Blocks_and_tagged_immediates blocks_and_tagged_immediates,
            Name_permutation.create ()]))

    let these_tagged_immediates_with_envs env_map =
      if Immediate.Map.is_empty env_map then
        bottom (K.value ())
      else
        let blocks_and_tagged_immediates : blocks_and_tagged_immediates =
          { immediates = Immediates.create_with_equations env_map;
            blocks = Blocks.create_bottom ();
          }
        in
        Value (No_alias (Join [
          Blocks_and_tagged_immediates blocks_and_tagged_immediates,
            Name_permutation.create ()]))

    let this_tagged_immediate imm =
      these_tagged_immediates (Immediate.Set.singleton imm)

    let any_tagged_bool () =
      let bools =
        Immediate.Set.add Immediate.bool_false
          (Immediate.Set.add Immediate.bool_true Immediate.Set.empty)
      in
      these_tagged_immediates bools

    let this_boxed_float f = box_float (this_naked_float f)
    let this_boxed_int32 f = box_int32 (this_naked_int32 f)
    let this_boxed_int64 f = box_int64 (this_naked_int64 f)
    let this_boxed_nativeint f = box_nativeint (this_naked_nativeint f)

    let these_boxed_floats f = box_float (these_naked_floats f)
    let these_boxed_int32s f = box_int32 (these_naked_int32s f)
    let these_boxed_int64s f = box_int64 (these_naked_int64s f)
    let these_boxed_nativeints f = box_nativeint (these_naked_nativeints f)

    let these_discriminants_as_ty_fabricated discriminants_to_env_extension
          : ty_fabricated =
      let discriminants =
        Discriminants.create_with_equations discriminants_to_env_extension
      in
      No_alias (Join [Discriminants discriminants, Name_permutation.create ()])

    let these_discriminants discriminants_to_env_extension : t =
      Fabricated (
        these_discriminants_as_ty_fabricated discriminants_to_env_extension)

    let this_discriminant_as_ty_fabricated discriminant =
      let discriminants =
        Discriminants.create (Discriminant.Set.singleton discriminant)
      in
      No_alias (Join [Discriminants discriminants, Name_permutation.create ()])

    let this_discriminant discriminant : t =
      Fabricated (this_discriminant_as_ty_fabricated discriminant)

    let any_discriminant_as_ty_fabricated () : ty_fabricated =
      No_alias Unknown

    let this_immutable_string_as_ty_value str : ty_value =
      let str =
        String_info.create ~contents:(Contents str)
          ~size:(Targetint.OCaml.of_int (String.length str))
      in
      let str = String_info.Set.singleton str in
      No_alias (Join [String str, Name_permutation.create ()])

    let this_immutable_string str : t =
      Value (this_immutable_string_as_ty_value str)

    let immutable_string_as_ty_value ~size : ty_value =
      let str = String_info.create ~contents:Unknown_or_mutable ~size in
      let str = String_info.Set.singleton str in
      No_alias (Join [String str, Name_permutation.create ()])

    let immutable_string ~size : t =
      Value (immutable_string_as_ty_value ~size)

    let mutable_string ~size : t =
      let str = String_info.create ~contents:Unknown_or_mutable ~size in
      let str = String_info.Set.singleton str in
      Value (No_alias (Join [String str, Name_permutation.create ()]))

    let kind (t : t) =
      match t with
      | Value _ -> K.value ()
      | Naked_number (_, K.Naked_number.Naked_immediate) -> K.naked_immediate ()
      | Naked_number (_, K.Naked_number.Naked_float) -> K.naked_float ()
      | Naked_number (_, K.Naked_number.Naked_int32) -> K.naked_int32 ()
      | Naked_number (_, K.Naked_number.Naked_int64) -> K.naked_int64 ()
      | Naked_number (_, K.Naked_number.Naked_nativeint) -> K.naked_nativeint ()
      | Fabricated _ -> K.fabricated ()

    let mutable_float_array ~size : t =
      match Targetint.OCaml.to_int_option size with
      | None ->
        (* CR mshinwell: Here and elsewhere, this should be a normal compilation
           error, not a fatal error. *)
        Misc.fatal_error "Mutable float array too long for host"
      | Some size ->
        let field_tys = List.init size (fun _index -> any_naked_float ()) in
        let blocks =
          Blocks.create ~field_tys (Closed Tag.double_array_tag)
        in
        let blocks_imms : blocks_and_tagged_immediates =
          { immediates = Immediates.create_bottom ();
            blocks;
          }
        in
        Value (No_alias (Join [
          Blocks_and_tagged_immediates blocks_imms,
          Name_permutation.create ()]))

    let immutable_float_array fields : t =
      match Targetint.OCaml.of_int_option (Array.length fields) with
      | None ->
        Misc.fatal_error "Immutable float array too long for target"
      | Some _size ->
        let field_tys =
          Array.map (fun ty_naked_number : t ->
              Naked_number (ty_naked_number, K.Naked_number.Naked_float))
            fields
        in
        let blocks =
          Blocks.create ~field_tys:(Array.to_list field_tys)
            (Closed Tag.double_array_tag)
        in
        let blocks_imms : blocks_and_tagged_immediates =
          { immediates = Immediates.create_bottom ();
            blocks;
          }
        in
        Value (No_alias (Join [
          Blocks_and_tagged_immediates blocks_imms,
          Name_permutation.create ()]))

    let this_immutable_float_array fields : t =
      let make_field f : _ ty_naked_number =
        No_alias (Join [
          Float (Float.Set.singleton f), Name_permutation.create ()])
      in
      let fields = Array.map make_field fields in
      immutable_float_array fields

    let block tag ~(fields : t list) =
      (* CR mshinwell: We should check the field kinds against the tag. *)
      match Targetint.OCaml.of_int_option (List.length fields) with
      | None ->
        Misc.fatal_error "Block too long for target"
      | Some _size ->
        let blocks = Blocks.create ~field_tys:fields (Closed tag) in
        let blocks_imms : blocks_and_tagged_immediates =
          { immediates = Immediates.create_bottom ();
            blocks;
          }
        in
        Value (No_alias (Join [
          Blocks_and_tagged_immediates blocks_imms,
            Name_permutation.create ()]))

    (* CR mshinwell: bad name *)
    let block_of_values tag ~(fields : ty_value list) =
      block tag ~fields:(List.map (fun field : t -> Value field) fields)

    let block_of_unknown_values _tag ~size:_ = Misc.fatal_error "TBD"
  (*
      let fields =
        Array.init size (fun _index : _ mutable_or_immutable ->
          Immutable (any_value_as_ty_value ()))
      in
      block_of_values tag ~fields
  *)

    let block_with_size_at_least ~n ~field_n_minus_one =
      let type_of_field_n_minus_one =
        alias_type_of (Flambda_kind.value ()) (Simple.var field_n_minus_one)
      in
      let field_tys =
        List.init n (fun index ->
          if index = n - 1 then type_of_field_n_minus_one
          else any_value ())
      in
      let blocks = Blocks.create ~field_tys Open in
      let blocks_imms : blocks_and_tagged_immediates =
        { immediates = Immediates.create_bottom ();
          blocks;
        }
      in
      Value (No_alias (Join [
        Blocks_and_tagged_immediates blocks_imms,
          Name_permutation.create ()]))

    let any_boxed_float () = box_float (any_naked_float ())
    let any_boxed_int32 () = box_int32 (any_naked_int32 ())
    let any_boxed_int64 () = box_int64 (any_naked_int64 ())
    let any_boxed_nativeint () = box_nativeint (any_naked_nativeint ())

    let check_of_kind t (expected_kind : K.t) =
      let actual_kind = kind t in
      if not (K.equal actual_kind expected_kind) then begin
        Misc.fatal_errorf "Type has wrong kind: have %a but expected %a"
          K.print actual_kind
          K.print expected_kind
      end

    let bottom_like t = bottom (kind t)
    let unknown_like t = unknown (kind t)

    let create_inlinable_function_declaration ~is_classic_mode ~closure_origin
          ~continuation_param ~exn_continuation_param ~params ~body ~code_id
          ~result_arity ~stub ~dbg ~inline ~specialise ~is_a_functor
          ~invariant_params ~size ~direct_call_surrogate ~my_closure
          : function_declaration =
      Inlinable {
        closure_origin;
        continuation_param;
        exn_continuation_param;
        is_classic_mode;
        params;
        body;
        code_id;
        free_names_in_body = Expr.free_names body;
        result_arity;
        stub;
        dbg;
        inline;
        specialise;
        is_a_functor;
        invariant_params;
        size;
        direct_call_surrogate;
        my_closure;
      }

    let create_non_inlinable_function_declaration () : function_declaration =
      Non_inlinable

    let closure closure_id function_decl ty closure_elements ~set_of_closures =
      let closure_elements' =
        let closure_elements =
          Var_within_closure.Map.map (fun ty_value : t -> Value ty_value)
            closure_elements
        in
        Closure_elements.create closure_elements
      in
      let closures_entry : closures_entry =
        { function_decl;
          ty;
          closure_elements = closure_elements';
          set_of_closures;
        }
      in
      let by_closure_id =
        Closures_entry_by_closure_id.create_exactly_multiple
          (Closure_id_and_var_within_closure_set.Map.singleton
            (closure_id, Var_within_closure.Map.keys closure_elements)
            closures_entry)
      in
      let closures : closures =
        { by_closure_id;
        }
      in
      Value (No_alias (Join [Closures closures, Name_permutation.create ()]))

    let closure_containing_at_least var_within_closure ty_value =
      let closure_elements =
        Var_within_closure.Map.singleton var_within_closure (Value ty_value)
      in
      let closure_elements = Closure_elements.create closure_elements in
      let closures_entry : closures_entry =
        { function_decl = Non_inlinable;
          ty = Function_type.create_unknown ();
          closure_elements;
          set_of_closures = any_fabricated_as_ty_fabricated ()
        }
      in
      let by_closure_id =
        Closures_entry_by_closure_id.create_at_least_multiple
          (Var_within_closure_set.Map.singleton
            (Var_within_closure.Set.singleton var_within_closure)
            closures_entry)
      in
      let closures : closures =
        { by_closure_id;
        }
      in
      Value (No_alias (Join [Closures closures, Name_permutation.create ()]))

    let set_of_closures ~closures =
      if Closure_id.Map.is_empty closures then bottom (Flambda_kind.value ())
      else
        let all_closures = Closure_id.Map.keys closures in
        let by_closure_id = Types_by_closure_id.create closures in
        let set_of_closures_entry : set_of_closures_entry =
           { by_closure_id; }
        in
        let closures =
          Closure_ids.create
            (Closure_id_set.Map.singleton all_closures set_of_closures_entry)
            Closed
        in
        Fabricated (No_alias (Join [Set_of_closures { closures; },
          Name_permutation.create ()]))

    let set_of_closures_containing_at_least closure_id =
      let by_closure_id =
        Types_by_closure_id.create
          (Closure_id.Map.singleton closure_id (any_value ()))
      in
      let set_of_closures_entry : set_of_closures_entry = { by_closure_id; } in
      let closure_id = Closure_id.Set.singleton closure_id in
      let closures =
        Closure_ids.create
          (Closure_id_set.Map.singleton closure_id set_of_closures_entry)
          Open
      in
      Fabricated (No_alias (Join [Set_of_closures { closures; },
        Name_permutation.create ()]))

    let free_names = Type_free_names.free_names

    let apply_name_permutation_unknown_or_join unknown_or_join perm =
      match unknown_or_join with
      | Unknown -> unknown_or_join
      | Join of_kind_foos ->
        let something_changed = ref false in
        let of_kind_foos =
          List.map (fun (of_kind_foo, existing_perm) ->
              let new_perm = Name_permutation.compose existing_perm perm in
              if not (new_perm == existing_perm) then begin
                something_changed := true
              end;
              of_kind_foo, new_perm)
            of_kind_foos
        in
        if not !something_changed then unknown_or_join
        else Join of_kind_foos

    let apply_name_permutation_ty ty perm =
      match ty with
      | No_alias unknown_or_join ->
        let unknown_or_join' =
          apply_name_permutation_unknown_or_join unknown_or_join perm
        in
        if unknown_or_join == unknown_or_join' then ty
        else No_alias unknown_or_join'
      | Type _ -> ty
      | Equals simple ->
        let simple' = Simple.apply_name_permutation simple perm in
        if simple == simple' then ty
        else Equals simple'

    let apply_name_permutation t perm =
      match t with
      | Value ty_value ->
        let ty_value' = apply_name_permutation_ty ty_value perm in
        if ty_value == ty_value' then t
        else Value ty_value'
      | Naked_number (ty_naked_number, kind) ->
        let ty_naked_number' = apply_name_permutation_ty ty_naked_number perm in
        if ty_naked_number == ty_naked_number' then t
        else Naked_number (ty_naked_number', kind)
      | Fabricated ty_fabricated ->
        let ty_fabricated' = apply_name_permutation_ty ty_fabricated perm in
        if ty_fabricated == ty_fabricated' then t
        else Fabricated ty_fabricated'

    let get_alias t =
      match t with
      | Value (Equals simple) -> Some simple
      | Value _ -> None
      | Naked_number (Equals simple, _) -> Some simple
      | Naked_number _ -> None
      | Fabricated (Equals simple) -> Some simple
      | Fabricated _ -> None

    module Set_of_closures_entry = struct
      type t = set_of_closures_entry

      let bottom () : t =
        { by_closure_id = Types_by_closure_id.create_bottom ();
        }

      let print_with_cache ~cache ppf { by_closure_id; } =
        Format.fprintf ppf
          "@[<hov 1>(@\
            @[<hov 1>(by_closure_id@ %a)@])@]"
          (Types_by_closure_id.print ~cache) by_closure_id

      let equal = Type_equality.equal_set_of_closures_entry

      let add_or_meet_equations { by_closure_id; } env equations =
        let by_closure_id =
          Types_by_closure_id.add_or_meet_equations by_closure_id env equations
        in
        { by_closure_id; }

      let meet env _fresh t1 t2 =
        Both_meet_and_join.meet_set_of_closures_entry env t1 t2

      let join env _fresh t1 t2 =
        Both_meet_and_join.join_set_of_closures_entry env t1 t2

      let free_names { by_closure_id; } =
        Types_by_closure_id.free_names by_closure_id

      let apply_name_permutation { by_closure_id; } perm : t =
        let by_closure_id =
          Types_by_closure_id.apply_name_permutation by_closure_id perm
        in
        { by_closure_id; }
    end

    module Closures_entry = struct
      type t = closures_entry

      let bottom () : t =
        { function_decl = Non_inlinable;
          ty = Function_type.create_bottom ();
          closure_elements = Closure_elements.create_bottom ();
          set_of_closures = bottom_as_ty_fabricated ();
        }

      let print_with_cache ~cache ppf
            { function_decl; ty; closure_elements; set_of_closures; } =
        Format.fprintf ppf
          "@[<hov 1>(@\
            @[<hov 1>(function_decl@ %a)@]@ \
            @[<hov 1>(ty@ %a)@]@ \
            @[<hov 1>(closure_elements@ %a)@]@ \
            @[<hov 1>(set_of_closures@ %a)@])@]"
          (Type_printers.print_function_declaration_with_cache ~cache)
            function_decl
          (Function_type.print_with_cache ~cache) ty
          (Closure_elements.print ~cache) closure_elements
          (Type_printers.print_ty_fabricated_with_cache ~cache) set_of_closures

      let add_or_meet_equations t _env _equations =
        (* CR mshinwell: Think about what should happen here. *)
        t

      let equal = Type_equality.equal_closures_entry

      let meet env _fresh t1 t2 =
        Both_meet_and_join.meet_closures_entry env t1 t2

      let join env _fresh t1 t2 =
        Both_meet_and_join.join_closures_entry env t1 t2

      let free_names
            { function_decl = _; ty; closure_elements; set_of_closures; } =
        Name_occurrences.union (Function_type.free_names ty)
          (Name_occurrences.union (Closure_elements.free_names closure_elements)
            (Type_free_names.free_names_of_ty_fabricated set_of_closures))

      let apply_name_permutation
            { function_decl; ty; closure_elements; set_of_closures; } perm : t =
        { function_decl;
          ty = Function_type.apply_name_permutation ty perm;
          closure_elements =
            Closure_elements.apply_name_permutation closure_elements perm;
          set_of_closures = apply_name_permutation_ty set_of_closures perm;
        }
    end
  end and Flambda_types : sig
    type 'a or_alias =
      | No_alias of 'a
      | Type of Export_id.t
      | Equals of Simple.t

    type t =
      | Value of ty_value
      | Naked_number :
          'kind ty_naked_number * 'kind Flambda_kind.Naked_number.t -> t
      | Fabricated of ty_fabricated

    and flambda_type = t

    and ty_value = of_kind_value ty
    and 'a ty_naked_number = 'a of_kind_naked_number ty
    and ty_fabricated = of_kind_fabricated ty

    and 'a ty = 'a unknown_or_join or_alias

    (** For each kind there is a lattice of types. *)
    and 'a unknown_or_join =
      | Unknown
      (** "Any value can flow to this point": the top element. *)
      | Join of ('a * Name_permutation.t) list
      (** - The list being empty means bottom, the least element: "no value can
            flow to this point".
          - The list containing a single element is the usual case where there
            is no join between incompatible types.
          - If the list contains more than one element:
            A join, between incompatible types, which has been remembered
            in case it is refined by a subsequent meet.  Joins between
            compatible types are immediately pushed down through the top level
            structure of the type.

          The [Name_permutation.t] is a delayed permutation which must be
          pushed down through the structure of the type as it is examined.

          Invariant: every member of a [Join] is incompatible with the other
          members. *)

    and of_kind_value =
      | Blocks_and_tagged_immediates of blocks_and_tagged_immediates
      | Boxed_number : _ of_kind_value_boxed_number -> of_kind_value
      | Closures of closures
      | String of String_info.Set.t

    and blocks_and_tagged_immediates = {
      immediates : Immediates.t;
      (** Cases for constant constructors (in the case of variants) and
          arbitrary tagged immediates. *)
      blocks : Blocks.t;
      (** Cases for non-constant constructors (in the case of variants) and
          normal blocks. *)
    }

    (** Boxed integer and floating-point numbers together with the types
        of their contents. *)
    and 'a of_kind_value_boxed_number =
      | Boxed_float
          : Float.Set.t ty_naked_number
          -> Float.Set.t ty_naked_number of_kind_value_boxed_number
      | Boxed_int32
          : Int32.Set.t ty_naked_number
          -> Int32.Set.t ty_naked_number of_kind_value_boxed_number
      | Boxed_int64
          : Int64.Set.t ty_naked_number
          -> Int64.Set.t ty_naked_number of_kind_value_boxed_number
      | Boxed_nativeint
          : Targetint.Set.t ty_naked_number
          -> Targetint.Set.t ty_naked_number of_kind_value_boxed_number

    (** A function declaration which is inlinable (which in particular implies
        that the code of the function's body is known).  Such declarations are
        completely closed entities in terms of names. *)
    and inlinable_function_declaration = {
      closure_origin : Closure_origin.t;
      continuation_param : Continuation.t;
      exn_continuation_param : Continuation.t;
      is_classic_mode : bool;
      (** Whether the file from which this function declaration originated was
          compiled in classic mode. *)
      params : Kinded_parameter.t list;
      (** The parameters of the function, in order. *)
      body : Expr.t;
      code_id : Code_id.t;
      free_names_in_body : Name_occurrences.t;
      stub : bool;
      result_arity : Flambda_arity.t;
      dbg : Debuginfo.t;
      inline : Inline_attribute.t;
      specialise : Specialise_attribute.t;
      is_a_functor : bool;
      invariant_params : Variable.Set.t lazy_t;
      size : int option lazy_t;
      (** For functions that are very likely to be inlined, the size of the
          function's body. *)
      direct_call_surrogate : Closure_id.t option;
      my_closure : Variable.t;
    }

    and function_declaration =
      | Non_inlinable
      | Inlinable of inlinable_function_declaration

    and closures_entry = {
      function_decl : function_declaration;
      (** Information from the term language about the function declaration
          associated with the closure (call it [C]) described by a
          [closures_entry]. *)
      ty : Function_type.t;
      (** The type of the function associated with [C].
          Note: function parameter types are covariant! *)
      closure_elements : Closure_elements.t;
      (** Relational product describing the variables within a closure and
          equations between them. *)
      set_of_closures : ty_fabricated;
      (** Link back to the type of the set of closures containing [C]. *)
    }

    and closures = {
      by_closure_id : Closures_entry_by_closure_id.t;
      (** Row-like structure that selects [closures_entry] structures based
          on closure ID and the set of variables in the closure. *)
    }

    (** Unboxed ("naked") integer and floating-point numbers together with
        any information known about which particular numbers they might be. *)
    and 'a of_kind_naked_number =
      | Immediate : Immediate.Set.t -> Immediate.Set.t of_kind_naked_number
      | Float : Float.Set.t -> Float.Set.t of_kind_naked_number
      | Int32 : Int32.Set.t -> Int32.Set.t of_kind_naked_number
      | Int64 : Int64.Set.t -> Int64.Set.t of_kind_naked_number
      | Nativeint : Targetint.Set.t -> Targetint.Set.t of_kind_naked_number

    and of_kind_naked_immediate = Immediate.Set.t of_kind_naked_number
    and of_kind_naked_float = Float.Set.t of_kind_naked_number
    and of_kind_naked_int32 = Int32.Set.t of_kind_naked_number
    and of_kind_naked_int64 = Int64.Set.t of_kind_naked_number
    and of_kind_naked_nativeint = Targetint.Set.t of_kind_naked_number

    and of_kind_fabricated =
      | Discriminants of Discriminants.t
        (** A discriminant is either:
            - a block tag, as returned by the [Get_tag] primitive; or
            - a constant constructor which has undergone a kind-cast to kind
              [Fabricated] using the [Discriminant_of_int] primitive. *)
      | Set_of_closures of set_of_closures
        (** A possibly mutually-recursive collection of closure values, which
            at runtime will be represented by a single block. *)

    and set_of_closures_entry = {
      by_closure_id : Types_by_closure_id.t;
      (** Relational product, indexed by individual closure IDs, that (via
          logical variables) describes the makeup of a set of closures. *)
    }

    and set_of_closures = {
      closures : Closure_ids.t;
      (** Row-like structure that maps _sets_ of [Closure_id.t]s to
          [set_of_closures_entry] structures. *)
    }
  end = struct
    include Flambda_types
  end and Function_type : sig
    type t 

    include Contains_names.S with type t := t

    val invariant : t -> unit

    val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

    val create
       : parameters:Flambda_types.t list
      -> results:Flambda_types.t list
      -> t

    val create_unknown : unit -> t

    val create_bottom : unit -> t

    val equal : Type_equality_env.t -> t -> t -> bool

(*
    val meet
       : Meet_env.t
      -> Relational_product.fresh_component_semantics
      -> t
      -> t
      -> (t * Typing_env_extension.t) Or_bottom.t
*)

    val meet_fresh
       : Meet_env.t
      -> t
      -> t
      -> (t * Typing_env_extension.t) Or_bottom.t

(*
    val join
       : Join_env.t
      -> Relational_product.fresh_component_semantics
      -> t
      -> t
      -> t
*)

    val join_fresh
       : Join_env.t
      -> t
      -> t
      -> t

    val introduce : t -> Typing_env.t -> Typing_env.t
  end = struct
    module RP = Relational_product.Make (Int_index) (Logical_variable_component)

    type t =
      | Unknown
      | Product of RP.t
      | Bottom

    let create ~parameters ~results =
      let assign_logical_variables tys =
        Targetint.OCaml.Map.of_list (
          List.mapi (fun index ty ->
              let index = Targetint.OCaml.of_int index in
              let kind = Flambda_type0_core.kind ty in
              let logical_var = Logical_variable.create kind in
              index, logical_var)
            tys)
      in
      let create_equations tys indexes_to_vars =
        let env_extension, _index =
          List.fold_left (fun (env_extension, index) ty ->
              let logical_var = Targetint.OCaml.Map.find index indexes_to_vars in
              let env_extension =
                Typing_env_extension.add_equation env_extension
                  (Name.logical_var logical_var) ty
              in
              let next_index = Targetint.OCaml.add index Targetint.OCaml.one in
              env_extension, next_index)
            (Typing_env_extension.empty (), Targetint.OCaml.zero)
            tys
        in
        env_extension
      in
      let param_vars = assign_logical_variables parameters in
      let param_env_extension = create_equations parameters param_vars in
      let result_vars = assign_logical_variables results in
      let result_env_extension = create_equations results result_vars in
      Product (RP.create [
        param_vars, param_env_extension;
        result_vars, result_env_extension;
      ])

    let create_unknown () = Unknown
    let create_bottom () = Bottom

    let invariant t =
      match t with
      | Product rp -> RP.invariant rp
      | Unknown | Bottom -> ()

    let print_with_cache ~cache ppf t =
      match t with
      | Unknown -> Format.pp_print_string ppf "Unknown"
      | Product rp -> RP.print_with_cache ~cache ppf rp
      | Bottom -> Format.pp_print_string ppf "Bottom"

    let equal env t1 t2 =
      match t1, t2 with
      | Product rp1, Product rp2 -> RP.equal env rp1 rp2
      | Unknown, Unknown -> true
      | Bottom, Bottom -> true
      | (Product _ | Unknown | Bottom), _ -> false

    let meet env fresh t1 t2 : _ Or_bottom.t =
      match t1, t2 with
      | Product rp1, Product rp2 ->
        begin match RP.meet env fresh rp1 rp2 with
        | Bottom -> Bottom
        | Ok (rp, env_extension) -> Ok (Product rp, env_extension)
        end
      | Product _, Unknown -> Ok (t1, Typing_env_extension.empty ())
      | Unknown, Product _ -> Ok (t2, Typing_env_extension.empty ())
      | Unknown, Unknown -> Ok (Unknown, Typing_env_extension.empty ())
      | Bottom, (Product _ | Bottom | Unknown)
      | (Product _ | Unknown), Bottom -> Bottom

    let meet_fresh env t1 t2 = meet env Fresh t1 t2

    let join env fresh t1 t2 =
      match t1, t2 with
      | Product rp1, Product rp2 -> Product (RP.join env fresh rp1 rp2)
      | Bottom, Product _ -> t2
      | Product _, Bottom -> t1
      | Bottom, Bottom -> Bottom
      | Unknown, (Product _ | Bottom | Unknown)
      | (Product _ | Bottom), Unknown -> Unknown

    let join_fresh env t1 t2 = join env Fresh t1 t2

    let free_names t =
      match t with
      | Product rp -> RP.free_names rp
      | Unknown | Bottom -> Name_occurrences.create ()

    let introduce t env =
      match t with
      | Product rp -> RP.introduce rp env
      | Unknown | Bottom -> env

    let apply_name_permutation t perm =
      match t with
      | Product rp ->
        let rp' = RP.apply_name_permutation rp perm in
        if rp == rp' then t
        else Product rp'
      | Unknown | Bottom -> Unknown
  end and Immediates : sig
    type t

    val print : cache:Printing_cache.t -> Format.formatter -> t -> unit

    (** Create a value which describes the presence of exactly no things. *)
    val create_bottom : unit -> t

    (** Create a value which describes the presence of an unknown set of
        things. *)
    val create_unknown : unit -> t

(*
    val create : Immediate.Set.t -> t
*)

    val create_with_equations
       : Typing_env_extension.t Immediate.Map.t
      -> t

    val equal : Type_equality_env.t -> t -> t -> bool

    val meet
       : Meet_env.t
      -> t
      -> t
      -> (t * Typing_env_extension.t) Or_bottom.t

    val join
       : Join_env.t
      -> t
      -> t
      -> t

    val get_singleton : t -> (Immediate.t * Typing_env_extension.t) option

    val all : t -> Immediate.Set.t Or_unknown.t

    include Contains_names.S with type t := t
  end = struct
    include Trivial_row_like.Make (Immediate)
  end and Join_env : sig
    type t

    (** Perform various invariant checks upon the given join environment. *)
    val invariant : t -> unit

    val create : Meet_env.t -> t

    val add_definition_central_environment
       : t
      -> Name.t
      -> Flambda_types.t
      -> t

    val add_extensions
       : t
      -> holds_on_left:Typing_env_extension.t
      -> holds_on_right:Typing_env_extension.t
      -> t

(*
    val add_extensions_and_extend_central_environment
       : t
      -> holds_on_left:Typing_env_extension.t
      -> holds_on_right:Typing_env_extension.t
      -> central_extension:Typing_env_extension.t
      -> t
*)

    val central_environment : t -> Meet_env.t

    val environment_on_left : t -> Typing_env.t

    val environment_on_right : t -> Typing_env.t

    val holds_on_left : t -> Typing_env_extension.t

    val holds_on_right : t -> Typing_env_extension.t

    val shortcut_precondition : t -> bool

    val perm_left : t -> Name_permutation.t

    val perm_right : t -> Name_permutation.t

    val clear_name_permutations : t -> t

    val compose_name_permutations
       : t
      -> perm_left:Name_permutation.t
      -> perm_right:Name_permutation.t
      -> t
  end = struct
    type t = {
      env : Meet_env.t;
      env_plus_extension1 : Typing_env.t;
      env_plus_extension2 : Typing_env.t;
      extension1 : Typing_env_extension.t;
      extension2 : Typing_env_extension.t;
    }

    let create env =
      { env;
        env_plus_extension1 = Meet_env.env env;
        env_plus_extension2 = Meet_env.env env;
        extension1 = Typing_env_extension.empty ();
        extension2 = Typing_env_extension.empty ();
      }

    let invariant _t =
      ()

    let add_extensions t ~holds_on_left ~holds_on_right =
      let env_plus_extension1 =
        Typing_env.add_or_meet_env_extension t.env_plus_extension1
          holds_on_left (Typing_env.max_level (Meet_env.env t.env))
      in
      let extension1 =
        Typing_env_extension.meet t.env t.extension1 holds_on_left
      in
      let env_plus_extension2 =
        Typing_env.add_or_meet_env_extension t.env_plus_extension2
          holds_on_right
          (Typing_env.max_level (Meet_env.env t.env))
      in
      let extension2 =
        Typing_env_extension.meet t.env t.extension2 holds_on_right
      in
      let t = {
        env = t.env;
        env_plus_extension1;
        env_plus_extension2;
        extension1;
        extension2;
      }
      in
      invariant t;
      t

    let add_definition_central_environment t name ty =
      let env =
        Meet_env.with_env t.env (fun env ->
          Typing_env.add env name (Typing_env.max_level env) (Definition ty))
      in
      let t = { t with env; } in
      invariant t;
      t

    let _add_extensions_and_extend_central_environment t
          ~holds_on_left ~holds_on_right ~central_extension =
      let env =
        Meet_env.with_env t.env (fun env ->
          Typing_env.add_or_meet_env_extension env
            central_extension
            (Typing_env.max_level (Meet_env.env t.env)))
      in
      let t = { t with env; } in
      invariant t;
      add_extensions t ~holds_on_left ~holds_on_right

    let central_environment t = t.env

    let environment_on_left t = t.env_plus_extension1

    let environment_on_right t = t.env_plus_extension2

    let holds_on_left t = t.extension1

    let holds_on_right t = t.extension2

    let fast_check_extensions_same_both_sides t =
      Typing_env_extension.fast_equal t.extension1 t.extension2

    let shortcut_precondition t =
      fast_check_extensions_same_both_sides t
        && Meet_env.shortcut_precondition t.env

    let perm_left t = Meet_env.perm_left t.env
    let perm_right t = Meet_env.perm_right t.env

    let clear_name_permutations t =
      { t with
        env = Meet_env.clear_name_permutations t.env;
      }

    let compose_name_permutations t ~perm_left ~perm_right =
      { t with
        env = Meet_env.compose_name_permutations t.env ~perm_left ~perm_right;
      }
  end and Make_meet_or_join : sig
    module Make
      (E : Either_meet_or_join_intf
        with module Join_env := Join_env
        with module Meet_env := Meet_env
        with module Typing_env_extension := Typing_env_extension)
      (S : Meet_and_join_spec_intf
        with module Flambda_types := Flambda_types
        with module Join_env := Join_env
        with module Typing_env_extension := Typing_env_extension) :
    sig
      val meet_or_join_ty
         : Join_env.t
        -> S.of_kind_foo Flambda_types.ty
        -> S.of_kind_foo Flambda_types.ty
        -> S.of_kind_foo Flambda_types.ty * Typing_env_extension.t
    end
  end = struct
    module Make
      (E : Either_meet_or_join_intf
        with module Join_env := Join_env
        with module Meet_env := Meet_env
        with module Typing_env_extension := Typing_env_extension)
      (S : Meet_and_join_spec_intf
        with module Flambda_types := Flambda_types
        with module Join_env := Join_env
        with module Typing_env_extension := Typing_env_extension) =
    struct
      (* CR mshinwell: Work out which properties we need to prove, e.g.
         Distributivity of meet over join:
           X n (X' u Y') == (X n X') u (X n Y'). *)

      let unknown_or_join_is_bottom (uj : _ Flambda_types.unknown_or_join) =
        match uj with
        | Join [] -> true
        | Unknown | Join _ -> false

      let unknown_or_join_is_unknown (uj : _ Flambda_types.unknown_or_join) =
        match uj with
        | Join _ -> false
        | Unknown -> true

      let print_ty ppf ty =
        S.print_ty ~cache:(Printing_cache.create ()) ppf ty

      let rec meet_on_unknown_or_join env
            (ou1 : S.of_kind_foo Flambda_types.unknown_or_join)
            (ou2 : S.of_kind_foo Flambda_types.unknown_or_join)
            : S.of_kind_foo Flambda_types.unknown_or_join
                * Typing_env_extension.t =
        if Meet_env.shortcut_precondition env && ou1 == ou2 then
          ou1, Typing_env_extension.empty ()
        else
          match ou1, ou2 with
          | Unknown, ou2 -> ou2, Typing_env_extension.empty ()
          | ou1, Unknown -> ou1, Typing_env_extension.empty ()
          | Join of_kind_foos1, Join of_kind_foos2 ->
            let of_kind_foos, env_extension_from_meet =
              List.fold_left
                (fun (of_kind_foos, env_extension_from_meet)
                     (of_kind_foo, perm1) ->
                  let new_env_extension_from_meet =
                    ref (Typing_env_extension.empty ())
                  in
                  let of_kind_foos =
                    Misc.Stdlib.List.filter_map (fun (of_kind_foo', perm2) ->
                        let meet =
                          let env =
                            Join_env.compose_name_permutations
                              (Join_env.create env)
                              ~perm_left:perm1 ~perm_right:perm2
                          in
                          S.meet_or_join_of_kind_foo env
                            of_kind_foo of_kind_foo'
                        in
                        match meet with
                        | Ok (of_kind_foo, new_env_extension_from_meet') ->
                          new_env_extension_from_meet :=
                            Typing_env_extension.meet env
                              new_env_extension_from_meet'
                                !new_env_extension_from_meet;
                          Some (of_kind_foo, Name_permutation.create ())
                        | Absorbing -> None)
                      of_kind_foos
                  in
                  let env_extension_from_meet =
                    Typing_env_extension.meet env
                      env_extension_from_meet !new_env_extension_from_meet;
                  in
                  of_kind_foos, env_extension_from_meet)
                (of_kind_foos2, Typing_env_extension.empty ())
                of_kind_foos1
            in
            let same_as input_of_kind_foos =
              List.compare_lengths input_of_kind_foos of_kind_foos = 0
                && List.for_all2
                     (fun (input_of_kind_foo, _perm1) (of_kind_foo, _perm2) ->
                       input_of_kind_foo == of_kind_foo)
                     input_of_kind_foos of_kind_foos
            in
            if same_as of_kind_foos1 then ou1, env_extension_from_meet
            else if same_as of_kind_foos2 then ou2, env_extension_from_meet
            else Join of_kind_foos, env_extension_from_meet

      and meet_ty env
            (or_alias1 : S.of_kind_foo Flambda_types.ty)
            (or_alias2 : S.of_kind_foo Flambda_types.ty)
            : S.of_kind_foo Flambda_types.ty * Typing_env_extension.t =
        if Meet_env.shortcut_precondition env && or_alias1 == or_alias2
        then begin
          or_alias1, Typing_env_extension.empty ()
        end else begin
          let unknown_or_join1, canonical_simple1 =
            Typing_env.resolve_aliases_and_squash_unresolved_names_on_ty'
              (Meet_env.env env)
              ~force_to_kind:S.force_to_kind
              ~print_ty
              or_alias1
          in
          let unknown_or_join2, canonical_simple2 =
            Typing_env.resolve_aliases_and_squash_unresolved_names_on_ty'
              (Meet_env.env env)
              ~force_to_kind:S.force_to_kind
              ~print_ty
              or_alias2
          in
          let add_equation_if_on_a_name env_extension (simple : Simple.t) ty =
            match simple with
            | Name name ->
              Typing_env_extension.add_equation env_extension name ty
            | Const _ | Discriminant _ -> env_extension
          in
          match canonical_simple1, canonical_simple2 with
          | Some simple1, Some simple2
              when Simple.equal simple1 simple2
                     || Meet_env.already_meeting env simple1 simple2 ->
            Equals simple1, Typing_env_extension.empty ()
          | Some simple1, _ when unknown_or_join_is_unknown unknown_or_join2 ->
            Equals simple1, Typing_env_extension.empty ()
          | _, Some simple2 when unknown_or_join_is_unknown unknown_or_join1 ->
            Equals simple2, Typing_env_extension.empty ()
          | Some simple1, Some simple2 ->
            let meet_unknown_or_join, env_extension_from_meet =
              let env = Meet_env.now_meeting env simple1 simple2 in
              meet_on_unknown_or_join env
                unknown_or_join1 unknown_or_join2
            in
            let meet_ty = S.to_type (No_alias meet_unknown_or_join) in
            let env_extension_from_meet =
              add_equation_if_on_a_name env_extension_from_meet
                simple1 meet_ty
            in
            let env_extension_from_meet =
              add_equation_if_on_a_name env_extension_from_meet
                simple2 (S.to_type (Equals simple1))
            in
            Equals simple1, env_extension_from_meet
          | Some simple1, None ->
            let meet_unknown_or_join, env_extension_from_meet =
              meet_on_unknown_or_join env
                unknown_or_join1 unknown_or_join2
            in
            let meet_ty = S.to_type (No_alias meet_unknown_or_join) in
            let env_extension_from_meet =
              add_equation_if_on_a_name env_extension_from_meet
                simple1 meet_ty
            in
            Equals simple1, env_extension_from_meet
          | None, Some simple2 ->
            let meet_unknown_or_join, env_extension_from_meet =
              meet_on_unknown_or_join env
                unknown_or_join1 unknown_or_join2
            in
            let meet_ty = S.to_type (No_alias meet_unknown_or_join) in
            let env_extension_from_meet =
              add_equation_if_on_a_name env_extension_from_meet
                simple2 meet_ty
            in
            Equals simple2, env_extension_from_meet
          | None, None ->
            let unknown_or_join, env_extension_from_meet =
              meet_on_unknown_or_join env
                unknown_or_join1 unknown_or_join2
            in
            if unknown_or_join == unknown_or_join1 then begin
              assert (match or_alias1 with No_alias _ -> true | _ -> false);
              or_alias1, env_extension_from_meet
            end else if unknown_or_join == unknown_or_join2 then begin
              assert (match or_alias2 with No_alias _ -> true | _ -> false);
              or_alias2, env_extension_from_meet
            end else begin
              No_alias unknown_or_join, env_extension_from_meet
            end
        end

      let rec join_on_unknown_or_join env
            (uj1 : S.of_kind_foo Flambda_types.unknown_or_join)
            (uj2 : S.of_kind_foo Flambda_types.unknown_or_join)
            : S.of_kind_foo Flambda_types.unknown_or_join =
        if Join_env.shortcut_precondition env && uj1 == uj2 then uj1
        else
          match uj1, uj2 with
          | Unknown, _ | _, Unknown -> Unknown
          | Join [], Join [] -> Join []
          | Join of_kind_foos1, Join of_kind_foos2 ->
            (* We rely on the invariant in flambda_type0_intf.ml.
               Everything in [of_kind_foos1] is mutually incompatible with each
               other; likewise in [of_kind_foos2]. *)
            let of_kind_foos =
              List.fold_left (fun of_kind_foos (of_kind_foo, perm1) ->
                  (* [of_kind_foo] can be compatible with at most one of the
                     elements of [of_kind_foos]. *)
                  let found_one = ref false in
                  let joined =
                    List.map (fun (of_kind_foo', perm2) ->
                        let join =
                          (* N.B. If we are here, [S.meet_or_join_of_kind_foo]
                             must be a "join" operation. *)
                          let env =
                            Join_env.compose_name_permutations env
                              ~perm_left:perm1 ~perm_right:perm2
                          in
                          S.meet_or_join_of_kind_foo env
                            of_kind_foo of_kind_foo'
                        in
                        match join with
                        | Ok (of_kind_foo, _env_extension) ->
                          if !found_one then begin
                            (* CR mshinwell: Add detail showing what was
                               wrong. *)
                            Misc.fatal_errorf "Invariant broken for [Join]"
                          end;
                          found_one := true;
                          of_kind_foo, Name_permutation.create ()
                        | Absorbing -> of_kind_foo', perm2)
                      of_kind_foos
                  in
                  if not !found_one then (of_kind_foo, perm1) :: of_kind_foos
                  else joined)
                of_kind_foos2
                of_kind_foos1
            in
            Join of_kind_foos

      and join_ty env
            (or_alias1 : S.of_kind_foo Flambda_types.ty)
            (or_alias2 : S.of_kind_foo Flambda_types.ty)
            : S.of_kind_foo Flambda_types.ty =
        if Join_env.shortcut_precondition env && or_alias1 == or_alias2
        then or_alias1
        else
          let unknown_or_join1, canonical_simple1 =
            Typing_env.resolve_aliases_and_squash_unresolved_names_on_ty'
              (Join_env.environment_on_left env)
              ~force_to_kind:S.force_to_kind
              ~print_ty
              or_alias1
          in
          let unknown_or_join2, canonical_simple2 =
            Typing_env.resolve_aliases_and_squash_unresolved_names_on_ty'
              (Join_env.environment_on_right env)
              ~force_to_kind:S.force_to_kind
              ~print_ty
              or_alias2
          in
          let all_aliases1 =
            match canonical_simple1 with
            | None -> Name.Set.empty
            | Some canonical_simple ->
              Typing_env.aliases_of_simple (Join_env.environment_on_left env)
                canonical_simple
          in
          let all_aliases2 =
            match canonical_simple2 with
            | None -> Name.Set.empty
            | Some canonical_simple ->
              Typing_env.aliases_of_simple (Join_env.environment_on_right env)
                canonical_simple
          in
          let all_aliases = Name.Set.inter all_aliases1 all_aliases2 in
          let alias_both_sides = Name.Set.choose_opt all_aliases in
          match alias_both_sides with
          | Some name -> Equals (Simple.name name)
          | None ->
            let alias1 = Name.Set.choose_opt all_aliases1 in
            let alias2 = Name.Set.choose_opt all_aliases2 in
            match alias1, alias2 with
            | Some name1, _ when unknown_or_join_is_bottom unknown_or_join2 ->
              Equals (Simple.name name1)
            | _, Some name2 when unknown_or_join_is_bottom unknown_or_join1 ->
              Equals (Simple.name name2)
            | None, None ->
              let unknown_or_join =
                join_on_unknown_or_join env
                  unknown_or_join1 unknown_or_join2
              in
              if unknown_or_join == unknown_or_join1 then begin
                assert (match or_alias1 with No_alias _ -> true | _ -> false);
                or_alias1
              end else if unknown_or_join == unknown_or_join2 then begin
                assert (match or_alias2 with No_alias _ -> true | _ -> false);
                or_alias2
              end else begin
                No_alias unknown_or_join
              end
            | _, _ ->
              let unknown_or_join =
                join_on_unknown_or_join env unknown_or_join1 unknown_or_join2
              in
              No_alias unknown_or_join

      let meet_or_join_ty env
            (or_alias1 : S.of_kind_foo Flambda_types.ty)
            (or_alias2 : S.of_kind_foo Flambda_types.ty) =
        let meet_env = Join_env.central_environment env in
        let or_alias1 : _ Flambda_types.ty =
          match or_alias1 with
          | No_alias _ | Type _ -> or_alias1
          | Equals simple ->
            let simple' =
              Simple.apply_name_permutation simple (Meet_env.perm_left meet_env)
            in
            if simple == simple' then or_alias1
            else Equals simple'
        in
        let or_alias2 : _ Flambda_types.ty =
          match or_alias2 with
          | No_alias _ | Type _ -> or_alias2
          | Equals simple ->
            let simple' =
              Simple.apply_name_permutation simple (Meet_env.perm_left meet_env)
            in
            if simple == simple' then or_alias2
            else Equals simple'
        in
        E.switch_no_bottom meet_ty join_ty env or_alias1 or_alias2
    end
  end and Meet_and_join : sig
    module Make
      (E : Either_meet_or_join_intf
        with module Join_env := Join_env
        with module Meet_env := Meet_env
        with module Typing_env_extension := Typing_env_extension) :
    sig
      val meet_or_join
         : Join_env.t
        -> Flambda_types.t
        -> Flambda_types.t
        -> Flambda_types.t * Typing_env_extension.t
    end
  end = struct
    module Make
      (E : Either_meet_or_join_intf
        with module Join_env := Join_env
        with module Meet_env := Meet_env
        with module Typing_env_extension := Typing_env_extension) =
    struct
      let meet_or_join env (t1 : Flambda_types.t) (t2 : Flambda_types.t)
            : Flambda_types.t * Typing_env_extension.t =
        let module Meet_and_join_of_kind_value =
          Meet_and_join_value.Make (E)
        in
        let module Meet_and_join_of_kind_naked_immediate =
          Meet_and_join_naked_immediate.Make (E)
        in
        let module Meet_and_join_of_kind_naked_float =
          Meet_and_join_naked_float.Make (E)
        in
        let module Meet_and_join_of_kind_naked_int32 =
          Meet_and_join_naked_int32.Make (E)
        in
        let module Meet_and_join_of_kind_naked_int64 =
          Meet_and_join_naked_int64.Make (E)
        in
        let module Meet_and_join_of_kind_naked_nativeint =
          Meet_and_join_naked_nativeint.Make (E)
        in
        let module Meet_and_join_of_kind_fabricated =
          Meet_and_join_fabricated.Make (E)
        in
        let module Meet_and_join_value =
          Make_meet_or_join.Make (E) (Meet_and_join_of_kind_value)
        in
        let module Meet_and_join_naked_immediate =
          Make_meet_or_join.Make (E) (Meet_and_join_of_kind_naked_immediate)
        in
        let module Meet_and_join_naked_float =
          Make_meet_or_join.Make (E) (Meet_and_join_of_kind_naked_float)
        in
        let module Meet_and_join_naked_int32 =
          Make_meet_or_join.Make (E) (Meet_and_join_of_kind_naked_int32)
        in
        let module Meet_and_join_naked_int64 =
          Make_meet_or_join.Make (E) (Meet_and_join_of_kind_naked_int64)
        in
        let module Meet_and_join_naked_nativeint =
          Make_meet_or_join.Make (E) (Meet_and_join_of_kind_naked_nativeint)
        in
        let module Meet_and_join_fabricated =
          Make_meet_or_join.Make (E) (Meet_and_join_of_kind_fabricated)
        in
        if Join_env.shortcut_precondition env
          && Type_equality.fast_equal t1 t2
        then t1, Typing_env_extension.empty ()
        else begin
          Join_env.invariant env;
          let t, env_extension =
            match t1, t2 with
            | Value ty_value1, Value ty_value2 ->
              let ty_value, env_extension =
                Meet_and_join_value.meet_or_join_ty env ty_value1 ty_value2
              in
              if ty_value == ty_value1 then t1, env_extension
              else if ty_value == ty_value2 then t2, env_extension
              else Flambda_types.Value ty_value, env_extension
            | Naked_number (ty_naked_number1, kind1),
                Naked_number (ty_naked_number2, kind2) ->
              let module N = K.Naked_number in
              begin match kind1, kind2 with
              | N.Naked_immediate, N.Naked_immediate ->
                let ty_naked_number, env_extension =
                  Meet_and_join_naked_immediate.meet_or_join_ty env
                    ty_naked_number1 ty_naked_number2
                in
                if ty_naked_number == ty_naked_number1 then t1, env_extension
                else if ty_naked_number == ty_naked_number2
                then t2, env_extension
                else
                  Flambda_types.Naked_number (ty_naked_number,
                      N.Naked_immediate),
                    env_extension
              | N.Naked_float, N.Naked_float ->
                let ty_naked_number, env_extension =
                  Meet_and_join_naked_float.meet_or_join_ty env
                    ty_naked_number1 ty_naked_number2
                in
                if ty_naked_number == ty_naked_number1 then t1, env_extension
                else if ty_naked_number == ty_naked_number2
                then t2, env_extension
                else
                  Flambda_types.Naked_number (ty_naked_number, N.Naked_float),
                    env_extension
              | N.Naked_int32, N.Naked_int32 ->
                let ty_naked_number, env_extension =
                  Meet_and_join_naked_int32.meet_or_join_ty env
                    ty_naked_number1 ty_naked_number2
                in
                if ty_naked_number == ty_naked_number1 then t1, env_extension
                else if ty_naked_number == ty_naked_number2
                then t2, env_extension
                else
                  Flambda_types.Naked_number (ty_naked_number, N.Naked_int32),
                    env_extension
              | N.Naked_int64, N.Naked_int64 ->
                let ty_naked_number, env_extension =
                  Meet_and_join_naked_int64.meet_or_join_ty env
                    ty_naked_number1 ty_naked_number2
                in
                if ty_naked_number == ty_naked_number1 then t1, env_extension
                else if ty_naked_number == ty_naked_number2
                then t2, env_extension
                else
                  Flambda_types.Naked_number (ty_naked_number, N.Naked_int64),
                    env_extension
              | N.Naked_nativeint, N.Naked_nativeint ->
                let ty_naked_number, env_extension =
                  Meet_and_join_naked_nativeint.meet_or_join_ty env
                    ty_naked_number1 ty_naked_number2
                in
                if ty_naked_number == ty_naked_number1 then t1, env_extension
                else if ty_naked_number == ty_naked_number2
                then t2, env_extension
                else
                  Flambda_types.Naked_number (ty_naked_number,
                      N.Naked_nativeint),
                    env_extension
              | _, _ ->
                Misc.fatal_errorf "Kind mismatch upon %s:@ %a@ versus@ %a"
                  (E.name ())
                  Type_printers.print t1
                  Type_printers.print t2
              end
            | Fabricated ty_fabricated1, Fabricated ty_fabricated2 ->
              let ty_fabricated, env_extension =
                Meet_and_join_fabricated.meet_or_join_ty env
                  ty_fabricated1 ty_fabricated2
              in
              if ty_fabricated == ty_fabricated1 then
                t1, env_extension
              else if ty_fabricated == ty_fabricated2 then
                t2, env_extension
              else
                Flambda_types.Fabricated ty_fabricated, env_extension
            | (Value _ | Naked_number _ | Fabricated _), _ ->
              Misc.fatal_errorf "Kind mismatch upon %s:@ %a@ versus@ %a"
                (E.name ())
                Type_printers.print t1
                Type_printers.print t2
          in
          t, env_extension
        end
    end
  end and Meet_and_join_fabricated : sig
    module Make
      (E : Either_meet_or_join_intf
        with module Join_env := Join_env
        with module Meet_env := Meet_env
        with module Typing_env_extension := Typing_env_extension) :
    sig
      include Meet_and_join_spec_intf
        with module Flambda_types := Flambda_types
        with module Join_env := Join_env
        with module Typing_env_extension := Typing_env_extension
        with type of_kind_foo = Flambda_types.of_kind_fabricated

      val meet_or_join_set_of_closures_entry
         : Join_env.t
        -> Flambda_types.set_of_closures_entry
        -> Flambda_types.set_of_closures_entry
        -> (Flambda_types.set_of_closures_entry * Typing_env_extension.t)
             Or_absorbing.t
    end
  end = struct
    module Make
      (E : Either_meet_or_join_intf
        with module Join_env := Join_env
        with module Meet_env := Meet_env
        with module Typing_env_extension := Typing_env_extension) =
    struct
      type of_kind_foo = Flambda_types.of_kind_fabricated

      let kind () = K.fabricated ()

      let to_type ty : Flambda_types.t = Fabricated ty

      let force_to_kind = Flambda_type0_core.force_to_kind_fabricated
      let print_ty = Type_printers.print_ty_fabricated_with_cache

      let meet_or_join_set_of_closures_entry env
              (({ by_closure_id = by_closure_id1; }
                : Flambda_types.set_of_closures_entry)
                as set_of_closures_entry1)
              (({ by_closure_id = by_closure_id2; }
                : Flambda_types.set_of_closures_entry)
                as set_of_closures_entry2) : _ Or_absorbing.t =
        if Join_env.shortcut_precondition env
          && set_of_closures_entry1 == set_of_closures_entry2
        then
          Ok (set_of_closures_entry1, Typing_env_extension.empty ())
        else
          let meet_or_join =
            E.switch Types_by_closure_id.meet Types_by_closure_id.join
              env by_closure_id1 by_closure_id2
          in
          match meet_or_join with
          | Bottom -> Absorbing
          | Ok (by_closure_id, env_extension) ->
            let set_of_closures_entry : Flambda_types.set_of_closures_entry =
              { by_closure_id; }
            in
            Ok (set_of_closures_entry, env_extension)

      let meet_or_join_of_kind_foo env
            (of_kind1 : Flambda_types.of_kind_fabricated)
            (of_kind2 : Flambda_types.of_kind_fabricated)
            : (Flambda_types.of_kind_fabricated * Typing_env_extension.t)
                Or_absorbing.t =
        if Join_env.shortcut_precondition env
          && of_kind1 == of_kind2
        then
          Ok (of_kind1, Typing_env_extension.empty ())
        else
          match of_kind1, of_kind2 with
          | Discriminants discriminants1, Discriminants discriminants2 ->
            let discriminants =
              E.switch Discriminants.meet Discriminants.join env
                discriminants1 discriminants2
            in
            begin match discriminants with
            | Bottom -> Absorbing
            | Ok (discriminants, env_extension) ->
              Ok (Discriminants discriminants, env_extension)
            end
          | Set_of_closures { closures = closures1 },
              Set_of_closures { closures = closures2 } ->
            let closures =
              E.switch Closure_ids.meet Closure_ids.join env closures1 closures2
            in
            begin match closures with
            | Bottom -> Absorbing
            | Ok (closures, env_extension) ->
              Ok (Set_of_closures { closures; }, env_extension)
            end
          | (Discriminants _ | Set_of_closures _), _ -> Absorbing
    end
  end and Meet_and_join_naked_float : sig
    include Meet_and_join_naked_number_intf
      with module Flambda_types := Flambda_types
      with module Join_env := Join_env
      with module Meet_env := Meet_env
      with module Naked_number := Float
      with module Typing_env_extension := Typing_env_extension
  end = struct
    module Make
      (E : Either_meet_or_join_intf
        with module Join_env := Join_env
        with module Meet_env := Meet_env
        with module Typing_env_extension := Typing_env_extension) =
    struct
      type of_kind_foo = Float.Set.t Flambda_types.of_kind_naked_number

      let kind () = K.naked_float ()

      let to_type ty : Flambda_types.t = Naked_number (ty, Naked_float)

      let force_to_kind = Flambda_type0_core.force_to_kind_naked_float
      let print_ty = Type_printers.print_ty_naked_float_with_cache

      let meet_or_join_of_kind_foo _meet_or_join_env
            (of_kind1 : Float.Set.t Flambda_types.of_kind_naked_number)
            (of_kind2 : Float.Set.t Flambda_types.of_kind_naked_number)
            : (Float.Set.t Flambda_types.of_kind_naked_number
                * Typing_env_extension.t) Or_absorbing.t =
        match of_kind1, of_kind2 with
        | Float fs1, Float fs2 ->
          let fs = E.Float.Set.union_or_inter fs1 fs2 in
          if Float.Set.is_empty fs then Absorbing
          else Ok (Float fs, Typing_env_extension.empty ())
        | _, _ -> Absorbing
    end
  end and Meet_and_join_naked_immediate : sig
    include Meet_and_join_naked_number_intf 
      with module Flambda_types := Flambda_types
      with module Join_env := Join_env
      with module Meet_env := Meet_env
      with module Naked_number := Immediate
      with module Typing_env_extension := Typing_env_extension
  end = struct
    module Make
      (E : Either_meet_or_join_intf
        with module Join_env := Join_env
        with module Meet_env := Meet_env
        with module Typing_env_extension := Typing_env_extension) =
    struct
      type of_kind_foo = Immediate.Set.t Flambda_types.of_kind_naked_number

      let kind () = K.naked_immediate ()

      let to_type ty : Flambda_types.t = Naked_number (ty, Naked_immediate)

      let force_to_kind = Flambda_type0_core.force_to_kind_naked_immediate
      let print_ty = Type_printers.print_ty_naked_immediate_with_cache

      let meet_or_join_of_kind_foo _meet_or_join_env
            (of_kind1 : Immediate.Set.t Flambda_types.of_kind_naked_number)
            (of_kind2 : Immediate.Set.t Flambda_types.of_kind_naked_number)
            : (Immediate.Set.t Flambda_types.of_kind_naked_number
                * Typing_env_extension.t) Or_absorbing.t =
        match of_kind1, of_kind2 with
        | Immediate fs1, Immediate fs2 ->
          let fs = E.Immediate.Set.union_or_inter fs1 fs2 in
          if Immediate.Set.is_empty fs then Absorbing
          else Ok (Immediate fs, Typing_env_extension.empty ())
        | _, _ -> Absorbing
    end
  end and Meet_and_join_naked_int32 : sig
    include Meet_and_join_naked_number_intf 
      with module Flambda_types := Flambda_types
      with module Join_env := Join_env
      with module Meet_env := Meet_env
      with module Naked_number := Int32
      with module Typing_env_extension := Typing_env_extension
  end = struct
    module Make
      (E : Either_meet_or_join_intf
        with module Join_env := Join_env
        with module Meet_env := Meet_env
        with module Typing_env_extension := Typing_env_extension) =
    struct
      type of_kind_foo = Int32.Set.t Flambda_types.of_kind_naked_number

      let kind () = K.naked_int32 ()

      let to_type ty : Flambda_types.t = Naked_number (ty, Naked_int32)

      let force_to_kind = Flambda_type0_core.force_to_kind_naked_int32
      let print_ty = Type_printers.print_ty_naked_int32_with_cache

      let meet_or_join_of_kind_foo _meet_or_join_env
            (of_kind1 : Int32.Set.t Flambda_types.of_kind_naked_number)
            (of_kind2 : Int32.Set.t Flambda_types.of_kind_naked_number)
            : (Int32.Set.t Flambda_types.of_kind_naked_number
                * Typing_env_extension.t) Or_absorbing.t =
        match of_kind1, of_kind2 with
        | Int32 fs1, Int32 fs2 ->
          let fs = E.Int32.Set.union_or_inter fs1 fs2 in
          if Int32.Set.is_empty fs then Absorbing
          else Ok (Int32 fs, Typing_env_extension.empty ())
        | _, _ -> Absorbing
    end
  end and Meet_and_join_naked_int64 : sig
    include Meet_and_join_naked_number_intf 
      with module Flambda_types := Flambda_types
      with module Join_env := Join_env
      with module Meet_env := Meet_env
      with module Naked_number := Int64
      with module Typing_env_extension := Typing_env_extension
  end = struct
    module Make
      (E : Either_meet_or_join_intf
        with module Join_env := Join_env
        with module Meet_env := Meet_env
        with module Typing_env_extension := Typing_env_extension) =
    struct
      type of_kind_foo = Int64.Set.t Flambda_types.of_kind_naked_number

      let kind () = K.naked_int64 ()

      let to_type ty : Flambda_types.t = Naked_number (ty, Naked_int64)

      let force_to_kind = Flambda_type0_core.force_to_kind_naked_int64
      let print_ty = Type_printers.print_ty_naked_int64_with_cache

      let meet_or_join_of_kind_foo _meet_or_join_env
            (of_kind1 : Int64.Set.t Flambda_types.of_kind_naked_number)
            (of_kind2 : Int64.Set.t Flambda_types.of_kind_naked_number)
            : (Int64.Set.t Flambda_types.of_kind_naked_number
                * Typing_env_extension.t) Or_absorbing.t =
        match of_kind1, of_kind2 with
        | Int64 fs1, Int64 fs2 ->
          let fs = E.Int64.Set.union_or_inter fs1 fs2 in
          if Int64.Set.is_empty fs then Absorbing
          else Ok (Int64 fs, Typing_env_extension.empty ())
        | _, _ -> Absorbing
    end
  end and Meet_and_join_naked_nativeint : sig
    include Meet_and_join_naked_number_intf 
      with module Flambda_types := Flambda_types
      with module Join_env := Join_env
      with module Meet_env := Meet_env
      with module Naked_number := Targetint
      with module Typing_env_extension := Typing_env_extension
  end = struct
    module Make
      (E : Either_meet_or_join_intf
        with module Join_env := Join_env
        with module Meet_env := Meet_env
        with module Typing_env_extension := Typing_env_extension) =
    struct
      type of_kind_foo = Targetint.Set.t Flambda_types.of_kind_naked_number

      let kind () = K.naked_nativeint ()

      let to_type ty : Flambda_types.t = Naked_number (ty, Naked_nativeint)

      let force_to_kind = Flambda_type0_core.force_to_kind_naked_nativeint
      let print_ty = Type_printers.print_ty_naked_nativeint_with_cache

      let meet_or_join_of_kind_foo _meet_or_join_env
            (of_kind1 : Targetint.Set.t Flambda_types.of_kind_naked_number)
            (of_kind2 : Targetint.Set.t Flambda_types.of_kind_naked_number)
            : (Targetint.Set.t Flambda_types.of_kind_naked_number
                * Typing_env_extension.t) Or_absorbing.t =
        match of_kind1, of_kind2 with
        | Nativeint fs1, Nativeint fs2 ->
          let fs = E.Targetint.Set.union_or_inter fs1 fs2 in
          if Targetint.Set.is_empty fs then Absorbing
          else Ok (Nativeint fs, Typing_env_extension.empty ())
        | _, _ -> Absorbing
    end
  end and Meet_and_join_value : sig
    module Make
      (E : Either_meet_or_join_intf
        with module Join_env := Join_env
        with module Meet_env := Meet_env
        with module Typing_env_extension := Typing_env_extension) :
    sig
      include Meet_and_join_spec_intf
        with module Flambda_types := Flambda_types
        with module Join_env := Join_env
        with module Typing_env_extension := Typing_env_extension
        with type of_kind_foo = Flambda_types.of_kind_value

      val meet_or_join_closures_entry
         : Join_env.t
        -> Flambda_types.closures_entry
        -> Flambda_types.closures_entry
        -> (Flambda_types.closures_entry * Typing_env_extension.t)
             Or_absorbing.t
    end
  end = struct
    module Make
      (E : Either_meet_or_join_intf
        with module Join_env := Join_env
        with module Meet_env := Meet_env
        with module Typing_env_extension := Typing_env_extension) =
    struct
      type of_kind_foo = Flambda_types.of_kind_value

      let kind () = K.value ()

      let to_type ty : Flambda_types.t = Value ty

      let force_to_kind = Flambda_type0_core.force_to_kind_value
      let print_ty = Type_printers.print_ty_value_with_cache

      let meet_or_join_blocks_and_tagged_immediates env
            ({ blocks = blocks1; immediates = immediates1; }
              : Flambda_types.blocks_and_tagged_immediates)
            ({ blocks = blocks2; immediates = immediates2; }
              : Flambda_types.blocks_and_tagged_immediates)
            : (Flambda_types.blocks_and_tagged_immediates
                * Typing_env_extension.t) Or_bottom.t =
        let blocks =
          E.switch Blocks.meet Blocks.join env blocks1 blocks2
        in
        let immediates =
          E.switch Immediates.meet Immediates.join env immediates1 immediates2
        in
        match blocks, immediates with
        | Ok (blocks, env_extension1), Ok (immediates, env_extension2) ->
          let env_extension =
            Typing_env_extension.join env env_extension1 env_extension2
          in
          Ok ({ blocks; immediates; }, env_extension)
        | Bottom, _ | _, Bottom -> Bottom

      let meet_or_join_closures_entry env
            ({ function_decl = function_decl1;
               ty = ty1;
               closure_elements = closure_elements1;
               set_of_closures = set_of_closures1;
             } : Flambda_types.closures_entry)
            ({ function_decl = function_decl2;
               ty = ty2;
               closure_elements = closure_elements2;
               set_of_closures = set_of_closures2;
             } : Flambda_types.closures_entry)
            : (Flambda_types.closures_entry * Typing_env_extension.t)
                Or_absorbing.t =
        let function_decl : Flambda_types.function_declaration =
          match function_decl1, function_decl2 with
          | Non_inlinable, (Non_inlinable | Inlinable _)
          | Inlinable _, Non_inlinable -> Non_inlinable
          | Inlinable {
              closure_origin = closure_origin1;
              continuation_param = continuation_param1;
              exn_continuation_param = exn_continuation_param1;
              is_classic_mode = is_classic_mode1;
              params = params1;
              body = _;
              code_id = code_id1;
              free_names_in_body = free_names_in_body1;
              stub = stub1;
              result_arity = result_arity1;
              dbg = dbg1;
              inline = inline1;
              specialise = specialise1;
              is_a_functor = is_a_functor1;
              invariant_params = invariant_params1;
              size = size1;
              direct_call_surrogate = direct_call_surrogate1;
              my_closure = my_closure1;
            },
            Inlinable {
              closure_origin = closure_origin2;
              continuation_param = continuation_param2;
              exn_continuation_param = exn_continuation_param2;
              is_classic_mode = is_classic_mode2;
              params = params2;
              body = _;
              code_id = code_id2;
              free_names_in_body = free_names_in_body2;
              stub = stub2;
              result_arity = result_arity2;
              dbg = dbg2;
              inline = inline2;
              specialise = specialise2;
              is_a_functor = is_a_functor2;
              invariant_params = invariant_params2;
              size = size2;
              direct_call_surrogate = direct_call_surrogate2;
              my_closure = my_closure2;
            } ->
            match E.op () with
            | Join ->
              if Code_id.equal code_id1 code_id2 then begin
                assert (Closure_origin.equal closure_origin1 closure_origin2);
                assert (Continuation.equal continuation_param1
                  continuation_param2);
                assert (Continuation.equal exn_continuation_param1
                  exn_continuation_param2);
                assert (Pervasives.(=) is_classic_mode1 is_classic_mode2);
                assert (Misc.Stdlib.List.equal Kinded_parameter.equal
                  params1 params2);
                assert (Name_occurrences.equal free_names_in_body1
                  free_names_in_body2);
                assert (Pervasives.(=) stub1 stub2);
                assert (Flambda_arity.equal result_arity1 result_arity2);
                assert (Debuginfo.equal dbg1 dbg2);
                assert (Inline_attribute.equal inline1 inline2);
                assert (Specialise_attribute.equal specialise1 specialise2);
                assert (Pervasives.(=) is_a_functor1 is_a_functor2);
                assert (Variable.Set.equal
                  (Lazy.force invariant_params1)
                  (Lazy.force invariant_params2));
                assert (Misc.Stdlib.Option.equal Pervasives.(=)
                  (Lazy.force size1) (Lazy.force size2));
                assert (Misc.Stdlib.Option.equal Closure_id.equal
                  direct_call_surrogate1 direct_call_surrogate2);
                assert (Variable.equal my_closure1 my_closure2);
                function_decl1
              end else begin
                Non_inlinable
              end
            | Meet ->
              (* We can arbitrarily pick one of the functions, since they must
                 both behave in the same way, even if we cannot prove it. *)
              function_decl1
        in
        let ty =
          E.switch Function_type.meet_fresh Function_type.join_fresh env ty1 ty2
        in
        let closure_elements =
          E.switch Closure_elements.meet Closure_elements.join env
            closure_elements1 closure_elements2
        in
        let module Meet_and_join_of_kind_fabricated =
          Meet_and_join_fabricated.Make (E)
        in
        let module Meet_and_join_fabricated =
          Make_meet_or_join.Make (E) (Meet_and_join_of_kind_fabricated)
        in
        let (set_of_closures, env_extension1) =
          Meet_and_join_fabricated.meet_or_join_ty
            env set_of_closures1 set_of_closures2
        in
        match ty, closure_elements with
        | Ok (ty, env_extension2), Ok (closure_elements, env_extension3) ->
          let env_extension =
            let env = Join_env.central_environment env in
            Typing_env_extension.meet env env_extension1
              (Typing_env_extension.meet env env_extension2 env_extension3)
          in
          let closures_entry : Flambda_types.closures_entry =
            { function_decl;
              ty;
              closure_elements;
              set_of_closures;
            }
          in
          Ok (closures_entry, env_extension)
        | _, _ -> Absorbing

      let meet_or_join_of_kind_foo env
            (of_kind1 : Flambda_types.of_kind_value)
            (of_kind2 : Flambda_types.of_kind_value)
            : (Flambda_types.of_kind_value * Typing_env_extension.t)
                Or_absorbing.t =
        if Join_env.shortcut_precondition env
          && of_kind1 == of_kind2
        then
          Ok (of_kind1, Typing_env_extension.empty ())
        else
          let module Meet_and_join_of_kind_naked_immediate =
            Meet_and_join_naked_immediate.Make (E)
          in
          let module Meet_and_join_of_kind_naked_float =
            Meet_and_join_naked_float.Make (E)
          in
          let module Meet_and_join_of_kind_naked_int32 =
            Meet_and_join_naked_int32.Make (E)
          in
          let module Meet_and_join_of_kind_naked_int64 =
            Meet_and_join_naked_int64.Make (E)
          in
          let module Meet_and_join_of_kind_naked_nativeint =
            Meet_and_join_naked_nativeint.Make (E)
          in
          let module Meet_and_join_naked_immediate =
            Make_meet_or_join.Make (E) (Meet_and_join_of_kind_naked_immediate)
          in
          let module Meet_and_join_naked_float =
            Make_meet_or_join.Make (E) (Meet_and_join_of_kind_naked_float)
          in
          let module Meet_and_join_naked_int32 =
            Make_meet_or_join.Make (E) (Meet_and_join_of_kind_naked_int32)
          in
          let module Meet_and_join_naked_int64 =
            Make_meet_or_join.Make (E) (Meet_and_join_of_kind_naked_int64)
          in
          let module Meet_and_join_naked_nativeint =
            Make_meet_or_join.Make (E) (Meet_and_join_of_kind_naked_nativeint)
          in
          match of_kind1, of_kind2 with
          | Blocks_and_tagged_immediates blocks_imms1,
              Blocks_and_tagged_immediates blocks_imms2 ->
            let blocks_imms =
              meet_or_join_blocks_and_tagged_immediates env
                blocks_imms1 blocks_imms2
            in
            begin match blocks_imms with
            | Ok (blocks_imms, equations) ->
              Ok (Blocks_and_tagged_immediates blocks_imms, equations)
            | Bottom -> Absorbing
            end
          | Boxed_number (Boxed_float n1),
              Boxed_number (Boxed_float n2) ->
            let (n : _ Flambda_types.ty_naked_number), equations =
              Meet_and_join_naked_float.meet_or_join_ty env n1 n2
            in
            Ok (Boxed_number (Boxed_float n), equations)
          | Boxed_number (Boxed_int32 n1),
            Boxed_number (Boxed_int32 n2) ->
            let (n : _ Flambda_types.ty_naked_number), equations =
              Meet_and_join_naked_int32.meet_or_join_ty env n1 n2
            in
            Ok (Boxed_number (Boxed_int32 n), equations)
          | Boxed_number (Boxed_int64 n1),
              Boxed_number (Boxed_int64 n2) ->
            let (n : _ Flambda_types.ty_naked_number), equations =
              Meet_and_join_naked_int64.meet_or_join_ty env n1 n2
            in
            Ok (Boxed_number (Boxed_int64 n), equations)
          | Boxed_number (Boxed_nativeint n1),
              Boxed_number (Boxed_nativeint n2) ->
            let (n : _ Flambda_types.ty_naked_number), equations =
              Meet_and_join_naked_nativeint.meet_or_join_ty env
                n1 n2
            in
            Ok (Boxed_number (Boxed_nativeint n), equations)
          | Closures { by_closure_id = by_closure_id1; },
              Closures { by_closure_id = by_closure_id2; } ->
            let by_closure_id =
              E.switch Closures_entry_by_closure_id.meet
                Closures_entry_by_closure_id.join
                env by_closure_id1 by_closure_id2
            in
            begin match by_closure_id with
            | Ok (by_closure_id, env_extension) ->
              Ok (Closures { by_closure_id; }, env_extension)
            | Bottom -> Absorbing
            end
          | String strs1, String strs2 ->
            let strs = E.String_info.Set.union_or_inter strs1 strs2 in
            if String_info.Set.is_empty strs then Absorbing
            else Ok (String strs, Typing_env_extension.empty ())
          | (Blocks_and_tagged_immediates _
              | Boxed_number _
              | Closures _
              | String _), _ ->
            Absorbing
    end
  end and Meet_env : sig
    type t

    val create
       : Typing_env.t
      -> perm_left:Name_permutation.t
      -> perm_right:Name_permutation.t
      -> t

    val print : Format.formatter -> t -> unit

    val env : t -> Typing_env.t

    val perm_left : t -> Name_permutation.t

    val perm_right : t -> Name_permutation.t

    (** Note that we are now in the process of meeting the given two
        [Simple]s. *)
    val now_meeting : t -> Simple.t -> Simple.t -> t

    (** Determine whether we are now in the process of meeting the given two
        [Simple]s.  The arguments do not have to be provided in the same order
        as when [now_meeting] was called. *)
    val already_meeting : t -> Simple.t -> Simple.t -> bool

    val shortcut_precondition : t -> bool

    val with_env : t -> (Typing_env.t -> Typing_env.t) -> t

    val clear_name_permutations : t -> t

    val compose_name_permutations
       : t
      -> perm_left:Name_permutation.t
      -> perm_right:Name_permutation.t
      -> t
  end = struct
    module Simple_pair = Hashtbl.Make_with_map_pair (Simple) (Simple)

    type t = {
      env : Typing_env.t;
      perm_left : Name_permutation.t;
      perm_right : Name_permutation.t;
      already_meeting : Simple_pair.Set.t;
    }

    let print ppf { env; perm_left; perm_right; already_meeting; } =
      Format.fprintf ppf
        "@[<hov 1>(\
          @[<hov 1>(env@ %a)@]@ \
          @[<hov 1>(perm_left@ %a)@]@ \
          @[<hov 1>(perm_right@ %a)@]@ \
          @[<hov 1>(already_meeting@ %a)@])@]"
        Typing_env.print env
        Name_permutation.print perm_left
        Name_permutation.print perm_right
        Simple_pair.Set.print already_meeting

    let create env ~perm_left ~perm_right =
      { env;
        perm_left;
        perm_right;
        already_meeting = Simple_pair.Set.empty;
      }

    let env t = t.env

    let perm_left t = t.perm_left
    let perm_right t = t.perm_right

    let fast_check_name_permutations_same_both_sides t =
      t.perm_left == t.perm_right

    let already_meeting t simple1 simple2 =
      Simple_pair.Set.mem (simple1, simple2) t.already_meeting
        || Simple_pair.Set.mem (simple2, simple1) t.already_meeting

    let now_meeting t simple1 simple2 =
      if already_meeting t simple1 simple2 then begin
        Misc.fatal_errorf "Already meeting %a and %a:@ %a"
          Simple.print simple1
          Simple.print simple2
          print t
      end;
      let already_meeting =
        Simple_pair.Set.add (simple1, simple2) t.already_meeting
      in
      { t with
        already_meeting;
      }

    let shortcut_precondition t =
      fast_check_name_permutations_same_both_sides t

    let with_env t f =
      { t with env = f t.env; }

    let clear_name_permutations t =
      { t with
        perm_left = Name_permutation.create ();
        perm_right = Name_permutation.create ();
      }

    let compose_name_permutations t ~perm_left ~perm_right =
      { t with
        perm_left = Name_permutation.compose t.perm_left perm_left;
        perm_right = Name_permutation.compose t.perm_right perm_right;
      }
  end and Parameters : sig
    type t

    include Contains_names.S with type t := t

    val print_or_omit_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> t
      -> unit

    val create : unit -> t

    val to_set : t -> Kinded_parameter.Set.t

(*
    (** Perform invariant checks upon the given function type. *)
    val invariant : t -> unit

    (** Create a representation of the names, order and type of a function's
        parameters. *)
    val create : (Kinded_parameter.t * Flambda_types.t) list -> t

    (** A conservative approximation to equality. *)
    val equal : Type_equality_env.t -> t -> t -> bool

    (** Greatest lower bound of two parameter lists. *)
    val meet : Meet_env.t -> t -> t -> (t * Typing_env_extension.t) Or_bottom.t

    (** Least upper bound of two parameter lists. *)
    val join : Join_env.t -> t -> t -> t
*)
  end = struct
    module KP = struct
      include Kinded_parameter

      let create kind =
        create (Parameter.wrap (Variable.create "param")) kind

      let apply_name_permutation t perm =
        Kinded_parameter.apply_name_permutation t perm
      
      let free_names t = Kinded_parameter.free_names t

      let equal env t1 t2 =
        let t1 =
          apply_name_permutation t1 (Type_equality_env.perm_left env)
        in
        let t2 =
          apply_name_permutation t2 (Type_equality_env.perm_right env)
        in
        equal t1 t2
    end

    module RP = Relational_product.Make (Int_index) (KP)

    type t = RP.t

    let _invariant = RP.invariant

    let print = RP.print

    let to_list t =
      match RP.components t with
      | [params] -> params
      | _ -> Misc.fatal_errorf "Wrong form of relational product:@ %a" print t

    let to_set t = Kinded_parameter.Set.of_list (to_list t)

    let print_or_omit_with_cache ~cache:_ ppf t =
      match to_list t with
      | [] -> ()
      | params ->
        Format.fprintf ppf "@[(%a)@]"
          (Format.pp_print_list ~pp_sep:Format.pp_print_space
            Kinded_parameter.print)
          params

    let _equal = RP.equal

    let _meet env t1 t2 = RP.meet env Fresh t1 t2
    let _join env t1 t2 = RP.join env Fresh t1 t2

    let _introduce = RP.introduce

    let free_names = RP.free_names
    let apply_name_permutation = RP.apply_name_permutation

    let create () =
      RP.create [Targetint.OCaml.Map.empty, Typing_env_extension.empty ()]

    let _create parameters =
      let indexes_to_parameters =
        Targetint.OCaml.Map.of_list (
          List.mapi (fun index (param, _ty) ->
              Targetint.OCaml.of_int index, param)
            parameters)
      in
      if List.length parameters
           <> Targetint.OCaml.Map.cardinal indexes_to_parameters
      then begin
        Misc.fatal_errorf "Duplicate parameter(s):@ %a"
          (Targetint.OCaml.Map.print Kinded_parameter.print)
          indexes_to_parameters
      end;
      let env_extension =
        List.fold_left (fun env_extension (param, ty) ->
            Typing_env_extension.add_equation env_extension
              (Kinded_parameter.name param) ty)
          (Typing_env_extension.empty ())
          parameters
      in
      RP.create [
        indexes_to_parameters, env_extension
      ]
  end and Relational_product : sig
    (* CR mshinwell: Update comment to reflect new binding structure *)
    (** A "relational product" represents a list of indexed products.  Each
        indexed product binds a set of components, thus:

           ------
            |  |
            |  |     (component_i : Component)
          i : Index

        and additionally holds relational information between the components
        expressed as a typing environment extension.

        Any indexed product in a relational product may depend on components'
        names bound by an earlier indexed product.  The overall structure is
        thus:

           ------    ------
            |  |      |  |
            |  |      |  |     (component_i_n : Component)
           n : int  i_n : Index

        where the outer (dependent) product corresponds to the list structure.
    *)

    module Make
      (Index : Name_like_intf.S)
      (Component : sig
        include Name_like_intf.S
        val create : Flambda_kind.t -> t
        val equal : Type_equality_env.t -> t -> t -> bool
        val name : t -> Name.t
        val kind : t -> Flambda_kind.t
        module Set : Contains_names.S
      end) :
    sig
      type t

      include Contains_names.S with type t := t

      (** Perform invariant checks upon the given relational product. *)
      val invariant : t -> unit

      (** Format the given relational product value as an s-expression. *)
      val print : Format.formatter -> t -> unit

      val print_with_cache
         : cache:Printing_cache.t
        -> Format.formatter
        -> t
        -> unit

      (** Create a relational product value given:
          - the indexes (with associated components) for the product;
          - the equations that hold between the components in the product;
          - any other relational product over which the newly-created one is
            to be scoped.  The newly-created one will bind references to
            components in the nested one. *)
      val create
         : ?nested:t option
        -> Component.t Index.Map.t
        -> Typing_env_extension.t
        -> t

      val create_bottom : unit -> t

      (** A conservative approximation to equality. *)
      val equal : Type_equality_env.t -> t -> t -> bool

      (** Greatest lower bound of two relational products. *)
      val meet
         : Meet_env.t
        -> t
        -> t
        -> (t * Typing_env_extension.t) Or_bottom.t

      (** Least upper bound of two relational products. *)
      val join : Join_env.t -> t -> t -> t

      (** The environment extension associated with the given relational
          product, including at the start, definitions of each component to
          bottom (hence the name "standalone"). *)
      val standalone_extension
         : t
        -> Typing_env.t
        -> Typing_env_extension.t

      (** Add or meet the definitions and equations from the given relational
          product value into the given typing environment. *)
      val introduce : t -> Typing_env.t -> Typing_env.t

      (** Add or meet the given equations into the environment extension held
          within the relational product. *)
      val add_or_meet_equations
         : t
        -> Meet_env.t
        -> Typing_env_extension.t
        -> t
    end
  end = struct
    module Make
      (Index : Name_like_intf.S)
      (Component : sig
        include Name_like_intf.S
        val create : Flambda_kind.t -> t
        val equal : Type_equality_env.t -> t -> t -> bool
        val name : t -> Name.t
        val kind : t -> Flambda_kind.t
      end) =
    struct
      module rec T0 : sig
        include Contains_names.S

      end = struct
        type t = {
          components_by_index : Component.t Index.Map.t;
          env_extension : Typing_env_extension.t;
          nested : T.t;
        }

        let invariant _t =
          (* CR mshinwell: This should check that the [env_extension] never
             contains [Definition]s for [Name]s occurring in the indexes. *)
          ()

        let bottom () =
          create Index.Map.empty (Typing_env_extension.empty ())

        let print ppf { components_by_index; env_extension; } =
          Format.fprintf ppf
            "@[<hov 1>(\
              @[<hov 1>(components_by_index@ %a)@]@ \
              @[<hov 1>(env_extension@ %a)@])@]"
            (Index.Map.print Component.print) components_by_index
            Typing_env_extension.print env_extension

        let print_with_cache ~cache ppf
              { components_by_index; env_extension; } =
          Format.fprintf ppf
            "@[<hov 1>(\
              @[<hov 1>(components_by_index@ %a)@]@ \
              @[<hov 1>(env_extension@ %a)@])@]"
            (Index.Map.print Component.print) components_by_index
            (Typing_env_extension.print_with_cache ~cache) env_extension

        let equal env
              { components_by_index = components_by_index1;
                env_extension = env_extension1;
                nested = nested1;
              }
              { components_by_index = components_by_index2;
                env_extension = env_extension2;
                nested = nested2;
              } =
          Index.Map.equal (Component.equal env)
            components_by_index1 components_by_index2
          && Typing_env_extension.equal env env_extension1 env_extension2
          && T.equal nested1 nested2

        let free_names
              ({ components_by_index; env_extension; nested; } as t) =
          let free_names_in_indexes =
            Index.Set.fold (fun index free_names ->
                Name_occurrences.union (Index.free_names index) free_names)
              (Index.Map.keys components_by_index)
              (Name_occurrences.create ())
          in
          let free_names_in_components =
            List.fold_left (fun free_names component ->
                Name_occurrences.union (Component.free_names component)
                  free_names)
              (Name_occurrences.create ())
              (Index.Map.data components_by_index)
          in
          let free_names_in_nested =
            match nested with
            | None -> Name_occurrences.create ()
            | Some nested -> T.free_names nested
          in
          Name_occurrences.union_list [
            free_names_in_indexes t;
            free_names_in_components t;
            Typing_env_extension.free_names env_extension;
            free_names_in_nested;
          ]

        let apply_name_permutation
              { components_by_index; env_extension; nested; } perm =
          let components_by_index =
            Index.Map.fold (fun index component components_by_index ->
                let index = Index.apply_name_permutation index perm in
                let component =
                  Component.apply_name_permutation component perm
                in
                Index.Map.add index component components_by_index)
              components_by_index
              Index.Map.empty
          in
          let env_extension =
            Typing_env_extension.apply_name_permutation env_extension perm
          in
          let nested =
            match nested with
            | None -> None
            | Some nested -> Some (T.apply_name_permutation nested perm)
          in
          { components_by_index;
            env_extension;
            nested;
          }

        let indexes t = Index.Map.keys t.components_by_index

        (* CR mshinwell: The [kind] may not be needed in [Component] but it
           isn't clear yet. We can sort this out later. At present all
           relational products map to components of kind [Value]. *)
        let kind = Flambda_kind.value ()

        let environment_for_meet_or_join env (t1 : t) (t2 : t)
              ~indexes =
          let components_by_index_in_result, env =
            Index.Set.fold (fun index (components_by_index_in_result, env) ->
                let component = Component.create kind in
                let components_by_index_in_result =
                  Index.Map.add index component components_by_index_in_result
                in
                let env =
                  Join_env.add_definition_central_environment env
                    (Component.name component) (Flambda_type0_core.bottom kind)
                in
                components_by_index_in_result, env)
              indexes
              (Index.Map.empty, env)
          in
          let result_components =
            Component.Set.of_list (Index.Map.data components_by_index_in_result)
          in
          let add_definitions_to_extension t =
            Index.Map.fold (fun _index component env_extension ->
                if Component.Set.mem component result_components then
                  env_extension
                else
                  let name = Component.name component in
                  let kind = Component.kind component in
                  Typing_env_extension.add_definition_at_beginning env_extension
                    name (Flambda_type0_core.bottom kind))
              t.components_by_index
              t.env_extension
          in
          let env_extension1 = add_definitions_to_extension t1 in
          let env_extension2 = add_definitions_to_extension t2 in
          let add_equalities_to_extension t env_extension =
            Index.Map.fold (fun index component env_extension ->
                let name = Component.name component in
                let kind = Component.kind component in
                match Index.Map.find index t.components_by_index with
                | exception Not_found -> env_extension
                | stale_component ->
                  let env = Type_equality_env.empty () in
                  if Component.equal env component stale_component
                  then env_extension
                  else
                    let stale_name = Component.name stale_component in
                    let name_ty =
                      Flambda_type0_core.alias_type_of kind
                        (Simple.name stale_name)
                    in
                    Typing_env_extension.add_equation env_extension
                      name name_ty)
              components_by_index_in_result
              env_extension
          in
          let env_extension1 = add_equalities_to_extension t1 env_extension1 in
          let env_extension2 = add_equalities_to_extension t2 env_extension2 in
          env, env_extension1, env_extension2, components_by_index_in_result,
            result_components

        let meet env t1 t2 : _ Or_bottom.t =
          if Meet_env.shortcut_precondition env && t1 == t2 then
            Ok (t1, env, None)
          else
            let indexes = Index.Set.inter (indexes t1) (indexes t2) in
            if Index.Set.is_empty indexes then Bottom
            else
              let env = Join_env.create env in
              let env, env_extension1, env_extension2, components_by_index,
                  result_components =
                environment_for_meet_or_join env t1 t2 ~indexes
              in
              let env = Join_env.central_environment env in
              let env_extension =
                Typing_env_extension.meet env env_extension1 env_extension2
              in
              let nested : _ Or_bottom.t =
                match t1.nested, t2.nested with
                | None, None -> Ok None
                | Some nested1, Some nested2 ->
                  begin match T.meet env nested1 nested2 with
                  | Ok nested -> Ok (Some nested)
                  | Bottom -> Bottom
                  end
              | None, Some _ | Some _, None ->
                Misc.fatal_errorf "Cannot meet relational products with \
                    different nesting structures:@ %a@ and@ %a"
                  print t1
                  print t2
              in
              match nested with
              | Bottom -> Bottom
              | Ok nested ->
                let t =
                  { components_by_index;
                    env_extension;
                    nested;
                  }
                in
                let env_extension = Typing_env_extension.create () in
                Ok (t, env_extension, Some result_components)

        let join env t1 t2 =
          if Join_env.shortcut_precondition env && t1 == t2 then
            t1, env, t1.env_extension, t1.env_extension, None
          else
            let indexes = Index.Set.union (indexes t1) (indexes t2) in
            let env, env_extension1, env_extension2, components_by_index,
                result_components =
              environment_for_meet_or_join env t1 t2 ~indexes
            in
            let env_extension =
              Typing_env_extension.join env env_extension1 env_extension2
            in
            let nested =
              match t1.nested, t2.nested with
              | None, None -> None
              | Some nested1, Some nested2 -> Some (T.join env nested1 nested2)
              | None, Some _ | Some _, None ->
                Misc.fatal_errorf "Cannot join relational products with \
                    different nesting structures:@ %a@ and@ %a"
                  print t1
                  print t2
            in
            let t =
              { components_by_index;
                env_extension;
                nested;
              }
            in
            t, env, env_extension1, env_extension2, Some result_components

        let standalone_extension t =
          Index.Map.fold (fun _index component env_extension ->
              let name = Component.name component in
              let kind = Component.kind component in
              Typing_env_extension.add_definition_at_beginning env_extension
                name (Flambda_type0_core.bottom kind))
            t.components_by_index
            t.env_extension

        let introduce t env =
          Typing_env.add_or_meet_env_extension env (standalone_extension t)
            (Typing_env.max_level env)

        let add_or_meet_equations t env new_equations =
          let env_extension =
            Typing_env_extension.meet env t.env_extension new_equations
          in
          { t with env_extension; }
      end and T : sig
        include Contains_names.S
        val invariant : t -> unit
        val print : Format.formatter -> t -> unit
        val print_with_cache
           : cache:Printing_cache.t
          -> Format.formatter
          -> t
          -> unit
        val create
           : ?nested:t option
          -> Component.t Index.Map.t
          -> Typing_env_extension.t
          -> t
        val create_bottom : unit -> t
        val equal : Type_equality_env.t -> t -> t -> bool
        val meet
           : Meet_env.t
          -> t
          -> t
          -> (t * Typing_env_extension.t) Or_bottom.t
        val join : Join_env.t -> t -> t -> t
        val standalone_extension
           : t
          -> Typing_env.t
          -> Typing_env_extension.t
        val introduce : t -> Typing_env.t -> Typing_env.t
        val add_or_meet_equations
           : t
          -> Meet_env.t
          -> Typing_env_extension.t
          -> t
      end = struct
        include Name_abstraction.Make (Component.Set) (T0)

        let create_abstraction = create

        let create components_by_index env_extension nested : t =
          let components = Index.Map.data components_by_index in
          let component_set = Component.Set.of_list components in
          if List.length components <> Component.Set.cardinal component_set
          then begin
            Misc.fatal_error "Can only create a relational product from \
              distinct components"
          end;
          let t0 : T0.t =
            { components_by_index;
              env_extension;
              nested;
            }
          in
          T0.invariant t0;
          A.create component_set t0

        let create_bottom () =
          create Index.Map.empty (Typing_env_extension.empty ())

        let invariant t =
          pattern_match t ~f:(fun _ t0 -> T0.invariant t0)

        let equal t1 t2 =
          pattern_match_pair t1 t2 ~f:(fun components t0_1 t0_2 ->
            T0.equal t0_1 t0_2)

        let meet env t1 t2 =
          pattern_match t1 ~f:(fun components1 t0_1 ->
            pattern_match t2 ~f:(fun components2 t0_2 ->
              match T0.meet env t0_1 t0_2 with
              | Bottom -> Bottom
              | Ok (t0, env_extension, Some components) ->
                Ok (create_abstraction components t0, env_extension)))

        let join env t1 t2 =
          pattern_match t1 ~f:(fun components1 t0_1 ->
            pattern_match t2 ~f:(fun components2 t0_2 ->
              let t0 = T0.join env t0_1 t0_2 in
              create_abstraction components t0))

        let standalone_extension t =
          pattern_match t ~f:(fun _ t0 -> T0.standalone_extension t0)

        let introduce t env =
          pattern_match t ~f:(fun _ t0 -> T0.introduce t0 env)

        let add_or_meet_equations t env new_equations =
          pattern_match t ~f:(fun _ t0 ->
            T0.add_or_meet_equations t0 env new_equations)
      end

      include T
    end
  end and Row_like : sig
    module Make
      (Tag : Hashtbl.With_map)
      (Index : Hashtbl.With_map)
      (Tag_and_index : sig
        (** These values will not contain any names. *)
        type t = Tag.t * Index.t
        include Hashtbl.With_map with type t := t
      end)
      (Maps_to : sig
        type t

        val bottom : unit -> t

        val print_with_cache
           : cache:Printing_cache.t
          -> Format.formatter
          -> t
          -> unit

        val equal : Type_equality_env.t -> t -> t -> bool

        val add_or_meet_equations
           : t
          -> Meet_env.t
          -> Typing_env_extension.t
          -> t

        val meet
           : Meet_env.t
          -> Relational_product.fresh_component_semantics
          -> t
          -> t
          -> (t * Typing_env_extension.t) Or_bottom.t

        val join
           : Join_env.t
          -> Relational_product.fresh_component_semantics
          -> t
          -> t
          -> t

        include Contains_names.S with type t := t
      end) :
    sig
      type t

      val print : cache:Printing_cache.t -> Format.formatter -> t -> unit

(*
      val create : unit -> t
*)

      val create_exactly : Tag.t -> Index.t -> Maps_to.t -> t

      val create_exactly_multiple : Maps_to.t Tag_and_index.Map.t -> t

      val create_at_least : Index.t -> Maps_to.t -> t

      val create_at_least_multiple : Maps_to.t Index.Map.t -> t

      val is_bottom : t -> bool

      val equal : Type_equality_env.t -> t -> t -> bool

      (** The [Maps_to] value which [meet] returns contains the join of all
          [Maps_to] values in the range of the row-like structure after the meet
          operation has been completed. *)
      val meet
         : Meet_env.t
        -> Relational_product.fresh_component_semantics
        -> t
        -> t
        -> (t * Maps_to.t) Or_bottom.t

      val join
         : Join_env.t
        -> Relational_product.fresh_component_semantics
        -> t
        -> t
        -> t

      val known : t -> Maps_to.t Tag_and_index.Map.t

      val at_least : t -> Maps_to.t Index.Map.t

      val get_singleton : t -> (Tag_and_index.t * Maps_to.t) option

      include Contains_names.S with type t := t
    end
  end = struct
    module Make
      (Tag : Hashtbl.With_map)
      (Index : Hashtbl.With_map)
      (Tag_and_index : sig
        type t = Tag.t * Index.t
        include Hashtbl.With_map with type t := t
      end)
      (Maps_to : sig
        type t

        val bottom : unit -> t

        val print_with_cache
           : cache:Printing_cache.t
          -> Format.formatter
          -> t
          -> unit

        val equal : Type_equality_env.t -> t -> t -> bool

        val add_or_meet_equations
           : t
          -> Meet_env.t
          -> Typing_env_extension.t
          -> t

        val meet
           : Meet_env.t
          -> Relational_product.fresh_component_semantics
          -> t
          -> t
          -> (t * Typing_env_extension.t) Or_bottom.t

        val join
           : Join_env.t
          -> Relational_product.fresh_component_semantics
          -> t
          -> t
          -> t

        include Contains_names.S with type t := t
      end) =
    struct
      module Tag_and_index = struct
        include Tag_and_index

        let create tag index = tag, index
        let index (_tag, index) = index
      end

      (* CR mshinwell: Think about what means bottom and what means unknown for
         this structure *)
      type t = {
        known : Maps_to.t Tag_and_index.Map.t;
        at_least : Maps_to.t Index.Map.t;
      }

      let print ~cache ppf ({ known; at_least } : t) =
        Format.fprintf ppf 
          "@[<hov 1>(\
             @[<hov 1>(known@ %a)@]@ \
             @[<hov 1>(at_least@ %a)@])@]"
          (Tag_and_index.Map.print (Maps_to.print_with_cache ~cache)) known
          (Index.Map.print (Maps_to.print_with_cache ~cache)) at_least

      let _create () =
        { known = Tag_and_index.Map.empty;
          at_least = Index.Map.empty;
        }

      let create_exactly tag index maps_to =
        let tag_and_index = Tag_and_index.create tag index in
        { known = Tag_and_index.Map.singleton tag_and_index maps_to;
          at_least = Index.Map.empty;
        }

      let create_exactly_multiple known =
        { known;
          at_least = Index.Map.empty;
        }

      let create_at_least index maps_to =
        { known = Tag_and_index.Map.empty;
          at_least = Index.Map.singleton index maps_to;
        }

      let create_at_least_multiple at_least =
        { known = Tag_and_index.Map.empty;
          at_least;
        }

      let equal env
            { known = known1; at_least = at_least1; }
            { known = known2; at_least = at_least2; } =
        Tag_and_index.Map.equal (Maps_to.equal env) known1 known2
          && Index.Map.equal (Maps_to.equal env) at_least1 at_least2

      let apply_name_permutation { known; at_least; } perm =
        let known =
          (* CR mshinwell: Can just use [Tag_and_index.Map.map] now. *)
          Tag_and_index.Map.fold (fun tag_and_index maps_to known ->
              let maps_to = Maps_to.apply_name_permutation maps_to perm in
              Tag_and_index.Map.add tag_and_index maps_to known)
            known
            Tag_and_index.Map.empty
        in
        let at_least =
          Index.Map.fold (fun index maps_to at_least ->
              let maps_to = Maps_to.apply_name_permutation maps_to perm in
              Index.Map.add index maps_to at_least)
            at_least
            Index.Map.empty
        in
        { known;
          at_least;
        }

      module Meet_or_join
        (E : Either_meet_or_join_intf
          with module Join_env := Join_env
          with module Meet_env := Meet_env
          with module Typing_env_extension := Typing_env_extension) =
      struct
        let meet_or_join env fresh_component_semantics t1 t2 =
          let t1 = apply_name_permutation t1 (Join_env.perm_left env) in
          let t2 = apply_name_permutation t2 (Join_env.perm_right env) in
          let env = Join_env.clear_name_permutations env in
          let ({ known = known1; at_least = at_least1; } : t) = t1 in
          let ({ known = known2; at_least = at_least2; } : t) = t2 in
          let one_side_only index1 maps_to1 at_least2
                ~get_equations_to_deposit1 =
            let from_at_least2 =
              Index.Map.find_last_opt
                (fun index -> Index.compare index index1 <= 0)
                at_least2
            in
            begin match from_at_least2 with
            | None ->
              begin match E.op () with
              | Meet -> None
              | Join ->
                let maps_to1 =
                  Maps_to.add_or_meet_equations
                    maps_to1
                    (Join_env.central_environment env)
                    (get_equations_to_deposit1 env)
                in
                Some maps_to1
              end
            | Some (index2, from_at_least2) ->
              assert (Index.compare index2 index1 <= 0);
              (* CR mshinwell: What happens to any generated equations in the
                 [meet] case (same below)? *)
              let maps_to =
                E.switch'_with_param Maps_to.meet Maps_to.join env
                  fresh_component_semantics maps_to1 from_at_least2
              in
              match maps_to with
              | Bottom -> None
              | Ok maps_to -> Some maps_to
            end
          in
          let merge index maps_to1 maps_to2 =
            match maps_to1, maps_to2 with
            | Some maps_to1, None ->
              one_side_only index maps_to1 at_least2
                ~get_equations_to_deposit1:Join_env.holds_on_left
            | None, Some maps_to2 ->
              one_side_only index maps_to2 at_least1
                ~get_equations_to_deposit1:Join_env.holds_on_right
            | Some maps_to1, Some maps_to2 ->
              let maps_to =
                E.switch'_with_param Maps_to.meet Maps_to.join env
                  fresh_component_semantics maps_to1 maps_to2
              in
              begin match maps_to with
              | Bottom -> None
              | Ok maps_to -> Some maps_to
              end
            | None, None -> None
          in
          (* CR mshinwell: Shouldn't we be applying name permutations to
             these two as well? *)
          let known =
            Tag_and_index.Map.merge (fun tag_and_index maps_to1 maps_to2 ->
                let index = Tag_and_index.index tag_and_index in
                merge index maps_to1 maps_to2)
              known1
              known2
          in
          let at_least =
            Index.Map.merge (fun index maps_to1 maps_to2 ->
                merge index maps_to1 maps_to2)
              at_least1
              at_least2
          in
          { known;
            at_least;
          }
      end

      let all_maps_to { known; at_least; } =
        (Tag_and_index.Map.data known) @ (Index.Map.data at_least)

      module Meet = Meet_or_join (Either_meet_or_join.For_meet)
      module Join = Meet_or_join (Either_meet_or_join.For_join)

      let meet env fresh_component_semantics t1 t2 : _ Or_bottom.t =
        let t =
          Meet.meet_or_join (Join_env.create env)
            fresh_component_semantics t1 t2
        in
        if Tag_and_index.Map.is_empty t.known && Index.Map.is_empty t.at_least
        then Bottom
        else
          let join_of_all_maps_to =
            (* Any name permutations have already been applied during
               [Meet.meet_or_join], above. *)
            let env = Join_env.clear_name_permutations (Join_env.create env) in
            List.fold_left (fun result maps_to ->
                Maps_to.join env fresh_component_semantics maps_to result)
              (Maps_to.bottom ())
              (all_maps_to t)
          in
          Ok (t, join_of_all_maps_to)

      let join = Join.meet_or_join

      let is_bottom { known; at_least; } =
        Tag_and_index.Map.is_empty known && Index.Map.is_empty at_least

      let known t = t.known
      let at_least t = t.at_least

      let get_singleton { known; at_least; } =
        if not (Index.Map.is_empty at_least) then None
        else Tag_and_index.Map.get_singleton known

      let free_names t =
        let { known; at_least; } = t in
        let from_known =
          Tag_and_index.Map.fold (fun _tag_and_index maps_to free_names ->
              Name_occurrences.union free_names
                (Maps_to.free_names maps_to))
            known
            (Name_occurrences.create ())
        in
        let from_at_least =
          Index.Map.fold (fun _index maps_to free_names ->
              Name_occurrences.union free_names
                (Maps_to.free_names maps_to))
            at_least
            (Name_occurrences.create ())
        in
        Name_occurrences.union from_known from_at_least
    end
  end and Trivial_row_like : sig
    module Make (Thing_without_names : Hashtbl.With_map) : sig
      type t

      val print : cache:Printing_cache.t -> Format.formatter -> t -> unit

      (** Create a value which describes the presence of exactly no things. *)
      val create_bottom : unit -> t

      (** Create a value which describes the presence of an unknown set of
          things. *)
      val create_unknown : unit -> t

      val create : Thing_without_names.Set.t -> t

      val create_with_equations
         : Typing_env_extension.t Thing_without_names.Map.t
        -> t

      val equal : Type_equality_env.t -> t -> t -> bool

      val meet
         : Meet_env.t
        -> t
        -> t
        -> (t * Typing_env_extension.t) Or_bottom.t

      val join
         : Join_env.t
        -> t
        -> t
        -> t

      val all : t -> Thing_without_names.Set.t Or_unknown.t

      val get_singleton
         : t
        -> (Thing_without_names.t * Typing_env_extension.t) option

      include Contains_names.S with type t := t
    end
  end = struct
    module Make (Thing_without_names : Hashtbl.With_map) = struct
      module TEE = struct
        include Typing_env_extension

        let add_or_meet_equations t env t' =
          meet env t t'

        let meet env _ t1 t2 : _ Or_bottom.t =
          let t = meet env t1 t2 in
          if is_empty t then Bottom
          else Ok (t, empty ())

        let join env _ t1 t2 = join env t1 t2

        let bottom () = empty ()
      end

      module Thing_without_names_and_unit =
        Hashtbl.Make_with_map_pair (Thing_without_names) (Unit)

      module RL =
        Row_like.Make (Thing_without_names) (Unit)
          (Thing_without_names_and_unit) (TEE)

      type t = RL.t

      let create_with_equations things_with_env_extensions =
        let things_with_env_extensions =
          Thing_without_names.Map.fold (fun thing extension result ->
              Thing_without_names_and_unit.Map.add (thing, ()) extension result)
            things_with_env_extensions
            Thing_without_names_and_unit.Map.empty
        in
        RL.create_exactly_multiple things_with_env_extensions

      let create things =
        let things_with_env_extensions =
          Thing_without_names.Map.of_set (fun _thing -> TEE.empty ()) things
        in
        create_with_equations things_with_env_extensions

      let create_bottom () =
        create Thing_without_names.Set.empty

      let create_unknown () =
        RL.create_at_least_multiple Unit.Map.empty

      let print = RL.print
      let equal = RL.equal

      let meet env t1 t2 = RL.meet env Fresh t1 t2
      let join env t1 t2 = RL.join env Fresh t1 t2

      let free_names = RL.free_names
      let apply_name_permutation = RL.apply_name_permutation

      let all t : _ Or_unknown.t =
        let indexes = RL.at_least t in
        if not (Unit.Map.is_empty indexes) then Unknown
        else
          let things =
            Thing_without_names_and_unit.Set.fold (fun (thing, ()) things ->
                Thing_without_names.Set.add thing things)
              (Thing_without_names_and_unit.Map.keys (RL.known t))
              Thing_without_names.Set.empty
          in
          Known things

      let get_singleton t =
        match RL.get_singleton t with
        | None -> None
        | Some ((thing, ()), env_extension) -> Some (thing, env_extension)
    end
  end and Type_equality : sig
    val fast_equal : Flambda_types.t -> Flambda_types.t -> bool

    val equal : Flambda_types.t -> Flambda_types.t -> bool

    (* CR mshinwell: Rename to [equal_in_env]. *)
    val equal_with_env
       : Type_equality_env.t
      -> Flambda_types.t
      -> Flambda_types.t
      -> bool

    val equal_closures_entry
       : Type_equality_env.t
      -> Flambda_types.closures_entry
      -> Flambda_types.closures_entry
      -> bool

    val equal_set_of_closures_entry
       : Type_equality_env.t
      -> Flambda_types.set_of_closures_entry
      -> Flambda_types.set_of_closures_entry
      -> bool
  end = struct
    module Float = Numbers.Float_by_bit_pattern
    module Int32 = Numbers.Int32
    module Int64 = Numbers.Int64

    let fast_equal (t1 : Flambda_types.t) (t2 : Flambda_types.t) =
      t1 == t2

    let equal_or_alias equal_contents env
          (or_alias1 : _ Flambda_types.or_alias)
          (or_alias2 : _ Flambda_types.or_alias) =
      match or_alias1, or_alias2 with
      | No_alias contents1, No_alias contents2 ->
        equal_contents env contents1 contents2
      | Type export_id1, Type export_id2 ->
        Export_id.equal export_id1 export_id2
      | Equals simple1, Equals simple2 ->
        let simple1 =
          Simple.apply_name_permutation simple1
            (Type_equality_env.perm_left env)
        in
        let simple2 =
          Simple.apply_name_permutation simple2
            (Type_equality_env.perm_right env)
        in
        Simple.equal simple1 simple2
      | (No_alias _ | Type _ | Equals _), _ -> false

    let equal_unknown_or_join equal_of_kind_foo env
          (uj1 : _ Flambda_types.unknown_or_join)
          (uj2 : _ Flambda_types.unknown_or_join) =
      match uj1, uj2 with
      | Unknown, Unknown -> true
      | Join join1, Join join2 ->
        Misc.Stdlib.List.equal (equal_of_kind_foo env) join1 join2
      | Unknown, _
      | Join _, _ -> false

    let equal_ty equal_of_kind_foo env ty1 ty2 =
      equal_or_alias (equal_unknown_or_join equal_of_kind_foo) env ty1 ty2

    let rec equal_with_env env (t1 : Flambda_types.t) (t2 : Flambda_types.t) =
      match t1, t2 with
      | Value ty_value1, Value ty_value2 ->
        equal_ty_value env ty_value1 ty_value2
      | Naked_number (ty_naked_number1, Naked_immediate),
          Naked_number (ty_naked_number2, Naked_immediate) ->
        equal_ty_naked_number env ty_naked_number1 ty_naked_number2
      | Naked_number (ty_naked_number1, Naked_float),
          Naked_number (ty_naked_number2, Naked_float) ->
        equal_ty_naked_number env ty_naked_number1 ty_naked_number2
      | Naked_number (ty_naked_number1, Naked_int32),
          Naked_number (ty_naked_number2, Naked_int32) ->
        equal_ty_naked_number env ty_naked_number1 ty_naked_number2
      | Naked_number (ty_naked_number1, Naked_int64),
          Naked_number (ty_naked_number2, Naked_int64) ->
        equal_ty_naked_number env ty_naked_number1 ty_naked_number2
      | Naked_number (ty_naked_number1, Naked_nativeint),
          Naked_number (ty_naked_number2, Naked_nativeint) ->
        equal_ty_naked_number env ty_naked_number1 ty_naked_number2
      | Fabricated ty_fabricated1, Fabricated ty_fabricated2 ->
        equal_ty_fabricated env ty_fabricated1 ty_fabricated2
      | Value _, _ -> false
      | Naked_number _, _ -> false
      | Fabricated _, _ -> false

    and equal_ty_value env ty_value1 ty_value2 =
      equal_ty equal_of_kind_value env ty_value1 ty_value2

    and equal_ty_naked_number
       : type a.
         Type_equality_env.t
      -> a Flambda_types.ty_naked_number
      -> a Flambda_types.ty_naked_number
      -> bool =
    fun env
        (ty_naked_number1 : a Flambda_types.ty_naked_number)
        (ty_naked_number2 : a Flambda_types.ty_naked_number) ->
      equal_or_alias (equal_unknown_or_join equal_of_kind_naked_number) env
        ty_naked_number1 ty_naked_number2

    and equal_ty_fabricated env ty_fabricated1 ty_fabricated2 =
      equal_ty equal_of_kind_fabricated env ty_fabricated1 ty_fabricated2

    and equal_of_kind_value env ((v1 : Flambda_types.of_kind_value), perm1)
          ((v2 : Flambda_types.of_kind_value), perm2) =
      let env =
        Type_equality_env.compose_name_permutations env
          ~perm_left:perm1 ~perm_right:perm2
      in
      match v1, v2 with
      | Blocks_and_tagged_immediates blocks1,
          Blocks_and_tagged_immediates blocks2 ->
        equal_blocks_and_tagged_immediates env blocks1 blocks2
      | Boxed_number (Boxed_float ty_naked_number1),
          Boxed_number (Boxed_float ty_naked_number2) ->
        equal_ty_naked_number env ty_naked_number1 ty_naked_number2
      | Boxed_number (Boxed_int32 ty_naked_number1),
          Boxed_number (Boxed_int32 ty_naked_number2) ->
        equal_ty_naked_number env ty_naked_number1 ty_naked_number2
      | Boxed_number (Boxed_int64 ty_naked_number1),
          Boxed_number (Boxed_int64 ty_naked_number2) ->
        equal_ty_naked_number env ty_naked_number1 ty_naked_number2
      | Boxed_number (Boxed_nativeint ty_naked_number1),
          Boxed_number (Boxed_nativeint ty_naked_number2) ->
        equal_ty_naked_number env ty_naked_number1 ty_naked_number2
      | Closures { by_closure_id = by_closure_id1; },
          Closures { by_closure_id = by_closure_id2; } ->
        Closures_entry_by_closure_id.equal env by_closure_id1 by_closure_id2
      | String string_set1, String string_set2 ->
        String_info.Set.equal string_set1 string_set2
      | (Blocks_and_tagged_immediates _ | Boxed_number _
          | Closures _ | String _), _ -> false

    and equal_blocks_and_tagged_immediates env
          ({ immediates = immediates1; blocks = blocks1; }
            : Flambda_types.blocks_and_tagged_immediates)
          ({ immediates = immediates2; blocks = blocks2; }
            : Flambda_types.blocks_and_tagged_immediates) =
      Immediates.equal env immediates1 immediates2
        && Blocks.equal env blocks1 blocks2

    and equal_function_declaration _env
          (decl1 : Flambda_types.function_declaration)
          (decl2 : Flambda_types.function_declaration) =
      match decl1, decl2 with
      | Inlinable decl1, Inlinable decl2 ->
        (* CR mshinwell: Add assertions like in the meet/join code? *)
        Code_id.equal decl1.code_id decl2.code_id
      | Non_inlinable, Non_inlinable -> true
      | Inlinable _, Non_inlinable
      | Non_inlinable, Inlinable _ -> false

    and equal_of_kind_naked_number
       : type a b.
         Type_equality_env.t
      -> (a Flambda_types.of_kind_naked_number * Name_permutation.t)
      -> (b Flambda_types.of_kind_naked_number * Name_permutation.t)
      -> bool =
    fun _env (of_kind_naked_number1, _) (of_kind_naked_number2, _) ->
      match of_kind_naked_number1, of_kind_naked_number2 with
      | Immediate imms1, Immediate imms2 -> Immediate.Set.equal imms1 imms2
      | Float floats1, Float floats2 -> Float.Set.equal floats1 floats2
      | Int32 ints1, Int32 ints2 -> Int32.Set.equal ints1 ints2
      | Int64 ints1, Int64 ints2 -> Int64.Set.equal ints1 ints2
      | Nativeint ints1, Nativeint ints2 -> Targetint.Set.equal ints1 ints2
      | Immediate _, _ -> false
      | Float _, _ -> false
      | Int32 _, _ -> false
      | Int64 _, _ -> false
      | Nativeint _, _ -> false

    and equal_of_kind_fabricated env
          ((of_kind_fabricated1 : Flambda_types.of_kind_fabricated), perm1)
          ((of_kind_fabricated2 : Flambda_types.of_kind_fabricated), perm2) =
      let env =
        Type_equality_env.compose_name_permutations env
          ~perm_left:perm1 ~perm_right:perm2
      in
      match of_kind_fabricated1, of_kind_fabricated2 with
      | Discriminants discrs1, Discriminants discrs2 ->
        Discriminants.equal env discrs1 discrs2
      | Set_of_closures { closures = closures1; },
          Set_of_closures { closures = closures2; } ->
        Closure_ids.equal env closures1 closures2
      | (Discriminants _ | Set_of_closures _), _ -> false

    and equal_closures_entry env
          ({ function_decl = function_decl1;
             ty = ty1;
             closure_elements = closure_elements1;
             set_of_closures = set_of_closures1;
           } : Flambda_types.closures_entry)
          ({ function_decl = function_decl2;
             ty = ty2;
             closure_elements = closure_elements2;
             set_of_closures = set_of_closures2;
           } : Flambda_types.closures_entry) =
      equal_function_declaration env function_decl1 function_decl2
        && Function_type.equal env ty1 ty2
        && Closure_elements.equal env closure_elements1 closure_elements2
        && equal_ty_fabricated env set_of_closures1 set_of_closures2

    and equal_set_of_closures_entry env
          ({ by_closure_id = by_closure_id1; }
            : Flambda_types.set_of_closures_entry)
          ({ by_closure_id = by_closure_id2; }
            : Flambda_types.set_of_closures_entry) =
      Types_by_closure_id.equal env by_closure_id1 by_closure_id2

    let equal t1 t2 = equal_with_env (Type_equality_env.empty ()) t1 t2
  end and Type_free_names : sig
    val free_names : Flambda_types.t -> Name_occurrences.t

    val free_names_of_ty_fabricated
       : Flambda_types.ty_fabricated
      -> Name_occurrences.t
  end = struct
    let free_names_or_alias free_names_contents
          (or_alias : _ Flambda_types.or_alias) : Name_occurrences.t =
      match or_alias with
      | No_alias contents -> free_names_contents contents
      | Type _export_id -> Name_occurrences.create ()
      | Equals simple -> Simple.free_names simple

    let free_names_unknown_or_join free_names_contents
          (o : _ Flambda_types.unknown_or_join) : Name_occurrences.t =
      match o with
      | Unknown -> Name_occurrences.create ()
      | Join contents_list ->
        List.fold_left (fun free_names (contents, perm) ->
            let names =
              Name_occurrences.apply_name_permutation
                (free_names_contents contents)
                perm
            in
            Name_occurrences.union names free_names)
          (Name_occurrences.create ())
          contents_list

    let free_names_ty free_names_contents ty : Name_occurrences.t =
      free_names_or_alias (free_names_unknown_or_join free_names_contents) ty

    let free_names_of_kind_naked_number (type n)
          (_ty : n Flambda_types.of_kind_naked_number) =
      Name_occurrences.create ()

    let rec free_names (t : Flambda_types.t) =
      match t with
      | Value ty -> free_names_ty free_names_of_kind_value ty
      | Naked_number (ty, _kind) ->
        free_names_ty free_names_of_kind_naked_number ty
      | Fabricated ty -> free_names_ty free_names_of_kind_fabricated ty

    and free_names_of_kind_value (of_kind : Flambda_types.of_kind_value)
          : Name_occurrences.t =
      match of_kind with
      | Blocks_and_tagged_immediates { blocks; immediates; } ->
        Name_occurrences.union (Blocks.free_names blocks)
          (Immediates.free_names immediates)
      | Boxed_number (Boxed_float n) ->
        free_names_ty free_names_of_kind_naked_number n
      | Boxed_number (Boxed_int32 n) ->
        free_names_ty free_names_of_kind_naked_number n
      | Boxed_number (Boxed_int64 n) ->
        free_names_ty free_names_of_kind_naked_number n
      | Boxed_number (Boxed_nativeint n) ->
        free_names_ty free_names_of_kind_naked_number n
      | Closures { by_closure_id; } ->
        Closures_entry_by_closure_id.free_names by_closure_id
      | String _ -> Name_occurrences.create ()

    and free_names_of_kind_fabricated
          (of_kind : Flambda_types.of_kind_fabricated) =
      match of_kind with
      | Discriminants discrs -> Discriminants.free_names discrs
      | Set_of_closures { closures; } -> Closure_ids.free_names closures

    let free_names_of_ty_fabricated ty =
      free_names_ty free_names_of_kind_fabricated ty
  end and Type_printers : sig
    val print : Format.formatter -> Flambda_types.t -> unit

    val print_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> Flambda_types.t
      -> unit

    val print_ty_value_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> Flambda_types.ty_value
      -> unit

    val print_ty_naked_immediate_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> Immediate.Set.t Flambda_types.ty_naked_number
      -> unit

    val print_ty_naked_float_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> Numbers.Float_by_bit_pattern.Set.t Flambda_types.ty_naked_number
      -> unit

    val print_ty_naked_int32_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> Numbers.Int32.Set.t Flambda_types.ty_naked_number
      -> unit

    val print_ty_naked_int64_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> Numbers.Int64.Set.t Flambda_types.ty_naked_number
      -> unit

    val print_ty_naked_nativeint_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> Targetint.Set.t Flambda_types.ty_naked_number
      -> unit

    val print_ty_fabricated_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> Flambda_types.ty_fabricated
      -> unit

    val print_function_declaration_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> Flambda_types.function_declaration
      -> unit
  end = struct
    let print_or_alias print_descr ppf (or_alias : _ Flambda_types.or_alias) =
      match or_alias with
      | No_alias descr -> print_descr ppf descr
      | Equals simple ->
        Format.fprintf ppf "@[(%s=%s %a)@]"
          (Misc_color.bold_red ())
          (Misc_color.reset ())
          Simple.print simple
      | Type export_id ->
        Format.fprintf ppf "@[(%s=export_id%s %a)@]"
          (Misc_color.bold_red ())
          (Misc_color.reset ())
          Export_id.print export_id

    let unicode = true  (* CR mshinwell: move elsewhere *)

    let print_unknown_or_join print_contents ppf
          (o : _ Flambda_types.unknown_or_join) =
      let colour = Misc_color.bold_red () in
      match o with
      | Unknown -> Format.fprintf ppf "%sT%s" colour (Misc_color.reset ())
      | Join [] ->
        if unicode then
          Format.fprintf ppf "%s\u{22a5}%s" colour (Misc_color.reset ())
        else
          Format.fprintf ppf "%s_|_%s" colour (Misc_color.reset ())
      | Join [contents] -> print_contents ppf contents
      | Join incompatibles ->
        Format.fprintf ppf "@[(Join_incompatible@ (%a))@]"
          (Format.pp_print_list print_contents) incompatibles

    let print_ty_generic print_contents ppf ty =
      (print_or_alias (print_unknown_or_join print_contents)) ppf ty

    let print_of_kind_naked_number (type n) ppf
          ((n : n Flambda_types.of_kind_naked_number), _perm) =
      match n with
      | Immediate i ->
        Format.fprintf ppf "@[(Naked_immediates@ (%a))@]"
          Immediate.Set.print i
      | Float f ->
        Format.fprintf ppf "@[(Naked_floats@ (%a))@]"
          Numbers.Float_by_bit_pattern.Set.print f
      | Int32 i ->
        Format.fprintf ppf "@[(Naked_int32s@ (%a))@]"
          Numbers.Int32.Set.print i
      | Int64 i ->
        Format.fprintf ppf "@[(Naked_int64s@ (%a))@]"
          Numbers.Int64.Set.print i
      | Nativeint i ->
        Format.fprintf ppf "@[(Naked_nativeints@ (%a))@]"
          Targetint.Set.print i

    let print_ty_naked_number (type n) ppf
          (ty : n Flambda_types.ty_naked_number) =
      print_ty_generic print_of_kind_naked_number ppf ty

    let print_ty_naked_immediate_with_cache ~cache:_ ppf ty =
      print_ty_generic (print_of_kind_naked_number) ppf ty

    let print_ty_naked_int32_with_cache ~cache:_ ppf ty =
      print_ty_generic (print_of_kind_naked_number) ppf ty

    let print_ty_naked_int64_with_cache ~cache:_ ppf ty =
      print_ty_generic (print_of_kind_naked_number) ppf ty

    let print_ty_naked_nativeint_with_cache ~cache:_ ppf ty =
      print_ty_generic (print_of_kind_naked_number) ppf ty

    let print_ty_naked_float_with_cache ~cache:_ ppf ty =
      print_ty_generic (print_of_kind_naked_number) ppf ty

    let print_of_kind_value_boxed_number (type n)
          ppf (n : n Flambda_types.of_kind_value_boxed_number) =
      match n with
      | Boxed_float f ->
        Format.fprintf ppf "@[(Boxed_float@ (%a))@]"
          print_ty_naked_number f
      | Boxed_int32 i ->
        Format.fprintf ppf "@[(Boxed_int32@ (%a))@]"
          print_ty_naked_number i
      | Boxed_int64 i ->
        Format.fprintf ppf "@[(Boxed_int64@ (%a))@]"
          print_ty_naked_number i
      | Boxed_nativeint i ->
        Format.fprintf ppf "@[(Boxed_nativeint@ (%a))@]"
          print_ty_naked_number i

    let rec print_of_kind_value ~cache ppf
            ((of_kind_value : Flambda_types.of_kind_value), _) =
      match of_kind_value with
      | Blocks_and_tagged_immediates { blocks; immediates; } ->
        (* CR mshinwell: Improve so that we elide blocks and/or immediates when
           they're empty.  Similarly we can elide the extensions when empty. *)
        Format.fprintf ppf
          "@[<hov 1>(Blocks_and_immediates@ \
            @[<hov 1>(blocks@ %a)@]@ \
            @[<hov 1>(immediates@ %a)@])@]"
          (Blocks.print_with_cache ~cache) blocks
          (Immediates.print ~cache) immediates
      | Boxed_number n ->
        Format.fprintf ppf "@[(Boxed_number %a)@]"
          print_of_kind_value_boxed_number n
      | Closures { by_closure_id; } ->
        Closures_entry_by_closure_id.print ~cache ppf by_closure_id
      | String str_infos ->
        Format.fprintf ppf "@[(Strings (%a))@]" String_info.Set.print str_infos

    and print_ty_value_with_cache ~cache ppf (ty : Flambda_types.ty_value) =
      print_ty_generic (print_of_kind_value ~cache) ppf ty

    and print_inlinable_function_declaration_with_cache ~cache ppf
          (({ closure_origin;
             continuation_param;
             exn_continuation_param;
             is_classic_mode;
             params;
             body;
             code_id;
             free_names_in_body;
             stub;
             result_arity;
             dbg;
             inline;
             specialise;
             is_a_functor;
             invariant_params;
             size;
             direct_call_surrogate;
             my_closure;
           } : Flambda_types.inlinable_function_declaration) as decl) =
      Printing_cache.with_cache cache ppf "inlinable_fundecl" decl
        (fun ppf () ->
          Format.fprintf ppf
            "@[<hov 1>(Inlinable@ \
              @[<hov 1>(closure_origin@ %a)@]@ \
              @[<hov 1>(continuation_param@ %a)@]@ \
              @[<hov 1>(exn_continuation_param@ %a)@]@ \
              @[<hov 1>(is_classic_mode@ %b)@]@ \
              @[<hov 1>(params@ %a)@]@ \
              @[<hov 1>(body@ %a)@]@ \
              @[<hov 1>(code_id@ %a)@]@ \
              @[<hov 1>(free_names_in_body@ %a)@]@ \
              @[<hov 1>(stub@ %b)@]@ \
              @[<hov 1>(result_arity@ %a)@]@ \
              @[<hov 1>(dbg@ %a)@]@ \
              @[<hov 1>(inline@ %a)@]@ \
              @[<hov 1>(specialise@ %a)@]@ \
              @[<hov 1>(is_a_functor@ %b)@]@ \
              @[<hov 1>(invariant_params@ %a)@]@ \
              @[<hov 1>(size@ %a)@]@ \
              @[<hov 1>(direct_call_surrogate@ %a)@]@ \
              @[<hov 1>(my_closure@ %a)@])@]"
            Closure_origin.print closure_origin
            Continuation.print continuation_param
            Continuation.print exn_continuation_param
            is_classic_mode
            Kinded_parameter.List.print params
            (Expr.print_with_cache ~cache) body
            Code_id.print code_id
            Name_occurrences.print free_names_in_body
            stub
            Flambda_arity.print result_arity
            Debuginfo.print_compact dbg
            Inline_attribute.print inline
            Specialise_attribute.print specialise
            is_a_functor
            Variable.Set.print (Lazy.force invariant_params)
            (Misc.Stdlib.Option.print Format.pp_print_int) (Lazy.force size)
            (Misc.Stdlib.Option.print Closure_id.print) direct_call_surrogate
            Variable.print my_closure)

    and print_function_declaration_with_cache ~cache ppf
          (decl : Flambda_types.function_declaration) =
      match decl with
      | Inlinable decl ->
        print_inlinable_function_declaration_with_cache ~cache ppf decl
      | Non_inlinable -> Format.pp_print_string ppf "Non_inlinable"

    and print_of_kind_fabricated ~cache ppf
          ((o : Flambda_types.of_kind_fabricated), _) =
      match o with
      | Discriminants discriminants ->
        Format.fprintf ppf "@[<hov 1>(Discriminants@ %a)@]"
          (Discriminants.print ~cache) discriminants
      | Set_of_closures { closures; } ->
        Closure_ids.print ~cache ppf closures

    and print_ty_fabricated_with_cache ~cache ppf
          (ty : Flambda_types.ty_fabricated) =
      print_ty_generic (print_of_kind_fabricated ~cache) ppf ty

    and print_with_cache ~cache ppf (t : Flambda_types.t) =
      match t with
      | Value ty ->
        Format.fprintf ppf "@[<hov 1>(Val@ %a)@]"
          (print_ty_value_with_cache ~cache) ty
      | Naked_number (ty, _kind) ->
        Format.fprintf ppf "@[<hov 1>(Naked@ %a)@]" print_ty_naked_number ty
      | Fabricated ty ->
        Format.fprintf ppf "@[<hov 1>(Fab@ %a)@]"
          (print_ty_fabricated_with_cache ~cache) ty

    and print ppf t =
      let cache : Printing_cache.t = Printing_cache.create () in
      print_with_cache ~cache ppf t
  end and Types_by_closure_id : sig
    type t

    val print : cache:Printing_cache.t -> Format.formatter -> t -> unit

    val create : Flambda_types.t Closure_id.Map.t -> t

    val create_bottom : unit -> t

    val equal : Type_equality_env.t -> t -> t -> bool

    (** Greatest lower bound of two values of type [t]. *)
    val meet
       : Meet_env.t
      -> t
      -> t
      -> (t * Typing_env_extension.t) Or_bottom.t

    (** Least upper bound of two values of type [t]. *)
    val join
       : Join_env.t
      -> t
      -> t
      -> t

    val add_or_meet_equations
       : t
      -> Meet_env.t
      -> Typing_env_extension.t
      -> t

    include Contains_names.S with type t := t
  end = struct
    (* CR mshinwell: Share with closures_entry_by_closure_id.ml *)
    module Closure_id = struct
      include Closure_id

      let free_names _t = Name_occurrences.create ()
      let apply_name_permutation t _perm = t
    end

    module RP =
      Relational_product.Make (Closure_id) (Logical_variable_component)

    type t = RP.t

    let create closure_ids_to_tys =
      let closure_ids_to_logical_variables =
        Closure_id.Map.map (fun _ty ->
            Logical_variable.create (Flambda_kind.value ()))
          closure_ids_to_tys
      in
      let env_extension =
        Closure_id.Map.fold (fun closure_id ty env_extension ->
            let logical_var =
              Closure_id.Map.find closure_id closure_ids_to_logical_variables
            in
            Typing_env_extension.add_equation env_extension
              (Name.logical_var logical_var) ty)
          closure_ids_to_tys
          (Typing_env_extension.empty ())
      in
      RP.create [
        closure_ids_to_logical_variables, env_extension;
      ]

    let create_bottom () = RP.create_bottom ~arity:1

    let print ~cache ppf t = RP.print_with_cache ~cache ppf t

    let meet env t1 t2 = RP.meet env Fresh t1 t2
    let join env t1 t2 = RP.join env Fresh t1 t2

    let equal = RP.equal
    let free_names = RP.free_names
    let apply_name_permutation = RP.apply_name_permutation
    let add_or_meet_equations = RP.add_or_meet_equations
  end and Typing_env : sig 
    type t

    type binding_type = Normal | Was_existential

    type typing_environment_entry0 =
      | Definition of Flambda_types.t
      | Equation of Flambda_types.t

    type typing_environment_entry =
      | Definition of Flambda_types.t
      | Equation of Flambda_types.t
      | CSE of Flambda_primitive.With_fixed_value.t
        (* CR mshinwell: Consider removing "of t" for [Definition] (and maybe
           change it to [Introduce_name] -- the "t" would be implicitly
           bottom) *)

    type levels_to_entries =
      (Name.t * typing_environment_entry)
        Scope_level.Sublevel.Map.t Scope_level.Map.t

    val invariant : t -> unit

    val print : Format.formatter -> t -> unit

    val print_levels_to_entries_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> levels_to_entries
      -> unit

    val create : resolver:(Export_id.t -> Flambda_types.t option) -> t

    val create_using_resolver_from : t -> t

    val resolver : t -> (Export_id.t -> Flambda_types.t option)

    val is_empty : t -> bool

    val fast_equal : t -> t -> bool

    val domain : t -> Name_occurrences.t

    val max_level : t -> Scope_level.t

    val add : t -> Name.t -> Scope_level.t -> typing_environment_entry -> t

    val singleton
       : resolver:(Export_id.t -> Flambda_types.t option)
      -> Name.t
      -> Scope_level.t
      -> typing_environment_entry
      -> t

    val add_equation : t -> Name.t -> Scope_level.t -> Flambda_types.t -> t

    val remove : t -> Name.t -> t

    val find_exn
       : t
      -> Name.t
      -> Flambda_types.t * binding_type

    val find_with_scope_level_exn
       : t
      -> Name.t
      -> Flambda_types.t * Scope_level.With_sublevel.t * binding_type

    val find_opt : t -> Name.t -> (Flambda_types.t * binding_type) option

    val find_cse : t -> Flambda_primitive.t -> Simple.t option

    val scope_level_exn : t -> Name.t -> Scope_level.With_sublevel.t

    val mem : t -> Name.t -> bool

    val was_existential_exn : t -> Name.t -> bool

    val fold
       : t
      -> init:'a
      -> f:('a
        -> Name.t
        -> binding_type
        -> Scope_level.With_sublevel.t
        -> typing_environment_entry0
        -> 'a)
      -> 'a

    val iter
       : t
      -> f:(Name.t
        -> binding_type
        -> Scope_level.With_sublevel.t
        -> typing_environment_entry0
        -> unit)
      -> unit

    val cut
       : t
      -> existential_if_defined_at_or_later_than:Scope_level.t
      -> Typing_env_extension.t

    val restrict_to_symbols : t -> t

    val filter
       : t
      -> f:(Name.t
        -> (Scope_level.With_sublevel.t * typing_environment_entry0)
        -> bool)
      -> t

    val add_or_meet_env_extension
       : t
      -> Typing_env_extension.t
      -> Scope_level.t
      -> t

    val add_or_meet_env_extension'
       : t
      -> Typing_env_extension.t
      -> Scope_level.t
      -> t * Name_permutation.t

    val resolve_aliases
       : ?bound_name:Name.t
      -> t
      -> Flambda_types.t
      -> Flambda_types.t * (Simple.t option)

    val resolve_aliases_and_squash_unresolved_names_on_ty'
       : t
      -> ?bound_name:Name.t
      -> print_ty:(Format.formatter -> 'a Flambda_types.ty -> unit)
      -> force_to_kind:(Flambda_types.t -> 'a Flambda_types.ty)
      -> 'a Flambda_types.ty
      -> 'a Flambda_types.unknown_or_join * (Simple.t option)

    val aliases_of_simple : t -> Simple.t -> Name.Set.t
  end = struct
    type binding_type = Normal | Was_existential

    (* CR mshinwell: rename "typing_environment" -> "typing_env" *)

    type typing_environment_entry0 =
      | Definition of Flambda_types.t
      | Equation of Flambda_types.t

    type typing_environment_entry =
      | Definition of Flambda_types.t
      | Equation of Flambda_types.t
      | CSE of Flambda_primitive.With_fixed_value.t

    type levels_to_entries =
      (Name.t * typing_environment_entry)
        Scope_level.Sublevel.Map.t Scope_level.Map.t

    type t = {
      resolver : (Export_id.t -> Flambda_types.t option);
      aliases : Name.Set.t Simple.Map.t;
      (* CR mshinwell: Rename names_to_types -> names_to_entries *)
      names_to_types :
        (Scope_level.With_sublevel.t * typing_environment_entry0) Name.Map.t;
      cse : Simple.t Flambda_primitive.With_fixed_value.Map.t;
      levels_to_entries : levels_to_entries;
      next_sublevel_by_level : Scope_level.Sublevel.t Scope_level.Map.t;
      were_existentials : Name.Set.t;
    }

    let print_typing_environment_entry0_with_cache ~cache ppf
          (entry : typing_environment_entry0) =
      match entry with
      | Definition ty ->
        Format.fprintf ppf "@[(Definition %a)@]"
          (Type_printers.print_with_cache ~cache) ty
      | Equation ty ->
        Format.fprintf ppf "@[(Equation %a)@]"
          (Type_printers.print_with_cache ~cache) ty

    let print_typing_environment_entry_with_cache ~cache ppf
          (entry : typing_environment_entry) =
      match entry with
      | Definition ty ->
        print_typing_environment_entry0_with_cache ~cache ppf
          ((Definition ty) : typing_environment_entry0)
      | Equation ty ->
        print_typing_environment_entry0_with_cache ~cache ppf
          ((Equation ty) : typing_environment_entry0)
      | CSE with_fixed_value ->
        Format.fprintf ppf "@[(CSE %a)@]"
          Flambda_primitive.With_fixed_value.print with_fixed_value

    let print_typing_environment_entry ppf entry =
      print_typing_environment_entry_with_cache
        ~cache:(Printing_cache.create ()) ppf entry

    let print_levels_to_entries_with_cache ~cache ppf levels_to_entries =
      Scope_level.Map.print (
        Scope_level.Sublevel.Map.print (fun ppf (name, entry) ->
          Format.fprintf ppf "@[(%a %a)@]"
            Name.print name
            (print_typing_environment_entry_with_cache ~cache) entry))
        ppf levels_to_entries

    let print_with_cache ~cache ppf
          ({ resolver = _; aliases; names_to_types; cse; levels_to_entries;
            next_sublevel_by_level = _; were_existentials; } as t) =
      if Name.Map.is_empty names_to_types then
        Format.pp_print_string ppf "Empty"
      else
        Printing_cache.with_cache cache ppf "env" t (fun ppf () ->
          let print_scope_level_and_entry0 ppf (_scope_level, entry) =
            print_typing_environment_entry0_with_cache ~cache ppf entry
          in
          (* CR mshinwell: Add flag to disable this filtering *)
          let names_to_types =
            Name.Map.filter (fun name _entry ->
                not (Name.is_predefined_exception name))
              names_to_types
          in
          let levels_to_entries =
            Scope_level.Map.filter_map (fun _cont_level by_sublevel ->
                let by_sublevel =
                  Scope_level.Sublevel.Map.filter_map
                    (fun _sublevel ((name, _) as entry) ->
                      if not (Name.is_predefined_exception name) then Some entry
                      else None)
                  by_sublevel
                in
                if Scope_level.Sublevel.Map.is_empty by_sublevel then None
                else Some by_sublevel)
              levels_to_entries
          in
          if Name.Set.is_empty were_existentials
              && Flambda_primitive.With_fixed_value.Map.is_empty cse
          then
            Format.fprintf ppf
              "@[<hov 1>(\
                  @[<hov 1>(aliases@ %a)@]@ \
                  @[<hov 1>(names_to_types@ %a)@]@ \
                  @[<hov 1>(levels_to_entries@ %a)@])@]"
              (Simple.Map.print Name.Set.print) aliases
              (Name.Map.print print_scope_level_and_entry0) names_to_types
              (print_levels_to_entries_with_cache ~cache) levels_to_entries
          else if Name.Set.is_empty were_existentials then
            Format.fprintf ppf
              "@[<hov 1>(\
                  @[<hov 1>(aliases@ %a)@]@ \
                  @[<hov 1>(names_to_types@ %a)@]@ \
                  @[<hov 1>(cse@ %a)@]@ \
                  @[<hov 1>(levels_to_entries@ %a)@])@]"
              (Simple.Map.print Name.Set.print) aliases
              (Name.Map.print print_scope_level_and_entry0) names_to_types
              (Flambda_primitive.With_fixed_value.Map.print Simple.print) cse
              (print_levels_to_entries_with_cache ~cache) levels_to_entries
          else
            Format.fprintf ppf
              "@[<hov 1>(\
                  @[<hov 1>(aliases@ %a)@]@ \
                  @[<hov 1>(names_to_types@ %a)@]@ \
                  @[<hov 1>(cse@ %a)@]@ \
                  @[<hov 1>(levels_to_names@ %a)@]@ \
                  @[<hov 1>(were_existentials@ %a)@])@]"
              (Simple.Map.print Name.Set.print) aliases
              (Name.Map.print print_scope_level_and_entry0) names_to_types
              (Flambda_primitive.With_fixed_value.Map.print Simple.print) cse
              (print_levels_to_entries_with_cache ~cache) levels_to_entries
              Name.Set.print were_existentials)

    let print ppf t =
      print_with_cache ~cache:(Printing_cache.create ()) ppf t

    let create ~resolver =
      { resolver;
        aliases = Simple.Map.empty;
        names_to_types = Name.Map.empty;
        cse = Flambda_primitive.With_fixed_value.Map.empty;
        levels_to_entries = Scope_level.Map.empty;
        next_sublevel_by_level = Scope_level.Map.empty;
        were_existentials = Name.Set.empty;
      }

    let create_using_resolver_from t = create ~resolver:t.resolver

    let resolver t = t.resolver

    let is_empty t = Name.Map.is_empty t.names_to_types

    let fast_equal t1 t2 =
      t1 == t2

    let domain t =
      let names =
        Name.Set.fold (fun name bindable_names ->
            Bindable_name.Set.add (Name name) bindable_names)
          (Name.Map.keys t.names_to_types)
          Bindable_name.Set.empty
      in
      Name_occurrences.create_from_set_in_terms names

    let find_exn t name : Flambda_types.t * binding_type =
      (* CR mshinwell: Maybe this should cause a fatal error and we shouldn't
         rely on catching the exception *)
      let _scope_level, entry = Name.Map.find name t.names_to_types in
      let binding_type : binding_type =
        if Name.Set.mem name t.were_existentials then Was_existential
        else Normal
      in
      let ty =
        match entry with
        | Definition ty | Equation ty -> ty
      in
      ty, binding_type

    type still_unresolved =
      | Resolved
      | Still_unresolved

    let resolve_aliases_on_ty0 (type a) t ?bound_name ~force_to_kind
          (ty : a Flambda_types.ty)
          : (a Flambda_types.ty) * (Simple.t option) * Name_or_export_id.Set.t
              * still_unresolved =
      let rec resolve_aliases names_seen ~canonical_simple
            (ty : a Flambda_types.ty) =
        let resolve (name : Name_or_export_id.t)
              : _ * _ * _ * still_unresolved =
          if Name_or_export_id.Set.mem name names_seen then begin
            Misc.fatal_errorf "Loop on %a whilst resolving aliases"
              Name_or_export_id.print name
          end;
          let continue_resolving t ~canonical_simple =
            let names_seen = Name_or_export_id.Set.add name names_seen in
            let ty = force_to_kind t in
            resolve_aliases names_seen ~canonical_simple ty
          in
          match name with
          | Name name ->
            let ty, _binding_type = find_exn t name in
            continue_resolving ty ~canonical_simple:(Some (Simple.name name))
          | Export_id export_id ->
            match t.resolver export_id with
            | Some ty -> continue_resolving ty ~canonical_simple
            | None -> ty, None, Name_or_export_id.Set.empty, Still_unresolved
        in
        match ty with
        | No_alias _ -> ty, canonical_simple, names_seen, Resolved
        | Type export_id -> resolve (Name_or_export_id.Export_id export_id)
        | Equals (Name name) -> resolve (Name_or_export_id.Name name)
        | Equals ((Const _ | Discriminant _) as simple) ->
          ty, Some simple, names_seen, Resolved
      in
      let seen =
        match bound_name with
        | None -> Name_or_export_id.Set.empty
        | Some bound_name -> Name_or_export_id.Set.singleton (Name bound_name)
      in
      resolve_aliases seen ~canonical_simple:None ty

    let resolve_aliases_on_ty t ?bound_name ~force_to_kind ty =
      let ty, canonical_name, names_seen, _still_unresolved =
        resolve_aliases_on_ty0 t ?bound_name ~force_to_kind ty
      in
      ty, canonical_name, names_seen

    let resolve_aliases_and_squash_unresolved_names_on_ty' env ?bound_name
          ~print_ty ~force_to_kind ty
          : _ Flambda_types.unknown_or_join * (Simple.t option) =
      let ty, canonical_name, _names_seen, _still_unresolved =
        try resolve_aliases_on_ty0 env ?bound_name ~force_to_kind ty
        with Misc.Fatal_error -> begin
          Format.eprintf "\n%sContext is: \
              resolve_aliases_and_squash_unresolved_names_on_ty':%s\
              @ %a@ Environment:@ %a\n"
            (Misc_color.bold_red ())
            (Misc_color.reset ())
            print_ty ty
            print env;
          raise Misc.Fatal_error
        end
      in
      match ty with
      | No_alias uoj -> uoj, canonical_name
      | Type _ | Equals _ -> Unknown, canonical_name

    (* CR mshinwell: It should be explicit in the code (maybe an invariant
       check on the end of this function) that if a canonical name is returned
       then the original type was an [Equals] or a [Type].  This fact
       should also be documented in the interface. *)
    let resolve_aliases ?bound_name t (ty : Flambda_types.t)
          : Flambda_types.t * (Simple.t option) =
      match ty with
      | Value ty_value ->
        let force_to_kind = Flambda_type0_core.force_to_kind_value in
        let ty_value, canonical_name, _names_seen =
          resolve_aliases_on_ty t ?bound_name ~force_to_kind ty_value
        in
        Value ty_value, canonical_name
      | Naked_number (ty_naked_number, kind) ->
        let force_to_kind =
          Flambda_type0_core.force_to_kind_naked_number kind
        in
        let ty_naked_number, canonical_name, _names_seen =
          resolve_aliases_on_ty t ?bound_name ~force_to_kind ty_naked_number
        in
        Naked_number (ty_naked_number, kind), canonical_name
      | Fabricated ty_fabricated ->
        let force_to_kind = Flambda_type0_core.force_to_kind_fabricated in
        let ty_fabricated, canonical_name, _names_seen =
          resolve_aliases_on_ty t ?bound_name ~force_to_kind ty_fabricated
        in
        Fabricated ty_fabricated, canonical_name

    let fold_all t ~init ~f =
      Scope_level.Map.fold (fun level by_sublevel acc ->
          Scope_level.Sublevel.Map.fold (fun sublevel (name, entry) acc ->
            let scope_level = Scope_level.With_sublevel.create level sublevel in
            let binding_type : binding_type =
              if Name.Set.mem name t.were_existentials then Was_existential
              else Normal
            in
            f acc name binding_type scope_level entry)
          by_sublevel
          acc)
        t.levels_to_entries
        init

    let fold t ~init ~f =
      Scope_level.Map.fold (fun level by_sublevel acc ->
          Scope_level.Sublevel.Map.fold (fun sublevel (name, entry) acc ->
            let entry0 =
              match entry with
              | CSE _ -> None
              | Definition ty ->
                Some ((Definition ty) : typing_environment_entry0)
              | Equation ty ->
                Some ((Equation ty) : typing_environment_entry0)
            in
            match entry0 with
            | None -> acc
            | Some entry0 ->
              let scope_level =
                Scope_level.With_sublevel.create level sublevel
              in
              let binding_type : binding_type =
                if Name.Set.mem name t.were_existentials then Was_existential
                else Normal
              in
              f acc name binding_type scope_level entry0)
          by_sublevel
          acc)
        t.levels_to_entries
        init

    let iter t ~f =
      fold t ~init:() ~f:(fun () name binding_type scope_level entry ->
          f name binding_type scope_level entry)

    let invariant t =
      (* CR mshinwell: Add more checks here *)
      if !Clflags.flambda_invariant_checks then begin
        (* Since [fold] operates in (scope level, sublevel) order, then the
           following check establishes that dependencies between bindings in
           the environment are only in one direction. *)
        ignore (fold_all t ~init:Bindable_name.Set.empty
          ~f:(fun names_seen (name : Name.t)
                  (_binding_type : binding_type)
                  _scope_level entry ->
            let free_names =
              match entry with
              | Definition ty | Equation ty ->
                Name_occurrences.everything (Type_free_names.free_names ty)
              | CSE prim ->
                Name_occurrences.everything
                  (Flambda_primitive.With_fixed_value.free_names prim)
            in
            if not (Bindable_name.Set.subset free_names names_seen) then begin
              Misc.fatal_errorf "Typing environment is not closed \
                  (%a free):@ %a"
                Bindable_name.Set.print
                (Bindable_name.Set.diff free_names names_seen)
                print t
            end;
            match entry with
            | Definition _ -> Bindable_name.Set.add (Name name) names_seen
            | Equation _ | CSE _ -> names_seen) : Bindable_name.Set.t);
        (* Checking that alias resolution works also ensures there are no
           cycles via aliases. *)
        Name.Map.iter (fun bound_name (_level, entry) ->
            let ty =
              match (entry : typing_environment_entry0) with
              | Definition ty | Equation ty -> ty
            in
            ignore (Sys.opaque_identity (resolve_aliases ~bound_name t ty)))
          t.names_to_types;
      end

    let mem t name =
      Name.Map.mem name t.names_to_types

    let scope_level_exn t name =
      match Name.Map.find name t.names_to_types with
      | exception Not_found ->
        Misc.fatal_errorf "scope_level: Cannot find %a in environment:@ %a"
          Name.print name
          print t
      | scope_level, _ty -> scope_level

    (* CR mshinwell: improve efficiency *)
    let find_with_scope_level_exn t name =
      let ty, binding_type = find_exn t name in
      let scope_level = scope_level_exn t name in
      ty, scope_level, binding_type

    let find_opt t name =
      match find_exn t name with
      | exception Not_found -> None
      | ty, binding_type -> Some (ty, binding_type)

    let find_cse (t : t) prim =
      match Flambda_primitive.With_fixed_value.create prim with
      | None -> None
      | Some prim ->
        match Flambda_primitive.With_fixed_value.Map.find prim t.cse with
        | exception Not_found -> None
        | name -> Some name

    let was_existential_exn t name =
      let _ty, binding_type = find_exn t name in
      match binding_type with
      | Normal -> false
      | Was_existential -> true

    let allocate_sublevel t level =
      let sublevel =
        match Scope_level.Map.find level t.next_sublevel_by_level with
        | exception Not_found -> Scope_level.Sublevel.initial
        | sublevel -> sublevel
      in
      let next_sublevel_by_level =
        Scope_level.Map.add level (Scope_level.Sublevel.next sublevel)
          t.next_sublevel_by_level
      in
      let t =
        { t with
          next_sublevel_by_level;
        }
      in
      t, sublevel

    let _min_level_for_new_binding t =
      let all_levels = Scope_level.Map.keys t.levels_to_entries in
      match Scope_level.Set.max_elt_opt all_levels with
      | None -> Scope_level.initial
      | Some level -> level

    type sense =
      | New_equation_must_be_more_precise
      | Existing_equation_must_be_more_precise

    let print_sense ppf (sense : sense) =
      match sense with
      | New_equation_must_be_more_precise ->
        Format.fprintf ppf "New_equation_must_be_more_precise"
      | Existing_equation_must_be_more_precise ->
        Format.fprintf ppf "Existing_equation_must_be_more_precise"

    let invariant_for_any_new_binding t name _level
          (entry : typing_environment_entry) =
      let free_names =
        match entry with
        | Definition ty | Equation ty -> Type_free_names.free_names ty
        | CSE prim -> Flambda_primitive.With_fixed_value.free_names prim
      in
      if Name_occurrences.mem free_names (Name name) then begin
        Misc.fatal_errorf "Cannot add binding@ %a = %a@ as it would produce \
            a circular dependency"
          Name.print name
          print_typing_environment_entry entry
      end;
      (* CR mshinwell: Unsure about levels for symbols yet
      ...well, we need to add them with a lower level than the current one...
      let min_level = min_level_for_new_binding t in
      if (not (Scope_level.equal level Scope_level.for_symbols))
        && Scope_level.(<) level min_level
      then begin
        Misc.fatal_errorf "Cannot add binding@ %a = %a@ to this environment \
            with scope level %a (minimum permitted level %a):@ %a"
          Name.print name
          print_typing_environment_entry entry
          Scope_level.print level
          Scope_level.print min_level
          print_typing_environment t
      end;
  *)
      match find_opt t name with
      | None ->
        begin match entry with
        | Definition _ | CSE _ -> ()
        | Equation _ ->
          Misc.fatal_errorf "Cannot add@ %a = %a@ for name undefined in \
              environment:@ %a"
            Name.print name
            print_typing_environment_entry entry
            print t
        end
      | Some _ ->
        match entry with
        | Definition _ ->
          Misc.fatal_errorf "Cannot redefine@ %a = %a@ in environment:@ %a"
            Name.print name
            print_typing_environment_entry entry
            print t
        | Equation _ | CSE _ -> ()

    let invariant_for_new_equation t name (ty : Flambda_types.t) ~sense =
      let existing_ty, _binding_type = find_exn t name in
      let meet_ty, _env_extension =
        let meet_env =
          Meet_env.create t
            ~perm_left:(Name_permutation.create ())
            ~perm_right:(Name_permutation.create ())
        in
        Both_meet_and_join.meet meet_env existing_ty ty
      in
      let ty_must_be_strictly_more_precise, other_ty =
        match sense with
        | New_equation_must_be_more_precise -> ty, existing_ty
        | Existing_equation_must_be_more_precise -> existing_ty, ty
      in
      let as_or_more_precise =
        Type_equality.equal meet_ty ty_must_be_strictly_more_precise
      in
      let strictly_more_precise =
        as_or_more_precise && not (Type_equality.equal meet_ty other_ty)
      in
      if not strictly_more_precise then begin
        Misc.fatal_errorf "Cannot add equation %a = %a@ to this environment: \
            as_or_more_precise %b,@ strictly_more_precise %b,@ meet_ty@ %a,@ \
            existing_ty@ %a,@ sense@ %a.@  Env:@ %a"
          Name.print name
          Type_printers.print ty
          as_or_more_precise
          strictly_more_precise
          Type_printers.print meet_ty
          Type_printers.print existing_ty
          print_sense sense
          print t
      end
  (* XXX Not sure about this part
      Typing_env_extension.iter env_extension
        ~f:(fun name _binding_type level (entry : typing_environment_entry0) ->
          let ty =
            match entry with
            | Definition ty | Equation ty -> ty
          in
          let level = Scope_level.With_sublevel.level level in
          invariant_for_any_new_binding t name level (Equation ty);
          invariant_for_new_equation t name ty
            ~sense:Existing_equation_must_be_more_precise)
  *)
    let _ = ignore Existing_equation_must_be_more_precise

    let invariant_for_new_binding t name level
          (entry : typing_environment_entry) =
      invariant_for_any_new_binding t name level entry;
      match entry with
      | Definition _ | CSE _ -> ()
      | Equation ty ->
        invariant_for_new_equation t name ty
          ~sense:New_equation_must_be_more_precise

  (*
    let canonical_name t name =
      match find_opt t name with
      | None -> None
      | Some (ty, _binding_type) ->
        let _ty, canonical_name = resolve_aliases (t, ty) in
        match canonical_name with
        | None -> None
        | Some canonical_name ->
          begin
            if Name.equal name canonical_name then begin
              Misc.fatal_errorf "Canonical name for %a is itself in \
                  environment:@ %a"
                Name.print name
                print_typing_environment t
            end;
            Some canonical_name
          end
  *)

    let aliases_of_simple (t : t) (simple : Simple.t) =
      match Simple.Map.find simple t.aliases with
      | exception Not_found ->
        begin match simple with
        | Const _ | Discriminant _ -> Name.Set.empty
        | Name name ->
          Misc.fatal_errorf "Typing_env.aliases_of_name: unbound name %a"
            Name.print name
        end
      | aliases -> aliases

    let add t (name : Name.t) cont_level (binding : typing_environment_entry) =
      invariant_for_new_binding t name cont_level binding;
      let alias =
        match binding with
        | Definition ty | Equation ty -> Flambda_type0_core.get_alias ty
        | CSE _ -> None
      in
      let equation_with_reverse_alias_already_present =
        match binding with
        | Equation _ ->
          begin match alias with
          | None | Some (Const _ | Discriminant _) -> false
          | Some (Name alias) ->
            Name.Set.mem alias (aliases_of_simple t (Simple.name name))
          end
        | Definition _ | CSE _ -> false
      in
      if equation_with_reverse_alias_already_present then begin
        t
      end else begin
        let aliases =
          match alias with
          | None -> t.aliases
          | Some alias ->
            Simple.Map.update alias (function
                | None -> Some (Name.Set.singleton name)
                | Some aliases -> Some (Name.Set.add name aliases))
              t.aliases
        in
        let t, sublevel = allocate_sublevel t cont_level in
        let level = Scope_level.With_sublevel.create cont_level sublevel in
        let names_to_types =
          match binding with
          | Definition ty ->
            let entry0 : typing_environment_entry0 = Definition ty in
            Name.Map.add name (level, entry0) t.names_to_types
          | Equation ty ->
            let entry0 : typing_environment_entry0 = Equation ty in
            Name.Map.add name (level, entry0) t.names_to_types
          | CSE _ -> t.names_to_types
        in
        let levels_to_entries =
          Scope_level.Map.update cont_level
            (function
              | None ->
                let by_sublevel =
                  Scope_level.Sublevel.Map.singleton sublevel (name, binding)
                in
                Some by_sublevel
              | Some by_sublevel ->
                assert (not (Scope_level.Sublevel.Map.mem sublevel by_sublevel));
                let by_sublevel =
                  Scope_level.Sublevel.Map.add sublevel (name, binding)
                    by_sublevel
                in
                Some by_sublevel)
            t.levels_to_entries
        in
        let cse =
          match binding with
          | Definition _ | Equation _ -> t.cse
          | CSE prim ->
            match
              Flambda_primitive.With_fixed_value.Map.find prim t.cse
            with
            | exception Not_found ->
              Flambda_primitive.With_fixed_value.Map.singleton prim
                (Simple.name name)
            | _name -> t.cse  (* Keep the furthest-out binding. *)
        in
        let t =
          { t with
            aliases;
            names_to_types;
            levels_to_entries;
            cse;
          }
        in
        invariant t;
        t
      end

    let singleton ~resolver name scope_level binding =
      add (create ~resolver) name scope_level binding

    let max_level t =
      match Scope_level.Map.max_binding_opt t.levels_to_entries with
      | None -> Scope_level.initial
      | Some (level, _) -> level

    let restrict_to_names0 (t : t) allowed =
      let aliases =
        Simple.Map.filter_map (fun (simple : Simple.t) aliases ->
            let aliases =
              Name.Set.filter (fun name -> Name.Set.mem name allowed) aliases
            in
            match simple with
            | Name name ->
              if Name.Set.mem name allowed then Some aliases
              else None
            | Const _ | Discriminant _ -> Some aliases)
          t.aliases
      in
      let names_to_types =
        Name.Map.filter (fun name _ty -> Name.Set.mem name allowed)
          t.names_to_types
      in
      let levels_to_entries =
        Scope_level.Map.filter_map (fun _cont_level by_sublevel ->
            let by_sublevel =
              Scope_level.Sublevel.Map.filter_map
                (fun _sublevel ((name, _) as entry) ->
                  if Name.Set.mem name allowed then Some entry
                  else None)
              by_sublevel
            in
            if Scope_level.Sublevel.Map.is_empty by_sublevel then None
            else Some by_sublevel)
          t.levels_to_entries
      in
      let were_existentials = Name.Set.inter t.were_existentials allowed in
      let cse =
        Flambda_primitive.With_fixed_value.Map.filter
          (fun prim (simple : Simple.t) ->
            let names_in_prim =
              Name_occurrences.everything_must_only_be_names
                (Flambda_primitive.With_fixed_value.free_names prim)
            in
            let names =
              match simple with
              | Name name -> Name.Set.add name names_in_prim
              | Const _ | Discriminant _ -> names_in_prim
            in
            Name.Set.is_empty (Name.Set.diff names allowed))
          t.cse
      in
      let t =
        { resolver = t.resolver;
          aliases;
          names_to_types;
          cse;
          levels_to_entries;
          next_sublevel_by_level = t.next_sublevel_by_level;
          were_existentials;
        }
      in
      try
        invariant t;
        t
      with Misc.Fatal_error -> begin
        Format.eprintf "\n%sContext is: \
            restrict_to_names0_typing_environment:%s\
            @ Restricting to: %a@ \nEnvironment:@ %a\n"
          (Misc_color.bold_red ())
          (Misc_color.reset ())
          Name.Set.print allowed
          print t;
        raise Misc.Fatal_error
      end

    let restrict_to_symbols t =
      let symbols = Name.symbols_only_map t.names_to_types in
      restrict_to_names0 t (Name.Map.keys symbols)

    let filter t ~f =
      let allowed =
        Name.Map.fold (fun name ty allowed ->
            if f name ty then Name.Set.add name allowed
            else allowed)
          t.names_to_types
          Name.Set.empty
      in
      restrict_to_names0 t allowed

    let remove t name =
      let allowed = Name.Set.remove name (Name.Map.keys t.names_to_types) in
      restrict_to_names0 t allowed

    let rec add_or_meet_env_extension' t
          (env_extension : Typing_env_extension.t) scope_level =
      let original_t = t in
      let add_equation t name ty =
        match find_opt t name with
        | None -> add t name scope_level (Equation ty)
        | Some (existing_ty, _binding_type) ->
          let meet =
            let meet_ty, meet_env_extension =
              let meet_env =
                Meet_env.create t
                  ~perm_left:(Name_permutation.create ())
                  ~perm_right:(Name_permutation.create ())
              in
              Both_meet_and_join.meet meet_env ty existing_ty
            in
            let as_or_more_precise = Type_equality.equal meet_ty ty in
            let strictly_more_precise =
              as_or_more_precise
                && not (Type_equality.equal meet_ty existing_ty)
            in
            if strictly_more_precise then Some (meet_ty, meet_env_extension)
            else None
          in
          match meet with
          | None -> t
          | Some (new_ty, new_env_extension)->
            let t =
              add_or_meet_env_extension' t new_env_extension scope_level
            in
            add t name scope_level (Equation new_ty)
      in
      let add_definition t (name : Name.t) ty =
        (* XXX check the next few lines, conditional seems dubious *)
        if mem t name then
         add_equation t name ty
        else
          let t = add t fresh_name scope_level (Definition ty) in
          begin match fresh_name with
          | Var _ | Logical_var _ -> ()
          | Symbol sym ->
            Misc.fatal_errorf "Definitions of symbols should never occur \
                in environment extensions: symbol %a, env@ %a,@ \
                env_extension@ %a"
              Symbol.print sym
              print original_t
              Typing_env_extension.print env_extension
          end;
          { t with
            were_existentials = Name.Set.add fresh_name t.were_existentials;
          }
      in
      let add_cse (t : t) bound_to prim =
        match
          Flambda_primitive.With_fixed_value.Map.find prim t.cse
        with
        | exception Not_found ->
          let t =
            match bound_to with
            | Name bound_to -> add t bound_to scope_level (CSE prim)
            | Const _ | Discriminant _ -> t
          in
          let cse =
            Flambda_primitive.With_fixed_value.Map.add prim bound_to t.cse
          in
          { t with cse; }
        | _bound_to ->
          (* As above, keep the outer binding. *)
          t
      in
      let t =
        List.fold_left (fun t (name, ty) ->
            add_definition t name ty)
          t
          (List.rev env_extension.first_definitions)
      in
      let t =
        Scope_level.Map.fold
          (fun _level by_sublevel t ->
            Scope_level.Sublevel.Map.fold
              (fun _sublevel
                   ((name : Name.t), (entry : typing_environment_entry))
                   t ->
                match entry with
                | Definition ty -> add_definition t name ty
                | Equation ty ->
                  let t = add_equation t name ty in
                  t
                | CSE prim ->
                  let t = add_cse t (Simple.name name) prim in
                  t)
              by_sublevel
              t)
          env_extension.at_or_after_cut_point
          t
      in
      let t =
        List.fold_left (fun t (name, ty) ->
            add_equation t name ty)
          t
          env_extension.last_equations_rev
      in
      Flambda_primitive.With_fixed_value.Map.fold (fun prim bound_to t ->
          add_cse t bound_to prim)
        env_extension.cse
        t

    let add_or_meet_env_extension t env_extension scope_level =
      Typing_env_extension.pattern_match env_extension
        ~f:(fun env_extension0 ->
          add_or_meet_env_extension' t env_extension0 scope_level)

    let add_equation t name scope_level ty =
      if not (mem t name) then begin
        Misc.fatal_errorf "Typing_env.replace_meet: name %a not bound in:@ %a"
          Name.print name
          print t
      end;
      let env_extension =
        Typing_env_extension.add_equation
          (Typing_env_extension.empty ()) name ty
      in
      add_or_meet_env_extension t env_extension scope_level

    (* CR mshinwell: Move to [Typing_env]? *)
    let free_names_transitive env ty =
      let original_ty = ty in
      let all_names = ref (Name_occurrences.create ()) in
      let rec loop to_follow =
        all_names := Name_occurrences.union !all_names to_follow;
        match
          Name_occurrences.choose_and_remove_amongst_everything to_follow
        with
        | None -> ()
        | Some (name, to_follow) ->
          begin match name with
          | Name name ->
            let ty =
              match Typing_env.find_exn env name with
              | exception Not_found ->
                Misc.fatal_errorf "Unbound name %a whilst finding free names,@ \
                    transitively, of %a@ in environment@ %a"
                  Name.print name
                  Type_printers.print ty
                  Typing_env.print env
              | ty, _binding_type -> ty
            in
            let names = Type_free_names.free_names ty in
            loop (Name_occurrences.union to_follow names)
          | Continuation _ ->
            Misc.fatal_errorf "Illegal name in type: %a"
              Type_printers.print original_ty
          end
      in
      loop (Type_free_names.free_names ty);
      !all_names

    let free_names_transitive_list (t : t) (env : Typing_env.t) tys =
      let scope_level = Scope_level.next (Typing_env.max_level env) in
      let env = Typing_env.add_or_meet_env_extension env t scope_level in
      List.fold_left (fun names ty ->
          Name_occurrences.union names (free_names_transitive env ty))
        (Name_occurrences.create ())
        tys
  end and Typing_env_extension0 : sig
    type t = private {
      first_definitions : (Name.t * Flambda_types.t) list;
      at_or_after_cut_point : Typing_env.levels_to_entries;
      last_equations_rev : (Name.t * Flambda_types.t) list;
      cse : Simple.t Flambda_primitive.With_fixed_value.Map.t;
    }

    include Contains_names.S with type t := t

    val empty : unit -> t

    val is_empty : t -> bool

    val create_for_cut
       : at_or_after_cut_point:Typing_env.levels_to_entries
      -> t

    val meet
       : Meet_env.t
      -> t
      -> t
      -> t

    val join
       : Join_env.t
      -> t
      -> t
      -> t

    val diff : t -> Typing_env.t -> t
  end = struct
    type t = {
      first_definitions : (Name.t * Flambda_types.t) list;
      at_or_after_cut_point : Typing_env.levels_to_entries;
      last_equations_rev : (Name.t * Flambda_types.t) list;
      cse : Simple.t Flambda_primitive.With_fixed_value.Map.t;
    }

    let empty () =
      { first_definitions = [];
        at_or_after_cut_point = Scope_level.Map.empty;
        last_equations_rev = [];
        cse = Flambda_primitive.With_fixed_value.Map.empty;
      }

    let is_empty t = Scope_level.Map.is_empty t.at_or_after_cut_point

    let create_for_cut ~at_or_after_cut_point =
      { first_definitions = [];
        at_or_after_cut_point;
        last_equations_rev = [];
        cse = Flambda_primitive.With_fixed_value.Map.empty;
      }

    let equal env t1 t2 =
      (* CR mshinwell: This should be improved *)
      let equal_names_and_types (name1, ty1) (name2, ty2) =
        Name.equal name1 name2
          && Type_equality.equal_with_env env ty1 ty2
      in
      Misc.Stdlib.List.equal equal_names_and_types
          t1.first_definitions t2.first_definitions
        && Scope_level.Map.equal
             (Scope_level.Sublevel.Map.equal
               (fun (name1, (entry1 : Typing_env.typing_environment_entry))
                    (name2, (entry2 : Typing_env.typing_environment_entry)) ->
                 Name.equal name1 name2
                   && match entry1, entry2 with
                      | Definition ty1, Definition ty2 ->
                        Type_equality.equal_with_env env ty1 ty2
                      | Equation ty1, Equation ty2 ->
                        Type_equality.equal_with_env env ty1 ty2
                      | CSE prim1, CSE prim2 ->
                        Flambda_primitive.With_fixed_value.equal prim1 prim2
                      | _, _ -> false))
             t1.at_or_after_cut_point
             t2.at_or_after_cut_point
        && Misc.Stdlib.List.equal equal_names_and_types
             t1.last_equations_rev t2.last_equations_rev

    let apply_permutation_typing_environment_entry
          (entry : Typing_env.typing_environment_entry) perm
          : Typing_env.typing_environment_entry =
      match entry with
      | Definition ty ->
        let ty' = Flambda_type0_core.apply_name_permutation ty perm in
        if ty == ty' then entry
        else Definition ty'
      | Equation ty ->
        let ty' = Flambda_type0_core.apply_name_permutation ty perm in
        if ty == ty' then entry
        else Equation ty'
      | CSE prim ->
        let prim' =
          Flambda_primitive.With_fixed_value.apply_name_permutation prim perm
        in
        if prim == prim' then entry
        else CSE prim'

    let apply_name_permutation
          ({ first_definitions; at_or_after_cut_point; last_equations_rev;
             cse; } as t)
          perm : t =
      let first_definitions_changed = ref false in
      let first_definitions' =
        List.map (fun (name, t) ->
            let name' = Name_permutation.apply_name perm name in
            let t' = Flambda_type0_core.apply_name_permutation t perm in
            if (not (name == name')) && (not (t == t')) then begin
              first_definitions_changed := true
            end;
            name', t')
          first_definitions
      in
      let at_or_after_cut_point' =
        Scope_level.Map.map_sharing (fun by_sublevel ->
            Scope_level.Sublevel.Map.map_sharing
              (fun ((name, (entry : Typing_env.typing_environment_entry))
                    as datum) ->
                let name' = Name_permutation.apply_name perm name in
                let entry' =
                  apply_permutation_typing_environment_entry entry perm
                in
                if name == name' && entry == entry' then datum
                else name', entry')
              by_sublevel)
          at_or_after_cut_point
      in
      let last_equations_rev_changed = ref false in
      let last_equations_rev' =
        List.map (fun (name, ty) ->
            let name' = Name_permutation.apply_name perm name in
            let ty' = Flambda_type0_core.apply_name_permutation ty perm in
            if (not (name == name')) || (not (ty == ty')) then begin
              last_equations_rev_changed := true
            end;
            name', ty')
          last_equations_rev
      in
      let cse_changed = ref false in
      let cse' =
        Flambda_primitive.With_fixed_value.Map.fold (fun prim simple cse' ->
            let simple' = Simple.apply_name_permutation simple perm in
            let prim' =
              Flambda_primitive.With_fixed_value.apply_name_permutation prim
                perm
            in
            if (not (simple == simple')) || (not (prim == prim')) then begin
              cse_changed := true
            end;
            Flambda_primitive.With_fixed_value.Map.add prim' simple' cse')
          cse
          Flambda_primitive.With_fixed_value.Map.empty
      in
      if (not !first_definitions_changed)
        && at_or_after_cut_point == at_or_after_cut_point'
        && (not !last_equations_rev_changed)
        && (not !cse_changed)
      then t
      else 
        { first_definitions = first_definitions';
          at_or_after_cut_point = at_or_after_cut_point';
          last_equations_rev = last_equations_rev';
          cse = cse';
        }

    let free_names
          { first_definitions; at_or_after_cut_point; last_equations_rev;
            cse; } =
      let free_names_first_definitions =
        List.fold_left (fun acc (_name, ty) ->
            Name.Set.union acc (
              Name_occurrences.everything_must_only_be_names (
                Type_free_names.free_names ty)))
          Name.Set.empty
          first_definitions
      in
      let free_names_at_or_after_cut_point =
        Scope_level.Map.fold (fun _level by_sublevel acc ->
            Scope_level.Sublevel.Map.fold
              (fun _sublevel
                   (name, (entry : Typing_env.typing_environment_entry))
                   acc ->
                match entry with
                | Definition ty ->
                  Name.Set.union acc (
                    Name_occurrences.everything_must_only_be_names (
                      Type_free_names.free_names ty))
                | Equation ty ->
                  Name.Set.add name (
                    Name.Set.union acc (
                      Name_occurrences.everything_must_only_be_names (
                        Type_free_names.free_names ty)))
                | CSE prim ->
                  Name.Set.union acc (
                    Name_occurrences.everything_must_only_be_names (
                      (Flambda_primitive.With_fixed_value.free_names prim))))
              by_sublevel
              acc)
          at_or_after_cut_point
          free_names_first_definitions
      in
      let free_names_last_equations_rev =
        List.fold_left (fun acc (_name, ty) ->
            Name.Set.union acc (
              Name_occurrences.everything_must_only_be_names (
                Type_free_names.free_names ty)))
          free_names_at_or_after_cut_point
          last_equations_rev
      in
      let free_names =
        Flambda_primitive.With_fixed_value.Map.fold
          (fun prim (simple : Simple.t) acc ->
            match simple with
            | Const _ | Discriminant _ -> acc
            | Name name ->
              let acc =
                Name.Set.union acc
                  (Name_occurrences.everything_must_only_be_names (
                    (Flambda_primitive.With_fixed_value.free_names prim)))
              in
              Name.Set.add name acc)
          cse
          free_names_last_equations_rev
      in
      Name_occurrences.create_from_set_in_types free_names

    let defined_names t =
      let from_first_definitions =
        Name.Set.of_list (
          List.map (fun (name, _ty) -> name) t.first_definitions)
      in
      Scope_level.Map.fold (fun _level by_sublevel defined_names ->
          Scope_level.Sublevel.Map.fold
            (fun _sublevel (name, (entry : Typing_env.typing_environment_entry))
                 defined_names ->
              match entry with
              | Definition _ -> Name.Set.add name defined_names
              | Equation _ | CSE _ -> defined_names)
            by_sublevel
            defined_names)
        t.at_or_after_cut_point
        from_first_definitions

    let equations_domain
          { first_definitions = _; at_or_after_cut_point;
            last_equations_rev; cse = _; } =
      let from_at_or_after_cut_point =
        Scope_level.Map.fold (fun _level by_sublevel domain ->
            Scope_level.Sublevel.Map.fold
              (fun _sublevel
                   (name, (entry : Typing_env.typing_environment_entry))
                   domain ->
                match entry with
                | Definition _ -> domain
                | Equation _ | CSE _ -> Name.Set.add name domain)
              by_sublevel
              domain)
          at_or_after_cut_point
          Name.Set.empty
      in
      let from_last_equations_rev =
        Name.Set.of_list (
          List.map (fun (name, _ty) -> name) last_equations_rev)
      in
      Name.Set.union from_at_or_after_cut_point from_last_equations_rev

    let restrict_to_names t allowed_names =
      let allowed_names = Name_occurrences.everything allowed_names in
      let first_definitions =
        List.filter (fun (name, _ty) ->
            Bindable_name.Set.mem (Name name) allowed_names)
          t.first_definitions
      in
      let at_or_after_cut_point =
        Scope_level.Map.filter_map (fun _cont_level by_sublevel ->
            let by_sublevel =
              Scope_level.Sublevel.Map.filter_map
                (fun _sublevel ((name, _) as entry) ->
                  if Bindable_name.Set.mem (Name name) allowed_names then
                    Some entry
                  else
                    None)
              by_sublevel
            in
            if Scope_level.Sublevel.Map.is_empty by_sublevel then None
            else Some by_sublevel)
          t.at_or_after_cut_point
      in
      let last_equations_rev =
        List.filter (fun (name, _ty) ->
            Bindable_name.Set.mem (Name name) allowed_names)
          t.last_equations_rev
      in
      let cse =
        Flambda_primitive.With_fixed_value.Map.filter
          (fun _prim (simple : Simple.t) ->
            match simple with
            | Name name -> Bindable_name.Set.mem (Name name) allowed_names
            | Const _ | Discriminant _ -> true)
          t.cse
      in
      let t =
        { first_definitions;
          at_or_after_cut_point;
          last_equations_rev;
          cse;
        }
      in
      invariant t;
      t

    let meet (env : Meet_env.t) (t1 : t) (t2 : t) : t =
      if Meet_env.shortcut_precondition env && fast_equal t1 t2 then t1
      else if is_empty t1 then t2
      else if is_empty t2 then t1
      else
        let t1 = apply_name_permutation t1 (Meet_env.perm_left env) in
        let t2 = apply_name_permutation t2 (Meet_env.perm_right env) in
        let env = Meet_env.clear_name_permutations env in
        let scope_level =
          Scope_level.next (Typing_env.max_level (Meet_env.env env))
        in
        let env =
          Meet_env.with_env env (fun env ->
            Typing_env.add_or_meet_env_extension env t1 scope_level)
        in
        let env =
          Meet_env.with_env env (fun env ->
            Typing_env.add_or_meet_env_extension env t2 scope_level)
        in
        Typing_env.cut (Meet_env.env env)
          ~existential_if_defined_at_or_later_than:scope_level

    let join (env : Join_env.t) (t1 : t) (t2 : t) : t =
      if Join_env.shortcut_precondition env && fast_equal t1 t2 then t1
      else if is_empty t1 then empty ()
      else if is_empty t2 then empty ()
      else
        let t1 = apply_name_permutation t1 (Join_env.perm_left env) in
        let t2 = apply_name_permutation t2 (Join_env.perm_right env) in
        let env = Join_env.clear_name_permutations env in
        let env =
          Join_env.add_extensions env ~holds_on_left:t1 ~holds_on_right:t2
        in
        let names_in_join =
          let equations_in_t1_on_env = free_names t1 in
          let equations_in_t2_on_env = free_names t2 in
          Name.Set.inter equations_in_t1_on_env equations_in_t2_on_env
        in
        let t =
          Name.Set.fold (fun name t ->
              let ty1 = find t1 name in
              let ty2 = find t2 name in
              let join_ty = Both_meet_and_join.join env ty1 ty2 in
              add_equation t name join_ty)
            names_in_join
            (empty ())
        in
        let preserved_cse_equations t =
          (* CR-someday mshinwell: This could be improved to preserve some of
             those CSE equations that talk about existentially-bound names. *)
          Flambda_primitive.With_fixed_value.Map.filter
            (fun prim (bound_to_or_value : Simple.t) ->
              match bound_to_or_value with
              | Name name when not (Name.Set.mem name names_in_join) ->
                false
              | Name _ | Const _ | Discriminant _ ->
                let free_names_prim =
                  Name_occurrences.everything_must_only_be_names
                    (Flambda_primitive.With_fixed_value.free_names prim)
                in
                Name.Set.subset free_names_prim names_in_join)
            t.cse
        in
        let cse =
          Flambda_primitive.With_fixed_value.Map.merge
            (fun _prim
                (simple1 : Simple.t option) (simple2 : Simple.t option) ->
              match simple1, simple2 with
              | None, None -> None
              | Some _, None -> simple1
              | None, Some _ -> simple2
              | Some simple1, Some simple2 ->
                (* For the moment just keep this very straightforward. *)
                (* CR-soon mshinwell: Make this take account of aliases. *)
                if Simple.equal simple1 simple2 then Some simple1
                else None)
            (preserved_cse_equations t1)
            (preserved_cse_equations t2)
        in
        let t =
          { t with
            cse;
          }
        in
        invariant t;
        t

    (* CR mshinwell: This needs to do something with [t.cse] perhaps *)
    (* CR mshinwell: Think carefully about whether the freshening is actually
       needed here *)
    let diff t env : t =
      let names_more_precise, _freshened_names_more_precise, _perm =
        fold t
          ~init:(Name.Set.empty, Name.Set.empty, Name_permutation.create ())
          ~f:(fun (names_more_precise, freshened_names_more_precise, perm)
                  (name : Name.t)
                  (info : fold_info) ->
            match info with
            | Definition_in_extension _ty ->
              let fresh_name = Name.rename name in
              let perm = Name_permutation.add_name perm name fresh_name in
              let names_more_precise =
                Name.Set.add name names_more_precise
              in
              let freshened_names_more_precise =
                Name.Set.add fresh_name freshened_names_more_precise
              in
              names_more_precise, freshened_names_more_precise, perm
            | Equation ty ->
              let unfreshened_name = name in
              let name = Name_permutation.apply_name perm name in
              let ty = Flambda_type0_core.apply_name_permutation ty perm in
              match Typing_env.find_opt env name with
              | None ->
                let names_more_precise =
                  Name.Set.add unfreshened_name names_more_precise
                in
                let freshened_names_more_precise =
                  Name.Set.add name names_more_precise
                in
                names_more_precise, freshened_names_more_precise, perm
              | Some (old_ty, _) ->
                let more_precise_using_old_types_for_free_names =
                  (* XXX Not sure [env] is right: shouldn't it contain names
                     from the extension too? *)
                  Both_meet_and_join.strictly_more_precise env ty ~than:old_ty
                in
                if more_precise_using_old_types_for_free_names then
                  let names_more_precise =
                    Name.Set.add name names_more_precise
                  in
                  names_more_precise, freshened_names_more_precise, perm
                else
                  let free_names =
                    Name_occurrences.everything_must_only_be_names
                      (Type_free_names.free_names ty)
                  in
                  let more_precise_using_new_types_for_free_names =
                    not (Name.Set.is_empty (
                      Name.Set.inter free_names names_more_precise))
                  in
                  if more_precise_using_new_types_for_free_names then
                    let names_more_precise =
                      Name.Set.add unfreshened_name names_more_precise
                    in
                    let freshened_names_more_precise =
                      Name.Set.add name names_more_precise
                    in
                    names_more_precise, freshened_names_more_precise, perm
                  else
                    names_more_precise, freshened_names_more_precise,
                      perm)
      in
      let names_more_precise =
        Name.Set.fold (fun name result ->
            Bindable_name.Set.add (Name name) result)
          names_more_precise
          Bindable_name.Set.empty
      in
      restrict_to_names t
        (Name_occurrences.create_from_set_in_types names_more_precise)
  end and Typing_env_extension : sig
    type t

    include Contains_names.S with type t := t

    val invariant : t -> unit

    val print : Format.formatter -> t -> unit

    val print_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> t
      -> unit

    val equal : Type_equality_env.t -> t -> t -> bool

    val fast_equal : t -> t -> bool

    val empty : unit -> t

    val is_empty : t -> bool

    val create_from_cut
       : Typing_env.t
      -> existential_if_defined_at_or_later_than:Scope_level.t
      -> t

    val add_definition_at_beginning : t -> Name.t -> Flambda_types.t -> t

    val add_equation : t -> Name.t -> Flambda_types.t -> t

    val add_cse : t -> Simple.t -> Flambda_primitive.With_fixed_value.t -> t

    val meet
       : Meet_env.t
      -> t
      -> t
      -> t

    val join
       : Join_env.t
      -> t
      -> t
      -> t

    val restrict_to_definitions : t -> t

    val restrict_names_to_those_occurring_in_types
       : t
      -> Typing_env.t
      -> Typing_env.t
      -> Flambda_types.t list
      -> t

    val diff : t -> Typing_env.t -> t

    val pattern_match : t -> f:(Typing_env_extension0.t -> 'a) -> 'a
  end = struct
    (* CR mshinwell: Move [Typing_env_extension0] into here? *)
    module T0 = Typing_env_extension0

    include Name_abstraction.Make (Bound_name_set) (T0)

    let print_with_cache ~cache ppf
          ({ first_definitions; at_or_after_cut_point; last_equations_rev;
             cse; } : t) =
      let print_binding_list =
        Format.pp_print_list ~pp_sep:Format.pp_print_space
          (fun ppf (name, ty) ->
            Format.fprintf ppf "@[(%a %a)@]"
              Name.print name
              (Type_printers.print_with_cache ~cache) ty)
      in
      Format.fprintf ppf
        "@[<hov 1>(\
            @[<hov 1>(first_definitions@ %a)@]@ \
            @[<hov 1>(at_or_after_cut_point@ %a)@]@ \
            @[<hov 1>(last_equations_rev@ %a)@]@ \
            @[<hov 1>(cse@ %a)@])@]"
        print_binding_list first_definitions
        (Typing_env.print_levels_to_entries_with_cache ~cache)
          at_or_after_cut_point
        print_binding_list last_equations_rev
        (Flambda_primitive.With_fixed_value.Map.print Simple.print) cse

    let print ppf t =
      print_with_cache ~cache:(Printing_cache.create ()) ppf t

    let fast_equal t1 t2 = (t1 == t2)

    let equal env t1 t2 =
      pattern_match_pair t1 t2 ~f:(fun _ t0_1 t0_2 ->
        Typing_env_extension0.equal env t0_1 t0_2)

    let invariant _t =
      (* CR mshinwell: Work out what to do here.  Probably just a check that
         the ordering is reasonable. *)
      ()

    let empty () =
      create Name.Set.empty (T0.empty ())

    let is_empty t =
      pattern_match t ~f:(fun _ t0 -> T0.is_empty t0)

    let create_from_cut (env : Typing_env.t)
          ~existential_if_defined_at_or_later_than : t =
      (* CR mshinwell: Add a split which only returns one map, the side we
         would like. *)
      let _before_cut_point, at_cut_point, after_cut_point =
        Scope_level.Map.split existential_if_defined_at_or_later_than
          env.levels_to_entries
      in
      let at_or_after_cut_point =
        match at_cut_point with
        | None -> after_cut_point
        | Some by_sublevel ->
          Scope_level.Map.add existential_if_defined_at_or_later_than
            by_sublevel after_cut_point
      in
      let t0 = T0.create_for_cut ~at_or_after_cut_point in
      let defined_names = T0.defined_names t0 in
      create defined_names t0

    let restrict_to_definitions t =
      pattern_match_mapi t ~f:(fun defined_names t0 ->
        T0.restrict_to_names t0 defined_names)

    let restrict_names_to_those_occurring_in_types t env env_allowed_names tys =
      let free_names = free_names_transitive_list t env tys in
      let env_allowed_names = Typing_env.domain env_allowed_names in
      let allowed_names = Name_occurrences.union free_names env_allowed_names in
      restrict_to_names t allowed_names

    type fold_info =
      | Definition_in_extension of Flambda_types.t
      | Equation of Flambda_types.t

    let fold t ~init ~(f : _ -> Name.t -> fold_info -> _) =
      let acc =
        List.fold_left (fun acc (name, ty) ->
            f acc name (Definition_in_extension ty))
          init
          (List.rev t.first_definitions)
      in
      let acc =
        Scope_level.Map.fold (fun _level by_sublevel acc ->
            Scope_level.Sublevel.Map.fold
              (fun _sublevel
                   (name, (entry : Typing_env.typing_environment_entry))
                   acc ->
                match entry with
                | Definition ty ->
                  f acc name (Definition_in_extension ty)
                | Equation ty ->
                  f acc name (Equation ty)
                | CSE _ -> acc)
              by_sublevel
              acc)
          t.at_or_after_cut_point
          acc
      in
      List.fold_left (fun acc (name, ty) ->
          f acc name (Equation ty))
        acc
        t.last_equations_rev

    let add_definition_at_beginning t name ty =
      let first_definitions = (name, ty) :: t.first_definitions in
      { t with
        first_definitions;
      }

    (* CR mshinwell: Invariant check for increased preciseness? *)
    let add_equation t name ty =
      let last_equations_rev = (name, ty) :: t.last_equations_rev in
      { t with
        last_equations_rev;
      }

    let add_cse t name prim =
      let cse =
        match Flambda_primitive.With_fixed_value.Map.find prim t.cse with
        | exception Not_found ->
          Flambda_primitive.With_fixed_value.Map.add prim name t.cse
        | _name -> t.cse
      in
      { t with cse; }

    let diff t env =
      pattern_match t ~f:(fun _ t0 ->
        let t0 = T0.diff t0 env in
        let defined_names = T0.defined_names t0 in
        create defined_names t0)
  end

  include Flambda_type0_core
  include Flambda_types

  let meet = Both_meet_and_join.meet
  let join = Both_meet_and_join.join

  let _meet_skeleton env t ~skeleton ~result ~result_kind =
    let level = Typing_env.max_level env in
    let env =
      Typing_env.add env result level (Definition (bottom result_kind))
    in
    let env =
      Meet_env.create env
        ~perm_left:(Name_permutation.create ())
        ~perm_right:(Name_permutation.create ())
    in
    let _meet_ty, env_extension = meet env t skeleton in
    env_extension

  (* Or maybe: the caller should provide the variable and this should just
     return the env_extension
  let meet_skeleton env t kind ~make_skeleton =
    let var = Variable.create "meet" in
    let name = Name.var var in
    let level = Typing_env.max_level env in
    let env = Typing_env.add env name level (Definition (bottom kind)) in
    let skeleton = make_skeleton (alias_type_of name) in
    let _meet_ty, env_extension = meet env t skeleton in
    let env = Typing_env.add_or_meet_extension env env_extension level in
    let t, _binding_type = Typing_env.find_exn env name in
    t
  *)

  let as_or_more_precise = Both_meet_and_join.as_or_more_precise
  let strictly_more_precise = Both_meet_and_join.strictly_more_precise

  let fast_equal = Type_equality.fast_equal
  let equal = Type_equality.equal

  let print = Type_printers.print
  let print_with_cache = Type_printers.print_with_cache

  module JE = Join_env

  type 'a type_accessor = Typing_env.t -> 'a

  let bottom_types_from_arity t =
    List.map (fun kind -> bottom kind) t

  let unknown_types_from_arity t =
    List.map (fun kind -> unknown kind) t

  let unknown_like_array t_array =
    Array.map (fun t -> unknown_like t) t_array

  let unit () =
    this_tagged_immediate Immediate.zero

  let unit_bottom () =
    bottom (K.value ())

  module Simplified_type : sig
    (* Simplified types omit the following at top level:
       - alias information;
       - joins between incompatible types (these turn into "Unknown").
    *)
    type t = private
      | Value of ty_value
      | Naked_number :
          'kind ty_naked_number * 'kind Flambda_kind.Naked_number.t -> t
      | Fabricated of ty_fabricated

    and ty_value = of_kind_value ty
    and 'a ty_naked_number = 'a of_kind_naked_number ty
    and ty_fabricated = of_kind_fabricated ty

    and 'a ty = private
      | Unknown
      | Ok of 'a * Name_permutation.t
      | Bottom

    (* Create a simple type from a type.  If the type has an alias at its
       top level stating that it is the type of some named value, that alias
       is (recursively) expanded, and the final ("canonical") simple value
       returned. *)
    val create : (flambda_type -> t * (Simple.t option)) type_accessor

    val is_unknown : t -> bool
    val is_bottom : t -> bool
  end = struct
    type 'a normal_ty = 'a ty

    type t =
      | Value of ty_value
      | Naked_number :
          'kind ty_naked_number * 'kind Flambda_kind.Naked_number.t -> t
      | Fabricated of ty_fabricated

    and ty_value = of_kind_value ty
    and 'a ty_naked_number = 'a of_kind_naked_number ty
    and ty_fabricated = of_kind_fabricated ty

    and 'a ty =
      | Unknown
      | Ok of 'a * Name_permutation.t
      | Bottom

    let is_unknown t =
      match t with
      | Value Unknown -> true
      | Naked_number (Unknown, _) -> true
      | Fabricated Unknown -> true
      | _ -> false

    let is_bottom t =
      match t with
      | Value Bottom -> true
      | Naked_number (Bottom, _) -> true
      | Fabricated Bottom -> true
      | _ -> false

    let ty_from_ty (ty : _ normal_ty) : _ ty =
      match ty with
      | Type _ | Equals _ -> Unknown
      | No_alias unknown_or_join ->
        match unknown_or_join with
        | Unknown -> Unknown
        | Join [] -> Bottom
        | Join [of_kind_foo, perm] -> Ok (of_kind_foo, perm)
        | Join _ -> Unknown

    let create env (t : flambda_type) : t * (Simple.t option) =
      let t, canonical_simple = Typing_env.resolve_aliases env t in
      let t : t =
        match t with
        | Value ty_value ->
          let ty_value : ty_value = ty_from_ty ty_value in
          Value ty_value
        | Naked_number (ty_naked_number, kind) ->
          let ty_naked_number : _ ty_naked_number =
            ty_from_ty ty_naked_number
          in
          Naked_number (ty_naked_number, kind)
        | Fabricated ty_fabricated ->
          let ty_fabricated : ty_fabricated = ty_from_ty ty_fabricated in
          Fabricated ty_fabricated
      in
      t, canonical_simple
  end

  let is_bottom env t =
    let simplified, _canonical_simple = Simplified_type.create env t in
    Simplified_type.is_bottom simplified

  let is_unknown env t =
    let simplified, _canonical_simple = Simplified_type.create env t in
    Simplified_type.is_unknown simplified

  let is_known env t =
    not (is_unknown env t)

  let is_useful env t =
    let simplified, _canonical_simple = Simplified_type.create env t in
    (not (Simplified_type.is_unknown simplified))
      && (not (Simplified_type.is_bottom simplified))

  let all_not_useful env ts =
    List.for_all (fun t -> not (is_useful env t)) ts

  type 'a proof =
    | Proved of 'a
    | Unknown
    | Invalid

  let _unknown_proof () = Unknown

  let prove_naked_float env t
        : Numbers.Float_by_bit_pattern.Set.t proof =
    let wrong_kind () =
      Misc.fatal_errorf "Wrong kind for something claimed to be a naked \
          float: %a"
        print t
    in
    let simplified, _canonical_simple = Simplified_type.create env t in
    match simplified with
    | Simplified_type.Naked_number (ty, K.Naked_number.Naked_float) ->
      begin match ty with
      | Unknown -> Unknown
      | Bottom -> Invalid
      | Ok (Float fs, _perm) -> Proved fs
      | Ok _ ->
        (* CR mshinwell: Find out why this case is still possible *)
        wrong_kind ()
      end
    | Simplified_type.Naked_number _ -> wrong_kind ()
    | Value _
    | Fabricated _ -> wrong_kind ()

  let prove_naked_int32 env t : Int32.Set.t proof =
    let wrong_kind () =
      Misc.fatal_errorf "Wrong kind for something claimed to be a naked \
          int32: %a"
        print t
    in
    let simplified, _canonical_simple = Simplified_type.create env t in
    match simplified with
    | Simplified_type.Naked_number (ty, K.Naked_number.Naked_int32) ->
      begin match ty with
      | Unknown -> Unknown
      | Bottom -> Invalid
      | Ok (Int32 is, _perm) -> Proved is
      | Ok _ -> wrong_kind ()
      end
    | Simplified_type.Naked_number _ -> wrong_kind ()
    | Value _
    | Fabricated _ -> wrong_kind ()

  let prove_naked_int64 env t : Int64.Set.t proof =
    let wrong_kind () =
      Misc.fatal_errorf "Wrong kind for something claimed to be a naked \
          int64: %a"
        print t
    in
    let simplified, _canonical_simple = Simplified_type.create env t in
    match simplified with
    | Simplified_type.Naked_number (ty, K.Naked_number.Naked_int64) ->
      begin match ty with
      | Unknown -> Unknown
      | Bottom -> Invalid
      | Ok (Int64 is, _perm) -> Proved is
      | Ok _ -> wrong_kind ()
      end
    | Simplified_type.Naked_number _ -> wrong_kind ()
    | Value _
    | Fabricated _ -> wrong_kind ()

  let prove_naked_nativeint env t : Targetint.Set.t proof =
    let wrong_kind () =
      Misc.fatal_errorf "Wrong kind for something claimed to be a naked \
          nativeint: %a"
        print t
    in
    let simplified, _canonical_simple = Simplified_type.create env t in
    match simplified with
    | Simplified_type.Naked_number (ty, K.Naked_number.Naked_nativeint) ->
      begin match ty with
      | Unknown -> Unknown
      | Bottom -> Invalid
      | Ok (Nativeint is, _perm) -> Proved is
      | Ok _ -> wrong_kind ()
      end
    | Simplified_type.Naked_number _ -> wrong_kind ()
    | Value _
    | Fabricated _ -> wrong_kind ()

  let prove_unique_naked_float env t : _ proof =
    match prove_naked_float env t with
    | Proved fs ->
      begin match Float.Set.get_singleton fs with
      | Some f -> Proved f
      | None -> Unknown
      end
    | Unknown -> Unknown
    | Invalid -> Invalid

  let prove_unique_naked_int32 env t : _ proof =
    match prove_naked_int32 env t with
    | Proved is ->
      begin match Int32.Set.get_singleton is with
      | Some f -> Proved f
      | None -> Unknown
      end
    | Unknown -> Unknown
    | Invalid -> Invalid

  let prove_unique_naked_int64 env t : _ proof =
    match prove_naked_int64 env t with
    | Proved is ->
      begin match Int64.Set.get_singleton is with
      | Some f -> Proved f
      | None -> Unknown
      end
    | Unknown -> Unknown
    | Invalid -> Invalid

  let prove_unique_naked_nativeint env t : _ proof =
    match prove_naked_nativeint env t with
    | Proved is ->
      begin match Targetint.Set.get_singleton is with
      | Some f -> Proved f
      | None -> Unknown
      end
    | Unknown -> Unknown
    | Invalid -> Invalid

(*
  let prove_closure env t : _ proof =
    let wrong_kind () =
      Misc.fatal_errorf "Wrong kind for something claimed to be a closure: %a"
        print t
    in
    let simplified, _canonical_simple = Simplified_type.create env t in
    match simplified with
    | Fabricated ty_fabricated ->
      begin match ty_fabricated with
      | Unknown -> Unknown
      | Bottom -> Invalid
      | Ok (Closure closure, perm) -> Proved closure
      | Ok _ -> Invalid
      end
    | Value _ -> wrong_kind ()
    | Simplified_type.Naked_number _ -> wrong_kind ()
*)

  type to_lift =
    | Boxed_float of Float.t
    | Boxed_int32 of Int32.t
    | Boxed_int64 of Int64.t
    | Boxed_nativeint of Targetint.t

  type reification_result =
    | Term of Simple.t * t
    | Lift of to_lift
    | Cannot_reify
    | Invalid

  let reify env ~allow_free_variables t : reification_result =
    let t, canonical_simple = Typing_env.resolve_aliases env t in
  (*
  Format.eprintf "CN is %a\n%!" (Misc.Stdlib.Option.print Name.print)
    canonical_simple;
  *)
    let can_lift =
      Name.Set.for_all (fun (name : Name.t) ->
          match name with
          | Var _ | Logical_var _ -> false
          | Symbol _ -> true)
        (Name_occurrences.everything_must_only_be_names (free_names t))
    in
    let simplified, canonical_simple' = Simplified_type.create env t in
    assert (Misc.Stdlib.Option.equal Simple.equal
      canonical_simple canonical_simple');
    if Simplified_type.is_bottom simplified then Invalid
    else
      let result, canonical_var =
        match canonical_simple with
        | Some ((Name (Symbol _) | Const _ | Discriminant _) as simple) ->
          Some (Term (simple, alias_type_of (kind t) simple)), None
        | Some ((Name ((Var _) as name)) as simple) ->
          if allow_free_variables
            && (not (Typing_env.was_existential_exn env name))
          then None, Some simple
          else None, None
        | Some (Name (Logical_var _)) | None -> None, None
      in
      match result with
      | Some result -> result
      | None ->
        let try_canonical_var () : reification_result =
          match canonical_var with
          | Some simple -> Term (simple, alias_type_of (kind t) simple)
          | None -> Cannot_reify
        in
        match simplified with
        | Value ty_value ->
          begin match ty_value with
          | Unknown -> try_canonical_var ()
          | Bottom -> Invalid
          | Ok (Blocks_and_tagged_immediates { blocks; immediates; }, _perm) ->
            if not (Blocks.is_empty blocks) then try_canonical_var ()
            else
              begin match Immediates.get_singleton immediates with
              | Some (imm, _env_extension) ->
                Term (Simple.const (Tagged_immediate imm), t)
              | None -> try_canonical_var ()
              end
          | Ok (Boxed_number (Boxed_float ty_naked_number), _perm) ->
            if not can_lift then try_canonical_var ()
            else
              let contents =
                of_ty_naked_number ty_naked_number K.Naked_number.Naked_float
              in
              begin match prove_unique_naked_float env contents with
              | Proved f -> Lift (Boxed_float f)
              | Unknown -> try_canonical_var ()
              | Invalid -> try_canonical_var ()
              end
          | Ok (Boxed_number (Boxed_int32 ty_naked_number), _perm) ->
            if not can_lift then try_canonical_var ()
            else
              let contents =
                of_ty_naked_number ty_naked_number K.Naked_number.Naked_int32
              in
              begin match prove_unique_naked_int32 env contents with
              | Proved i -> Lift (Boxed_int32 i)
              | Unknown -> try_canonical_var ()
              | Invalid -> try_canonical_var ()
              end
          | Ok (Boxed_number (Boxed_int64 ty_naked_number), _perm) ->
            if not can_lift then try_canonical_var ()
            else
              let contents =
                of_ty_naked_number ty_naked_number K.Naked_number.Naked_int64
              in
              begin match prove_unique_naked_int64 env contents with
              | Proved i -> Lift (Boxed_int64 i)
              | Unknown -> try_canonical_var ()
              | Invalid -> try_canonical_var ()
              end
          | Ok (Boxed_number (Boxed_nativeint ty_naked_number), _perm) ->
            if not can_lift then try_canonical_var ()
            else
              let contents =
                of_ty_naked_number ty_naked_number
                  K.Naked_number.Naked_nativeint
              in
              begin match prove_unique_naked_nativeint env contents with
              | Proved i -> Lift (Boxed_nativeint i)
              | Unknown -> try_canonical_var ()
              | Invalid -> try_canonical_var ()
              end
          | Ok ((Closures _ | String _), _perm) -> try_canonical_var ()
          end
        | Simplified_type.Naked_number (ty_naked_number, _) ->
          begin match ty_naked_number with
          | Unknown -> try_canonical_var ()
          | Bottom -> Invalid
          | Ok (Immediate imms, _perm) ->
            begin match Immediate.Set.get_singleton imms with
            | Some imm -> Term (Simple.const (Untagged_immediate imm), t)
            | None -> try_canonical_var ()
            end
          | Ok (Float fs, _perm) ->
            begin match Float.Set.get_singleton fs with
            | Some f -> Term (Simple.const (Naked_float f), t)
            | None -> try_canonical_var ()
            end
          | Ok (Int32 is, _perm) ->
            begin match Int32.Set.get_singleton is with
            | Some i -> Term (Simple.const (Naked_int32 i), t)
            | None -> try_canonical_var ()
            end
          | Ok (Int64 is, _perm) ->
            begin match Int64.Set.get_singleton is with
            | Some i -> Term (Simple.const (Naked_int64 i), t)
            | None -> try_canonical_var ()
            end
          | Ok (Nativeint is, _perm) ->
            begin match Targetint.Set.get_singleton is with
            | Some i -> Term (Simple.const (Naked_nativeint i), t)
            | None -> try_canonical_var ()
            end
          end
        | Fabricated (Ok (Set_of_closures _set_of_closures, _perm)) ->
          try_canonical_var ()
        | Fabricated (Ok (Discriminants discriminants, _perm)) ->
          begin match Discriminants.get_singleton discriminants with
          | None -> try_canonical_var ()
          | Some (discriminant, _env_extension) ->
            (* CR mshinwell: Here and above, should the [env_extension] be
               returned? *)
            Term (Simple.discriminant discriminant, t)
          end
        | Fabricated Unknown -> try_canonical_var ()
        | Fabricated Bottom -> Invalid

  (* CR mshinwell: rename to "prove_must_be_tagged_immediate" *)
  let prove_tagged_immediate env t : Immediate.Set.t proof =
    let wrong_kind () =
      Misc.fatal_errorf "Wrong kind for something claimed to be a tagged \
          immediate: %a"
        print t
    in
    let simplified, _canonical_simple = Simplified_type.create env t in
    match simplified with
    | Value ty_value ->
      begin match ty_value with
      | Unknown -> Unknown
      | Bottom -> Invalid
      | Ok (Blocks_and_tagged_immediates { blocks; immediates; }, _perm) ->
        if not (Blocks.is_empty blocks) then Invalid
        else
          begin match Immediates.all immediates with
          | Unknown -> Unknown
          | Known imms ->
            if Immediate.Set.is_empty imms then Invalid
            else Proved imms
          end
      | Ok (Boxed_number _, _perm) -> Invalid
      | Ok ((Closures _ | String _), _perm) -> Invalid
      end
    | Simplified_type.Naked_number _ -> wrong_kind ()
    | Fabricated _ -> wrong_kind ()

(*
  type tagged_immediate_as_discriminants_proof =
    | By_discriminant of Typing_env_extension.t Discriminant.Map.t

  let prove_tagged_immediate_as_discriminants env t
        : tagged_immediate_as_discriminants_proof proof =
    let wrong_kind () =
      Misc.fatal_errorf "Wrong kind for something claimed to be a tagged \
          immediate: %a"
        print t
    in
    let simplified, _canonical_simple = Simplified_type.create env t in
    Simplified_type.check_not_phantom simplified
      "prove_tagged_immediate_as_discriminants";
    match simplified with
    | Value ty_value ->
      begin match ty_value with
      | Unknown -> Unknown
      | Bottom -> Invalid
      | Ok (Blocks_and_tagged_immediates blocks_imms) ->
        begin match blocks_imms.blocks, blocks_imms.immediates with
        | Unknown, _ | _, Unknown -> Unknown
        | Known blocks, Known imms ->
          match no_blocks blocks, Immediate.Map.is_empty imms with
          | true, true -> Invalid
          | false, false -> Unknown
          | true, false ->
            let by_discr =
              Immediate.Map.fold
                (fun imm (imm_case : immediate_case) by_discr ->
                  let bad_discriminant () =
                    Misc.fatal_errorf "Immediate %a cannot be interpreted \
                        as a discriminant.  In type: %a"
                      Immediate.print imm
                      print t
                  in
                  let imm = Immediate.to_targetint imm in
                  match Discriminant.create imm with
                  | None -> bad_discriminant ()
                  | Some discr ->
                    Discriminant.Map.add discr imm_case.env_extension by_discr)
                imms
                Discriminant.Map.empty
            in
            Proved (By_discriminant by_discr)
          | false, true -> Invalid
        end
      | Ok (Boxed_number _) -> Invalid
      | Ok (Closures _ | String _) -> Invalid
      end
    | Simplified_type.Naked_number _ -> wrong_kind ()
    | Fabricated _ -> wrong_kind ()

  type is_tagged_immediate =
    | Never_a_tagged_immediate
    | Always_a_tagged_immediate

  let prove_is_tagged_immediate env t : is_tagged_immediate proof =
    let wrong_kind () =
      Misc.fatal_errorf "Wrong kind for something claimed to be a tagged \
          immediate: %a"
        print t
    in
    let simplified, _canonical_simple = Simplified_type.create env t in
    Simplified_type.check_not_phantom simplified "prove_is_tagged_immediate";
    match simplified with
    | Value ty_value ->
      begin match ty_value with
      | Unknown -> Unknown
      | Bottom -> Invalid
      | Ok (Blocks_and_tagged_immediates blocks_imms) ->
        begin match blocks_imms.blocks, blocks_imms.immediates with
        | Unknown, _ | _, Unknown -> Unknown
        | Known blocks, Known imms ->
          match no_blocks blocks, Immediate.Map.is_empty imms with
          | true, true -> Invalid
          | false, false -> Unknown
          | true, false -> Proved Always_a_tagged_immediate
          | false, true -> Proved Never_a_tagged_immediate
        end
      | Ok (Boxed_number _) -> Invalid
      | Ok (Closures _ | String _) -> Invalid
      end
    | Simplified_type.Naked_number _ -> wrong_kind ()
    | Fabricated _ -> wrong_kind ()

  let valid_block_tag_for_kind ~tag ~(field_kind : K.t) =
    (* CR-someday mshinwell: Note that we could easily extend
       this to handle blocks of the other unboxed number kinds. *)
    match field_kind with
    | Value -> Tag.is_structured_block tag
    | Fabricated -> Tag.equal tag Tag.zero
    | Naked_number Naked_float -> Tag.equal tag Tag.double_array_tag
    | Naked_number _ | Phantom _ ->
      Misc.fatal_errorf "Bad kind for block field: %a"
        K.print field_kind

  let field_n_of_block env ({ known_tags_and_sizes; size_at_least_n; } : blocks)
        ~index ~field_kind =
    let params =
      Tag_and_size.Map.fold (fun tag_and_size params acc ->
          let tag = Tag_and_size.tag tag_and_size in
          if not (valid_block_tag_for_kind ~tag ~field_kind) then acc
          else
            let size = Tag_and_size.size tag_and_size in
            if Targetint.OCaml.(<) index size then params::acc
            else acc)
        known_tags_and_sizes
        []
    in
    let from_size_at_least_n =
      Targetint.OCaml.Map.find_first_opt
        (fun size -> Targetint.OCaml.(<) index size)
        size_at_least_n
    in
    let params =
      match from_size_at_least_n with
      | None -> params
      | Some (_size, new_params) -> new_params::params
    in
    let env = JE.create env in
    match params with
    | [] -> None
    | params0::params ->
      let params =
        List.fold_left (fun joined_params params ->
            Parameters.join env joined_params params)
          params0
          params
      in
      match Parameters.nth params index with
      | Some kinded_param ->
        let name = Kinded_parameter.name kinded_param in
        let env_extension = Parameters.standalone_extension params in
        Some (name, env_extension)
      | None ->
        Misc.fatal_errorf "[Parameters.t] should contain index %a:@ %a"
          Targetint.OCaml.print index
          Parameters.print params

  let prove_get_field_from_block env t ~index ~field_kind
        : (Name.t * Typing_env_extension.t) proof =
  (*
  Format.eprintf "get_field_from_block index %a type@ %a\n"
    Targetint.OCaml.print index print t;
  *)
    let wrong_kind () =
      Misc.fatal_errorf "Wrong kind for something claimed to be a block: %a"
        print t
    in
    let simplified, _canonical_simple = Simplified_type.create env t in
    Simplified_type.check_not_phantom simplified "prove_get_field_from_block";
    match simplified with
    | Value ty_value ->
      begin match ty_value with
      | Unknown -> Unknown
      | Bottom -> Invalid
      | Ok (Blocks_and_tagged_immediates blocks_imms) ->
        if Targetint.OCaml.compare index Targetint.OCaml.zero < 0 then Invalid
        else
          begin match blocks_imms.blocks with
          | Unknown -> Unknown
          | Known blocks ->
            assert (not (no_blocks blocks));
            begin match field_n_of_block env blocks ~index ~field_kind with
            | None -> Invalid
            | Some result -> Proved result
            end
          end 
      | Ok (Boxed_number _) -> Invalid
      | Ok (Closures _ | String _) -> Invalid
      end
    | Simplified_type.Naked_number _ -> wrong_kind ()
    | Fabricated _ -> wrong_kind ()

  (* XXX re-enable *)
  let tags_all_valid _t (_blocks : blocks) ~kind_of_all_fields:_ = true
  (*
    Tag.Map.for_all (fun tag ((Blocks { by_length; }) : block_cases) ->
        Targetint.OCaml.Map.iter
          (fun _length (block : singleton_block) ->
            Array.iter (fun (field : _ mutable_or_immutable) ->
                match field with
                | Mutable -> ()
                | Immutable field ->
                  let field_kind = kind field in
                  let compatible =
                    K.compatible field_kind
                      ~if_used_at:kind_of_all_fields
                  in
                  if not compatible then begin
                    Misc.fatal_errorf "Kind %a is not compatible \
                        with all fields of this block: %a"
                      K.print kind_of_all_fields
                      print t
                  end)
              block.fields)
          by_length;
        valid_block_tag_for_kind ~tag ~field_kind:kind_of_all_fields)
      blocks
  *)

  let prove_must_be_a_block env t ~kind_of_all_fields : unit proof =
    let wrong_kind () =
      Misc.fatal_errorf "Wrong kind for something claimed to be a block: %a"
        print t
    in
    let simplified, _canonical_simple = Simplified_type.create env t in
    Simplified_type.check_not_phantom simplified "prove_is_a_block";
    match simplified with
    | Value ty_value ->
      begin match ty_value with
      | Unknown -> Unknown
      | Bottom -> Invalid
      | Ok (Blocks_and_tagged_immediates blocks_imms) ->
        begin match blocks_imms.blocks, blocks_imms.immediates with
        | Unknown, _ | _, Unknown -> Unknown
        | Known blocks, Known imms ->
          match no_blocks blocks, Immediate.Map.is_empty imms with
          | true, true -> Invalid
          | false, false -> Unknown
          | true, false -> Invalid
          | false, true ->
            let tags_all_valid = tags_all_valid t blocks ~kind_of_all_fields in
            if tags_all_valid then Proved () else Invalid
        end
      | Ok (Boxed_number _) -> Invalid
      | Ok (Closures _ | String _) -> Invalid
      end
    | Simplified_type.Naked_number _ -> wrong_kind ()
    | Fabricated _ -> wrong_kind ()

  type unboxable_variant_or_block_of_values0 = {
    block_sizes_by_tag : Targetint.OCaml.t Tag.Scannable.Map.t;
    constant_ctors : Immediate.Set.t;
  }

  type unboxable_variant_or_block_of_values =
    | Unboxable of unboxable_variant_or_block_of_values0
    | Not_unboxable

  let prove_unboxable_variant_or_block_of_values env t
        : unboxable_variant_or_block_of_values proof =
    let wrong_kind () =
      Misc.fatal_errorf "Wrong kind for something claimed to be a \
          variant or block of values: %a"
        print t
    in
    let simplified, _canonical_simple = Simplified_type.create env t in
    Simplified_type.check_not_phantom simplified
      "prove_unboxable_variant_or_block_of_values";
    match simplified with
    | Value ty_value ->
      begin match ty_value with
      | Unknown -> Unknown
      | Bottom -> Invalid
      | Ok (Blocks_and_tagged_immediates blocks_imms) ->
        begin match blocks_imms.blocks, blocks_imms.immediates with
        | Unknown, _ | _, Unknown -> Unknown
        | Known { known_tags_and_sizes; size_at_least_n; }, Known imms ->
          if not (Targetint.OCaml.Map.is_empty size_at_least_n) then
            Proved Not_unboxable
          else
            let block_sizes_by_tag =
              Tag_and_size.Map.fold
                (fun tag_and_size _params block_sizes_by_tag ->
                  match block_sizes_by_tag with
                  | None -> None
                  | Some block_sizes_by_tag ->
                    let tag = Tag_and_size.tag tag_and_size in
                    match Tag.Scannable.of_tag tag with
                    | None -> None
                    | Some tag ->
                      let size = Tag_and_size.size tag_and_size in
                      let size_mismatch = ref false in
                      let block_sizes_by_tag =
                        Tag.Scannable.Map.update tag (function
                            | None -> Some size
                            | Some size' ->
                              if Targetint.OCaml.equal size size' then begin
                                Some size
                              end else begin
                                size_mismatch := true;
                                None
                              end)
                          block_sizes_by_tag
                      in
                      if !size_mismatch then None
                      else Some block_sizes_by_tag)
                known_tags_and_sizes
                (Some Tag.Scannable.Map.empty)
            in
            match block_sizes_by_tag with
            | None -> Proved Not_unboxable
            | Some block_sizes_by_tag ->
              let imms = Immediate.Map.keys imms in
  (* XXX re-enable
              if tags_all_valid t blocks ~kind_of_all_fields:(K.value ()) then
  *)
                Proved (Unboxable {
                  block_sizes_by_tag;
                  constant_ctors = imms;
                })
  (*
              else Proved Not_unboxable
  *)
        end
      | Ok (Boxed_number _) | Ok (Closures _ | String _) -> Invalid
      end
    | Simplified_type.Naked_number _ -> wrong_kind ()
    | Fabricated _ -> wrong_kind ()

  type float_array_proof =
    | Of_length of Targetint.OCaml.t
    | Not_unique_length

  let _ = Of_length Targetint.OCaml.zero
  let _ = Not_unique_length

  (* CR mshinwell: This should probably return the field types rather than
     just the length; then it can be exposed in the .mli. *)
  let prove_float_array _env _t : float_array_proof proof =
    Misc.fatal_error "Not yet implemented"
  (* XXX
    let wrong_kind () =
      Misc.fatal_errorf "Wrong kind for something claimed to be a float array: \
          %a"
        print t
    in
    let simplified, _canonical_simple = Simplified_type.create env t in
    Simplified_type.check_not_phantom simplified "prove_float_array";
    match simplified with
    | Value ty_value ->
      begin match ty_value with
      | Unknown -> Unknown
      | Bottom -> Invalid
      | Ok (Blocks_and_tagged_immediates blocks_imms) ->
        begin match blocks_imms.blocks, blocks_imms.immediates with
        | Unknown, _ | _, Unknown -> Unknown
        | Known blocks, Known imms ->
          if no_blocks blocks
            || not (Immediate.Map.is_empty imms)
          then Invalid
          else
            let cannot_unbox = ref false in
            (* CR mshinwell: share with previous function (maybe) *)
            let block_sizes_by_tag =
              Tag.Map.fold (fun tag (Blocks { by_length; }) blocks ->
                  match Targetint.OCaml.Map.get_singleton by_length with
                  | Some (length, _) ->
                    Tag.Map.add tag length blocks
                  | None ->
                    cannot_unbox := true;
                    blocks)
                blocks
                Tag.Map.empty
            in
            if !cannot_unbox then Proved Not_unique_length
            else if tags_all_valid t blocks ~kind_of_all_fields:(K.naked_float ())
            then
              match Tag.Map.get_singleton block_sizes_by_tag with
              | Some (tag, size) ->
                if Tag.equal tag Tag.double_array_tag then
                  Proved (Of_length size)
                else Invalid
              | None -> Invalid (* CR mshinwell: double-check *)
            else Invalid
        end
      | Ok (Boxed_number _) | Ok (Closures _ | String _) -> Invalid
      end
    | Simplified_type.Naked_number _ -> wrong_kind ()
    | Fabricated _ -> wrong_kind ()
  *)

  type tags =
    | Tags of Tag.Set.t

  (* CR mshinwell: There's a bit of a wart here (in conjunction with the
     [Get_tag] primitive -- some of these tags don't really make any sense in
     that context, e.g. for closures, since there isn't a
     [Blocks_and_tagged_immediates] description for them.  Double_array_tag
     is of course an exception... maybe there should be a submodule of Tag
     which permits < No_scan_tag and also Double_array_tag? *)
  let prove_tags env t : tags proof =
    let wrong_kind () =
      Misc.fatal_errorf "Wrong kind for something claimed to be a value: %a"
        print t
    in
    let simplified, _canonical_simple = Simplified_type.create env t in
    Simplified_type.check_not_phantom simplified "prove_tags";
    match simplified with
    | Value ty_value ->
      begin match ty_value with
      | Unknown -> Unknown
      | Bottom -> Invalid
      | Ok (Blocks_and_tagged_immediates blocks_imms) ->
        begin match blocks_imms.blocks with
        | Known { known_tags_and_sizes; size_at_least_n; } ->
          if not (Targetint.OCaml.Map.is_empty size_at_least_n) then Unknown
          else
            let tags_and_sizes = Tag_and_size.Map.keys known_tags_and_sizes in
            let tags =
              Tag_and_size.Set.fold (fun tag_and_size tags ->
                  Tag.Set.add (Tag_and_size.tag tag_and_size) tags)
                tags_and_sizes
                Tag.Set.empty
            in
            Proved (Tags tags)
        | Unknown -> Unknown
        end
      | Ok (Boxed_number (Boxed_float _)) ->
        Proved (Tags (Tag.Set.singleton Tag.double_tag))
      | Ok (Boxed_number (Boxed_int32 _)) ->
        Proved (Tags (Tag.Set.singleton Tag.custom_tag))
      | Ok (Boxed_number (Boxed_int64 _)) ->
        Proved (Tags (Tag.Set.singleton Tag.custom_tag))
      | Ok (Boxed_number (Boxed_nativeint _)) ->
        Proved (Tags (Tag.Set.singleton Tag.custom_tag))
      | Ok (Closures _) ->
        Proved (Tags (Tag.Set.singleton Tag.closure_tag))
      | Ok (String _) ->
        Proved (Tags (Tag.Set.singleton Tag.string_tag))
      end
    | Simplified_type.Naked_number _ -> wrong_kind ()
    | Fabricated _ -> wrong_kind ()

  let prove_string env t : String_info.Set.t proof =
    let wrong_kind () =
      Misc.fatal_errorf "Wrong kind for something claimed to be a string: %a"
        print t
    in
    let simplified, _canonical_simple = Simplified_type.create env t in
    Simplified_type.check_not_phantom simplified "prove_string";
    match simplified with
    | Value ty_value ->
      begin match ty_value with
      | Unknown -> Unknown
      | Bottom -> Invalid
      | Ok (String strs) -> Proved strs
      | Ok (Blocks_and_tagged_immediates _ | Closures _) -> Invalid
      | Ok (Boxed_number _) -> Invalid
      end
    | Simplified_type.Naked_number _ -> wrong_kind ()
    | Fabricated _ -> wrong_kind ()
*)

  let prove_boxed_float env t : Float.Set.t ty_naked_number proof =
    let wrong_kind () =
      Misc.fatal_errorf "Wrong kind for something claimed to be a boxed \
          float: %a"
        print t
    in
    let simplified, _canonical_simple = Simplified_type.create env t in
    match simplified with
    | Value ty_value ->
      begin match ty_value with
      | Unknown -> Unknown
      | Bottom -> Invalid
      | Ok (Boxed_number (Boxed_float ty_naked_number), _perm) ->
        Proved ty_naked_number
      | Ok _ -> Invalid
      end
    | Simplified_type.Naked_number _ -> wrong_kind ()
    | Fabricated _ -> wrong_kind ()

  let prove_boxed_int32 env t : Int32.Set.t ty_naked_number proof =
    let wrong_kind () =
      Misc.fatal_errorf "Wrong kind for something claimed to be a boxed \
          int32: %a"
        print t
    in
    let simplified, _canonical_simple = Simplified_type.create env t in
    match simplified with
    | Value ty_value ->
      begin match ty_value with
      | Unknown -> Unknown
      | Bottom -> Invalid
      | Ok (Boxed_number (Boxed_int32 ty_naked_number), _perm) ->
        Proved ty_naked_number
      | Ok _ -> Invalid
      end
    | Simplified_type.Naked_number _ -> wrong_kind ()
    | Fabricated _ -> wrong_kind ()

  let prove_boxed_int64 env t : Int64.Set.t ty_naked_number proof =
    let wrong_kind () =
      Misc.fatal_errorf "Wrong kind for something claimed to be a boxed \
          int64: %a"
        print t
    in
    let simplified, _canonical_simple = Simplified_type.create env t in
    match simplified with
    | Value ty_value ->
      begin match ty_value with
      | Unknown -> Unknown
      | Bottom -> Invalid
      | Ok (Boxed_number (Boxed_int64 ty_naked_number), _perm) ->
        Proved ty_naked_number
      | Ok _ -> Invalid
      end
    | Simplified_type.Naked_number _ -> wrong_kind ()
    | Fabricated _ -> wrong_kind ()

  let prove_boxed_nativeint env t : Targetint.Set.t ty_naked_number proof =
    let wrong_kind () =
      Misc.fatal_errorf "Wrong kind for something claimed to be a boxed \
          nativeint: %a"
        print t
    in
    let simplified, _canonical_simple = Simplified_type.create env t in
    match simplified with
    | Value ty_value ->
      begin match ty_value with
      | Unknown -> Unknown
      | Bottom -> Invalid
      | Ok (Boxed_number (Boxed_nativeint ty_naked_number), _perm) ->
        Proved ty_naked_number
      | Ok _ -> Invalid
      end
    | Simplified_type.Naked_number _ -> wrong_kind ()
    | Fabricated _ -> wrong_kind ()

(*
  let prove_closures env t : closures proof =
    let wrong_kind () =
      Misc.fatal_errorf "Wrong kind for something claimed to be one or more \
          closures: %a"
        print t
    in
    let simplified, _canonical_simple = Simplified_type.create env t in
    Simplified_type.check_not_phantom simplified "prove_closures";
    match simplified with
    | Value ty_value ->
      begin match ty_value with
      | Unknown -> Unknown
      | Bottom -> Invalid
      | Ok (Closures closures) -> Proved closures
      | Ok _ -> Invalid
      end
    | Simplified_type.Naked_number _ -> wrong_kind ()
    | Fabricated _ -> wrong_kind ()

  let prove_sets_of_closures env t : _ proof =
    let wrong_kind () =
      Misc.fatal_errorf "Wrong kind for something claimed to be a set of \
          closures: %a"
        print t
    in
    let simplified, canonical_simple = Simplified_type.create env t in
    Simplified_type.check_not_phantom simplified "prove_sets_of_closures";
    match simplified with
    | Fabricated ty_fabricated ->
      begin match ty_fabricated with
      | Unknown -> Unknown
      | Bottom -> Invalid
      | Ok (Set_of_closures set_of_closures) ->
        begin match canonical_simple with
        | None -> Proved (None, set_of_closures)
        | Some (Name name) -> Proved (Some name, set_of_closures)
        | Some (Const _ | Discriminant _) -> Invalid
        end
      | Ok _ -> Invalid
      end
    | Value _ -> wrong_kind ()
    | Simplified_type.Naked_number _ -> wrong_kind ()

  (* XXX What about [Obj.truncate]?
     In fact, what happens regarding this for block access too? *)

  (* XXX Lengths of strings: for this, I think we can assume that Obj.truncate
     is always illegal here *)

  let prove_lengths_of_arrays_or_blocks env t
        : Targetint.OCaml.Set.t proof =
    let wrong_kind () =
      Misc.fatal_errorf "Wrong kind for something claimed to be a block: %a"
        print t
    in
    let simplified, _canonical_simple = Simplified_type.create env t in
    Simplified_type.check_not_phantom simplified
      "prove_lengths_of_arrays_or_blocks";
    match simplified with
    | Value ty_value ->
      begin match ty_value with
      | Unknown -> Unknown
      | Bottom -> Invalid
      | Ok (Blocks_and_tagged_immediates blocks_imms) ->
        let no_immediates =
          match blocks_imms.immediates with
          | Known imms when Immediate.Map.is_empty imms -> true
          | Known _ | Unknown -> false
        in
        if not no_immediates then begin
          Invalid
        end else begin
          match blocks_imms.blocks with
          | Unknown -> Unknown
          | Known { known_tags_and_sizes; size_at_least_n; } ->
            if not (Targetint.OCaml.Map.is_empty size_at_least_n) then Unknown
            else
              let tags_and_sizes = Tag_and_size.Map.keys known_tags_and_sizes in
              let sizes =
                Tag_and_size.Set.fold (fun tag_and_size sizes ->
                    Targetint.OCaml.Set.add (Tag_and_size.size tag_and_size)
                      sizes)
                  tags_and_sizes
                  Targetint.OCaml.Set.empty
              in
              Proved sizes
        end
      | Ok (Boxed_number _) -> Invalid
      | Ok (Closures _ | String _) -> Invalid
      end
    | Simplified_type.Naked_number _ -> wrong_kind ()
    | Fabricated _ -> wrong_kind ()

  let prove_of_kind_value t =
    let actual_kind = kind t in
    let expected_kind = K.value () in
    if not (Flambda_kind.compatible actual_kind ~if_used_at:expected_kind)
    then begin
      Misc.fatal_errorf "Type should be compatible with kind %a but \
          has incompatible kind %a: %a"
        Flambda_kind.print expected_kind
        Flambda_kind.print actual_kind
        print t
      end;
    force_to_kind_value t

  let prove_of_kind_naked_float t =
    let actual_kind = kind t in
    let expected_kind = K.naked_float () in
    if not (Flambda_kind.compatible actual_kind ~if_used_at:expected_kind)
    then begin
      Misc.fatal_errorf "Type should be compatible with kind %a but \
          has incompatible kind %a: %a"
        Flambda_kind.print expected_kind
        Flambda_kind.print actual_kind
        print t
      end;
    force_to_kind_naked_float t

  let values_physically_equal (t1 : t) (t2 : t) =
    let check_aliases (ty1 : _ ty) (ty2 : _ ty) =
      match ty1, ty2 with
      | No_alias _, _ | _, No_alias _ ->
        (* We don't need to check cases such as immediates, where we could
           prove physical equality, since [simplify_eq_comp] in
           [Simplify_binary_primitive] will have already done that. *)
        false
      | Type _, _ | _, Type _ ->
        (* CR mshinwell: Presumably we could look up the Export_id.t in the
           environment and continue?  Maybe not worth it. *)
        false
      | Equals simple1, Equals simple2 ->
        (* CR mshinwell: (see comment in simplify_primitives.ml in the existing
           Flambda) We didn't used to check equality on variable aliases in case
           the variables weren't bound. However everything should be bound now,
           so this seems like it should be ok. (Remember that Name covers both
           variables and symbols.) *)
        Simple.equal simple1 simple2
    in
    match t1.phantom, t2.phantom with
    | Some _, _ | _, Some _ -> false
    | None, None ->
      match t1.descr, t2.descr with
      | Value ty_value1, Value ty_value2 ->
        check_aliases ty_value1 ty_value2
      | Naked_number (ty_naked_number1, _),
          Naked_number (ty_naked_number2, _) ->
        check_aliases ty_naked_number1 ty_naked_number2
      | Fabricated _, Fabricated _ -> false
      | _, _ ->
        Misc.fatal_errorf "Kind mismatch for [physically_equal]: %a versus %a"
          print t1
          print t2

  let values_structurally_distinct (env1, (t1 : t)) (env2, (t2 : t)) =
  Format.eprintf "SD check: %a vs %a\n%!" print t1 print t2;
    let simplified1, _canonical_simple1 = Simplified_type.create env1 t1 in
    let simplified2, _canonical_simple2 = Simplified_type.create env2 t2 in
    let module S = Simplified_type in
    if S.is_phantom simplified1 || S.is_phantom simplified2 then false
    else
      (* Note: this function relies on the fact that sets of "known values" are
         always exact, and never a subset of the possible known values. (This
         distinction would be important if, for example, a type's knowledge has
         to be cut down because it is getting too large. Some random subset
         cannot be chosen: we must forget all of the possible values. *)
      match simplified1.descr, simplified2.descr with
      | Value ty_value1, Value ty_value2 ->
        begin match ty_value1, ty_value2 with
        | Unknown, _ | _, Unknown | Bottom, _ | _, Bottom -> false
        | Ok of_kind_value1, Ok of_kind_value2 ->
          begin match of_kind_value1, of_kind_value2 with
          | Blocks_and_tagged_immediates
                { blocks = blocks1; immediates = imms1; },
              Blocks_and_tagged_immediates
                { blocks = blocks2; immediates = imms2; } ->
            (* CR-someday mshinwell: This could be improved if required. *)
            begin match blocks1, blocks2 with
            | Unknown, _ | _, Unknown -> false
            | Known { known_tags_and_sizes = known_tags_and_sizes1;
                      size_at_least_n = size_at_least_n1;
                    },
              Known { known_tags_and_sizes = known_tags_and_sizes2;
                      size_at_least_n = size_at_least_n2;
                    } ->
              begin match imms1, imms2 with
              | Unknown, _ | _, Unknown -> false
              | Known imms1, Known imms2 ->
                let imm_intersection =
                  Immediate.Set.inter (Immediate.Map.keys imms1)
                    (Immediate.Map.keys imms2)
                in
                if (not (Immediate.Set.is_empty imm_intersection))
                  || (not (Targetint.OCaml.Map.is_empty size_at_least_n1))
                  || (not (Targetint.OCaml.Map.is_empty size_at_least_n2))
                then
                  false
                else
                  let tags_and_sizes1 =
                    Tag_and_size.Map.keys known_tags_and_sizes1
                  in
                  let tags_and_sizes2 =
                    Tag_and_size.Map.keys known_tags_and_sizes2
                  in
                  Tag_and_size.Set.is_empty
                    (Tag_and_size.Set.inter tags_and_sizes1 tags_and_sizes2)
              end
            end
          | Blocks_and_tagged_immediates _, _
          | _, Blocks_and_tagged_immediates _ -> true
          | Boxed_number (Boxed_float ty_naked_number1),
              Boxed_number (Boxed_float ty_naked_number2) ->
            begin match
              prove_naked_float env1
                (of_ty_naked_number ty_naked_number1
                  K.Naked_number.Naked_float),
              prove_naked_float env2
                (of_ty_naked_number ty_naked_number2
                  K.Naked_number.Naked_float)
            with
            | Proved nums1, Proved nums2 ->
              Float_by_bit_pattern.Set.is_empty
                (Float_by_bit_pattern.Set.inter nums1 nums2)
            | _, _ -> false
            end
          | Boxed_number (Boxed_int32 ty_naked_number1),
              Boxed_number (Boxed_int32 ty_naked_number2) ->
            begin match
              prove_naked_int32 env1
                (of_ty_naked_number ty_naked_number1
                  K.Naked_number.Naked_int32),
              prove_naked_int32 env2
                (of_ty_naked_number ty_naked_number2
                  K.Naked_number.Naked_int32)
            with
            | Proved nums1, Proved nums2 ->
              Int32.Set.is_empty (Int32.Set.inter nums1 nums2)
            | _, _ -> false
            end
          | Boxed_number (Boxed_int64 ty_naked_number1),
              Boxed_number (Boxed_int64 ty_naked_number2) ->
            begin match
              prove_naked_int64 env1
                (of_ty_naked_number ty_naked_number1
                  K.Naked_number.Naked_int64),
              prove_naked_int64 env2
                (of_ty_naked_number ty_naked_number2
                  K.Naked_number.Naked_int64)
            with
            | Proved nums1, Proved nums2 ->
              Int64.Set.is_empty (Int64.Set.inter nums1 nums2)
            | _, _ -> false
            end
          | Boxed_number (Boxed_nativeint ty_naked_number1),
              Boxed_number (Boxed_nativeint ty_naked_number2) ->
            begin match
              prove_naked_nativeint env1
                (of_ty_naked_number ty_naked_number1
                  K.Naked_number.Naked_nativeint),
              prove_naked_nativeint env2
                (of_ty_naked_number ty_naked_number2
                  K.Naked_number.Naked_nativeint)
            with
            | Proved nums1, Proved nums2 ->
              Targetint.Set.is_empty (Targetint.Set.inter nums1 nums2)
            | _, _ -> false
            end
          | Boxed_number _, _ -> true
          | _, Boxed_number _ -> true
          | Closures _, Closures _ -> false
          | Closures _, _ | _, Closures _ -> true
          | String strs1, String strs2 ->
            String_info.Set.is_empty (String_info.Set.inter strs1 strs2)
          end
        end
      | S.Naked_number (_ty_naked_number1, K.Naked_number.Naked_immediate),
          S.Naked_number (_ty_naked_number2, K.Naked_number.Naked_immediate) ->
        (* CR-someday mshinwell: Support to be implemented later. *)
        false
      | S.Naked_number (_, K.Naked_number.Naked_float),
          S.Naked_number (_, K.Naked_number.Naked_float) ->
        begin match
          prove_naked_float env1 t1,
            prove_naked_float env2 t2
        with
        | Proved nums1, Proved nums2 ->
          Float_by_bit_pattern.Set.is_empty
            (Float_by_bit_pattern.Set.inter nums1 nums2)
        | _, _ -> false
        end
      | S.Naked_number (_, K.Naked_number.Naked_int32),
          S.Naked_number (_, K.Naked_number.Naked_int32) ->
        begin match
          prove_naked_int32 env1 t1,
            prove_naked_int32 env2 t2
        with
        | Proved nums1, Proved nums2 ->
          Int32.Set.is_empty
            (Int32.Set.inter nums1 nums2)
        | _, _ -> false
        end
      | S.Naked_number (_, K.Naked_number.Naked_int64),
          S.Naked_number (_, K.Naked_number.Naked_int64) ->
        begin match
          prove_naked_int64 env1 t1,
            prove_naked_int64 env2 t2
        with
        | Proved nums1, Proved nums2 ->
          Int64.Set.is_empty
            (Int64.Set.inter nums1 nums2)
        | _, _ -> false
        end
      | S.Naked_number (_, K.Naked_number.Naked_nativeint),
          S.Naked_number (_, K.Naked_number.Naked_nativeint) ->
        begin match
          prove_naked_nativeint env1 t1,
            prove_naked_nativeint env2 t2
        with
        | Proved nums1, Proved nums2 ->
          Targetint.Set.is_empty
            (Targetint.Set.inter nums1 nums2)
        | _, _ -> false
        end
      | Fabricated _, Fabricated _ -> false
      | _, _ ->
        Misc.fatal_errorf "Kind mismatch for [structurally_different]: %a \
            versus %a"
          print t1
          print t2

  let switch_arms env t ~arms =
    let no_env_extension = Typing_env_extension.empty in
    let wrong_kind () =
      Misc.fatal_errorf
        "Wrong kind for something claimed to be a discriminant: %a"
        print t
    in
    let unknown () =
      Discriminant.Map.fold (fun arm cont result ->
          Discriminant.Map.add arm (no_env_extension, cont) result)
        arms
        Discriminant.Map.empty
    in
    let invalid () = Discriminant.Map.empty in
    let simplified, _canonical_simple = Simplified_type.create env t in
    Simplified_type.check_not_phantom simplified "discriminant_switch_arms";
    match simplified with
    | Fabricated ty_fabricated ->
      begin match ty_fabricated with
      | Unknown -> unknown ()
      | Bottom -> invalid ()
      | Ok (Discriminant discriminant_map) ->
        Discriminant.Map.fold (fun arm cont result ->
            match Discriminant.Map.find arm discriminant_map with
            | exception Not_found -> result
            | { env_extension; } ->
              Discriminant.Map.add arm (env_extension, cont) result)
          arms
          Discriminant.Map.empty
      | Ok (Set_of_closures _) | Ok (Closure _) -> invalid ()
      end
    | Simplified_type.Naked_number _ -> wrong_kind ()
    | Value _ -> wrong_kind ()

  type unboxable_proof =
    | Variant_or_block_of_values of unboxable_variant_or_block_of_values0
    | Float_array of { length : Targetint.OCaml.t; }
    | Boxed_float
    | Boxed_int32
    | Boxed_int64
    | Boxed_nativeint
    | Cannot_unbox

  let prove_unboxable env ~unboxee_ty : unboxable_proof =
    let kind = kind unboxee_ty in
    if not (K.is_value kind) then Cannot_unbox
    else
      match prove_unboxable_variant_or_block_of_values env unboxee_ty with
      | Proved (Unboxable unboxable) -> Variant_or_block_of_values unboxable
      | Proved Not_unboxable -> Cannot_unbox
      | Invalid | Unknown ->
        match prove_float_array env unboxee_ty with
        | Proved (Of_length length) -> Float_array { length; }
        | Proved Not_unique_length -> Cannot_unbox
        | Invalid | Unknown ->
          match prove_boxed_float env unboxee_ty with
          | Proved _ty_naked_number -> Boxed_float
          | Invalid | Unknown ->
            match prove_boxed_int32 env unboxee_ty with
            | Proved _ty_naked_number -> Boxed_int32
            | Invalid | Unknown ->
              match prove_boxed_int64 env unboxee_ty with
              | Proved _ty_naked_number -> Boxed_int64
              | Invalid | Unknown ->
                match prove_boxed_nativeint env unboxee_ty with
                | Proved _ty_naked_number -> Boxed_nativeint
                | Invalid | Unknown -> Cannot_unbox
*)
end
