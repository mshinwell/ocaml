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

(* CR mshinwell: Decide on doc or non-doc comments in here.  There are some
   modules which aren't exposed in the interface but probably require
   documentation. *)

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

      let bottom () = create_bottom ()
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
      let product = RP.create indexes_to_vars env_extension in
      let size = Targetint.OCaml.of_int (List.length field_tys) in
      match open_or_closed with
      | Open -> RL.create_at_least size product
      | Closed tag -> RL.create_exactly tag size product

    let create_bottom = RL.create_bottom 

    let _invariant _t = () (* CR mshinwell: RL.invariant *)
    let print_with_cache = RL.print

    let equal = RL.equal
    let is_empty = RL.is_bottom

    let meet env t1 t2 : _ Or_bottom.t =
      match RL.meet env t1 t2 with
      | Bottom -> Bottom
      | Ok (t, product) ->
        Ok (t, RP.standalone_extension product)

    let join = RL.join

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
      if Type_equality.fast_equal env env t1 t2 then true
      else if Flambda_type0_core.is_obviously_bottom t1 then true
      else
        let meet_env =
          Meet_env.create env
            ~perm_left:(Name_permutation.create ())
            ~perm_right:(Name_permutation.create ())
        in
        let meet_t, env_extension = meet meet_env t1 t2 in
        let env = Typing_env.add_or_meet_env_extension env env_extension in
        Type_equality.equal env env meet_t t1

    let strictly_more_precise env t1 ~than:t2 =
      if Type_equality.fast_equal env env t1 t2 then false
      else if
        Flambda_type0_core.is_obviously_bottom t1
          && not (Flambda_type0_core.is_obviously_bottom t2)
      then true
      else
        let meet_env =
          Meet_env.create env
            ~perm_left:(Name_permutation.create ())
            ~perm_right:(Name_permutation.create ())
        in
        let meet_t, env_extension = meet meet_env t1 t2 in
        let env = Typing_env.add_or_meet_env_extension env env_extension in
        Type_equality.equal env env meet_t t1
          && not (Type_equality.equal env env meet_t t2)
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

      (* CR mshinwell: Add [Var_within_closure.rename] *)
      let rename t =
        Var_within_closure.wrap (Variable.rename (Var_within_closure.unwrap t))
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
      RP.create closure_elements_to_logical_variables env_extension

    let create_bottom = RP.create_bottom

    let print ~cache:_ ppf t = RP.print ppf t

    let meet = RP.meet
    let join = RP.join

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
      match RL.meet env t1 t2 with
      | Bottom -> Bottom
      | Ok (t, _set_of_closures_entry) -> Ok (t, Typing_env_extension.empty ())

    let join = RL.join

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
      match RL.meet env t1 t2 with
      | Bottom -> Bottom
      | Ok (t, _closures_entry) -> Ok (t, Typing_env_extension.empty ())

    let join = RL.join

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

      val equal
         : Type_equality_env.t
        -> Type_equality_result.t
        -> t
        -> t
        -> Type_equality_result.t

      val meet
         : Meet_env.t
        -> t
        -> t
        -> (t * Typing_env_extension.t) Or_bottom.t

      val join : Join_env.t -> t -> t -> t

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

      val equal
         : Type_equality_env.t
        -> Type_equality_result.t
        -> t
        -> t
        -> Type_equality_result.t

      val meet
         : Meet_env.t
        -> t
        -> t
        -> (t * Typing_env_extension.t) Or_bottom.t

      val join : Join_env.t -> t -> t -> t

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

    let create_inlinable_function_declaration function_decl
          ~invariant_params ~size ~direct_call_surrogate
          : function_declaration =
      Inlinable {
        function_decl;
        invariant_params;
        size;
        direct_call_surrogate;
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
              let new_perm =
                Name_permutation.compose ~first:existing_perm ~second:perm
              in
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

      let meet = Both_meet_and_join.meet_set_of_closures_entry
      let join = Both_meet_and_join.join_set_of_closures_entry

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

      let meet = Both_meet_and_join.meet_closures_entry
      let join = Both_meet_and_join.join_closures_entry

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
      function_decl : Flambda.Function_declaration.t;
      invariant_params : Variable.Set.t lazy_t;
      size : int option lazy_t;
      (** For functions that are very likely to be inlined, the size of the
          function's body. *)
      direct_call_surrogate : Closure_id.t option;
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

    val equal
       : Type_equality_env.t
      -> Type_equality_result.t
      -> t
      -> t
      -> Type_equality_result.t

    val meet
       : Meet_env.t
      -> t
      -> t
      -> (t * Typing_env_extension.t) Or_bottom.t

    val join : Join_env.t -> t -> t -> t

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
      let inner_rp = RP.create result_vars result_env_extension in
      let outer_rp =
        RP.create ~nested:inner_rp param_vars param_env_extension
      in
      Product outer_rp

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

    let equal env result t1 t2 =
      match t1, t2 with
      | Product rp1, Product rp2 -> RP.equal env result rp1 rp2
      | Unknown, Unknown -> result
      | Bottom, Bottom -> result
      | (Product _ | Unknown | Bottom), _ ->
        Type_equality_result.types_known_unequal ()

    let meet env t1 t2 : _ Or_bottom.t =
      match t1, t2 with
      | Product rp1, Product rp2 ->
        begin match RP.meet env rp1 rp2 with
        | Bottom -> Bottom
        | Ok (rp, env_extension) -> Ok (Product rp, env_extension)
        end
      | Product _, Unknown -> Ok (t1, Typing_env_extension.empty ())
      | Unknown, Product _ -> Ok (t2, Typing_env_extension.empty ())
      | Unknown, Unknown -> Ok (Unknown, Typing_env_extension.empty ())
      | Bottom, (Product _ | Bottom | Unknown)
      | (Product _ | Unknown), Bottom -> Bottom

    let join env t1 t2 =
      match t1, t2 with
      | Product rp1, Product rp2 -> Product (RP.join env rp1 rp2)
      | Bottom, Product _ -> t2
      | Product _, Bottom -> t1
      | Bottom, Bottom -> Bottom
      | Unknown, (Product _ | Bottom | Unknown)
      | (Product _ | Bottom), Unknown -> Unknown

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

    val central_typing_environment : t -> Typing_env.t

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
          holds_on_left
      in
      let extension1 =
        Typing_env_extension.meet t.env t.extension1 holds_on_left
      in
      let env_plus_extension2 =
        Typing_env.add_or_meet_env_extension t.env_plus_extension2
          holds_on_right
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
          let kind = Flambda_type0_core.kind ty in
          let env = Typing_env.add_definition env name kind in
          Typing_env.add_equation env name ty)
      in
      let t = { t with env; } in
      invariant t;
      t

    let _add_extensions_and_extend_central_environment t
          ~holds_on_left ~holds_on_right ~central_extension =
      let env =
        Meet_env.with_env t.env (fun env ->
          Typing_env.add_or_meet_env_extension env central_extension)
      in
      let t = { t with env; } in
      invariant t;
      add_extensions t ~holds_on_left ~holds_on_right

    let central_environment t = t.env

    let central_typing_environment t = Meet_env.env t.env

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
  end and Logical_variable_component : sig
    type t = Logical_variable.t
    
    val create : Flambda_kind.t -> t
    val kind : t -> Flambda_kind.t
    val rename : t -> t
    val in_compilation_unit : t -> Compilation_unit.t -> bool
    val equal : Type_equality_env.t -> t -> t -> bool
    val name : t -> Name.t

    include Map.With_set with type t := t
    include Contains_names.S with type t := t
  end = struct
    include Logical_variable
    
    let free_names t =
      Name_occurrences.singleton_in_types (Name (Name.logical_var t))
    
    (* CR mshinwell: This is strange.  Should logical variables not be in [Name]
       and instead separately in [Bindable_name]? *)
    let apply_name_permutation t perm =
      match Name_permutation.apply_name perm (Name.logical_var t) with
      | Logical_var var -> var
      | _ ->
        Misc.fatal_errorf "Illegal name permutation on logical variables: %a"
          Name_permutation.print perm
    
    let name t = Name.logical_var t
    
    let equal env t1 t2 =
      let t1 = apply_name_permutation t1 (Type_equality_env.perm_left env) in
      let t2 = apply_name_permutation t2 (Type_equality_env.perm_right env) in
      equal t1 t2
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
          | Join [], Join _ -> ou1, Typing_env_extension.empty ()
          | Join _, Join [] -> ou2, Typing_env_extension.empty ()
          | Join of_kind_foos1, Join of_kind_foos2 ->
            (* We rely on the invariant in flambda_type0_intf.ml.
               Everything in [of_kind_foos1] is mutually incompatible with each
               other; likewise in [of_kind_foos2]. *)
            let of_kind_foos, env_extension_from_meet =
              List.fold_left
                (fun (of_kind_foos, env_extension_from_meet)
                     (of_kind_foo, perm1) ->
                  (* [of_kind_foo] can be compatible with at most one of the
                     elements of [of_kind_foos]. *)
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
Format.eprintf "lengths: calcd %d, first %d, second %d\n%!"
  (List.length of_kind_foos)
  (List.length of_kind_foos1)
  (List.length of_kind_foos2);
            if same_as of_kind_foos1 then begin
Format.eprintf "case 1\n%!";
ou1, env_extension_from_meet
end
            else if same_as of_kind_foos2 then begin
Format.eprintf "case 2\n%!";
ou2, env_extension_from_meet
end
            else begin
Format.eprintf "case 3\n%!";
Join of_kind_foos, env_extension_from_meet
end

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
Format.eprintf "CS1 %a, CS2 %a\n%!"
  (Misc.Stdlib.Option.print Simple.print) canonical_simple1
  (Misc.Stdlib.Option.print Simple.print) canonical_simple2;
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
Format.eprintf "***\n%!";
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
Format.eprintf "Returning =%a, env_extension:@ %a\n%!"
  Simple.print simple1
  Typing_env_extension.print env_extension_from_meet;
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
Format.eprintf "Returning =%a, env_extension:@ %a\n%!"
  Simple.print simple1
  Typing_env_extension.print env_extension_from_meet;
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
          | Join [], Join _ -> uj2
          | Join _, Join [] -> uj1
          | Join of_kind_foos1, Join of_kind_foos2 ->
            let of_kind_foos =
              List.fold_left (fun of_kind_foos (of_kind_foo, perm1) ->
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

      and join_ty ?bound_name env
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
              ?bound_name
              or_alias1
          in
          let unknown_or_join2, canonical_simple2 =
            Typing_env.resolve_aliases_and_squash_unresolved_names_on_ty'
              (Join_env.environment_on_right env)
              ~force_to_kind:S.force_to_kind
              ~print_ty
              ?bound_name
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
          let all_aliases =
            match bound_name with
            | None -> all_aliases
            | Some bound_name ->
              let all_aliases_of_bound_name =
                Typing_env.aliases_of_simple
                  (Join_env.central_typing_environment env)
                  (Simple.name bound_name)
              in
              Name.Set.diff all_aliases all_aliases_of_bound_name
          in
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

      let meet_or_join_ty ?bound_name env
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
        E.switch_no_bottom meet_ty (join_ty ?bound_name) env or_alias1 or_alias2
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
          && Type_equality.fast_equal (Join_env.central_typing_environment env)
               (Join_env.central_typing_environment env) t1 t2
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
              (* CR mshinwell: We should be checking the [function_decls] too *)
              function_decl = function_decl1;
              invariant_params = invariant_params1;
              size = size1;
              direct_call_surrogate = direct_call_surrogate1;
            },
            Inlinable {
              function_decl = function_decl2;
              invariant_params = invariant_params2;
              size = size2;
              direct_call_surrogate = direct_call_surrogate2;
            } ->
            match E.op () with
            | Join ->
              let code_id1 =
                Expr.Function_declaration.code_id function_decl1
              in
              let code_id2 =
                Expr.Function_declaration.code_id function_decl2
              in
              if Code_id.equal code_id1 code_id2 then begin
                assert (Variable.Set.equal
                  (Lazy.force invariant_params1)
                  (Lazy.force invariant_params2));
                assert (Misc.Stdlib.Option.equal Pervasives.(=)
                  (Lazy.force size1) (Lazy.force size2));
                assert (Misc.Stdlib.Option.equal Closure_id.equal
                  direct_call_surrogate1 direct_call_surrogate2);
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
          E.switch Function_type.meet Function_type.join env ty1 ty2
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

      module Meet_and_join_of_kind_naked_immediate =
        Meet_and_join_naked_immediate.Make (E)
      module Meet_and_join_of_kind_naked_float =
        Meet_and_join_naked_float.Make (E)
      module Meet_and_join_of_kind_naked_int32 =
        Meet_and_join_naked_int32.Make (E)
      module Meet_and_join_of_kind_naked_int64 =
        Meet_and_join_naked_int64.Make (E)
      module Meet_and_join_of_kind_naked_nativeint =
        Meet_and_join_naked_nativeint.Make (E)
      module Meet_and_join_naked_immediate =
        Make_meet_or_join.Make (E) (Meet_and_join_of_kind_naked_immediate)
      module Meet_and_join_naked_float =
        Make_meet_or_join.Make (E) (Meet_and_join_of_kind_naked_float)
      module Meet_and_join_naked_int32 =
        Make_meet_or_join.Make (E) (Meet_and_join_of_kind_naked_int32)
      module Meet_and_join_naked_int64 =
        Make_meet_or_join.Make (E) (Meet_and_join_of_kind_naked_int64)
      module Meet_and_join_naked_nativeint =
        Make_meet_or_join.Make (E) (Meet_and_join_of_kind_naked_nativeint)

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
        perm_left =
          Name_permutation.compose ~first:t.perm_left ~second:perm_left;
        perm_right =
          Name_permutation.compose ~first:t.perm_right ~second:perm_right;
      }
  end and Parameters : sig
    include Contains_names.S

    val print : Format.formatter -> t -> unit

    val invariant : t -> unit

    val create_bottom : unit -> t

    val create : Flambda_types.t list -> t

    (** Greatest lower bound. *)
    val meet : Meet_env.t -> t -> t -> (t * Typing_env_extension.t) Or_bottom.t

    (** Least upper bound. *)
    val join : Join_env.t -> t -> t -> t
  end = struct
    include Relational_product.Make (Int_index) (Logical_variable_component)

    let create tys =
      let lvs = List.map (fun _ty -> Logical_variable.create ()) tys in
      let indexes_to_lvs =
        Targetint.OCaml.Map.of_list (
          List.mapi (fun index lv ->
              Targetint.OCaml.of_int index, lv)
            lvs)
      in
      let env_extension =
        List.fold_left2 (fun env_extension lv ty ->
            Typing_env_extension.add_equation env_extension lv ty)
          (Typing_env_extension.empty ())
          lvs tys
      in
      RP.create indexes_to_lvs env_extension
  end and Relational_product0 : sig
    (* A "relational product" represents a list of indexed products.  Each
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
        val equal
           : Type_equality_env.t
          -> t
          -> t
          -> bool
        val name : t -> Name.t
        val kind : t -> Flambda_kind.t
      end)
      (Nested : sig
        include Contains_names.S
(*
        val invariant : t -> unit
*)
        val print : Format.formatter -> t -> unit
        val print_with_cache
           : cache:Printing_cache.t
          -> Format.formatter
          -> t
          -> unit
        val equal
           : Type_equality_env.t
          -> Type_equality_result.t
          -> t
          -> t
          -> Type_equality_result.t
        val meet
           : Meet_env.t
          -> t
          -> t
          -> (t * Typing_env_extension.t) Or_bottom.t
        val join : Join_env.t -> t -> t -> t
      end) :
    sig
      (* See the signature of [Relational_product] below for documentation. *)

      type t

      include Contains_names.S with type t := t

      val invariant : t -> unit

      val print : Format.formatter -> t -> unit

      val print_with_cache
         : cache:Printing_cache.t
        -> Format.formatter
        -> t
        -> unit

      val create
         : ?nested:Nested.t
        -> Component.t Index.Map.t
        -> Typing_env_extension.t
        -> t

      val create_bottom : unit -> t

      val equal
         : Type_equality_env.t
        -> Type_equality_result.t
        -> t
        -> t
        -> Type_equality_result.t

      val meet
         : Meet_env.t
        -> t
        -> t
        -> (t * Typing_env_extension.t) Or_bottom.t

      val join : Join_env.t -> t -> t -> t

      val standalone_extension : t -> Typing_env_extension.t

      val introduce : t -> Typing_env.t -> Typing_env.t

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
        val equal
           : Type_equality_env.t
          -> t
          -> t
          -> bool
        val name : t -> Name.t
        val kind : t -> Flambda_kind.t
      end)
      (Nested : sig
        include Contains_names.S
(*
        val invariant : t -> unit
*)
        val print : Format.formatter -> t -> unit
        val print_with_cache
           : cache:Printing_cache.t
          -> Format.formatter
          -> t
          -> unit
        val equal
           : Type_equality_env.t
          -> Type_equality_result.t
          -> t
          -> t
          -> Type_equality_result.t
        val meet
           : Meet_env.t
          -> t
          -> t
          -> (t * Typing_env_extension.t) Or_bottom.t
        val join : Join_env.t -> t -> t -> t
      end) =
    struct
      module T0 : sig
        type t = {
          components_by_index : Component.t Index.Map.t;
          env_extension : Typing_env_extension.t;
          nested : Nested.t option;
        }

        include Contains_names.S with type t := t

        val invariant : t -> unit
        val print : Format.formatter -> t -> unit
        val print_with_cache
           : cache:Printing_cache.t
          -> Format.formatter
          -> t
          -> unit
        val equal
           : Type_equality_env.t
          -> Type_equality_result.t
          -> t
          -> t
          -> Type_equality_result.t
        val meet
           : Meet_env.t
          -> t
          -> t
          -> (t * Typing_env_extension.t * Component.t list) option Or_bottom.t
        val join : Join_env.t -> t -> t -> (t * Component.t list) option
        val standalone_extension : t -> Typing_env_extension.t
        val introduce : t -> Typing_env.t -> Typing_env.t
        val add_or_meet_equations
           : t
          -> Meet_env.t
          -> Typing_env_extension.t
          -> t
      end = struct
        type t = {
          components_by_index : Component.t Index.Map.t;
          env_extension : Typing_env_extension.t;
          nested : Nested.t option;
        }

        let invariant _t =
          (* CR mshinwell: This should check that the [env_extension] never
             contains [Definition]s for [Name]s occurring in the indexes. *)
          ()

        let print ppf { components_by_index; env_extension; nested; } =
          Format.fprintf ppf
            "@[<hov 1>(\
              @[<hov 1>(components_by_index@ %a)@]@ \
              @[<hov 1>(env_extension@ %a)@]@ \
              @[<hov 1>(nested@ %a)@])@]"
            (Index.Map.print Component.print) components_by_index
            Typing_env_extension.print env_extension
            (Misc.Stdlib.Option.print Nested.print) nested

        let print_with_cache ~cache ppf
              { components_by_index; env_extension; nested; } =
          Format.fprintf ppf
            "@[<hov 1>(\
              @[<hov 1>(components_by_index@ %a)@]@ \
              @[<hov 1>(env_extension@ %a)@]@ \
              @[<hov 1>(nested@ %a)@])@]"
            (Index.Map.print Component.print) components_by_index
            (Typing_env_extension.print_with_cache ~cache) env_extension
            (Misc.Stdlib.Option.print (Nested.print_with_cache ~cache)) nested

        let equal env result
              { components_by_index = components_by_index1;
                env_extension = env_extension1;
                nested = nested1;
              }
              { components_by_index = components_by_index2;
                env_extension = env_extension2;
                nested = nested2;
              } =
          let (>>=) = Type_equality_result.(>>=) in
          result
          >>= fun result ->
          Index.Map.fold2_stop_on_key_mismatch
            (fun _index component1 component2 result ->
              result
              >>= fun result ->
              Component.equal env result component1 component2)
            components_by_index1 components_by_index2 result
          >>= fun result ->
          let env =
            Index.Map.fold (fun _index component env ->
                let name = Component.name component in
                Type_equality_env.add_definition_typing_env_left env name ty)
              components_by_index1
              env
          in
          let env =
            Index.Map.fold (fun _index component env ->
                let name = Component.name component in
                Type_equality_env.add_definition_typing_env_right env name ty)
              components_by_index2
              env
          in
          Typing_env_extension.equal env result env_extension1 env_extension2
          >>= fun result ->
          match nested1, nested2 with
          | None, None -> result
          | None, Some _ | Some _, None ->
            Type_equality_result.types_known_unequal ()
          | Some nested1, Some nested2 ->
            Nested.equal env result nested1 nested2

        let free_names
              { components_by_index; env_extension; nested; } =
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
            | Some nested -> Nested.free_names nested
          in
          Name_occurrences.union_list [
            free_names_in_indexes;
            free_names_in_components;
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
            | Some nested -> Some (Nested.apply_name_permutation nested perm)
          in
          { components_by_index;
            env_extension;
            nested;
          }

        let indexes t = Index.Map.keys t.components_by_index

        (* CR mshinwell: The [kind] may not be needed in [Component] but it
           isn't clear yet. We can sort this out later. At present all
           relational products map to components of kind [Value]. *)
        (* XXX Isn't this wrong?  Float arrays... *)
        let kind = Flambda_kind.value ()

        let environment_for_meet_or_join env (t1 : t) (t2 : t)
              ~indexes =
          let components_by_index_in_result, env =
            Index.Set.fold (fun index (components_by_index_in_result, env) ->
                let component = Component.create kind in
Format.eprintf "Made fresh component %a for RP meet/join\n%!"
  Component.print component;
                let components_by_index_in_result =
                  Index.Map.add index component components_by_index_in_result
                in
                let env =
                  Join_env.add_definition_central_environment env
                    (Component.name component) (Flambda_type0_core.unknown kind)
                in
                components_by_index_in_result, env)
              indexes
              (Index.Map.empty, env)
          in
          let result_components =
            Index.Map.data components_by_index_in_result
          in
          let add_equalities_to_extension t =
            Index.Map.fold (fun index component env_extension ->
                let name = Component.name component in
                let kind = Component.kind component in
                match Index.Map.find index components_by_index_in_result with
                | exception Not_found -> env_extension
                | result_component ->
                  let result_name = Component.name result_component in
                  let name_ty =
                    Flambda_type0_core.alias_type_of kind
                      (Simple.name name)
                  in
                  Typing_env_extension.add_equation env_extension
                    result_name name_ty)
              t.components_by_index
              t.env_extension
          in
          let env_extension1 = add_equalities_to_extension t1 in
          let env_extension2 = add_equalities_to_extension t2 in
          let add_definitions_to_extension t env_extension =
            Index.Map.fold (fun index component env_extension ->
                if not (Index.Set.mem index indexes) then
                  env_extension
                else
                  let name = Component.name component in
                  let kind = Component.kind component in
                  Typing_env_extension.add_definition_at_beginning env_extension
                    name (Flambda_type0_core.unknown kind))
              t.components_by_index
              env_extension
          in
          let env_extension1 = add_definitions_to_extension t1 env_extension1 in
          let env_extension2 = add_definitions_to_extension t2 env_extension2 in
          env, env_extension1, env_extension2, components_by_index_in_result,
            result_components

        let meet env t1 t2 : _ Or_bottom.t =
          (* CR mshinwell: Use a proper type instead of "option" for the return
             value? *)
          if Meet_env.shortcut_precondition env && t1 == t2 then
            Ok None
          else
            let indexes = Index.Set.inter (indexes t1) (indexes t2) in
            if Index.Set.is_empty indexes then Bottom
            else
              let env = Join_env.create env in
Format.eprintf "For RP meet:@ t1: %a@;t2: %a\n%!"
  print t1 print t2;
              let env, env_extension1, env_extension2, components_by_index,
                  result_components =
                environment_for_meet_or_join env t1 t2 ~indexes
              in
Format.eprintf "Env for RP meet:@ env: %a@;env_extension1: %a@;env_extension2: %a\n%!"
  Typing_env.print (Meet_env.env (Join_env.central_environment env))
  Typing_env_extension.print env_extension1
  Typing_env_extension.print env_extension2;
              let env = Join_env.central_environment env in
              let env_extension =
                Typing_env_extension.meet env env_extension1 env_extension2
              in
              let nested : _ Or_bottom.t =
                match t1.nested, t2.nested with
                | None, None -> Ok None
                | Some nested1, Some nested2 ->
                  begin match Nested.meet env nested1 nested2 with
                  | Ok (nested, env_extension) ->
                    Ok (Some (nested, env_extension))
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
              | Ok None ->
                let t =
                  { components_by_index;
                    env_extension;
                    nested = None;
                  }
                in
                let env_extension = Typing_env_extension.empty () in
                Ok (Some (t, env_extension, result_components))
              | Ok (Some (nested, _env_extension)) ->
                (* CR mshinwell: _env_extension should always be empty, but
                   this seems like a wart *)
                let t =
                  { components_by_index;
                    env_extension;
                    nested = Some nested;
                  }
                in
                let env_extension = Typing_env_extension.empty () in
                Ok (Some (t, env_extension, result_components))

        let join env t1 t2 =
          (* CR mshinwell: Same comment as above re. return value.
             Or alternatively, maybe pass the components set into this
             function? *)
          if Join_env.shortcut_precondition env && t1 == t2 then
            None
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
              | Some nested1, Some nested2 ->
                Some (Nested.join env nested1 nested2)
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
            Some (t, result_components)

        let standalone_extension t =
          Index.Map.fold (fun _index component env_extension ->
              let name = Component.name component in
              let kind = Component.kind component in
              Typing_env_extension.add_definition_at_beginning env_extension
                name (Flambda_type0_core.unknown kind))
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
      end

      include Name_abstraction.Make_list (T0)

      let create_abstraction = create

      let bindable_name_list_from_components components =
        List.map (fun component : Bindable_name.t ->
            Name (Component.name component))
          components

      let create ?nested components_by_index env_extension : t =
        let in_binding_position =
          let components = Index.Map.data components_by_index in
          bindable_name_list_from_components components
        in
        let t0 : T0.t =
          { components_by_index;
            env_extension;
            nested;
          }
        in
        T0.invariant t0;
        create in_binding_position t0

      let create_bottom () =
        create Index.Map.empty (Typing_env_extension.empty ())

      let invariant t =
        pattern_match t ~f:(fun _ t0 -> T0.invariant t0)

      let print ppf t = print ppf t

      let print_with_cache ~cache ppf t = print_with_cache ~cache ppf t

      let equal env result t1 t2 =
        pattern_match_pair t1 t2 ~f:(fun _ t0_1 t0_2 ->
          T0.equal env result t0_1 t0_2)

      let meet env t1 t2 =
        pattern_match t1 ~f:(fun _ t0_1 ->
          pattern_match t2 ~f:(fun _ t0_2 : _ Or_bottom.t ->
            match T0.meet env t0_1 t0_2 with
            | Bottom -> Bottom
            | Ok None -> Ok (t1, Typing_env_extension.empty ())
            | Ok (Some (t0, env_extension, components)) ->
              let in_binding_position =
                bindable_name_list_from_components components
              in
              Ok (create_abstraction in_binding_position t0, env_extension)))

      let join env t1 t2 =
        pattern_match t1 ~f:(fun components1 t0_1 ->
          pattern_match t2 ~f:(fun _ t0_2 ->
            match T0.join env t0_1 t0_2 with
            | None -> create_abstraction components1 t0_1
            | Some (t0, components) ->
              let in_binding_position =
                bindable_name_list_from_components components
              in
              create_abstraction in_binding_position t0))

      let standalone_extension t =
        pattern_match t ~f:(fun _ t0 -> T0.standalone_extension t0)

      let introduce t env =
        pattern_match t ~f:(fun _ t0 -> T0.introduce t0 env)

      let add_or_meet_equations t env new_equations =
        pattern_match_map t ~f:(fun t0 ->
          T0.add_or_meet_equations t0 env new_equations)
    end
  end and Relational_product : sig
    (* CR mshinwell: See if we can simplify this.  Originally I wanted to
       recursively-define modules [T0] with another [T] inside
       [Relational_product.Make] such that [T0] contained
       "nested : T.t option" -- but this suffers from the double vision
       problem.  We can't use [module type of] on the result of the
       [Relational_product0] functor here as it produces an illegal recursive
       module reference error. *)
    module Make
      (Index : Name_like_intf.S)
      (Component : sig
        include Name_like_intf.S
        val create : Flambda_kind.t -> t
        val equal : Type_equality_env.t -> t -> t -> bool
        val name : t -> Name.t
        val kind : t -> Flambda_kind.t
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
         : ?nested:t
        -> Component.t Index.Map.t
        -> Typing_env_extension.t
        -> t

      val create_bottom : unit -> t

      (** A conservative approximation to equality. *)
      val equal
         : Type_equality_env.t
        -> Type_equality_result.t
        -> t
        -> t
        -> Type_equality_result.t

      (** Greatest lower bound of two relational products. *)
      val meet
         : Meet_env.t
        -> t
        -> t
        -> (t * Typing_env_extension.t) Or_bottom.t

      (** Least upper bound of two relational products. *)
      val join : Join_env.t -> t -> t -> t

      (** The environment extension associated with the given relational
          product, including at the start, existentially-bound definitions
          of each component to bottom (hence the name "standalone"). *)
      val standalone_extension : t -> Typing_env_extension.t

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
        val equal
           : Type_equality_env.t
          -> t
          -> t
          -> bool
        val name : t -> Name.t
        val kind : t -> Flambda_kind.t
      end) =
    struct
      module rec RP : sig
        (* CR mshinwell: This is the THIRD copy of this signature *)
        type t

        include Contains_names.S with type t := t

        val invariant : t -> unit

        val print : Format.formatter -> t -> unit

        val print_with_cache
           : cache:Printing_cache.t
          -> Format.formatter
          -> t
          -> unit

        val create
           : ?nested:RP.t
          -> Component.t Index.Map.t
          -> Typing_env_extension.t
          -> t

        val create_bottom : unit -> t

        val equal
           : Type_equality_env.t
          -> Type_equality_result.t
          -> t
          -> t
          -> Type_equality_result.t

        val meet
           : Meet_env.t
          -> t
          -> t
          -> (t * Typing_env_extension.t) Or_bottom.t

        val join : Join_env.t -> t -> t -> t

        val standalone_extension : t -> Typing_env_extension.t

        val introduce : t -> Typing_env.t -> Typing_env.t

        val add_or_meet_equations
           : t
          -> Meet_env.t
          -> Typing_env_extension.t
          -> t
      end = Relational_product0.Make (Index) (Component) (RP)

      include RP
    end
(*
  end and Relational_product_unit : sig
    include Contains_names.S
    val invariant : t -> unit
    val print : Format.formatter -> t -> unit
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
      -> (t * Typing_env_extension.t * Component.Set.t) option Or_bottom.t
    val join : Join_env.t -> t -> t -> (t * Component.Set.t) option
  end = struct
*)
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

        val equal
           : Type_equality_env.t
          -> Type_equality_result.t
          -> t
          -> t
          -> Type_equality_result.t

        val add_or_meet_equations
           : t
          -> Meet_env.t
          -> Typing_env_extension.t
          -> t

        val meet
           : Meet_env.t
          -> t
          -> t
          -> (t * Typing_env_extension.t) Or_bottom.t

        val join : Join_env.t -> t -> t -> t

        include Contains_names.S with type t := t
      end) :
    sig
      type t

      val print : cache:Printing_cache.t -> Format.formatter -> t -> unit

      val create_bottom : unit -> t

      val create_unknown : unit -> t

      val create_exactly : Tag.t -> Index.t -> Maps_to.t -> t

      val create_exactly_multiple : Maps_to.t Tag_and_index.Map.t -> t

      val create_at_least : Index.t -> Maps_to.t -> t

      val create_at_least_multiple : Maps_to.t Index.Map.t -> t

      val is_bottom : t -> bool

      val equal
         : Type_equality_env.t
        -> Type_equality_result.t
        -> t
        -> t
        -> Type_equality_result.t

      (** The [Maps_to] value which [meet] returns contains the join of all
          [Maps_to] values in the range of the row-like structure after the meet
          operation has been completed. *)
      val meet
         : Meet_env.t
        -> t
        -> t
        -> (t * Maps_to.t) Or_bottom.t

      val join : Join_env.t -> t -> t -> t

      val known : t -> Maps_to.t Tag_and_index.Map.t Or_unknown.t

      val at_least : t -> Maps_to.t Index.Map.t Or_unknown.t

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

        val equal
           : Type_equality_env.t
          -> Type_equality_result.t
          -> t
          -> t
          -> Type_equality_result.t

        val add_or_meet_equations
           : t
          -> Meet_env.t
          -> Typing_env_extension.t
          -> t

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
      end) =
    struct
      module Tag_and_index = struct
        include Tag_and_index

        let create tag index = tag, index
        let index (_tag, index) = index
      end

      module T0 = struct
        type t = {
          known : Maps_to.t Tag_and_index.Map.t;
          at_least : Maps_to.t Index.Map.t;
        }

        let print ~cache ppf ({ known; at_least } : t) =
          Format.fprintf ppf 
            "@[<v 1>(\
               @[<hov 1>(known@ %a)@]@ \
               @[<hov 1>(at_least@ %a)@])@]"
            (Tag_and_index.Map.print (Maps_to.print_with_cache ~cache)) known
            (Index.Map.print (Maps_to.print_with_cache ~cache)) at_least

        let create_bottom () =
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

        let equal env result
              { known = known1; at_least = at_least1; }
              { known = known2; at_least = at_least2; } =
          let (>>=) = Type_equality_result.(>>=) in
          result
          >>= fun result ->
          Tag_and_index.Map.fold2_stop_on_key_mismatch
            (fun _index maps_to1 maps_to2 result ->
              result
              >>= fun result ->
              Maps_to.equal env result maps_to1 maps_to2)
            known1 known2 result
          >>= fun result ->
          Index.Map.fold2_stop_on_key_mismatch
            (fun _index maps_to1 maps_to2 result ->
              result
              >>= fun result ->
              Maps_to.equal env result maps_to1 maps_to2)
            at_least1 at_least2 result

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
          let meet_or_join env t1 t2 =
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
                  E.switch' Maps_to.meet Maps_to.join env
                    maps_to1 from_at_least2
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
                  E.switch' Maps_to.meet Maps_to.join env
                    maps_to1 maps_to2
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

        let meet = Meet.meet_or_join
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

        let join_of_all_maps_to env t =
          (* Any name permutations have already been applied during
             [Meet.meet_or_join], above. *)
          let env = Join_env.clear_name_permutations (Join_env.create env) in
          List.fold_left (fun result maps_to ->
              Maps_to.join env maps_to result)
            (Maps_to.bottom ())
            (all_maps_to t)
      end

      type t = T0.t Or_unknown.t

      let print ~cache ppf (t : t) =
        match t with
        | Known t0 -> T0.print ~cache ppf t0
        | Unknown ->
          Format.fprintf ppf "%sT%s"
            (Misc_color.bold_red ())
            (Misc_color.reset ())

      let create_bottom () : t = Known (T0.create_bottom ())

      let create_unknown () : t = Unknown

      let create_exactly tag index maps_to : t =
        Known (T0.create_exactly tag index maps_to)

      let create_exactly_multiple known : t =
        Known (T0.create_exactly_multiple known)

      let create_at_least index maps_to : t =
        Known (T0.create_at_least index maps_to)

      let create_at_least_multiple at_least : t =
        Known (T0.create_at_least_multiple at_least)

      let equal env result (t1 : t) (t2 : t) =
        match t1, t2 with
        | Known t0_1, Known t0_2 -> T0.equal env result t0_1 t0_2
        | Known _, Unknown | Unknown, Known _ ->
          Type_equality_result.types_known_unequal ()
        | Unknown, Unknown -> result

      let apply_name_permutation (t : t) perm : t =
        match t with
        | Known t0 ->
          let t0' = T0.apply_name_permutation t0 perm in
          if t0 == t0' then t
          else Known t0'
        | Unknown -> Unknown

      let meet env (t1 : t) (t2 : t) : (t * Maps_to.t) Or_bottom.t =
        match t1, t2 with
        | Known t0_1, Known t0_2 ->
          let t0 = T0.meet (Join_env.create env) t0_1 t0_2 in
          Ok (Known t0, T0.join_of_all_maps_to env t0)
        | Unknown, Known t0 ->
          Ok (t2, T0.join_of_all_maps_to env t0)
        | Known t0, Unknown ->
          Ok (t1, T0.join_of_all_maps_to env t0)
        | Unknown, Unknown ->
          Ok (Unknown, Maps_to.bottom ())

      let join env (t1 : t) (t2 : t) : t =
        match t1, t2 with
        | Known t0_1, Known t0_2 -> Known (T0.join env t0_1 t0_2)
        | Unknown, t2 -> t2
        | t1, Unknown -> t1

      let is_bottom (t : t) =
        match t with
        | Known t0 -> T0.is_bottom t0
        | Unknown -> false

      let known (t : t) : _ Or_unknown.t =
        match t with
        | Known t0 -> Known (T0.known t0)
        | Unknown -> Unknown

      let at_least (t : t) : _ Or_unknown.t =
        match t with
        | Known t0 -> Known (T0.at_least t0)
        | Unknown -> Unknown

      let get_singleton (t : t) =
        match t with
        | Known t0 -> T0.get_singleton t0
        | Unknown -> None

      let free_names (t : t) =
        match t with
        | Known t0 -> T0.free_names t0
        | Unknown -> Name_occurrences.create ()
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

      val equal
         : Type_equality_env.t
        -> Type_equality_result.t
        -> t
        -> t
        -> Type_equality_result.t

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

        let meet env t1 t2 : _ Or_bottom.t =
          let t = meet env t1 t2 in
          if is_empty t then Bottom
          else Ok (t, empty ())

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

      let create_bottom = RL.create_bottom
      let create_unknown = RL.create_unknown

      let print = RL.print
      let equal = RL.equal

      let meet = RL.meet
      let join = RL.join

      let free_names = RL.free_names
      let apply_name_permutation = RL.apply_name_permutation

      let all t : _ Or_unknown.t =
        match RL.at_least t, RL.known t with
        | Unknown, _ | _, Unknown -> Unknown
        | Known indexes, Known known ->
          if not (Unit.Map.is_empty indexes) then Unknown
          else
            let things =
              Thing_without_names_and_unit.Set.fold (fun (thing, ()) things ->
                  Thing_without_names.Set.add thing things)
                (Thing_without_names_and_unit.Map.keys known)
                Thing_without_names.Set.empty
            in
            Known things

      let get_singleton t =
        match RL.get_singleton t with
        | None -> None
        | Some ((thing, ()), env_extension) -> Some (thing, env_extension)
    end
  end and Type_equality : sig
    val fast_equal
       : Typing_env.t
      -> Typing_env.t
      -> Flambda_types.t
      -> Flambda_types.t
      -> bool

    val equal
       : ?bound_name:Name.t
      -> Typing_env.t
      -> Typing_env.t
      -> Flambda_types.t
      -> Flambda_types.t
      -> bool

    val equal_with_env
       : ?bound_name:Name.t
      -> Type_equality_env.t
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

    let fast_equal env1 env2 ty1 ty2 =
      env1 == env2 && ty1 == ty2

    let (>>=) = Type_equality_result.(>>=)

    let equal_or_alias ?bound_name equal_unknown_or_join env result
          ~force_to_kind ~print_ty
          (or_alias1 : _ Flambda_types.or_alias)
          (or_alias2 : _ Flambda_types.or_alias) =
      let unknown_or_join1, canonical_simple1 =
        Typing_env.resolve_aliases_and_squash_unresolved_names_on_ty'
          (Type_equality_env.typing_env_left env)
          ~force_to_kind
          ~print_ty
          ~bound_name
          or_alias1
      in
      let unknown_or_join2, canonical_simple2 =
        Typing_env.resolve_aliases_and_squash_unresolved_names_on_ty'
          (Type_equality_env.typing_env_right env)
          ~force_to_kind
          ~print_ty
          ~bound_name
          or_alias2
      in
      let all_aliases1 =
        match canonical_simple1 with
        | None -> Name.Set.empty
        | Some canonical_simple ->
          Typing_env.aliases_of_simple (Type_equality_env.typing_env_left env)
            canonical_simple
      in
      let all_aliases2 =
        match canonical_simple2 with
        | None -> Name.Set.empty
        | Some canonical_simple ->
          Typing_env.aliases_of_simple (Type_equality_env.typing_env_right env)
            canonical_simple
      in
      let all_aliases1 =
        match bound_name with
        | None -> all_aliases1
        | Some bound_name -> Name.Set.remove bound_name all_aliases1
      in
      let all_aliases2 =
        match bound_name with
        | None -> all_aliases2
        | Some bound_name -> Name.Set.remove bound_name all_aliases2
      in
      let all_aliases1_minus_existentials =
        Name.Set.diff all_aliases1 (Type_equality_env.existentials_left env)
      in
      let all_aliases2_minus_existentials =
        Name.Set.diff all_aliases2 (Type_equality_env.existentials_right env)
      in
      if not (Name.Set.equal all_aliases1_minus_existentials
        all_aliases2_minus_existentials)
      then
        Type_equality_result.types_known_unequal ()
      else
        let result =
          let all_aliases1_that_are_existentials =
            Name.Set.inter all_aliases1
              (Type_equality_env.existentials_left env)
          in
          let all_aliases2_that_are_existentials =
            Name.Set.inter all_aliases2
              (Type_equality_env.existentials_right env)
          in
          let delay_existentials result names ~must_equal_one_of =
            Name.Set.fold (fun name result ->
                Type_equality_result.delay_existential result name
                  ~must_equal_one_of)
              names
              result
          in
          let result =
            delay_existentials result all_aliases1_that_are_exisistentials
              ~must_equal_one_of:all_aliases2_that_are_existentials
          in
          delay_existentials result all_aliases2_that_are_exisistentials
            ~must_equal_one_of:all_aliases1_that_are_existentials
        in
        equal_unknown_or_join env result unknown_or_join1 unknown_or_join2

    let equal_unknown_or_join equal_of_kind_foo env result
          (uj1 : _ Flambda_types.unknown_or_join)
          (uj2 : _ Flambda_types.unknown_or_join) =
      match uj1, uj2 with
      | Unknown, Unknown -> true
      | Join join1, Join join2 ->
        let rec loop join1 join2 result =
          if not (Type_equality_result.are_types_known_equal result) then
            result
          else
            match join1, join2 with
            | [], _::_ | _::_, [] ->
              Type_equality_result.types_known_unequal ()
            | [], [] -> result
            | of_kind_foo1::join1, of_kind_foo2::join2 ->
              let result = equal_of_kind_foo env result join1 join2 in
              loop join1 join2 result
        in
        loop join1 join2
      | Unknown, _
      | Join _, _ -> Type_equality_result.types_known_unequal ()

    let equal_ty ?bound_name equal_of_kind_foo env result ~force_to_kind
          ~print_ty ty1 ty2 =
      equal_or_alias ?bound_name (equal_unknown_or_join equal_of_kind_foo)
        env result ~force_to_kind ~print_ty ty1 ty2

    let rec equal_with_env ?bound_name env result
          (t1 : Flambda_types.t) (t2 : Flambda_types.t) =
      match t1, t2 with
      | Value ty_value1, Value ty_value2 ->
        equal_ty_value ?bound_name env result
          ~force_to_kind:Flambda_type0_core.force_to_kind_value
          ~print_ty:Type_printers.print_ty_value
          ty_value1 ty_value2
      | Naked_number (ty_naked_number1, Naked_immediate),
          Naked_number (ty_naked_number2, Naked_immediate) ->
        equal_ty_naked_number ?bound_name env result
          ~force_to_kind:Flambda_type0_core.force_to_kind_naked_immediate
          ~print_ty:Type_printers.print_ty_naked_immediate
          ty_naked_number1 ty_naked_number2
      | Naked_number (ty_naked_number1, Naked_float),
          Naked_number (ty_naked_number2, Naked_float) ->
        equal_ty_naked_number ?bound_name env result
          ~force_to_kind:Flambda_type0_core.force_to_kind_naked_float
          ~print_ty:Type_printers.print_ty_naked_float
          ty_naked_number1 ty_naked_number2
      | Naked_number (ty_naked_number1, Naked_int32),
          Naked_number (ty_naked_number2, Naked_int32) ->
        equal_ty_naked_number ?bound_name env result
          ~force_to_kind:Flambda_type0_core.force_to_kind_naked_int32
          ~print_ty:Type_printers.print_ty_naked_int32
          ty_naked_number1 ty_naked_number2
      | Naked_number (ty_naked_number1, Naked_int64),
          Naked_number (ty_naked_number2, Naked_int64) ->
        equal_ty_naked_number ?bound_name env result
          ~force_to_kind:Flambda_type0_core.force_to_kind_naked_int64
          ~print_ty:Type_printers.print_ty_naked_int64
          ty_naked_number1 ty_naked_number2
      | Naked_number (ty_naked_number1, Naked_nativeint),
          Naked_number (ty_naked_number2, Naked_nativeint) ->
        equal_ty_naked_number ?bound_name env result
          ~force_to_kind:Flambda_type0_core.force_to_kind_naked_nativeint
          ~print_ty:Type_printers.print_ty_naked_nativeint
          ty_naked_number1 ty_naked_number2
      | Fabricated ty_fabricated1, Fabricated ty_fabricated2 ->
        equal_ty_fabricated ?bound_name env result
          ~force_to_kind:Flambda_type0_core.force_to_kind_fabricated
          ~print_ty:Type_printers.print_ty_fabricated
          ty_fabricated1 ty_fabricated2
      | Value _, _ -> Type_equality_result.types_known_unequal ()
      | Naked_number _, _ -> Type_equality_result.types_known_unequal ()
      | Fabricated _, _ -> Type_equality_result.types_known_unequal ()

    and equal_ty_value ?bound_name env result ty_value1 ty_value2 =
      equal_ty ?bound_name equal_of_kind_value env result ty_value1 ty_value2

    and equal_ty_naked_number
       : type a.
         ?bound_name:Name.t
      -> Type_equality_env.t
      -> Type_equality_result.t
      -> a Flambda_types.ty_naked_number
      -> a Flambda_types.ty_naked_number
      -> Type_equality_result.t =
    fun ?bound_name env result
        (ty_naked_number1 : a Flambda_types.ty_naked_number)
        (ty_naked_number2 : a Flambda_types.ty_naked_number) ->
      equal_or_alias ?bound_name
        (equal_unknown_or_join equal_of_kind_naked_number)
        env result
        ty_naked_number1 ty_naked_number2

    and equal_ty_fabricated ?bound_name env result
          ty_fabricated1 ty_fabricated2 =
      equal_ty ?bound_name equal_of_kind_fabricated env result
        ty_fabricated1 ty_fabricated2

    and equal_of_kind_value env result
          ((v1 : Flambda_types.of_kind_value), perm1)
          ((v2 : Flambda_types.of_kind_value), perm2) =
      let env =
        Type_equality_env.compose_name_permutations env
          ~perm_left:perm1 ~perm_right:perm2
      in
      match v1, v2 with
      | Blocks_and_tagged_immediates blocks1,
          Blocks_and_tagged_immediates blocks2 ->
        equal_blocks_and_tagged_immediates env result blocks1 blocks2
      | Boxed_number (Boxed_float ty_naked_number1),
          Boxed_number (Boxed_float ty_naked_number2) ->
        equal_ty_naked_number env result ty_naked_number1 ty_naked_number2
      | Boxed_number (Boxed_int32 ty_naked_number1),
          Boxed_number (Boxed_int32 ty_naked_number2) ->
        equal_ty_naked_number env result ty_naked_number1 ty_naked_number2
      | Boxed_number (Boxed_int64 ty_naked_number1),
          Boxed_number (Boxed_int64 ty_naked_number2) ->
        equal_ty_naked_number env result ty_naked_number1 ty_naked_number2
      | Boxed_number (Boxed_nativeint ty_naked_number1),
          Boxed_number (Boxed_nativeint ty_naked_number2) ->
        equal_ty_naked_number env result ty_naked_number1 ty_naked_number2
      | Closures { by_closure_id = by_closure_id1; },
          Closures { by_closure_id = by_closure_id2; } ->
        Closures_entry_by_closure_id.equal env result
          by_closure_id1 by_closure_id2
      | String string_set1, String string_set2 ->
        if String_info.Set.equal string_set1 string_set2 then result
        else Type_equality_result.types_known_unequal ()
      | (Blocks_and_tagged_immediates _ | Boxed_number _
          | Closures _ | String _), _ ->
        Type_equality_result.types_known_unequal ()

    and equal_blocks_and_tagged_immediates env
          ({ immediates = immediates1; blocks = blocks1; }
            : Flambda_types.blocks_and_tagged_immediates)
          ({ immediates = immediates2; blocks = blocks2; }
            : Flambda_types.blocks_and_tagged_immediates) =
      Immediates.equal env immediates1 immediates2
      >>= fun result ->
      Blocks.equal env blocks1 blocks2

    and equal_function_declaration _env
          (decl1 : Flambda_types.function_declaration)
          (decl2 : Flambda_types.function_declaration) =
      match decl1, decl2 with
      | Inlinable decl1, Inlinable decl2 ->
        (* CR mshinwell: Add assertions like in the meet/join code? *)
        if Code_id.equal decl1.code_id decl2.code_id then result
        else Type_equality_result.types_known_unequal ()
      | Non_inlinable, Non_inlinable -> result
      | Inlinable _, Non_inlinable
      | Non_inlinable, Inlinable _ ->
        Type_equality_result.types_known_unequal ()

    and equal_of_kind_naked_number
       : type a b.
         Type_equality_env.t
      -> Type_equality_result.t
      -> (a Flambda_types.of_kind_naked_number * Name_permutation.t)
      -> (b Flambda_types.of_kind_naked_number * Name_permutation.t)
      -> Type_equality_result.t =
    fun _env (of_kind_naked_number1, _) (of_kind_naked_number2, _) ->
      match of_kind_naked_number1, of_kind_naked_number2 with
      | Immediate imms1, Immediate imms2 ->
        if Immediate.Set.equal imms1 imms2 then result
        else Type_equality_result.types_known_unequal ()
      | Float floats1, Float floats2 ->
        if Float.Set.equal floats1 floats2 then result
        else Type_equality_result.types_known_unequal ()
      | Int32 ints1, Int32 ints2 ->
        if Int32.Set.equal ints1 ints2 then result
        else Type_equality_result.types_known_unequal ()
      | Int64 ints1, Int64 ints2 ->
        if Int64.Set.equal ints1 ints2 then result
        else Type_equality_result.types_known_unequal ()
      | Nativeint ints1, Nativeint ints2 ->
        if Targetint.Set.equal ints1 ints2 then result
        else Type_equality_result.types_known_unequal ()
      | Immediate _, _ -> Type_equality_result.types_known_unequal ()
      | Float _, _ -> Type_equality_result.types_known_unequal ()
      | Int32 _, _ -> Type_equality_result.types_known_unequal ()
      | Int64 _, _ -> Type_equality_result.types_known_unequal ()
      | Nativeint _, _ -> Type_equality_result.types_known_unequal ()

    and equal_of_kind_fabricated env result
          ((of_kind_fabricated1 : Flambda_types.of_kind_fabricated), perm1)
          ((of_kind_fabricated2 : Flambda_types.of_kind_fabricated), perm2) =
      let env =
        Type_equality_env.compose_name_permutations env
          ~perm_left:perm1 ~perm_right:perm2
      in
      match of_kind_fabricated1, of_kind_fabricated2 with
      | Discriminants discrs1, Discriminants discrs2 ->
        Discriminants.equal env result discrs1 discrs2
      | Set_of_closures { closures = closures1; },
          Set_of_closures { closures = closures2; } ->
        Closure_ids.equal env result closures1 closures2
      | (Discriminants _ | Set_of_closures _), _ ->
        Type_equality_result.types_known_unequal ()

    and equal_closures_entry env result
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
      equal_function_declaration env result function_decl1 function_decl2
      >>= fun result ->
      Function_type.equal env result ty1 ty2
      >>= fun result ->
      Closure_elements.equal env result closure_elements1 closure_elements2
      >>= fun result ->
      equal_ty_fabricated env result set_of_closures1 set_of_closures2

    and equal_set_of_closures_entry env result
          ({ by_closure_id = by_closure_id1; }
            : Flambda_types.set_of_closures_entry)
          ({ by_closure_id = by_closure_id2; }
            : Flambda_types.set_of_closures_entry) =
      Types_by_closure_id.equal env result by_closure_id1 by_closure_id2

    let equal ~bound_name env_left env_right t1 t2 =
      let env = Type_equality_env.empty ~env_left ~env_right in
      let result = Type_equality_result.create () in
      let rec loop result =
        if not (Type_equality_result.types_known_equal result) then
          false
        else
          match Type_equality_result.next_pair_of_types result with
          | None -> true
          | Some (result, t1, t2) -> loop (equal_with_env env result t1 t2)
      in
      loop (equal_with_env ?bound_name env result t1 t2)
  end and Type_equality_env : sig
    type t

    val create
       : typing_env_left:Typing_env.t
      -> typing_env_right:Typing_env.t
      -> perm_left:Name_permutation.t
      -> perm_right:Name_permutation.t
      -> t

    val empty : env_left:Typing_env.t -> env_right:Typing_env.t -> t

    val print : Format.formatter -> t -> unit

    val typing_env_left : t -> Typing_env.t

    val typing_env_right : t -> Typing_env.t

    val replace_typing_environments
       : t
      -> left:Typing_env.t
      -> right:Typing_env.t
      -> t

    val add_definition_typing_env_left
       : t
      -> Name.t
      -> Flambda_types.t
      -> t

    val add_definition_typing_env_right
       : t
      -> Name.t
      -> Flambda_types.t
      -> t

    val perm_left : t -> Name_permutation.t

    val perm_right : t -> Name_permutation.t

    val shortcut_precondition : t -> bool

    val compose_name_permutations
       : t
      -> perm_left:Name_permutation.t
      -> perm_right:Name_permutation.t
      -> t
  end = struct
    type t = {
      typing_env_left : Typing_env.t;
      typing_env_right : Typing_env.t;
      perm_left : Name_permutation.t;
      perm_right : Name_permutation.t;
    }

    let print ppf { typing_env_left; typing_env_right;
                    perm_left; perm_right; } =
      Format.fprintf ppf
        "@[<hov 1>(\
          @[<hov 1>(typing_env_left@ %a)@]@ \
          @[<hov 1>(typing_env_right@ %a)@]@ \
          @[<hov 1>(perm_left@ %a)@]@ \
          @[<hov 1>(perm_right@ %a)@])@]"
        Typing_env.print typing_env_left
        Typing_env.print typing_env_right
        Name_permutation.print perm_left
        Name_permutation.print perm_right

    let create ~typing_env_left ~typing_env_right ~perm_left ~perm_right =
      { typing_env_left;
        typing_env_right;
        perm_left;
        perm_right;
      }

    let empty ~typing_env_left ~typing_env_right =
      create ~typing_env_left ~typing_env_right
        ~perm_left:(Name_permutation.create ())
        ~perm_right:(Name_permutation.create ())

    let typing_env_left t = t.typing_env_left
    let typing_env_right t = t.typing_env_right
    let perm_left t = t.perm_left
    let perm_right t = t.perm_right

    let replace_typing_environments t ~left:typing_env_left
          ~right:typing_env_right =
       { t with
         typing_env_left;
         typing_env_right;
       }

    let add_definition_typing_env_left t name ty =
      let level = Typing_env.max_level t.typing_env_left in
      let kind = Flambda_type0_core.kind ty in
      let typing_env_left = Typing_env.add t name level (Definition kind) in
      let typing_env_left = Typing_env.add t name level (Equation ty) in
      { t with typing_env_left; }

    let add_definition_typing_env_right t name ty =
      let level = Typing_env.max_level t.typing_env_right in
      let kind = Flambda_type0_core.kind ty in
      let typing_env_right = Typing_env.add t name level (Definition kind) in
      let typing_env_right = Typing_env.add t name level (Equation ty) in
      { t with typing_env_right; }

    let shortcut_precondition t =
      t.typing_env_left == t.typing_env_right
        && t.perm_left == t.perm_right

    let compose_name_permutations t ~perm_left ~perm_right =
      { typing_env_left = t.typing_env_left;
        typing_env_right = t.typing_env_right;
        perm_left =
          Name_permutation.compose ~first:t.perm_left ~second:perm_left;
        perm_right =
          Name_permutation.compose ~first:t.perm_right ~second:perm_right;
      }
  end and Type_equality_result : sig
    type t

    val create : unit -> t

    val (>>=) : t -> (t -> t) -> t

    val delay_existential : t -> Name.t -> must_equal_one_of:Name.Set.t -> t

    module Uses : sig
      type t

      val more_than_one_use_and_empty : t -> bool
    end

    val leaving_scope_of_existential
       : t
      -> bound_names:Name.Set.t
      -> Uses.t Name.Map.t * t

    val types_known_unequal : unit -> t

    val are_types_known_equal : t -> bool
  end = struct
    module Uses : sig
      type t

      val create : must_equal_one_of:Name.Set.t -> t

      val combine : t -> must_equal_one_of:Name.Set.t -> t

      val more_than_one_use_and_empty : t -> bool
    end = struct
      type t =
        | One of { must_equal_one_of : Name.Set.t; }
        | Many of { must_equal_one_of : Name.Set.t; }

      let create ~must_equal_one_of =
        One { must_equal_one_of; }

      let combine t ~must_equal_one_of:must_equal_one_of' =
        match t with
        | One { must_equal_one_of; }
        | Many { must_equal_one_of; } ->
          let must_equal_one_of =
            Name.Set.inter must_equal_one_of must_equal_one_of'
          in
          Many { must_equal_one_of; }

      let more_than_one_use_and_empty = function
        | One _ -> false
        | Many { must_equal_one_of; } -> Name.Set.is_empty must_equal_one_of
    end

    type t =
      | Ok of { delayed_existentials : Uses.t Name.Map.t; }
      | Unequal

    let create () =
      Ok { delayed_existentials = Name.Map.empty; }

    let (>>=) result f =
      match result with
      | Unequal -> Unequal
      | Ok result -> f result

    let delay_existential t name ~must_equal_one_of =
      match t with
      | Unequal -> Unequal
      | Ok { delayed_existentials; } ->
        let delayed_existentials =
          Name.Map.update name (function
              | None -> Some (Uses.create ~must_equal_one_of)
              | Some uses ->
                Some (Uses.combine uses ~must_equal_one_of))
            delayed_existentials
        in
        Ok { delayed_existentials; }

    let leaving_scope_of_existential t names =
      match t with
      | Unequal -> Unequal
      | Ok { delayed_existentials; } ->
        let check_now, delayed_existentials =
          Name.Map.partition (fun name must_equal_one_of ->
              Name.Set.mem name names)
            delayed_existentials
        in
        let t = Ok { delayed_existentials; } in
        check_now, t

    let types_known_unequal () = Unequal

    let are_types_known_equal t =
      match t with
      | Unequal -> false
      | Ok _ -> true
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
      | Unknown ->
        if unicode then
          Format.fprintf ppf "%s\u{22a4}%s" colour (Misc_color.reset ())
        else
          Format.fprintf ppf "%sT%s" colour (Misc_color.reset ())
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
          "(Blocks_and_immediates@ \
            @[<v>@[<hov 1>(blocks@ %a)@]@ \
            @[<hov 1>(immediates@ %a)@]@])"
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
        Format.fprintf ppf "@[<hov 1>(Val %a)@]"
          (print_ty_value_with_cache ~cache) ty
      | Naked_number (ty, _kind) ->
        Format.fprintf ppf "@[<hov 1>(Naked %a)@]" print_ty_naked_number ty
      | Fabricated ty ->
        Format.fprintf ppf "@[<hov 1>(Fab %a)@]"
          (print_ty_fabricated_with_cache ~cache) ty

    and print ppf t =
      let cache : Printing_cache.t = Printing_cache.create () in
      print_with_cache ~cache ppf t
  end and Types_by_closure_id : sig
    type t

    val print : cache:Printing_cache.t -> Format.formatter -> t -> unit

    val create : Flambda_types.t Closure_id.Map.t -> t

    val create_bottom : unit -> t

    val equal
       : Type_equality_env.t
      -> Type_equality_result.t
      -> t
      -> t
      -> Type_equality_result.t

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
      let rename t = t
    end

    include
      Relational_product.Make (Closure_id) (Logical_variable_component)

    (* CR mshinwell: Any line of the following form should be removed *)
    let print = print_with_cache

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
      create closure_ids_to_logical_variables env_extension
  end and Typing_env : sig 
    type t

    type binding_type = private
      | Normal
      | Was_existential

    val invariant : t -> unit

    val print : Format.formatter -> t -> unit

    val create : resolver:(Export_id.t -> Flambda_types.t option) -> t

    val create_using_resolver_from : t -> t

    val resolver : t -> (Export_id.t -> Flambda_types.t option)

    val is_empty : t -> bool

    val increment_scope_level_to : t -> Scope_level.t -> t

    val fast_equal : t -> t -> bool

    val domain : t -> Name_occurrences.t

    val add_definition : t -> Name.t -> Flambda_kind.t -> t

    val add_equation : t -> Name.t -> Flambda_types.t -> t

    val add_cse : t -> Simple.t -> Flambda_primitive.With_fixed_value.t -> t

    val find_exn : t -> Name.t -> Flambda_types.t * binding_type

    val find_opt : t -> Name.t -> (Flambda_types.t * binding_type) option

    val find_cse : t -> Flambda_primitive.t -> Simple.t option

    val mem : t -> Name.t -> bool

    val add_or_meet_env_extension
       : t
      -> Typing_env_extension.t
      -> t

    val add_or_meet_opened_env_extension
       : t
      -> Typing_env_level.t
      -> t

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

    val cut
       : t
      -> existential_if_defined_at_or_later_than:Scope_level.t
      -> Typing_env_extension.t

(*
    val scope_level_exn : t -> Name.t -> Scope_level.With_sublevel.t

    val was_existential_exn : t -> Name.t -> bool

    val fold
       : t
      -> init:'a
      -> f:('a
        -> Name.t
        -> binding_type
        -> Scope_level.With_sublevel.t
        -> typing_env_entry0
        -> 'a)
      -> 'a

    val iter
       : t
      -> f:(Name.t
        -> binding_type
        -> Scope_level.With_sublevel.t
        -> typing_env_entry0
        -> unit)
      -> unit

    val restrict_to_symbols : t -> t

    val filter
       : t
      -> f:(Name.t
        -> (Scope_level.With_sublevel.t * typing_env_entry0)
        -> bool)
      -> t
*)
  end = struct
    type binding_type = Normal | Was_existential

    type cached = {
      names_to_types : Flambda_types.t Name.Map.t;
      cse : Simple.t Flambda_primitive.With_fixed_value.Map.t;
      were_existentials : Name.Set.t;
      aliases : Name.Set.t Simple.Map.t;
    }

    type one_level = {
      level : Typing_env_level.t;
      just_after_level : cached;
    }

    type t = {
      resolver : (Export_id.t -> Flambda_types.t option);
      prev_levels : one_level Scope_level.Map.t;
      current_level : Scope_level.t * one_level;
    }

    let is_empty t = Name.Map.is_empty t.names_to_types

    let print_cached ~cache ppf
          { names_to_types; cse; were_existentials; aliases; } =
      Format.fprintf ppf
        "@[<hov 1>(\
            @[<hov 1>(names_to_types@ %a)@]@ \
            @[<hov 1>(cse@ %a)@]@ \
            @[<hov 1>(were_existentials@ %a)@]@ \
            @[<hov 1>(aliases@ %a)@])@]"
        (Name.Map.print (Type_printers.print_with_cache ~cache)) names_to_types
        (Flambda_primitive.With_fixed_value.Map.print Simple.print) cse
        Name.Set.print were_existentials
        (Simple.Map.print Name.Set.print) aliases

    let print_one_level ~cache ppf { level; just_after_level = _; } =
      Typing_env_level.print ppf level

    let print_with_cache ~cache ppf
          ({ resolver = _; prev_levels; current_level; } as t) =
      if is_empty t then
        Format.pp_print_string ppf "Empty"
      else
        Printing_cache.with_cache cache ppf "env" t (fun ppf () ->
          Format.fprintf ppf
            "@[<hov 1>(\
                @[<hov 1>(prev_levels@ %a)@]@ \
                @[<hov 1>(current_level@ (%a@ %a))@])@]"
            (Scope_level.Map.print (print_one_level ~cache)) prev_levels
            Scope_level.print (fst current_level)
            (print_one_level ~cache) (snd current_level))

(*
          (* CR mshinwell: Add flag to disable this filtering *)
          let names_to_levels =
            Name.Map.filter (fun name _entry ->
                not (Name.is_predefined_exception name))
              names_to_levels
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
          if Name.Set.is_empty just_after_current_level.were_existentials
              && Flambda_primitive.With_fixed_value.Map.is_empty
                   just_after_current_level.cse
          then
            Format.fprintf ppf
              "@[<hov 1>(\
                  @[<hov 1>(aliases@ %a)@]@ \
                  @[<hov 1>(levels@ %a)@]@ \
                  @[<hov 1>(names_to_types@ %a)@])@]"
              (Simple.Map.print Name.Set.print) aliases
              (Scope_level.Map.print Typing_env_level.print) levels
              (Name.Map.print (Type_printers.print_with_cache ~cache)
                names_to_types)
          else if Name.Set.is_empty were_existentials then
            Format.fprintf ppf
              "@[<hov 1>(\
                  @[<hov 1>(aliases@ %a)@]@ \
                  @[<hov 1>(levels@ %a)@]@ \
                  @[<hov 1>(cse@ %a)@]@ \
                  @[<hov 1>(names_to_types@ %a)@])@]"
              (Simple.Map.print Name.Set.print) aliases
              (Scope_level.Map.print Typing_env_level.print) levels
              (Flambda_primitive.With_fixed_value.Map.print Simple.print) cse
              (Name.Map.print (Type_printers.print_with_cache ~cache)
                names_to_types)
          else
            Format.fprintf ppf
              "@[<hov 1>(\
                  @[<hov 1>(aliases@ %a)@]@ \
                  @[<hov 1>(levels@ %a)@]@ \
                  @[<hov 1>(cse@ %a)@]@ \
                  @[<hov 1>(names_to_types@ %a)@]@ \
                  @[<hov 1>(were_existentials@ %a)@])@]"
              (Simple.Map.print Name.Set.print) aliases
              (Scope_level.Map.print Typing_env_level.print) levels
              (Flambda_primitive.With_fixed_value.Map.print Simple.print) cse
              (Name.Map.print (Type_printers.print_with_cache ~cache)
                names_to_types)
              Name.Set.print were_existentials)
*)

    let print ppf t =
      print_with_cache ~cache:(Printing_cache.create ()) ppf t

    let empty_one_level () =
      let just_after_level =
        { names_to_types = Name.Map.empty;
          cse = Flambda_primitive.With_fixed_value.Map.empty;
          were_existentials = Name.Set.empty;
          aliases = Simple.Map.empty;
        }
      in
      { level = Typing_env_level.empty ();
        just_after_level;
      }

    let create ~resolver =
      { resolver;
        prev_levels = Scope_level.Map.empty;
        current_level = (Scope_level.initial, empty_one_level ());
      }

    let create_using_resolver_from t = create ~resolver:t.resolver

    let resolver t = t.resolver

    let increment_scope_level_to t level =
      if Scope_level.(<=) level (fst t.current_level) then begin
        Misc.fatal_errorf "Invalid new level %a:@ %a"
          Scope_level.print level
          print t
      end;
      let current_level = Scope_level.next (fst t.current_level) in
      { t with
        current_level = (current_level, empty_one_level ());
      }

    let fast_equal t1 t2 =
      t1 == t2

    let domain t =
      let names =
        Name.Set.fold (fun name bindable_names ->
            Bindable_name.Set.add (Name name) bindable_names)
          (Name.Map.keys t.current_level.just_after_level.names_to_types)
          Bindable_name.Set.empty
      in
      Name_occurrences.create_from_set_in_terms names

    let find_exn t name : Flambda_types.t * binding_type =
      (* CR mshinwell: Maybe this should cause a fatal error and we shouldn't
         rely on catching the exception *)
      let ty = Name.Map.find name t.current_level.just_after_level.names_to_types in
      let binding_type : binding_type =
        if Name.Set.mem name t.current_level.just_after_level.were_existentials then
          Was_existential
        else
          Normal
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
      let canonical_simple =
        match bound_name with
        | None -> None
        | Some bound_name -> Simple.name bound_name
      in
      resolve_aliases seen ~canonical_simple ty

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

(*
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
              | Definition kind ->
                Some ((Definition kind) : typing_env_entry0)
              | Equation ty ->
                Some ((Equation ty) : typing_env_entry0)
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
              | Definition _kind -> Bindable_name.Set.empty
              | Equation ty ->
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
            match (entry : typing_env_entry0) with
            | Definition _kind -> ()
            | Equation ty ->
              ignore (Sys.opaque_identity (resolve_aliases ~bound_name t ty)))
          t.names_to_levels;
      end
*)

    let invariant _t = ()

    let mem t name =
      Name.Map.mem name t.current_level.just_after_level.names_to_types

(*
    let scope_level_exn t name =
      match Name.Map.find name t.names_to_levels with
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
*)

    let find_opt t name =
      match find_exn t name with
      | exception Not_found -> None
      | ty, binding_type -> Some (ty, binding_type)

    let find_cse (t : t) prim =
      match Flambda_primitive.With_fixed_value.create prim with
      | None -> None
      | Some prim ->
        match
          Flambda_primitive.With_fixed_value.Map.find
            prim t.current_level.just_after_level.cse
        with
        | exception Not_found -> None
        | name -> Some name

    let was_existential_exn t name =
      let _ty, binding_type = find_exn t name in
      match binding_type with
      | Normal -> false
      | Was_existential -> true

(*
    let _min_level_for_new_binding t =
      let all_levels = Scope_level.Map.keys t.levels_to_entries in
      match Scope_level.Set.max_elt_opt all_levels with
      | None -> Scope_level.initial
      | Some level -> level
*)

    type sense =
      | New_equation_must_be_more_precise
      | Existing_equation_must_be_more_precise

    let _print_sense ppf (sense : sense) =
      match sense with
      | New_equation_must_be_more_precise ->
        Format.fprintf ppf "New_equation_must_be_more_precise"
      | Existing_equation_must_be_more_precise ->
        Format.fprintf ppf "Existing_equation_must_be_more_precise"

    let invariant_for_new_equation t name ty =
      let free_names = Type_free_names.free_names ty in
      if Name_occurrences.mem free_names (Name name) then begin
        Misc.fatal_errorf "Cannot add binding@ %a = %a@ as it would produce \
            a circular dependency"
          Name.print name
          print_typing_env_entry entry
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
          print_typing_env_entry entry
          Scope_level.print level
          Scope_level.print min_level
          print_typing_environment t
      end;
  *)
      match find_opt t name with
      | None ->
        Misc.fatal_errorf "Cannot add@ %a = %a@ for name undefined in \
            environment:@ %a"
          Name.print name
          Flambda_types.print ty
          print t
      | Some _ -> ()

    (* XXX Invariant check: [current_level] should be the maximum key in
       [levels] *)

(*
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
      (* XXX This should be done by [equal] *)
      (* XXX And we need to think about this (likewise similar code in
         [add_or_meet_env_extension'].  If correctness is hinging on
         aliases being preserved, could we cause unsoundness by failing to add
         an alias due to not being able to prove that the corresponding type is
         more precise? *)
      let meet_ty, _ = Typing_env.resolve_aliases t meet_ty in
      let existing_ty, _ = Typing_env.resolve_aliases t existing_ty in
      let ty, _ = Typing_env.resolve_aliases t ty in
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
        ~f:(fun name _binding_type level (entry : typing_env_entry0) ->
          let ty =
            match entry with
            | Definition ty | Equation ty -> ty
          in
          let level = Scope_level.With_sublevel.level level in
          invariant_for_any_new_binding t name level (Equation ty);
          invariant_for_new_equation t name ty
            ~sense:Existing_equation_must_be_more_precise)
  *)

    let invariant_for_new_binding t name level
          (entry : typing_env_entry) =
      invariant_for_any_new_binding t name level entry;
      match entry with
      | Definition _ | CSE _ -> ()
      | Equation ty ->
        invariant_for_new_equation t name ty
          ~sense:New_equation_must_be_more_precise
*)
    let _ = ignore New_equation_must_be_more_precise
    let _ = ignore Existing_equation_must_be_more_precise

    let aliases_of_simple (t : t) (simple : Simple.t) =
      match Simple.Map.find simple t.current_level.just_after_level.aliases with
      | exception Not_found ->
        begin match simple with
        | Const _ | Discriminant _ -> Name.Set.empty
        | Name name -> Name.Set.singleton name
        end
      | aliases ->
        begin match simple with
        | Const _ | Discriminant _ -> aliases
        | Name name -> Name.Set.add name aliases
        end

    let add_definition t name kind =
      if mem t name then begin
        Misc.fatal_errorf "Cannot rebind %a in environment:@ %a"
          Name.print name
          print t
      end;
      let level =
        Typing_env_level.add_definition t.current_level.level name kind
      in
      let names_to_types =
        Name.Map.add name (Flambda_type0_core.unknown kind)
          t.current_level.just_after_level.names_to_types
      in
      let just_after_level =
        { t.current_level.just_after_level with
          names_to_types;
        }
      in
      let current_level =
        { level;
          just_after_level;
        }
      in
      let t =
        { t with
          current_level;
        }
      in
      invariant t;
      t

    let add_equation t name ty =
      if not (mem t name) then begin
        Misc.fatal_errorf "Cannot add equation on unbound name %a in \
            environment:@ %a"
          Name.print name
          print t
      end;
      invariant_for_new_equation t name ty;
      let alias = Flambda_type0_core.get_alias ty in
      let equation_with_reverse_alias_already_present =
        match alias with
        | None | Some (Const _ | Discriminant _) -> false
        | Some (Name alias) ->
          Name.Set.mem alias (aliases_of_simple t (Simple.name name))
      in
      if equation_with_reverse_alias_already_present then
        t
      else
        let level =
          Typing_env_level.add_or_replace_equation t.current_level.level name ty
        in
        let names_to_types =
          Name.Map.add name ty t.current_level.just_after_level.names_to_types
        in
        let just_after_level =
          { t.current_level.just_after_level with
            names_to_types;
          }
        in
        let current_level =
          { level;
            just_after_level;
          }
        in
        let t =
          { t with
            current_level;
          }
        in
        invariant t;
        t

    let add_cse t simple prim =
      let cse =
       Flambda_primitive.With_fixed_value.Map.update prim
         (function
           | None -> Some simple
           | Some simple ->
             (* Keep the existing (furthest-out) binding. *)
             Some simple)
         t.cse
      in
      if cse == t.cse then t
      else { t with cse; }

    let rec add_or_meet_opened_env_extension t level : t =
      let t =
        Name.Map.fold (fun name kind t ->
            add_definition t name kind)
          (Typing_env_level.defined_names level)
          t
      in
      let t =
        Name.Map.fold (fun name ty t ->
            match find_opt t name with
            | None -> add_equation t name ty
            | Some (existing_ty, _binding_type) ->
              let meet_ty, meet_env_extension =
                let meet_env =
                  Meet_env.create t
                    ~perm_left:(Name_permutation.create ())
                    ~perm_right:(Name_permutation.create ())
                in
                Both_meet_and_join.meet meet_env ty existing_ty
              in
              let t = add_or_meet_env_extension t meet_env_extension in
              let as_or_more_precise =
                Flambda_type0_core.is_obviously_bottom meet_ty
                  || Type_equality.equal meet_ty ty
              in
              let strictly_more_precise =
                as_or_more_precise
                  && ((Flambda_type0_core.is_obviously_bottom meet_ty
                        && not (Flambda_type0_core.is_obviously_bottom ty))
                   || (not (Type_equality.equal meet_ty existing_ty)))
              in
              if not strictly_more_precise then t
              else add_equation t name ty)
          (Typing_env_level.equations level)
          t
      in
      Simple.Map.fold (fun simple prim ->
          add_cse t simple prim)
        (Typing_env_level.cse level)
        t

    and add_or_meet_env_extension t env_extension : t =
      Typing_env_extension.pattern_match env_extension
        ~f:(fun ~defined_names:_ level ->
          add_or_meet_opened_env_extension t level)

    let levels t = t.levels

    let current_level t = fst (t.current_level)
    let current_level_data t = snd (t.current_level)

    let cut t ~existential_if_defined_at_or_later_than:min_level =
      if Scope_level.(>) min_level (current_level t) then
        Typing_env_extension.empty ()
      else
        let all_levels =
          Scope_level.Map.add (current_level t) (current_level_data t)
            t.prev_levels
        in
        let strictly_less, at_min_level, strictly_greater =
          Scope_level.Map.split all_levels min_level
        in
        let at_or_after_cut =
          match at_min_level with
          | None -> strictly_greater
          | Some typing_env_level ->
            Scope_level.Map.add min_level typing_env_level strictly_greater
        in
        let t =
          if Scope_level.Map.is_empty strictly_less then
            empty ~resolver:t.resolver
          else
            let current_level, current_level_data =
              Scope_level.Map.max_elt strictly_less
            in
            let prev_levels =
              Scope_level.Map.remove current_level strictly_less
            in
            { resolver = t.resolver;
              prev_levels;
              current_level = (current_level, current_level_data);
            }
        in
        invariant t;
        let meet_env =
          Meet_env.create t
            ~perm_left:(Name_permutation.create ())
            ~perm_right:(Name_permutation.create ())
        in
        let level =
          Scope_level.Map.fold (fun _level typing_env_level result ->
              Typing_env_level.meet meet_env typing_env_level result)
            at_or_after_cut
            (Typing_env_level.empty ())
        in
        Typing_env_extension.create level

(*
Format.eprintf "Adding equation on %a: meet_ty is %a; ty %a; existing_ty %a; \
    AOMP %b; SMP %b\n%!"
  Name.print name
  Type_printers.print meet_ty
  Type_printers.print ty
  Type_printers.print existing_ty
  as_or_more_precise
  strictly_more_precise;
*)

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
(*
    let max_level t =
      match Scope_level.Map.max_binding_opt t.levels with
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
      let names_to_levels =
        Name.Map.filter (fun name _ty -> Name.Set.mem name allowed)
          t.names_to_levels
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
          names_to_levels;
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
      let symbols = Name.symbols_only_map t.names_to_levels in
      restrict_to_names0 t (Name.Map.keys symbols)

    let filter t ~f =
      let allowed =
        Name.Map.fold (fun name ty allowed ->
            if f name ty then Name.Set.add name allowed
            else allowed)
          t.names_to_levels
          Name.Set.empty
      in
      restrict_to_names0 t allowed

    let remove t name =
      let allowed = Name.Set.remove name (Name.Map.keys t.names_to_levels) in
      restrict_to_names0 t allowed

    let replace_most_recent t name ~(existing : names_to_levels_entry)
          (new_entry0 : typing_env_entry0) =
      let most_recent_ty =
        match existing.most_recent_entry with
        | Definition ty | Equation ty -> ty
      in
      assert (match Flambda_type0_core.get_alias most_recent_ty with
        | None -> true
        | Some _ -> false);
      let new_ty =
        match new_entry0 with
        | Definition ty | Equation ty -> ty
      in
      let aliases =
        match Flambda_type0_core.get_alias new_ty with
        | None -> t.aliases
        | Some alias ->
          Simple.Map.update alias (function
              | None -> Some (Name.Set.singleton name)
              | Some aliases -> Some (Name.Set.add name aliases))
            t.aliases
      in
      let names_to_levels_entry =
        { most_recent_entry = new_entry0;
          most_recent_level = existing.most_recent_level;
        }
      in
      let names_to_levels =
        Name.Map.add name names_to_levels_entry t.names_to_levels
      in
      let levels_to_entries =
        Scope_level.Map.update
          existing.most_recent_level
          (function
            | None ->
              Misc.fatal_errorf "Level %a does not exist"
                Scope_level.print existing.most_recent_level
            | Some by_name ->
              let new_entry : typing_env_entry =
                match new_entry0 with
                | Definition ty -> Definition ty
                | Equation ty -> Equation ty
              in
              Some (Name.Map.add name new_entry by_name))
          t.levels_to_entries
      in
      { resolver = t.resolver;
        aliases;
        names_to_levels;
        cse = t.cse;
        levels_to_entries;
        were_existentials = t.were_existentials;
      }
*)

(* To be enabled when we re-add removal of unused existentially-bound vars
    let free_names_transitive t ty =
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
              match find_exn t name with
              | exception Not_found ->
                Misc.fatal_errorf "Unbound name %a whilst finding free names,@ \
                    transitively, of %a@ in environment@ %a"
                  Name.print name
                  Type_printers.print ty
                  print t
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

    let free_names_transitive_list (env : Typing_env.t) tys =
      let scope_level = Scope_level.next (Typing_env.max_level env) in
      let env = add_or_meet_env_extension env scope_level in
      List.fold_left (fun names ty ->
          Name_occurrences.union names (free_names_transitive env ty))
        (Name_occurrences.create ())
        tys
*)
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

    val equal
       : Type_equality_env.t
      -> Type_equality_result.t
      -> t
      -> t
      -> Type_equality_result.t

    val fast_equal : t -> t -> bool

    val empty : unit -> t

    val is_empty : t -> bool

    val create : Typing_env_level.t -> t

    val add_definition_at_beginning : t -> Name.t -> Flambda_types.t -> t

    val add_equation : ?env:Typing_env.t -> t -> Name.t -> Flambda_types.t -> t

    val add_cse : t -> Simple.t -> Flambda_primitive.With_fixed_value.t -> t

    val meet : Meet_env.t -> t -> t -> t

    val join : Join_env.t -> t -> t -> t

    val restrict_to_definitions : t -> t

    val restrict_names_to_those_occurring_in_types
       : t
      -> Typing_env.t
      -> Typing_env.t
      -> Flambda_types.t list
      -> t

    val diff : t -> Typing_env.t -> t

    val pattern_match
       : t
      -> f:(
           defined_names:Flambda_kind.t Name.Map.t
        -> equations:Flambda_types.t Name.Map.t
        -> cse:Simple.t Flambda_primitive.With_fixed_value.Map.t
        -> 'a)
      -> 'a
  end = struct
    module A = Name_abstraction.Make_list (Typing_env_level)

    (* The record is here to avoid the double vision problem.  (Otherwise
       there would already be an equality
         t = Name_abstraction.Make_list (Typing_env_level)
       meaning that the equality
         t = Typing_env_extension.t
       could not be added by the type checker.) *)
    type t = {
      abst : A.t;
    } [@@unboxed]

    let print ppf { abst; } = A.print ~style:Existential ppf abst

    let print_with_cache ~cache ppf { abst; } =
      A.print_with_cache ~style:Existential ~cache ppf abst

    let free_names { abst; } = A.free_names abst

    let apply_name_permutation ({ abst; } as t) perm =
      let abst' = A.apply_name_permutation abst perm in
      if abst == abst' then t
      else { abst = abst'; }

    let fast_equal t1 t2 = (t1 == t2)

    let equal env result t1 t2 =
      A.pattern_match_pair abst1 abst2 ~f:(fun existentials level1 level2 ->
        let (>>=) = Type_equality_result.(>>=) in
        let env, result =
          Typing_env_level.equal env result level1 level2
        in
        result
        >>= fun result ->
        let check_now, result =
          Type_equality_result.leaving_scope_of_existential result
            existentials
        in
        Name.Map.fold (fun _name uses result ->
            result
            >>= fun result ->
            if Type_equality_result.Uses.more_than_one_use_and_empty uses
            then Type_equality_result.types_known_unequal ()
            else result)
          check_now
          result)
        >>= fun _result ->
        assert (not (Type_equality_result.are_types_known_unequal result));
        true

    let invariant { abst; } =
      A.pattern_match abst ~f:(fun _ level -> Typing_env_level.invariant level)

    let empty () =
      { abst = A.create [] (Typing_env_level.empty ()); }

    let is_empty { abst; } =
      A.pattern_match abst ~f:(fun _ level -> Typing_env_level.is_empty level)

(*
    let restrict_to_definitions { abst; } =
      let abst =
        A.pattern_match_mapi abst ~f:(fun defined_names level ->
          (* CR mshinwell: Does "in terms" really make sense (or is it even
             correct?) here? *)
          Typing_env_level.restrict_to_names level
            (Name_occurrences.create_from_name_set_in_terms defined_names))
      in
      { abst; }

    let restrict_names_to_those_occurring_in_types _t _env _env_allowed_names
          _tys =
      Misc.fatal_error "Not yet implemented"
*)
(*
      let free_names = free_names_transitive_list t env tys in
      let env_allowed_names = Typing_env.domain env_allowed_names in
      let allowed_names = Name_occurrences.union free_names env_allowed_names in
      pattern_match_map t ~f:(fun level ->
        Typing_env_level.restrict_to_names level allowed_names)
*)

    (* CR mshinwell: We should provide a termination proof for algorithms
       such as this. *)
    let meet (env : Meet_env.t) (t1 : t) (t2 : t) : t =
      if Meet_env.shortcut_precondition env && fast_equal t1 t2 then t1
      else if is_empty t1 then empty ()
      else if is_empty t2 then empty ()
      else
        let t1 = apply_name_permutation t1 (Meet_env.perm_left env) in
        let t2 = apply_name_permutation t2 (Meet_env.perm_right env) in
        let env = Meet_env.clear_name_permutations env in
        let abst =
          A.pattern_match t1.abst ~f:(fun _ level_1 ->
            A.pattern_match t2.abst ~f:(fun _ level_2 ->
              A.create (Typing_env_level.defined_names level)
                (Typing_env_level.meet env level_1 level_2)))
        in
        { abst; }

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
        let abst =
          A.pattern_match t1.abst ~f:(fun _ level_1 ->
            A.pattern_match t2.abst ~f:(fun _ level_2 ->
              A.create (Typing_env_level.defined_names level)
                (Typing_env_level.join env level_1 level_2)))
        in
        { abst; }

    let add_definition { abst; } name =
      A.pattern_match abst ~f:(fun defined_names level ->
        let level = Typing_env_level.add_definition level name in
        { abst = A.create (name :: defined_names) level; })

    let add_equation ?env ({ abst; } as t) name ty =
      let abst =
        A.pattern_match_map abst ~f:(fun level ->
          Typing_env_level.add_equation level name ty)
      in
      { abst; }

    let add_cse { abst; } name prim =
      let abst =
        A.pattern_match_map abst ~f:(fun level ->
          Typing_env_level.add_cse level name prim)
      in
      { abst; }

(*
    let diff { abst; } env =
      A.pattern_match abst ~f:(fun _ level ->
        let level = Typing_env_level.diff level env in
        let defined_names = Typing_env_level.defined_names level in
        { abst = A.create defined_names level; })
*)

    let pattern_match { abst; } ~f =
      A.pattern_match abst ~f:(fun _ level ->
        f ~defined_names:level.defined_names
          ~equations:level.equations
          ~cse:level.cse)
  end and Typing_env_level : sig
    include Contains_names.S

    val print_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> t
      -> unit

    val print : Format.formatter -> t -> unit

    val invariant : t -> unit

    val empty : unit -> t

    val is_empty : t -> unit

    val equal
       : Type_equality_env.t
      -> Type_equality_result.t
      -> t
      -> t
      -> Type_equality_result.t

(*
    val restrict_to_names : t -> Name_occurrences.t -> t
*)

    val find_opt : t -> Name.t -> Flambda_types.t option

    val existentially_bind : t -> Name.t -> Flambda_kind.t -> t

    val meet_equation : t -> Name.t -> Flambda_types.t -> t

    val add_or_replace_equation : t -> Name.t -> Flambda_types.t -> t

    val add_cse : t -> Name.t -> Flambda_primitive.With_fixed_value.t -> t

    val meet : Meet_env.t -> t -> t -> t

    val join : Join_env.t -> t -> t -> t

    val defined_names_set : t -> Bindable_name.Set.t

    val defined_names : t -> Flambda_kind.t Name.Map.t

    val equations_domain : t -> Name.Set.t

    val equations_on_outer_env_domain : t -> Name.Set.t

    val equations : t -> Flambda_types.t Name.Map.t

    val cse : t -> Simple.t Flambda_primitive.With_fixed_value.Map.t
  end = struct
    type t = {
      (* When used for [Typing_env_extension], the [defined_names] are those
         that are existentially bound. *)
      defined_names : Flambda_kind.t Name.Map.t;
      equations : Flambda_types.t Name.Map.t;
      cse : Simple.t Flambda_primitive.With_fixed_value.Map.t;
    }

    let print_with_cache ~cache ppf
          ({ defined_names = _; equations; cse; } : t) =
      let print_equations ppf equations =
        let equations = Name.Map.bindings equations in
        match equations with
        | [] -> Format.pp_print_string ppf "()"
        | _::_ ->
          Format.pp_print_string ppf "(";
          Format.pp_print_list ~pp_sep:Format.pp_print_space
            (fun ppf (name, ty) ->
              Format.fprintf ppf
                "@[<hov 1>%s%a%s :@ %a@]"
                (Misc_color.bold_green ())
                Name.print name
                (Misc_color.reset ())
                (Type_printers.print_with_cache ~cache) ty)
            ppf equations;
          Format.pp_print_string ppf ")"
      in
      Format.fprintf ppf
        "@[<v 1>(\
            @[<hov 1>(equations@ @[<v 1>%a@])@]@;\
            @[<hov 1>(cse@ %a)@])@]"
        print_equations equations
        (Flambda_primitive.With_fixed_value.Map.print Simple.print) cse

    let print ppf t =
      print_with_cache ~cache:(Printing_cache.create ()) ppf t

    let invariant _t = ()

    let empty () =
      { defined_names = Name.Map.empty;
        equations = Name.Map.empty;
        cse = Flambda_primitive.With_fixed_value.Map.empty;
      }

    let is_empty { defined_names; equations; cse; } =
      Name.Map.is_empty defined_names
        && Name.Map.is_empty equations
        && Flambda_primitive.With_fixed_value.Map.is_empty cse

    let equal env result t1 t2 =
      let (>>=) = Type_equality_result.(>>=) in
      let env_left =
        Typing_env.add_or_meet_opened_env_extension
          (Type_equality_env.typing_env_left env)
          t1
      in
      let env_right =
        Typing_env.add_or_meet_opened_env_extension
          (Type_equality_env.typing_env_right env)
          t2
      in
      let env =
        Type_equality_env.replace_typing_environments env
          ~left:env_left ~right:env_right
      in
      let names_to_check =
        Name.Set.union (Typing_env_level.equations_on_outer_env level1)
          (Typing_env_level.equations_on_outer_env level2)
      in
      Name.Set.fold (fun name result ->
          result
          >>= fun result ->
          let ty1 = Typing_env.find env name env_left in
          let ty2 = Typing_env.find env name env_right in
          Type_equality.equal_with_env env result ty1 ty2)
        names_to_check
        result
      >>= fun result ->
      let cse1 = Typing_env_level.cse level1 in
      let cse2 = Typing_env_level.cse level2 in
      if Flambda_primitive.With_fixed_value.Map.equal Simple.equal cse1 cse2
      then result
      else Type_equality_result.types_known_unequal ()

    let apply_name_permutation ({ defined_names; equations; cse; } as t)
          perm =
      let defined_names_changed = ref false in
      let defined_names' =
        Name.Map.fold (fun name kind defined_names ->
            let name' = Name_permutation.apply_name perm name in
            if not (name == name') then begin
              defined_names_changed := true
            end;
            Name.Map.add name' kind defined_names)
          defined_names
          Name.Map.empty
      in
      let equations_changed = ref false in
      let equations' =
        Name.Map.fold (fun name ty equations ->
            let name' = Name_permutation.apply_name perm name in
            let ty' =
              Flambda_type0_core.apply_name_permutation ty perm
            in
            if not (name == name' && ty == ty') then begin
              equations_changed := true
            end;
            Name.Map.add name' ty' equations)
          equations
          Name.Map.empty
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
      if (not !defined_names_changed)
        && (not !equations_changed)
        && (not !cse_changed)
      then t
      else 
        { defined_names = defined_names';
          equations = equations';
          cse = cse';
        }

    let free_names_in_defined_names t =
      Name_occurrences.create_from_name_set_in_types
        (Name.Map.keys t.defined_names)

    let free_names_in_equations_and_cse
          { defined_names = _; equations; cse; } =
      let free_names_equations =
        Name.Map.fold (fun name ty free_names ->
            Name_occurrences.add (Type_free_names.free_names ty)
              (Bindable_name.Name name) In_types)
          equations
          (Name_occurrences.create ())
      in
      Flambda_primitive.With_fixed_value.Map.fold
        (fun prim (simple : Simple.t) acc ->
          match simple with
          | Const _ | Discriminant _ -> acc
          | Name name ->
            Name_occurrences.add
              (Flambda_primitive.With_fixed_value.free_names prim)
              (Bindable_name.Name name) In_types)
        cse
        free_names_equations

    let free_names t =
      Name_occurrences.union (free_names_in_defined_names t)
        (free_names_in_equations_and_cse t)

    let free_names_minus_defined_names t =
      Name_occurrences.union (free_names_in_equations_and_cse t)
        (free_names_in_defined_names t)

    let restrict_to_names { defined_names; equations; cse; } allowed_names =
      let allowed_names =
        Name_occurrences.everything_must_only_be_names allowed_names
      in
      let defined_names =
        Name.Map.filter (fun name _kind -> Name.Set.mem name allowed_names)
          defined_names
      in
      let equations =
        Name.Map.filter (fun name _ty -> Name.Set.mem name allowed_names)
          equations
      in
      let cse =
        Flambda_primitive.With_fixed_value.Map.filter
          (fun _prim (simple : Simple.t) ->
            match simple with
            | Name name -> Name.Set.mem name allowed_names
            | Const _ | Discriminant _ -> true)
          cse
      in
      let t =
        { defined_names;
          equations;
          cse;
        }
      in
      invariant t;
      t

    let find_opt t name =
      match Name.Map.find name t.equations with
      | exception Not_found ->
        begin match Name.Map.find name t.defined_names with
        | exception Not_found -> None
        | kind -> Some (Flambda_type0_core.unknown kind)
        end
      | ty -> Some ty

(*
    let _tidy t =
      let free_names_without_defined_names' =
        free_names_without_defined_names t
      in
      let defined_names =
        Name_occurrences.create_from_set_in_types (defined_names' t)
      in
      (* CR mshinwell: We should do this "inlining" on [first_definitions]
         and [last_equations_rev], too. *)
      let at_or_after_cut_point =
        Scope_level.Map.map (fun by_name ->
            Name.Map.map_sharing
              (fun (entry : Typing_env.typing_env_entry) ->
                match entry with
                | CSE _ -> entry
                | Definition ty | Equation ty ->
                  let rec resolve_aliases ty
                        : Typing_env.typing_env_entry =
                    (* CR mshinwell: Needs check for cycles *)
                    match Flambda_type0_core.get_alias ty with
                    | None
                    | Some (Const _ | Discriminant _) ->
                      begin match entry with
                      | Definition _ -> Definition ty
                      | Equation _ -> Equation ty
                      | CSE _ -> assert false
                      end
                    | Some (Name alias) ->
                      let alias_is_defined_name =
                        Name_occurrences.mem defined_names (Name alias)
                      in
                      let alias_is_not_used_on_rhs =
                        Name_occurrences.mem free_names_without_defined_names'
                          (Name alias)
                      in
                      if alias_is_defined_name && alias_is_not_used_on_rhs
                      then resolve_aliases (find t alias)
                      else entry
                  in
                  resolve_aliases ty)
              by_name)
          t.at_or_after_cut_point
      in
      let t =
        { t with at_or_after_cut_point; }
      in
      (* CR mshinwell: We can probably avoid re-computing this by calculating
         it as we go along, just above. *)
      let free_names_without_defined_names =
        free_names_without_defined_names t
      in
      let unused_defined_names =
        Name_occurrences.diff defined_names free_names_without_defined_names
      in
      let allowed =
        Name_occurrences.diff free_names_without_defined_names
          unused_defined_names
      in
      restrict_to_names t allowed
*)

    let existentially_bind t name kind =
      if Name.Map.mem name t.defined_names then begin
        Misc.fatal_errorf "Typing environment extension already binds \
            name %a:@ %a"
          Name.print name
          print t
      end;
      { t with
        defined_names = Name.Map.add name kind t.defined_names
      }

    let add_or_replace_equation t name ty =
      { t with
        equations = Name.Map.add name ty t.equations;
      }

    let add_cse t name prim =
      let cse =
        match Flambda_primitive.With_fixed_value.Map.find prim t.cse with
        | exception Not_found ->
          Flambda_primitive.With_fixed_value.Map.add prim name t.cse
        | _name -> t.cse
      in
      { t with cse; }

    let update_cse_for_meet_or_join t _t1 _t2 _meet_or_join _names =
      t

(* XXX Uncomment once the rest is working again
      (* XXX This should follow aliases to the canonical name. *)
      let preserved_cse_equations t =
        (* CR-someday mshinwell: This could be improved to preserve some of
           those CSE equations that talk about existentially-bound names. *)
        Flambda_primitive.With_fixed_value.Map.filter
          (fun prim (bound_to_or_value : Simple.t) ->
            match bound_to_or_value with
            | Name name when not (Name.Set.mem name names) ->
              false
            | Name _ | Const _ | Discriminant _ ->
              let free_names_prim =
                Name_occurrences.everything_must_only_be_names
                  (Flambda_primitive.With_fixed_value.free_names prim)
              in
              Name.Set.subset free_names_prim names)
          t.cse
      in
      let cse =
        (* XXX This should be intersection for join and union for meet *)
        Flambda_primitive.With_fixed_value.Map.merge
          (fun _prim
              (simple1 : Simple.t option) (simple2 : Simple.t option) ->
            match simple1, simple2 with
            | None, None -> None
            | Some _, None -> simple1
            | None, Some _ -> simple2
            | Some simple1, Some simple2 ->
              (* For the moment just keep this very straightforward. *)
              (* CR-soon mshinwell: Make this take account of alias sets
                 properly. *)
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
*)

    let equations_domain t = Name.Map.keys t1.equations

    let equations_on_outer_env_domain t =
      Name.Set.remove (equations_domain t) t.defined_names

    let rec meet env (t1 : t) (t2 : t) : t =
      let defined_names =
        Name.Map.disjoint_union t1.defined_names t2.defined_names
      in
      let t =
        { (empty ()) with
          defined_names;
        }
      in
      let names_in_meet =
        Name.Set.union (equations_domain t1) (equations_domain t2)
      in
      let t =
        Name.Set.fold (fun name t ->
            assert (not (Name.Map.mem name t.equations));
            let ty1 = find_opt t1 name in
            let ty2 = find_opt t2 name in
            match ty1, ty2 with
            | None, None -> assert false
            | Some ty1, None -> add_or_replace_equation t name ty1
            | None, Some ty2 -> add_or_replace_equation t name ty2
            | Some ty1, Some ty2 ->
              let meet_ty, meet_equations =
                Both_meet_and_join.meet env ty1 ty2
              in
              A.pattern_match meet_equations.abst ~f:(fun _ t' ->
                let t = meet meet_env t t' in
                assert (not (Name.Map.mem name t.equations));
                add_or_replace_equation t name meet_ty))
          names_in_meet
          t
      in
      update_cse_for_meet_or_join t0 t1 t2 Meet names_in_meet

    let rec join env (t1 : t) (t2 : t) : t =
      let names_with_equations_in_join =
        Name.Set.inter (equations_on_outer_env_domain t1)
          (equations_on_outer_env_domain t2)
      in
      let t =
        Name.Set.fold (fun name t ->
            assert (not (Name.Map.mem name t.equations));
            let ty1 = Typing_env_level.find level_1 name in
            let ty2 = Typing_env_level.find level_2 name in
            let join_ty =
              Both_meet_and_join.join ~bound_name:name env ty1 ty2
            in
            add_or_replace_equation t name join_ty)
          names_in_join
          t
      in
      update_cse_for_meet_or_join t0 t1 t2 Join names_in_join

    let meet_equation ?env t name ty =
      let t' =
        { (empty ()) with
          equations = Name.Map.singleton name ty;
        }
      in
      let env =
        match env with
        | None ->
          Typing_env.create ~resolver:(fun name_or_export_id ->
            Misc.fatal_errorf "Cannot resolve external names in this \
                environment: %a"
              Export_id.print name_or_export_id)
        | Some env -> env
      in
      let meet_env =
        Meet_env.create env
          ~perm_left:(Name_permutation.create ())
          ~perm_right:(Name_permutation.create ())
      in
      meet env t t'

    let defined_names t =
      Name.Set.fold (fun name defined_names ->
          Bindable_name.Set.add (Name name) defined_names)
        (Name.Map.keys t.defined_names)
        Bindable_name.Set.empty

(*
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
                   (name, (entry : Typing_env.typing_env_entry))
                   acc ->
                match entry with
                | Definition ty -> f acc name (Definition_in_extension ty)
                | Equation ty -> f acc name (Equation ty)
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
*)
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
