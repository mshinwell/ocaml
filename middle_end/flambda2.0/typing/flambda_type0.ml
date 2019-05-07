(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
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

module Make (Term_language_function_declaration : Expr_intf.S) = struct
  (* rec binding here *)

  include Flambda_type0_core
  include Flambda_types

  let meet = Both_meet_and_join.meet
  let join = Both_meet_and_join.join

  let meet_skeleton env t ~skeleton ~result ~result_kind : _ Or_bottom.t =
    let env =
      Typing_env.add env result level (Definition (unknown result_kind))
    in
    let env =
      Meet_env.create env
        ~perm_left:(Name_permutation.create ())
        ~perm_right:(Name_permutation.create ())
    in
    let meet_ty, env_extension = meet env t skeleton in
    if is_obviously_bottom meet_ty then Bottom
    else Ok env_extension

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
end
