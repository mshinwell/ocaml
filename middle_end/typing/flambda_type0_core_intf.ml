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

module type S = sig
  module Blocks : sig type t end
  module Closure_elements : sig type t end
  module Closure_ids : sig type t end
  module Closures_entry_by_closure_id : sig type t end
  module Discriminants : sig type t end
  module Expr : sig type t end
  module Function_type : sig type t end
  module Immediates : sig type t end
  module Types_by_closure_id : sig type t end
  module Typing_env : sig type t end
  module Typing_env_extension : sig type t end

  include module type of struct include
    Flambda_types.S_impl
      (Blocks)
      (Closure_elements)
      (Closure_ids)
      (Closures_entry_by_closure_id)
      (Discriminants)
      (Expr)
      (Function_type)
      (Immediates)
      (Types_by_closure_id)
  end

  include Contains_names.S with type t := t

  module Closures_entry : sig
    type t = closures_entry

    val add_or_meet_equations
       : t
      -> Typing_env.t
      -> Typing_env_extension.t
      -> t

    include Contains_names.S with type t := t
  end

  module Set_of_closures_entry : sig
    type t = set_of_closures_entry

    val add_or_meet_equations
       : t
      -> Typing_env.t
      -> Typing_env_extension.t
      -> t

    include Contains_names.S with type t := t
  end

  val print : Format.formatter -> t -> unit

  val print_ty_value : Format.formatter -> ty_value -> unit
  val print_ty_naked_number : Format.formatter -> 'a ty_naked_number -> unit
  val print_ty_fabricated : Format.formatter -> ty_fabricated -> unit

  val kind : t -> Flambda_kind.t
  val get_alias : t -> Simple.t option

  val bottom : Flambda_kind.t -> t
  val unknown : Flambda_kind.t -> t

  val alias_type_of : Flambda_kind.t -> Simple.t -> t

  val free_names : t -> Name_occurrences.t
  val free_names_set : t -> Name.Set.t

  val force_to_kind_value : t -> of_kind_value ty
  val force_to_kind_naked_number
     : 'a Flambda_kind.Naked_number.t
    -> t
    -> 'a of_kind_naked_number ty
  val force_to_kind_naked_int32 : t -> Int32.Set.t ty_naked_number
  val force_to_kind_naked_int64 : t -> Int64.Set.t ty_naked_number
  val force_to_kind_naked_nativeint : t -> Targetint.Set.t ty_naked_number
  val force_to_kind_naked_float : t -> Float.Set.t ty_naked_number
  val force_to_kind_naked_immediate : t -> Immediate.Set.t ty_naked_number
  val force_to_kind_fabricated : t -> of_kind_fabricated ty

  val any_value_as_ty_value : unit -> ty_value
  val any_fabricated_as_ty_fabricated : unit -> ty_fabricated

  val bottom_as_ty_value : unit -> ty_value
  val bottom_as_ty_fabricated : unit -> ty_fabricated

  val ty_is_obviously_bottom : 'a ty -> bool
  val is_obviously_bottom : t -> bool
end
