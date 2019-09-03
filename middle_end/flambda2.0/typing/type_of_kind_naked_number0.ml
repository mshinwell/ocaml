(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

type 'k t0 =
  | Immediate : Immediate.Set.t -> Flambda_kind.naked_immediate t0
  | Float : Float.Set.t -> Flambda_kind.naked_float t0
  | Int32 : Int32.Set.t -> Flambda_kind.naked_int32 t0
  | Int64 : Int64.Set.t -> Flambda_kind.naked_int64 t0
  | Nativeint : Targetint.Set.t -> Flambda_kind.naked_nativeint t0

type t = N : 'k t0 * 'k Flambda_kind.Naked_number.t -> t

let print0 (type n) ppf (n : n t0) =
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

let print ppf t =
  match t with
  | N (_kind, n) -> print0 ppf n

let apply_name_permutation t _perm = t

let free_names _t = Name_occurrences.empty

let erase_aliases t ~allowed:_ = t

let apply_rec_info t rec_info : _ Or_bottom.t =
  if Rec_info.is_initial rec_info then Ok t
  else Bottom

[@@@ocaml.warning "+a-4-30-40-41-42"]

module TEE = Typing_env_extension

module Make
  (E : Lattice_ops_intf.S
    with type meet_env := Meet_env.t
    with type typing_env := Typing_env.t
    with type typing_env_extension := Typing_env_extension.t) =
struct
  type of_kind_foo = K.naked_float Type_grammar.of_kind_naked_number

  let kind = K.naked_float
  let to_type ty : Type_grammar.t = Naked_number (ty, Naked_float)
  let force_to_kind = Basic_type_ops.force_to_kind_naked_float
  let print_ty = Type_printers.print_ty_naked_float_with_cache
  let apply_rec_info = Basic_type_ops.apply_rec_info_of_kind_naked_number

  let meet_or_join_of_kind_foo _meet_or_join_env ~meet_or_join_ty:_
        (of_kind1 : K.naked_float Type_grammar.of_kind_naked_number)
        (of_kind2 : K.naked_float Type_grammar.of_kind_naked_number)
        : (K.naked_float Type_grammar.of_kind_naked_number * TEE.t)
            Or_bottom_or_absorbing.t =
    match of_kind1, of_kind2 with
    | Float fs1, Float fs2 ->
      let fs = E.Float.Set.union_or_inter fs1 fs2 in
      if Float.Set.is_empty fs then Bottom
      else Ok (Float fs, TEE.empty ())
end
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make
  (E : Lattice_ops_intf.S
    with type meet_env := Meet_env.t
    with type typing_env := Typing_env.t
    with type typing_env_extension := Typing_env_extension.t) =
struct
  type of_kind_foo = K.naked_immediate Type_grammar.of_kind_naked_number

  let kind = K.naked_immediate
  let to_type ty : Type_grammar.t = Naked_number (ty, Naked_immediate)
  let force_to_kind = Basic_type_ops.force_to_kind_naked_immediate
  let print_ty = Type_printers.print_ty_naked_immediate_with_cache
  let apply_rec_info = Basic_type_ops.apply_rec_info_of_kind_naked_number

  let meet_or_join_of_kind_foo _meet_or_join_env ~meet_or_join_ty:_
        (of_kind1 : K.naked_immediate Type_grammar.of_kind_naked_number)
        (of_kind2 : K.naked_immediate Type_grammar.of_kind_naked_number)
        : (K.naked_immediate Type_grammar.of_kind_naked_number
            * Typing_env_extension.t) Or_bottom_or_absorbing.t =
    match of_kind1, of_kind2 with
    | Immediate fs1, Immediate fs2 ->
      let fs = E.Immediate.Set.union_or_inter fs1 fs2 in
      if Immediate.Set.is_empty fs then Bottom
      else Ok (Immediate fs, Typing_env_extension.empty ())
end
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module TEE = Typing_env_extension

module Make
  (E : Lattice_ops_intf.S
    with type meet_env := Meet_env.t
    with type typing_env := Typing_env.t
    with type typing_env_extension := Typing_env_extension.t) =
struct
  type of_kind_foo = K.naked_int32 Type_grammar.of_kind_naked_number

  let kind = K.naked_int32
  let to_type ty : Type_grammar.t = Naked_number (ty, Naked_int32)
  let force_to_kind = Basic_type_ops.force_to_kind_naked_int32
  let print_ty = Type_printers.print_ty_naked_int32_with_cache
  let apply_rec_info = Basic_type_ops.apply_rec_info_of_kind_naked_number

  let meet_or_join_of_kind_foo _meet_or_join_env ~meet_or_join_ty:_
        (of_kind1 : K.naked_int32 Type_grammar.of_kind_naked_number)
        (of_kind2 : K.naked_int32 Type_grammar.of_kind_naked_number)
        : (K.naked_int32 Type_grammar.of_kind_naked_number * TEE.t)
            Or_bottom_or_absorbing.t =
    match of_kind1, of_kind2 with
    | Int32 fs1, Int32 fs2 ->
      let fs = E.Int32.Set.union_or_inter fs1 fs2 in
      if Int32.Set.is_empty fs then Bottom
      else Ok (Int32 fs, TEE.empty ())
end
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module TEE = Typing_env_extension

module Make
  (E : Lattice_ops_intf.S
    with type meet_env := Meet_env.t
    with type typing_env := Typing_env.t
    with type typing_env_extension := Typing_env_extension.t) =
struct
  type of_kind_foo = K.naked_int64 Type_grammar.of_kind_naked_number

  let kind = K.naked_int64
  let to_type ty : Type_grammar.t = Naked_number (ty, Naked_int64)
  let force_to_kind = Basic_type_ops.force_to_kind_naked_int64
  let print_ty = Type_printers.print_ty_naked_int64_with_cache
  let apply_rec_info = Basic_type_ops.apply_rec_info_of_kind_naked_number

  let meet_or_join_of_kind_foo _meet_or_join_env ~meet_or_join_ty:_
        (of_kind1 : K.naked_int64 Type_grammar.of_kind_naked_number)
        (of_kind2 : K.naked_int64 Type_grammar.of_kind_naked_number)
        : (K.naked_int64 Type_grammar.of_kind_naked_number * TEE.t)
            Or_bottom_or_absorbing.t =
    match of_kind1, of_kind2 with
    | Int64 fs1, Int64 fs2 ->
      let fs = E.Int64.Set.union_or_inter fs1 fs2 in
      if Int64.Set.is_empty fs then Bottom
      else Ok (Int64 fs, TEE.empty ())
end
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module TEE = Typing_env_extension

module Make
  (E : Lattice_ops_intf.S
    with type meet_env := Meet_env.t
    with type typing_env := Typing_env.t
    with type typing_env_extension := Typing_env_extension.t) =
struct
  type of_kind_foo = K.naked_nativeint Type_grammar.of_kind_naked_number

  let kind = K.naked_nativeint
  let to_type ty : Type_grammar.t = Naked_number (ty, Naked_nativeint)
  let force_to_kind = Basic_type_ops.force_to_kind_naked_nativeint
  let print_ty = Type_printers.print_ty_naked_nativeint_with_cache
  let apply_rec_info = Basic_type_ops.apply_rec_info_of_kind_naked_number

  let meet_or_join_of_kind_foo _meet_or_join_env ~meet_or_join_ty:_
        (of_kind1 : K.naked_nativeint Type_grammar.of_kind_naked_number)
        (of_kind2 : K.naked_nativeint Type_grammar.of_kind_naked_number)
        : (K.naked_nativeint Type_grammar.of_kind_naked_number * TEE.t)
            Or_bottom_or_absorbing.t =
    match of_kind1, of_kind2 with
    | Nativeint fs1, Nativeint fs2 ->
      let fs = E.Targetint.Set.union_or_inter fs1 fs2 in
      if Targetint.Set.is_empty fs then Bottom
      else Ok (Nativeint fs, TEE.empty ())
end
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Module signature used for abbreviating .mli files that correspond to
    meet and join operations on naked numbers. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type S = sig
  type flambda_type
  type 'a ty
  type 'a of_kind_naked_number
  type typing_env
  type meet_env
  type typing_env_extension

  type naked_number_kind

  module Naked_number : sig
    type t
    module Set : Set.S with type elt = t
  end

  module Make
    (E : Lattice_ops_intf.S
      with type typing_env := typing_env
      with type meet_env := meet_env
      with type typing_env_extension := typing_env_extension) :
  sig
    include Meet_and_join_spec_intf.S
      with type flambda_type := flambda_type
      with type 'a ty := 'a ty
      with type meet_env := meet_env
      with type typing_env_extension := typing_env_extension
      with type of_kind_foo = naked_number_kind of_kind_naked_number
  end
end
