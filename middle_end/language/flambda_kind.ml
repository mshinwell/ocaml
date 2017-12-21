(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017 OCamlPro SAS                                          *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Value_kind = struct
  type t =
    | Unknown
    | Definitely_pointer
    | Definitely_immediate

  (* [Unknown] is the top element.  [Definitely_pointer] and
     [Definitely_immediate] are incomparable. *)

  let join t0 t1 =
    match t0, t1 with
    | Unknown, _
    | _, Unknown
    | Definitely_pointer, Definitely_immediate
    | Definitely_immediate, Definitely_pointer -> Unknown
    | Definitely_pointer, Definitely_pointer -> Definitely_pointer
    | Definitely_immediate, Definitely_immediate -> Definitely_immediate

  type 'a or_bottom = Ok of 'a | Bottom

  let meet t0 t1 : _ or_bottom =
    match t0, t1 with
    | Unknown, t
    | t, Unknown -> Ok t
    | Definitely_pointer, Definitely_immediate
    | Definitely_immediate, Definitely_pointer -> Bottom
    | Definitely_pointer, Definitely_pointer -> Ok Definitely_pointer
    | Definitely_immediate, Definitely_immediate -> Ok Definitely_immediate

  let compatible t ~if_used_at =
    match t, if_used_at with
    | _, Unknown -> true
    | Unknown, _ -> false
    | Definitely_pointer, Definitely_immediate
    | Definitely_immediate, Definitely_pointer -> false
    | Definitely_pointer, Definitely_pointer -> true
    | Definitely_immediate, Definitely_immediate -> true

  let print ppf t =
    match t with
    | Unknown -> Format.fprintf ppf "Unk"
    | Definitely_pointer -> Format.fprintf ppf "Ptr"
    | Definitely_immediate -> Format.fprintf ppf "Imm"

  let compare = Pervasives.compare
end

module Naked_number_kind = struct
  type t =
    | Naked_immediate
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint
end

type t =
  | Value of Value_kind.t
  | Naked_number of Naked_number_kind.t
  | Fabricated of Fabricated_kind.t
  | Phantom of Phantom_kind.t

type kind = t

let value value_kind = Value value_kind

let unit () = value Definitely_immediate

(* CR mshinwell: can remove lambdas now *)
let naked_immediate () = Naked_immediate

let naked_float () = Naked_float

let naked_int32 () = Naked_int32

let naked_int64 () = Naked_int64

let naked_nativeint () = Naked_nativeint

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 = Pervasives.compare t1 t2
  let equal t1 t2 = (compare t1 t2 = 0)

  let hash = Hashtbl.hash

  let print ppf t =
    match t with
    | Value value_kind ->
      Format.fprintf ppf "value(%a)" Value_kind.print value_kind
    | Naked_immediate -> Format.pp_print_string ppf "naked_immediate"
    | Naked_float -> Format.pp_print_string ppf "naked_float"
    | Naked_int32 -> Format.pp_print_string ppf "naked_int32"
    | Naked_int64 -> Format.pp_print_string ppf "naked_int64"
    | Naked_nativeint -> Format.pp_print_string ppf "naked_nativeint"
end)

let compatible t ~if_used_at =
  match t, if_used_at with
  | Value value_kind, Value if_used_at ->
    Value_kind.compatible value_kind ~if_used_at
  | _, _ -> equal t if_used_at

let is_value t =
  match t with
  | Value _ -> true
  | Naked_immediate
  | Naked_float
  | Naked_int32
  | Naked_int64
  | Naked_nativeint -> false

let is_naked_float t =
  match t with
  | Naked_float -> true
  | Value _
  | Naked_immediate
  | Naked_int32
  | Naked_int64
  | Naked_nativeint -> false

module Standard_int = struct
  type t =
    | Tagged_immediate
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  let to_kind t : kind =
    match t with
    | Tagged_immediate -> Value Definitely_immediate
    | Naked_int32 -> Naked_int32
    | Naked_int64 -> Naked_int64
    | Naked_nativeint -> Naked_nativeint

  include Identifiable.Make (struct
    type nonrec t = t

    let print ppf t =
      match t with
      | Tagged_immediate -> Format.pp_print_string ppf "Tagged_immediate"
      | Naked_int32 -> Format.pp_print_string ppf "Naked_int32"
      | Naked_int64 -> Format.pp_print_string ppf "Naked_int64"
      | Naked_nativeint -> Format.pp_print_string ppf "Naked_nativeint"

    let compare = Pervasives.compare
    let equal t1 t2 = (compare t1 t2 = 0)
    let hash = Hashtbl.hash
  end)

  let print_lowercase ppf t =
    match t with
    | Tagged_immediate -> Format.pp_print_string ppf "tagged_immediate"
    | Naked_int32 -> Format.pp_print_string ppf "naked_int32"
    | Naked_int64 -> Format.pp_print_string ppf "naked_int64"
    | Naked_nativeint -> Format.pp_print_string ppf "naked_nativeint"
end

module Standard_int_or_float = struct
  type t =
    | Tagged_immediate
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  let to_kind t : kind =
    match t with
    | Tagged_immediate -> Value Definitely_immediate
    | Naked_float -> Naked_float
    | Naked_int32 -> Naked_int32
    | Naked_int64 -> Naked_int64
    | Naked_nativeint -> Naked_nativeint

  include Identifiable.Make (struct
    type nonrec t = t

    let print ppf t =
      match t with
      | Tagged_immediate -> Format.pp_print_string ppf "Tagged_immediate"
      | Naked_float -> Format.pp_print_string ppf "Naked_float"
      | Naked_int32 -> Format.pp_print_string ppf "Naked_int32"
      | Naked_int64 -> Format.pp_print_string ppf "Naked_int64"
      | Naked_nativeint -> Format.pp_print_string ppf "Naked_nativeint"

    let compare = Pervasives.compare
    let equal t1 t2 = (compare t1 t2 = 0)
    let hash = Hashtbl.hash
  end)

  let print_lowercase ppf t =
    match t with
    | Tagged_immediate -> Format.pp_print_string ppf "tagged_immediate"
    | Naked_float -> Format.pp_print_string ppf "naked_float"
    | Naked_int32 -> Format.pp_print_string ppf "naked_int32"
    | Naked_int64 -> Format.pp_print_string ppf "naked_int64"
    | Naked_nativeint -> Format.pp_print_string ppf "naked_nativeint"
end

module Boxable_number = struct
  type t =
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  let to_kind t : kind =
    match t with
    | Naked_float -> Naked_float
    | Naked_int32 -> Naked_int32
    | Naked_int64 -> Naked_int64
    | Naked_nativeint -> Naked_nativeint

  include Identifiable.Make (struct
    type nonrec t = t

    let print ppf t =
      match t with
      | Naked_float -> Format.pp_print_string ppf "Naked_float"
      | Naked_int32 -> Format.pp_print_string ppf "Naked_int32"
      | Naked_int64 -> Format.pp_print_string ppf "Naked_int64"
      | Naked_nativeint -> Format.pp_print_string ppf "Naked_nativeint"

    let compare = Pervasives.compare
    let equal t1 t2 = (compare t1 t2 = 0)
    let hash = Hashtbl.hash
  end)

  let print_lowercase ppf t =
    match t with
    | Naked_float -> Format.pp_print_string ppf "naked_float"
    | Naked_int32 -> Format.pp_print_string ppf "naked_int32"
    | Naked_int64 -> Format.pp_print_string ppf "naked_int64"
    | Naked_nativeint -> Format.pp_print_string ppf "naked_nativeint"
end

module Naked_number = struct
  type 'values t =
    | Naked_immediate : Immediate.Set.t t
    | Naked_float : Numbers.Float_by_bit_pattern.Set.t t
    | Naked_int32 : Numbers.Int32.Set.t t
    | Naked_int64 : Numbers.Int64.Set.t t
    | Naked_nativeint : Targetint.Set.t t
end
