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

module Naked_number_kind = struct
  type t =
    | Naked_immediate
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  let print ppf t =
    match t with
    | Naked_immediate -> Format.pp_print_string ppf "Naked_immediate"
    | Naked_float -> Format.pp_print_string ppf "Naked_float"
    | Naked_int32 -> Format.pp_print_string ppf "Naked_int32"
    | Naked_int64 -> Format.pp_print_string ppf "Naked_int64"
    | Naked_nativeint -> Format.pp_print_string ppf "Naked_nativeint"
end

module Phantom_kind = struct
  type t =
    | Value
    | Naked_number of Naked_number_kind.t
    | Fabricated

  let print ppf t =
    match t with
    | Value ->
      Format.fprintf ppf "Value"
    | Naked_number naked_number_kind ->
      Format.fprintf ppf "(Naked_number %a)"
        Naked_number_kind.print naked_number_kind
    | Fabricated ->
      Format.fprintf ppf "Fabricated"

  type occurrences =
    | In_types
    | Debug_only

  let print_occurrences ppf occs =
    match occs with
    | In_types -> Format.pp_print_string ppf "In_types"
    | Debug_only -> Format.pp_print_string ppf "Debug_only"
end

type t =
  | Value
  | Naked_number of Naked_number_kind.t
  | Fabricated
  | Phantom of Phantom_kind.occurrences * Phantom_kind.t

type kind = t

let value () = Value

let unit () = Value

(* CR mshinwell: can remove lambdas now *)
let naked_immediate () = Naked_number Naked_immediate

let naked_float () = Naked_number Naked_float

let naked_int32 () = Naked_number Naked_int32

let naked_int64 () = Naked_number Naked_int64

let naked_nativeint () = Naked_number Naked_nativeint

let fabricated () = Fabricated

let phantom occs phantom_kind = Phantom (occs, phantom_kind)

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 = Pervasives.compare t1 t2
  let equal t1 t2 = (compare t1 t2 = 0)

  let hash = Hashtbl.hash

  let print ppf t =
    match t with
    | Value ->
      Format.fprintf ppf "Value"
    | Naked_number naked_number_kind ->
      Format.fprintf ppf "(Naked_number %a)"
        Naked_number_kind.print naked_number_kind
    | Fabricated ->
      Format.fprintf ppf "Fabricated"
    | Phantom (occs, phantom_kind) ->
      Format.fprintf ppf "(Phantom %a %a)"
        Phantom_kind.print_occurrences occs
        Phantom_kind.print phantom_kind
end)

let compatible t ~if_used_at =
  equal t if_used_at

let compatible_allowing_phantom t ~if_used_at =
  let without_phantom : t =
    match if_used_at with
    | Value
    | Naked_number _
    | Fabricated -> t
    | Phantom (_, Value) -> Value
    | Phantom (_, Naked_number kind) -> Naked_number kind
    | Phantom (_, Fabricated) -> Fabricated
  in
  compatible t ~if_used_at:without_phantom

let is_value t =
  match t with
  | Value -> true
  | Naked_number _
  | Fabricated
  | Phantom _ -> false

let is_naked_float t =
  match t with
  | Naked_number Naked_float -> true
  | Value
  | Naked_number _
  | Fabricated
  | Phantom _ -> false

let is_phantom t =
  match t with
  | Value
  | Naked_number _
  | Fabricated -> false
  | Phantom _ -> true

let phantomize_in_types t : t =
  match t with
  | Value ->
    Phantom (In_types, Value)
  | Naked_number number_kind ->
    Phantom (In_types, Phantom_kind.Naked_number number_kind)
  | Fabricated ->
    Phantom (In_types, Fabricated)
  | Phantom (In_types, _) -> t
  | Phantom (Debug_only, _) ->
    Misc.fatal_errorf "Cannot phantomize kind %a to [In_types]"
      print t

let phantomize_debug_only t =
  match t with
  | Value ->
    Phantom (Debug_only, Value)
  | Naked_number number_kind ->
    Phantom (Debug_only, Phantom_kind.Naked_number number_kind)
  | Fabricated ->
    Phantom (Debug_only, Fabricated)
  | Phantom (Debug_only, _) -> t
  | Phantom (In_types, kind) -> Phantom (Debug_only, kind)

module Standard_int = struct
  type t =
    | Tagged_immediate
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  let to_kind t : kind =
    match t with
    | Tagged_immediate -> Value
    | Naked_int32 -> Naked_number Naked_int32
    | Naked_int64 -> Naked_number Naked_int64
    | Naked_nativeint -> Naked_number Naked_nativeint

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
    | Tagged_immediate -> Value
    | Naked_float -> Naked_number Naked_float
    | Naked_int32 -> Naked_number Naked_int32
    | Naked_int64 -> Naked_number Naked_int64
    | Naked_nativeint -> Naked_number Naked_nativeint

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
    | Naked_float -> Naked_number Naked_float
    | Naked_int32 -> Naked_number Naked_int32
    | Naked_int64 -> Naked_number Naked_int64
    | Naked_nativeint -> Naked_number Naked_nativeint

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

  let print (type a) ppf (t : a t) =
    match t with
    | Naked_immediate -> Format.pp_print_string ppf "Naked_immediate"
    | Naked_float -> Format.pp_print_string ppf "Naked_float"
    | Naked_int32 -> Format.pp_print_string ppf "Naked_int32"
    | Naked_int64 -> Format.pp_print_string ppf "Naked_int64"
    | Naked_nativeint -> Format.pp_print_string ppf "Naked_nativeint"
end
