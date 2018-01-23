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
    | Bottom

  (* [Unknown] is the top element.  [Definitely_pointer] and
     [Definitely_immediate] are incomparable.  [Bottom] is the least
     element. *)

  let join t0 t1 =
    match t0, t1 with
    | Unknown, _
    | _, Unknown
    | Definitely_pointer, Definitely_immediate
    | Definitely_immediate, Definitely_pointer -> Unknown
    | Definitely_pointer, Definitely_pointer -> Definitely_pointer
    | Definitely_immediate, Definitely_immediate -> Definitely_immediate
    | t0, Bottom -> t0
    | Bottom, t1 -> t1

  let meet t0 t1 =
    match t0, t1 with
    | Unknown, t
    | t, Unknown -> t
    | _, Bottom | Bottom, _
    | Definitely_pointer, Definitely_immediate
    | Definitely_immediate, Definitely_pointer -> Bottom
    | Definitely_pointer, Definitely_pointer -> Definitely_pointer
    | Definitely_immediate, Definitely_immediate -> Definitely_immediate

  let compatible t ~if_used_at =
    match t, if_used_at with
    | _, Unknown -> true
    | Unknown, _ -> false
    | Bottom, _ -> true
    | _, Bottom -> false
    | Definitely_pointer, Definitely_immediate
    | Definitely_immediate, Definitely_pointer -> false
    | Definitely_pointer, Definitely_pointer -> true
    | Definitely_immediate, Definitely_immediate -> true

  let print ppf t =
    match t with
    (* CR mshinwell: Change the name of the constructor *)
    | Unknown -> Format.fprintf ppf "Any"
    | Definitely_pointer -> Format.fprintf ppf "Definitely_pointer"
    | Definitely_immediate -> Format.fprintf ppf "Definitely_immediate"
    | Bottom -> Format.fprintf ppf "Bottom"

  let compare = Pervasives.compare
end

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

  let join_or_meet t1 t2 =
    match t1, t2 with
    | Naked_immediate, Naked_immediate
    | Naked_float, Naked_float
    | Naked_int32, Naked_int32
    | Naked_int64, Naked_int64
    | Naked_nativeint, Naked_nativeint -> t1
    | _ ->
      Misc.fatal_errorf "Naked_number_kind.join: kind error: %a vs. %a"
        print t1
        print t2
end

module Phantom_kind = struct
  type t =
    | Unknown
    | Value of Value_kind.t
    | Naked_number of Naked_number_kind.t
    | Fabricated of Value_kind.t
    | Bottom

  let print ppf t =
    match t with
    | Unknown ->
      Format.pp_print_string ppf "Unknown"
    | Value value_kind ->
      Format.fprintf ppf "(Value %a)" Value_kind.print value_kind
    | Naked_number naked_number_kind ->
      Format.fprintf ppf "(Naked_number %a)"
        Naked_number_kind.print naked_number_kind
    | Fabricated value_kind ->
      Format.fprintf ppf "(Fabricated %a)" Value_kind.print value_kind
    | Bottom ->
      Format.pp_print_string ppf "Bottom"

  (* CR mshinwell: We should review exactly where this is used *)
  let join t1 t2 =
    match t1, t2 with
    | Unknown, _ | _, Unknown -> Unknown
    | Bottom, t2 -> t2
    | t1, Bottom -> t1
    | Value k1, Value k2 -> Value (Value_kind.join k1 k2)
    | Naked_number k1, Naked_number k2 ->
      Naked_number (Naked_number_kind.join_or_meet k1 k2)
    | Fabricated k1, Fabricated k2 ->
      Fabricated (Value_kind.join k1 k2)
    | _, _ ->
      Misc.fatal_errorf "Phantom_kind.join: kind error: %a vs. %a"
        print t1
        print t2

  let meet t1 t2 =
    match t1, t2 with
    | Unknown, t2 -> t2
    | t1, Unknown -> t1
    | Bottom, _ | _, Bottom -> Bottom
    | Value k1, Value k2 -> Value (Value_kind.meet k1 k2)
    | Naked_number k1, Naked_number k2 ->
      Naked_number (Naked_number_kind.join_or_meet k1 k2)
    | Fabricated k1, Fabricated k2 -> Fabricated (Value_kind.meet k1 k2)
    | _, _ ->
      Misc.fatal_errorf "Phantom_kind.meet: kind error: %a vs. %a"
        print t1
        print t2
end

type t =
  | Value of Value_kind.t
  | Naked_number of Naked_number_kind.t
  | Fabricated of Value_kind.t
  | Phantom of Phantom_kind.t

type kind = t

let value value_kind = Value value_kind

let unit () = value Definitely_immediate

(* CR mshinwell: can remove lambdas now *)
let naked_immediate () = Naked_number Naked_immediate

let naked_float () = Naked_number Naked_float

let naked_int32 () = Naked_number Naked_int32

let naked_int64 () = Naked_number Naked_int64

let naked_nativeint () = Naked_number Naked_nativeint

let fabricated value_kind = Fabricated value_kind

let phantom phantom_kind = Phantom phantom_kind

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 = Pervasives.compare t1 t2
  let equal t1 t2 = (compare t1 t2 = 0)

  let hash = Hashtbl.hash

  let print ppf t =
    match t with
    | Value value_kind ->
      Format.fprintf ppf "(Value %a)" Value_kind.print value_kind
    | Naked_number naked_number_kind ->
      Format.fprintf ppf "(Naked_number %a)"
        Naked_number_kind.print naked_number_kind
    | Fabricated value_kind ->
      Format.fprintf ppf "(Fabricated %a)" Value_kind.print value_kind
    | Phantom phantom_kind ->
      Format.fprintf ppf "(Phantom %a)" Phantom_kind.print phantom_kind
end)

let compatible t ~if_used_at =
  match t, if_used_at with
  | Value value_kind, Value if_used_at ->
    Value_kind.compatible value_kind ~if_used_at
  | Fabricated value_kind, Fabricated if_used_at ->
    Value_kind.compatible value_kind ~if_used_at
  | _, _ -> equal t if_used_at

type coercion_result =
  | Always_ok
  | Needs_runtime_check
  | Always_wrong

let coerce ~actual_kind ~desired_kind : coercion_result =
  if compatible actual_kind ~if_used_at:desired_kind then
    Always_ok
  else if compatible desired_kind ~if_used_at:actual_kind then
    Needs_runtime_check
  else
    Always_wrong

let is_value t =
  match t with
  | Value _ -> true
  | Naked_number _
  | Fabricated _
  | Phantom _ -> false

let is_naked_float t =
  match t with
  | Naked_number Naked_float -> true
  | Value _
  | Naked_number _
  | Fabricated _
  | Phantom _ -> false

module Standard_int = struct
  type t =
    | Tagged_immediate
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  let to_kind t : kind =
    match t with
    | Tagged_immediate -> Value Definitely_immediate
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
    | Tagged_immediate -> Value Definitely_immediate
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
