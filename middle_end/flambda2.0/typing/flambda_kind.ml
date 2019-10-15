(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017--2018 OCamlPro SAS                                    *)
(*   Copyright 2017--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

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

type t =
  | Value
  | Naked_number of Naked_number_kind.t
  | Fabricated

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

let unicode = true  (* CR mshinwell: move elsewhere *)

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 = Stdlib.compare t1 t2

  let equal t1 t2 = (compare t1 t2 = 0)

  let hash = Hashtbl.hash

  let print ppf t =
    let colour = Misc.Color.bold_green () in
    match t with
    | Value ->
      if unicode then
        Format.fprintf ppf "@<0>%s@<0>\u{1d54d}@<0>%s" colour
          (Misc.Color.reset ())
      else
        Format.fprintf ppf "Val"
    | Naked_number naked_number_kind ->
      if unicode then begin
        match naked_number_kind with
        | Naked_immediate ->
          Format.fprintf ppf "@<0>%s@<0>\u{2115}@<0>\u{1d55a}@<0>%s"
            colour (Misc.Color.reset ())
        | Naked_float ->
          Format.fprintf ppf "@<0>%s@<0>\u{2115}@<0>\u{1d557}@<0>%s"
            colour (Misc.Color.reset ())
        | Naked_int32 ->
          Format.fprintf ppf "@<0>%s@<0>\u{2115}@<0>\u{1d55d}@<0>%s"
            colour (Misc.Color.reset ())
        | Naked_int64 ->
          Format.fprintf ppf "@<0>%s@<0>\u{2115}@<0>\u{1d543}@<0>%s"
            colour (Misc.Color.reset ())
        | Naked_nativeint ->
          Format.fprintf ppf "@<0>%s@<0>\u{2115}@<0>\u{1d555}@<0>%s"
            colour (Misc.Color.reset ())
      end else begin
        Format.fprintf ppf "(Naked_number %a)"
          Naked_number_kind.print naked_number_kind
      end
    | Fabricated ->
      if unicode then
        Format.fprintf ppf "@<0>%s@<0>\u{1d53d}@<0>%s"
          colour (Misc.Color.reset ())
      else
        Format.fprintf ppf "Fab"

  let output chan t =
    print (Format.formatter_of_out_channel chan) t
end)

let compatible t ~if_used_at =
  equal t if_used_at

let is_value t =
  match t with
  | Value -> true
  | Naked_number _
  | Fabricated -> false

let is_naked_float t =
  match t with
  | Naked_number Naked_float -> true
  | Value
  | Naked_number _
  | Fabricated -> false

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

    let output chan t =
      print (Format.formatter_of_out_channel chan) t

    let compare = Stdlib.compare
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

    let output chan t =
      print (Format.formatter_of_out_channel chan) t

    let compare = Stdlib.compare
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

    let output chan t =
      print (Format.formatter_of_out_channel chan) t

    let compare = Stdlib.compare
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
