(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | Naked_immediate of Immediate.t
  | Tagged_immediate of Immediate.t
  | Naked_float of Numbers.Float_by_bit_pattern.t
  | Naked_int32 of Int32.t
  | Naked_int64 of Int64.t
  | Naked_nativeint of Targetint.t
  | Initial_rec_info

let const_true = Tagged_immediate (Immediate.bool_true)
let const_false = Tagged_immediate (Immediate.bool_false)

let const_int i = Tagged_immediate (Immediate.int i) 

let const_zero = const_false
let const_unit = const_zero

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    match t1, t2 with
    | Naked_immediate i1, Naked_immediate i2 ->
      Immediate.compare i1 i2
    | Tagged_immediate i1, Tagged_immediate i2 ->
      Immediate.compare i1 i2
    | Naked_float f1, Naked_float f2 ->
      Numbers.Float_by_bit_pattern.compare f1 f2
    | Naked_int32 n1, Naked_int32 n2 ->
      Int32.compare n1 n2
    | Naked_int64 n1, Naked_int64 n2 ->
      Int64.compare n1 n2
    | Naked_nativeint n1, Naked_nativeint n2 ->
      Targetint.compare n1 n2
    | Initial_rec_info, Initial_rec_info -> 0
    | Naked_immediate _, _ -> -1
    | _, Naked_immediate _ -> 1
    | Tagged_immediate _, _ -> -1
    | _, Tagged_immediate _ -> 1
    | Naked_float _, _ -> -1
    | _, Naked_float _ -> 1
    | Naked_int32 _, _ -> -1
    | _, Naked_int32 _ -> 1
    | Naked_int64 _, _ -> -1
    | _, Naked_int64 _ -> 1
    | Naked_nativeint _, _ -> -1
    | _, Naked_nativeint _ -> 1

  let equal t1 t2 = (compare t1 t2 = 0)

  let hash t =
    match t with
    | Naked_immediate n -> Hashtbl.hash (0, Immediate.hash n)
    | Tagged_immediate n -> Hashtbl.hash (1, Immediate.hash n)
    | Naked_float n -> Hashtbl.hash (2, Numbers.Float_by_bit_pattern.hash n)
    | Naked_int32 n -> Hashtbl.hash (3, n)
    | Naked_int64 n -> Hashtbl.hash (4, n)
    | Naked_nativeint n -> Hashtbl.hash (5, Targetint.hash n)
    | Initial_rec_info -> Hashtbl.hash 6

  let print ppf (t : t) =
    match t with
    | Naked_immediate i ->
      Format.fprintf ppf "@<0>%s#%a@<0>%s"
        (Flambda_colours.naked_number ())
        Immediate.print i
        (Flambda_colours.normal ())
    | Tagged_immediate i ->
      Format.fprintf ppf "@<0>%s%a@<0>%s"
        (Flambda_colours.tagged_immediate ())
        Immediate.print i
        (Flambda_colours.normal ())
    | Naked_float f ->
      Format.fprintf ppf "@<0>%s#%a@<0>%s"
        (Flambda_colours.naked_number ())
        Numbers.Float_by_bit_pattern.print f
        (Flambda_colours.normal ())
    | Naked_int32 n ->
      Format.fprintf ppf "@<0>%s#%ldl@<0>%s"
        (Flambda_colours.naked_number ())
        n
        (Flambda_colours.normal ())
    | Naked_int64 n ->
      Format.fprintf ppf "@<0>%s#%LdL@<0>%s"
        (Flambda_colours.naked_number ())
        n
        (Flambda_colours.normal ())
    | Naked_nativeint n ->
      Format.fprintf ppf "@<0>%s#%an@<0>%s"
        (Flambda_colours.naked_number ())
        Targetint.print n
        (Flambda_colours.normal ())
    | Initial_rec_info ->
      Format.fprintf ppf "<initial>"

  let output chan t =
    print (Format.formatter_of_out_channel chan) t
end)

let kind t =
  let module K = Flambda_kind in
  match t with
  | Naked_immediate _ -> K.naked_immediate
  | Tagged_immediate _ -> K.value
  | Naked_float _ -> K.naked_float
  | Naked_int32 _ -> K.naked_int32
  | Naked_int64 _ -> K.naked_int64
  | Naked_nativeint _ -> K.naked_nativeint
  | Initial_rec_info -> K.fabricated
