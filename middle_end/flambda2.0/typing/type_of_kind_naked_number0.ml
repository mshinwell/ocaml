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