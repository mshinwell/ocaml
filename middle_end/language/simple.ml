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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Const = struct
  type t =
    | Untagged_immediate of Immediate.t
    | Tagged_immediate of Immediate.t
    | Naked_float of Numbers.Float_by_bit_pattern.t
    | Naked_int32 of Int32.t
    | Naked_int64 of Int64.t
    | Naked_nativeint of Targetint.t

  let const_true = Tagged_immediate (Immediate.bool_true)
  let const_false = Tagged_immediate (Immediate.bool_false)

  include Identifiable.Make (struct
    type nonrec t = t

    let compare t1 t2 =
      match t1, t2 with
      | Untagged_immediate i1, Untagged_immediate i2 ->
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
      | Untagged_immediate _, _ -> -1
      | _, Untagged_immediate _ -> 1
      | Tagged_immediate _, _ -> -1
      | _, Tagged_immediate _ -> 1
      | Naked_float _, _ -> -1
      | _, Naked_float _ -> 1
      | Naked_int32 _, _ -> -1
      | _, Naked_int32 _ -> 1
      | Naked_int64 _, _ -> -1
      | _, Naked_int64 _ -> 1

    let equal t1 t2 = (compare t1 t2 = 0)

    let hash t =
      match t with
      | Untagged_immediate n -> Immediate.hash n
      | Tagged_immediate n -> Immediate.hash n
      | Naked_float n -> Numbers.Float_by_bit_pattern.hash n
      | Naked_int32 n -> Hashtbl.hash n
      | Naked_int64 n -> Hashtbl.hash n
      | Naked_nativeint n -> Targetint.hash n

    let print ppf (t : t) =
      match t with
      | Untagged_immediate i -> Format.fprintf ppf "%a!" Immediate.print i
      | Tagged_immediate i -> Format.fprintf ppf "%a" Immediate.print i
      | Naked_float f ->
        Format.fprintf ppf "%a!" Numbers.Float_by_bit_pattern.print f
      | Naked_int32 n -> Format.fprintf ppf "%ld!" n
      | Naked_int64 n -> Format.fprintf ppf "%Ld!" n
      | Naked_nativeint n -> Format.fprintf ppf "%a!" Targetint.print n
  end)

  let kind t =
    let module K = Flambda_kind in
    match t with
    | Untagged_immediate _ -> K.naked_immediate ()
    | Tagged_immediate _ -> K.value ()
    | Naked_float _ -> K.naked_float ()
    | Naked_int32 _ -> K.naked_int32 ()
    | Naked_int64 _ -> K.naked_int64 ()
    | Naked_nativeint _ -> K.naked_nativeint ()
end

type t =
  | Name of Name.t
  | Const of Const.t

let name t = Name t
let var t = Name (Name.var t)
let symbol t = Name (Name.symbol t)
let const t = Const t

let const_bool b = Const (if b then Const.const_true else Const.const_false)

let const_true = Const Const.const_true
let const_false = Const Const.const_false

let free_names t =
  match t with
  | Name name -> Name.Set.singleton name
  | Const _ -> Name.Set.empty

let map_var t ~f =
  match t with
  | Name name ->
    let name' = Name.map_var name ~f in
    if name == name' then t
    else Name name'
  | Const _ -> t

let map_symbol t ~f =
  match t with
  | Name name ->
    let name' = Name.map_symbol name ~f in
    if name == name' then t
    else Name name'
  | Const _ -> t

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    match t1, t2 with
    | Name n1, Name n2 -> Name.compare n1 n2
    | Const c1, Const c2 -> Const.compare c1 c2
    | Name _, Const _ -> -1
    | Const _, Name _ -> 1

  let equal t1 t2 =
    compare t1 t2 = 0

  let hash t =
    match t with
    | Name name -> Hashtbl.hash (0, Name.hash name)
    | Const c -> Hashtbl.hash (1, Const.hash c)

  let print ppf t =
    match t with
    | Name name -> Name.print ppf name
    | Const c -> Const.print ppf c
end)

module List = struct
  type nonrec t = t list

  let free_names t =
    List.fold_left (fun free t ->
        Name.Set.union free (free_names t))
      Name.Set.empty
      t

  include Identifiable.Make (struct
    type nonrec t = t

    let equal t1 t2 =
      List.compare_lengths t1 t2 = 0
        && List.for_all2 equal t1 t2

    let compare t1 t2 =
      Misc.Stdlib.List.compare compare t1 t2

    let hash t =
      Hashtbl.hash (List.map hash t)

    let print ppf t =
      (Format.pp_print_list print ~pp_sep:Format.pp_print_space) ppf t
  end)
end

module With_kind = struct
  type nonrec t = t * Flambda_kind.t

  include Identifiable.Make (struct
    type nonrec t = t

    let equal (s1, k1) (s2, k2) =
      equal s1 s2 && Flambda_kind.equal k1 k2

    let compare (s1, k1) (s2, k2) =
      let c = compare s1 s2 in
      if c <> 0 then c
      else Flambda_kind.compare k1 k2

    let hash (s, k) =
      Hashtbl.hash (hash s, Flambda_kind.hash k)

    let print ppf (s, k) =
      Format.fprintf ppf "@[(%a :: %a)@]"
        print s
        Flambda_kind.print k
  end)
end
