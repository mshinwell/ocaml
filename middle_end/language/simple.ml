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

  let const_int i = Tagged_immediate (Immediate.int i) 

  let const_zero = const_false
  let const_unit = const_zero

  include Hashtbl.Make_with_map (struct
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
  | Discriminant of Discriminant.t

let name t = Name t
let var t = Name (Name.var t)
let symbol t = Name (Name.symbol t)
let const t = Const t

let const_bool b = Const (if b then Const.const_true else Const.const_false)

let const_int i = Const (Const.const_int i)

let const_true = Const Const.const_true
let const_false = Const Const.const_false

let const_zero = Const Const.const_zero
let unit = Const Const.const_unit

let discriminant t = Discriminant t

let map_name t ~f =
  match t with
  | Name name ->
    let name' = f name in
    if name == name' then t
    else Name name'
  | Const _ | Discriminant _ -> t

let map_var t ~f =
  match t with
  | Name name ->
    let name' = Name.map_var name ~f in
    if name == name' then t
    else Name name'
  | Const _ | Discriminant _ -> t

let map_symbol t ~f =
  match t with
  | Name name ->
    let name' = Name.map_symbol name ~f in
    if name == name' then t
    else Name name'
  | Const _ | Discriminant _ -> t

let free_names t =
  match t with
  | Name name -> Name_occurrences.singleton_in_terms (Name name)
  | Const _ | Discriminant _ -> Name_occurrences.create ()

let apply_name_permutation t perm =
  match t with
  | Name name ->
    let name' = Name_permutation.apply_name perm name in
    if name == name' then t
    else Name name'
  | Const _ | Discriminant _ -> t

include Hashtbl.Make_with_map (struct
  type nonrec t = t

  let compare t1 t2 =
    match t1, t2 with
    | Name n1, Name n2 -> Name.compare n1 n2
    | Const c1, Const c2 -> Const.compare c1 c2
    | Discriminant t1, Discriminant t2 -> Discriminant.compare t1 t2
    | Name _, Const _ -> -1
    | Name _, Discriminant _ -> -1
    | Const _, Name _ -> 1
    | Const _, Discriminant _ -> -1
    | Discriminant _, Name _ -> 1
    | Discriminant _, Const _ -> 1

  let hash t =
    match t with
    | Name name -> Hashtbl.hash (0, Name.hash name)
    | Const c -> Hashtbl.hash (1, Const.hash c)
    | Discriminant t -> Hashtbl.hash (2, Discriminant.hash t)

  let print ppf t =
    match t with
    | Name name -> Name.print ppf name
    | Const c -> Const.print ppf c
    | Discriminant t -> Discriminant.print ppf t
end)

module List = struct
  type nonrec t = t list

  include Hashtbl.Make_with_map (struct
    type nonrec t = t

    let compare t1 t2 =
      Misc.Stdlib.List.compare compare t1 t2

    let hash t =
      Hashtbl.hash (List.map hash t)

    let print ppf t =
      (Format.pp_print_list print ~pp_sep:Format.pp_print_space) ppf t
  end)

  let free_names t =
    List.fold_left (fun free t ->
        Name_occurrences.union free (free_names t))
      (Name_occurrences.create ())
      t

  let apply_name_permutation t perm =
    let changed = ref false in
    let result =
      List.map (fun simple ->
          let simple' = apply_name_permutation simple perm in
          if not (simple == simple') then begin
            changed := true
          end;
          simple')
        t
    in
    if not !changed then t
    else result
end

module With_kind = struct
  type nonrec t = t * Flambda_kind.t

  include Hashtbl.Make_with_map (struct
    type nonrec t = t

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

  let free_names (simple, _kind) = free_names simple

  let apply_name_permutation ((simple, kind) as t) perm =
    let simple' = apply_name_permutation simple perm in
    if simple == simple' then t
    else simple', kind
end
