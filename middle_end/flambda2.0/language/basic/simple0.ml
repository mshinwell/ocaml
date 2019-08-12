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

[@@@ocaml.warning "+a-30-40-41-42"]

type t =
  | Var of Variable.t
  | Symbol of Symbol.t
  | Const of Reg_width_const.t
  | Discriminant of Discriminant.t

let name (name : Name.t) =
  match name with
  | Var var -> Var var
  | Symbol sym -> Symbol sym

let var var = Var var
let vars vars = List.map var vars
let symbol sym = Symbol sym
let const cst = Const cst
let discriminant t = Discriminant t

let is_var t =
  match t with
  | Var _ -> true
  | Symbol _ | Const _ | Discriminant _ -> false

let is_symbol t =
  match t with
  | Symbol _ -> true
  | Var _ | Const _ | Discriminant _ -> false

let must_be_var t =
  match t with
  | Var var -> Some var
  | Symbol _ | Const _ | Discriminant _ -> None

let allowed t ~allowed =
  match must_be_var t with
  | None -> true
  | Some var -> Variable.Set.mem var allowed

let to_name t =
  match t with
  | Var var -> Some (Name.var var)
  | Symbol sym -> Some (Name.symbol sym)
  | Const _ | Discriminant _ -> None

let map_name t ~f =
  match to_name t with
  | None -> t
  | Some name' -> name (f name')

let map_var t ~f =
  match t with
  | Var var ->
    let var' = f var in
    if var == var' then t
    else Var var'
  | Symbol _ | Const _ | Discriminant _ -> t

let map_symbol t ~f =
  match t with
  | Symbol sym ->
    let sym' = f sym in
    if sym == sym' then t
    else Symbol sym'
  | Var _ | Const _ | Discriminant _ -> t

let free_names t =
  match t with
  | Var var ->
    Name_occurrences.singleton_variable var Name_occurrence_kind.normal
  | Symbol sym ->
    Name_occurrences.singleton_symbol sym Name_occurrence_kind.normal
  | Const _ | Discriminant _ -> Name_occurrences.empty

let free_names_in_types t =
  match t with
  | Var var ->
    Name_occurrences.singleton_variable var Name_occurrence_kind.in_types
  | Symbol sym ->
    Name_occurrences.singleton_symbol sym Name_occurrence_kind.in_types
  | Const _ | Discriminant _ -> Name_occurrences.empty

let apply_name_permutation t perm =
  match t with
  | Var var ->
    let var' = Name_permutation.apply_variable perm var in
    if var == var' then t
    else Var var'
  | Symbol _ | Const _ | Discriminant _ -> t

module T0 = Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    match t1, t2 with
    | Var var1, Var var2 -> Variable.compare var1 var2
    | Symbol sym1, Symbol sym2 -> Symbol.compare sym1 sym2
    | Const c1, Const c2 -> Reg_width_const.compare c1 c2
    | Discriminant t1, Discriminant t2 -> Discriminant.compare t1 t2
    | Var _, _ -> -1
    | _, Var _ -> 1
    | Symbol _, _ -> -1
    | _, Symbol _ -> 1
    | Const _, _ -> -1
    | _, Const _ -> 1

  let equal t1 t2 = (compare t1 t2 = 0)

  let hash t =
    match t with
    | Var var -> Hashtbl.hash (0, Variable.hash var)
    | Symbol sym -> Hashtbl.hash (1, Symbol.hash sym)
    | Const c -> Hashtbl.hash (2, Reg_width_const.hash c)
    | Discriminant t -> Hashtbl.hash (3, Discriminant.hash t)

  let print ppf t =
    match t with
    | Var var -> Variable.print ppf var
    | Symbol sym -> Symbol.print ppf sym
    | Const c -> Reg_width_const.print ppf c
    | Discriminant t -> Discriminant.print ppf t

  let output chan t =
    print (Format.formatter_of_out_channel chan) t
end)

include T0

module List = struct
  type nonrec t = t list

  include Identifiable.Make (struct
    type nonrec t = t

    let compare t1 t2 =
      Misc.Stdlib.List.compare compare t1 t2

    let equal t1 t2 = (compare t1 t2 = 0)

    let hash t =
      Hashtbl.hash (List.map hash t)

    let print ppf t =
      (Format.pp_print_list print ~pp_sep:Format.pp_print_space) ppf t

    let output chan t =
      print (Format.formatter_of_out_channel chan) t
  end)

  let free_names t =
    List.fold_left (fun free t ->
        Name_occurrences.union free (free_names t))
      (Name_occurrences.empty)
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

module Pair = struct
  include Identifiable.Make_pair
    (struct type nonrec t = t include T0 end)
    (struct type nonrec t = t include T0 end)

  type nonrec t = t * t
end

module With_kind = struct
  type nonrec t = t * Flambda_kind.t

  include Identifiable.Make (struct
    type nonrec t = t

    let compare (s1, k1) (s2, k2) =
      let c = compare s1 s2 in
      if c <> 0 then c
      else Flambda_kind.compare k1 k2

    let equal t1 t2 = (compare t1 t2 = 0)

    let hash (s, k) =
      Hashtbl.hash (hash s, Flambda_kind.hash k)

    let print ppf (s, k) =
      Format.fprintf ppf "@[(%a@ \u{2237}@ %a)@]"
        print s
        Flambda_kind.print k

    let output chan t =
      print (Format.formatter_of_out_channel chan) t
  end)

  let free_names (simple, _kind) = free_names simple

  let apply_name_permutation ((simple, kind) as t) perm =
    let simple' = apply_name_permutation simple perm in
    if simple == simple' then t
    else simple', kind
end

type descr =
  | Name of Name.t
  | Const of Reg_width_const.t
  | Discriminant of Discriminant.t

let descr (t : t) : descr =
  match t with
  | Var var -> Name (Name.var var)
  | Symbol sym -> Name (Name.symbol sym)
  | Const const -> Const const
  | Discriminant discr -> Discriminant discr

let of_descr (descr : descr) : t =
  match descr with
  | Name (Var var) -> Var var
  | Name (Symbol sym) -> Symbol sym
  | Const const -> Const const
  | Discriminant discr -> Discriminant discr
