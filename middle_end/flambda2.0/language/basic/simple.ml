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

module RIS = Rec_info_sequence
module RWC = Reg_width_const
module S0 = Simple0

type t = {
  simple : S0.t;
  rec_info : RIS.t;
}

let create simple =
  { simple;
    rec_info = RIS.empty;
  }

let name name = create (S0.name name)
let var var = create (S0.var var)
let vars vars = List.map var vars
let symbol sym = create (S0.symbol sym)
let const cst = create (S0.const cst)
let discriminant discr = create (S0.discriminant discr)

let const_bool b = const (if b then RWC.const_true else RWC.const_false)
let const_int i = const (RWC.const_int i)

let const_true = const RWC.const_true
let const_false = const RWC.const_false
let const_zero = const RWC.const_zero
let const_unit = const RWC.const_unit

let is_var t = S0.is_var t.simple
let is_symbol t = S0.is_symbol t.simple

let add_rec_info t ~newer_rec_info =
  { t with
    rec_info =
      RIS.add_newer_rec_info t.rec_info newer_rec_info;
  }

let rec_info t = t.rec_info

let without_rec_info t = create t.simple

let must_be_var t = S0.must_be_var t.simple
let allowed t ~allowed = S0.allowed t.simple ~allowed
let to_name t = S0.to_name t.simple

let map_name ({ simple; rec_info; } as t) ~f =
  let simple' = S0.map_name simple ~f in
  if simple == simple' then t
  else
    { simple = simple';
      rec_info;
    }

let map_var ({ simple; rec_info; } as t) ~f =
  let simple' = S0.map_var simple ~f in
  if simple == simple' then t
  else
    { simple = simple';
      rec_info;
    }

let map_symbol ({ simple; rec_info; } as t) ~f =
  let simple' = S0.map_symbol simple ~f in
  if simple == simple' then t
  else
    { simple = simple';
      rec_info;
    }

let free_names { simple; rec_info; } =
  Name_occurrences.union (S0.free_names simple)
    (RIS.free_names rec_info)

let free_names_in_types { simple; rec_info; } =
  Name_occurrences.union (S0.free_names_in_types simple)
    (RIS.free_names_in_types rec_info)

let apply_name_permutation ({ simple; rec_info; } as t) perm =
  let simple' = S0.apply_name_permutation simple perm in
  let rec_info' =
    RIS.apply_name_permutation rec_info perm
  in
  if simple == simple' && rec_info == rec_info'
  then t
  else
    { simple = simple';
      rec_info = rec_info';
    }

module T0 = Identifiable.Make (struct
  type nonrec t = t

  let compare
        { simple = simple1; rec_info = rec_info1; }
        { simple = simple2; rec_info = rec_info2; } =
    let c = S0.compare simple1 simple2 in
    if c <> 0 then c
    else RIS.compare rec_info1 rec_info2

  let equal t1 t2 = (compare t1 t2 = 0)

  let hash { simple; rec_info; } =
    Hashtbl.hash (S0.hash simple, RIS.hash rec_info)

  let print ppf { simple; rec_info; } =
    if RIS.is_empty rec_info then S0.print ppf simple
    else
      Format.fprintf ppf "@[%a@ %a@]"
        S0.print simple
        RIS.print rec_info

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
  | Const of RWC.t
  | Discriminant of Discriminant.t

let descr t : descr =
  match S0.descr t.simple with
  | Name name -> Name name
  | Const const -> Const const
  | Discriminant discr -> Discriminant discr

let of_descr (descr : descr) : t =
  match descr with
  | Name name' -> name name'
  | Const const' -> const const'
  | Discriminant discr' -> discriminant discr'
