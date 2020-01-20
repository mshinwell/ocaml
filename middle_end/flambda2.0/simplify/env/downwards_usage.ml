(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare

(* The analysis proceeds in two steps:
   * accumulate the atomic <- relations
   * compute the least-fixed point

  The <- relation is represented by the type

     t Variable.Map.t

  if [Variable.Map.find (f, x) relation = Top] then (f, x) <- Top
  is in the relation.

  if [Variable.Map.find (f, x) relation = Implication s] and
  [Variable.Set.mem (g, y) s] then (f, x) <- (g, y) is in the
  relation.
*)

type entry =
  | Top
  | Implication of Variable.Set.t

type t = entry Variable.Map.t

let print_entry ppf entry =
  match entry with
  | Top -> Format.fprintf ppf "Top"
  | Implication args ->
    Format.fprintf ppf "@[<hov 1>(Implication@ %a)@]"
      Variable.Set.print args

let print ppf t = Variable.Map.print print_entry ppf t

let top t p = Variable.Map.add p Top t

let implies t ~from ~to_ =
  match Variable.Map.find to_ t with
  | Top -> t
  | Implication set ->
    Variable.Map.add to_ (Implication (Variable.Set.add from set)) t
  | exception Not_found ->
    Variable.Map.add to_ (Implication (Variable.Set.singleton from)) t

let transitive_closure t =
  let union s1 s2 =
    match s1, s2 with
    | Top, _ | _, Top -> Top
    | Implication s1, Implication s2 ->
      Implication (Variable.Set.union s1 s2)
  in
  let equal s1 s2 =
    match s1, s2 with
    | Top, Implication _ | Implication _, Top -> false
    | Top, Top -> true
    | Implication s1, Implication s2 -> Variable.Set.equal s1 s2
  in
  let update arg t =
    let original_set =
      match Variable.Map.find arg t with
      | exception Not_found -> Implication Variable.Set.empty
      | entry -> entry
    in
    match original_set with
    | Top -> t
    | Implication arguments ->
      let set =
        Variable.Set.fold (fun orig acc ->
            let set =
              match Variable.Map.find orig t with
              | exception Not_found -> Implication Variable.Set.empty
              | entry -> entry
            in
            union set acc)
          arguments original_set
      in
      Variable.Map.add arg set t
  in
  let once t = Variable.Map.fold (fun arg _ t -> update arg t) t t in
  let rec least_fixed_point t =
    let t' = once t in
    if Variable.Map.equal equal t t'
    then t
    else least_fixed_point t'
  in
  least_fixed_point t

let record_use_of_variable t ?var_being_defined var =
  match var_being_defined with
  | None -> top t var
  | Some var_being_defined -> implies t ~from:var_being_defined ~to:var

let record_uses_of_variables t ~var_being_defined vars =
  Variable.Set.fold (fun var t ->
      record_use_of_variable t ?var_being_defined var)
    vars
    t

let used_variables t =
  transitive_closure t
  |> Variable.Map.filter (fun entry ->
       match entry with
       | Top -> true
       | Implication _ -> false)
  |> Variable.Map.keys
