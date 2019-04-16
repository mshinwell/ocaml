(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*            Mark Shinwell and Xavier Clerc, Jane Street Europe          *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   Copyright 2017--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Mutable or immutable *)

type mutable_or_immutable =
  | Immutable
  | Mutable

let print_mutable_or_immutable ppf mut =
  match mut with
  | Immutable -> Format.pp_print_string ppf "Immutable"
  | Mutable -> Format.pp_print_string ppf "Mutable"

let compare_mutable_or_immutable mut1 mut2 =
  match mut1, mut2 with
  | Immutable, Immutable
  | Mutable, Mutable -> 0
  | Immutable, Mutable -> -1
  | Mutable, Immutable -> 1


(* Effects *)

type t =
  | No_effects
  | Only_generative_effects of mutable_or_immutable
  | Arbitrary_effects

let print ppf eff =
  match eff with
  | No_effects ->
      Format.fprintf ppf "no effects"
  | Only_generative_effects mut ->
      Format.fprintf ppf "only generative effects %a"
        print_mutable_or_immutable mut
  | Arbitrary_effects ->
      Format.fprintf ppf "Arbitrary effects"

let compare eff1 eff2 =
  match eff1, eff2 with
  | No_effects, No_effects -> 0
  | No_effects, (Only_generative_effects _ | Arbitrary_effects) -> -1
  | Only_generative_effects mut1,
    Only_generative_effects mut2 ->
      compare_mutable_or_immutable mut1 mut2
  | Only_generative_effects _, No_effects -> 1
  | Only_generative_effects _, Arbitrary_effects -> -1
  | Arbitrary_effects, Arbitrary_effects -> 0
  | Arbitrary_effects, (No_effects | Only_generative_effects _) -> 1


