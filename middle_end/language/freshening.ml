(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module NP = Name_permutation

type t = NP.t

let create () = NP.create ()

let freshen_continuation t k =
  let k' = Continuation.create () in
  k', NP.add_continuation t k k'

let apply_continuation t k = NP.apply_continuation t k

let freshen_kinded_parameter t p =
  let p' = Kinded_parameter.rename p in
  p', NP.add_kinded_parameter t p p'

let freshen_kinded_parameters t ps =
  let ps_rev, t =
    List.fold_left (fun (ps_rev, t) p ->
        let p', t = freshen_kinded_parameter t p in
        p' :: ps_rev, t)
      ([], t)
      ps
  in
  List.rev ps_rev, t

let apply_kinded_parameter t p = NP.apply_kinded_parameter t p

let apply_kinded_parameters t ps =
  List.map (fun p -> apply_kinded_parameter t p) ps

let freshen_mutable_variable t v =
  let v' = Mutable_variable.rename v in
  v', NP.add_mutable_variable t v v'

let apply_mutable_variable t v = NP.apply_mutable_variable t v

let freshen_name t n =
  let n' = Name.rename n in
  n', NP.add_name t n n'

let apply_name t n = NP.apply_name t n

let apply_simple t n = NP.apply_simple t n

let freshen_symbol t s =
  let s' = Symbol.rename s in
  s', NP.add_symbol t s s'

let apply_symbol t s = NP.apply_symbol t s

let freshen_variable t v =
  let v' = Variable.rename v in
  v', NP.add_variable t v v'

let apply_variable t v = NP.apply_variable t v

let name_permutation t = t
