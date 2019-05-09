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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module K = Flambda_kind

type t = {
  scrutinee : Name.t;
  arms : Continuation.t Discriminant.Map.t;
}

let fprintf = Format.fprintf

let print_arms ppf arms =
  let spc = ref false in
  Discriminant.Map.iter (fun discriminant l ->
      if !spc then fprintf ppf "@ " else spc := true;
      fprintf ppf "@[<hv 1>| %a ->@ %sgoto%s %a@]"
        Discriminant.print discriminant
        (Misc.Color.bold_cyan ())
        (Misc.Color.reset ())
        Continuation.print l)
    arms

let print ppf { scrutinee; arms; } =
  fprintf ppf
    "@[<v 1>(%sswitch%s %a@ @[<v 0>%a@])@]"
    (Misc.Color.bold_cyan ())
    (Misc.Color.reset ())
    Name.print scrutinee
    print_arms arms

let print_with_cache ~cache:_ ppf t = print ppf t

let invariant env ({ scrutinee; arms; } as t) =
  let module E = Invariant_env in
  let unbound_continuation cont reason =
    Misc.fatal_errorf "Unbound continuation %a in %s: %a"
      Continuation.print cont
      reason
      print t
  in
  E.check_name_is_bound_and_of_kind env scrutinee K.fabricated;
  assert (Discriminant.Map.cardinal arms >= 2);
  let check discr k =
    ignore (discr : Discriminant.t);
    match E.find_continuation_opt env k with
    | None ->
      unbound_continuation k "[Switch] term"
    | Some (arity, kind (*, cont_stack *)) ->
(*
      let current_stack = E.current_continuation_stack env in
      E.Continuation_stack.unify k cont_stack current_stack;
*)
      begin match kind with
      | Normal -> ()
      | Exn_handler ->
        Misc.fatal_errorf "Continuation %a is an exception handler \
            but is used in this [Switch] as a normal continuation:@ %a"
          Continuation.print k
          print t
      end;
      if List.length arity <> 0 then begin
        Misc.fatal_errorf "Continuation %a is used in this [Switch] \
            and thus must have arity [], but has arity %a"
          Continuation.print k
          Flambda_arity.print arity
      end
  in
  Discriminant.Map.iter check arms

let create ~scrutinee ~arms = { scrutinee; arms; }

let iter t ~f = Discriminant.Map.iter f t.arms

let num_arms t = Discriminant.Map.cardinal t.arms

let scrutinee t = t.scrutinee
let arms t = t.arms

let free_names { scrutinee; arms; } =
  let free_names_in_arms =
    Discriminant.Map.fold (fun _discr k free_names ->
        Name_occurrences.add_continuation free_names k)
      arms
      (Name_occurrences.empty)
  in
  Name_occurrences.union_list [
    Name_occurrences.singleton_name_in_terms scrutinee;
    free_names_in_arms;
  ]

let apply_name_permutation ({ scrutinee; arms; } as t) perm =
  let scrutinee' = Name_permutation.apply_name perm scrutinee in
  let arms' =
    Discriminant.Map.map_sharing (fun k ->
        Name_permutation.apply_continuation perm k)
      arms
  in
  if scrutinee == scrutinee' && arms == arms' then t
  else { scrutinee = scrutinee'; arms = arms'; }

let update_arms t ~arms =
  if arms == t.arms then t
  else { t with arms; }
