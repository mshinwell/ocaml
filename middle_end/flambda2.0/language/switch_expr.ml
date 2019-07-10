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
  discriminant_kind : Discriminant.Kind.t;
  scrutinee : Simple.t;
  arms : Continuation.t Discriminant.Map.t;
}

let fprintf = Format.fprintf

let print_arms ppf arms =
  let spc = ref false in
  Discriminant.Map.iter (fun discriminant l ->
      if !spc then fprintf ppf "@ " else spc := true;
      fprintf ppf "@[<h>| %a ->@ @<0>%sgoto@<0>%s %a@]"
        Discriminant.print discriminant
        (Flambda_colours.expr_keyword ())
        (Flambda_colours.normal ())
        Continuation.print l)
    arms

let print ppf { discriminant_kind; scrutinee; arms; } =
  let discriminant_kind =
    Discriminant.Kind.to_lowercase_string discriminant_kind
  in
  fprintf ppf
    "@[<hov 1>(@<0>%sswitch_%s@<0>%s %a@ @[<v 0>%a@])@]"
    discriminant_kind
    (Flambda_colours.expr_keyword ())
    (Flambda_colours.normal ())
    Simple.print scrutinee
    print_arms arms

let print_with_cache ~cache:_ ppf t = print ppf t

let invariant env ({ discriminant_kind; scrutinee; arms; } as t) =
  let module E = Invariant_env in
  let unbound_continuation cont reason =
    Misc.fatal_errorf "Unbound continuation %a in %s: %a"
      Continuation.print cont
      reason
      print t
  in
  E.check_simple_is_bound_and_of_kind env scrutinee K.fabricated;
  assert (Discriminant.Map.cardinal arms >= 2);
  let check discr k =
    let discr_kind = Discriminant.kind discr in
    if not (Discriminant.Kind.equal discr_kind discriminant_kind) then begin
      Misc.fatal_errorf "[Switch] has arm(s) whose discriminant kind does \
          not match the discriminant kind of the whole [Switch]:@ %a"
        print t
    end;
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

let create discriminant_kind ~scrutinee ~arms =
  { discriminant_kind; scrutinee; arms; }

let iter t ~f = Discriminant.Map.iter f t.arms

let num_arms t = Discriminant.Map.cardinal t.arms

let discriminant_kind t = t.discriminant_kind
let scrutinee t = t.scrutinee
let arms t = t.arms

let free_names { discriminant_kind = _; scrutinee; arms; } =
  let free_names_in_arms =
    Discriminant.Map.fold (fun _discr k free_names ->
        Name_occurrences.add_continuation free_names k)
      arms
      (Name_occurrences.empty)
  in
  Name_occurrences.union (Simple.free_names scrutinee) free_names_in_arms

let apply_name_permutation ({ discriminant_kind; scrutinee; arms; } as t) perm =
  let scrutinee' = Simple.apply_name_permutation scrutinee perm in
  let arms' =
    Discriminant.Map.map_sharing (fun k ->
        Name_permutation.apply_continuation perm k)
      arms
  in
  if scrutinee == scrutinee' && arms == arms' then t
  else { discriminant_kind; scrutinee = scrutinee'; arms = arms'; }
