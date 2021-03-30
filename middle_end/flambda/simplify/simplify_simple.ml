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

module DA = Downwards_acc
module T = Flambda_type
module TE = T.Typing_env

let simplify_simple dacc simple ~min_name_mode =
  let typing_env = DA.typing_env dacc in
  match TE.type_simple_in_term_exn typing_env simple ~min_name_mode with
  | exception Not_found ->
    Misc.fatal_errorf "No canonical [Simple] for %a exists at the@ \
        requested name mode (%a) or one greater.@ Downwards accumulator:@ %a"
      Simple.print simple
      Name_mode.print min_name_mode
      DA.print dacc
  | ty -> ty

type simplify_simples_result = {
  simples : Simple.t list;
  simple_tys : Flambda_type.t list;
}

let simplify_simples dacc simples =
  let typing_env = DA.typing_env dacc in
  let simple_tys =
    ListLabels.map simples ~f:(fun simple ->
      match
        TE.type_simple_in_term_exn typing_env simple
          ~min_name_mode:Name_mode.normal
      with
      | ty ->
        (* [ty] will always be an alias type; see the implementation of
           [TE.get_canonical_simple_in_term_exn]. *)
        ty
      | exception Not_found ->
        Misc.fatal_errorf "No canonical [Simple] for %a exists at the@ \
            requested name mode (Normal) or one greater.@ \
            Downwards accumulator:@ %a"
          Simple.print simple
          DA.print dacc)
  in
  { simples = ListLabels.map simple_tys ~f:T.get_alias_exn;
    simple_tys;
  }
