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
module DE = Simplify_env_and_result.Downwards_env
module T = Flambda_type
module TE = T.Typing_env

let simplify_simple dacc simple ~min_occurrence_kind : _ Or_bottom.t * _ =
  let typing_env = DE.typing_env (DA.denv dacc) in
  match
    TE.get_canonical_simple_with_kind typing_env simple ~min_occurrence_kind
  with
  | Bottom, kind -> Bottom, T.bottom kind
  | Ok (Some simple), kind -> Ok simple, T.alias_type_of kind simple
  | Ok None, _kind ->
    Misc.fatal_errorf "No canonical [Simple] for %a exists at the \
        requested occurrence kind (%a).  Downwards accumulator:@ %a"
      Simple.print simple
      Name_occurrence_kind.print min_occurrence_kind
      DA.print dacc

let simplify_simples dacc simples ~min_occurrence_kind =
  let typing_env = DE.typing_env (DA.denv dacc) in
  Or_bottom.all (List.map (fun simple : _ Or_bottom.t ->
      match
        TE.get_canonical_simple_with_kind typing_env simple
          ~min_occurrence_kind
      with
      | Bottom, _kind -> Bottom
      | Ok (Some simple), kind -> Ok (simple, T.alias_type_of kind simple)
      | Ok None, _kind ->
        Misc.fatal_errorf "No canonical [Simple] for %a exists at the \
            requested occurrence kind (%a).  Downwards accumulator:@ %a"
          Simple.print simple
          Name_occurrence_kind.print min_occurrence_kind
          DA.print dacc)
    simples)
