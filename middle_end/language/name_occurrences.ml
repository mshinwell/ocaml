(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type occurrence_kind =
  | In_terms
  | In_types
  | Debug_only

type t = {
  in_terms : Name.Set.t;
  in_types : Name.Set.t;
  in_debug_only : Name.Set.t;
}

let create () =
  { in_terms = Name.Set.empty;
    in_types = Name.Set.empty;
    in_debug_only = Name.Set.empty;
  }

let create_from_set_in_types in_types =
  { in_terms = Name.Set.empty;
    in_types;
    in_debug_only = Name.Set.empty;
  }

let add t name kind =
  match kind with
  | In_terms ->
    { t with in_terms = Name.Set.add name t.in_terms; }
  | In_types ->
    { t with in_types = Name.Set.add name t.in_types; }
  | Debug_only ->
    { t with in_debug_only = Name.Set.add name t.in_debug_only; }

let in_terms t = t.in_terms
let in_types t = t.in_types
let in_debug_only t = t.in_debug_only
