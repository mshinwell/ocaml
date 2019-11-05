(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2019 OCamlPro SAS                                    *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Continuations = Permutation.Make (Continuation)
module Variables = Permutation.Make (Variable)

type t = {
  continuations : Continuations.t;
  variables : Variables.t;
}

let empty =
  { continuations = Continuations.empty;
    variables = Variables.empty;
  }

let print ppf { continuations; variables; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(continuations@ %a)@]@ \
      @[<hov 1>(variables@ %a)@])\
      @]"
    Continuations.print continuations
    Variables.print variables

let is_empty { continuations; variables }  =
  Continuations.is_empty continuations
    && Variables.is_empty variables

let compose
      ~second:
        { continuations = continuations2;
          variables = variables2;
        }
      ~first:
        { continuations = continuations1;
          variables = variables1;
        } =
  { continuations =
      Continuations.compose ~second:continuations2 ~first:continuations1;
    variables = Variables.compose ~second:variables2 ~first:variables1;
  }

let add_variable t var1 var2 =
  { t with
    variables = Variables.compose_one ~first:t.variables var1 var2;
  }

let apply_variable t var =
  Variables.apply t.variables var

let apply_variable_set t vars =
  Variable.Set.fold (fun var result ->
      let var = apply_variable t var in
      Variable.Set.add var result)
    vars
    Variable.Set.empty

let apply_name t (name : Name.t) =
  match name with
  | Var var -> Name.var (apply_variable t var)
  | Symbol _ -> name

let add_continuation t k1 k2 =
  { t with
    continuations = Continuations.compose_one ~first:t.continuations k1 k2;
  }

let apply_continuation t k =
  Continuations.apply t.continuations k
