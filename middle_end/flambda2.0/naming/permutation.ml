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

let check_invariants = false

module Make (N : Identifiable.S) = struct
  type t = {
    forwards : N.t N.Map.t;
    backwards : N.t N.Map.t;
  }

  let create () =
    { forwards = N.Map.empty;
      backwards = N.Map.empty;
    }

  let print ppf { forwards; backwards; } =
    Format.fprintf ppf "@[((forwards %a)@ (backwards %a))@]"
      (N.Map.print N.print) forwards
      (N.Map.print N.print) backwards

  let [@inline always] invariant { forwards; backwards; } =
    if check_invariants then begin
      let is_bijection map =
        let domain = N.Map.keys map in
        let range_list = N.Map.data map in
        let range = N.Set.of_list range_list in
        N.Set.equal domain range
      in
      assert (is_bijection forwards);
      assert (N.Map.cardinal forwards = N.Map.cardinal backwards);
      assert (N.Map.for_all (fun n1 n2 ->
          assert (N.compare n1 n2 <> 0);
          match N.Map.find n2 backwards with
          | exception Not_found -> false
          | n1' -> N.compare n1 n1' = 0)
        forwards)
    end

  let apply t n =
    match N.Map.find n t.forwards with
    | exception Not_found -> n
    | n -> n

  let apply_backwards t n =
    match N.Map.find n t.backwards with
    | exception Not_found -> n
    | n -> n

  let add_to_map n1 n2 map =
    if N.compare n1 n2 = 0 then N.Map.remove n1 map
    else N.Map.add n1 n2 map

  let flip t =
    { forwards = t.backwards;
      backwards = t.forwards;
    }

  let post_swap t n1 n2 =
    let n1' = apply_backwards t n1 in
    let n2' = apply_backwards t n2 in
    (* CR mshinwell: These next two lines are a major source of allocation *)
    let forwards = add_to_map n1' n2 (add_to_map n2' n1 t.forwards) in
    let backwards = add_to_map n2 n1' (add_to_map n1 n2' t.backwards) in
    let t = { forwards; backwards; } in
    invariant t;
    t

  let pre_swap t n1 n2 =
    flip (post_swap (flip t) n1 n2)

  let is_empty t =
    N.Map.is_empty t.forwards

  let compose ~second ~first =
    let rec compose ~second ~first =
      match N.Map.choose second.forwards with
      | exception Not_found -> first
      | n1, n2 ->
        let first = post_swap first n1 n2 in
        let second = pre_swap second n1 n2 in
        compose ~second ~first
    in
    let t = compose ~second ~first in
    t
end

module Continuations = Make (Continuation)
module Variables = Make (Variable)

type t = {
  continuations : Continuations.t;
  variables : Variables.t;
}

let empty =
  { continuations = Continuations.create ();
    variables = Variables.create ();
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
    variables = Variables.post_swap t.variables var1 var2;
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
    continuations = Continuations.post_swap t.continuations k1 k2;
  }

let apply_continuation t k =
  Continuations.apply t.continuations k
