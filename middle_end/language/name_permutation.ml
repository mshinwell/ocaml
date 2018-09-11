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

let check_invariants = true

module Make (N : Map.With_set) = struct
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
      match N.Map.choose_opt second.forwards with
      | None -> first
      | Some (n1, n2) ->
        let first = post_swap first n1 n2 in
        let second = pre_swap second n1 n2 in
        compose ~second ~first
    in
    let t = compose ~second ~first in
    t
end

module Continuations = Make (Continuation)
module Names = Make (Name)

(* We don't use [Bindable_name.t]: this enables us to statically enforce that
   different varieties of names are not permuted with each other (e.g. a
   variable with a continuation). *)
type t = {
  continuations : Continuations.t;
  names : Names.t;
}

let create () =
  { continuations = Continuations.create ();
    names = Names.create ();
  }

let print ppf { continuations; names; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(continuations@ %a)@]@ \
      @[<hov 1>(names@ %a)@])@]"
    Continuations.print continuations
    Names.print names

let is_empty { continuations; names }  =
  Continuations.is_empty continuations
    && Names.is_empty names

let compose
      ~second:
        { continuations = continuations2;
          names = names2;
        }
      ~first:
        { continuations = continuations1;
          names = names1;
        } =
  { continuations =
      Continuations.compose ~second:continuations2 ~first:continuations1;
    names = Names.compose ~second:names2 ~first:names1;
  }

let add_continuation t k1 k2 =
  { t with
    continuations = Continuations.post_swap t.continuations k1 k2;
  }

let apply_continuation t k =
  Continuations.apply t.continuations k

let add_name t n1 n2 =
  { t with
    names = Names.post_swap t.names n1 n2;
  }

let add_bindable_name_exn t (bn1 : Bindable_name.t) (bn2 : Bindable_name.t) =
  match bn1, bn2 with
  | Continuation k1, Continuation k2 -> add_continuation t k1 k2
  | Name n1, Name n2 -> add_name t n1 n2
  | Continuation _, Name _
  | Name _, Continuation _ ->
    Misc.fatal_errorf "Cannot add bindable names of different forms to \
        name permutation: %a and %a"
      Bindable_name.print bn1
      Bindable_name.print bn2

let apply_name t n =
  Names.apply t.names n

let apply_name_set t names =
  Name.Set.fold (fun name result ->
      let name = apply_name t name in
      Name.Set.add name result)
    names
    Name.Set.empty

let apply_bindable_name t (bn : Bindable_name.t) : Bindable_name.t =
  match bn with
  | Continuation k -> Continuation (apply_continuation t k)
  | Name name -> Name (apply_name t name)

let apply_bindable_name_list t bns =
  List.map (fun bn -> apply_bindable_name t bn) bns

(* CR mshinwell: add phys-equal checks *)
let apply_bindable_name_set t names =
  Bindable_name.Set.fold (fun name result ->
      let name = apply_bindable_name t name in
      Bindable_name.Set.add name result)
    names
    Bindable_name.Set.empty
