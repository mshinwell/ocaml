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

module Make (N : Map.With_set) = struct
  type t = {
    permutation : N.t N.Map.t;
  }

  let create () =
    { permutation = N.Map.empty;
    }

  let print ppf { permutation; } =
    Format.fprintf ppf "@[((permutation %a))@]"
      (N.Map.print N.print) permutation

  let apply t n =
    match N.Map.find n t.permutation with
    | exception Not_found -> n
    | n -> n

  let add t n1 n2 =
    let n1' = apply t n1 in
    let n2' = apply t n2 in
    let permutation = N.Map.add n1' n2 (N.Map.add n2' n1 t.permutation) in
    { permutation; }

  let is_empty t =
    N.Map.is_empty t.permutation

  let compose ~second:t2 ~first:t1 =
    N.Map.fold (fun n1 n2 output ->
        add output n1 n2)
      t2.permutation
      t1
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
    continuations = Continuations.add t.continuations k1 k2;
  }

let apply_continuation t k =
  Continuations.apply t.continuations k

let add_name t n1 n2 =
  { t with
    names = Names.add t.names n1 n2;
  }

let apply_name t n =
  Names.apply t.names n

let apply_name_set t names =
  Name.Set.fold (fun name result ->
      let name = apply_name t name in
      Name.Set.add name result)
    names
    Name.Set.empty

(* CR mshinwell: add phys-equal checks *)
let apply_bindable_name_set t names =
  Bindable_name.Set.fold (fun (name : Bindable_name.t) result ->
      let name : Bindable_name.t =
        match name with
        | Continuation k -> Continuation (apply_continuation t k)
        | Name name -> Name (apply_name t name)
      in
      Bindable_name.Set.add name result)
    names
    Bindable_name.Set.empty
