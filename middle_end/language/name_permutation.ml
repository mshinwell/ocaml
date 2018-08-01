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
    let n1 = apply t n1 in
    let n2 = apply t n2 in
    let permutation = N.Map.add n1 n2 (N.Map.add n2 n1 t.permutation) in
    { permutation; }

  let is_empty t =
    N.Map.is_empty t.permutation

  let compose t1 t2 =
    N.Map.fold (fun n1 n2 output ->
        add output n1 n2)
      t2.permutation
      t1
end

module Continuations = Make (Continuation)
module Kinded_parameters = Make (Kinded_parameter)
module Logical_variables = Make (Logical_variable)
module Names = Make (Name)
module Symbols = Make (Symbol)
module Trap_ids = Make (Trap_id)
module Variables = Make (Variable)

type t = {
  continuations : Continuations.t;
  kinded_parameters : Kinded_parameters.t;
  logical_variables : Logical_variables.t;
  names : Names.t;
  symbols : Symbols.t;
  trap_ids : Trap_ids.t;
  variables : Variables.t;
}

let create () =
  { continuations = Continuations.create ();
    kinded_parameters = Kinded_parameters.create ();
    logical_variables = Logical_variables.create ();
    names = Names.create ();
    symbols = Symbols.create ();
    trap_ids = Trap_ids.create ();
    variables = Variables.create ();
  }

let print ppf { continuations; kinded_parameters;
      names; symbols; trap_ids; variables; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(continuations@ %a)@]@ \
      @[<hov 1>(kinded_parameters@ %a)@]@ \
      @[<hov 1>(logical_variables@ %a)@]@ \
      @[<hov 1>(mutable_variables@ %a)@]@ \
      @[<hov 1>(names@ %a)@]@ \
      @[<hov 1>(symbols@ %a)@]@ \
      @[<hov 1>(trap_ids@ %a)@]@ \
      @[<hov 1>(variables@ %a)@])@]"
    Continuations.print continuations
    Kinded_parameters.print kinded_parameters
    Logical_variables.print logical_variables
    Names.print names
    Symbols.print symbols
    Trap_ids.print trap_ids
    Variables.print variables

let is_empty { continuations; kinded_parameters; logical_variables;
      mutable_variables; names; symbols; trap_ids; variables; } =
  Continuations.is_empty continuations
    && Kinded_parameters.is_empty kinded_parameters
    && Logical_variables.is_empty logical_variables
    && Names.is_empty names
    && Symbols.is_empty symbols
    && Trap_ids.is_empty trap_ids
    && Variables.is_empty variables

let compose
      { continuations = continuations1;
        kinded_parameters = kinded_parameters1;
        logical_variables = logical_variables1;
        mutable_variables = mutable_variables1;
        names = names1;
        symbols = symbols1;
        trap_ids = trap_ids1;
        variables = variables1;
      }
      { continuations = continuations2;
        kinded_parameters = kinded_parameters2;
        logical_variables = logical_variables2;
        mutable_variables = mutable_variables2;
        names = names2;
        symbols = symbols2;
        trap_ids = trap_ids2;
        variables = variables2;
      } =
  { continuations = Continuations.compose continuations1 continuations2;
    kinded_parameters =
      Kinded_parameters.compose kinded_parameters1 kinded_parameters2;
    logical_variables =
      logical_variables.compose logical_variables1 logical_variables2;
    names = Names.compose names1 names2;
    symbols = Symbols.compose symbols1 symbols2;
    trap_ids = Trap_ids.compose trap_ids1 trap_ids2;
    variables = Variables.compose variables1 variables2;
  }

let add_continuation t k1 k2 =
  { t with
    continuations = Continuations.add t.continuations k1 k2;
  }

let apply_continuation t k =
  Continuations.apply t.continuations k

let add_kinded_parameter t p1 p2 =
  { t with
    kinded_parameters = Kinded_parameters.add t.kinded_parameters p1 p2;
  }

let apply_kinded_parameter t p =
  Kinded_parameters.apply t.kinded_parameters p

let add_logical_variable t p1 p2 =
  { t with
    logical_variables = Logical_variables.add t.logical_variables p1 p2;
  }

let apply_logical_variable t p =
  Logical_variables.apply t.logical_variables p

let add_name t n1 n2 =
  { t with
    names = Names.add t.names n1 n2;
  }

let apply_name t n =
  Names.apply t.names n

let apply_simple t (s : Simple.t) =
  match s with
  | Name name ->
    let name' = apply_name t name in
    if name == name' then s
    else Simple.name name'
  | Const _ | Discriminant _ -> s

let apply_simples t ss =
  List.map (fun s -> apply_simple t s) ss

let add_symbol t s1 s2 =
  { t with
    symbols = Symbols.add t.symbols s1 s2;
  }

let apply_symbol t s =
  Symbols.apply t.symbols s

let add_variable t v1 v2 =
  { t with
    variables = Variables.add t.variables v1 v2;
  }

let apply_variable t v =
  Variables.apply t.variables v
