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
      (N.print N.Map.print) permutation

  let apply t n =
    match N.Map.find n t with
    | exception Not_found -> n
    | n -> n

  let add t n1 n2 =
    let n1 = apply t n1 in
    let n2 = apply t n2 in
    let permutation = N.Map.add n1 n2 (N.Map.add n2 n1 t.permutation) in
    { permutation; }
end

module Continuations = Make (Continuation)
module Kinded_parameters = Make (Kinded_parameter)
module Mutable_variables = Make (Mutable_variable)
module Names = Make (Name)
module Symbols = Make (Symbol)
module Trap_ids = Make (Trap_id)

type t = {
  continuations : Continuations.t;
  kinded_parameters : Kinded_parameters.t;
  mutable_variables : Mutable_variables.t;
  names : Names.t;
  symbols : Symbols.t;
  trap_ids : Trap_ids.t;
}

let create () =
  { continuations = Continuations.create ();
    kinded_parameters = Kinded_parameters.create ();
    mutable_variables = Mutable_variables.create ();
    names = Names.create ();
    symbols = Symbols.create ();
    trap_ids = Trap_ids.create ();
  }

let print ppf { continuations; kinded_parameters; mutable_variables;
      names; symbols; trap_ids; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(continuations@ %a)@]@ \
      @[<hov 1>(kinded_parameters@ %a)@]@ \
      @[<hov 1>(mutable_variables@ %a)@]@ \
      @[<hov 1>(names@ %a)@]@ \
      @[<hov 1>(symbols@ %a)@]@ \
      @[<hov 1>(trap_ids@ %a)@])@]"
    Continuations.print continuations
    Kinded_parameters.print kinded_parameters
    Mutable_variables.print mutable_variables
    Names.print names
    Symbols.print symbols
    Trap_ids.print trap_ids

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

let add_mutable_variable t v1 v2 =
  { t with
    mutable_variables = Mutable_variables.add t.mutable_variables v1 v2;
  }

let apply_mutable_variable t v =
  Mutable_variables.apply t.mutable_variables v

let add_name t n1 n2 =
  { t with
    names = Names.add t.names n1 n2;
  }

let apply_name t n =
  Names.apply t.names n

let add_symbols t s1 s2 =
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
