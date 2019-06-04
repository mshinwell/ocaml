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

module T = Flambda_type
module TE = Flambda_type.Typing_env

module Bound_symbols = Flambda_static.Program_body.Bound_symbols

type t = {
  types : T.t Symbol.Map.t;
  bound_symbols : Bound_symbols.t;
  static_part : Flambda_static.Static_part.t;
}

let print ppf { types; bound_symbols; static_part; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(types@ %a)@]@ \
      @[<hov 1>(bound_symbols@ %a)@]@ \
      @[<hov 1>(static_part@ %a)@]\
      )@]"
    (Symbol.Map.print T.print) types
    Bound_symbols.print bound_symbols
    Flambda_static.Static_part.print static_part

let create types bound_symbols static_part =
  let being_defined = Bound_symbols.being_defined bound_symbols in
  if not (Symbol.Set.equal being_defined (Symbol.Map.keys types)) then begin
    Misc.fatal_errorf "[types]:@ %a@ does not match [bound_symbols]:@ %a"
      (Symbol.Map.print T.print) types
      Bound_symbols.print bound_symbols
  end;
(* Think about this in conjunction with the erasure below
  Symbol.Map.iter (fun _sym typ ->
      let free_names = T.free_names typ in
      if not (Name_occurrences.only_contains_symbols free_names) then begin
        Misc.fatal_errorf "The type of a lifted constant cannot contain \
            any free names that are not [Symbol]s:@ %a"
          (Symbol.Map.print T.print) types
      end)
    types;
*)
  { types;
    bound_symbols;
    static_part;
  }

let introduce t typing_env =
  let allowed = TE.var_domain typing_env in
  Symbol.Map.fold (fun sym typ typing_env ->
      let sym = Name.symbol sym in
      if not (TE.mem typing_env sym) then
        let typing_env = TE.add_definition typing_env sym (T.kind typ) in
        TE.add_equation typing_env sym (T.erase_aliases typ ~allowed)
      else
        typing_env)
    t.types
    typing_env

let static_structure t =
  [t.bound_symbols, t.static_part]
