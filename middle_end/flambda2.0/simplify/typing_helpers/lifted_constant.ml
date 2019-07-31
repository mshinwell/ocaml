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

(* CR mshinwell: Add [Flambda_static.Import] *)
module Bound_symbols = Flambda_static.Program_body.Bound_symbols
module Program_body = Flambda_static.Program_body
module Static_part = Flambda_static.Static_part

type t =
  | T : {
    types : T.t Symbol.Map.t;
    bound_symbols : 'k Bound_symbols.t;
    static_part : 'k Static_part.t;
  } -> t

let print ppf (T { types; bound_symbols; static_part; }) =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(types@ %a)@]@ \
      @[<hov 1>(bound_symbols@ %a)@]@ \
      @[<hov 1>(static_part@ %a)@]\
      )@]"
    (Symbol.Map.print T.print) types
    Bound_symbols.print bound_symbols
    Static_part.print static_part

let create types bound_symbols static_part =
  let being_defined = Bound_symbols.being_defined bound_symbols in
  if not (Symbol.Set.subset (Symbol.Map.keys types) being_defined) then begin
    Misc.fatal_errorf "[types]:@ %a@ does not cover [bound_symbols]:@ %a"
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
  T {
    types;
    bound_symbols;
    static_part;
  }

let create_from_static_structure types
      ((S pieces) : Program_body.Static_structure.t) =
  List.map (fun (bound_symbols, static_part) ->
      create types bound_symbols static_part)
    pieces

let introduce (T { types; _ }) typing_env =
  let orig_typing_env = typing_env in
  let allowed = TE.var_domain typing_env in
  let typing_env =
    Symbol.Map.fold (fun sym typ typing_env ->
        let sym = Name.symbol sym in
        if not (TE.mem typing_env sym) then
          let sym =
            Name_in_binding_pos.create sym Name_occurrence_kind.normal
          in
          TE.add_definition typing_env sym (T.kind typ)
        else
          typing_env)
      types
      typing_env
  in
  Symbol.Map.fold (fun sym typ typing_env ->
      let sym = Name.symbol sym in
      if not (TE.mem typing_env sym) then
        TE.add_equation typing_env sym
          (T.erase_aliases orig_typing_env ~bound_name:(Some sym)
            ~allowed typ)
      else
        typing_env)
    types
    typing_env

let static_structure (T { bound_symbols; static_part; _ })
      : Program_body.Static_structure.t =
  S [bound_symbols, static_part]
