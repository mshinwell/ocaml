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
module Definition = Flambda_static.Program_body.Definition
module Program_body = Flambda_static.Program_body

type t = {
  env : TE.t;
  types : T.t Symbol.Map.t;
  definition : Definition.t;
}

let print ppf { env = _ ; types; definition; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(types@ %a)@]@ \
      @[<hov 1>(definition@ %a)@]\
      )@]"
    (Symbol.Map.print T.print) types
    Definition.print definition

let create ?computation env types bound_symbols static_part =
  let being_defined = Bound_symbols.being_defined bound_symbols in
  if not (Symbol.Set.subset (Symbol.Map.keys types) being_defined) then begin
    Misc.fatal_errorf "[types]:@ %a@ does not cover [bound_symbols]:@ %a"
      (Symbol.Map.print T.print) types
      Bound_symbols.print bound_symbols
  end;
  let definition : Definition.t =
    { computation;
      static_structure = [S (bound_symbols, static_part)];
    }
  in
  { env;
    types;
    definition;
  }

let create_from_static_structure env types pieces =
  List.map
    (fun (Program_body.Static_structure.S (bound_symbols, static_part)) ->
      create env types bound_symbols static_part)
    pieces

let create_from_definition env types definition =
  { env;
    types;
    definition;
  }

let introduce { env = orig_typing_env; types; _ } typing_env =
  (* CR mshinwell: Can't we use [Simplify_static] to give the type of the
     definition? *)
  let typing_env_before = typing_env in
  let typing_env =
    Symbol.Map.fold (fun sym typ typing_env ->
        let sym = Name.symbol sym in
        if TE.mem typing_env sym then typing_env
        else
          let sym =
            Name_in_binding_pos.create sym Name_mode.normal
          in
          TE.add_definition typing_env sym (T.kind typ))
      types
      typing_env
  in
  Symbol.Map.fold (fun sym typ typing_env ->
      let sym = Name.symbol sym in
      if not (TE.mem typing_env_before sym) then begin
        let env_extension =
          T.make_suitable_for_environment typ orig_typing_env
            ~suitable_for:typing_env
            ~bind_to:sym
        in
        TE.add_env_extension typing_env ~env_extension
      end else begin
        typing_env
      end)
    types
    typing_env

let definition t = t.definition
