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
    env : TE.t;
    types : T.t Symbol.Map.t;
    bound_symbols : 'k Bound_symbols.t;
    static_part : 'k Static_part.t;
  } -> t

let print ppf (T { env = _ ; types; bound_symbols; static_part; }) =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(types@ %a)@]@ \
      @[<hov 1>(bound_symbols@ %a)@]@ \
      @[<hov 1>(static_part@ %a)@]\
      )@]"
    (Symbol.Map.print T.print) types
    Bound_symbols.print bound_symbols
    Static_part.print static_part

let create env types bound_symbols static_part =
  let being_defined = Bound_symbols.being_defined bound_symbols in
  if not (Symbol.Set.subset (Symbol.Map.keys types) being_defined) then begin
    Misc.fatal_errorf "[types]:@ %a@ does not cover [bound_symbols]:@ %a"
      (Symbol.Map.print T.print) types
      Bound_symbols.print bound_symbols
  end;
  T {
    env;
    types;
    bound_symbols;
    static_part;
  }

let create_from_static_structure env types
      ((S pieces) : Program_body.Static_structure.t) =
  List.map (fun (bound_symbols, static_part) ->
      create env types bound_symbols static_part)
    pieces

let introduce (T { env = orig_typing_env; types; _ }) typing_env =
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
      if not (TE.mem typing_env sym) then
        let var = Variable.create "lifted" in
        let kind = Flambda_kind.value in
        let typing_env =
          let name =
            Name_in_binding_pos.create (Name.var var)
              Name_mode.in_types
          in
          TE.add_definition typing_env name kind
        in
        let env_extension =
          T.make_suitable_for_environment typ orig_typing_env
            ~suitable_for:typing_env
            ~bind_to:var
        in
        let typing_env = TE.add_env_extension typing_env ~env_extension in
        let typ = T.alias_type_of kind (Simple.var var) in
        TE.add_equation typing_env sym typ
      else
        typing_env)
    types
    typing_env

let static_structure (T { bound_symbols; static_part; _ })
      : Program_body.Static_structure.t =
  S [bound_symbols, static_part]
