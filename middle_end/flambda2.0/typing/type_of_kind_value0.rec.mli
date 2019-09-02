(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell type Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

type blocks_and_tagged_immediates = {
  immediates : Immediates.t Or_unknown.t;
  blocks : Blocks.t Or_unknown.t;
}

type inlinable_function_declaration = {
  function_decl : Term_language_function_declaration.t;
  rec_info : Rec_info.t;
}

type function_declaration =
  | Non_inlinable of {
      param_arity : Flambda_arity.t;
      result_arity : Flambda_arity.t;
      recursive : Recursive.t;
    }
  | Inlinable of inlinable_function_declaration

type closures_entry = {
  function_decls : function_declaration Or_unknown.t Closure_id.Map.t;
  closure_types : Types_by_closure_id.t;
  closure_var_types : Types_by_var_within_closure.t;
}

type closures = {
  by_closure_id : Closures_entry_by_set_of_closures_contents.t;
}

type t =
  | Blocks_and_tagged_immediates of blocks_and_tagged_immediates
  | Boxed_number of Type_of_kind_naked_number.t
  | Closures of closures
  | String of String_info.Set.t
  | Array of { length : Type_of_kind_value.t; }

val erase_aliases : t -> allowed:Variable.Set.t -> t