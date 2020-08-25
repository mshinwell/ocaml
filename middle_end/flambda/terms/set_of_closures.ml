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

module Env_entry = struct
  type t =
    | Simple of Simple.t
    | Symbol_projection of Symbol_projection.t

  let simple simple = Simple simple
  let symbol_projection proj = Symbol_projection proj

  let print ppf t =
    match t with
    | Simple simple -> Simple.print ppf simple
    | Symbol_projection proj -> Symbol_projection.print ppf proj

  let apply_name_permutation t perm =
    match t with
    | Simple simple ->
      let simple' = Simple.apply_name_permutation simple perm in
      if simple == simple' then t
      else Simple simple'
    | Symbol_projection proj ->
      let proj' = Symbol_projection.apply_name_permutation proj perm in
      if proj == proj' then t
      else Symbol_projection proj'

  let free_names t =
    match t with
    | Simple simple -> Simple.free_names simple
    | Symbol_projection proj -> Symbol_projection.free_names proj

  let all_ids_for_export t =
    match t with
    | Simple simple -> Ids_for_export.from_simple simple
    | Symbol_projection proj -> Symbol_projection.all_ids_for_export proj

  let import import_map t =
    match t with
    | Simple simple ->
      Simple (Ids_for_export.Import_map.simple import_map simple)
    | Symbol_projection proj ->
      Symbol_projection (Symbol_projection.import import_map proj)

  let compare t1 t2 =
    match t1, t2 with
    | Simple simple1, Simple simple2 -> Simple.compare simple1 simple2
    | Symbol_projection proj1, Symbol_projection proj2 ->
      Symbol_projection.compare proj1 proj2
    | Simple _, Symbol_projection _ -> -1
    | Symbol_projection _, Simple _ -> 1
end

type t = {
  function_decls : Function_declarations.t;
  closure_elements : Env_entry.t Var_within_closure.Map.t;
}

let print_with_cache ~cache ppf
      { function_decls;
        closure_elements;
      } =
  Format.fprintf ppf "@[<hov 1>(%sset_of_closures%s@ \
      @[<hov 1>(function_decls@ %a)@]@ \
      @[<hov 1>(closure_elements@ %a)@]\
      )@]"
    (Flambda_colours.prim_constructive ())
    (Flambda_colours.normal ())
    (Function_declarations.print_with_cache ~cache) function_decls
    (Var_within_closure.Map.print Env_entry.print) closure_elements

include Identifiable.Make (struct
  type nonrec t = t

  let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let output _ _ = Misc.fatal_error "Not yet implemented"

  let hash _ = Misc.fatal_error "Not yet implemented"

  let compare
        { function_decls = function_decls1;
          closure_elements = closure_elements1;
        }
        { function_decls = function_decls2;
          closure_elements = closure_elements2;
        } =
    let c = Function_declarations.compare function_decls1 function_decls2 in
    if c <> 0 then c
    else
      Var_within_closure.Map.compare Env_entry.compare
        closure_elements1 closure_elements2

  let equal t1 t2 = (compare t1 t2 = 0)
end)

(* CR mshinwell: A sketch of code for the invariant check is on cps_types. *)
let invariant _env _t = ()

let empty =
  { function_decls = Function_declarations.empty;
    closure_elements = Var_within_closure.Map.empty;
  }

let is_empty { function_decls; closure_elements; } =
  Function_declarations.is_empty function_decls
    && Var_within_closure.Map.is_empty closure_elements

let create function_decls ~closure_elements =
  (* CR mshinwell: Make sure invariant checks are applied here, e.g. that
     the set of closures is indeed closed. *)
  { function_decls;
    closure_elements;
  }

let function_decls t = t.function_decls
let closure_elements t = t.closure_elements

let print_with_cache ~cache ppf
      { function_decls;
        closure_elements;
      } =
  if Var_within_closure.Map.is_empty closure_elements then
    Format.fprintf ppf "@[<hov 1>(%sset_of_closures%s@ \
        @[<hov 1>%a@]\
        )@]"
      (Flambda_colours.prim_constructive ())
      (Flambda_colours.normal ())
      (Function_declarations.print_with_cache ~cache) function_decls
  else
    Format.fprintf ppf "@[<hov 1>(%sset_of_closures%s@ \
        @[<hov 1>%a@]@ \
        @[<hov 1>(env@ %a)@]\
        )@]"
      (Flambda_colours.prim_constructive ())
      (Flambda_colours.normal ())
      (Function_declarations.print_with_cache ~cache) function_decls
      (Var_within_closure.Map.print Env_entry.print) closure_elements

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let free_names
      { function_decls;
        closure_elements;
      } =
  Var_within_closure.Map.fold (fun _ env_entry free_names ->
      Name_occurrences.union free_names (Env_entry.free_names env_entry))
    closure_elements
    (Function_declarations.free_names function_decls)

let apply_name_permutation
      ({ function_decls;
         closure_elements;
       } as t) perm =
  let function_decls' =
    Function_declarations.apply_name_permutation function_decls perm
  in
  let closure_elements' =
    Var_within_closure.Map.map_sharing (fun env_entry ->
        Env_entry.apply_name_permutation env_entry perm)
      closure_elements
  in
  if function_decls == function_decls'
    && closure_elements == closure_elements'
  then t
  else
    { function_decls = function_decls';
      closure_elements = closure_elements';
    }

let all_ids_for_export { function_decls; closure_elements; } =
  Var_within_closure.Map.fold (fun _closure_var env_entry ids ->
      Ids_for_export.union ids (Env_entry.all_ids_for_export env_entry))
    closure_elements
    (Function_declarations.all_ids_for_export function_decls)

let import import_map { function_decls; closure_elements; } =
  let function_decls =
    Function_declarations.import import_map function_decls
  in
  let closure_elements =
    Var_within_closure.Map.filter_map ~f:(fun var env_entry ->
        if Ids_for_export.Import_map.closure_var_is_used import_map var
        then Some (Env_entry.import import_map env_entry)
        else None)
      closure_elements
  in
  { function_decls; closure_elements; }
