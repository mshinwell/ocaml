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

type t = {
  function_decls : Function_declarations.t;
  closure_elements : Simple.t Var_within_closure.Map.t;
}

(* CR mshinwell: A sketch of code for the invariant check is on cps_types. *)
let invariant _env _t = ()

let create function_decls ~closure_elements =
  (* CR mshinwell: Make sure invariant checks are applied here, e.g. that
     the set of closures is indeed closed. *)
  { function_decls;
    closure_elements;
  }

let function_decls t = t.function_decls
let closure_elements t = t.closure_elements

let has_empty_environment t =
  Var_within_closure.Map.is_empty t.closure_elements

let environment_doesn't_mention_variables t =
  Var_within_closure.Map.for_all (fun _vwc (simple : Simple.t) ->
      match Simple.descr simple with
      | Name (Symbol _) -> true
      | _ -> false)
    t.closure_elements

let print_with_cache ~cache ppf
      { function_decls; 
        closure_elements;
      } =
  fprintf ppf "@[<hov 1>(%sset_of_closures%s@ \
      @[<hov 1>(function_decls@ %a)@]@ \
      @[<hov 1>(closure_elements@ %a)@]\
      )@]"
    (Flambda_colours.prim_constructive ())
    (Flambda_colours.normal ())
    (Function_declarations.print_with_cache ~cache) function_decls
    (Var_within_closure.Map.print Simple.print) closure_elements

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let free_names
      { function_decls;
        closure_elements;
      } =
  Name_occurrences.union_list [
    Function_declarations.free_names function_decls;
    Simple.List.free_names (Var_within_closure.Map.data closure_elements);
  ]

let apply_name_permutation
      ({ function_decls; 
         closure_elements;
       } as t) perm =
  let function_decls' =
    Function_declarations.apply_name_permutation function_decls perm
  in
  let closure_elements' =
    Var_within_closure.Map.map_sharing (fun simple ->
        Simple.apply_name_permutation simple perm)
      closure_elements
  in
  if function_decls == function_decls'
    && closure_elements == closure_elements'
  then t
  else
    { function_decls = function_decls';
      closure_elements = closure_elements';
    }
