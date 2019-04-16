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
  funs : Function_declaration.t Closure_id.Map.t;
}

let invariant _env _t = ()

let create funs =
  { funs;
  }

let funs t = t.funs

let find ({ funs; } : t) closure_id =
  Closure_id.Map.find closure_id funs

let update _function_decls ~funs =
  { funs;
  }

let print_with_cache ~cache ppf { funs; } =
  fprintf ppf "@[<hov 1>(%a)@]"
    (Closure_id.Map.print (Function_declaration.print_with_cache ~cache)) funs

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let free_names { funs; } =
  Closure_id.Map.fold
    (fun _closure_id (func_decl : Function_declaration.t) syms ->
      Name_occurrences.union syms (Function_declaration.free_names func_decl))
    funs
    (Name_occurrences.empty)

let apply_name_permutation ({ funs; } as t) perm =
  let funs' =
    Closure_id.Map.map_sharing (fun func_decl ->
        Function_declaration.apply_name_permutation func_decl perm)
      funs
  in
  if funs == funs' then t
  else { funs = funs'; }
