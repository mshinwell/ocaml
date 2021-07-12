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
  code_id : Code_id.t;
}

let invariant _env _t = ()

let create ~code_id =
  { code_id;
  }

let print_with_cache ~cache:_ ppf
      { code_id;
      } =
  Format.fprintf ppf "%a" Code_id.print code_id

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let code_id t = t.code_id

let free_names
      { code_id;
      } =
  Name_occurrences.add_code_id Name_occurrences.empty code_id Name_mode.normal

let apply_renaming ({ code_id; } as t) perm =
  let code_id' = Renaming.apply_code_id perm code_id in
  if code_id == code_id' then t
  else { code_id = code_id'; }

let all_ids_for_export
      { code_id;
      } =
  Ids_for_export.add_code_id Ids_for_export.empty code_id

let update_code_id _t code_id = { code_id; }

(* CR mshinwell: In the "equal" case this should assert that all of the
   other things in [t1] and [t2] are equal *)
let compare t1 t2 =
  Code_id.compare t1.code_id t2.code_id

let equal t1 t2 = (compare t1 t2 = 0)
