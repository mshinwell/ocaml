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
  dbg : Debuginfo.t;
  is_tupled : bool;
}

let invariant _env _t = ()

let create ~code_id ~dbg ~is_tupled =
  { code_id;
    dbg;
    is_tupled;
  }

let print_with_cache ~cache:_ ppf
      { code_id;
        dbg;
        is_tupled;
      } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(code_id@ %a)@]@ \
      @[<hov 1>@<0>%s(dbg@ %a)@<0>%s@]@ \
      @[<hov 1>@<0>%s(is_tupled @ %b)@<0>%s@])@]"
    Code_id.print code_id
    (Flambda_colours.debuginfo ())
    Debuginfo.print_compact dbg
    (Flambda_colours.normal ())
    (if is_tupled
     then Flambda_colours.normal ()
     else Flambda_colours.elide ())
    is_tupled
    (Flambda_colours.normal ())

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let code_id t = t.code_id
let dbg t = t.dbg
let is_tupled t = t.is_tupled

let free_names
      { code_id;
        dbg = _;
        is_tupled = _;
      } =
  Name_occurrences.add_code_id Name_occurrences.empty code_id Name_mode.normal

let apply_renaming ({ code_id; dbg = _; is_tupled = _; } as t) perm =
  let code_id' = Renaming.apply_code_id perm code_id in
  if code_id == code_id' then t
  else { t with code_id = code_id'; }

let all_ids_for_export
      { code_id;
        dbg = _;
        is_tupled = _;
      } =
  Ids_for_export.add_code_id Ids_for_export.empty code_id

let update_code_id t code_id = { t with code_id; }

(* CR mshinwell: In the "equal" case this should assert that all of the
   other things in [t1] and [t2] are equal *)
let compare t1 t2 =
  Code_id.compare t1.code_id t2.code_id

let equal t1 t2 = (compare t1 t2 = 0)
