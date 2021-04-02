(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42-66"]

type t = {
  compilation_unit : Compilation_unit.t;
  linkage_name : Linkage_name.t;
  hash : int;
}

let linkage_name t = t.linkage_name

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    if t1 == t2 then 0
    else
      let c = compare t1.hash t2.hash in
      if c <> 0 then c
      else
        (* Linkage names are unique across a whole project, so just comparing
           those is sufficient. *)
        Linkage_name.compare t1.label t2.label

  let equal t1 t2 = compare t1 t2 = 0
  let output chan t = Linkage_name.output chan (label t)
  let hash { hash; } = hash
  let print ppf t = Linkage_name.print ppf (label t)
end)

let of_global_linkage compilation_unit label =
  let hash = Linkage_name.hash label in
  Linkage { compilation_unit; hash; label }

let for_ident id =
  let linkage_name = Linkage_name.for_ident id in
  let hash = Linkage_name.hash linkage_name in
  let compilation_unit =
    if Ident.is_global_or_predef id
    then Ident.compilation_unit_of_global_or_predef_ident id
    else Compilation_unit.get_current_exn ()
  in
  { compilation_unit;
    linkage_name;
    hash;
  }

let for_variable var =
  let linkage_name = Linkage_name.Flambda.for_variable var in
  { compilation_unit = Variable.get_compilation_unit var;
    linkage_name;
    hash = Linkage_name.hash linkage_name;
  }

let for_closure closure_id =
  let linkage_name = Linkage_name.Flambda.for_closure closure_id in
  { compilation_unit = Closure_id.get_compilation_unit closure_id;
    linkage_name;
    hash = Linkage_name.hash linkage_name;
  }

let for_code_of_closure closure_id =
  let linkage_name = Linkage_name.Flambda.for_code_of_closure closure_id in
  { compilation_unit = Closure_id.get_compilation_unit closure_id;
    linkage_name;
    hash = Linkage_name.hash linkage_name;
  }
