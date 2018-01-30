(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type occurrence_kind =
  | In_terms
  | In_types
  | Debug_only

type t = {
  in_terms : Name.Set.t;
  in_types : Name.Set.t;
  in_debug_only : Name.Set.t;
}

let print ppf t =
  Format.fprintf ppf "@[(in_terms %a)@ (in_types %a)@ (in_debug_only %a)@]"
    Name.Set.print t.in_terms
    Name.Set.print t.in_types
    Name.Set.print t.in_debug_only

let create () =
  { in_terms = Name.Set.empty;
    in_types = Name.Set.empty;
    in_debug_only = Name.Set.empty;
  }

let create_from_set_in_terms in_terms =
  { in_terms;
    in_types = Name.Set.empty;
    in_debug_only = Name.Set.empty;
  }

let create_from_set_in_types in_types =
  { in_terms = Name.Set.empty;
    in_types;
    in_debug_only = Name.Set.empty;
  }

let add t name kind =
  match kind with
  | In_terms ->
    { t with in_terms = Name.Set.add name t.in_terms; }
  | In_types ->
    { t with in_types = Name.Set.add name t.in_types; }
  | Debug_only ->
    { t with in_debug_only = Name.Set.add name t.in_debug_only; }

let add_set t names kind =
  Name.Set.fold (fun name t -> add t name kind) names t

let in_terms t = t.in_terms
let in_types t = t.in_types
let in_debug_only t = t.in_debug_only

let mem_in_terms t name = Name.Set.mem name t.in_terms
let mem_in_types t name = Name.Set.mem name t.in_types
let mem_in_debug_only t name = Name.Set.mem name t.in_debug_only

let everything t =
  Name.Set.union t.in_terms (Name.Set.union t.in_types t.in_debug_only)

let diff t1 t2 =
  { in_terms = Name.Set.diff t1.in_terms t2.in_terms;
    in_types = Name.Set.diff t1.in_types t2.in_types;
    in_debug_only = Name.Set.diff t1.in_debug_only t2.in_debug_only;
  }

let union t1 t2 =
  { in_terms = Name.Set.union t1.in_terms t2.in_terms;
    in_types = Name.Set.union t1.in_types t2.in_types;
    in_debug_only = Name.Set.union t1.in_debug_only t2.in_debug_only;
  }

let subset
      { in_terms = in_terms1; in_types = in_types1;
        in_debug_only = in_debug_only1; }
      { in_terms = in_terms2; in_types = in_types2;
        in_debug_only = in_debug_only2; } =
  Name.Set.subset in_terms1 in_terms2
    && Name.Set.subset in_types1 in_types2
    && Name.Set.subset in_debug_only1 in_debug_only2

let promote_to_in_types t =
  if not (Name.Set.is_empty t.in_debug_only) then begin
    Misc.fatal_errorf "Cannot promote set of names including one or more \
        marked as ``debug only'' to a set of names ``only occurring in \
        types''"
      print t
  end;
  let in_types = Name.Set.union t.in_terms t.in_types in
  { in_terms = Name.Set.empty;
    in_types;
    in_debug_only = Name.Set.empty;
  }

let promote_to_debug_only t =
  { in_terms = Name.Set.empty;
    in_types = Name.Set.empty;
    in_debug_only = everything t;
  }

let variables_only t =
  { in_terms = Name.variables_only t.in_terms;
    in_types = Name.variables_only t.in_types;
    in_debug_only = Name.variables_only t.in_debug_only;
  }

let equal
      { in_terms = in_terms1; in_types = in_types1;
        in_debug_only = in_debug_only1; }
      { in_terms = in_terms2; in_types = in_types2;
        in_debug_only = in_debug_only2; } =
  Name.Set.equal in_terms1 in_terms2
    && Name.Set.equal in_types1 in_types2
    && Name.Set.equal in_debug_only1 in_debug_only2

let fold_everything t ~init ~f =
  let acc =
    Name.Set.fold (fun name acc -> f acc name)
      t.in_terms
      init
  in
  let acc =
    Name.Set.fold (fun name acc -> f acc name)
      t.in_types
      acc
  in
  Name.Set.fold (fun name acc -> f acc name)
    t.in_debug_only
    acc

let choose_and_remove_amongst_everything t =
  match Name.Set.get_singleton t.in_terms with
  | Some name ->
    let t =
      { t with
        in_terms = Name.Set.remove name t.in_terms;
      }
    in
    Some (name, t)
  | None ->
    match Name.Set.get_singleton t.in_types with
    | Some name ->
      let t =
        { t with
          in_types = Name.Set.remove name t.in_types;
        }
      in
      Some (name, t)
    | None ->
      match Name.Set.get_singleton t.in_debug_only with
      | Some name ->
        let t =
          { t with
            in_debug_only = Name.Set.remove name t.in_debug_only;
          }
        in
        Some (name, t)
      | None -> None
