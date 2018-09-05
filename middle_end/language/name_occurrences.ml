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
  in_terms : Bindable_name.Set.t;
  in_types : Bindable_name.Set.t;
  in_debug_only : Bindable_name.Set.t;
}

let print ppf t =
  Format.fprintf ppf "@[(in_terms %a)@ (in_types %a)@ (in_debug_only %a)@]"
    Bindable_name.Set.print t.in_terms
    Bindable_name.Set.print t.in_types
    Bindable_name.Set.print t.in_debug_only

let create () =
  { in_terms = Bindable_name.Set.empty;
    in_types = Bindable_name.Set.empty;
    in_debug_only = Bindable_name.Set.empty;
  }

let create_from_set_in_terms in_terms =
  { in_terms;
    in_types = Bindable_name.Set.empty;
    in_debug_only = Bindable_name.Set.empty;
  }

let create_from_name_set_in_terms in_terms =
  let in_terms =
    Name.Set.fold (fun name result ->
        Bindable_name.Set.add (Name name) result)
      in_terms
      Bindable_name.Set.empty
  in
  create_from_set_in_terms in_terms

let create_from_set_in_types in_types =
  { in_terms = Bindable_name.Set.empty;
    in_types;
    in_debug_only = Bindable_name.Set.empty;
  }

let singleton_in_terms name =
  create_from_set_in_terms (Bindable_name.Set.singleton name)

let singleton_in_types name =
  create_from_set_in_types (Bindable_name.Set.singleton name)

let of_list_in_terms names =
  create_from_set_in_terms (Bindable_name.Set.of_list names)

let add t name kind =
  match kind with
  | In_terms ->
    { t with in_terms = Bindable_name.Set.add name t.in_terms; }
  | In_types ->
    { t with in_types = Bindable_name.Set.add name t.in_types; }
  | Debug_only ->
    { t with in_debug_only = Bindable_name.Set.add name t.in_debug_only; }

let add_set t names kind =
  Bindable_name.Set.fold (fun name t -> add t name kind) names t

let in_terms t = t.in_terms
let in_types t = t.in_types
let in_debug_only t = t.in_debug_only

let mem_in_terms t name = Bindable_name.Set.mem name t.in_terms
let mem_in_types t name = Bindable_name.Set.mem name t.in_types
let mem_in_debug_only t name = Bindable_name.Set.mem name t.in_debug_only

let mem t name =
  mem_in_terms t name || mem_in_types t name || mem_in_debug_only t name

let everything t =
  Bindable_name.Set.union t.in_terms
    (Bindable_name.Set.union t.in_types t.in_debug_only)

let everything_must_only_be_names t =
  Bindable_name.Set.fold (fun (bindable : Bindable_name.t) result ->
      match bindable with
      | Name name -> Name.Set.add name result
      | _ -> Misc.fatal_errorf "Only [Name]s allowed: %a " print t)
    (everything t)
    Name.Set.empty

let remove t name =
  { in_terms = Bindable_name.Set.remove name t.in_terms;
    in_types = Bindable_name.Set.remove name t.in_types;
    in_debug_only = Bindable_name.Set.remove name t.in_debug_only;
  }

(* CR mshinwell: Rename to "diff_free_and_bound" or something?
   Also double-check the semantics are correct here *)
let diff t1 t2 =
  { in_terms = Bindable_name.Set.diff t1.in_terms t2.in_terms;
    in_types = Bindable_name.Set.diff t1.in_types
      (Bindable_name.Set.union t2.in_terms t2.in_types);
    in_debug_only = Bindable_name.Set.diff t1.in_debug_only
      (Bindable_name.Set.union t2.in_terms t2.in_debug_only);
  }

let union t1 t2 =
  { in_terms = Bindable_name.Set.union t1.in_terms t2.in_terms;
    in_types = Bindable_name.Set.union t1.in_types t2.in_types;
    in_debug_only = Bindable_name.Set.union t1.in_debug_only t2.in_debug_only;
  }

let rec union_list ts =
  match  ts with
  | [] -> create ()
  | t::ts -> union t (union_list ts)

let subset
      { in_terms = in_terms1; in_types = in_types1;
        in_debug_only = in_debug_only1; }
      { in_terms = in_terms2; in_types = in_types2;
        in_debug_only = in_debug_only2; } =
  Bindable_name.Set.subset in_terms1 in_terms2
    && Bindable_name.Set.subset in_types1 in_types2
    && Bindable_name.Set.subset in_debug_only1 in_debug_only2

let promote_to_in_types t =
  if not (Bindable_name.Set.is_empty t.in_debug_only) then begin
    Misc.fatal_errorf "Cannot promote set of names including one or more \
        marked as ``debug only'' to a set of names ``only occurring in \
        types''"
      print t
  end;
  let in_types = Bindable_name.Set.union t.in_terms t.in_types in
  { in_terms = Bindable_name.Set.empty;
    in_types;
    in_debug_only = Bindable_name.Set.empty;
  }

let promote_to_debug_only t =
  { in_terms = Bindable_name.Set.empty;
    in_types = Bindable_name.Set.empty;
    in_debug_only = everything t;
  }

let variables_only _t = Misc.fatal_error "NYI"
(*
  { in_terms = Bindable_name.variables_only t.in_terms;
    in_types = Bindable_name.variables_only t.in_types;
    in_debug_only = Bindable_name.variables_only t.in_debug_only;
  }
*)

let equal
      { in_terms = in_terms1; in_types = in_types1;
        in_debug_only = in_debug_only1; }
      { in_terms = in_terms2; in_types = in_types2;
        in_debug_only = in_debug_only2; } =
  Bindable_name.Set.equal in_terms1 in_terms2
    && Bindable_name.Set.equal in_types1 in_types2
    && Bindable_name.Set.equal in_debug_only1 in_debug_only2

let fold_everything t ~init ~f =
  let acc =
    Bindable_name.Set.fold (fun name acc -> f acc name)
      t.in_terms
      init
  in
  let acc =
    Bindable_name.Set.fold (fun name acc -> f acc name)
      t.in_types
      acc
  in
  Bindable_name.Set.fold (fun name acc -> f acc name)
    t.in_debug_only
    acc

let choose_and_remove_amongst_everything t =
  match Bindable_name.Set.get_singleton t.in_terms with
  | Some name ->
    let t =
      { t with
        in_terms = Bindable_name.Set.remove name t.in_terms;
      }
    in
    Some (name, t)
  | None ->
    match Bindable_name.Set.get_singleton t.in_types with
    | Some name ->
      let t =
        { t with
          in_types = Bindable_name.Set.remove name t.in_types;
        }
      in
      Some (name, t)
    | None ->
      match Bindable_name.Set.get_singleton t.in_debug_only with
      | Some name ->
        let t =
          { t with
            in_debug_only = Bindable_name.Set.remove name t.in_debug_only;
          }
        in
        Some (name, t)
      | None -> None

let apply_name_permutation { in_terms; in_types; in_debug_only; } perm =
  { in_terms = Name_permutation.apply_bindable_name_set perm in_terms;
    in_types = Name_permutation.apply_bindable_name_set perm in_types;
    in_debug_only = Name_permutation.apply_bindable_name_set perm in_debug_only;
  }
