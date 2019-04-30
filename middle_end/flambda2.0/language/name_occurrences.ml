(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2019 OCamlPro SAS                                          *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(*
  type occurrence_kind =
    | In_terms
    | In_types
    | Debug_only
*)

module For_one_variety_of_names (Bindable : Bindable.S) = struct

(*
  type t = {
    in_terms : int Bindable.Map.t;
    in_types : int Bindable.Map.t;
    in_debug_only : int Bindable.Map.t;
  }

*)

  (* The integers gives the counts of free occurrences. *)
  type t = int Bindable.Map.t

  let print ppf t =
    Bindable.Map.print Format.pp_print_int t

  let empty = Bindable.Map.empty

  let singleton bindable = Bindable.Map.singleton bindable

  let add t bindable =
    Bindable.Map.update bindable
      (function None -> 1 | Some count -> count + 1)
      t

(*
  let create_from_set_in_terms in_terms =
    { in_terms;
      in_types = Bindable.Map.empty;
      in_debug_only = Bindable.Map.empty;
    }

  let create_from_name_set_in_terms in_terms =
    let in_terms =
      Name.Set.fold (fun name result ->
          Bindable.Map.add (Name name) result)
        in_terms
        Bindable.Map.empty
    in
    create_from_set_in_terms in_terms

  let create_from_set_in_types in_types =
    { in_terms = Bindable.Map.empty;
      in_types;
      in_debug_only = Bindable.Map.empty;
    }

  let create_from_name_set_in_types in_types =
    let in_types =
      Name.Set.fold (fun name result ->
          Bindable.Map.add (Name name) result)
        in_types
        Bindable.Map.empty
    in
    create_from_set_in_types in_types

  let is_empty { in_terms; in_types; in_debug_only; } =
    Bindable.Map.is_empty in_terms
      && Bindable.Map.is_empty in_types
      && Bindable.Map.is_empty in_debug_only
*)
(*
  let singleton_in_terms name =
    create_from_set_in_terms (Bindable.Map.singleton name)
*)

(*
  let singleton_in_types name =
    create_from_set_in_types (Bindable.Map.singleton name)

  let of_list_in_terms names =
    create_from_set_in_terms (Bindable.Map.of_list names)


  let add { in_terms; in_types; debug_only; } bindable kind =
    match kind with
    | In_terms ->
      { t with in_terms = add0 bindable in_terms; }
    | In_types ->
      { t with in_types = add0 bindable in_types; }
    | Debug_only ->
      { t with in_debug_only = add0 bindable in_debug_only; }

  let add_set t names kind =
    Bindable.Map.fold (fun name t -> add t name kind) names t

  let in_terms t = t.in_terms
  let in_types t = t.in_types
  let in_debug_only t = t.in_debug_only

  let mem_in_terms t name = Bindable.Map.mem name t.in_terms
  let mem_in_types t name = Bindable.Map.mem name t.in_types
  let mem_in_debug_only t name = Bindable.Map.mem name t.in_debug_only

  let mem t name =
    mem_in_terms t name || mem_in_types t name || mem_in_debug_only t name

  let mem_var t var =
    mem t (Bindable.Name (Name.var var))

  let everything t =
    Bindable.Map.union t.in_terms
      (Bindable.Map.union t.in_types t.in_debug_only)

  let everything_must_only_be_names t =
    Bindable.Map.fold (fun (bindable : Bindable.t) result ->
        match bindable with
        | Name name -> Name.Set.add name result
        | _ -> Misc.fatal_errorf "Only [Name]s allowed: %a " print t)
      (everything t)
      Name.Set.empty

  let remove t name =
    { in_terms = Bindable.Map.remove name t.in_terms;
      in_types = Bindable.Map.remove name t.in_types;
      in_debug_only = Bindable.Map.remove name t.in_debug_only;
    }

  let remove_var t var =
    remove t (Bindable.Name (Name.var var))

  (* CR mshinwell: Rename to "diff_free_and_bound" or something?
     Also double-check the semantics are correct here *)
  let diff t1 t2 =
    { in_terms = Bindable.Map.diff t1.in_terms t2.in_terms;
      in_types = Bindable.Map.diff t1.in_types
        (Bindable.Map.union t2.in_terms t2.in_types);
      in_debug_only = Bindable.Map.diff t1.in_debug_only
        (Bindable.Map.union t2.in_terms t2.in_debug_only);
    }

  let union t1 t2 =
    { in_terms = Bindable.Map.union t1.in_terms t2.in_terms;
      in_types = Bindable.Map.union t1.in_types t2.in_types;
      in_debug_only = Bindable.Map.union t1.in_debug_only t2.in_debug_only;
    }

  let rec union_list ts =
    match  ts with
    | [] -> empty
    | t::ts -> union t (union_list ts)

  let inter t1 t2 =
    { in_terms = Bindable.Map.inter t1.in_terms t2.in_terms;
      in_types = Bindable.Map.inter t1.in_types t2.in_types;
      in_debug_only = Bindable.Map.inter t1.in_debug_only t2.in_debug_only;
    }

  let subset
        { in_terms = in_terms1; in_types = in_types1;
          in_debug_only = in_debug_only1; }
        { in_terms = in_terms2; in_types = in_types2;
          in_debug_only = in_debug_only2; } =
    Bindable.Map.subset in_terms1 in_terms2
      && Bindable.Map.subset in_types1 in_types2
      && Bindable.Map.subset in_debug_only1 in_debug_only2

  let promote_to_in_types t =
    if not (Bindable.Map.is_empty t.in_debug_only) then begin
      Misc.fatal_errorf "Cannot promote set of names including one or more \
          marked as ``debug only'' to a set of names ``only occurring in \
          types''"
        print t
    end;
    let in_types = Bindable.Map.union t.in_terms t.in_types in
    { in_terms = Bindable.Map.empty;
      in_types;
      in_debug_only = Bindable.Map.empty;
    }

  let promote_to_debug_only t =
    { in_terms = Bindable.Map.empty;
      in_types = Bindable.Map.empty;
      in_debug_only = everything t;
    }

  let variables_only _t = Misc.fatal_error "NYI"
  (*
    { in_terms = Bindable.variables_only t.in_terms;
      in_types = Bindable.variables_only t.in_types;
      in_debug_only = Bindable.variables_only t.in_debug_only;
    }
  *)

  let equal
        { in_terms = in_terms1; in_types = in_types1;
          in_debug_only = in_debug_only1; }
        { in_terms = in_terms2; in_types = in_types2;
          in_debug_only = in_debug_only2; } =
    Bindable.Map.equal in_terms1 in_terms2
      && Bindable.Map.equal in_types1 in_types2
      && Bindable.Map.equal in_debug_only1 in_debug_only2

  let fold_everything t ~init ~f =
    let acc =
      Bindable.Map.fold (fun name acc -> f acc name)
        t.in_terms
        init
    in
    let acc =
      Bindable.Map.fold (fun name acc -> f acc name)
        t.in_types
        acc
    in
    Bindable.Map.fold (fun name acc -> f acc name)
      t.in_debug_only
      acc

  let choose_and_remove_amongst_everything t =
    match Bindable.Map.get_singleton t.in_terms with
    | Some name ->
      let t =
        { t with
          in_terms = Bindable.Map.remove name t.in_terms;
        }
      in
      Some (name, t)
    | None ->
      match Bindable.Map.get_singleton t.in_types with
      | Some name ->
        let t =
          { t with
            in_types = Bindable.Map.remove name t.in_types;
          }
        in
        Some (name, t)
      | None ->
        match Bindable.Map.get_singleton t.in_debug_only with
        | Some name ->
          let t =
            { t with
              in_debug_only = Bindable.Map.remove name t.in_debug_only;
            }
          in
          Some (name, t)
        | None -> None

  let apply_name_permutation { in_terms; in_types; in_debug_only; } perm =
    { in_terms = Name_permutation.apply_bindable_name_set perm in_terms;
      in_types = Name_permutation.apply_bindable_name_set perm in_types;
      in_debug_only = Name_permutation.apply_bindable_name_set perm in_debug_only;
    }
*)

  let count0 bindable map =
    match Bindable.Map.find bindable map with
    | exception Not_found -> 0
    | count -> count

  let count { in_terms; in_types; debug_only; } bindable =
    count0 bindable in_terms
      + count0 bindable in_types
      + count0 bindable debug_only
end

module For_variables = For_one_variety_of_names (Bindable_variable)
module For_continuations = For_one_variety_of_names (Bindable_continuation)

type t = {
  variables_in_terms : For_variables.t;
  variables_in_types : For_variables.t;
  variables_debug_only : For_variables.t;
  continuations : For_continuations.t;
}

let print ppf { variables_in_terms; variables_in_types; variables_debug_only;
      continuations; } =
  Format.fprintf ppf "@[\
      (variables_terms %a)@ \
      (variables_types %a)@ \
      (variables_debug_only %a)@ \
      (continuations %a)\
      @]"
    For_variables.print variables_in_terms
    For_variables.print variables_in_types
    For_variables.print variables_debug_only
    For_continuations.print continuations

let empty = {
  variables_in_terms = For_variables.empty;
  variables_in_types = For_variables.empty;
  variables_debug_only = For_variables.empty;
  continuations = For_continuations.empty;
}

let singleton_continuation t cont =
  { empty with
    continuations = For_continuations.singleton cont;
  }

let add_continuation t cont =
  { t with
    continuations = For_continuations.add t.continuations cont;
  }

let count_continuation t cont =
  For_continuations.count t.continuations cont
