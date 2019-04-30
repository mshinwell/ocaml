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

module For_one_variety_of_names (N : sig
  include Identifiable.S
  val apply_name_permutation : t -> Name_permutation.t -> t
end) = struct

(*
  type t = {
    in_terms : int N.Map.t;
    in_types : int N.Map.t;
    in_debug_only : int N.Map.t;
  }

*)

  (* The integers gives the counts of free occurrences. *)
  type t = int N.Map.t

  let print ppf t =
    N.Map.print Format.pp_print_int ppf t

  let empty = N.Map.empty

  let singleton name = N.Map.singleton name 1

  let add t name =
    N.Map.update name
      (function None -> Some 1 | Some count -> Some (count + 1))
      t

  let apply_name_permutation t perm =
    N.Map.fold (fun name count result ->
        let name = N.apply_name_permutation name perm in
        N.Map.add name count result)
      t
      N.Map.empty

  let union t1 t2 =
    N.Map.merge (fun _name count1 count2 ->
        let count1 = Option.value count1 ~default:0 in
        let count2 = Option.value count2 ~default:0 in
        let count = count1 + count2 in
        if count < 1 then None
        else Some count)
      t1 t2

  let keys t = N.Map.keys t

(*
  let create_from_set_in_terms in_terms =
    { in_terms;
      in_types = N.Map.empty;
      in_debug_only = N.Map.empty;
    }

  let create_from_name_set_in_terms in_terms =
    let in_terms =
      Name.Set.fold (fun name result ->
          N.Map.add (Name name) result)
        in_terms
        N.Map.empty
    in
    create_from_set_in_terms in_terms

  let create_from_set_in_types in_types =
    { in_terms = N.Map.empty;
      in_types;
      in_debug_only = N.Map.empty;
    }

  let create_from_name_set_in_types in_types =
    let in_types =
      Name.Set.fold (fun name result ->
          N.Map.add (Name name) result)
        in_types
        N.Map.empty
    in
    create_from_set_in_types in_types

  let is_empty { in_terms; in_types; in_debug_only; } =
    N.Map.is_empty in_terms
      && N.Map.is_empty in_types
      && N.Map.is_empty in_debug_only
*)
(*
  let singleton_in_terms name =
    create_from_set_in_terms (N.Map.singleton name)
*)

(*
  let singleton_in_types name =
    create_from_set_in_types (N.Map.singleton name)

  let of_list_in_terms names =
    create_from_set_in_terms (N.Map.of_list names)


  let add { in_terms; in_types; debug_only; } name kind =
    match kind with
    | In_terms ->
      { t with in_terms = add0 name in_terms; }
    | In_types ->
      { t with in_types = add0 name in_types; }
    | Debug_only ->
      { t with in_debug_only = add0 name in_debug_only; }

  let add_set t names kind =
    N.Map.fold (fun name t -> add t name kind) names t

  let in_terms t = t.in_terms
  let in_types t = t.in_types
  let in_debug_only t = t.in_debug_only

  let mem_in_terms t name = N.Map.mem name t.in_terms
  let mem_in_types t name = N.Map.mem name t.in_types
  let mem_in_debug_only t name = N.Map.mem name t.in_debug_only

  let mem t name =
    mem_in_terms t name || mem_in_types t name || mem_in_debug_only t name

  let mem_var t var =
    mem t (N.Name (Name.var var))

  let everything t =
    N.Map.union t.in_terms
      (N.Map.union t.in_types t.in_debug_only)

  let everything_must_only_be_names t =
    N.Map.fold (fun (name : N.t) result ->
        match name with
        | Name name -> Name.Set.add name result
        | _ -> Misc.fatal_errorf "Only [Name]s allowed: %a " print t)
      (everything t)
      Name.Set.empty

  let remove t name =
    { in_terms = N.Map.remove name t.in_terms;
      in_types = N.Map.remove name t.in_types;
      in_debug_only = N.Map.remove name t.in_debug_only;
    }

  let remove_var t var =
    remove t (N.Name (Name.var var))

  (* CR mshinwell: Rename to "diff_free_and_bound" or something?
     Also double-check the semantics are correct here *)
  let diff t1 t2 =
    { in_terms = N.Map.diff t1.in_terms t2.in_terms;
      in_types = N.Map.diff t1.in_types
        (N.Map.union t2.in_terms t2.in_types);
      in_debug_only = N.Map.diff t1.in_debug_only
        (N.Map.union t2.in_terms t2.in_debug_only);
    }

  let union t1 t2 =
    { in_terms = N.Map.union t1.in_terms t2.in_terms;
      in_types = N.Map.union t1.in_types t2.in_types;
      in_debug_only = N.Map.union t1.in_debug_only t2.in_debug_only;
    }

  let rec union_list ts =
    match  ts with
    | [] -> empty
    | t::ts -> union t (union_list ts)

  let inter t1 t2 =
    { in_terms = N.Map.inter t1.in_terms t2.in_terms;
      in_types = N.Map.inter t1.in_types t2.in_types;
      in_debug_only = N.Map.inter t1.in_debug_only t2.in_debug_only;
    }

  let subset
        { in_terms = in_terms1; in_types = in_types1;
          in_debug_only = in_debug_only1; }
        { in_terms = in_terms2; in_types = in_types2;
          in_debug_only = in_debug_only2; } =
    N.Map.subset in_terms1 in_terms2
      && N.Map.subset in_types1 in_types2
      && N.Map.subset in_debug_only1 in_debug_only2

  let promote_to_in_types t =
    if not (N.Map.is_empty t.in_debug_only) then begin
      Misc.fatal_errorf "Cannot promote set of names including one or more \
          marked as ``debug only'' to a set of names ``only occurring in \
          types''"
        print t
    end;
    let in_types = N.Map.union t.in_terms t.in_types in
    { in_terms = N.Map.empty;
      in_types;
      in_debug_only = N.Map.empty;
    }

  let promote_to_debug_only t =
    { in_terms = N.Map.empty;
      in_types = N.Map.empty;
      in_debug_only = everything t;
    }

  let variables_only _t = Misc.fatal_error "NYI"
  (*
    { in_terms = N.variables_only t.in_terms;
      in_types = N.variables_only t.in_types;
      in_debug_only = N.variables_only t.in_debug_only;
    }
  *)

  let equal
        { in_terms = in_terms1; in_types = in_types1;
          in_debug_only = in_debug_only1; }
        { in_terms = in_terms2; in_types = in_types2;
          in_debug_only = in_debug_only2; } =
    N.Map.equal in_terms1 in_terms2
      && N.Map.equal in_types1 in_types2
      && N.Map.equal in_debug_only1 in_debug_only2

  let fold_everything t ~init ~f =
    let acc =
      N.Map.fold (fun name acc -> f acc name)
        t.in_terms
        init
    in
    let acc =
      N.Map.fold (fun name acc -> f acc name)
        t.in_types
        acc
    in
    N.Map.fold (fun name acc -> f acc name)
      t.in_debug_only
      acc

  let choose_and_remove_amongst_everything t =
    match N.Map.get_singleton t.in_terms with
    | Some name ->
      let t =
        { t with
          in_terms = N.Map.remove name t.in_terms;
        }
      in
      Some (name, t)
    | None ->
      match N.Map.get_singleton t.in_types with
      | Some name ->
        let t =
          { t with
            in_types = N.Map.remove name t.in_types;
          }
        in
        Some (name, t)
      | None ->
        match N.Map.get_singleton t.in_debug_only with
        | Some name ->
          let t =
            { t with
              in_debug_only = N.Map.remove name t.in_debug_only;
            }
          in
          Some (name, t)
        | None -> None

  let apply_name_permutation { in_terms; in_types; in_debug_only; } perm =
    { in_terms = Name_permutation.apply_name_name_set perm in_terms;
      in_types = Name_permutation.apply_name_name_set perm in_types;
      in_debug_only = Name_permutation.apply_name_name_set perm in_debug_only;
    }
*)

  let count t name =
    match N.Map.find name t with
    | exception Not_found -> 0
    | count -> count
end

module For_variables = For_one_variety_of_names (struct
  include Variable
  let apply_name_permutation t perm = Name_permutation.apply_variable perm t
end)

module For_continuations = For_one_variety_of_names (struct
  include Continuation
  let apply_name_permutation t perm = Name_permutation.apply_continuation perm t
end)

module For_symbols = For_one_variety_of_names (struct
  include Symbol
  (* We never bind [Symbol]s using [Name_abstraction]. *)
  let apply_name_permutation t _perm = t
end)

(* CR mshinwell: We might need an "in terms" / "in types" distinction for
   [Symbol]s, perhaps?  Maybe for "debug only" too, e.g. phantom let. *)

type t = {
  variables_in_terms : For_variables.t;
  variables_in_types : For_variables.t;
  variables_debug_only : For_variables.t;
  continuations : For_continuations.t;
  symbols : For_symbols.t;
}

let print ppf { variables_in_terms; variables_in_types; variables_debug_only;
      continuations; symbols; } =
  Format.fprintf ppf "@[\
      (variables_terms %a)@ \
      (variables_types %a)@ \
      (variables_debug_only %a)@ \
      (continuations %a)\
      (symbols %a)\
      @]"
    For_variables.print variables_in_terms
    For_variables.print variables_in_types
    For_variables.print variables_debug_only
    For_continuations.print continuations
    For_symbols.print symbols

let empty = {
  variables_in_terms = For_variables.empty;
  variables_in_types = For_variables.empty;
  variables_debug_only = For_variables.empty;
  continuations = For_continuations.empty;
  symbols = For_symbols.empty;
}

let singleton_continuation cont =
  { empty with
    continuations = For_continuations.singleton cont;
  }

let add_continuation t cont =
  { t with
    continuations = For_continuations.add t.continuations cont;
  }

let count_continuation t cont =
  For_continuations.count t.continuations cont

let singleton_variable_in_terms var =
  { empty with
    variables_in_terms = For_variables.singleton var;
  }

let add_variable_in_terms t var =
  { t with
    variables_in_terms = For_variables.add t.variables_in_terms var;
  }

let singleton_symbol sym =
  { empty with
    symbols = For_symbols.singleton sym;
  }

let singleton_name_in_terms (name : Name.t) =
  match name with
  | Var var -> singleton_variable_in_terms var
  | Symbol sym -> singleton_symbol sym
  | Logical_var _ -> empty

let apply_name_permutation { variables_in_terms; variables_in_types;
      variables_debug_only; continuations; symbols; } perm =
  let variables_in_terms =
    For_variables.apply_name_permutation variables_in_terms perm
  in
  let variables_in_types =
    For_variables.apply_name_permutation variables_in_types perm
  in
  let variables_debug_only =
    For_variables.apply_name_permutation variables_debug_only perm
  in
  let continuations =
    For_continuations.apply_name_permutation continuations perm
  in
  let symbols =
    For_symbols.apply_name_permutation symbols perm
  in
  { variables_in_terms;
    variables_in_types;
    variables_debug_only;
    continuations;
    symbols;
  }

let union
      { variables_in_terms = variables_in_terms1;
        variables_in_types = variables_in_types1;
        variables_debug_only = variables_debug_only1;
        continuations = continuations1;
        symbols = symbols1;
      }
      { variables_in_terms = variables_in_terms2;
        variables_in_types = variables_in_types2;
        variables_debug_only = variables_debug_only2;
        continuations = continuations2;
        symbols = symbols2;
      } =
  let variables_in_terms =
    For_variables.union variables_in_terms1 variables_in_terms2
  in
  let variables_in_types =
    For_variables.union variables_in_types1 variables_in_types2
  in
  let variables_debug_only =
    For_variables.union variables_debug_only1 variables_debug_only2
  in
  let continuations =
    For_continuations.union continuations1 continuations2
  in
  let symbols =
    For_symbols.union symbols1 symbols2
  in
  { variables_in_terms;
    variables_in_types;
    variables_debug_only;
    continuations;
    symbols;
  }

let rec union_list ts =
  match  ts with
  | [] -> empty
  | t::ts -> union t (union_list ts)

let symbols t = For_symbols.keys t.symbols
