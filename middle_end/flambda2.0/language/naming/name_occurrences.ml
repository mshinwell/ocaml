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

module Kind = Name_occurrence_kind

module Num_occurrences = struct
  type t =
    | Zero
    | One
    | More_than_one

  let print ppf t =
    match t with
    | Zero -> Format.fprintf ppf "Zero"
    | One -> Format.fprintf ppf "One"
    | More_than_one -> Format.fprintf ppf "More_than_one"
end

module For_one_variety_of_names (N : sig
  include Identifiable.S
  val apply_name_permutation : t -> Name_permutation.t -> t
end) = struct
  module For_one_name = struct
    type t = {
      num_occurrences : int;
      by_kind : int Kind.Map.t;
      (* [by_kind] is closed in the following sense: if there is an
         occurrence at some particular kind, that occurrence is counted in
         [by_kind] at that kind and all kinds lower than it. *)
    }

    let print ppf { num_occurrences; by_kind; } =
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(num_occurrences %d)@]@ \
          @[<hov 1>(by_kind %a)@]\
          )@]"
        num_occurrences
        (Kind.Map.print Format.pp_print_int) by_kind

    let invariant ({ num_occurrences = _; by_kind; } as t) =
      if Kind.Map.is_empty by_kind
        || Kind.Map.exists (fun _kind count -> count <= 0) by_kind
      then begin
        Misc.fatal_errorf "[For_one_name] invariant failed:@ %a" print t
      end

    let num_occurrences t = t.num_occurrences
    let by_kind t = t.by_kind

    let empty = {
      num_occurrences = 0;
      by_kind = Kind.Map.empty;
    }

    let downgrade_occurrences_at_strictly_greater_kind t max_kind =
      let strictly_less, at_max_kind, _strictly_greater =
        Kind.Map.split max_kind t.by_kind
      in
      let by_kind =
        match at_max_kind with
        | None -> strictly_less
        | Some at_max_kind -> Kind.Map.add max_kind at_max_kind strictly_less
      in
      { num_occurrences = t.num_occurrences;
        by_kind;
      }
  end

  type t = For_one_name.t N.Map.t

  let print ppf t =
    N.Map.print For_one_name.print ppf t

  let invariant t =
    if !Clflags.flambda_invariant_checks then begin
      N.Map.iter (fun _name for_one_name ->
          For_one_name.invariant for_one_name)
        t
    end

  let empty = N.Map.empty

  let is_empty = N.Map.is_empty

  let add0 t name kind ~update_num_occurrences =
    N.Map.update name (function
        | None ->
          let num_occurrences =
            if update_num_occurrences then 1 else 0
          in
          let for_one_name : For_one_name.t =
            { num_occurrences;
              by_kind = Kind.Map.singleton kind 1;
            }
          in
          Some for_one_name
        | Some for_one_name ->
          let num_occurrences =
            if update_num_occurrences then
               For_one_name.num_occurrences for_one_name + 1
            else
               For_one_name.num_occurrences for_one_name
          in
          let by_kind =
            Kind.Map.update kind (function
                | None -> Some 1
                | Some count -> Some (count + 1))
              (For_one_name.by_kind for_one_name)
          in
          let for_one_name : For_one_name.t =
            { num_occurrences;
              by_kind;
            }
          in
          Some for_one_name)
      t

  let add t name kind =
    Kind.Set.fold (fun kind' t ->
        let update_num_occurrences = Name_occurrence_kind.equal kind kind' in
        add0 t name kind' ~update_num_occurrences)
      (Kind.all_less_than_or_equal_to kind)
      t

  let singleton name kind =
    add empty name kind

  let apply_name_permutation t perm =
    N.Map.fold (fun name for_one_name result ->
        let name = N.apply_name_permutation name perm in
        N.Map.add name for_one_name result)
      t
      N.Map.empty

  let diff t1 t2 =
    N.Set.fold (fun name t -> N.Map.remove name t)
      (N.Map.keys t2)
      t1

  let union t1 t2 =
    let t =
      N.Map.merge (fun _name for_one_name1 for_one_name2 ->
          let for_one_name1 =
            Option.value for_one_name1 ~default:For_one_name.empty
          in
          let for_one_name2 =
            Option.value for_one_name2 ~default:For_one_name.empty
          in
          let by_kind1 = For_one_name.by_kind for_one_name1 in
          let by_kind2 = For_one_name.by_kind for_one_name2 in
          let by_kind =
            Kind.Map.merge (fun _kind count1 count2 ->
                let count1 = Option.value count1 ~default:0 in
                let count2 = Option.value count2 ~default:0 in
                let count = count1 + count2 in
                if count < 1 then None
                else Some count)
              by_kind1 by_kind2
          in
          assert (not (Kind.Map.is_empty by_kind));
          let num_occurrences =
            For_one_name.num_occurrences for_one_name1
              + For_one_name.num_occurrences for_one_name2
          in
          let for_one_name : For_one_name.t =
            { num_occurrences;
              by_kind;
            }
          in
          Some for_one_name)
        t1 t2
    in
    invariant t;
    t

  let keys t = N.Map.keys t

  let subset_domain t1 t2 = N.Set.subset (N.Map.keys t1) (N.Map.keys t2)

  let mem t name = N.Map.mem name t

  let remove t name = N.Map.remove name t

  let count t name : Num_occurrences.t =
    match N.Map.find name t with
    | exception Not_found -> Zero
    | for_one_name ->
      let num_occurrences = For_one_name.num_occurrences for_one_name in
      assert (num_occurrences >= 0);
      if num_occurrences = 0 then Zero
      else if num_occurrences = 1 then One
      else More_than_one

  let greatest_occurrence_kind t name : Kind.Or_absent.t =
    match N.Map.find name t with
    | exception Not_found -> Kind.Or_absent.absent
    | for_one_name ->
      match Kind.Map.max_binding_opt (For_one_name.by_kind for_one_name) with
      | Some (kind, _count) -> Kind.Or_absent.present kind
      | None ->
        invariant t;
        assert false

  let downgrade_occurrences_at_strictly_greater_kind t max_kind =
    N.Map.map (fun for_one_name ->
        For_one_name.downgrade_occurrences_at_strictly_greater_kind for_one_name
          max_kind)
      t
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

type t = {
  variables : For_variables.t;
  continuations : For_continuations.t;
  symbols : For_symbols.t;
}

let print ppf { variables; continuations; symbols; } =
  Format.fprintf ppf "@[<hov 1>\
      @[<hov 1>(variables %a)@]@ \
      @[<hov 1>(continuations %a)@]@ \
      @[<hov 1>(symbols %a)@]\
      @]"
    For_variables.print variables
    For_continuations.print continuations
    For_symbols.print symbols

let empty = {
  variables = For_variables.empty;
  continuations = For_continuations.empty;
  symbols = For_symbols.empty;
}

let singleton_continuation cont =
  { empty with
    continuations = For_continuations.singleton cont Kind.normal;
  }

let add_continuation t cont =
  { t with
    continuations = For_continuations.add t.continuations cont Kind.normal;
  }

let count_continuation t cont =
  For_continuations.count t.continuations cont

let singleton_variable var kind =
  { empty with
    variables = For_variables.singleton var kind;
  }

let add_variable t var kind =
  { t with
    variables = For_variables.add t.variables var kind;
  }

let add_symbol t sym kind =
  { t with
    symbols = For_symbols.add t.symbols sym kind;
  }

let add_name t (name : Name.t) kind =
  match name with
  | Var var -> add_variable t var kind
  | Symbol sym -> add_symbol t sym kind

let singleton_symbol sym kind =
  { empty with
    symbols = For_symbols.singleton sym kind;
  }

let singleton_name (name : Name.t) kind =
  match name with
  | Var var -> singleton_variable var kind
  | Symbol sym -> singleton_symbol sym kind

let create_variables vars kind =
  Variable.Set.fold (fun (var : Variable.t) t ->
      add_variable t var kind)
    vars
    empty

let create_names names kind =
  Name.Set.fold (fun (name : Name.t) t ->
      match name with
      | Var var -> add_variable t var kind
      | Symbol sym -> add_symbol t sym kind)
    names
    empty

let apply_name_permutation { variables; continuations; symbols; } perm =
  let variables =
    For_variables.apply_name_permutation variables perm
  in
  let continuations =
    For_continuations.apply_name_permutation continuations perm
  in
  let symbols =
    For_symbols.apply_name_permutation symbols perm
  in
  { variables;
    continuations;
    symbols;
  }

let binary_predicate ~for_variables ~for_continuations ~for_symbols
      { variables = variables1;
        continuations = continuations1;
        symbols = symbols1;
      }
      { variables = variables2;
        continuations = continuations2;
        symbols = symbols2;
      } =
  for_variables variables1 variables2
    && for_continuations continuations1 continuations2
    && for_symbols symbols1 symbols2

let binary_op ~for_variables ~for_continuations ~for_symbols
      { variables = variables1;
        continuations = continuations1;
        symbols = symbols1;
      }
      { variables = variables2;
        continuations = continuations2;
        symbols = symbols2;
      } =
  let variables = for_variables variables1 variables2 in
  let continuations = for_continuations continuations1 continuations2 in
  let symbols = for_symbols symbols1 symbols2 in
  { variables;
    continuations;
    symbols;
  }

let diff t1 t2 =
  binary_op ~for_variables:For_variables.diff
    ~for_continuations:For_continuations.diff
    ~for_symbols:For_symbols.diff
    t1 t2

let union t1 t2 =
  binary_op ~for_variables:For_variables.union
    ~for_continuations:For_continuations.union
    ~for_symbols:For_symbols.union
    t1 t2

let subset_domain t1 t2 =
  binary_predicate ~for_variables:For_variables.subset_domain
    ~for_continuations:For_continuations.subset_domain
    ~for_symbols:For_symbols.subset_domain
    t1 t2

let rec union_list ts =
  match ts with
  | [] -> empty
  | t::ts -> union t (union_list ts)

let variables t = For_variables.keys t.variables
let symbols t = For_symbols.keys t.symbols

let mem_var t var = For_variables.mem t.variables var
let mem_symbol t symbol = For_symbols.mem t.symbols symbol

let mem_name t (name : Name.t) =
  match name with
  | Var var -> mem_var t var
  | Symbol symbol -> mem_symbol t symbol

let remove_var t var =
  let variables = For_variables.remove t.variables var in
  { t with
    variables;
  }

let only_contains_symbols { variables; continuations; symbols; } =
  For_variables.is_empty variables
    && For_continuations.is_empty continuations
    && For_symbols.is_empty symbols

let greatest_occurrence_kind_var t var =
  For_variables.greatest_occurrence_kind t.variables var

let downgrade_occurrences_at_strictly_greater_kind
      { variables; continuations; symbols; } max_kind =
  let variables =
    For_variables.downgrade_occurrences_at_strictly_greater_kind
      variables max_kind
  in
  let continuations =
    For_continuations.downgrade_occurrences_at_strictly_greater_kind
      continuations max_kind
  in
  let symbols =
    For_symbols.downgrade_occurrences_at_strictly_greater_kind
      symbols max_kind
  in
  { variables;
    continuations;
    symbols;
  }
