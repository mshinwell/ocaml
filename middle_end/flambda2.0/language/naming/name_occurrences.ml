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

module Kind = struct
  type t =
    | Normal
    | In_types
    | Phantom

  type kind = t

  let normal = Normal
  let in_types = In_types
  let phantom = Phantom

  include Identifiable.Make (struct
    type nonrec t = t

    let print ppf t =
      match t with
      | Normal -> Format.pp_print_string ppf "Normal"
      | In_types -> Format.pp_print_string ppf "In_types"
      | Phantom -> Format.pp_print_string ppf "Phantom"

    let output _ _ = Misc.fatal_error "Not yet implemented"

    let hash _ = Misc.fatal_error "Not yet implemented"

    let number t =
      match t with
      | Normal -> 0
      | In_types -> 1
      | Phantom -> 2

    let compare t1 t2 =
      Stdlib.compare (number t1) (number t2)

    let equal t1 t2 =
      compare t1 t2 = 0
  end)

  module Or_absent : sig
    type t =
      | Absent
      | Present of kind

    let absent = Absent
    let present kind = Present kind

    include Identifiable.Make (struct
      type nonrec t = t

      let print ppf t =
        match t with
        | Absent -> Format.pp_print_string ppf "Absent"
        | Present kind ->
          Format.fprintf ppf "@[<hov 1>(Present@ %a)@]" print kind

      let output _ _ = Misc.fatal_error "Not yet implemented"

      let hash _ = Misc.fatal_error "Not yet implemented"

      let compare t1 t2 =
        match t1, t2 with
        | Absent, Absent -> 0
        | Absent, Present _ -> -1
        | Present _, Absent -> 1
        | Present kind1, Present kind2 -> compare kind1 kind2

      let equal t1 t2 =
        compare t1 t2 = 0
    end)
  end
end

module For_one_variety_of_names (N : sig
  include Identifiable.S
  val apply_name_permutation : t -> Name_permutation.t -> t
end) = struct
  (* The integers give the number of occurrences. *)
  type t = int Kind.Map.t N.Map.t

  let print ppf t =
    N.Map.print Format.pp_print_int ppf t

  let invariant t =
    if !Clflags.flambda_invariant_checks then begin
      let kind_map_ok by_kind =
        Kind.Map.for_all (fun _kind count -> count > 0) by_kind
      in
      let by_name_map_ok =
        N.Map.for_all (fun _name by_kind ->
            (not (Kind.Map.is_empty by_kind)) && kind_map_ok by_kind)
          t
      in
      if not by_name_map_ok then begin
        Misc.fatal_errorf "Invariant failed:@ %a" print t
      end
    end

  let empty = N.Map.empty

  let is_empty = N.Map.is_empty

  let singleton name kind = N.Map.singleton name (Kind.Map.singleton kind 1)

  let add t name kind =
    N.Map.update name (function
        | None -> Some (Kind.Map.singleton kind 1)
        | Some by_kind ->
          Kind.Map.update by_kind (function
              | None -> Some 1
              | Some count -> Some (count + 1))
            by_kind)
      t

  let apply_name_permutation t perm =
    N.Map.fold (fun name counts_by_kind result ->
        let name = N.apply_name_permutation name perm in
        N.Map.add name counts_by_kind result)
      t
      N.Map.empty

  let diff t1 t2 =
    let t =
      N.Map.merge (fun _name by_kind1 by_kind2 ->
          Kind.Map.merge (fun _kind count1 count2 ->
              let count1 = Option.value count1 ~default:0 in
              let count2 = Option.value count2 ~default:0 in
              let count = count1 - count2 in
              if count < 1 then None
              else Some count)
            by_kind1 by_kind2)
        t1 t2
    in
    invariant t;
    t

  let union t1 t2 =
    let t =
      N.Map.merge (fun _name count1 count2 ->
          Kind.Map.merge (fun _kind count1 count2 ->
              let count1 = Option.value count1 ~default:0 in
              let count2 = Option.value count2 ~default:0 in
              let count = count1 + count2 in
              if count < 1 then None
              else Some count)
            by_kind1 by_kind2)
        t1 t2
    in
    invariant t;
    t

  let subset t1 t2 = is_empty (diff t1 t2)

  let keys t = N.Map.keys t

  let mem t name = N.Map.mem name t

  let remove t name = N.Map.remove name t

  let count t name =
    (* CR-someday mshinwell: We could consider keeping the total count
       separately. *)
    match N.Map.find name t with
    | exception Not_found -> 0
    | by_kind ->
      Kind.Map.fold (fun _kind count total -> count + total) by_kind 0

  let greatest_occurrence_kind t name : Kind.Or_absent.t =
    match N.Map.find name t with
    | exception Not_found -> Absent
    | by_kind ->
      match Kind.Map.max_binding_opt by_kind with
      | Some (kind, _count) -> Present kind
      | None ->
        invariant t;
        assert false
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

let print ppf { variables continuations; symbols; } =
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
    continuations = For_continuations.singleton cont Normal;
  }

let add_continuation t cont =
  { t with
    continuations = For_continuations.add t.continuations cont Normal;
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

let singleton_symbol sym kind =
  { empty with
    symbols = For_symbols.singleton sym kind;
  }

let singleton_name_in_terms (name : Name.t) kind =
  match name with
  | Var var -> singleton_variable var kind
  | Symbol sym -> singleton_symbol sym kind

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

let subset t1 t2 =
  binary_predicate ~for_variables:For_variables.subset
    ~for_continuations:For_continuations.subset
    ~for_symbols:For_symbols.subset
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
