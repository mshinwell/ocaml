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

module Kind = Name_mode

module Num_occurrences : sig
  type t =
    | Zero
    | One
    | More_than_one

  val print : Format.formatter -> t -> unit
end = struct
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
end) : sig
  type t

  val print : Format.formatter -> t -> unit

  val equal : t -> t -> bool

  val invariant : t -> unit

  val empty : t

  val is_empty : t -> bool

  val singleton : N.t -> Kind.t -> t

  val add : t -> N.t -> Kind.t -> t

  val apply_name_permutation : t -> Name_permutation.t -> t

  val diff : t -> t -> t

  val union : t -> t -> t

  val keys : t -> N.Set.t

  val subset_domain : t -> t -> bool

  val overlap : t -> t -> bool

  val mem : t -> N.t -> bool

  val remove : t -> N.t -> t

  val count : t -> N.t -> Num_occurrences.t

  val greatest_name_mode : t -> N.t -> Kind.Or_absent.t

  val downgrade_occurrences_at_strictly_greater_kind : t -> Kind.t -> t
end = struct
  module For_one_name : sig
    type t

    val print : Format.formatter -> t -> unit

    val invariant : t -> unit

    val equal : t -> t -> bool

    val one_occurrence : Kind.t -> t

    val add : t -> Kind.t -> t

    val num_occurrences : t -> int

    val downgrade_occurrences_at_strictly_greater_kind : t -> Kind.t -> t

    val max_kind_opt : t -> Kind.t option

    val union : t -> t -> t
  end = struct
    type t = int array

    let print ppf t =
      let by_kind =
        t
        |> Array.mapi (fun mode count -> Kind.of_int mode, count)
        |> Array.to_list
        |> Kind.Map.of_list
      in
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(by_kind %a)@]\
          )@]"
        (Kind.Map.print Format.pp_print_int) by_kind

    let invariant t =
      (* CR mshinwell: Check num_occurrences *)
      if Array.length t <> Kind.max_to_int + 1
        || Array.exists (fun count -> count < 0) t
      then begin
        Misc.fatal_errorf "[For_one_name] invariant failed:@ %a" print t
      end

    let equal t1 t2 = Stdlib.compare t1 t2 = 0

    let num_occurrences t =
      Array.fold_left (fun total num_occs -> num_occs + total) 0 t

    let one_occurrence kind =
      let kind = Kind.to_int kind in
      Array.init (Kind.max_to_int + 1)
        (fun mode -> if mode = kind then 1 else 0)

    let add t kind =
      let kind = Kind.to_int kind in
      let t = Array.copy t in
      t.(kind) <- t.(kind) + 1;
      t

    (* CR mshinwell: Add -strict-sequence to the build *)

    let downgrade_occurrences_at_strictly_greater_kind t max_kind =
      let max_kind = Kind.to_int max_kind in
      let needed = ref false in
      for kind = max_kind + 1 to Kind.max_to_int do
        if t.(kind) > 0 then begin
          needed := true
        end
      done;
      if not !needed then t
      else begin
        let t = Array.copy t in
        for kind = max_kind + 1 to Kind.max_to_int do
          let count = t.(kind) in
          t.(max_kind) <- t.(max_kind) + count;
          t.(kind) <- 0
        done;
        t
      end

    let max_kind_opt t =
      let result = ref (-1) in
      for mode = 0 to Kind.max_to_int do
        if t.(mode) > 0 then begin
          result := mode
        end;
      done;
      if !result < 0 then None else Some (Kind.of_int !result)

    let union t1 t2 =
      let t = Array.copy t1 in
      for mode = 0 to Kind.max_to_int do
        t.(mode) <- t.(mode) + t2.(mode)
      done;
      t
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

  let equal t1 t2 = N.Map.equal For_one_name.equal t1 t2

  let empty = N.Map.empty

  let is_empty = N.Map.is_empty

  let singleton name kind =
    N.Map.singleton name (For_one_name.one_occurrence kind)

  let add t name kind =
    N.Map.update name (function
        | None -> Some (For_one_name.one_occurrence kind)
        | Some for_one_name -> Some (For_one_name.add for_one_name kind))
      t

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
          match for_one_name1, for_one_name2 with
          | None, None -> None
          | None, Some _ -> for_one_name2
          | Some _, None -> for_one_name1
          | Some for_one_name1, Some for_one_name2 ->
            Some (For_one_name.union for_one_name1 for_one_name2))
        t1 t2
    in
    invariant t;
    t

  let keys t = N.Map.keys t

  let subset_domain t1 t2 = N.Set.subset (N.Map.keys t1) (N.Map.keys t2)

  let overlap t1 t2 =
    not (N.Set.is_empty (N.Set.inter (N.Map.keys t1) (N.Map.keys t2)))

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

  let greatest_name_mode t name : Kind.Or_absent.t =
    match N.Map.find name t with
    | exception Not_found -> Kind.Or_absent.absent
    | for_one_name ->
      match For_one_name.max_kind_opt for_one_name with
      | None -> Kind.Or_absent.absent
      | Some kind -> Kind.Or_absent.present kind

  let downgrade_occurrences_at_strictly_greater_kind t max_kind =
    (* CR-someday mshinwell: This can be condensed when the compiler
       removes the closure allocation if [max_kind] is captured. *)
    match Kind.descr max_kind with
    | Normal -> t
    | Phantom ->
      N.Map.map (fun for_one_name ->
          For_one_name.downgrade_occurrences_at_strictly_greater_kind
            for_one_name Kind.phantom)
        t
    | In_types ->
      N.Map.map (fun for_one_name ->
          For_one_name.downgrade_occurrences_at_strictly_greater_kind
            for_one_name Kind.in_types)
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

module For_closure_vars = For_one_variety_of_names (struct
  include Var_within_closure
  (* We never bind [Var_within_closure]s using [Name_abstraction]. *)
  let apply_name_permutation t _perm = t
end)

module For_code_ids = For_one_variety_of_names (struct
  include Code_id
  (* We never bind [Code_id]s using [Name_abstraction]. *)
  let apply_name_permutation t _perm = t
end)

type t = {
  variables : For_variables.t;
  continuations : For_continuations.t;
  symbols : For_symbols.t;
  closure_vars : For_closure_vars.t;
  code_ids : For_code_ids.t;
  newer_version_of_code_ids : For_code_ids.t;
  (* [newer_version_of_code_ids] tracks those code IDs that occur in
     "newer version of" fields (e.g. in [Flambda_static.Static_part.code]). *)
}

let print ppf { variables; continuations; symbols; closure_vars;
                code_ids; newer_version_of_code_ids; } =
  Format.fprintf ppf "@[<hov 1>\
      @[<hov 1>(variables %a)@]@ \
      @[<hov 1>(continuations %a)@]@ \
      @[<hov 1>(symbols %a)@]@ \
      @[<hov 1>(closure_vars %a)@]@ \
      @[<hov 1>(code_ids %a)@]\
      @[<hov 1>(newer_version_of_code_ids %a)@]\
      @]"
    For_variables.print variables
    For_continuations.print continuations
    For_symbols.print symbols
    For_closure_vars.print closure_vars
    For_code_ids.print code_ids
    For_code_ids.print newer_version_of_code_ids

let empty = {
  variables = For_variables.empty;
  continuations = For_continuations.empty;
  symbols = For_symbols.empty;
  closure_vars = For_closure_vars.empty;
  code_ids = For_code_ids.empty;
  newer_version_of_code_ids = For_code_ids.empty;
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

let count_variable t var =
  For_variables.count t.variables var

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

let add_closure_var t clos_var kind =
  { t with
    closure_vars = For_closure_vars.add t.closure_vars clos_var kind;
  }

let add_code_id t id kind =
  { t with
    code_ids = For_code_ids.add t.code_ids id kind;
  }

let add_newer_version_of_code_id t id kind =
  { t with
    newer_version_of_code_ids =
      For_code_ids.add t.newer_version_of_code_ids id kind;
  }

let singleton_symbol sym kind =
  { empty with
    symbols = For_symbols.singleton sym kind;
  }

let singleton_name (name : Name.t) kind =
  match name with
  | Var var -> singleton_variable var kind
  | Symbol sym -> singleton_symbol sym kind

let create_variables vars kind =
  (* CR mshinwell: This reallocates the record for each [var]! *)
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

let create_closure_vars clos_vars =
  let closure_vars =
    Var_within_closure.Set.fold (fun clos_var closure_vars ->
        For_closure_vars.add closure_vars clos_var Name_mode.normal)
      clos_vars
      For_closure_vars.empty
  in
  { empty with closure_vars; }

let apply_name_permutation
      ({ variables; continuations; symbols; closure_vars; code_ids;
         newer_version_of_code_ids; } as t)
      perm =
  if Name_permutation.is_empty perm then t
  else
    let variables =
      For_variables.apply_name_permutation variables perm
    in
    let continuations =
      For_continuations.apply_name_permutation continuations perm
    in
    (* [Symbol]s, [Var_within_closure]s and [Code_id]s are never bound using
       [Name_abstraction]. *)
    { variables;
      continuations;
      symbols;
      closure_vars;
      code_ids;
      newer_version_of_code_ids;
    }

let binary_conjunction ~for_variables ~for_continuations ~for_symbols
      ~for_closure_vars ~for_code_ids 
      { variables = variables1;
        continuations = continuations1;
        symbols = symbols1;
        closure_vars = closure_vars1;
        code_ids = code_ids1;
        newer_version_of_code_ids = newer_version_of_code_ids1;
      }
      { variables = variables2;
        continuations = continuations2;
        symbols = symbols2;
        closure_vars = closure_vars2;
        code_ids = code_ids2;
        newer_version_of_code_ids = newer_version_of_code_ids2;
      } =
  for_variables variables1 variables2
    && for_continuations continuations1 continuations2
    && for_symbols symbols1 symbols2
    && for_closure_vars closure_vars1 closure_vars2
    && for_code_ids code_ids1 code_ids2
    && for_code_ids newer_version_of_code_ids1 newer_version_of_code_ids2

let binary_disjunction ~for_variables ~for_continuations ~for_symbols
      ~for_closure_vars ~for_code_ids 
      { variables = variables1;
        continuations = continuations1;
        symbols = symbols1;
        closure_vars = closure_vars1;
        code_ids = code_ids1;
        newer_version_of_code_ids = newer_version_of_code_ids1;
      }
      { variables = variables2;
        continuations = continuations2;
        symbols = symbols2;
        closure_vars = closure_vars2;
        code_ids = code_ids2;
        newer_version_of_code_ids = newer_version_of_code_ids2;
      } =
  for_variables variables1 variables2
    || for_continuations continuations1 continuations2
    || for_symbols symbols1 symbols2
    || for_closure_vars closure_vars1 closure_vars2
    || for_code_ids code_ids1 code_ids2
    || for_code_ids newer_version_of_code_ids1 newer_version_of_code_ids2

let binary_op ~for_variables ~for_continuations ~for_symbols ~for_closure_vars
      ~for_code_ids
      { variables = variables1;
        continuations = continuations1;
        symbols = symbols1;
        closure_vars = closure_vars1;
        code_ids = code_ids1;
        newer_version_of_code_ids = newer_version_of_code_ids1;
      }
      { variables = variables2;
        continuations = continuations2;
        symbols = symbols2;
        closure_vars = closure_vars2;
        code_ids = code_ids2;
        newer_version_of_code_ids = newer_version_of_code_ids2;
      } =
  let variables = for_variables variables1 variables2 in
  let continuations = for_continuations continuations1 continuations2 in
  let symbols = for_symbols symbols1 symbols2 in
  let closure_vars = for_closure_vars closure_vars1 closure_vars2 in
  let code_ids = for_code_ids code_ids1 code_ids2 in
  let newer_version_of_code_ids =
    for_code_ids newer_version_of_code_ids1 newer_version_of_code_ids2
  in
  { variables;
    continuations;
    symbols;
    closure_vars;
    code_ids;
    newer_version_of_code_ids;
  }

let diff t1 t2 =
  binary_op ~for_variables:For_variables.diff
    ~for_continuations:For_continuations.diff
    ~for_symbols:For_symbols.diff
    ~for_closure_vars:For_closure_vars.diff
    ~for_code_ids:For_code_ids.diff
    t1 t2

let union t1 t2 =
  binary_op ~for_variables:For_variables.union
    ~for_continuations:For_continuations.union
    ~for_symbols:For_symbols.union
    ~for_closure_vars:For_closure_vars.union
    ~for_code_ids:For_code_ids.union
    t1 t2

let equal t1 t2 =
  binary_conjunction ~for_variables:For_variables.equal
    ~for_continuations:For_continuations.equal
    ~for_symbols:For_symbols.equal
    ~for_closure_vars:For_closure_vars.equal
    ~for_code_ids:For_code_ids.equal
    t1 t2

let is_empty t = equal t empty

let subset_domain t1 t2 =
  binary_conjunction ~for_variables:For_variables.subset_domain
    ~for_continuations:For_continuations.subset_domain
    ~for_symbols:For_symbols.subset_domain
    ~for_closure_vars:For_closure_vars.subset_domain
    ~for_code_ids:For_code_ids.subset_domain
    t1 t2

let overlap t1 t2 =
  binary_disjunction ~for_variables:For_variables.overlap
    ~for_continuations:For_continuations.overlap
    ~for_symbols:For_symbols.overlap
    ~for_closure_vars:For_closure_vars.overlap
    ~for_code_ids:For_code_ids.overlap
    t1 t2

let rec union_list ts =
  match ts with
  | [] -> empty
  | t::ts -> union t (union_list ts)

let variables t = For_variables.keys t.variables
let symbols t = For_symbols.keys t.symbols
let closure_vars t = For_closure_vars.keys t.closure_vars
let continuations t = For_continuations.keys t.continuations
let code_ids t = For_code_ids.keys t.code_ids
let newer_version_of_code_ids t = For_code_ids.keys t.newer_version_of_code_ids

let code_ids_and_newer_version_of_code_ids t =
  Code_id.Set.union (code_ids t) (newer_version_of_code_ids t)

let only_newer_version_of_code_ids t =
  Code_id.Set.diff (newer_version_of_code_ids t) (code_ids t)

let names t =
  Name.Set.union (Name.set_of_var_set (variables t))
    (Name.set_of_symbol_set (symbols t))

let mem_var t var = For_variables.mem t.variables var
let mem_symbol t symbol = For_symbols.mem t.symbols symbol
let mem_code_id t code_id = For_code_ids.mem t.code_ids code_id

let mem_name t (name : Name.t) =
  match name with
  | Var var -> mem_var t var
  | Symbol symbol -> mem_symbol t symbol

let remove_var t var =
  let variables = For_variables.remove t.variables var in
  { t with
    variables;
  }

let remove_vars t vars =
  Variable.Set.fold (fun var t -> remove_var t var) vars t

let greatest_name_mode_var t var =
  For_variables.greatest_name_mode t.variables var

let downgrade_occurrences_at_strictly_greater_kind
      { variables; continuations; symbols; closure_vars; code_ids;
        newer_version_of_code_ids; }
      max_kind =
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
  let closure_vars =
    For_closure_vars.downgrade_occurrences_at_strictly_greater_kind
      closure_vars max_kind
  in
  let code_ids =
    For_code_ids.downgrade_occurrences_at_strictly_greater_kind
      code_ids max_kind
  in
  let newer_version_of_code_ids =
    For_code_ids.downgrade_occurrences_at_strictly_greater_kind
      newer_version_of_code_ids max_kind
  in
  { variables;
    continuations;
    symbols;
    closure_vars;
    code_ids;
    newer_version_of_code_ids;
  }

let without_code_ids t =
  { t with
    code_ids = For_code_ids.empty;
    newer_version_of_code_ids = For_code_ids.empty;
  }
