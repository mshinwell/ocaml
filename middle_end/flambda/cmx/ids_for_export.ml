(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2020 OCamlPro SAS                                          *)
(*   Copyright 2020 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Const = Reg_width_things.Const
module Simple = Reg_width_things.Simple

type t = {
  symbols : Symbol.Set.t;
  variables : Variable.Set.t;
  simples : Simple.Set.t;
  consts : Const.Set.t;
  code_ids : Code_id.Set.t;
  continuations : Continuation.Set.t;
}

let empty = {
  symbols = Symbol.Set.empty;
  variables = Variable.Set.empty;
  simples = Simple.Set.empty;
  consts = Const.Set.empty;
  code_ids = Code_id.Set.empty;
  continuations = Continuation.Set.empty;
}

let create
    ?(symbols = Symbol.Set.empty)
    ?(variables = Variable.Set.empty)
    ?(simples = Simple.Set.empty)
    ?(consts = Const.Set.empty)
    ?(code_ids = Code_id.Set.empty)
    ?(continuations = Continuation.Set.empty)
    () =
  { symbols;
    variables;
    simples;
    consts;
    code_ids;
    continuations;
  }

let singleton_code_id code_id =
  create ~code_ids:(Code_id.Set.singleton code_id) ()

let singleton_continuation cont =
  create ~continuations:(Continuation.Set.singleton cont) ()

let singleton_symbol symbol =
  create ~symbols:(Symbol.Set.singleton symbol) ()

let add_const t const =
  { t with consts = Const.Set.add const t.consts }

let add_variable t var =
  { t with variables = Variable.Set.add var t.variables }

let add_symbol t sym =
  { t with symbols = Symbol.Set.add sym t.symbols }

let add_name t name =
  Name.pattern_match name
    ~var:(add_variable t)
    ~symbol:(add_symbol t)

let add_simple t simple =
  let simples =
    match Simple.rec_info simple with
    | None -> t.simples
    | Some _ -> Simple.Set.add simple t.simples
  in
  let t = { t with simples; } in
  Simple.pattern_match simple
    ~const:(add_const t)
    ~name:(add_name t)

let add_code_id t code_id =
  { t with code_ids = Code_id.Set.add code_id t.code_ids }

let add_continuation t continuation =
  { t with continuations = Continuation.Set.add continuation t.continuations }


let from_simple simple =
  let simples =
    match Simple.rec_info simple with
    | None ->
      (* This simple will not be in the grand_table_of_simples *)
      Simple.Set.empty
    | Some _ -> Simple.Set.singleton simple
  in
  Simple.pattern_match simple
    ~const:(fun const ->
      create ~simples
        ~consts:(Const.Set.singleton const)
        ())
    ~name:(fun name ->
      Name.pattern_match name
        ~var:(fun var ->
          create ~simples
            ~variables:(Variable.Set.singleton var)
            ())
        ~symbol:(fun sym ->
          create ~simples
            ~symbols:(Symbol.Set.singleton sym)
            ()))

let union t1 t2 =
  { symbols = Symbol.Set.union t1.symbols t2.symbols;
    variables = Variable.Set.union t1.variables t2.variables;
    simples = Simple.Set.union t1.simples t2.simples;
    consts = Const.Set.union t1.consts t2.consts;
    code_ids = Code_id.Set.union t1.code_ids t2.code_ids;
    continuations = Continuation.Set.union t1.continuations t2.continuations;
  }

let rec union_list ts =
  match ts with
  | [] -> empty
  | t::ts -> union t (union_list ts)

module Import_map = struct
  type t = {
    symbols : Symbol.t Symbol.Map.t;
    variables : Variable.t Variable.Map.t;
    simples : Simple.t Simple.Map.t;
    consts : Const.t Const.Map.t;
    code_ids : Code_id.t Code_id.Map.t;
    continuations : Continuation.t Continuation.Map.t;
    used_closure_vars : Var_within_closure.Set.t;
    (* CR vlaviron: [used_closure_vars] is here because we need to rewrite the
       types to remove occurrences of unused closure variables, as otherwise
       the types can contain references to code that is neither exported nor
       present in the actual object file. But this means rewriting types, and
       the only place a rewriting traversal is done at the moment is during
       import. This solution is not ideal because the missing code IDs will
       still be present in the emitted cmx files, and during the traversal
       in [Flambda_cmx.compute_reachable_names_and_code] we have to assume
       that code IDs can be missing (and so we cannot detect code IDs that
       are really missing at this point). *)
  }

  let has_no_action
        { symbols; variables; simples; consts; code_ids; continuations;
          used_closure_vars = _;
        } =
    Symbol.Map.is_empty symbols
    && Variable.Map.is_empty variables
    && Simple.Map.is_empty simples
    && Const.Map.is_empty consts
    && Code_id.Map.is_empty code_ids
    && Continuation.Map.is_empty continuations

  let is_empty
        { symbols; variables; simples; consts; code_ids; continuations;
          used_closure_vars;
        } =
    Symbol.Map.is_empty symbols
    && Variable.Map.is_empty variables
    && Simple.Map.is_empty simples
    && Const.Map.is_empty consts
    && Code_id.Map.is_empty code_ids
    && Continuation.Map.is_empty continuations
    && Var_within_closure.Set.is_empty used_closure_vars

  let create
      ~symbols
      ~variables
      ~simples
      ~consts
      ~code_ids
      ~continuations
      ~used_closure_vars =
    { symbols;
      variables;
      simples;
      consts;
      code_ids;
      continuations;
      used_closure_vars;
    }

  module Make_composition (T : Identifiable.S) = struct
    let compose ~second ~first =
      let mapped_by_first_and_maybe_by_second =
        T.Map.map (fun after_first_subst ->
            match T.Map.find after_first_subst second with
            | exception Not_found -> after_first_subst
            | after_both_substs -> after_both_substs)
          first
      in
      let image_of_first = T.Set.of_list (T.Map.data first) in
      let mapped_by_second_but_not_in_image_of_first =
        T.Map.filter (fun key _ -> not (T.Set.mem key image_of_first))
          second
      in
      T.Map.disjoint_union ~eq:(fun _ _ -> assert false)
        mapped_by_second_but_not_in_image_of_first
          mapped_by_first_and_maybe_by_second
  end

  module Symbol_composition = Make_composition (Symbol)
  module Variable_composition = Make_composition (Variable)
  module Simple_composition = Make_composition (Simple)
  module Const_composition = Make_composition (Const)
  module Code_id_composition = Make_composition (Code_id)
  module Continuation_composition = Make_composition (Continuation)

  let compose
        ~second:{
          symbols = symbols2;
          variables = variables2;
          simples = simples2;
          consts = consts2;
          code_ids = code_ids2;
          continuations = continuations2;
          used_closure_vars = used_closure_vars2;
        }
        ~first:{
          symbols = symbols1;
          variables = variables1;
          simples = simples1;
          consts = consts1;
          code_ids = code_ids1;
          continuations = continuations1;
          used_closure_vars = used_closure_vars1;
        } =
    { symbols =
        Symbol_composition.compose ~second:symbols2 ~first:symbols1;
      variables =
        Variable_composition.compose ~second:variables2 ~first:variables1;
      simples =
        Simple_composition.compose ~second:simples2 ~first:simples1;
      consts =
        Const_composition.compose ~second:consts2 ~first:consts1;
      code_ids =
        Code_id_composition.compose ~second:code_ids2 ~first:code_ids1;
      continuations =
        Continuation_composition.compose ~second:continuations2
          ~first:continuations1;
      used_closure_vars =
        Var_within_closure.Set.union used_closure_vars1 used_closure_vars2;
    }

  let symbol t orig =
    match Symbol.Map.find orig t.symbols with
    | symbol -> symbol
    | exception Not_found -> orig

  let variable t orig =
    match Variable.Map.find orig t.variables with
    | variable -> variable
    | exception Not_found -> orig

  let const t orig =
    match Const.Map.find orig t.consts with
    | const -> const
    | exception Not_found -> orig

  let code_id t orig =
    match Code_id.Map.find orig t.code_ids with
    | code_id -> code_id
    | exception Not_found -> orig

  let continuation t orig =
    match Continuation.Map.find orig t.continuations with
    | continuation -> continuation
    | exception Not_found -> orig

  let name t name =
    Name.pattern_match name
      ~var:(fun var -> Name.var (variable t var))
      ~symbol:(fun sym -> Name.symbol (symbol t sym))

  let simple t simple =
    (* [t.simples] only holds those [Simple]s with [Rec_info] (analogously
       to the grand table of [Simple]s, see reg_width_things.ml). *)
    match Simple.Map.find simple t.simples with
    | simple -> simple
    | exception Not_found -> simple

  let closure_var_is_used t var =
    Var_within_closure.Set.mem var t.used_closure_vars

  let apply_renaming
        { symbols;
          variables;
          simples;
          consts;
          code_ids;
          continuations;
          used_closure_vars;
        }
        ~rename_symbol ~rename_variable ~rename_simple
        ~rename_const ~rename_code_id ~rename_continuation =
    let symbols =
      Symbol.Map.fold (fun src target symbols ->
          Symbol.Map.add (rename_symbol src) (rename_symbol target)
            symbols)
        Symbol.Map.empty
        symbols
    in
    let variables =
      Variable.Map.fold (fun src target variables ->
          Variable.Map.add (rename_variable src) (rename_variable target)
            variables)
        Variable.Map.empty
        variables
    in
    let simples =
      Simple.Map.fold (fun src target simples ->
          Simple.Map.add (rename_simple src) (rename_simple target)
            simples)
        Simple.Map.empty
        simples
    in
    let consts =
      Const.Map.fold (fun src target consts ->
          Const.Map.add (rename_const src) (rename_const target)
            consts)
        Const.Map.empty
        consts
    in
    let code_ids =
      Code_id.Map.fold (fun src target code_ids ->
          Code_id.Map.add (rename_code_id src) (rename_code_id target)
            code_ids)
        Code_id.Map.empty
        code_ids
    in
    let continuations =
      Continuation.Map.fold (fun src target continuations ->
          Continuation.Map.add (rename_continuation src)
            (rename_continuation target) continuations)
        Continuation.Map.empty
        continuations
    in
    { symbols;
      variables;
      simples;
      consts;
      code_ids;
      continuations;
      used_closure_vars;
    }
end
