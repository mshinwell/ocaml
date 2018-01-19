(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module F0 = Flambda0

module Of_kind_value = struct
  type t =
    | Symbol of Symbol.t
    | Tagged_immediate of Immediate.t
    | Dynamically_computed of Variable.t

  let compare (t1 : t) (t2 : t) =
    match t1, t2 with
    | Symbol s1, Symbol s2 -> Symbol.compare s1 s2
    | Tagged_immediate t1, Tagged_immediate t2 -> Immediate.compare t1 t2
    | Dynamically_computed v1, Dynamically_computed v2 -> Variable.compare v1 v2
    | Symbol _, Tagged_immediate _ -> -1
    | Tagged_immediate _, Symbol _ -> 1
    | Symbol _, Dynamically_computed _ -> -1
    | Dynamically_computed _, Symbol _ -> 1
    | Tagged_immediate _, Dynamically_computed _ -> -1
    | Dynamically_computed _, Tagged_immediate _ -> 1

  let print ppf (field : t) =
    match field with
    | Symbol symbol -> Symbol.print ppf symbol
    | Tagged_immediate immediate -> Immediate.print ppf immediate
    | Dynamically_computed var -> Variable.print ppf var

  let needs_gc_root t =
    match t with
    | Symbol _ | Tagged_immediate _ -> false
    | Dynamically_computed _ -> true

  let free_names t =
    match t with
    | Dynamically_computed var -> Name.Set.singleton (Name.var var)
    | Symbol sym -> Name.Set.singleton (Name.symbol sym)
    | Tagged_immediate _ -> Name.Set.empty
end

module Static_part = struct
  type 'a or_variable =
    | Const of 'a
    | Var of Variable.t

  type t =
    | Block of Tag.Scannable.t * Flambda0.mutable_or_immutable
        * (Of_kind_value.t list)
    | Fabricated_block of Variable.t
    | Set_of_closures of Flambda0.Set_of_closures.t
    | Closure of Symbol.t * Closure_id.t
    | Boxed_float of Numbers.Float_by_bit_pattern.t or_variable
    | Boxed_int32 of Int32.t or_variable
    | Boxed_int64 of Int64.t or_variable
    | Boxed_nativeint of Targetint.t or_variable
    | Mutable_float_array of
        { initial_value : Numbers.Float_by_bit_pattern.t or_variable list; }
    | Immutable_float_array of Numbers.Float_by_bit_pattern.t or_variable list
    | Mutable_string of { initial_value : string or_variable; }
    | Immutable_string of string or_variable

  let needs_gc_root t =
    match t with
    | Block (_tag, mut, fields) ->
      begin match mut with
      | Mutable -> true
      | Immutable -> List.exists Of_kind_value.needs_gc_root fields
      end
    | Fabricated_block _ -> true
    | Set_of_closures set ->
      not (Var_within_closure.Map.is_empty set.free_vars)
    | Closure _
    | Boxed_float _
    | Boxed_int32 _
    | Boxed_int64 _
    | Boxed_nativeint _
    | Mutable_float_array _
    | Immutable_float_array _
    | Mutable_string _
    | Immutable_string _ -> false

  let free_names t =
    match t with
    | Block (_tag, _mut, fields) ->
      List.fold_left (fun fvs field ->
          Name.Set.union fvs (Of_kind_value.free_names field))
        Name.Set.empty
        fields
    | Fabricated_block var -> Name.Set.singleton (Name.var var)
    | Set_of_closures set -> Flambda0.Set_of_closures.free_names set
    | Closure (sym, _) -> Name.Set.singleton (Name.symbol sym)
    | Boxed_float (Var v)
    | Boxed_int32 (Var v)
    | Boxed_int64 (Var v)
    | Boxed_nativeint (Var v)
    | Mutable_string { initial_value = Var v; }
    | Immutable_string (Var v) -> Name.Set.singleton (Name.var v)
    | Boxed_float (Const _)
    | Boxed_int32 (Const _)
    | Boxed_int64 (Const _)
    | Boxed_nativeint (Const _)
    | Mutable_string { initial_value = Const _; }
    | Immutable_string (Const _) -> Name.Set.empty
    | Mutable_float_array { initial_value = fields; }
    | Immutable_float_array fields ->
      List.fold_left (fun fns (field : _ or_variable) ->
          match field with
          | Var v -> Name.Set.add (Name.var v) fns
          | Const _ -> fns)
        Name.Set.empty
        fields

  let free_symbols t = Name.set_to_symbol_set (free_names t)

  let print ppf (t : t) =
    let fprintf = Format.fprintf in
    let print_float_array_field ppf = function
      | Const f -> fprintf ppf "%a" Numbers.Float_by_bit_pattern.print f
      | Var v -> Variable.print ppf v
    in
    match t with
    | Block (tag, mut, fields) ->
      fprintf ppf "@[(%sblock (tag %a) (%a))@]"
        (match mut with Immutable -> "Immutable_" | Mutable -> "Mutable_")
        Tag.Scannable.print tag
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
          Of_kind_value.print) fields
    | Fabricated_block field ->
      fprintf ppf "@[(Fabricated_block %a)@]"
        Variable.print field
    | Set_of_closures set_of_closures ->
      fprintf ppf "@[(Set_of_closures@ (%a))@]"
        F0.Set_of_closures.print set_of_closures
    | Closure (set_of_closures, closure_id) ->
      fprintf ppf "@[(Closure (set_of_closures %a) (closure_id %a))@]"
        Symbol.print set_of_closures
        Closure_id.print closure_id
    | Boxed_float (Const f) ->
      fprintf ppf "@[(Boxed_float %a)@]" Numbers.Float_by_bit_pattern.print f
    | Boxed_float (Var v) ->
      fprintf ppf "@[(Boxed_float %a)@]" Variable.print v
    | Boxed_int32 (Const n) ->
      fprintf ppf "@[(Boxed_int32 %ld)@]" n
    | Boxed_int32 (Var v) ->
      fprintf ppf "@[(Boxed_int32 %a)@]" Variable.print v
    | Boxed_int64 (Const n) ->
      fprintf ppf "@[(Boxed_int64 %Ld)@]" n
    | Boxed_int64 (Var v) ->
      fprintf ppf "@[(Boxed_int64 %a)@]" Variable.print v
    | Boxed_nativeint (Const n) ->
      fprintf ppf "@[(Boxed_nativeint %a)@]" Targetint.print n
    | Boxed_nativeint (Var v) ->
      fprintf ppf "@[(Boxed_nativeint %a)@]" Variable.print v
    | Mutable_float_array { initial_value; } ->
      fprintf ppf "@[(Mutable_float_array@ @[[| %a |]@])@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "@[; @]")
           print_float_array_field)
        initial_value
    | Immutable_float_array fields ->
      fprintf ppf "@[(Immutable_float_array@ @[[| %a |]@])@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "@[; ")
           print_float_array_field)
        fields
    | Mutable_string { initial_value = Const s; } ->
      fprintf ppf "@[(Mutable_string@ \"%s\")@]" s
    | Mutable_string { initial_value = Var v; } ->
      fprintf ppf "@[(Mutable_string@ %a)@]" Variable.print v
    | Immutable_string (Const s) ->
      fprintf ppf "@[(Immutable_string@ \"%s\")@]" s
    | Immutable_string (Var v) ->
      fprintf ppf "@[(Immutable_string@ %a)@]" Variable.print v
end

module Program_body = struct
  type computation = {
    expr : Flambda0.Expr.t;
    return_cont : Continuation.t;
    exception_cont : Continuation.t;
    computed_values : (Variable.t * Flambda_kind.t) list;
  }

  let print_computation ppf comp =
    Format.fprintf ppf "@[<2>(\
        @[(expr@ %a)@]@ \
        @[(return_cont@ %a)@]@ \
        @[(exception_cont@ %a)@]@ \
        @[(computed_values@ @[%a@])@])@]"
      Flambda0.Expr.print comp.expr
      Continuation.print comp.return_cont
      Continuation.print comp.exception_cont
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
        (fun ppf (var, kind) ->
          Format.fprintf ppf "@[(%a :: %a)@]"
            Variable.print var
            Flambda_kind.print kind))
      comp.computed_values

  let free_symbols_of_computation comp =
    Flambda0.Expr.free_symbols comp.expr

  type static_structure = (Symbol.t * Flambda_kind.t * Static_part.t) list

  type definition = {
    computation : computation option;
    static_structure : static_structure;
  }

  let print_definition ppf defn =
    Format.fprintf ppf "@[<v 2>(\
        @[(computation@ %a)@]@ \
        @[(static_structure@ @[(%a)@])@])@]"
      (Misc.Stdlib.Option.print print_computation)
      defn.computation
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
        (fun ppf (sym, kind, static_part) ->
          Format.fprintf ppf "@[((symbol %a)@ (kind %a)@ (static_part %a))@]"
            Symbol.print sym
            Flambda_kind.print kind
            Static_part.print static_part))
      defn.static_structure

  let free_symbols_of_definition defn (recursive : Flambda0.recursive) =
    let free_in_computation =
      match defn.computation with
      | None -> Symbol.Set.empty
      | Some computation -> free_symbols_of_computation computation
    in
    let being_defined =
      Symbol.Set.of_list (List.map (fun (sym, _, _) -> sym)
        defn.static_structure)
    in
    let bound_recursively =
      match recursive with
      | Non_recursive -> Symbol.Set.empty
      | Recursive -> being_defined
    in
    let free_in_static_parts =
      let symbols =
        List.fold_left (fun syms (_sym, _kind, static_part) ->
            Symbol.Set.union syms (Static_part.free_symbols static_part))
          Symbol.Set.empty
          defn.static_structure
      in
      Symbol.Set.diff symbols bound_recursively
    in
    Symbol.Set.union free_in_computation free_in_static_parts

  type t =
    | Define_symbol of definition * t
    | Define_symbol_rec of definition * t
    | Root of Symbol.t

  let rec print ppf t =
    match t with
    | Define_symbol (defn, t) ->
      Format.fprintf ppf "@[<v 2>(Define_symbol@ %a)@]@;"
        print_definition defn;
      print ppf t
    | Define_symbol_rec (defn, t) ->
      Format.fprintf ppf "@[<v 2>(Define_symbol_rec@ %a)@]@;"
        print_definition defn;
      print ppf t
    | Root sym ->
      Format.fprintf ppf "@[(Root %a)@]" Symbol.print sym

  let gc_roots t =
    let rec gc_roots t roots =
      match t with
      | Root _ -> roots
      | Define_symbol (defn, t) | Define_symbol_rec (defn, t) ->
        let roots =
          List.fold_left (fun roots (sym, _kind, static_part) ->
              (* CR mshinwell: check [kind] against the result of
                 [needs_gc_root] *)
              if Static_part.needs_gc_root static_part then
                Symbol.Set.add sym roots
              else
                roots)
            roots
            defn.static_structure
        in
        gc_roots t roots
    in
    gc_roots t Symbol.Set.empty

  let rec free_symbols t =
    match t with
    | Define_symbol (defn, t) ->
      Symbol.Set.union (free_symbols_of_definition defn Non_recursive)
        (free_symbols t)
    | Define_symbol_rec (defn, t) ->
      Symbol.Set.union (free_symbols_of_definition defn Recursive)
        (free_symbols t)
    | Root sym -> Symbol.Set.singleton sym
end

module Program = struct
  type t = {
    imported_symbols : Flambda_kind.t Symbol.Map.t;
    body : Program_body.t;
  }

  let print ppf t =
    Format.fprintf ppf "@[(@[(imported_symbols %a)@]@ @[<1>(body@ %a)@])@]"
      (Symbol.Map.print Flambda_kind.print) t.imported_symbols
      Program_body.print t.body

  let gc_roots t =
    let syms = Program_body.gc_roots t.body in
    if !Clflags.flambda_invariant_checks then begin
      Symbol.Set.iter (fun sym ->
          if not (Compilation_unit.equal (Compilation_unit.get_current_exn ())
            (Symbol.compilation_unit sym))
          then begin
            Misc.fatal_errorf "Symbol %a deemed as needing a GC root yet it \
                comes from another compilation unit"
              Symbol.print sym
          end)
        syms;
    end;
    syms

  let free_symbols t =
    (* N.B. [imported_symbols] are not treated as free. *)
    Program_body.free_symbols t.body
end
