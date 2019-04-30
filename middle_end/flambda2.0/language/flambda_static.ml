(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

let fprintf = Format.fprintf

module Of_kind_value = struct
  type t =
    | Symbol of Symbol.t
    | Tagged_immediate of Immediate.t
    | Dynamically_computed of Variable.t

  include Identifiable.Make (struct
    type nonrec t = t

    let compare t1 t2 =
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

    let equal t1 t2 =
      compare t1 t2 = 0

    let hash t =
      match t with
      | Symbol symbol -> Hashtbl.hash (0, Symbol.hash symbol)
      | Tagged_immediate immediate ->
        Hashtbl.hash (1, Immediate.hash immediate)
      | Dynamically_computed var -> Hashtbl.hash (2, Variable.hash var)

    let print ppf t =
      match t with
      | Symbol symbol -> Symbol.print ppf symbol
      | Tagged_immediate immediate -> Immediate.print ppf immediate
      | Dynamically_computed var -> Variable.print ppf var

    let output chan t =
      print (Format.formatter_of_out_channel chan) t
  end)

  let needs_gc_root t =
    match t with
    | Symbol _ | Tagged_immediate _ -> false
    | Dynamically_computed _ -> true

  let free_names t =
    match t with
    | Dynamically_computed var ->
      Name_occurrences.singleton_variable_in_terms var
    | Symbol sym -> Name_occurrences.singleton_symbol sym
    | Tagged_immediate _ -> Name_occurrences.empty

  let invariant env t =
    let module E = Invariant_env in
    match t with
    | Symbol sym -> E.check_symbol_is_bound env sym
    | Tagged_immediate _ -> ()
    | Dynamically_computed var ->
      E.check_variable_is_bound_and_of_kind env var (Flambda_kind.value ())
end

module Static_part = struct
  type 'a or_variable =
    | Const of 'a
    | Var of Variable.t

  type mutable_or_immutable = Mutable | Immutable

  type t =
    | Block of Tag.Scannable.t * mutable_or_immutable * (Of_kind_value.t list)
    | Fabricated_block of Variable.t
    | Set_of_closures of Flambda.Set_of_closures.t
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
      not (Flambda.Set_of_closures.has_empty_environment set)
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
          Name_occurrences.union fvs (Of_kind_value.free_names field))
        (Name_occurrences.empty)
        fields
    | Fabricated_block v ->
      Name_occurrences.singleton_variable_in_terms v
    | Set_of_closures set -> Flambda.Set_of_closures.free_names set
    | Closure (sym, _) ->
      Name_occurrences.singleton_symbol sym
    | Boxed_float (Var v)
    | Boxed_int32 (Var v)
    | Boxed_int64 (Var v)
    | Boxed_nativeint (Var v)
    | Mutable_string { initial_value = Var v; }
    | Immutable_string (Var v) ->
      Name_occurrences.singleton_variable_in_terms v
    | Boxed_float (Const _)
    | Boxed_int32 (Const _)
    | Boxed_int64 (Const _)
    | Boxed_nativeint (Const _)
    | Mutable_string { initial_value = Const _; }
    | Immutable_string (Const _) -> Name_occurrences.empty
    | Mutable_float_array { initial_value = fields; }
    | Immutable_float_array fields ->
      List.fold_left (fun fns (field : _ or_variable) ->
          match field with
          | Var v -> Name_occurrences.add_variable_in_terms fns v
          | Const _ -> fns)
        (Name_occurrences.empty)
        fields

  let free_symbols t = Name_occurrences.symbols (free_names t)

  let print_with_cache ~cache ppf (t : t) =
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
        (Flambda.Set_of_closures.print_with_cache ~cache) set_of_closures
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

  let print ppf t =
    print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let _invariant env t =
    try
      let module E = Invariant_env in
      match t with
      | Block (_tag, _mut, fields) ->
        List.iter (fun field -> Of_kind_value.invariant env field) fields
      | Fabricated_block field ->
        E.check_variable_is_bound_and_of_kind env field
          (Flambda_kind.fabricated ())
      | Set_of_closures set ->
        Flambda.Set_of_closures.invariant env set
      | Closure (sym, _closure_id) ->
        E.check_symbol_is_bound env sym
      | Boxed_float (Var v) ->
        E.check_variable_is_bound_and_of_kind env v
          (Flambda_kind.naked_float ())
      | Boxed_int32 (Var v) ->
        E.check_variable_is_bound_and_of_kind env v
          (Flambda_kind.naked_int32 ())
      | Boxed_int64 (Var v) ->
        E.check_variable_is_bound_and_of_kind env v
          (Flambda_kind.naked_int64 ())
      | Boxed_nativeint (Var v) ->
        E.check_variable_is_bound_and_of_kind env v
          (Flambda_kind.naked_nativeint ())
      | Mutable_string { initial_value = Var v; }
      | Immutable_string (Var v) ->
        E.check_variable_is_bound_and_of_kind env v
          (Flambda_kind.value ())
      | Boxed_float (Const _)
      | Boxed_int32 (Const _)
      | Boxed_int64 (Const _)
      | Boxed_nativeint (Const _)
      | Mutable_string { initial_value = Const _; }
      | Immutable_string (Const _) -> ()
      | Mutable_float_array { initial_value = fields; }
      | Immutable_float_array fields ->
        List.iter (fun (field : _ or_variable) ->
            match field with
            | Var v ->
              E.check_variable_is_bound_and_of_kind env v
                (Flambda_kind.naked_float ())
            | Const _ -> ())
          fields
    with Misc.Fatal_error ->
      Misc.fatal_errorf "(during invariant checks) Context is:@ %a" print t
end

module Program_body = struct
  type computation = {
    expr : Flambda.Expr.t;
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
      Flambda.Expr.print comp.expr
      Continuation.print comp.return_cont
      Continuation.print comp.exception_cont
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
        (fun ppf (var, kind) ->
          Format.fprintf ppf "@[(%a :: %a)@]"
            Variable.print var
            Flambda_kind.print kind))
      comp.computed_values

  let free_symbols_of_computation comp =
    Name_occurrences.symbols (Flambda.Expr.free_names comp.expr)

  type bound_symbols =
    | Singleton of Symbol.t * Flambda_kind.t
      (** A binding of a single symbol of the given kind. *)
    | Set_of_closures of {
        set_of_closures_symbol : Symbol.t;
        closure_symbols : Symbol.t Closure_id.Map.t;
      }
      (** A binding of a single symbol to a set of closures together with
          the binding of possibly multiple symbols to the individual closures
          within such set of closures. *)

  type static_structure = (bound_symbols * Static_part.t) list

  type definition = {
    computation : computation option;
    static_structure : static_structure;
  }

  let print_definition_with_cache ~cache ppf defn =
    Format.fprintf ppf "@[<v 2>(\
        @[(computation@ %a)@]@ \
        @[(static_structure@ @[(%a)@])@])@]"
      (Misc.Stdlib.Option.print print_computation)
      defn.computation
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
        (fun ppf (bound_symbols, static_part) ->
          Format.fprintf ppf "@[((symbol %a)@ (kind %a)@ (static_part@ %a))@]"
            print_bound_symbols bound_symbols
            Flambda_kind.print kind
            (Static_part.print_with_cache ~cache) static_part))
      defn.static_structure

  let free_symbols_of_definition defn (recursive : Recursive.t) =
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
    | Root of Symbol.t

  let rec print_with_cache ~cache ppf t =
    match t with
    | Define_symbol (defn, t) ->
      Format.fprintf ppf "@[<v 2>(%sDefine_symbol%s@ %a)@]@;"
        (Misc.Color.bold_blue ())
        (Misc.Color.reset ())
        (print_definition_with_cache ~cache) defn;
      print_with_cache ~cache ppf t
    | Root sym ->
      Format.fprintf ppf "@[(%sRoot%s %a)@]"
        (Misc.Color.bold_blue ())
        (Misc.Color.reset ())
        Symbol.print sym

  let print ppf t =
    print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let gc_roots t =
    let rec gc_roots t roots =
      match t with
      | Root _ -> roots
      | Define_symbol (defn, t) ->
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
    | Root sym -> Symbol.Set.singleton sym

(* To be re-enabled
  let invariant_define_symbol env defn (recursive : Flambda.recursive) =
    let module E = Invariant_env in
    begin match defn.computation with
    | None -> ()
    | Some computation ->
      (* [Flambda.Expr.invariant] will also catch unbound variables and
         continuations, but we can give a better error message by having
         these two specific checks.  It also gives some amount of testing
         of [free_variables] and [free_continuations] in [Expr]. *)
      let free_variables =
        Name.set_to_var_set (Name_occurrences.everything (
          Flambda.Expr.free_names computation.expr))
      in
      if not (Variable.Set.is_empty free_variables) then begin
        Misc.fatal_errorf "Toplevel computation is not closed (free \
            variable(s) %a):@ %a"
          Variable.Set.print free_variables
          Flambda.Expr.print computation.expr
      end;
      let free_conts = Flambda.Expr.free_continuations computation.expr in
      if not (Continuation.Set.is_empty free_conts) then begin
        let allowed =
          Continuation.Set.of_list [
            computation.return_cont;
            computation.exception_cont;
          ]
        in
        let remaining = Continuation.Set.diff free_conts allowed in
        if not (Continuation.Set.is_empty remaining) then begin
          Misc.fatal_errorf "Toplevel computation has illegal free \
              continuation(s) %a;@ the only permitted free continuations are \
              the return continuation, %a@ and the exception continuation, %a"
            Continuation.Set.print free_conts
            Continuation.print computation.return_cont
            Continuation.print computation.exception_cont
        end
      end;
      let computation_env =
        let return_arity =
          List.map (fun (_var, kind) -> kind) computation.computed_values
        in
        E.add_continuation env
          computation.return_cont
          return_arity
          Normal
          (E.Continuation_stack.var ())
      in
      Flambda.Expr.invariant computation_env computation.expr;
      List.iter (fun (var, _kind) ->
          if Invariant_env.variable_is_bound env var then begin
            Misc.fatal_errorf "[computed_values] of a toplevel computation \
                must contain fresh variables.@ %a is not fresh.@ \
                Computation:@ %a"
              Variable.print var
              Flambda.Expr.print computation.expr;
          end)
        computation.computed_values
    end;
    (* CR mshinwell: lwhite/xclerc have some fixes on the old Flambda in this
       area which we should port forward *)
    let env =
      match recursive with
      | Non_recursive -> env
      | Recursive ->
        List.fold_left (fun env (sym, kind, _static_part) ->
            E.add_symbol env sym kind)
          env
          defn.static_structure
    in
    let allowed_fns =
      match defn.computation with
      | None -> Name.Set.empty
      | Some computation ->
        Name.Set.of_list (
          List.map (fun (var, _kind) -> Name.var var)
            computation.computed_values)
    in
    let _allowed_fns = Name_occurrences.create_from_set_in_terms allowed_fns in
    let static_part_env =
      match defn.computation with
      | None -> env
      | Some computation ->
        List.fold_left (fun static_part_env (var, kind) ->
            E.add_variable static_part_env var kind)
          env
          computation.computed_values
    in
    List.iter (fun (_sym, _kind, static_part) ->
        let _free_names = Static_part.free_names static_part in
(* XXX This is broken -- isn't allowing previously-defined symbols
        (* This will also be caught by [invariant_static_part], but will
           give a better message; and allows some testing of
           [Static_part.free_variables]. *)
        if not (Name_occurrences.subset free_names allowed_fns) then begin
          Misc.fatal_errorf "Static part is only allowed to reference \
              the following free names: { %a }, whereas it references \
              { %a }.  Static part:@ %a = %a"
            Name_occurrences.print allowed_fns
            Name_occurrences.print free_names
            Symbol.print sym
            Static_part.print static_part
        end;
*)
        Static_part.invariant static_part_env static_part)
      defn.static_structure;
    List.fold_left (fun env (sym, kind, _static_part) ->
        match recursive with
        | Non_recursive -> E.add_symbol env sym kind
        | Recursive ->
          (* If we ever store data about symbols, this place needs updating
             to do a "redefine_symbol" operation on [env]. *)
          env)
      env
      defn.static_structure

  let rec invariant env t =
    let module E = Invariant_env in
    match t with
    | Define_symbol (defn, t) ->
      let env = invariant_define_symbol env defn Non_recursive in
      invariant env t
    | Root sym -> E.check_symbol_is_bound env sym
*)

  let _invariant _env _t = ()
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

  let imported_symbols t = t.imported_symbols

  let root_symbol t =
    let rec loop (body : Program_body.t) =
      match body with
      | Define_symbol (_, body) -> loop body
      | Root root -> root
    in
    loop t.body

  let invariant _t = ()
(* To be re-enabled
    let module E = Invariant_env in
    let every_used_function_from_current_unit_is_declared env t =
      let current_compilation_unit = Compilation_unit.get_current_exn () in
      let not_declared = E.closure_ids_not_declared env in
      let not_declared_from_current_unit =
        Closure_id.Set.filter (fun cu ->
            Closure_id.in_compilation_unit cu current_compilation_unit)
          not_declared
      in
      if not (Closure_id.Set.is_empty not_declared_from_current_unit) then begin
        Misc.fatal_errorf "Closure ID(s) { %a } from the current compilation \
            unit is/are referenced but not declared:@ %a"
          Closure_id.Set.print not_declared
          print t
      end
    in
    let every_used_var_within_closure_from_current_unit_is_declared env t =
      let current_compilation_unit = Compilation_unit.get_current_exn () in
      let not_declared = E.var_within_closures_not_declared env in
      let not_declared_from_current_unit =
        Var_within_closure.Set.filter (fun cu ->
            Var_within_closure.in_compilation_unit cu current_compilation_unit)
          not_declared
      in
      if not (Var_within_closure.Set.is_empty not_declared_from_current_unit)
      then begin
        Misc.fatal_errorf "Closure ID(s) { %a } from the current compilation \
            unit is/are referenced but not declared:@ %a"
          Var_within_closure.Set.print not_declared
          print t
      end
    in
    let env =
      Symbol.Map.fold (fun symbol kind env ->
          E.add_symbol env symbol kind)
        t.imported_symbols
        (E.create ())
    in
    Program_body.invariant env t.body;
    every_used_function_from_current_unit_is_declared env t;
    every_used_var_within_closure_from_current_unit_is_declared env t
*)
end
