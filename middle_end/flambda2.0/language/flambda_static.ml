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

module K = Flambda_kind

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
    | Symbol sym -> Name_occurrences.singleton_symbol_in_terms sym
    | Tagged_immediate _ -> Name_occurrences.empty

  let invariant env t =
    let module E = Invariant_env in
    match t with
    | Symbol sym -> E.check_symbol_is_bound env sym
    | Tagged_immediate _ -> ()
    | Dynamically_computed var ->
      E.check_variable_is_bound_and_of_kind env var K.value
end

module Static_part = struct
  type 'a or_variable =
    | Const of 'a
    | Var of Variable.t

  type mutable_or_immutable = Mutable | Immutable

  type 'k t =
    | Block : Tag.Scannable.t * mutable_or_immutable
              * (Of_kind_value.t list) -> K.value t
    | Fabricated_block : Variable.t -> K.value t
    | Set_of_closures : Flambda.Set_of_closures.t -> K.fabricated t
    | Boxed_float : Numbers.Float_by_bit_pattern.t or_variable
                    -> K.value t
    | Boxed_int32 : Int32.t or_variable -> K.value t
    | Boxed_int64 : Int64.t or_variable -> K.value t
    | Boxed_nativeint : Targetint.t or_variable -> K.value t
    | Immutable_float_array : Numbers.Float_by_bit_pattern.t or_variable list
                              -> K.value t
    | Mutable_string : { initial_value : string or_variable; }
                       -> K.value t
    | Immutable_string : string or_variable -> K.value t

  let needs_gc_root (type k) (t : k t) =
    match t with
    | Block (_tag, mut, fields) ->
      begin match mut with
      | Mutable -> true
      | Immutable -> List.exists Of_kind_value.needs_gc_root fields
      end
    | Fabricated_block _ -> true
    | Set_of_closures set ->
      not (Flambda.Set_of_closures.has_empty_environment set)
    | Boxed_float _
    | Boxed_int32 _
    | Boxed_int64 _
    | Boxed_nativeint _
    | Immutable_float_array _
    | Mutable_string _
    | Immutable_string _ -> false

  let free_names (type k) (t : k t) =
    match t with
    | Block (_tag, _mut, fields) ->
      List.fold_left (fun fvs field ->
          Name_occurrences.union fvs (Of_kind_value.free_names field))
        (Name_occurrences.empty)
        fields
    | Fabricated_block v ->
      Name_occurrences.singleton_variable_in_terms v
    | Set_of_closures set -> Flambda.Set_of_closures.free_names set
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
    | Immutable_float_array fields ->
      List.fold_left (fun fns (field : _ or_variable) ->
          match field with
          | Var v -> Name_occurrences.add_variable_in_terms fns v
          | Const _ -> fns)
        (Name_occurrences.empty)
        fields

  let free_symbols t = Name_occurrences.symbols (free_names t)

  let free_variables t = Name_occurrences.variables (free_names t)

  let print_with_cache (type k) ~cache ppf (t : k t) =
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

  let _invariant (type k) env (t : k t) =
    try
      let module E = Invariant_env in
      match t with
      | Block (_tag, _mut, fields) ->
        List.iter (fun field -> Of_kind_value.invariant env field) fields
      | Fabricated_block field ->
        E.check_variable_is_bound_and_of_kind env field K.fabricated
      | Set_of_closures set ->
        Flambda.Set_of_closures.invariant env set
      | Boxed_float (Var v) ->
        E.check_variable_is_bound_and_of_kind env v K.naked_float
      | Boxed_int32 (Var v) ->
        E.check_variable_is_bound_and_of_kind env v K.naked_int32
      | Boxed_int64 (Var v) ->
        E.check_variable_is_bound_and_of_kind env v K.naked_int64
      | Boxed_nativeint (Var v) ->
        E.check_variable_is_bound_and_of_kind env v K.naked_nativeint
      | Mutable_string { initial_value = Var v; }
      | Immutable_string (Var v) ->
        E.check_variable_is_bound_and_of_kind env v K.value
      | Boxed_float (Const _)
      | Boxed_int32 (Const _)
      | Boxed_int64 (Const _)
      | Boxed_nativeint (Const _)
      | Mutable_string { initial_value = Const _; }
      | Immutable_string (Const _) -> ()
      | Immutable_float_array fields ->
        List.iter (fun (field : _ or_variable) ->
            match field with
            | Var v ->
              E.check_variable_is_bound_and_of_kind env v
                K.naked_float
            | Const _ -> ())
          fields
    with Misc.Fatal_error ->
      Misc.fatal_errorf "(during invariant checks) Context is:@ %a" print t
end

module Program_body = struct
  module Computation = struct
    type t = {
      expr : Flambda.Expr.t;
      return_continuation : Continuation.t;
      exn_continuation : Exn_continuation.t;
      computed_values : Kinded_parameter.t list;
    }

    let print ppf { expr; return_continuation; exn_continuation;
                    computed_values; } =
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(expr@ %a)@]@ \
          @[<hov 1>(return_continuation@ %a)@]@ \
          @[<hov 1>(exn_continuation@ %a)@]@ \
          @[<hov 1>(computed_values@ @[(%a)@])@])@]"
        Flambda.Expr.print expr
        Continuation.print return_continuation
        Exn_continuation.print exn_continuation
        Kinded_parameter.List.print computed_values

    let free_symbols t =
      Name_occurrences.symbols (Flambda.Expr.free_names t.expr)
  end

  module Bound_symbols = struct
    type 'k t =
      | Singleton : Symbol.t -> K.value t
      | Set_of_closures : {
          set_of_closures_symbol : Symbol.t;
          closure_symbols : Symbol.t Closure_id.Map.t;
        } -> K.fabricated t

    let print (type k) ppf (t : k t) =
      match t with
      | Singleton sym ->
        Format.fprintf ppf "@[(singleton@ (%a@ \u{2237}@ %a))@]"
          Symbol.print sym
          K.print K.value
      | Set_of_closures { set_of_closures_symbol; closure_symbols; } ->
        Format.fprintf ppf "@[(set_of_closures@ \
            (set_of_closures_symbol %a)@ \
            (closure_symbol %a)\
            )@]"
          Symbol.print set_of_closures_symbol
          (Closure_id.Map.print Symbol.print) closure_symbols

    (* CR mshinwell: This should have an [invariant] function.  One thing to
       check is that the [closure_symbols] are all distinct (and presumably
       different from the set of closure symbol too, even though one will
       eventually end up the same).
    *)

    let being_defined (type k) (t : k t) =
      match t with
      | Singleton sym -> Symbol.Set.singleton sym
      | Set_of_closures { set_of_closures_symbol; closure_symbols; } ->
        Symbol.Set.add set_of_closures_symbol
          (Symbol.Set.of_list (Closure_id.Map.data closure_symbols))

    let gc_roots (type k) (t : k t) =
      match t with
      | Singleton sym -> Symbol.Set.singleton sym
      | Set_of_closures { set_of_closures_symbol; closure_symbols = _; } ->
        (* Since all of the closures will be within the set of closures
           value, using [Infix_tag], the individual closure symbols do not
           need to be registered as roots. *)
        Symbol.Set.singleton set_of_closures_symbol
  end

  module Static_structure = struct
    type t =
      | S : ('k Bound_symbols.t * 'k Static_part.t) list -> t
      [@@unboxed]

    let print_with_cache ~cache ppf (S pieces) =
      Format.pp_print_list ~pp_sep:Format.pp_print_space
        (fun ppf (bound_symbols, static_part) ->
          Format.fprintf ppf "@[<hov 1>(\
              @[<hov 1>(bound_symbols@ %a)@]@ \
              @[<hov 1>(static_part@ %a)@]\
              )@]"
            Bound_symbols.print bound_symbols
            (Static_part.print_with_cache ~cache) static_part)
        ppf pieces

    let being_defined (S pieces) =
      List.fold_left (fun being_defined (bound_syms, _static_part) ->
          Symbol.Set.union (Bound_symbols.being_defined bound_syms)
            being_defined)
        Symbol.Set.empty
        pieces

    let free_symbols ((S pieces) as t) =
      let free_in_static_parts =
        List.fold_left (fun free_in_static_parts (_bound_syms, static_part) ->
            Symbol.Set.union (Static_part.free_symbols static_part)
              free_in_static_parts)
          Symbol.Set.empty
          pieces
      in
      Symbol.Set.diff free_in_static_parts (being_defined t)

    let free_variables (S pieces) =
      List.fold_left (fun free_in_static_parts (_bound_syms, static_part) ->
          Variable.Set.union (Static_part.free_variables static_part)
            free_in_static_parts)
        Variable.Set.empty
        pieces
  end

  module Definition = struct
    type t = {
      computation : Computation.t option;
      static_structure : Static_structure.t;
    }

    let print_with_cache ~cache ppf { computation; static_structure; } =
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(computation@ %a)@]@ \
          @[<hov 1>(static_structure@ @[<hov 1>(%a)@])@])@]"
        (Misc.Stdlib.Option.print Computation.print) computation
        (Static_structure.print_with_cache ~cache) static_structure

    let free_symbols t =
      let free_in_computation =
        match t.computation with
        | None -> Symbol.Set.empty
        | Some computation -> Computation.free_symbols computation
      in
      let free_in_static_structure =
        Static_structure.free_symbols t.static_structure
      in
      Symbol.Set.union free_in_computation free_in_static_structure
  end

  type t =
    | Define_symbol of Definition.t * t
    | Root of Symbol.t

  let rec print_with_cache ~cache ppf t =
    match t with
    | Define_symbol (defn, t) ->
      Format.fprintf ppf "@[<v 2>(@<0>%sDefine_symbol@<0>%s@ %a)@]@;"
        (Misc.Color.bold_yellow ())
        (Misc.Color.reset ())
        (Definition.print_with_cache ~cache) defn;
      print_with_cache ~cache ppf t
    | Root sym ->
      Format.fprintf ppf "@[(@<0>%sRoot@<0>%s %a)@]"
        (Misc.Color.bold_yellow ())
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
          match defn.static_structure with
          | S pieces ->
            List.fold_left (fun roots (bound_symbols, static_part) ->
                (* CR mshinwell: check [kind] against the result of
                   [needs_gc_root] *)
                if Static_part.needs_gc_root static_part then
                  Symbol.Set.union (Bound_symbols.gc_roots bound_symbols) roots
                else
                  roots)
              roots
              pieces
        in
        gc_roots t roots
    in
    gc_roots t Symbol.Set.empty

  let rec free_symbols t =
    match t with
    | Define_symbol (defn, t) ->
      Symbol.Set.union (Definition.free_symbols defn) (free_symbols t)
    | Root sym -> Symbol.Set.singleton sym

  let _invariant _env _t = ()
end

module Program = struct
  type t = {
    imported_symbols : K.t Symbol.Map.t;
    body : Program_body.t;
  }

  let print ppf t =
    Format.fprintf ppf "@[(@[(imported_symbols %a)@]@ @[<1>(body@ %a)@])@]"
      (Symbol.Map.print K.print) t.imported_symbols
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
end
