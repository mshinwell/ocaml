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

  let free_names t =
    match t with
    | Dynamically_computed var ->
      Name_occurrences.singleton_variable var Name_mode.normal
    | Symbol sym ->
      Name_occurrences.singleton_symbol sym Name_mode.normal
    | Tagged_immediate _ -> Name_occurrences.empty

(*
  let invariant env t =
    let module E = Invariant_env in
    match t with
    | Symbol sym -> E.check_symbol_is_bound env sym
    | Tagged_immediate _ -> ()
    | Dynamically_computed var ->
      E.check_variable_is_bound_and_of_kind env var K.value
*)
end

module Static_part = struct
  type 'a or_variable =
    | Const of 'a
    | Var of Variable.t

  type mutable_or_immutable = Mutable | Immutable

  type code = {
    params_and_body : Flambda.Function_params_and_body.t or_deleted;
    newer_version_of : Code_id.t option;
  }
  and 'a or_deleted =
    | Present of 'a
    | Deleted

  type code_and_set_of_closures = {
    code : code Code_id.Map.t;
    set_of_closures : Flambda.Set_of_closures.t option;
  }

  type 'k t =
    | Block : Tag.Scannable.t * mutable_or_immutable
              * (Of_kind_value.t list) -> K.value t
    | Fabricated_block : Variable.t -> K.value t
      (* CR mshinwell: This used to say K.fabricated.  Use a different
         index from [K.t]? *)
    | Code_and_set_of_closures : code_and_set_of_closures
        -> Flambda_kind.fabricated t
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

  let get_pieces_of_code (type k) (t : k t) =
    match t with
    | Code_and_set_of_closures { code; set_of_closures = _; } ->
      Code_id.Map.filter_map code
        ~f:(fun _code_id { params_and_body; newer_version_of; } ->
          match params_and_body with
          | Present params_and_body -> Some (params_and_body, newer_version_of)
          | Deleted -> None)
    | Block _
    | Fabricated_block _
    | Boxed_float _
    | Boxed_int32 _
    | Boxed_int64 _
    | Boxed_nativeint _
    | Immutable_float_array _
    | Mutable_string _
    | Immutable_string _ -> Code_id.Map.empty

  let free_names (type k) (t : k t) =
    match t with
    | Block (_tag, _mut, fields) ->
      List.fold_left (fun fvs field ->
          Name_occurrences.union fvs (Of_kind_value.free_names field))
        (Name_occurrences.empty)
        fields
    | Fabricated_block v ->
      Name_occurrences.singleton_variable v Name_mode.normal
    | Code_and_set_of_closures { code; set_of_closures; } ->
      let from_set_of_closures =
        match set_of_closures with
        | None -> Name_occurrences.empty
        | Some set -> Flambda.Set_of_closures.free_names set
      in
      Code_id.Map.fold
        (fun code_id { params_and_body; newer_version_of; } free_names ->
          let from_newer_version_of =
            match newer_version_of with
            | None -> Name_occurrences.empty
            | Some older ->
              Name_occurrences.add_newer_version_of_code_id
                Name_occurrences.empty older Name_mode.normal
          in
          let from_params_and_body =
            match params_and_body with
            | Deleted -> Name_occurrences.empty
            | Present params_and_body ->
              Flambda.Function_params_and_body.free_names params_and_body
          in
          Name_occurrences.union_list [
            (Name_occurrences.add_code_id Name_occurrences.empty
              code_id Name_mode.normal);
            from_params_and_body;
            from_newer_version_of;
            free_names;
          ])
        code
        from_set_of_closures
    | Boxed_float (Var v)
    | Boxed_int32 (Var v)
    | Boxed_int64 (Var v)
    | Boxed_nativeint (Var v)
    | Mutable_string { initial_value = Var v; }
    | Immutable_string (Var v) ->
      Name_occurrences.singleton_variable v Name_mode.normal
    | Boxed_float (Const _)
    | Boxed_int32 (Const _)
    | Boxed_int64 (Const _)
    | Boxed_nativeint (Const _)
    | Mutable_string { initial_value = Const _; }
    | Immutable_string (Const _) -> Name_occurrences.empty
    | Immutable_float_array fields ->
      List.fold_left (fun fns (field : _ or_variable) ->
          match field with
          | Var v ->
            Name_occurrences.add_variable fns v Name_mode.normal
          | Const _ -> fns)
        (Name_occurrences.empty)
        fields

  let print_params_and_body_with_cache ~cache ppf params_and_body =
    match params_and_body with
    | Deleted -> Format.fprintf ppf "@[<hov 1>(params_and_body@ Deleted)@]"
    | Present params_and_body ->
      Flambda.Function_params_and_body.print_with_cache ~cache ppf
        params_and_body

  let print_code_with_cache ~cache ppf { params_and_body; newer_version_of; } =
    (* CR mshinwell: elide "newer_version_of" when None *)
    Format.fprintf ppf "@[<hov 1>(\
        @[(newer_version_of@ %a)@]@ \
        %a\
        )@]"
      (Misc.Stdlib.Option.print Code_id.print) newer_version_of
      (print_params_and_body_with_cache ~cache) params_and_body

  let print_with_cache (type k) ~cache ppf (t : k t) =
    let print_float_array_field ppf = function
      | Const f -> fprintf ppf "%a" Numbers.Float_by_bit_pattern.print f
      | Var v -> Variable.print ppf v
    in
    match t with
    | Block (tag, mut, fields) ->
      fprintf ppf "@[<hov 1>(@<0>%s%sblock@<0>%s (tag %a) (%a))@]"
        (Flambda_colours.static_part ())
        (match mut with Immutable -> "Immutable_" | Mutable -> "Mutable_")
        (Flambda_colours.normal ())
        Tag.Scannable.print tag
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
          Of_kind_value.print) fields
    | Fabricated_block field ->
      fprintf ppf "@[<hov 1>@<0>%sFabricated_block@<0>%s %a)@]"
        (Flambda_colours.static_part ())
        (Flambda_colours.normal ())
        Variable.print field
    | Code_and_set_of_closures { code; set_of_closures; } ->
      fprintf ppf "@[<hov 1>(@<0>%sCode_and_set_of_closures@<0>%s@ (\
          @[<hov 1>(code@ (%a))@]@ \
          @[<hov 1>(set_of_closures@ (%a))@]\
          ))@]"
        (Flambda_colours.static_part ())
        (Flambda_colours.normal ())
        (Code_id.Map.print (print_code_with_cache ~cache)) code
        (Misc.Stdlib.Option.print
          (Flambda.Set_of_closures.print_with_cache ~cache))
          set_of_closures
    | Boxed_float (Const f) ->
      fprintf ppf "@[@<0>%sBoxed_float@<0>%s %a)@]"
        (Flambda_colours.static_part ())
        (Flambda_colours.normal ())
        Numbers.Float_by_bit_pattern.print f
    | Boxed_float (Var v) ->
      fprintf ppf "@[@<0>%sBoxed_float@<0>%s %a)@]"
        (Flambda_colours.static_part ())
        (Flambda_colours.normal ())
        Variable.print v
    | Boxed_int32 (Const n) ->
      fprintf ppf "@[@<0>%sBoxed_int32@<0>%s %ld)@]"
        (Flambda_colours.static_part ())
        (Flambda_colours.normal ())
        n
    | Boxed_int32 (Var v) ->
      fprintf ppf "@[@<0>%sBoxed_int32@<0>%s %a)@]"
        (Flambda_colours.static_part ())
        (Flambda_colours.normal ())
        Variable.print v
    | Boxed_int64 (Const n) ->
      fprintf ppf "@[@<0>%sBoxed_int64@<0>%s %Ld)@]"
        (Flambda_colours.static_part ())
        (Flambda_colours.normal ())
        n
    | Boxed_int64 (Var v) ->
      fprintf ppf "@[@<0>%sBoxed_int64@<0>%s %a)@]"
        (Flambda_colours.static_part ())
        (Flambda_colours.normal ())
        Variable.print v
    | Boxed_nativeint (Const n) ->
      fprintf ppf "@[@<0>%sBoxed_nativeint@<0>%s %a)@]"
        (Flambda_colours.static_part ())
        (Flambda_colours.normal ())
        Targetint.print n
    | Boxed_nativeint (Var v) ->
      fprintf ppf "@[@<0>%sBoxed_nativeint@<0>%s %a)@]"
        (Flambda_colours.static_part ())
        (Flambda_colours.normal ())
        Variable.print v
    | Immutable_float_array fields ->
      fprintf ppf "@[@<0>%sImmutable_float_array@<0>%s@ @[[| %a |]@])@]"
        (Flambda_colours.static_part ())
        (Flambda_colours.normal ())
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "@; ")
           print_float_array_field)
        fields
    | Mutable_string { initial_value = Const s; } ->
      fprintf ppf "@[@<0>%sMutable_string@<0>%s@ \"%s\")@]"
        (Flambda_colours.static_part ())
        (Flambda_colours.normal ())
        s
    | Mutable_string { initial_value = Var v; } ->
      fprintf ppf "@[@<0>%sMutable_string@<0>%s@ %a)@]"
        (Flambda_colours.static_part ())
        (Flambda_colours.normal ())
        Variable.print v
    | Immutable_string (Const s) ->
      fprintf ppf "@[@<0>%sImmutable_string@<0>%s@ \"%s\")@]"
        (Flambda_colours.static_part ())
        (Flambda_colours.normal ())
        s
    | Immutable_string (Var v) ->
      fprintf ppf "@[@<0>%sImmutable_string@<0>%s@ %a)@]"
        (Flambda_colours.static_part ())
        (Flambda_colours.normal ())
        Variable.print v

  let print ppf t =
    print_with_cache ~cache:(Printing_cache.create ()) ppf t

(*
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
*)
end

type static_part_iterator = {
  f : 'k. ('k Static_part.t -> unit);
}

type static_part_mapper = {
  f : 'k. ('k Static_part.t -> 'k Static_part.t);
}

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
          @[<hov 1>(computed_values@ (%a))@]\
          )@]"
        Flambda.Expr.print expr
        Continuation.print return_continuation
        Exn_continuation.print exn_continuation
        Kinded_parameter.List.print computed_values

    let free_names t = Flambda.Expr.free_names t.expr

    let iter_expr t ~f = f t.expr

    let map_expr t ~f =
      { t with
        expr = f t.expr;
      }
  end

  module Bound_symbols = struct
    type 'k t =
      | Singleton : Symbol.t -> K.value t
      | Code_and_set_of_closures : {
          code_ids : Code_id.Set.t;
          closure_symbols : Symbol.t Closure_id.Map.t;
        } -> K.fabricated t

    (* CR mshinwell: Share with [Bindable_let_bound] *)
    let print_closure_binding ppf (closure_id, sym) =
      Format.fprintf ppf "@[(%a \u{21a6} %a)@]"
        Closure_id.print closure_id
        Symbol.print sym

    let print (type k) ppf (t : k t) =
      match t with
      | Singleton sym ->
        Format.fprintf ppf "@[%a@ \u{2237}@ %a@ @<0>%s=@<0>%s@ @]"
          Symbol.print sym
          K.print K.value
          (Flambda_colours.elide ())
          (Flambda_colours.normal ())
      | Code_and_set_of_closures { code_ids; closure_symbols; } ->
        Format.fprintf ppf "@[<hov 1>\
            @[<hov 1>(code_ids@ %a)@]@ \
            @[<hov 1>(closure_symbols@ {%a})@]\
            @]"
          Code_id.Set.print code_ids
          (Format.pp_print_list ~pp_sep:Format.pp_print_space
            print_closure_binding)
          (Closure_id.Map.bindings closure_symbols)

    (* CR mshinwell: This should have an [invariant] function.  One thing to
       check is that the [closure_symbols] are all distinct. *)

    let being_defined (type k) (t : k t) =
      match t with
      | Singleton sym -> Symbol.Set.singleton sym
      | Code_and_set_of_closures { code_ids = _; closure_symbols; } ->
        Symbol.Set.of_list (Closure_id.Map.data closure_symbols)

    let code_being_defined (type k) (t : k t) =
      match t with
      | Singleton _ -> Code_id.Set.empty
      | Code_and_set_of_closures { code_ids; closure_symbols = _; } -> code_ids
  end

  module Static_structure = struct
    type t0 =
      | S : 'k Bound_symbols.t * 'k Static_part.t -> t0

    let print_t0_with_cache ~cache ppf (S (bound_symbols, static_part)) =
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>%a@]@ \
          @[<hov 1>(%a)@]\
          )@]"
        Bound_symbols.print bound_symbols
        (Static_part.print_with_cache ~cache) static_part

    type t = t0 list

    let print_with_cache ~cache ppf t =
      Format.pp_print_list ~pp_sep:Format.pp_print_space
        (print_t0_with_cache ~cache) ppf t

    let print ppf t =
      print_with_cache ~cache:(Printing_cache.create ()) ppf t

    let is_empty t =
      match t with
      | [] -> true
      | _::_ -> false

    let being_defined t =
      List.fold_left (fun being_defined (S (bound_syms, _static_part)) ->
          Symbol.Set.union (Bound_symbols.being_defined bound_syms)
            being_defined)
        Symbol.Set.empty
        t

    let code_being_defined t =
      List.fold_left (fun code_being_defined (S (bound_syms, _static_part)) ->
          Code_id.Set.union (Bound_symbols.code_being_defined bound_syms)
            code_being_defined)
        Code_id.Set.empty
        t

    let free_names t =
      List.fold_left (fun free_names (S (_bound_syms, static_part)) ->
          Name_occurrences.union free_names
            (Static_part.free_names static_part))
        Name_occurrences.empty
        t

    let delete_bindings t ~free_names_after code_age_relation =
      let rec delete t ~free_names_after ~result =
        match t with
        | [] -> result
        | ((S (bound_syms, static_part)) as part)::t ->
          let symbols_after = Name_occurrences.symbols free_names_after in
          let can_delete_symbols =
            Symbol.Set.is_empty (
              Symbol.Set.inter (Bound_symbols.being_defined bound_syms)
                symbols_after)
          in
          let code_ids_being_defined =
            Bound_symbols.code_being_defined bound_syms
          in
          let code_ids_after = Name_occurrences.code_ids free_names_after in
          let code_unused =
            Code_id.Set.is_empty
              (Code_id.Set.inter code_ids_being_defined code_ids_after)
          in
          let can_delete_code =
            code_unused
              &&
              Code_id.Set.for_all (fun code_id ->
                  Code_age_relation.newer_versions_form_linear_chain
                    code_age_relation code_id)
                code_ids_being_defined
          in
          let free_names_after =
            Name_occurrences.union free_names_after
              (Static_part.free_names static_part)
          in
          if can_delete_code && can_delete_symbols then
            delete t ~free_names_after ~result
          else
            delete t ~free_names_after ~result:(part :: result)
      in
      delete (List.rev t) ~free_names_after ~result:[]

    let iter_static_parts t (iter : static_part_iterator) =
      List.iter (fun (S (_bound_syms, static_part)) ->
          iter.f static_part)
        t

    let map_static_parts t (mapper : static_part_mapper) =
      List.map (fun (S (bound_syms, static_part)) ->
          S (bound_syms, mapper.f static_part))
        t

    let get_pieces_of_code t =
      let code =
        List.fold_left (fun code (S (_bound_syms, static_part)) ->
            Code_id.Map.disjoint_union code
              (Static_part.get_pieces_of_code static_part))
          Code_id.Map.empty
          t
      in
      assert (Code_id.Set.equal (Code_id.Map.keys code)
        (code_being_defined t));
      code

    let pieces_of_code ?newer_versions_of ?set_of_closures code =
      let newer_versions_of =
        Option.value newer_versions_of ~default:Code_id.Map.empty
      in
      let code =
        Code_id.Map.mapi (fun id params_and_body : Static_part.code ->
            let newer_version_of =
              Code_id.Map.find_opt id newer_versions_of
            in
            { params_and_body = Present params_and_body;
              newer_version_of;
            })
          code
      in
      let static_part : K.fabricated Static_part.t =
        let set_of_closures = Option.map snd set_of_closures in
        Code_and_set_of_closures {
          code;
          set_of_closures;
        }
      in
      let bound_symbols : K.fabricated Bound_symbols.t =
        let closure_symbols =
          Option.value (Option.map fst set_of_closures)
            ~default:Closure_id.Map.empty
        in
        Code_and_set_of_closures {
          code_ids = Code_id.Map.keys code;
          closure_symbols;
        }
      in
      S (bound_symbols, static_part)
  end

  module Definition = struct
    type t = {
      computation : Computation.t option;
      static_structure : Static_structure.t;
    }

    let print_with_cache ~cache ppf { computation; static_structure; } =
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>@<0>%s(computation@ %a)@<0>%s@]@ \
          @[<hov 1>(%a)@]\
          )@]"
        (if Option.is_none computation then Flambda_colours.elide ()
         else Flambda_colours.normal ())
        (Misc.Stdlib.Option.print Computation.print) computation
        (Flambda_colours.normal ())
        (Static_structure.print_with_cache ~cache) static_structure

    let print ppf t =
      print_with_cache ~cache:(Printing_cache.create ()) ppf t

    let free_names t =
      let free_in_computation =
        match t.computation with
        | None -> Name_occurrences.empty
        | Some computation -> Computation.free_names computation
      in
      let free_in_static_structure =
        Static_structure.free_names t.static_structure
      in
      Name_occurrences.union free_in_computation free_in_static_structure

    let static_structure t = t.static_structure

    let singleton_symbol symbol static_part =
      { computation = None;
        static_structure = [S (Singleton symbol, static_part)];
      }

    let pieces_of_code ?newer_versions_of code =
      let static_structure_part : Static_structure.t0 =
        Static_structure.pieces_of_code ?newer_versions_of code
      in
      { computation = None;
        static_structure = [static_structure_part];
      }

    let iter_computation t ~f =
      match t.computation with
      | None -> ()
      | Some computation -> f computation

    let map_computation t ~f =
      { t with
        computation = Option.map f t.computation;
      }

    let only_generative_effects t =
      (* CR-someday mshinwell: Could do a proper effects check. *)
      Option.is_none t.computation

    let iter_static_parts t iter =
      Static_structure.iter_static_parts t.static_structure iter

    let map_static_parts t mapper =
      { t with
        static_structure =
          Static_structure.map_static_parts t.static_structure mapper;
      }

    let being_defined t = Static_structure.being_defined t.static_structure

    let code_being_defined t =
      Static_structure.code_being_defined t.static_structure

    let get_pieces_of_code t =
      Static_structure.get_pieces_of_code t.static_structure

    let delete_bindings t ~free_names_after code_age_relation =
      let static_structure =
        Static_structure.delete_bindings t.static_structure ~free_names_after
          code_age_relation
      in
      { computation = t.computation;
        static_structure;
      }
  end

  type t =
    | Definition of {
        free_names : Name_occurrences.t;
        defn : Definition.t;
        body : t;
      }
    | Root of Symbol.t

  let rec print_with_cache ~cache ppf t =
    match t with
    | Definition { free_names = _; defn; body; } ->
      Format.fprintf ppf "@[<v 2>(@<0>%sDefinition@<0>%s@ %a)@]@;"
        (Flambda_colours.static_keyword ())
        (Flambda_colours.normal ())
        (Definition.print_with_cache ~cache) defn;
      print_with_cache ~cache ppf body
    | Root sym ->
      Format.fprintf ppf "@[(@<0>%sRoot@<0>%s %a)@]"
        (Flambda_colours.static_keyword ())
        (Flambda_colours.normal ())
        Symbol.print sym

  let print ppf t =
    print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let _invariant _env _t = ()

  let free_names t =
    match t with
    | Definition { free_names; _ } -> free_names
    | Root sym -> Name_occurrences.singleton_symbol sym Name_mode.normal

  let used_closure_vars t =
    match t with
    | Definition { free_names; _ } ->
      Name_occurrences.closure_vars free_names
    | Root _ -> Var_within_closure.Set.empty

  let define_symbol defn ~body code_age_relation =
    let free_names_of_body = free_names body in
    let defn =
      Definition.delete_bindings defn ~free_names_after:free_names_of_body
        code_age_relation
    in
    let can_delete =
      Definition.only_generative_effects defn
        && Static_structure.is_empty (Definition.static_structure defn)
    in
    if can_delete then body
    else
      let free_names =
        Name_occurrences.union (Definition.free_names defn)
          free_names_of_body
      in
      Definition { free_names; defn; body; }

  let root sym = Root sym

  let rec iter_definitions t ~f =
    match t with
    | Definition { defn; body; _ } ->
      f defn;
      iter_definitions body ~f
    | Root _ -> ()

  type descr =
    | Definition of Definition.t * t
    | Root of Symbol.t

  let descr (t : t) : descr =
    match t with
    | Definition { defn; body; free_names = _; } ->
      Definition (defn, body)
    | Root sym -> Root sym
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

  let free_names t =
    (* N.B. [imported_symbols] are not treated as free. *)
    Program_body.free_names t.body

  let used_closure_vars t =
    Program_body.used_closure_vars t.body

  let imported_symbols t = t.imported_symbols

  let root_symbol t =
    let rec loop (body : Program_body.t) =
      match body with
      | Definition { body; _ } -> loop body
      | Root root -> root
    in
    loop t.body

  let invariant _t = ()

  let iter_body t ~f = f t.body

  let map_body t ~f =
    { t with
      body = f t.body;
    }
end
