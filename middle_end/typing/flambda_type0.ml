(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR mshinwell: This warning appears to be broken (e.g. it claims
   [Meet_and_join_value] is unused) *)
[@@@ocaml.warning "-60"]

module Float_by_bit_pattern = Numbers.Float_by_bit_pattern
module Int32 = Numbers.Int32
module Int64 = Numbers.Int64

module Real_typing_environment0 = Typing_environment0

module K = Flambda_kind

module Make (Expr : sig
  type t
  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit
  val free_names : t -> Name_occurrences.t
end) = struct
  module T = Flambda_type0_internal_intf.S_impl (Expr)
  include T

  (* CR mshinwell: think about existentials *)
  let is_empty_typing_environment (env : typing_environment) =
    Name.Map.is_empty env.names_to_types

  let is_empty_equations (equations : equations) =
    match equations.typing_judgements with
    | None -> true
    | Some typing_judgements ->
      is_empty_typing_environment typing_judgements

  let create_equations () =
    { typing_judgements = None;
    }

  let domain_equations (equations : equations) =
    match equations.typing_judgements with
    | None -> Name.Set.empty
    | Some typing_judgements -> Name.Map.keys typing_judgements.names_to_types

  let print_extensibility print_contents ppf (e : _ extensibility) =
    match e with
    | Open contents ->
      Format.fprintf ppf "@[(Open@ %a)@]" print_contents contents
    | Exactly contents ->
      Format.fprintf ppf "@[(Exactly@ %a)@]" print_contents contents

  let print_mutable_or_immutable print_contents ppf
        (mut : _ mutable_or_immutable) =
    match mut with
    | Immutable contents -> print_contents ppf contents
    | Mutable -> Format.pp_print_string ppf "<mutable>"

  let print_or_unknown print_contents ppf (or_unknown : _ or_unknown) =
    match or_unknown with
    | Known contents -> print_contents ppf contents
    | Unknown -> Format.pp_print_string ppf "<unknown>"

  let print_or_alias print_descr ppf (or_alias : _ or_alias) =
    match or_alias with
    | No_alias descr -> print_descr ppf descr
    | Equals name ->
      Format.fprintf ppf "@[(%s=%s %a)@]"
        (Misc_color.bold_red ())
        (Misc_color.reset ())
        Name.print name
    | Type export_id ->
      Format.fprintf ppf "@[(%s=export_id%s %a)@]"
        (Misc_color.bold_red ())
        (Misc_color.reset ())
        Export_id.print export_id

  let print_unknown_or_join print_contents ppf (o : _ unknown_or_join) =
    let colour = Misc_color.bold_red () in
    match o with
    | Unknown -> Format.fprintf ppf "%sT%s" colour (Misc_color.reset ())
    | Join [] -> Format.fprintf ppf "%s_|_%s" colour (Misc_color.reset ())
    | Join [contents] -> print_contents ppf contents
    | Join incompatibles ->
      Format.fprintf ppf "@[(Join_incompatible@ (%a))@]"
        (Format.pp_print_list print_contents) incompatibles

  let print_ty_generic print_contents ppf ty =
    (print_or_alias (print_unknown_or_join print_contents)) ppf ty

  let print_of_kind_naked_number (type n) ppf (n : n of_kind_naked_number) =
    match n with
    | Immediate i ->
      Format.fprintf ppf "@[(Naked_immediates@ (%a))@]"
        Immediate.Set.print i
    | Float f ->
      Format.fprintf ppf "@[(Naked_floats@ (%a))@]"
        Float_by_bit_pattern.Set.print f
    | Int32 i ->
      Format.fprintf ppf "@[(Naked_int32s@ (%a))@]"
        Int32.Set.print i
    | Int64 i ->
      Format.fprintf ppf "@[(Naked_int64s@ (%a))@]"
        Int64.Set.print i
    | Nativeint i ->
      Format.fprintf ppf "@[(Naked_nativeints@ (%a))@]"
        Targetint.Set.print i

  let print_ty_naked_number (type n) ppf (ty : n ty_naked_number) =
    print_ty_generic print_of_kind_naked_number ppf ty

  let print_of_kind_value_boxed_number (type n)
        ppf (n : n of_kind_value_boxed_number) =
    match n with
    | Boxed_float f ->
      Format.fprintf ppf "@[(Boxed_float@ (%a))@]"
        print_ty_naked_number f
    | Boxed_int32 i ->
      Format.fprintf ppf "@[(Boxed_int32@ (%a))@]"
        print_ty_naked_number i
    | Boxed_int64 i ->
      Format.fprintf ppf "@[(Boxed_int64@ (%a))@]"
        print_ty_naked_number i
    | Boxed_nativeint i ->
      Format.fprintf ppf "@[(Boxed_nativeint@ (%a))@]"
        print_ty_naked_number i

  let rec print_immediate_case ~cache ppf
        ({ equations; } : immediate_case) =
    Format.fprintf ppf "@[<hov 1>(equations@ %a)@]"
      (print_equations_with_cache ~cache) equations

  and print_fields ~cache ppf (fields : t mutable_or_immutable array) =
    Format.fprintf ppf "@[[| %a |]@]"
      (Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
        (print_mutable_or_immutable (print_with_cache ~cache)))
      (Array.to_list fields)

  and print_singleton_block ~cache ppf { equations; fields; } =
    let no_equations = is_empty_equations equations in
    if no_equations then
      print_fields ~cache ppf fields
    else
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(equations@ %a)@]@ \
          @[<hov 1>(fields@ %a)@])@]"
        (print_equations_with_cache ~cache) equations
        (print_fields ~cache) fields

  and print_block_cases ~cache ppf ((Blocks { by_length; }) : block_cases) =
    match Targetint.OCaml.Map.get_singleton by_length with
    | Some (_length, block) -> print_singleton_block ~cache ppf block
    | None ->
      Format.fprintf ppf "@[(Join (by_length@ %a))@]"
        (Targetint.OCaml.Map.print (print_singleton_block ~cache)) by_length

  and print_immediates ~cache ppf cases =
    let no_equations =
      Immediate.Map.for_all (fun _imm ({ equations; } : immediate_case) ->
          is_empty_equations equations)
        cases
    in
    if no_equations then
      Immediate.Set.print ppf (Immediate.Map.keys cases)
    else
      Immediate.Map.print (print_immediate_case ~cache) ppf cases

  and print_blocks ~cache ppf cases =
    Tag.Map.print (print_block_cases ~cache) ppf cases

  and print_of_kind_value ~cache ppf (of_kind_value : of_kind_value) =
    match of_kind_value with
    | Blocks_and_tagged_immediates { blocks; immediates; is_int; get_tag; } ->
      begin match blocks, immediates, is_int, get_tag with
      | Known blocks, Known immediates, None, None
          when not (Tag.Map.is_empty blocks)
            && Immediate.Map.is_empty immediates ->
        Format.fprintf ppf "@[<hv 1>(blocks@ @[%a@])@])@]"
          (print_blocks ~cache) blocks
      | Known blocks, Known immediates, None, None
          when Tag.Map.is_empty blocks
            && not (Immediate.Map.is_empty immediates)
            && Immediate.Map.for_all
                 (fun _imm ({ equations; } : immediate_case) ->
                   is_empty_equations equations)
                 immediates ->
        Format.fprintf ppf "@[%a@]"
          Immediate.Set.print (Immediate.Map.keys immediates)
      | _ ->
        match is_int, get_tag with
        | None, None ->
          Format.fprintf ppf
            "@[<hov 1>(Blocks_and_immediates@ \
               @[<hov 1>(blocks@ %a)@]@ \
               @[<hov 1>(immediates@ %a)@])@]"
            (print_or_unknown (print_blocks ~cache)) blocks
            (print_or_unknown (print_immediates ~cache)) immediates
        | _, _ ->
          Format.fprintf ppf
            "@[<hov 1>(Blocks_and_immediates@ \
               @[<hov 1>(blocks@ %a)@]@ \
               @[<hov 1>(immediates@ %a)@]@ \
               @[<hov 1>(is_int@ %a)@]@ \
               @[<hov 1>(get_tag@ %a)@])@]"
            (print_or_unknown (print_blocks ~cache)) blocks
            (print_or_unknown (print_immediates ~cache)) immediates
            (Misc.Stdlib.Option.print Name.print) is_int
            (Misc.Stdlib.Option.print Name.print) get_tag
      end
    | Boxed_number n ->
      Format.fprintf ppf "@[(Boxed_number %a)@]"
        print_of_kind_value_boxed_number n
    | Closures closures -> print_closures ~cache ppf closures
    | String str_infos ->
      Format.fprintf ppf "@[(Strings (%a))@]" String_info.Set.print str_infos

  and print_ty_value_with_cache ~cache ppf (ty : ty_value) =
    print_ty_generic (print_of_kind_value ~cache) ppf ty

  and print_ty_value ppf (ty : ty_value) =
    print_ty_value_with_cache ~cache:(Printing_cache.create ()) ppf ty

  and print_ty_value_array ~cache ppf ty_values =
    Format.fprintf ppf "@[[| %a |]@]"
      (Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
        (print_ty_value_with_cache ~cache))
      (Array.to_list ty_values)

  and print_closures ~cache ppf (closures : closures) =
    Format.fprintf ppf "@[(Closures@ %a)@]"
      (Closure_id.Map.print (print_closures_entry ~cache)) closures

  and print_closures_entry ~cache ppf (entry : closures_entry) =
    Format.fprintf ppf "@[(set_of_closures@ %a)@]"
      (print_ty_fabricated_with_cache ~cache) entry.set_of_closures

  and print_inlinable_function_declaration_with_cache ~cache ppf
        (decl : inlinable_function_declaration) =
    Printing_cache.with_cache cache ppf "inlinable_fundecl" decl (fun ppf () ->
      Format.fprintf ppf
        "@[<hov 1>(inlinable@ \
          @[<hov 1>(closure_origin@ %a)@]@ \
          @[<hov 1>(continuation_param@ %a)@]@ \
          @[<hov 1>(exn_continuation_param@ %a)@]@ \
          @[<hov 1>(is_classic_mode@ %b)@]@ \
          @[<hov 1>(params@ (%a))@]@ \
          @[<hov 1>(body@ %a)@]@ \
          @[<hov 1>(free_names_in_body@ %a)@]@ \
          @[<hov 1>(result@ (%a))@]@ \
          @[<hov 1>(result_equations@ (%a))@]@ \
          @[<hov 1>(stub@ %b)@]@ \
          @[<hov 1>(dbg@ %a)@]@ \
          @[<hov 1>(inline@ %a)@]@ \
          @[<hov 1>(specialise@ %a)@]@ \
          @[<hov 1>(is_a_functor@ %b)@]@ \
          @[<hov 1>(invariant_params@ %a)@]@ \
          @[<hov 1>(size@ %a)@]@ \
          @[<hov 1>(direct_call_surrogate@ %a)@]@ \
          @[<hov 1>(my_closure@ %a)@])@]"
        Closure_origin.print decl.closure_origin
        Continuation.print decl.continuation_param
        Continuation.print decl.exn_continuation_param
        decl.is_classic_mode
        (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
          (fun ppf (param, ty) ->
            Format.fprintf ppf "@[<hov 1>(%a : %a)@]"
              Parameter.print param
              (print_with_cache ~cache) ty)) decl.params
        (Expr.print_with_cache ~cache) decl.body
        Name_occurrences.print decl.free_names_in_body
        (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
          (fun ppf ty ->
            Format.fprintf ppf "%a"
              print ty)) decl.result
        (print_equations_with_cache ~cache) decl.result_equations
        decl.stub
        Debuginfo.print_compact decl.dbg
        print_inline_attribute decl.inline
        print_specialise_attribute decl.specialise
        decl.is_a_functor
        Variable.Set.print (Lazy.force decl.invariant_params)
        (Misc.Stdlib.Option.print Format.pp_print_int) (Lazy.force decl.size)
        (Misc.Stdlib.Option.print Closure_id.print) decl.direct_call_surrogate
        Variable.print decl.my_closure)

  and print_inlinable_function_declaration ppf decl =
    print_inlinable_function_declaration_with_cache
      ~cache:(Printing_cache.create ())
      ppf decl

  and print_non_inlinable_function_declarations ppf ~cache
        (decl : non_inlinable_function_declarations) =
    Format.fprintf ppf
      "@[(Non_inlinable@ \
        @[(result (%a))@]@ \
        @[(direct_call_surrogate %a)@])@]"
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
        (fun ppf ty ->
          Format.fprintf ppf "%a"
            (print_with_cache ~cache) ty)) decl.result
      (Misc.Stdlib.Option.print Closure_id.print) decl.direct_call_surrogate

  and print_function_declarations ~cache ppf
        (decl : function_declarations) =
    match decl with
    | Inlinable decl ->
      print_inlinable_function_declaration_with_cache ~cache ppf decl
    | Non_inlinable decl ->
      begin match decl with
      | None -> Format.fprintf ppf "Non_inlinable"
      | Some decl ->
        print_non_inlinable_function_declarations ~cache ppf decl
      end

  and print_set_of_closures ~cache ppf (set : set_of_closures) =
    Printing_cache.with_cache cache ppf "set" set (fun ppf () ->
      Format.fprintf ppf
        "@[<hov 1>(Set_of_closures@ \
            @[<hov 1>(closures@ %a)@]@ \
            @[<hov 1>(closure_elements@ %a)@])@]"
        (print_extensibility (
            Closure_id.Map.print (print_ty_fabricated_with_cache ~cache)))
          set.closures
        (print_extensibility (
            Var_within_closure.Map.print (print_ty_value_with_cache ~cache)))
          set.closure_elements)

  and print_closure ~cache ppf (closure : closure) =
    Format.fprintf ppf "@[<hov 1>(Closure (function_decls@ %a))@]"
      (print_function_declarations ~cache) closure.function_decls

  and print_discriminant_case ~cache ppf ({ equations; } : discriminant_case) =
    Format.fprintf ppf "@[<hov 1>(equations@ %a)@]"
      (print_equations_with_cache ~cache) equations

  and print_of_kind_fabricated ~cache ppf (o : of_kind_fabricated) =
    match o with
    | Discriminant discriminant_map ->
      let no_equations =
        Discriminant.Map.for_all
          (fun _ ({ equations; } : discriminant_case) ->
            is_empty_equations equations)
          discriminant_map
      in
      if not no_equations then
        Format.fprintf ppf "@[<hov 1>(Discriminant@ %a)@]"
          (Discriminant.Map.print (print_discriminant_case ~cache))
          discriminant_map
      else
        Format.fprintf ppf "@[%a@]"
          Discriminant.Set.print (Discriminant.Map.keys discriminant_map)
    | Set_of_closures set -> print_set_of_closures ~cache ppf set
    | Closure closure -> print_closure ~cache ppf closure

  and print_ty_fabricated_with_cache ~cache ppf (ty : ty_fabricated) =
    print_ty_generic (print_of_kind_fabricated ~cache) ppf ty

  and print_ty_fabricated ppf (ty : ty_fabricated) =
    print_ty_fabricated_with_cache ~cache:(Printing_cache.create ()) ppf ty

  and print_descr ~cache ppf (descr : descr) =
    match descr with
    | Value ty ->
      Format.fprintf ppf "@[<hov 1>(Val@ %a)@]"
        (print_ty_value_with_cache ~cache) ty
    | Naked_number (ty, _kind) ->
      Format.fprintf ppf "@[<hov 1>(Naked@ %a)@]" print_ty_naked_number ty
    | Fabricated ty ->
      Format.fprintf ppf "@[<hov 1>(Fab@ %a)@]"
        (print_ty_fabricated_with_cache ~cache) ty

  and print_with_cache ~cache ppf (t : t) =
    match t.phantom with
    | None -> print_descr ~cache ppf t.descr
    | Some In_types ->
      Format.fprintf ppf "@[(Phantom_in_types@ (%a))@]"
        (print_descr ~cache) t.descr
    | Some Debug_only ->
      Format.fprintf ppf "@[(Phantom_debug_only@ (%a))@]"
        (print_descr ~cache) t.descr

  and print ppf (t : t) =
    let cache : Printing_cache.t = Printing_cache.create () in
    print_with_cache ~cache ppf t

  and print_typing_environment_with_cache ~cache ppf
        ({ resolver = _; canonical_names_to_aliases; names_to_types;
           levels_to_names; existentials; existential_freshening;
           must_be_closed = _; } as env) =
    if Name.Map.is_empty names_to_types then
      Format.pp_print_string ppf "Empty"
    else
      Printing_cache.with_cache cache ppf "env" env (fun ppf () ->
        let print_scope_level_and_type ppf (_scope_level, ty) =
          print_with_cache ~cache ppf ty
        in
        (* CR mshinwell: Add flag to disable this filtering *)
        let names_to_types =
          Name.Map.filter (fun name _ty ->
              not (Name.is_predefined_exception name))
            names_to_types
        in
        let levels_to_names =
          Scope_level.Map.map (fun names ->
              Name.Set.filter (fun name ->
                  not (Name.is_predefined_exception name))
                names)
            levels_to_names
        in
        if Name.Set.is_empty existentials then
          Format.fprintf ppf
            "@[<hov 1>(\
                @[<hov 1>(names_to_types@ %a)@]@ \
                @[<hov 1>(levels_to_names@ %a)@]@ \
                @[<hov 1>(canonical_names_to_aliases@ %a)@])@]"
            (Name.Map.print print_scope_level_and_type) names_to_types
            (Scope_level.Map.print Name.Set.print) levels_to_names
            (Name.Map.print Name.Set.print) canonical_names_to_aliases
        else
          Format.fprintf ppf
            "@[<hov 1>(\
                @[<hov 1>(names_to_types@ %a)@]@ \
                @[<hov 1>(levels_to_names@ %a)@]@ \
                @[<hov 1>(existentials@ %a)@]@ \
                @[<hov 1>(existential_freshening@ %a)@]@ \
                @[<hov 1>(canonical_names_to_aliases@ %a)@])@]"
            (Name.Map.print print_scope_level_and_type) names_to_types
            (Scope_level.Map.print Name.Set.print) levels_to_names
            Name.Set.print existentials
            Freshening.print existential_freshening
            (Name.Map.print Name.Set.print) canonical_names_to_aliases)

  and print_equations_with_cache ~cache ppf equations =
    match equations.typing_judgements with
    | None -> Format.pp_print_string ppf "()"
    | Some typing_judgements ->
      print_typing_environment_with_cache ~cache ppf typing_judgements

  let print_typing_environment ppf env =
    print_typing_environment_with_cache ~cache:(Printing_cache.create ())
      ppf env

  let print_equations ppf { typing_judgements; } =
    match typing_judgements with
    | None -> Format.pp_print_string ppf "()"
    | Some typing_judgements ->
      print_typing_environment ppf typing_judgements

  let free_names_or_alias free_names_contents (or_alias : _ or_alias) acc =
    match or_alias with
    | No_alias contents -> free_names_contents contents acc
    | Type _export_id -> acc
    | Equals name -> Name.Set.add name acc

  let free_names_unknown_or_join free_names_contents (o : _ unknown_or_join)
        acc =
    match o with
    | Unknown -> acc
    | Join contents_list ->
      List.fold_left (fun free_names contents ->
          free_names_contents contents free_names)
        acc
        contents_list

  let free_names_ty free_names_contents ty acc =
    free_names_or_alias (free_names_unknown_or_join free_names_contents) ty acc

  let free_names_of_kind_naked_number (type n) (_ty : n of_kind_naked_number)
        acc =
    acc

  let rec free_names (t : t) acc =
    match t.descr with
    | Value ty -> free_names_ty free_names_of_kind_value ty acc
    | Naked_number (ty, _kind) ->
      free_names_ty free_names_of_kind_naked_number ty acc
    | Fabricated ty -> free_names_ty free_names_of_kind_fabricated ty acc

  and free_names_of_kind_value (of_kind : of_kind_value) acc =
    match of_kind with
    | Blocks_and_tagged_immediates { blocks; immediates; is_int; get_tag; } ->
      let acc =
        match blocks with
        | Unknown -> acc
        | Known blocks ->
          Tag.Map.fold (fun _tag ((Blocks { by_length; }) : block_cases) acc ->
              Targetint.OCaml.Map.fold
                (fun _length (singleton : singleton_block) acc ->
                  let acc =
                    free_names_of_equations singleton.equations acc
                  in
                  Array.fold_left (fun acc (field : _ mutable_or_immutable) ->
                      match field with
                      | Immutable t -> free_names t acc
                      | Mutable -> acc)
                    acc singleton.fields)
                by_length
                acc)
            blocks
            acc
      in
      let acc =
        match immediates with
        | Unknown -> acc
        | Known immediates ->
          Immediate.Map.fold (fun _imm (case : immediate_case) acc ->
              free_names_of_equations case.equations acc)
            immediates
            acc
      in
      let acc =
        match is_int with
        | None -> acc
        | Some is_int -> Name.Set.add is_int acc
      in
      begin match get_tag with
      | None -> acc
      | Some get_tag -> Name.Set.add get_tag acc
      end
    | Boxed_number (Boxed_float n) ->
      free_names_ty free_names_of_kind_naked_number n acc
    | Boxed_number (Boxed_int32 n) ->
      free_names_ty free_names_of_kind_naked_number n acc
    | Boxed_number (Boxed_int64 n) ->
      free_names_ty free_names_of_kind_naked_number n acc
    | Boxed_number (Boxed_nativeint n) ->
      free_names_ty free_names_of_kind_naked_number n acc
    | Closures closures ->
      Closure_id.Map.fold (fun _closure_id (entry : closures_entry) acc ->
          free_names_ty free_names_of_kind_fabricated entry.set_of_closures acc)
        closures
        acc
    | String _ -> acc

  and free_names_of_kind_fabricated (of_kind : of_kind_fabricated) acc =
    match of_kind with
    | Discriminant discriminant_map ->
      Discriminant.Map.fold
        (fun _discriminant ({ equations; } : discriminant_case) acc ->
          free_names_of_equations equations acc)
        discriminant_map
        acc
    | Set_of_closures set ->
      let acc =
        Closure_id.Map.fold (fun _closure_id ty_fabricated acc ->
            free_names_ty free_names_of_kind_fabricated ty_fabricated acc)
          (extensibility_contents set.closures) acc
      in
      Var_within_closure.Map.fold (fun _var ty_value acc ->
          free_names_ty free_names_of_kind_value ty_value acc)
        (extensibility_contents set.closure_elements) acc
    | Closure closure -> free_names_of_closure closure acc

  and free_names_of_closure (closure : closure) acc =
    match closure.function_decls with
    | Inlinable decl ->
      let param_names =
        List.fold_left (fun param_names (param, _t) ->
            let name = Parameter.name param in
            Name.Set.add name param_names)
          Name.Set.empty
          decl.params
      in
      let free_names_result =
        List.fold_left (fun acc t ->
          free_names t acc)
          Name.Set.empty
          decl.result
      in
      let result_equations_domain = domain_equations decl.result_equations in
      let acc =
        Name.Set.union acc
          (Name.Set.diff
            (Name.Set.diff free_names_result result_equations_domain)
            param_names)
      in
      List.fold_left (fun acc (_param, t) ->
          free_names t acc)
        acc
        decl.params
    | Non_inlinable None -> acc
    | Non_inlinable (Some decls) ->
      List.fold_left (fun acc t ->
        free_names t acc)
        acc
        decls.result

  and free_names_of_typing_environment (_env : typing_environment) acc =
    (* Typing environments are always closed. *)
    acc
(*
    let all_names =
      Name.Map.fold (fun _bound_name (_scope_level, t) all_names ->
          free_names t all_names)
        env.names_to_types
        Name.Set.empty
    in
    let bound_names =
      Name.Set.union (Name.Map.keys env.names_to_types)
        env.existentials
    in
    let free_names = Name.Set.diff all_names bound_names in
    Name.Set.union free_names acc
*)

  and free_names_of_equations { typing_judgements; } acc =
    match typing_judgements with
    | None -> acc
    | Some typing_judgements ->
      free_names_of_typing_environment typing_judgements acc

  let free_names_set t =
    free_names t Name.Set.empty

  let free_names t =
let result =
    Name_occurrences.create_from_set_in_types (free_names_set t)
in
(*
Format.eprintf "Free names %a from: %a\n%!"
  Name_occurrences.print result print t;
*)
result

  let force_to_kind_value t =
    match t.descr with
    | Value ty_value -> ty_value
    | Naked_number _
    | Fabricated _ ->
      Misc.fatal_errorf "Type has wrong kind (expected [Value]):@ %a"
        print t

  let force_to_kind_naked_immediate (t : t) : Immediate.Set.t ty_naked_number =
    match t.descr with
    | Naked_number (ty_naked_number, K.Naked_number.Naked_immediate) ->
      ty_naked_number
    | Naked_number _
    | Fabricated _
    | Value _ ->
      Misc.fatal_errorf
        "Type has wrong kind (expected [Naked_number Immediate]):@ %a"
        print t

  let force_to_kind_naked_float (t : t)
        : Float_by_bit_pattern.Set.t ty_naked_number =
    match t.descr with
    | Naked_number (ty_naked_number, K.Naked_number.Naked_float) ->
      ty_naked_number
    | Naked_number _
    | Fabricated _
    | Value _ ->
      Misc.fatal_errorf
        "Type has wrong kind (expected [Naked_number Float]):@ %a"
        print t

  let force_to_kind_naked_int32 (t : t) : Int32.Set.t ty_naked_number =
    match t.descr with
    | Naked_number (ty_naked_number, K.Naked_number.Naked_int32) ->
      ty_naked_number
    | Naked_number _
    | Fabricated _
    | Value _ ->
      Misc.fatal_errorf
        "Type has wrong kind (expected [Naked_number Int32]):@ %a"
        print t

  let force_to_kind_naked_int64 (t : t) : Int64.Set.t ty_naked_number =
    match t.descr with
    | Naked_number (ty_naked_number, K.Naked_number.Naked_int64) ->
      ty_naked_number
    | Naked_number _
    | Fabricated _
    | Value _ ->
      Misc.fatal_errorf
        "Type has wrong kind (expected [Naked_number Int64]):@ %a"
        print t

  let force_to_kind_naked_nativeint (t : t) : Targetint.Set.t ty_naked_number =
    match t.descr with
    | Naked_number (ty_naked_number, K.Naked_number.Naked_nativeint) ->
      ty_naked_number
    | Naked_number _
    | Fabricated _
    | Value _ ->
      Misc.fatal_errorf
        "Type has wrong kind (expected [Naked_number Nativeint]):@ %a"
        print t

  let force_to_kind_naked_number (type n) (kind : n K.Naked_number.t) (t : t)
        : n ty_naked_number =
    match t.descr, kind with
    | Naked_number (ty_naked_number, K.Naked_number.Naked_immediate),
        K.Naked_number.Naked_immediate ->
      ty_naked_number
    | Naked_number (ty_naked_number, K.Naked_number.Naked_float),
        K.Naked_number.Naked_float ->
      ty_naked_number
    | Naked_number (ty_naked_number, K.Naked_number.Naked_int32),
        K.Naked_number.Naked_int32 ->
      ty_naked_number
    | Naked_number (ty_naked_number, K.Naked_number.Naked_int64),
        K.Naked_number.Naked_int64 ->
      ty_naked_number
    | Naked_number (ty_naked_number, K.Naked_number.Naked_nativeint),
        K.Naked_number.Naked_nativeint ->
      ty_naked_number
    | Naked_number _, _
    | Fabricated _, _
    | Value _, _ ->
      Misc.fatal_errorf "Type has wrong kind (expected [Naked_number %a]):@ %a"
        K.Naked_number.print kind
        print t

  let force_to_kind_fabricated t =
    match t.descr with
    | Fabricated ty_fabricated -> ty_fabricated
    | Value _
    | Naked_number _ ->
      Misc.fatal_errorf "Type has wrong kind (expected [Fabricated]):@ %a"
        print t

  let ty_is_obviously_bottom (ty : _ ty) =
    match ty with
    | No_alias (Join []) -> true
    | _ -> false

  let is_obviously_bottom (t : t) =
    match t.descr with
    | Value ty -> ty_is_obviously_bottom ty
    | Naked_number (ty, _) -> ty_is_obviously_bottom ty
    | Fabricated ty -> ty_is_obviously_bottom ty

  let of_ty_value ty_value : t =
    { descr = Value ty_value;
      phantom = None;
    }

  let of_ty_naked_number (type n) (ty_naked_number : n ty_naked_number)
        (kind : n K.Naked_number.t) : t =
    { descr = Naked_number (ty_naked_number, kind);
      phantom = None;
    }

  let of_ty_fabricated ty_fabricated : t =
    { descr = Fabricated ty_fabricated;
      phantom = None;
    }

  (* CR-someday mshinwell: Functions such as [alias] and [bottom] could be
     simplified if [K.t] were a GADT. *)

  let phantomize t (occs : K.Phantom_kind.occurrences) : t =
    let ok () = { t with phantom = Some occs; } in
    match t.phantom with
    | None -> ok ()
    | Some In_types ->
      begin match occs with
      | In_types -> t
      | Debug_only -> ok ()
      end
    | Some Debug_only ->
      begin match occs with
      | Debug_only -> t
      | In_types ->
        Misc.fatal_errorf "Cannot change [Debug_only] phantom type back \
            into an [In_types]"
          print t
      end

  type 'a type_accessor = typing_environment -> 'a

  let alias_type_of (kind : K.t) name : t =
    match kind with
    | Value ->
      { descr = Value (Equals name);
        phantom = None;
      }
    | Naked_number Naked_immediate ->
      { descr = Naked_number (Equals name, K.Naked_number.Naked_immediate);
        phantom = None;
      }
    | Naked_number Naked_float ->
      { descr = Naked_number (Equals name, K.Naked_number.Naked_float);
        phantom = None;
      }
    | Naked_number Naked_int32 ->
      { descr = Naked_number (Equals name, K.Naked_number.Naked_int32);
        phantom = None;
      }
    | Naked_number Naked_int64 ->
      { descr = Naked_number (Equals name, K.Naked_number.Naked_int64);
        phantom = None;
      }
    | Naked_number Naked_nativeint ->
      { descr = Naked_number (Equals name, K.Naked_number.Naked_nativeint);
        phantom = None;
      }
    | Fabricated ->
      { descr = Fabricated (Equals name);
        phantom = None;
      }
    | Phantom (occs, phantom_kind) ->
      let descr : descr =
        match phantom_kind with
        | Value -> Value (Equals name)
        | Naked_number Naked_immediate ->
          Naked_number (Equals name, K.Naked_number.Naked_immediate)
        | Naked_number Naked_float ->
          Naked_number (Equals name, K.Naked_number.Naked_float)
        | Naked_number Naked_int32 ->
          Naked_number (Equals name, K.Naked_number.Naked_int32)
        | Naked_number Naked_int64 ->
          Naked_number (Equals name, K.Naked_number.Naked_int64)
        | Naked_number Naked_nativeint ->
          Naked_number (Equals name, K.Naked_number.Naked_nativeint)
        | Fabricated -> Fabricated (Equals name)
      in
      { descr;
        phantom = Some occs;
      }

  let alias_type_of_as_ty_value name : ty_value = Equals name

  let alias_type_of_as_ty_fabricated name : ty_fabricated = Equals name

  let alias_type (kind : K.t) export_id : t =
    match kind with
    | Value ->
      { descr = Value (Type export_id);
        phantom = None;
      }
    | Naked_number Naked_immediate ->
      { descr = Naked_number (Type export_id, K.Naked_number.Naked_immediate);
        phantom = None;
      }
    | Naked_number Naked_float ->
      { descr = Naked_number (Type export_id, K.Naked_number.Naked_float);
        phantom = None;
      }
    | Naked_number Naked_int32 ->
      { descr = Naked_number (Type export_id, K.Naked_number.Naked_int32);
        phantom = None;
      }
    | Naked_number Naked_int64 ->
      { descr = Naked_number (Type export_id, K.Naked_number.Naked_int64);
        phantom = None;
      }
    | Naked_number Naked_nativeint ->
      { descr = Naked_number (Type export_id, K.Naked_number.Naked_nativeint);
        phantom = None;
      }
    | Fabricated ->
      { descr = Fabricated (Type export_id);
        phantom = None;
      }
    | Phantom (occs, phantom_kind) ->
      let descr : descr =
        match phantom_kind with
        | Value -> Value (Type export_id)
        | Naked_number Naked_immediate ->
          Naked_number (Type export_id, K.Naked_number.Naked_immediate)
        | Naked_number Naked_float ->
          Naked_number (Type export_id, K.Naked_number.Naked_float)
        | Naked_number Naked_int32 ->
          Naked_number (Type export_id, K.Naked_number.Naked_int32)
        | Naked_number Naked_int64 ->
          Naked_number (Type export_id, K.Naked_number.Naked_int64)
        | Naked_number Naked_nativeint ->
          Naked_number (Type export_id, K.Naked_number.Naked_nativeint)
        | Fabricated -> Fabricated (Type export_id)
      in
      { descr;
        phantom = Some occs;
      }

  let bottom_as_ty_value () : ty_value =
    No_alias (Join [])

  let bottom_as_ty_fabricated () : ty_fabricated =
    No_alias (Join [])

  let bottom (kind : K.t) : t =
    match kind with
    | Value ->
      { descr = Value (No_alias (Join []));
        phantom = None;
      }
    | Naked_number Naked_immediate ->
      { descr =
          Naked_number (No_alias (Join []), K.Naked_number.Naked_immediate);
        phantom = None;
      }
    | Naked_number Naked_float ->
      { descr = Naked_number (No_alias (Join []), K.Naked_number.Naked_float);
        phantom = None;
      }
    | Naked_number Naked_int32 ->
      { descr = Naked_number (No_alias (Join []), K.Naked_number.Naked_int32);
        phantom = None;
      }
    | Naked_number Naked_int64 ->
      { descr = Naked_number (No_alias (Join []), K.Naked_number.Naked_int64);
        phantom = None;
      }
    | Naked_number Naked_nativeint ->
      { descr =
          Naked_number (No_alias (Join []), K.Naked_number.Naked_nativeint);
        phantom = None;
      }
    | Fabricated ->
      { descr = Fabricated (No_alias (Join []));
        phantom = None;
      }
    | Phantom (occs, phantom_kind) ->
      let descr : descr =
        match phantom_kind with
        | Value -> Value (No_alias (Join []))
        | Naked_number Naked_immediate ->
          Naked_number (No_alias (Join []), K.Naked_number.Naked_immediate)
        | Naked_number Naked_float ->
          Naked_number (No_alias (Join []), K.Naked_number.Naked_float)
        | Naked_number Naked_int32 ->
          Naked_number (No_alias (Join []), K.Naked_number.Naked_int32)
        | Naked_number Naked_int64 ->
          Naked_number (No_alias (Join []), K.Naked_number.Naked_int64)
        | Naked_number Naked_nativeint ->
          Naked_number (No_alias (Join []), K.Naked_number.Naked_nativeint)
        | Fabricated -> Fabricated (No_alias (Join []))
      in
      { descr;
        phantom = Some occs;
      }

  let any_value_as_ty_value () : ty_value =
    No_alias Unknown

  let any_fabricated_as_ty_fabricated () : ty_fabricated =
    No_alias Unknown

  let any_naked_float_as_ty_naked_float () : _ ty_naked_number =
    No_alias Unknown

  let any_value () : t =
    { descr = Value (any_value_as_ty_value ());
      phantom = None;
    }

  let any_tagged_immediate () : t =
    { descr =
        Value (No_alias (Join [Blocks_and_tagged_immediates {
          immediates = Unknown;
          blocks = Known Tag.Map.empty;
          is_int = None;
          get_tag = None;
        }]));
      phantom = None;
    }

  let any_naked_immediate () : t =
    { descr = Naked_number (No_alias Unknown, K.Naked_number.Naked_immediate);
      phantom = None;
    }

  let any_naked_float () : t =
    { descr = Naked_number (No_alias Unknown, K.Naked_number.Naked_float);
      phantom = None;
    }

  let any_naked_int32 () : t =
    { descr = Naked_number (No_alias Unknown, K.Naked_number.Naked_int32);
      phantom = None;
    }

  let any_naked_int64 () : t =
    { descr = Naked_number (No_alias Unknown, K.Naked_number.Naked_int64);
      phantom = None;
    }

  let any_naked_nativeint () : t =
    { descr = Naked_number (No_alias Unknown, K.Naked_number.Naked_nativeint);
      phantom = None;
    }

  let any_fabricated () : t =
    { descr = Fabricated (No_alias Unknown);
      phantom = None;
    }

  let unknown (kind : K.t) =
    match kind with
    | Value ->
      { descr = Value (No_alias Unknown);
        phantom = None;
      }
    | Naked_number Naked_immediate ->
      { descr =
          Naked_number (No_alias Unknown, K.Naked_number.Naked_immediate);
        phantom = None;
      }
    | Naked_number Naked_float ->
      { descr = Naked_number (No_alias Unknown, K.Naked_number.Naked_float);
        phantom = None;
      }
    | Naked_number Naked_int32 ->
      { descr = Naked_number (No_alias Unknown, K.Naked_number.Naked_int32);
        phantom = None;
      }
    | Naked_number Naked_int64 ->
      { descr = Naked_number (No_alias Unknown, K.Naked_number.Naked_int64);
        phantom = None;
      }
    | Naked_number Naked_nativeint ->
      { descr =
          Naked_number (No_alias Unknown, K.Naked_number.Naked_nativeint);
        phantom = None;
      }
    | Fabricated ->
      { descr = Fabricated (No_alias Unknown);
        phantom = None;
      }
    | Phantom (occs, phantom_kind) ->
      let descr : descr =
        match phantom_kind with
        | Value -> Value (No_alias Unknown)
        | Naked_number Naked_immediate ->
          Naked_number (No_alias Unknown, K.Naked_number.Naked_immediate)
        | Naked_number Naked_float ->
          Naked_number (No_alias Unknown, K.Naked_number.Naked_float)
        | Naked_number Naked_int32 ->
          Naked_number (No_alias Unknown, K.Naked_number.Naked_int32)
        | Naked_number Naked_int64 ->
          Naked_number (No_alias Unknown, K.Naked_number.Naked_int64)
        | Naked_number Naked_nativeint ->
          Naked_number (No_alias Unknown, K.Naked_number.Naked_nativeint)
        | Fabricated -> Fabricated (No_alias Unknown)
      in
      { descr;
        phantom = Some occs;
      }

  let these_naked_immediates (is : Immediate.Set.t) : t =
    let of_kind : _ of_kind_naked_number = Immediate is in
    { descr =
        Naked_number (No_alias (Join [of_kind]),
          K.Naked_number.Naked_immediate);
      phantom = None;
    }

  let these_naked_floats (is : Float_by_bit_pattern.Set.t) : t =
    let of_kind : _ of_kind_naked_number = Float is in
    { descr =
        Naked_number (No_alias (Join [of_kind]),
          K.Naked_number.Naked_float);
      phantom = None;
    }

  let these_naked_int32s (is : Int32.Set.t) : t =
    let of_kind : _ of_kind_naked_number = Int32 is in
    { descr =
        Naked_number (No_alias (Join [of_kind]),
          K.Naked_number.Naked_int32);
      phantom = None;
    }

  let these_naked_int64s (is : Int64.Set.t) : t =
    let of_kind : _ of_kind_naked_number = Int64 is in
    { descr =
        Naked_number (No_alias (Join [of_kind]),
          K.Naked_number.Naked_int64);
      phantom = None;
    }

  let these_naked_nativeints (is : Targetint.Set.t) : t =
    let of_kind : _ of_kind_naked_number = Nativeint is in
    { descr =
        Naked_number (No_alias (Join [of_kind]),
          K.Naked_number.Naked_nativeint);
      phantom = None;
    }

  let this_naked_immediate i =
    these_naked_immediates (Immediate.Set.singleton i)

  let this_naked_float f =
    these_naked_floats (Float_by_bit_pattern.Set.singleton f)

  let this_naked_float_as_ty_naked_float f =
    let fs = Float_by_bit_pattern.Set.singleton f in
    let of_kind : _ of_kind_naked_number = Float fs in
    No_alias (Join [of_kind])

  let this_naked_int32 i =
    these_naked_int32s (Int32.Set.singleton i)

  let this_naked_int64 i =
    these_naked_int64s (Int64.Set.singleton i)

  let this_naked_nativeint i =
    these_naked_nativeints (Targetint.Set.singleton i)

(* This one is tricky
  let tag_immediate (t : t) : t =
    match t with
    | Naked_number (ty_naked_number, Naked_immediate) ->


      Value (No_alias (Ok (No_alias (
        Tagged_immediate ty_naked_immediate))))
    | Value _
    | Naked_number _
    | Fabricated _
    | Phantom _ ->
      Misc.fatal_errorf "Type of wrong kind for [tag_immediate]: %a"
        print t
*)

  let check_not_phantom t reason =
    match t.phantom with
    | None -> ()
    | Some _ ->
      Misc.fatal_errorf "Type given to [%s] cannot be phantom: %a"
        reason
        print t

  let box_float (t : t) : t =
    check_not_phantom t "box_float";
    match t.descr with
    | Naked_number (ty_naked_float, K.Naked_number.Naked_float) ->
      { descr =
          Value (No_alias (Join [
            Boxed_number (Boxed_float ty_naked_float)]));
        phantom = None;
      }
    | Value _
    | Naked_number _
    | Fabricated _ ->
      Misc.fatal_errorf "Type of wrong kind for [box_float]: %a"
        print t

  let box_int32 (t : t) : t =
    check_not_phantom t "box_int32";
    match t.descr with
    | Naked_number (ty_naked_int32, K.Naked_number.Naked_int32) ->
      { descr =
          Value (No_alias (Join [
            Boxed_number (Boxed_int32 ty_naked_int32)]));
        phantom = None;
      }
    | Value _
    | Naked_number _
    | Fabricated _ ->
      Misc.fatal_errorf "Type of wrong kind for [box_int32]: %a"
        print t

  let box_int64 (t : t) : t =
    check_not_phantom t "box_int64";
    match t.descr with
    | Naked_number (ty_naked_int64, K.Naked_number.Naked_int64) ->
      { descr =
          Value (No_alias (Join [
            Boxed_number (Boxed_int64 ty_naked_int64)]));
        phantom = None;
      }
    | Value _
    | Naked_number _
    | Fabricated _ ->
      Misc.fatal_errorf "Type of wrong kind for [box_int64]: %a"
        print t

  let box_nativeint (t : t) : t =
    check_not_phantom t "box_nativeint";
    match t.descr with
    | Naked_number (ty_naked_nativeint, K.Naked_number.Naked_nativeint) ->
      { descr =
          Value (No_alias (Join [
            Boxed_number (Boxed_nativeint ty_naked_nativeint)]));
        phantom = None;
      }
    | Value _
    | Naked_number _
    | Fabricated _ ->
      Misc.fatal_errorf "Type of wrong kind for [box_nativeint]: %a"
        print t

  let these_tagged_immediates imms : t =
    if Immediate.Set.is_empty imms then
      bottom (K.value ())
    else
      let immediates =
        Immediate.Set.fold (fun imm map ->
            let case : immediate_case =
              { equations = create_equations ();
              }
            in
            Immediate.Map.add imm case map)
          imms
          Immediate.Map.empty
      in
      (* CR mshinwell: See if we can have a creation function for this *)
      let blocks_and_tagged_immediates : blocks_and_tagged_immediates =
        { immediates = Known immediates;
          blocks = Known Tag.Map.empty;
          is_int = None;
          get_tag = None;
        }
      in
      { descr =
          Value (No_alias (Join [Blocks_and_tagged_immediates
            blocks_and_tagged_immediates]));
        phantom = None;
      }

  (* CR mshinwell: share code with previous function *)
  let these_tagged_immediates_with_envs env_map =
    if Immediate.Map.is_empty env_map then
      bottom (K.value ())
    else
      let immediates =
        Immediate.Map.map (fun equations : immediate_case ->
            { equations; })
          env_map
      in
      let blocks_and_tagged_immediates : blocks_and_tagged_immediates =
        { immediates = Known immediates;
          blocks = Known Tag.Map.empty;
          is_int = None;
          get_tag = None;
        }
      in
      { descr =
          Value (No_alias (Join [Blocks_and_tagged_immediates
            blocks_and_tagged_immediates]));
        phantom = None;
      }

  let this_tagged_immediate imm =
    these_tagged_immediates (Immediate.Set.singleton imm)

  let any_tagged_bool () =
    let bools =
      Immediate.Set.add Immediate.bool_false
        (Immediate.Set.add Immediate.bool_true Immediate.Set.empty)
    in
    these_tagged_immediates bools

  let this_boxed_float f = box_float (this_naked_float f)
  let this_boxed_int32 f = box_int32 (this_naked_int32 f)
  let this_boxed_int64 f = box_int64 (this_naked_int64 f)
  let this_boxed_nativeint f = box_nativeint (this_naked_nativeint f)

  let these_boxed_floats f = box_float (these_naked_floats f)
  let these_boxed_int32s f = box_int32 (these_naked_int32s f)
  let these_boxed_int64s f = box_int64 (these_naked_int64s f)
  let these_boxed_nativeints f = box_nativeint (these_naked_nativeints f)

  let these_discriminants_as_ty_fabricated discriminants_to_equations
        : ty_fabricated =
    let discriminant_map =
      Discriminant.Map.map (fun equations : discriminant_case ->
          { equations; })
        discriminants_to_equations
    in
    No_alias (Join [Discriminant discriminant_map])

  let these_discriminants discriminants_to_equations : t =
    { descr = Fabricated (
        these_discriminants_as_ty_fabricated discriminants_to_equations);
      phantom = None;
    }

  let this_discriminant_as_ty_fabricated discriminant =
    let discriminant_map =
      Discriminant.Map.singleton discriminant
        ({ equations = create_equations (); } : discriminant_case)
    in
    No_alias (Join [Discriminant discriminant_map])

  let this_discriminant discriminant : t =
    { descr = Fabricated (
        this_discriminant_as_ty_fabricated discriminant);
      phantom = None;
    }

  let any_discriminant_as_ty_fabricated () : ty_fabricated =
    No_alias Unknown

  let this_immutable_string_as_ty_value str : ty_value =
    let str : String_info.t =
      { contents = Contents str;
        (* CR mshinwell: Possibility for exception? *)
        size = Targetint.OCaml.of_int (String.length str);
      }
    in
    let str = String_info.Set.singleton str in
    No_alias (Join [String str])

  let this_immutable_string str : t =
    { descr = Value (this_immutable_string_as_ty_value str);
      phantom = None;
    }

  let immutable_string_as_ty_value ~size : ty_value =
    let str : String_info.t =
      { contents = Unknown_or_mutable;
        size;
      }
    in
    let str = String_info.Set.singleton str in
    No_alias (Join [String str])

  let immutable_string ~size : t =
    { descr = Value (immutable_string_as_ty_value ~size);
      phantom = None;
    }

  let mutable_string ~size : t =
    let str : String_info.t =
      { contents = Unknown_or_mutable;
        size;
      }
    in
    let str = String_info.Set.singleton str in
    { descr = Value (No_alias (Join [String str]));
      phantom = None;
    }

  let mutable_float_array ~size : t =
    let fields =
      Array.init (Targetint.OCaml.to_int size)
        (fun _index : _ mutable_or_immutable -> Mutable)
    in
    let singleton_block : singleton_block =
      { equations = create_equations ();
        fields;
      }
    in
    let by_length =
      Targetint.OCaml.Map.add size singleton_block
        Targetint.OCaml.Map.empty
    in
    let block_cases : block_cases = Blocks { by_length; } in
    let blocks =
      Tag.Map.add Tag.double_array_tag block_cases Tag.Map.empty
    in
    let blocks_imms : blocks_and_tagged_immediates =
      { immediates = Known Immediate.Map.empty;
        blocks = Known blocks;
        is_int = None;
        get_tag = None;
      }
    in
    { descr =
        Value (No_alias (Join [Blocks_and_tagged_immediates blocks_imms]));
      phantom = None;
    }

  let immutable_float_array fields : t =
    match Targetint.OCaml.of_int_option (Array.length fields) with
    | None ->
      Misc.fatal_error "Immutable float array too long for target"
    | Some length ->
      let fields =
        Array.map (fun ty_naked_number : _ mutable_or_immutable ->
            let t : t =
              { descr =
                  Naked_number (ty_naked_number, K.Naked_number.Naked_float);
                phantom = None;
              }
            in
            Immutable t)
          fields
      in
      let singleton_block : singleton_block =
        { equations = create_equations ();
          fields;
        }
      in
      let by_length =
        Targetint.OCaml.Map.add length singleton_block
          Targetint.OCaml.Map.empty
      in
      let block_cases : block_cases = Blocks { by_length; } in
      let blocks =
        Tag.Map.add Tag.double_array_tag block_cases Tag.Map.empty
      in
      let blocks_imms : blocks_and_tagged_immediates =
        { immediates = Known Immediate.Map.empty;
          blocks = Known blocks;
          is_int = None;
          get_tag = None;
        }
      in
      { descr =
          Value (No_alias (Join [Blocks_and_tagged_immediates blocks_imms]));
        phantom = None;
      }

  let this_immutable_float_array fields : t =
    let make_field f : _ ty_naked_number =
      No_alias (Join [Float (Float_by_bit_pattern.Set.singleton f)])
    in
    let fields = Array.map make_field fields in
    immutable_float_array fields

  let block tag ~fields =
    (* CR mshinwell: We should check the field kinds against the tag. *)
    match Targetint.OCaml.of_int_option (Array.length fields) with
    | None ->
      Misc.fatal_error "Block too long for target"
    | Some length ->
      let fields =
        Array.map
          (fun (field : _ mutable_or_immutable) : t mutable_or_immutable ->
            match field with
            | Immutable t -> Immutable t
            | Mutable -> Mutable)
          fields
      in
      let singleton_block : singleton_block =
        { equations = create_equations ();
          fields;
        }
      in
      let by_length =
        Targetint.OCaml.Map.add length singleton_block
          Targetint.OCaml.Map.empty
      in
      let block_cases : block_cases = Blocks { by_length; } in
      let blocks = Tag.Map.add tag block_cases Tag.Map.empty in
      let blocks_imms : blocks_and_tagged_immediates =
        { immediates = Known Immediate.Map.empty;
          blocks = Known blocks;
          is_int = None;
          get_tag = None;
        }
      in
      { descr =
          Value (No_alias (Join [Blocks_and_tagged_immediates blocks_imms]));
        phantom = None;
      }

  let block_of_values tag ~fields =
    (* CR mshinwell: Express in terms of the new [block] function above *)
    let tag = Tag.Scannable.to_tag tag in
    match Targetint.OCaml.of_int_option (Array.length fields) with
    | None ->
      Misc.fatal_error "Block of values too long for target"
    | Some length ->
      let fields =
        Array.map
          (fun (field : _ mutable_or_immutable) : t mutable_or_immutable ->
            match field with
            | Immutable ty_value ->
              Immutable { descr = Value ty_value; phantom = None; }
            | Mutable -> Mutable)
          fields
      in
      let singleton_block : singleton_block =
        { equations = create_equations ();
          fields;
        }
      in
      let by_length =
        Targetint.OCaml.Map.add length singleton_block
          Targetint.OCaml.Map.empty
      in
      let block_cases : block_cases = Blocks { by_length; } in
      let blocks = Tag.Map.add tag block_cases Tag.Map.empty in
      let blocks_imms : blocks_and_tagged_immediates =
        { immediates = Known Immediate.Map.empty;
          blocks = Known blocks;
          is_int = None;
          get_tag = None;
        }
      in
      { descr =
          Value (No_alias (Join [Blocks_and_tagged_immediates blocks_imms]));
        phantom = None;
      }

  let block_of_unknown_values tag ~size =
    let fields =
      Array.init size (fun _index : _ mutable_or_immutable ->
        Immutable (any_value_as_ty_value ()))
    in
    block_of_values tag ~fields

  let variant_whose_discriminants_are ~is_int ~get_tag : t =
    let blocks_imms : blocks_and_tagged_immediates =
      { immediates = Unknown;
        blocks = Unknown;
        is_int;
        get_tag;
      }
    in
    { descr =
        Value (No_alias (Join [Blocks_and_tagged_immediates blocks_imms]));
      phantom = None;
    }

  let any_boxed_float () = box_float (any_naked_float ())
  let any_boxed_int32 () = box_int32 (any_naked_int32 ())
  let any_boxed_int64 () = box_int64 (any_naked_int64 ())
  let any_boxed_nativeint () = box_nativeint (any_naked_nativeint ())

  let kind (t : t) =
    match t.phantom with
    | None ->
      begin match t.descr with
      | Value _ -> K.value ()
      | Naked_number (_, K.Naked_number.Naked_immediate) -> K.naked_immediate ()
      | Naked_number (_, K.Naked_number.Naked_float) -> K.naked_float ()
      | Naked_number (_, K.Naked_number.Naked_int32) -> K.naked_int32 ()
      | Naked_number (_, K.Naked_number.Naked_int64) -> K.naked_int64 ()
      | Naked_number (_, K.Naked_number.Naked_nativeint) -> K.naked_nativeint ()
      | Fabricated _ -> K.fabricated ()
      end
    | Some occurrences ->
      let phantom_kind =
        let module PK = K.Phantom_kind in
        match t.descr with
        | Value _ -> PK.Value
        | Naked_number (_, K.Naked_number.Naked_immediate) ->
          PK.Naked_number Naked_immediate
        | Naked_number (_, K.Naked_number.Naked_float) ->
          PK.Naked_number Naked_float
        | Naked_number (_, K.Naked_number.Naked_int32) ->
          PK.Naked_number Naked_int32
        | Naked_number (_, K.Naked_number.Naked_int64) ->
          PK.Naked_number Naked_int64
        | Naked_number (_, K.Naked_number.Naked_nativeint) ->
          PK.Naked_number Naked_nativeint
        | Fabricated _ -> PK.Fabricated
      in
      match occurrences with
      | In_types -> K.phantom In_types phantom_kind
      | Debug_only -> K.phantom Debug_only phantom_kind

  let check_of_kind t (expected_kind : K.t) =
    let actual_kind = kind t in
    if not (K.equal actual_kind expected_kind) then begin
      Misc.fatal_errorf "Type has wrong kind: have %a but expected %a"
        K.print actual_kind
        K.print expected_kind
    end

  let bottom_like t = bottom (kind t)
  let unknown_like t = unknown (kind t)

(*
  let is_unknown (env, ty) =
    let ty, _canonical_name = resolve_aliases (env, ty) in
    match ty.descr with
    | Value (No_alias Unknown) -> true
    | Naked_number (No_alias Unknown, _) -> true
    | Fabricated (No_alias Unknown) -> true
    | Value _ -> false
    | Naked_number _ -> false
    | Fabricated _ -> false
*)

  let create_inlinable_function_declaration ~is_classic_mode ~closure_origin
        ~continuation_param ~exn_continuation_param
        ~params ~body ~result ~result_equations ~stub ~dbg ~inline
        ~specialise ~is_a_functor ~invariant_params ~size ~direct_call_surrogate
        ~my_closure : function_declarations =
    Inlinable {
      closure_origin;
      continuation_param;
      exn_continuation_param;
      is_classic_mode;
      params;
      body;
      code_id = Code_id.create (Compilation_unit.get_current_exn ());
      free_names_in_body = Expr.free_names body;
      result_equations;
      result;
      stub;
      dbg;
      inline;
      specialise;
      is_a_functor;
      invariant_params;
      size;
      direct_call_surrogate;
      my_closure;
    }

  let create_non_inlinable_function_declaration ~params ~result
        ~result_equations ~direct_call_surrogate
        : function_declarations =
    let decl : non_inlinable_function_declarations =
      { params;
        result;
        result_equations;
        direct_call_surrogate;
      }
    in
    Non_inlinable (Some decl)

  let closure function_decls : ty_fabricated =
    No_alias (Join [Closure { function_decls; }])

  let closures_entry ~set_of_closures : closures_entry =
    { set_of_closures; }

  let closures by_closure_id : t =
    { descr = Value (No_alias (Join [Closures by_closure_id]));
      phantom = None;
    }

  let set_of_closures ~closures ~closure_elements =
    let set_of_closures : set_of_closures =
      { closures;
        closure_elements;
      }
    in
    let no_closures =
      match set_of_closures.closures with
      | Open _ -> false
      | Exactly map -> Closure_id.Map.is_empty map
    in
    let descr : descr =
      if no_closures then
        Fabricated (No_alias (Join []))
      else
        Fabricated (No_alias (Join [Set_of_closures set_of_closures]))
    in
    { descr;
      phantom = None;
    }

  let ensure_phantomness_matches t1 t2 reason =
    match t1.phantom, t2.phantom with
    | None, None
    | Some In_types, Some In_types
    | Some Debug_only, Some Debug_only -> ()
    | _, _ ->
      Misc.fatal_errorf "Phantom kind mismatch (%s): %a vs. %a"
        reason
        print t1
        print t2

  type changes = Neither | Left | Right | Both

  let join_changes (changes1 : changes) (changes2 : changes) =
    match changes1, changes2 with
    | Neither, Neither -> Neither
    | Neither, Left -> Left
    | Neither, Right -> Right
    | Neither, Both -> Both
    | Left, Neither -> Left
    | Left, Left -> Left
    | Left, Right -> Both
    | Left, Both -> Both
    | Right, Neither -> Right
    | Right, Left -> Both
    | Right, Right -> Right
    | Right, Both -> Both
    | Both, Neither -> Both
    | Both, Left -> Both
    | Both, Right -> Both
    | Both, Both -> Both

  module type Meet_and_join_spec = sig
    type of_kind_foo

    val kind : Flambda_kind.t

    val to_type : of_kind_foo ty -> t

    val force_to_kind : t -> of_kind_foo ty

    val print_ty : Format.formatter -> of_kind_foo ty -> unit

    val meet_of_kind_foo
       : typing_environment
      -> typing_environment
      -> of_kind_foo
      -> of_kind_foo
      -> (of_kind_foo * equations) Or_bottom.t

    (* If the supplied types are compatible, the join must be pushed inside
       their structure, and [Ok] returned.  Otherwise [Unknown] must be
       returned. *)
    (* CR mshinwell: add comment about requirement for equivalence
       relationness *)
    val join_of_kind_foo
       : typing_environment
      -> typing_environment
      -> of_kind_foo
      -> of_kind_foo
      -> of_kind_foo or_unknown
  end

  module type Meet_and_join = sig
    type of_kind_foo

    (* Least upper bound of two types of a particular kind. *)
    val join_ty
       : typing_environment
      -> typing_environment
      -> of_kind_foo ty
      -> of_kind_foo ty
      -> of_kind_foo ty

    (* Greatest lower bound of two types of a particular kind. *)
    val meet_ty
       : typing_environment
      -> typing_environment
      -> of_kind_foo ty
      -> of_kind_foo ty
      -> of_kind_foo ty * equations
  end

  (* CR mshinwell: Work out which properties we need to prove, e.g.
     Distributivity of meet over join:
       X n (X' u Y') == (X n X') u (X n Y'). *)
  module rec Make_meet_and_join : functor
       (S : Meet_and_join_spec)
    -> sig
         include Meet_and_join
         with type of_kind_foo := S.of_kind_foo
       end =
  functor (S : Meet_and_join_spec) -> struct
    let unknown_or_join_is_bottom (uj : _ unknown_or_join) =
      match uj with
      | Join [] -> true
      | Unknown | Join _ -> false

    let rec join_on_unknown_or_join env1 env2
          (uj1 : S.of_kind_foo unknown_or_join)
          (uj2 : S.of_kind_foo unknown_or_join)
          : S.of_kind_foo unknown_or_join =
      if env1 == env2 && uj1 == uj2 then uj1
      else
        match uj1, uj2 with
        | Unknown, _ | _, Unknown -> Unknown
        | Join of_kind_foos1, Join of_kind_foos2 ->
          (* We rely on the invariant in flambda_type0_intf.ml.
             Everything in [of_kind_foos1] is mutually incompatible with each
             other; likewise in [of_kind_foos2]. *)
          let of_kind_foos =
            List.fold_left (fun of_kind_foos of_kind_foo ->
                (* [of_kind_foo] can be compatible with at most one of the
                   elements of [of_kind_foos]. *)
                let found_one = ref false in
                let joined =
                  List.map (fun of_kind_foo' ->
                      let join =
                        S.join_of_kind_foo env1 env2 of_kind_foo of_kind_foo'
                      in
                      match join with
                      | Known of_kind_foo ->
                        if !found_one then begin
                          (* CR mshinwell: Add detail showing what was wrong. *)
                          Misc.fatal_errorf "Invariant broken for [Join]"
                        end;
                        found_one := true;
                        of_kind_foo
                      | Unknown -> of_kind_foo')
                    of_kind_foos
                in
                if not !found_one then of_kind_foo :: of_kind_foos
                else joined)
              of_kind_foos2
              of_kind_foos1
          in
          Join of_kind_foos

    and join_ty env1 env2
          (or_alias1 : S.of_kind_foo ty)
          (or_alias2 : S.of_kind_foo ty)
          : S.of_kind_foo ty =
      if env1 == env2 && or_alias1 == or_alias2 then or_alias1
      else
        let unknown_or_join1, canonical_name1 =
          Typing_environment0.
              resolve_aliases_and_squash_unresolved_names_on_ty'
            env1
            ~kind:S.kind
            ~force_to_kind:S.force_to_kind
            ~unknown:(No_alias Unknown)
            ~print_ty:S.print_ty
            or_alias1
        in
        let unknown_or_join2, canonical_name2 =
          Typing_environment0.
              resolve_aliases_and_squash_unresolved_names_on_ty'
            env2
            ~kind:S.kind
            ~force_to_kind:S.force_to_kind
            ~unknown:(No_alias Unknown)
            ~print_ty:S.print_ty
            or_alias2
        in
        match canonical_name1, canonical_name2 with
        | Some name1, Some name2 when Name.equal name1 name2 ->
          Equals name1
        (* CR mshinwell: The symmetrical cases ("is unknown") should be
           present on the [meet] function, below. *)
        | Some name1, _ when unknown_or_join_is_bottom unknown_or_join2 ->
          Equals name1
        | _, Some name2 when unknown_or_join_is_bottom unknown_or_join1 ->
          Equals name2
        | None, None ->
          let unknown_or_join =
            join_on_unknown_or_join env1 env2
              unknown_or_join1 unknown_or_join2
          in
          if unknown_or_join == unknown_or_join1 then begin
            assert (match or_alias1 with No_alias _ -> true | _ -> false);
            or_alias1
          end else if unknown_or_join == unknown_or_join2 then begin
            assert (match or_alias2 with No_alias _ -> true | _ -> false);
            or_alias2
          end else begin
            No_alias unknown_or_join
          end
        | _, _ ->
          let unknown_or_join =
            join_on_unknown_or_join env1 env2
              unknown_or_join1 unknown_or_join2
          in
          No_alias unknown_or_join

    let rec meet_on_unknown_or_join env1 env2
          (ou1 : S.of_kind_foo unknown_or_join)
          (ou2 : S.of_kind_foo unknown_or_join)
          : S.of_kind_foo unknown_or_join * equations =
      let resolver = env1.resolver in
      if env1 == env2 && ou1 == ou2 then ou1, Equations.create ()
      else
        match ou1, ou2 with
        | Unknown, ou2 -> ou2, Equations.create ()
        | ou1, Unknown -> ou1, Equations.create ()
        | Join of_kind_foos1, Join of_kind_foos2 ->
          let of_kind_foos, equations_from_meet =
            List.fold_left
              (fun (of_kind_foos, equations_from_meet) of_kind_foo ->
                let new_equations_from_meet = ref (Equations.create ()) in
                let of_kind_foos =
                  Misc.Stdlib.List.filter_map (fun of_kind_foo' ->
                      let meet =
                        S.meet_of_kind_foo env1 env2 of_kind_foo of_kind_foo'
                      in
                      match meet with
                      | Ok (of_kind_foo, new_equations_from_meet') ->
                        new_equations_from_meet :=
                          Meet_and_join.meet_equations ~resolver
                            new_equations_from_meet' !new_equations_from_meet;
                        Some of_kind_foo
                      | Bottom -> None)
                    of_kind_foos
                in
                let equations_from_meet =
                  Meet_and_join.meet_equations ~resolver
                    equations_from_meet !new_equations_from_meet;
                in
                of_kind_foos, equations_from_meet)
              (of_kind_foos2, Equations.create ())
              of_kind_foos1
          in
          let same_as input_of_kind_foos =
            List.compare_lengths input_of_kind_foos of_kind_foos = 0
              && List.for_all2 (fun input_of_kind_foo of_kind_foo ->
                     input_of_kind_foo == of_kind_foo)
                   input_of_kind_foos of_kind_foos
          in
          if same_as of_kind_foos1 then ou1, equations_from_meet
          else if same_as of_kind_foos2 then ou2, equations_from_meet
          else Join of_kind_foos, equations_from_meet

    and meet_ty env1 env2
          (or_alias1 : S.of_kind_foo ty)
          (or_alias2 : S.of_kind_foo ty)
          : S.of_kind_foo ty * equations =
      let resolver = env1.resolver in
      if env1 == env2 && or_alias1 == or_alias2 then
        or_alias1, Equations.create ()
      else
        let unknown_or_join1, canonical_name1 =
          Typing_environment0.
              resolve_aliases_and_squash_unresolved_names_on_ty'
            env1
            ~kind:S.kind
            ~force_to_kind:S.force_to_kind
            ~unknown:(No_alias Unknown)
            ~print_ty:S.print_ty
            or_alias1
        in
        let unknown_or_join2, canonical_name2 =
          Typing_environment0.
              resolve_aliases_and_squash_unresolved_names_on_ty'
            env2
            ~kind:S.kind
            ~force_to_kind:S.force_to_kind
            ~unknown:(No_alias Unknown)
            ~print_ty:S.print_ty
            or_alias2
        in
        match canonical_name1, canonical_name2 with
        | Some name1, Some name2 when Name.equal name1 name2 ->
          Equals name1, Equations.create ()
        | Some name1, Some name2 ->
          (* N.B. This needs to respect the [bias_towards] argument on the
             [meet] function exposed in the interface (below). *)
          let level1 = Typing_environment0.scope_level env1 name1 in
          let level2 = Typing_environment0.scope_level env2 name2 in
          let meet_unknown_or_join, equations_from_meet =
            meet_on_unknown_or_join env1 env2
              unknown_or_join1 unknown_or_join2
          in
          let meet_ty = S.to_type (No_alias meet_unknown_or_join) in
          let equations_from_meet =
            Equations.add_or_replace ~resolver equations_from_meet
              name1 level1 meet_ty
          in
          let equations_from_meet =
            Equations.add_or_replace ~resolver equations_from_meet
              name2 level2 (S.to_type (Equals name1))
          in
          Equals name1, equations_from_meet
        | Some name1, None ->
          let level1 = Typing_environment0.scope_level env1 name1 in
          let meet_unknown_or_join, equations_from_meet =
            meet_on_unknown_or_join env1 env2
              unknown_or_join1 unknown_or_join2
          in
          let meet_ty = S.to_type (No_alias meet_unknown_or_join) in
          let equations_from_meet =
            Equations.add_or_replace ~resolver equations_from_meet
              name1 level1 meet_ty
          in
          Equals name1, equations_from_meet
        | None, Some name2 ->
          let level2 = Typing_environment0.scope_level env2 name2 in
          let meet_unknown_or_join, equations_from_meet =
            meet_on_unknown_or_join env1 env2
              unknown_or_join1 unknown_or_join2
          in
          let meet_ty = S.to_type (No_alias meet_unknown_or_join) in
          let equations_from_meet =
            Equations.add_or_replace ~resolver equations_from_meet
              name2 level2 meet_ty
          in
          Equals name2, equations_from_meet
        | None, None ->
          let unknown_or_join, equations_from_meet =
            meet_on_unknown_or_join env1 env2
              unknown_or_join1 unknown_or_join2
          in
          if unknown_or_join == unknown_or_join1 then begin
            assert (match or_alias1 with No_alias _ -> true | _ -> false);
            or_alias1, equations_from_meet
          end else if unknown_or_join == unknown_or_join2 then begin
            assert (match or_alias2 with No_alias _ -> true | _ -> false);
            or_alias2, equations_from_meet
          end else begin
            No_alias unknown_or_join, equations_from_meet
          end
  end and Meet_and_join_value : sig
    include Meet_and_join
      with type of_kind_foo := of_kind_value
  end = Make_meet_and_join (struct
    type of_kind_foo = of_kind_value

    let kind = K.value ()

    let to_type ty : t = { descr = Value ty; phantom = None; }
    let force_to_kind = force_to_kind_value
    let print_ty = print_ty_value

    let meet_immediate_case env1 _env2
          ({ equations = equations1; } : immediate_case)
          ({ equations = equations2; } : immediate_case)
          : immediate_case =
      let resolver = env1.resolver in
      let equations =
        Meet_and_join.meet_equations ~resolver equations1 equations2
      in
      { equations; }

    let join_immediate_case env1 _env2
          ({ equations = equations1; } : immediate_case)
          ({ equations = equations2; } : immediate_case)
          : immediate_case =
      let resolver = env1.resolver in
      let equations =
        Meet_and_join.join_equations ~resolver equations1 equations2
      in
      { equations; }

    let meet_immediates env1 env2 immediates1 immediates2 : _ Or_bottom.t =
      let immediates =
        Immediate.Map.inter_merge (fun imm1 imm2 ->
            meet_immediate_case env1 env2 imm1 imm2)
          immediates1
          immediates2
      in
      if Immediate.Map.is_empty immediates then Bottom
      else Ok immediates

    let join_immediates env1 env2 immediates1 immediates2 =
      Immediate.Map.union_merge (fun imm1 imm2 ->
          join_immediate_case env1 env2 imm1 imm2)
        immediates1
        immediates2

    let meet_singleton_block env1 env2
          ({ equations = equations1;
             fields = fields1;
           } : singleton_block)
          ({ equations = equations2;
             fields = fields2;
           } : singleton_block) : singleton_block * equations =
      let resolver = env1.resolver in
      let equations =
        Meet_and_join.meet_equations ~resolver equations1 equations2
      in
      assert (Array.length fields1 = Array.length fields2);
      let equations_from_meet = ref (Equations.create ()) in
      let fields =
        Array.map2
          (fun (field1 : _ mutable_or_immutable)
               (field2 : _ mutable_or_immutable) : _ mutable_or_immutable ->
            match field1, field2 with
            | Mutable, _ | _, Mutable -> Mutable
            | Immutable field1, Immutable field2 ->
              let field, new_equations_from_meet =
                Meet_and_join.meet ~bias_towards:(env1, field1) (env2, field2)
              in
              equations_from_meet :=
                Meet_and_join.meet_equations ~resolver new_equations_from_meet
                  !equations_from_meet;
              Immutable field)
          fields1
          fields2
      in
      { equations;
        fields;
      }, !equations_from_meet

    let join_singleton_block env1 env2
          ({ equations = equations1;
             fields = fields1;
           } : singleton_block)
          ({ equations = equations2;
             fields = fields2;
           } : singleton_block) : singleton_block =
      let resolver = env1.resolver in
      let equations =
        Meet_and_join.join_equations ~resolver equations1 equations2
      in
      assert (Array.length fields1 = Array.length fields2);
      let fields =
        Array.map2
          (fun (field1 : _ mutable_or_immutable)
               (field2 : _ mutable_or_immutable) : _ mutable_or_immutable ->
            match field1, field2 with
            | Mutable, _ | _, Mutable -> Mutable
            | Immutable field1, Immutable field2 ->
              Immutable (Meet_and_join.join (env1, field1) (env2, field2)))
          fields1
          fields2
      in
      { equations;
        fields;
      }

    let meet_block_cases env1 env2
          ((Blocks { by_length = singleton_blocks1; }) : block_cases)
          ((Blocks { by_length = singleton_blocks2; }) : block_cases)
          : (block_cases * equations) Or_bottom.t =
      let resolver = env1.resolver in
      let equations_from_meet = ref (Equations.create ()) in
      let by_length =
        Targetint.OCaml.Map.inter_merge
          (fun singleton_block1 singleton_block2 ->
            let singleton_block, new_equations_from_meet =
              meet_singleton_block env1 env2
                singleton_block1 singleton_block2
            in
            equations_from_meet :=
              Meet_and_join.meet_equations ~resolver new_equations_from_meet
                !equations_from_meet;
            singleton_block)
          singleton_blocks1
          singleton_blocks2
      in
      if Targetint.OCaml.Map.is_empty by_length then Bottom
      else Ok (((Blocks { by_length; }) : block_cases), !equations_from_meet)

    let join_block_cases env1 env2
          ((Blocks { by_length = singleton_blocks1; }) : block_cases)
          ((Blocks { by_length = singleton_blocks2; }) : block_cases)
          : block_cases =
      let by_length =
        Targetint.OCaml.Map.union_merge
          (fun singleton_block1 singleton_block2 ->
            join_singleton_block env1 env2
              singleton_block1 singleton_block2)
          singleton_blocks1
          singleton_blocks2
      in
      Blocks { by_length; }

    let meet_blocks env1 env2 blocks1 blocks2 : _ Or_bottom.t =
      let resolver = env1.resolver in
      let equations_from_meet = ref (Equations.create ()) in
      let blocks =
        Tag.Map.inter (fun block_cases1 block_cases2 ->
            match meet_block_cases env1 env2 block_cases1 block_cases2 with
            | Ok (block_cases, new_equations_from_meet) ->
              equations_from_meet :=
                Meet_and_join.meet_equations ~resolver new_equations_from_meet
                  !equations_from_meet;
              Some block_cases
            | Bottom -> None)
          blocks1
          blocks2
      in
      if Tag.Map.is_empty blocks then Bottom
      else Ok (blocks, !equations_from_meet)

    let join_blocks env1 env2 blocks1 blocks2 =
      Tag.Map.union_merge (fun block_cases1 block_cases2 ->
          join_block_cases env1 env2 block_cases1 block_cases2)
        blocks1
        blocks2

    let meet_blocks_and_tagged_immediates env1 env2
          { blocks = blocks1; immediates = imms1; is_int = is_int1;
            get_tag = get_tag1; }
          { blocks = blocks2; immediates = imms2; is_int = is_int2;
            get_tag = get_tag2; }
          : (blocks_and_tagged_immediates * equations) Or_bottom.t =
      let resolver = env1.resolver in
      let (blocks : _ or_unknown), equations_from_meet =
        match blocks1, blocks2 with
        | Unknown, _ -> blocks2, Equations.create ()
        | _, Unknown -> blocks1, Equations.create ()
        | Known blocks1, Known blocks2 ->
          match meet_blocks env1 env2 blocks1 blocks2 with
          | Bottom -> Known Tag.Map.empty, Equations.create ()
          | Ok (blocks, equations_from_meet) ->
            Known blocks, equations_from_meet
      in
      let immediates : _ or_unknown =
        match imms1, imms2 with
        | Unknown, _ -> imms2
        | _, Unknown -> imms1
        | Known imms1, Known imms2 ->
          match meet_immediates env1 env2 imms1 imms2 with
          | Bottom -> Known Immediate.Map.empty
          | Ok immediates -> Known immediates
      in
      let is_int =
        match is_int1, is_int2 with
        | None, None -> None
        | None, Some _ -> is_int2
        | Some _, None -> is_int1
        | Some is_int1, Some is_int2 ->
          if Name.equal is_int1 is_int2 then Some is_int1 else None
      in
      let get_tag =
        match get_tag1, get_tag2 with
        | None, None -> None
        | None, Some _ -> get_tag2
        | Some _, None -> get_tag1
        | Some get_tag1, Some get_tag2 ->
          if Name.equal get_tag1 get_tag2 then Some get_tag1 else None
      in
      let is_bottom =
        begin match blocks with
        | Known blocks when Tag.Map.is_empty blocks -> true
        | Known _ | Unknown -> false
        end
          && begin match immediates with
             | Known imms when Immediate.Map.is_empty imms -> true
             | Known _ | Unknown -> false
             end
      in
      (* CR mshinwell: If we end up with [Bottom], should that be signalled
         as a judgement? *)
      if is_bottom then Bottom
      else
        let equations_from_meet =
          match immediates with
          | Unknown -> equations_from_meet
          | Known imms ->
            if not (Immediate.Map.is_empty imms) then equations_from_meet
            else  (* CR mshinwell: This should maybe meet across all blocks *)
              match blocks with
              | Unknown -> equations_from_meet
              | Known blocks ->
                match Tag.Map.get_singleton blocks with
                | None -> equations_from_meet
                | Some (_, Blocks { by_length; }) ->
                  (* CR mshinwell: This should remove equations propagated
                     upwards from the block cases *)
                  match Targetint.OCaml.Map.get_singleton by_length with
                  | None -> equations_from_meet
                  | Some (_, singleton_block) ->
                    Meet_and_join.meet_equations ~resolver
                      singleton_block.equations equations_from_meet
        in
        Ok ({ blocks; immediates; is_int; get_tag; }, equations_from_meet)

    let join_blocks_and_tagged_immediates env1 env2
          { blocks = blocks1; immediates = imms1; is_int = is_int1;
            get_tag = get_tag1; }
          { blocks = blocks2; immediates = imms2; is_int = is_int2;
            get_tag = get_tag2; }
          : blocks_and_tagged_immediates =
      let blocks : _ or_unknown =
        match blocks1, blocks2 with
        | Unknown, _ | _, Unknown -> Unknown
        | Known blocks1, Known blocks2 ->
          Known (join_blocks env1 env2 blocks1 blocks2)
      in
      let immediates : _ or_unknown =
        match imms1, imms2 with
        | Unknown, _ | _, Unknown -> Unknown
        | Known imms1, Known imms2 ->
          Known (join_immediates env1 env2 imms1 imms2)
      in
      (* CR mshinwell: Refactor between is_int / get_tag; then share with
         meet. *)
      let is_int =
        match is_int1, is_int2 with
        | None, None -> None
        | None, Some _ -> is_int2
        | Some _, None -> is_int1
        | Some is_int1, Some is_int2 ->
          if Name.equal is_int1 is_int2 then Some is_int1 else None
      in
      let get_tag =
        match get_tag1, get_tag2 with
        | None, None -> None
        | None, Some _ -> get_tag2
        | Some _, None -> get_tag1
        | Some get_tag1, Some get_tag2 ->
          if Name.equal get_tag1 get_tag2 then Some get_tag1 else None
      in
      { blocks; immediates; is_int; get_tag; }

    let meet_of_kind_foo env1 env2
          (of_kind1 : of_kind_value) (of_kind2 : of_kind_value)
          : (of_kind_value * equations) Or_bottom.t =
      let resolver = env1.resolver in
      match of_kind1, of_kind2 with
      | Blocks_and_tagged_immediates blocks_imms1,
          Blocks_and_tagged_immediates blocks_imms2 ->
        let blocks_imms =
          meet_blocks_and_tagged_immediates env1 env2
            blocks_imms1 blocks_imms2
        in
        begin match blocks_imms with
        | Ok (blocks_imms, equations_from_meet) ->
          Ok (Blocks_and_tagged_immediates blocks_imms, equations_from_meet)
        | Bottom -> Bottom
        end
      | Boxed_number (Boxed_float n1),
          Boxed_number (Boxed_float n2) ->
        let (n : _ ty_naked_number), equations_from_meet =
          Meet_and_join_naked_float.meet_ty env1 env2 n1 n2
        in
        Ok (Boxed_number (Boxed_float n), equations_from_meet)
      | Boxed_number (Boxed_int32 n1),
          Boxed_number (Boxed_int32 n2) ->
        let (n : _ ty_naked_number), equations_from_meet =
          Meet_and_join_naked_int32.meet_ty env1 env2 n1 n2
        in
        Ok (Boxed_number (Boxed_int32 n), equations_from_meet)
      | Boxed_number (Boxed_int64 n1),
          Boxed_number (Boxed_int64 n2) ->
        let (n : _ ty_naked_number), equations_from_meet =
          Meet_and_join_naked_int64.meet_ty env1 env2 n1 n2
        in
        Ok (Boxed_number (Boxed_int64 n), equations_from_meet)
      | Boxed_number (Boxed_nativeint n1),
          Boxed_number (Boxed_nativeint n2) ->
        let (n : _ ty_naked_number), equations_from_meet =
          Meet_and_join_naked_nativeint.meet_ty env1 env2 n1 n2
        in
        Ok (Boxed_number (Boxed_nativeint n), equations_from_meet)
      | Closures closures1, Closures closures2 ->
        let equations_from_meet = ref (Equations.create ()) in
        let closures =
          Closure_id.Map.inter
            (fun (closures_entry1 : closures_entry)
                 (closures_entry2 : closures_entry) : closures_entry option ->
              let set1 = closures_entry1.set_of_closures in
              let set2 = closures_entry2.set_of_closures in
              let set, new_equations_from_meet =
                Meet_and_join_fabricated.meet_ty env1 env2 set1 set2
              in
              if ty_is_obviously_bottom set then begin
                None
              end else begin
                equations_from_meet :=
                  Meet_and_join.meet_equations ~resolver
                    new_equations_from_meet !equations_from_meet;
                Some { set_of_closures = set; }
              end)
            closures1
            closures2
        in
        if Closure_id.Map.is_empty closures then Bottom
        else Ok (Closures closures, !equations_from_meet)
      | String strs1, String strs2 ->
        let strs = String_info.Set.inter strs1 strs2 in
        if String_info.Set.is_empty strs then Bottom
        else Ok (String strs, Equations.create ())
      | (Blocks_and_tagged_immediates _
          | Boxed_number _
          | Closures _
          | String _), _ ->
        Bottom

    let join_of_kind_foo env1 env2
          (of_kind1 : of_kind_value) (of_kind2 : of_kind_value)
          : of_kind_value or_unknown =
      match of_kind1, of_kind2 with
      | Blocks_and_tagged_immediates blocks_imms1,
          Blocks_and_tagged_immediates blocks_imms2 ->
        let blocks_imms =
          join_blocks_and_tagged_immediates env1 env2
            blocks_imms1 blocks_imms2
        in
        Known (Blocks_and_tagged_immediates blocks_imms)
      | Boxed_number (Boxed_float n1), Boxed_number (Boxed_float n2) ->
        let n : _ ty_naked_number =
          Meet_and_join_naked_float.join_ty env1 env2 n1 n2
        in
        Known (Boxed_number (Boxed_float n))
      | Boxed_number (Boxed_int32 n1),
          Boxed_number (Boxed_int32 n2) ->
        let n : _ ty_naked_number =
          Meet_and_join_naked_int32.join_ty env1 env2 n1 n2
        in
        Known (Boxed_number (Boxed_int32 n))
      | Boxed_number (Boxed_int64 n1),
          Boxed_number (Boxed_int64 n2) ->
        let n : _ ty_naked_number =
          Meet_and_join_naked_int64.join_ty env1 env2 n1 n2
        in
        Known (Boxed_number (Boxed_int64 n))
      | Boxed_number (Boxed_nativeint n1),
          Boxed_number (Boxed_nativeint n2) ->
        let n : _ ty_naked_number =
          Meet_and_join_naked_nativeint.join_ty env1 env2 n1 n2
        in
        Known (Boxed_number (Boxed_nativeint n))
      | Closures closures1, Closures closures2 ->
        let closures =
          Closure_id.Map.union_merge
            (fun (closures_entry1 : closures_entry)
                 (closures_entry2 : closures_entry) : closures_entry ->
              let set1 = closures_entry1.set_of_closures in
              let set2 = closures_entry2.set_of_closures in
              let set =
                Meet_and_join_fabricated.join_ty env1 env2 set1 set2
              in
              { set_of_closures = set; })
            closures1
            closures2
        in
        Known (Closures closures)
      | String strs1, String strs2 ->
        let strs = String_info.Set.union strs1 strs2 in
        Known (String strs)
      | (Blocks_and_tagged_immediates _
          | Boxed_number _
          | Closures _
          | String _), _ ->
        Unknown
  end) and Meet_and_join_naked_immediate : sig
    (* CR mshinwell: See if we can abstract these naked number cases some
       more? *)
    include Meet_and_join
      with type of_kind_foo := Immediate.Set.t of_kind_naked_number
  end = Make_meet_and_join (struct
    type of_kind_foo = Immediate.Set.t of_kind_naked_number

    let kind = K.naked_immediate ()

    let to_type ty : t =
      { descr = Naked_number (ty, Naked_immediate);
        phantom = None;
      }

    let force_to_kind = force_to_kind_naked_immediate

    let print_ty = print_ty_naked_number

    let meet_of_kind_foo _env1 _env2
          (of_kind1 : Immediate.Set.t of_kind_naked_number)
          (of_kind2 : Immediate.Set.t of_kind_naked_number)
          : (Immediate.Set.t of_kind_naked_number * equations)
              Or_bottom.t =
      match of_kind1, of_kind2 with
      | Immediate fs1, Immediate fs2 ->
        let fs = Immediate.Set.inter fs1 fs2 in
        if Immediate.Set.is_empty fs then Bottom
        else Ok (Immediate fs, Equations.create ())
      | _, _ -> Bottom

    let join_of_kind_foo _env1 _env2
          (of_kind1 : Immediate.Set.t of_kind_naked_number)
          (of_kind2 : Immediate.Set.t of_kind_naked_number)
          : Immediate.Set.t of_kind_naked_number or_unknown =
      match of_kind1, of_kind2 with
      | Immediate fs1, Immediate fs2 ->
        let fs = Immediate.Set.union fs1 fs2 in
        Known (Immediate fs)
      | _, _ -> Unknown
  end) and Meet_and_join_naked_float : sig
    (* CR mshinwell: See if we can abstract these naked number cases some
       more? *)
    include Meet_and_join
      with type of_kind_foo := Float_by_bit_pattern.Set.t of_kind_naked_number
  end = Make_meet_and_join (struct
    type of_kind_foo = Float_by_bit_pattern.Set.t of_kind_naked_number

    let kind = K.naked_float ()

    let to_type ty =
      { descr = Naked_number (ty, Naked_float);
        phantom = None;
      }

    let force_to_kind = force_to_kind_naked_float
    let print_ty = print_ty_naked_number

    let meet_of_kind_foo _env1 _env2
          (of_kind1 : Float_by_bit_pattern.Set.t of_kind_naked_number)
          (of_kind2 : Float_by_bit_pattern.Set.t of_kind_naked_number)
          : (Float_by_bit_pattern.Set.t of_kind_naked_number
              * equations) Or_bottom.t =
      match of_kind1, of_kind2 with
      | Float fs1, Float fs2 ->
        let fs = Float_by_bit_pattern.Set.inter fs1 fs2 in
        if Float_by_bit_pattern.Set.is_empty fs then Bottom
        else Ok (Float fs, Equations.create ())
      | _, _ -> Bottom

    let join_of_kind_foo _env1 _env2
          (of_kind1 : Float_by_bit_pattern.Set.t of_kind_naked_number)
          (of_kind2 : Float_by_bit_pattern.Set.t of_kind_naked_number)
          : Float_by_bit_pattern.Set.t of_kind_naked_number or_unknown =
      match of_kind1, of_kind2 with
      | Float fs1, Float fs2 ->
        let fs = Float_by_bit_pattern.Set.union fs1 fs2 in
        Known (Float fs)
      | _, _ -> Unknown
  end) and Meet_and_join_naked_int32 : sig
    include Meet_and_join
      with type of_kind_foo := Int32.Set.t of_kind_naked_number
  end = Make_meet_and_join (struct
    type of_kind_foo = Int32.Set.t of_kind_naked_number

    let kind = K.naked_int32 ()

    let to_type ty : t =
      { descr = Naked_number (ty, Naked_int32);
        phantom = None;
      }

    let force_to_kind = force_to_kind_naked_int32
    let print_ty = print_ty_naked_number

    let meet_of_kind_foo _env1 _env2
          (of_kind1 : Int32.Set.t of_kind_naked_number)
          (of_kind2 : Int32.Set.t of_kind_naked_number)
          : (Int32.Set.t of_kind_naked_number * equations) Or_bottom.t =
      match of_kind1, of_kind2 with
      | Int32 is1, Int32 is2 ->
        let is = Int32.Set.inter is1 is2 in
        if Int32.Set.is_empty is then Bottom
        else Ok (Int32 is, Equations.create ())
      | _, _ -> Bottom

    let join_of_kind_foo _env1 _env2
          (of_kind1 : Int32.Set.t of_kind_naked_number)
          (of_kind2 : Int32.Set.t of_kind_naked_number)
          : Int32.Set.t of_kind_naked_number or_unknown =
      match of_kind1, of_kind2 with
      | Int32 is1, Int32 is2 ->
        let is = Int32.Set.union is1 is2 in
        Known (Int32 is)
      | _, _ -> Unknown
  end) and Meet_and_join_naked_int64 : sig
    include Meet_and_join
      with type of_kind_foo := Int64.Set.t of_kind_naked_number
  end = Make_meet_and_join (struct
    type of_kind_foo = Int64.Set.t of_kind_naked_number

    let kind = K.naked_int64 ()

    let to_type ty : t =
      { descr = Naked_number (ty, Naked_int64);
        phantom = None;
      }

    let force_to_kind = force_to_kind_naked_int64
    let print_ty = print_ty_naked_number

    let meet_of_kind_foo _env1 _env2
          (of_kind1 : Int64.Set.t of_kind_naked_number)
          (of_kind2 : Int64.Set.t of_kind_naked_number)
          : (Int64.Set.t of_kind_naked_number * equations) Or_bottom.t =
      match of_kind1, of_kind2 with
      | Int64 is1, Int64 is2 ->
        let is = Int64.Set.inter is1 is2 in
        if Int64.Set.is_empty is then Bottom
        else Ok (Int64 is, Equations.create ())
      | _, _ -> Bottom

    let join_of_kind_foo _env1 _env2
          (of_kind1 : Int64.Set.t of_kind_naked_number)
          (of_kind2 : Int64.Set.t of_kind_naked_number)
          : Int64.Set.t of_kind_naked_number or_unknown =
      match of_kind1, of_kind2 with
      | Int64 is1, Int64 is2 ->
        let is = Int64.Set.union is1 is2 in
        Known (Int64 is)
      | _, _ -> Unknown
  end) and Meet_and_join_naked_nativeint : sig
    include Meet_and_join
      with type of_kind_foo := Targetint.Set.t of_kind_naked_number
  end = Make_meet_and_join (struct
    type of_kind_foo = Targetint.Set.t of_kind_naked_number

    let kind = K.naked_nativeint ()

    let to_type ty : t =
      { descr = Naked_number (ty, Naked_nativeint);
        phantom = None;
      }

    let force_to_kind = force_to_kind_naked_nativeint
    let print_ty = print_ty_naked_number

    let meet_of_kind_foo _env1 _env2
          (of_kind1 : Targetint.Set.t of_kind_naked_number)
          (of_kind2 : Targetint.Set.t of_kind_naked_number)
          : (Targetint.Set.t of_kind_naked_number * equations) Or_bottom.t =
      match of_kind1, of_kind2 with
      | Nativeint is1, Nativeint is2 ->
        let is = Targetint.Set.inter is1 is2 in
        if Targetint.Set.is_empty is then Bottom
        else Ok (Nativeint is, Equations.create ())
      | _, _ -> Bottom

    let join_of_kind_foo _env1 _env2
          (of_kind1 : Targetint.Set.t of_kind_naked_number)
          (of_kind2 : Targetint.Set.t of_kind_naked_number)
          : Targetint.Set.t of_kind_naked_number or_unknown =
      match of_kind1, of_kind2 with
      | Nativeint is1, Nativeint is2 ->
        let is = Targetint.Set.union is1 is2 in
        Known (Nativeint is)
      | _, _ -> Unknown
  end) and Meet_and_join_fabricated : sig
    include Meet_and_join
      with type of_kind_foo := of_kind_fabricated
  end = Make_meet_and_join (struct
    type of_kind_foo = of_kind_fabricated

    let kind = K.fabricated ()

    let to_type ty : t =
      { descr = Fabricated ty;
        phantom = None;
      }

    let force_to_kind = force_to_kind_fabricated
    let print_ty = print_ty_fabricated

    (* CR mshinwell: We need to work out how to stop direct call
       surrogates from being dropped e.g. when in a second round, a
       function type (with a surrogate) propagated from the first round is
       put into a meet with a type for the same function, but a new
       surrogate. *)
    let meet_closure env1 env2
          (closure1 : closure) (closure2 : closure)
          : (closure * equations) Or_bottom.t =
      if env1 == env2 && closure1 == closure2 then begin
        Ok (closure1, Equations.create ())
      end else begin
        let resolver = env1.resolver in
        let cannot_prove_different ~params1 ~params2
              ~param_names1 ~param_names2 ~result1 ~result2
              ~result_equations1 ~result_equations2 : _ Or_bottom.t =
          let same_arity = List.compare_lengths params1 params2 = 0 in
          let same_num_results = List.compare_lengths result1 result2 = 0 in
          let equations_from_meet = ref (Equations.create ()) in
          let has_bottom params = List.exists is_obviously_bottom params in
          let params_changed = ref Neither in
          let params : _ Or_bottom.t =
            if not same_arity then Bottom
            else
              let params =
                List.map2 (fun t1 t2 ->
                    let t, new_equations_from_meet =
                      Meet_and_join.meet ~bias_towards:(env1, t1) (env2, t2)
                    in
                    if not (t == t1) then begin
                      params_changed := join_changes !params_changed Left
                    end;
                    if not (t == t2) then begin
                      params_changed := join_changes !params_changed Right
                    end;
                    equations_from_meet :=
                      Meet_and_join.meet_equations ~resolver
                        new_equations_from_meet !equations_from_meet;
                    t)
                  params1
                  params2
              in
              if has_bottom params then Bottom
              else Ok params
          in
          let env_for_result env ~params ~param_names =
            match param_names with
            | None -> env
            | Some param_names ->
              List.fold_left2 (fun env param param_ty ->
                  let param_name = Parameter.name param in
                  (* CR mshinwell: This level shouldn't be hard-coded *)
                  let level = Scope_level.initial in
                  Typing_environment0.add_or_replace_meet env
                    param_name level param_ty)
                env
                param_names params
          in
          let result_changed = ref Neither in
          let result : _ Or_bottom.t =
            if not same_num_results then Bottom
            else
              let result =
                List.map2 (fun t1 t2 ->
                    let result_equations1 =
                      Equations.to_typing_environment ~resolver:env1.resolver
                       result_equations1
                    in
                    let result_equations2 =
                      Equations.to_typing_environment ~resolver:env1.resolver
                       result_equations2
                    in
                    let result_env1 =
                      Typing_environment0.meet
                        (env_for_result env1 ~params:params1
                          ~param_names:param_names1)
                        result_equations1
                    in
                    let result_env2 =
                      Typing_environment0.meet
                        (env_for_result env2 ~params:params2
                          ~param_names:param_names2)
                        result_equations2
                    in
                    let t, new_equations_from_meet =
                      Meet_and_join.meet ~bias_towards:(result_env1, t1)
                        (result_env2, t2)
                    in
                    if not (t == t1) then begin
                      result_changed := join_changes !result_changed Left
                    end;
                    if not (t == t2) then begin
                      result_changed := join_changes !result_changed Right
                    end;
                    equations_from_meet :=
                      Meet_and_join.meet_equations ~resolver
                        new_equations_from_meet !equations_from_meet;
                    t)
                  result1
                  result2
              in
              if has_bottom result then Bottom
              else Ok result
          in
          let result_equations =
            Meet_and_join.meet_equations ~resolver:env1.resolver
              result_equations1 result_equations2
          in
          let result_equations_changed : changes =
            let changed1 =
              not (Equations.phys_equal result_equations1 result_equations)
            in
            let changed2 =
              not (Equations.phys_equal result_equations2 result_equations)
            in
            match changed1, changed2 with
            | false, false -> Neither
            | true, false -> Left
            | false, true -> Right
            | true, true -> Both
          in
          match params, result with
          | Ok params, Ok result ->
            let changed =
              join_changes !params_changed
                (join_changes !result_changed result_equations_changed)
            in
            Ok (params, changed, result, result_equations, !equations_from_meet)
          | _, _ -> Bottom
        in
        let function_decls : _ Or_bottom.t =
          match closure1.function_decls, closure2.function_decls with
          | Inlinable inlinable1, Inlinable inlinable2 ->
            let params1 = List.map snd inlinable1.params in
            let params2 = List.map snd inlinable2.params in
            let param_names1 = List.map fst inlinable1.params in
            let param_names2 = List.map fst inlinable2.params in
            let result =
              cannot_prove_different ~params1 ~params2
                ~param_names1:(Some param_names1)
                ~param_names2:(Some param_names2)
                ~result1:inlinable1.result
                ~result2:inlinable2.result
                ~result_equations1:inlinable1.result_equations
                ~result_equations2:inlinable2.result_equations
            in
            begin match result with
            | Ok (params, changed, result, result_equations,
                  equations_from_meet) ->
              (* [closure1.function_decls] and [closure2.function_decls] may be
                 different, but we cannot prove it.  We arbitrarily pick
                 [closure1.function_decls] to return, with parameter and result
                 types refined. *)
              let params =
                List.map2 (fun (param, _old_ty) new_ty ->
                    param, new_ty)
                  inlinable1.params
                  params
              in
              begin match changed with
              | Neither -> Ok (closure1.function_decls, equations_from_meet)
              | Left -> Ok (closure2.function_decls, equations_from_meet)
              | Right -> Ok (closure1.function_decls, equations_from_meet)
              | Both ->
                Ok (Inlinable { inlinable1 with
                  params;
                  result;
                  result_equations;
                }, equations_from_meet)
              end
            | Bottom ->
              (* [closure1] and [closure2] are definitely different. *)
              Bottom
            end
          | Non_inlinable None, Non_inlinable None ->
            Ok (Non_inlinable None, Equations.create ())
          | Non_inlinable (Some non_inlinable), Non_inlinable None
          | Non_inlinable None, Non_inlinable (Some non_inlinable) ->
            (* We can arbitrarily pick one side or the other: we choose the
               side which gives a more precise type. *)
            Ok (Non_inlinable (Some non_inlinable), Equations.create ())
          | Non_inlinable None, Inlinable inlinable
          | Inlinable inlinable, Non_inlinable None ->
            (* Likewise. *)
            Ok (Inlinable inlinable, Equations.create ())
          | Non_inlinable (Some non_inlinable1),
              Non_inlinable (Some non_inlinable2) ->
            let result =
              cannot_prove_different
                ~params1:non_inlinable1.params
                ~params2:non_inlinable2.params
                ~param_names1:None
                ~param_names2:None
                ~result1:non_inlinable1.result
                ~result2:non_inlinable2.result
                ~result_equations1:non_inlinable1.result_equations
                ~result_equations2:non_inlinable2.result_equations
            in
            begin match result with
            | Ok (params, _params_changed, result, result_equations,
                  equations_from_meet) ->
              let non_inlinable_function_decl =
                { non_inlinable1 with
                  params;
                  result;
                  result_equations;
                }
              in
              Ok (Non_inlinable (Some non_inlinable_function_decl),
                equations_from_meet)
            | Bottom ->
              Bottom
            end
          | Non_inlinable (Some non_inlinable), Inlinable inlinable
          | Inlinable inlinable, Non_inlinable (Some non_inlinable) ->
            let params1 = List.map snd inlinable.params in
            let param_names1 = List.map fst inlinable.params in
            let result =
              cannot_prove_different
                ~params1
                ~params2:non_inlinable.params
                ~param_names1:(Some param_names1)
                ~param_names2:None
                ~result1:inlinable.result
                ~result2:non_inlinable.result
                ~result_equations1:inlinable.result_equations
                ~result_equations2:non_inlinable.result_equations
            in
            begin match result with
            | Ok (params, _params_changed, result, result_equations,
                  equations_from_meet) ->
              (* For the arbitrary choice, we pick the inlinable declaration,
                 since it gives more information. *)
              let params =
                List.map2 (fun (param, _old_ty) new_ty -> param, new_ty)
                  inlinable.params
                  params
              in
              let inlinable_function_decl =
                { inlinable with
                  params;
                  result;
                  result_equations;
                }
              in
              Ok (Inlinable inlinable_function_decl, equations_from_meet)
            | Bottom ->
              Bottom
            end
        in
        match function_decls with
        | Bottom -> Bottom
        | Ok (function_decls, equations_from_meet) ->
          if function_decls == closure1.function_decls then
            Ok (closure1, equations_from_meet)
          else if function_decls == closure2.function_decls then
            Ok (closure2, equations_from_meet)
          else
            Ok (({ function_decls; } : closure), equations_from_meet)
      end

    let join_closure env1 env2
          (closure1 : closure) (closure2 : closure)
          : closure =
      if env1 == env2 && closure1 == closure2 then begin
        closure1
      end else begin
        let produce_non_inlinable ~params1 ~params2 ~result1 ~result2
              ~result_equations1 ~result_equations2
              ~direct_call_surrogate1 ~direct_call_surrogate2 =
          let same_arity =
            List.compare_lengths params1 params2 = 0
          in
          let same_num_results =
            List.compare_lengths result1 result2 = 0
          in
          if same_arity && same_num_results then
            let params =
              List.map2 (fun t1 t2 ->
                  Meet_and_join.join (env1, t1) (env2, t2))
                params1
                params2
            in
            (* XXX needs fixing as regards environments for the result, see
               meet function above *)
            let result =
              List.map2 (fun t1 t2 ->
                  Meet_and_join.join
                    (Equations.to_typing_environment ~resolver:env1.resolver
                       result_equations1, t1)
                    (Equations.to_typing_environment ~resolver:env2.resolver
                       result_equations2, t2))
                result1
                result2
            in
            let direct_call_surrogate =
              match direct_call_surrogate1, direct_call_surrogate2 with
              | Some closure_id1, Some closure_id2
                  when Closure_id.equal closure_id1 closure_id2 ->
                Some closure_id1
              | _, _ -> None
            in
            let result_equations =
              Meet_and_join.join_equations ~resolver:env1.resolver
                result_equations1 result_equations2
            in
            let non_inlinable : non_inlinable_function_declarations =
              { params;
                result;
                result_equations;
                direct_call_surrogate;
              }
            in
            Non_inlinable (Some non_inlinable)
          else
            Non_inlinable None
        in
        let function_decls : function_declarations =
          match closure1.function_decls, closure2.function_decls with
          | Non_inlinable None, _ | _, Non_inlinable None -> Non_inlinable None
          | Non_inlinable (Some non_inlinable1),
              Non_inlinable (Some non_inlinable2) ->
            produce_non_inlinable
              ~params1:non_inlinable1.params
              ~params2:non_inlinable2.params
              ~result1:non_inlinable1.result
              ~result2:non_inlinable2.result
              ~result_equations1:non_inlinable1.result_equations
              ~result_equations2:non_inlinable2.result_equations
              ~direct_call_surrogate1:non_inlinable1.direct_call_surrogate
              ~direct_call_surrogate2:non_inlinable2.direct_call_surrogate
          | Non_inlinable (Some non_inlinable), Inlinable inlinable
          | Inlinable inlinable, Non_inlinable (Some non_inlinable) ->
            let params1 = List.map snd inlinable.params in
            produce_non_inlinable
              ~params1
              ~params2:non_inlinable.params
              ~result1:inlinable.result
              ~result2:non_inlinable.result
              ~result_equations1:inlinable.result_equations
              ~result_equations2:non_inlinable.result_equations
              ~direct_call_surrogate1:inlinable.direct_call_surrogate
              ~direct_call_surrogate2:non_inlinable.direct_call_surrogate
          | Inlinable inlinable1, Inlinable inlinable2 ->
            if not (Code_id.equal inlinable1.code_id inlinable2.code_id)
            then begin
              let params1 = List.map snd inlinable1.params in
              let params2 = List.map snd inlinable2.params in
              produce_non_inlinable
                ~params1
                ~params2
                ~result1:inlinable1.result
                ~result2:inlinable2.result
                ~result_equations1:inlinable1.result_equations
                ~result_equations2:inlinable2.result_equations
                ~direct_call_surrogate1:inlinable1.direct_call_surrogate
                ~direct_call_surrogate2:inlinable2.direct_call_surrogate
            end else begin
              if !Clflags.flambda_invariant_checks then begin
                assert (Closure_origin.equal inlinable1.closure_origin
                  inlinable2.closure_origin);
                assert (Continuation.equal inlinable1.continuation_param
                  inlinable2.continuation_param);
                assert (Continuation.equal inlinable1.exn_continuation_param
                  inlinable2.exn_continuation_param);
                assert (Pervasives.(=) inlinable1.is_classic_mode
                  inlinable2.is_classic_mode);
                assert (List.compare_lengths inlinable1.params inlinable2.params
                  = 0);
                assert (List.compare_lengths inlinable1.result inlinable2.result
                  = 0);
                assert (Name_occurrences.equal inlinable1.free_names_in_body
                  inlinable2.free_names_in_body);
                assert (Pervasives.(=) inlinable1.stub inlinable2.stub);
                assert (Debuginfo.equal inlinable1.dbg inlinable2.dbg);
                assert (Pervasives.(=) inlinable1.inline inlinable2.inline);
                assert (Pervasives.(=) inlinable1.specialise
                  inlinable2.specialise);
                assert (Pervasives.(=) inlinable1.is_a_functor
                  inlinable2.is_a_functor);
                assert (Variable.Set.equal
                  (Lazy.force inlinable1.invariant_params)
                  (Lazy.force inlinable2.invariant_params));
                assert (Pervasives.(=)
                  (Lazy.force inlinable1.size)
                  (Lazy.force inlinable2.size));
                assert (Variable.equal inlinable1.my_closure
                  inlinable2.my_closure)
              end;
              (* Parameter types are treated covariantly. *)
              (* CR mshinwell: Add documentation for this -- the types provide
                 information about the calling context rather than the code of
                 the function. *)
              let result_equations =
                Meet_and_join.join_equations ~resolver:env1.resolver
                  inlinable1.result_equations
                  inlinable2.result_equations
              in
              let params =
                List.map2 (fun (param1, t1) (param2, t2) ->
                    assert (Parameter.equal param1 param2);
                    let t = Meet_and_join.join (env1, t1) (env2, t2) in
                    param1, t)
                  inlinable1.params
                  inlinable2.params
              in
              let result =
                List.map2 (fun t1 t2 ->
                    Meet_and_join.join
                      (Equations.to_typing_environment ~resolver:env1.resolver
                         inlinable1.result_equations, t1)
                      (Equations.to_typing_environment ~resolver:env2.resolver
                         inlinable2.result_equations, t2))
                  inlinable1.result
                  inlinable2.result
              in
              let direct_call_surrogate =
                match inlinable1.direct_call_surrogate,
                      inlinable2.direct_call_surrogate
                with
                | Some closure_id1, Some closure_id2
                    when Closure_id.equal closure_id1 closure_id2 ->
                  Some closure_id1
                | _, _ -> None
              in
              Inlinable {
                closure_origin = inlinable1.closure_origin;
                continuation_param = inlinable1.continuation_param;
                exn_continuation_param = inlinable1.exn_continuation_param;
                is_classic_mode = inlinable1.is_classic_mode;
                params;
                code_id = inlinable1.code_id;
                body = inlinable1.body;
                free_names_in_body = inlinable1.free_names_in_body;
                result;
                result_equations;
                stub = inlinable1.stub;
                dbg = inlinable1.dbg;
                inline = inlinable1.inline;
                specialise = inlinable1.specialise;
                is_a_functor = inlinable1.is_a_functor;
                invariant_params = inlinable1.invariant_params;
                size = inlinable1.size;
                direct_call_surrogate;
                my_closure = inlinable1.my_closure;
              }
            end
        in
        { function_decls; }
      end

    let meet_set_of_closures env1 env2
          (set1 : set_of_closures) (set2 : set_of_closures)
          : (set_of_closures * equations) Or_bottom.t =
      let resolver = env1.resolver in
      let equations_from_meet = ref (Equations.create ()) in
      (* CR mshinwell: Try to refactor this code to shorten it. *)
      let closures : _ extensibility =
        match set1.closures, set2.closures with
        | Exactly closures1, Exactly closures2 ->
          let closures =
            Closure_id.Map.inter (fun ty_fabricated1 ty_fabricated2 ->
                let ty_fabricated, new_equations_from_meet =
                  Meet_and_join_fabricated.meet_ty env1 env2
                    ty_fabricated1 ty_fabricated2
                in
                if ty_is_obviously_bottom ty_fabricated then begin
                  None
                end else begin
                  equations_from_meet :=
                    Meet_and_join.meet_equations ~resolver
                      new_equations_from_meet !equations_from_meet;
                  Some ty_fabricated
                end)
              closures1
              closures2
          in
          (* CR mshinwell: Try to move this check into the intersection
             operation above (although note we still need to check the
             cardinality) *)
          let same_as_closures old_closures =
            match
              Closure_id.Map.for_all2_opt (fun ty_fabricated1 ty_fabricated2 ->
                  ty_fabricated1 == ty_fabricated2)
                old_closures closures
            with
            | None -> false
            | Some same -> same
          in
          if same_as_closures closures1 then set1.closures
          else if same_as_closures closures2 then set2.closures
          else Exactly closures
        | Exactly closures1, Open closures2
        | Open closures2, Exactly closures1 ->
          let closures =
            Closure_id.Map.filter_map closures1 ~f:(fun closure_id ty1 ->
              match Closure_id.Map.find closure_id closures2 with
              | exception Not_found -> Some ty1
              | ty2 ->
                let ty_fabricated, new_equations_from_meet =
                  Meet_and_join_fabricated.meet_ty env1 env2 ty1 ty2
                in
                if ty_is_obviously_bottom ty_fabricated then begin
                  None
                end else begin
                  equations_from_meet :=
                    Meet_and_join.meet_equations ~resolver
                      new_equations_from_meet !equations_from_meet;
                  Some ty_fabricated
                end)
          in
          Exactly closures
        | Open closures1, Open closures2 ->
          let closures =
            Closure_id.Map.union_merge (fun ty_fabricated1 ty_fabricated2 ->
                let ty_fabricated, new_equations_from_meet =
                  Meet_and_join_fabricated.meet_ty env1 env2
                    ty_fabricated1 ty_fabricated2
                in
                if ty_is_obviously_bottom ty_fabricated then begin
                  bottom_as_ty_fabricated ()
                end else begin
                  equations_from_meet :=
                    Meet_and_join.meet_equations ~resolver
                      new_equations_from_meet !equations_from_meet;
                  ty_fabricated
                end)
              closures1
              closures2
          in
          Open closures
      in
      let closure_elements =
        match set1.closure_elements, set2.closure_elements with
        | Exactly closure_elements1, Exactly closure_elements2 ->
          let closure_elements =
            Var_within_closure.Map.inter (fun ty_value1 ty_value2 ->
                let ty_value, new_equations_from_meet =
                  Meet_and_join_value.meet_ty env1 env2
                    ty_value1 ty_value2
                in
                if ty_is_obviously_bottom ty_value then begin
                  None
                end else begin
                  equations_from_meet :=
                    Meet_and_join.meet_equations ~resolver
                      new_equations_from_meet !equations_from_meet;
                  Some ty_value
                end)
              closure_elements1
              closure_elements2
          in
          let same_as_closure_elements old_closure_elements =
            match
              Var_within_closure.Map.for_all2_opt (fun ty_value1 ty_value2 ->
                  ty_value1 == ty_value2)
                old_closure_elements closure_elements
            with
            | None -> false
            | Some same -> same
          in
          if same_as_closure_elements closure_elements1 then
            set1.closure_elements
          else if same_as_closure_elements closure_elements2 then
            set2.closure_elements
          else
            Exactly closure_elements
        | Exactly closure_elements1, Open closure_elements2
        | Open closure_elements2, Exactly closure_elements1 ->
          let closure_elements =
            Var_within_closure.Map.filter_map closure_elements1
              ~f:(fun closure_id ty1 ->
                match
                  Var_within_closure.Map.find closure_id closure_elements2
                with
                | exception Not_found -> Some ty1
                | ty2 ->
                  let ty_value, new_equations_from_meet =
                    Meet_and_join_value.meet_ty env1 env2 ty1 ty2
                  in
                  if ty_is_obviously_bottom ty_value then begin
                    None
                  end else begin
                    equations_from_meet :=
                      Meet_and_join.meet_equations ~resolver
                        new_equations_from_meet !equations_from_meet;
                    Some ty_value
                  end)
          in
          Exactly closure_elements
        | Open closure_elements1, Open closure_elements2 ->
          let closure_elements =
            Var_within_closure.Map.union_merge (fun ty_value1 ty_value2 ->
                let ty_value, new_equations_from_meet =
                  Meet_and_join_value.meet_ty env1 env2
                    ty_value1 ty_value2
                in
                if ty_is_obviously_bottom ty_value then begin
                  bottom_as_ty_value ()
                end else begin
                  equations_from_meet :=
                    Meet_and_join.meet_equations ~resolver new_equations_from_meet
                      !equations_from_meet;
                  ty_value
                end)
              closure_elements1
              closure_elements2
          in
          Open closure_elements
      in
      match closures with
      | Exactly map when Closure_id.Map.is_empty map -> Bottom
      | _ ->
        if closures == set1.closures
          && closure_elements == set1.closure_elements
        then Ok (set1, !equations_from_meet)
        else if closures == set2.closures
          && closure_elements == set2.closure_elements
        then Ok (set2, !equations_from_meet)
        else begin
          let set : set_of_closures =
            { closures;
              closure_elements;
            }
          in
          Ok (set, !equations_from_meet)
        end

    let join_set_of_closures env1 env2
          (set1 : set_of_closures) (set2 : set_of_closures)
          : set_of_closures =
      let closures : _ extensibility =
        match set1.closures, set2.closures with
        | Exactly closures1, Exactly closures2 ->
          let closures =
            Closure_id.Map.union_merge
              (fun ty_fabricated1 ty_fabricated2 ->
                Meet_and_join_fabricated.join_ty env1 env2
                  ty_fabricated1 ty_fabricated2)
              closures1
              closures2
          in
          Exactly closures
        | Exactly closures1, Open closures2
        | Open closures1, Exactly closures2 ->
          let closures =
            Closure_id.Map.union_merge
              (fun ty_fabricated1 ty_fabricated2 ->
                Meet_and_join_fabricated.join_ty env1 env2
                  ty_fabricated1 ty_fabricated2)
              closures1
              closures2
          in
          Open closures
        | Open closures1, Open closures2 ->
          let closures =
            Closure_id.Map.union_both
              (fun _ty_fabricated ->
                any_fabricated_as_ty_fabricated ())
              (fun ty_fabricated1 ty_fabricated2 ->
                Meet_and_join_fabricated.join_ty env1 env2
                  ty_fabricated1 ty_fabricated2)
              closures1
              closures2
          in
          Open closures
      in
      let closure_elements : _ extensibility =
        match set1.closure_elements, set2.closure_elements with
        | Exactly closure_elements1, Exactly closure_elements2 ->
          let closure_elements =
            Var_within_closure.Map.union_merge
              (fun ty_value1 ty_value2 ->
                Meet_and_join_value.join_ty env1 env2
                  ty_value1 ty_value2)
              closure_elements1
              closure_elements2
          in
          Exactly closure_elements
        | Exactly closure_elements1, Open closure_elements2
        | Open closure_elements1, Exactly closure_elements2 ->
          let closure_elements =
            Var_within_closure.Map.union_merge
              (fun ty_value1 ty_value2 ->
                Meet_and_join_value.join_ty env1 env2
                  ty_value1 ty_value2)
              closure_elements1
              closure_elements2
          in
          Open closure_elements
        | Open closure_elements1, Open closure_elements2 ->
          let closure_elements =
            Var_within_closure.Map.union_both
              (fun _ty_value ->
                any_value_as_ty_value ())
              (fun ty_value1 ty_value2 ->
                Meet_and_join_value.join_ty env1 env2
                  ty_value1 ty_value2)
              closure_elements1
              closure_elements2
          in
          Open closure_elements
      in
      if closures == set1.closures
        && closure_elements == set1.closure_elements
      then
        set1
      else if closures == set2.closures
        && closure_elements == set2.closure_elements
      then
        set2
      else
        { closures;
          closure_elements;
        }

    let meet_of_kind_foo env1 env2
          (of_kind1 : of_kind_fabricated) (of_kind2 : of_kind_fabricated)
          : (of_kind_fabricated * equations) Or_bottom.t =
      let resolver = env1.resolver in
      match of_kind1, of_kind2 with
      | Discriminant discriminants1, Discriminant discriminants2 ->
        let discriminants =
          Discriminant.Map.inter_merge
            (fun ({ equations = equations1; } : discriminant_case)
                  ({ equations = equations2; } : discriminant_case)
                  : discriminant_case ->
              let equations =
                Meet_and_join.meet_equations ~resolver
                  equations1 equations2
              in
              (* CR mshinwell: Do we ever flip back to [Bottom] here? *)
              { equations; })
            discriminants1
            discriminants2
        in
        begin match Discriminant.Map.get_singleton discriminants with
        | None -> Ok (Discriminant discriminants, Equations.create ())
        | Some (discriminant, discriminant_case) ->
          let equations_from_meet = discriminant_case.equations in
          let discriminants =
            Discriminant.Map.singleton discriminant
              ({ equations = Equations.create (); } : discriminant_case)
          in
          Ok (Discriminant discriminants, equations_from_meet)
        end
      | Set_of_closures set1, Set_of_closures set2 ->
        begin match meet_set_of_closures env1 env2 set1 set2 with
        | Ok (set_of_closures, equations_from_meet) ->
          if set_of_closures == set1 then Ok (of_kind1, equations_from_meet)
          else if set_of_closures == set2 then Ok (of_kind2, equations_from_meet)
          else Ok (Set_of_closures set_of_closures, equations_from_meet)
        | Bottom -> Bottom
        end
      | Closure closure1, Closure closure2 ->
        begin match meet_closure env1 env2 closure1 closure2 with
        | Ok (closure, equations_from_meet) ->
          if closure == closure1 then Ok (of_kind1, equations_from_meet)
          else if closure == closure2 then Ok (of_kind2, equations_from_meet)
          else Ok (Closure closure, equations_from_meet)
        | Bottom -> Bottom
        end
      | (Discriminant _ | Set_of_closures _ | Closure _), _ -> Bottom

    let join_of_kind_foo env1 env2
          (of_kind1 : of_kind_fabricated) (of_kind2 : of_kind_fabricated)
          : of_kind_fabricated or_unknown =
      match of_kind1, of_kind2 with
      | Discriminant discriminants1, Discriminant discriminants2 ->
        let discriminants =
          Discriminant.Map.union_merge
            (fun ({ equations = equations1; } : discriminant_case)
                  ({ equations = equations2; } : discriminant_case)
                  : discriminant_case ->
              let equations =
                Meet_and_join.join_equations ~resolver:env1.resolver
                  equations1 equations2
              in
              { equations; })
            discriminants1
            discriminants2
        in
        Known (Discriminant discriminants)
      | Set_of_closures set1, Set_of_closures set2 ->
        let set_of_closures = join_set_of_closures env1 env2 set1 set2 in
        Known (Set_of_closures set_of_closures)
      | Closure closure1, Closure closure2 ->
        let closure = join_closure env1 env2 closure1 closure2 in
        Known (Closure closure)
      | (Discriminant _ | Set_of_closures _ | Closure _), _ -> Unknown
  end) and Meet_and_join : sig
    val meet : bias_towards:t_in_context -> t_in_context -> t * equations

    val join : t_in_context -> t_in_context -> t

    val meet_equations
       : resolver:(Export_id.t -> t option)
      -> equations
      -> equations
      -> equations

    val join_equations
       : resolver:(Export_id.t -> t option)
      -> equations
      -> equations
      -> equations
  end = struct
    let meet ~bias_towards:(env1, (t1 : t)) (env2, (t2 : t)) : t * equations =
      if env1 == env2 && t1 == t2 then t1, Equations.create ()
      else begin
        ensure_phantomness_matches t1 t2 "kind mismatch upon meet";
        let descr, equations_from_meet =
          match t1.descr, t2.descr with
          | Value ty_value1, Value ty_value2 ->
            let ty_value, equations_from_meet =
              Meet_and_join_value.meet_ty env1 env2 ty_value1 ty_value2
            in
            if ty_value == ty_value1 then t1.descr, equations_from_meet
            else if ty_value == ty_value2 then t2.descr, equations_from_meet
            else Value ty_value, equations_from_meet
          | Naked_number (ty_naked_number1, kind1),
              Naked_number (ty_naked_number2, kind2) ->
            let module N = K.Naked_number in
            begin match kind1, kind2 with
            | N.Naked_immediate, N.Naked_immediate ->
              let ty_naked_number, equations_from_meet =
                Meet_and_join_naked_immediate.meet_ty env1 env2
                  ty_naked_number1 ty_naked_number2
              in
              Naked_number (ty_naked_number, N.Naked_immediate),
                equations_from_meet
            | N.Naked_float, N.Naked_float ->
              let ty_naked_number, equations_from_meet =
                Meet_and_join_naked_float.meet_ty env1 env2
                  ty_naked_number1 ty_naked_number2
              in
              Naked_number (ty_naked_number, N.Naked_float),
                equations_from_meet
            | N.Naked_int32, N.Naked_int32 ->
              let ty_naked_number, equations_from_meet =
                Meet_and_join_naked_int32.meet_ty env1 env2
                  ty_naked_number1 ty_naked_number2
              in
              Naked_number (ty_naked_number, N.Naked_int32),
                equations_from_meet
            | N.Naked_int64, N.Naked_int64 ->
              let ty_naked_number, equations_from_meet =
                Meet_and_join_naked_int64.meet_ty env1 env2
                  ty_naked_number1 ty_naked_number2
              in
              Naked_number (ty_naked_number, N.Naked_int64),
                equations_from_meet
            | N.Naked_nativeint, N.Naked_nativeint ->
              let ty_naked_number, equations_from_meet =
                Meet_and_join_naked_nativeint.meet_ty env1 env2
                  ty_naked_number1 ty_naked_number2
              in
              Naked_number (ty_naked_number, N.Naked_nativeint),
                equations_from_meet
            | _, _ ->
              Misc.fatal_errorf "Kind mismatch upon meet:@ %a@ versus@ %a"
                print t1
                print t2
            end
          | Fabricated ty_fabricated1, Fabricated ty_fabricated2 ->
            let ty_fabricated, equations_from_meet =
              Meet_and_join_fabricated.meet_ty env1 env2
                ty_fabricated1 ty_fabricated2
            in
            if ty_fabricated == ty_fabricated1 then
              t1.descr, equations_from_meet
            else if ty_fabricated == ty_fabricated2 then
              t2.descr, equations_from_meet
            else
              Fabricated ty_fabricated, equations_from_meet
          | (Value _ | Naked_number _ | Fabricated _), _ ->
            Misc.fatal_errorf "Kind mismatch upon meet:@ %a@ versus@ %a"
              print t1
              print t2
        in
        let t =
          if t1.descr == descr then t1
          else if t2.descr == descr then t2
          else { t1 with descr; }
        in
        t, equations_from_meet
      end

    let join (env1, (t1 : t)) (env2, (t2 : t)) =
      if env1 == env2 && t1 == t2 then t1
      else begin
        ensure_phantomness_matches t1 t2 "kind mismatch upon join";
        let descr =
          match t1.descr, t2.descr with
          | Value ty_value1, Value ty_value2 ->
            let ty_value =
              Meet_and_join_value.join_ty env1 env2 ty_value1 ty_value2
            in
            if ty_value == ty_value1 then t1.descr
            else if ty_value == ty_value2 then t2.descr
            else Value ty_value
          | Naked_number (ty_naked_number1, kind1),
              Naked_number (ty_naked_number2, kind2) ->
            let module N = K.Naked_number in
            begin match kind1, kind2 with
            | N.Naked_immediate, N.Naked_immediate ->
              let ty_naked_number =
                Meet_and_join_naked_immediate.join_ty env1 env2
                  ty_naked_number1 ty_naked_number2
              in
              Naked_number (ty_naked_number, N.Naked_immediate)
            | N.Naked_float, N.Naked_float ->
              let ty_naked_number =
                Meet_and_join_naked_float.join_ty env1 env2
                  ty_naked_number1 ty_naked_number2
              in
              Naked_number (ty_naked_number, N.Naked_float)
            | N.Naked_int32, N.Naked_int32 ->
              let ty_naked_number =
                Meet_and_join_naked_int32.join_ty env1 env2
                  ty_naked_number1 ty_naked_number2
              in
              Naked_number (ty_naked_number, N.Naked_int32)
            | N.Naked_int64, N.Naked_int64 ->
              let ty_naked_number =
                Meet_and_join_naked_int64.join_ty env1 env2
                  ty_naked_number1 ty_naked_number2
              in
              Naked_number (ty_naked_number, N.Naked_int64)
            | N.Naked_nativeint, N.Naked_nativeint ->
              let ty_naked_number =
                Meet_and_join_naked_nativeint.join_ty env1 env2
                  ty_naked_number1 ty_naked_number2
              in
              Naked_number (ty_naked_number, N.Naked_nativeint)
            | _, _ ->
              Misc.fatal_errorf "Kind mismatch upon join:@ %a@ versus %a"
                print t1
                print t2
            end
          | Fabricated ty_fabricated1, Fabricated ty_fabricated2 ->
            let ty_fabricated =
              Meet_and_join_fabricated.join_ty env1 env2
                ty_fabricated1 ty_fabricated2
            in
            if ty_fabricated == ty_fabricated1 then t1.descr
            else if ty_fabricated == ty_fabricated2 then t2.descr
            else Fabricated ty_fabricated
          | (Value _ | Naked_number _ | Fabricated _), _ ->
            Misc.fatal_errorf "Kind mismatch upon join:@ %a@ versus %a"
              print t1
              print t2
        in
        { t1 with descr; }
      end

    let meet_or_join_equations ~resolver ~meet_or_join
          equations1 equations2 =
      let typing_judgements =
        let env1 = Equations.to_typing_environment ~resolver equations1 in
        let env2 = Equations.to_typing_environment ~resolver equations2 in
        meet_or_join env1 env2
      in
(*
      let typing_judgements =
        let allowed_names =
          Name.Map.fold (fun name (_level, ty) allowed_names ->
              if is_unknown (env, ty) then allowed_names
              else Name.Set.add name allowed_names)
            env.names_to_types
            Name.Set.empty
        in
        restrict_to_names0_typing_environment env allowed_names
      in
      let typing_judgements =
        { typing_judgements with
          must_be_closed = true;
        }
      in
*)
      { typing_judgements = Some typing_judgements; }

    let meet_equations ~resolver equations1 equations2 =
      meet_or_join_equations ~resolver ~meet_or_join:Typing_environment0.meet
        equations1 equations2

    let join_equations ~resolver equations1 equations2 =
      meet_or_join_equations ~resolver ~meet_or_join:Typing_environment0.join
        equations1 equations2
  end and Typing_environment0 : sig
    include Typing_environment0_intf.S
      with type typing_environment := typing_environment
      with type equations := equations
      with type flambda_type := flambda_type
      with type t_in_context := t_in_context
      with type 'a ty = 'a ty
      with type 'a unknown_or_join = 'a unknown_or_join
  end = Real_typing_environment0.Make (struct
    include T

    module Equations = Equations

    let is_empty_typing_environment = is_empty_typing_environment
    let meet = Meet_and_join.meet
    let join = Meet_and_join.join
    let kind = kind
    let force_to_kind_fabricated = force_to_kind_fabricated
    let force_to_kind_naked_number = force_to_kind_naked_number
    let force_to_kind_value = force_to_kind_value
    let unknown = unknown
    let free_names_set = free_names_set
    let free_names = free_names
    let print_typing_environment = print_typing_environment
    let print = print
  end) and Equations : sig
    include Equations_intf.S
      with type equations := equations
      with type typing_environment := typing_environment
      with type flambda_type := flambda_type

    val phys_equal : t -> t -> bool

    val add_or_replace
       : resolver:(Export_id.t -> flambda_type option)
      -> t
      -> Name.t
      -> Scope_level.t
      -> flambda_type
      -> t
  end = struct
    type t = equations

    let create = create_equations

    let singleton_equations ~resolver name scope_level ty =
      { typing_judgements =
          Some (Typing_environment0.singleton0 ~resolver name scope_level ty
            ~must_be_closed:false);
      }

    let add_or_replace ~resolver t name scope_level ty =
      match t.typing_judgements with
      | None -> singleton_equations ~resolver name scope_level ty
      | Some typing_judgements ->
        { typing_judgements =
            Some (Typing_environment0.add_or_replace typing_judgements name
              scope_level ty);
        }

    let invariant t =
      match t.typing_judgements with
      | None -> ()
      | Some typing_judgements ->
        assert (not typing_judgements.must_be_closed);
        Typing_environment0.invariant typing_judgements

    let singleton = singleton_equations

    let add ~resolver t name scope_level ty =
      match t.typing_judgements with
      | None -> singleton ~resolver name scope_level ty
      | Some typing_judgements ->
        { typing_judgements =
            Some (Typing_environment0.add typing_judgements name scope_level ty);
        }

    let add_or_replace_meet ~resolver t name scope_level ty =
      match t.typing_judgements with
      | None -> singleton ~resolver name scope_level ty
      | Some typing_judgements ->
        { typing_judgements =
            Some (Typing_environment0.add_or_replace_meet typing_judgements
              name scope_level ty);
        }

    let meet = Meet_and_join.meet_equations

    let equal ~equal_type
          { typing_judgements = typing_judgements1;
          }
          { typing_judgements = typing_judgements2;
          } =
      Misc.Stdlib.Option.equal (Typing_environment0.equal ~equal_type)
        typing_judgements1 typing_judgements2

    let phys_equal { typing_judgements = typing_judgements1; }
          { typing_judgements = typing_judgements2; } =
      typing_judgements1 == typing_judgements2
        || match typing_judgements1, typing_judgements2 with
           | None, None -> true
           | None, Some _ | Some _, None -> false
           | Some env1, Some env2 ->
             Typing_environment0.phys_equal env1 env2

    let print = print_equations

    let remove ({ typing_judgements; } as t) name =
      match typing_judgements with
      | None -> t
      | Some typing_judgements ->
        let typing_judgements =
          Typing_environment0.remove typing_judgements name
        in
        { typing_judgements = Some typing_judgements; }

    let to_typing_environment ~resolver { typing_judgements; } =
      match typing_judgements with
      | None -> Typing_environment0.create ~resolver
      | Some typing_judgements -> typing_judgements
  end

  let meet = Meet_and_join.meet
  let join = Meet_and_join.join

  let join_ty_value (env1, ty_value1) (env2, ty_value2) =
    Meet_and_join_value.join_ty env1 env2 ty_value1 ty_value2

  let add_equations (env, t) equations_to_add : t =
    let t, _canonical_name = Typing_environment0.resolve_aliases (env, t) in
    match t.descr with
    | Value (No_alias (Join of_kind_values)) ->
      let of_kind_values =
        List.map
          (fun (of_kind_value : of_kind_value) : of_kind_value ->
            match of_kind_value with
            | Blocks_and_tagged_immediates { blocks; immediates;
                is_int; get_tag; } ->
              let blocks : _ or_unknown =
                match blocks with
                | Unknown -> Unknown
                | Known blocks ->
                  let blocks =
                    Tag.Map.map
                      (fun ((Blocks { by_length }) : block_cases)
                            : block_cases ->
                        let by_length =
                          Targetint.OCaml.Map.map
                            (fun (block : singleton_block) : singleton_block ->
                              let equations =
                                Equations.meet ~resolver:env.resolver
                                  block.equations equations_to_add
                              in
                              { block with equations; })
                            by_length
                        in
                        Blocks { by_length; })
                      blocks
                  in
                  Known blocks
              in
              let immediates : _ or_unknown =
                match immediates with
                | Unknown -> Unknown
                | Known imm_map ->
                  let imm_map =
                    Immediate.Map.map
                      (fun ({ equations; } : immediate_case)
                            : immediate_case ->
                        let equations =
                          Equations.meet ~resolver:env.resolver
                            equations equations_to_add
                        in
                        { equations; })
                      imm_map
                  in
                  Known imm_map
              in
              Blocks_and_tagged_immediates { blocks; immediates; is_int;
                get_tag; }
            | Boxed_number _ | Closures _ | String _ -> of_kind_value)
          of_kind_values
      in
      { t with
        descr = Value (No_alias (Join of_kind_values));
      }
    | Fabricated (No_alias (Join of_kind_fabricateds)) ->
      let of_kind_fabricateds =
        List.map
          (fun (of_kind_fabricated : of_kind_fabricated) : of_kind_fabricated ->
            match of_kind_fabricated with
            | Discriminant discriminant_map ->
              let discriminant_map =
                Discriminant.Map.map
                  (fun ({ equations; } : discriminant_case)
                        : discriminant_case ->
                    let equations =
                      Equations.meet ~resolver:env.resolver
                        equations equations_to_add
                    in
                    { equations; })
                  discriminant_map
              in
              Discriminant discriminant_map
            | Set_of_closures _
            | Closure _ -> of_kind_fabricated)
          of_kind_fabricateds
      in
      { t with
        descr = Fabricated (No_alias (Join of_kind_fabricateds));
      }
    | Value (Type _ | Equals _ | No_alias Unknown)
    | Fabricated (Type _ | Equals _ | No_alias Unknown) -> t
    | Naked_number _ -> t
end
