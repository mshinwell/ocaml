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

module K = Flambda_kind

module Make (Expr : sig
  type t
  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit
  val free_names : t -> Name_occurrences.t
end) = struct
  type expr = Expr.t

  type inline_attribute =
    | Always_inline
    | Never_inline
    | Unroll of int
    | Default_inline

  let print_inline_attribute ppf attr =
    let fprintf = Format.fprintf in
    match attr with
    | Always_inline -> fprintf ppf "Always_inline"
    | Never_inline -> fprintf ppf "Never_inline"
    | Unroll n -> fprintf ppf "@[(Unroll %d)@]" n
    | Default_inline -> fprintf ppf "Default_inline"

  type specialise_attribute =
    | Always_specialise
    | Never_specialise
    | Default_specialise

  let print_specialise_attribute ppf attr =
    let fprintf = Format.fprintf in
    match attr with
    | Always_specialise -> fprintf ppf "Always_specialise"
    | Never_specialise -> fprintf ppf "Never_specialise"
    | Default_specialise -> fprintf ppf "Default_specialise"

  type string_contents =
    | Contents of string
    | Unknown_or_mutable

  module String_info = struct
    type t = {
      contents : string_contents;
      size : Targetint.OCaml.t;
    }

    include Identifiable.Make (struct
      type nonrec t = t

      let compare t1 t2 =
        let c =
          match t1.contents, t2.contents with
          | Contents s1, Contents s2 -> String.compare s1 s2
          | Unknown_or_mutable, Unknown_or_mutable -> 0
          | Contents _, Unknown_or_mutable -> -1
          | Unknown_or_mutable, Contents _ -> 1
        in
        if c <> 0 then c
        else Pervasives.compare t1.size t2.size

      let equal t1 t2 =
        compare t1 t2 = 0

      let hash t = Hashtbl.hash t

      let print ppf { contents; size; } =
        match contents with
        | Unknown_or_mutable ->
          Format.fprintf ppf "(size %a)" Targetint.OCaml.print size
        | Contents s ->
          let s, dots =
            let max_size = Targetint.OCaml.ten in
            let long = Targetint.OCaml.compare size max_size > 0 in
            if long then String.sub s 0 8, "..."
            else s, ""
          in
          Format.fprintf ppf "(size %a) (contents \"%S\"%s)"
            Targetint.OCaml.print size
            s dots
    end)
  end

  type 'a mutable_or_immutable =
    | Immutable of 'a
    | Mutable

  type 'a or_unknown =
    | Known of 'a
    | Unknown

  type 'a or_alias =
    | No_alias of 'a
    | Type of Export_id.t
    | Type_of of Name.t

  type 'a extensibility =
    | Open of 'a
    | Exactly of 'a

  let extensibility_contents (e : _ extensibility) =
    match e with
    | Open contents | Exactly contents -> contents

  type t = {
    descr : descr;
    phantom : Flambda_kind.Phantom_kind.occurrences option;
  }

  and flambda_type = t

  and t_in_context = typing_environment * t

  and descr =
    | Value of ty_value
    | Naked_number :
        'kind ty_naked_number * 'kind Flambda_kind.Naked_number.t -> descr
    | Fabricated of ty_fabricated

  and ty_value = of_kind_value ty
  and ty_value_in_context = typing_environment * ty_value
  and 'a ty_naked_number = 'a of_kind_naked_number ty
  and ty_fabricated = of_kind_fabricated ty

  and 'a ty = 'a unknown_or_join or_alias
  and 'a ty_in_context = typing_environment * ('a ty)

  and 'a unknown_or_join =
    | Unknown
    | Join of 'a list

  and of_kind_value =
    | Blocks_and_tagged_immediates of blocks_and_tagged_immediates
    | Boxed_number : _ of_kind_value_boxed_number -> of_kind_value
    | Closures of closures
    | String of String_info.Set.t

  and immediate_case = {
    (* Environment extensions have an [option] type so that the information
       required to create a typing environment isn't required for various
       trivial functions such as [these_tagged_immediates]. *)
    env_extension : typing_environment option;
  }
 
  and singleton_block = {
    env_extension : typing_environment option;
    fields : t mutable_or_immutable array;
  }

  and block_cases =
    | Join of { by_length : singleton_block Targetint.OCaml.Map.t; }

  and blocks_and_tagged_immediates = {
    immediates : immediate_case Immediate.Map.t or_unknown;
    blocks : block_cases Tag.Map.t or_unknown;
    is_int : Name.t option;
    get_tag : Name.t option;
  }

  and 'a of_kind_value_boxed_number =
    | Boxed_float
         : Numbers.Float_by_bit_pattern.Set.t ty_naked_number
        -> Numbers.Float_by_bit_pattern.Set.t ty_naked_number
             of_kind_value_boxed_number
    | Boxed_int32
         : Int32.Set.t ty_naked_number
        -> Int32.Set.t ty_naked_number of_kind_value_boxed_number
    | Boxed_int64
         : Int64.Set.t ty_naked_number
        -> Int64.Set.t ty_naked_number of_kind_value_boxed_number
    | Boxed_nativeint
         : Targetint.Set.t ty_naked_number
        -> Targetint.Set.t ty_naked_number of_kind_value_boxed_number

  and inlinable_function_declaration = {
    closure_origin : Closure_origin.t;
    continuation_param : Continuation.t;
    exn_continuation_param : Continuation.t;
    is_classic_mode : bool;
    params : (Parameter.t * t) list;
    code_id : Code_id.t;
    body : expr;
    free_names_in_body : Name_occurrences.t;
    result : t list;
    result_env_extension : typing_environment option;
    stub : bool;
    dbg : Debuginfo.t;
    inline : inline_attribute;
    specialise : specialise_attribute;
    is_a_functor : bool;
    invariant_params : Variable.Set.t lazy_t;
    size : int option lazy_t;
    direct_call_surrogate : Closure_id.t option;
    my_closure : Variable.t;
  }

  and non_inlinable_function_declarations = {
    params : t list;
    result : t list;
    result_env_extension : typing_environment option;
    direct_call_surrogate : Closure_id.t option;
  }

  and function_declarations =
    | Non_inlinable of non_inlinable_function_declarations option
    | Inlinable of inlinable_function_declaration

  and closures_entry = {
    set_of_closures : ty_fabricated;
  }

  and closures = closures_entry Closure_id.Map.t

  and 'a of_kind_naked_number =
    | Immediate : Immediate.Set.t -> Immediate.Set.t of_kind_naked_number
    | Float : Numbers.Float_by_bit_pattern.Set.t
        -> Numbers.Float_by_bit_pattern.Set.t of_kind_naked_number
    | Int32 : Int32.Set.t -> Int32.Set.t of_kind_naked_number
    | Int64 : Int64.Set.t -> Int64.Set.t of_kind_naked_number
    | Nativeint : Targetint.Set.t -> Targetint.Set.t of_kind_naked_number

  and tag_case = {
    env_extension : typing_environment option;
  }

  and of_kind_fabricated =
    | Tag of tag_case Tag.Map.t
    | Set_of_closures of set_of_closures
    | Closure of closure

  and set_of_closures = {
    closures : ty_fabricated Closure_id.Map.t extensibility;
    closure_elements : ty_value Var_within_closure.Map.t extensibility;
  }

  and closure = {
    function_decls : function_declarations;
  }

  and typing_environment = {
    resolver : (Export_id.t -> t option);
    canonical_names_to_aliases : Name.Set.t Name.Map.t;
    names_to_types : (Scope_level.t * t) Name.Map.t;
    levels_to_names : Name.Set.t Scope_level.Map.t;
    existentials : Name.Set.t;
    existential_freshening : Freshening.t;
  }

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
    | Type_of name ->
      Format.fprintf ppf "@[(= type_of %a)@]" Name.print name
    | Type export_id ->
      Format.fprintf ppf "@[(= %a)@]" Export_id.print export_id

  let print_unknown_or_join print_contents ppf (o : _ unknown_or_join) =
    match o with
    | Unknown -> Format.fprintf ppf "Unknown"
    | Join [] -> Format.fprintf ppf "Bottom"
    | Join [contents] -> print_contents ppf contents
    | Join incompatibles ->
      Format.fprintf ppf "@[Join_incompatible@ (%a)@]"
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
        ({ env_extension; } : immediate_case) =
    Format.fprintf ppf "@[(env_extension@ %a)@]"
      (Misc.Stdlib.Option.print (print_typing_environment_with_cache ~cache))
      env_extension

  and print_fields ~cache ppf (fields : t mutable_or_immutable array) =
    Format.fprintf ppf "@[[| %a |]@]"
      (Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ";@ ")
        (print_mutable_or_immutable (print_with_cache ~cache)))
      (Array.to_list fields)

  and print_singleton_block ~cache ppf { env_extension; fields; } =
    Format.fprintf ppf "@[((env_extension@ %a)@ (fields@ %a))@]"
      (Misc.Stdlib.Option.print (print_typing_environment_with_cache ~cache))
      env_extension
      (print_fields ~cache) fields

  and print_block_cases ~cache ppf ((Join { by_length; }) : block_cases) =
    match Targetint.OCaml.Map.get_singleton by_length with
    | Some (_length, block) -> print_singleton_block ~cache ppf block
    | None ->
      Format.fprintf ppf "@[(Join (by_length@ %a))@]"
        (Targetint.OCaml.Map.print (print_singleton_block ~cache)) by_length

  and print_immediates ~cache ppf cases =
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
        Format.fprintf ppf "@[(blocks@ @[%a@])@])@]"
          (print_blocks ~cache) blocks
      | Known blocks, Known immediates, None, None
          when Tag.Map.is_empty blocks
            && not (Immediate.Map.is_empty immediates)
            && Immediate.Map.for_all
                 (fun _imm ({ env_extension; } : immediate_case) ->
                   match env_extension with
                   | None -> true
                   | Some env -> Name.Map.is_empty env.names_to_types)
                 immediates ->
        Format.fprintf ppf "@[(immediates@ @[(%a)@])@]"
          Immediate.Set.print (Immediate.Map.keys immediates)
      | _ ->
        match is_int, get_tag with
        | None, None ->
          Format.fprintf ppf
            "@[(Blocks_and_immediates@ \
             @[(blocks@ @[%a@])@]@ \
             @[(immediates@ @[%a@])@])@]"
            (print_or_unknown (print_blocks ~cache)) blocks
            (print_or_unknown (print_immediates ~cache)) immediates
        | _, _ ->
          Format.fprintf ppf
            "@[(Blocks_and_immediates@ \
             @[(blocks@ @[%a@])@]@ \
             @[(immediates@ @[%a@])@]@ \
             @[(is_int@ %a)@]@ \
             @[(get_tag@ %a)@])@]"
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
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ";@ ")
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
        "@[(inlinable@ \
          @[(closure_origin@ %a)@]@ \
          @[(continuation_param@ %a)@]@ \
          @[(exn_continuation_param@ %a)@]@ \
          @[(is_classic_mode@ %b)@]@ \
          @[(params (%a))@]@ \
          @[(body@ %a)@]@ \
          @[(free_names_in_body@ %a)@]@ \
          @[(result@ (%a))@]@ \
          @[(result_env_extension@ (%a))@]@ \
          @[(stub@ %b)@]@ \
          @[(dbg@ %a)@]@ \
          @[(inline@ %a)@]@ \
          @[(specialise@ %a)@]@ \
          @[(is_a_functor@ %b)@]@ \
          @[(invariant_params@ %a)@]@ \
          @[(size@ %a)@]@ \
          @[(direct_call_surrogate@ %a)@]@ \
          @[(my_closure@ %a)@])@]"
        Closure_origin.print decl.closure_origin
        Continuation.print decl.continuation_param
        Continuation.print decl.exn_continuation_param
        decl.is_classic_mode
        (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
          (fun ppf (param, ty) ->
            Format.fprintf ppf "@[(%a@ :@ %a)@]"
              Parameter.print param
              (print_with_cache ~cache) ty)) decl.params
        (Expr.print_with_cache ~cache) decl.body
        Name_occurrences.print decl.free_names_in_body
        (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
          (fun ppf ty ->
            Format.fprintf ppf "%a"
              print ty)) decl.result
        (Misc.Stdlib.Option.print (print_typing_environment_with_cache ~cache))
          decl.result_env_extension
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
    Format.fprintf ppf
      "@[(Set_of_closures@ \
          @[(closures@ %a)@]@ \
          @[(closure_elements@ %a)@])@]"
      (print_extensibility (
          Closure_id.Map.print (print_ty_fabricated_with_cache ~cache)))
        set.closures
      (print_extensibility (
          Var_within_closure.Map.print (print_ty_value_with_cache ~cache)))
        set.closure_elements

  and print_closure ~cache ppf (closure : closure) =
    Format.fprintf ppf "@[(Closure (function_decls@ %a))@]"
      (print_function_declarations ~cache) closure.function_decls

  and print_tag_case ~cache ppf ({ env_extension; } : tag_case) =
    Format.fprintf ppf "@[(env_extension@ %a)@]"
      (Misc.Stdlib.Option.print (print_typing_environment_with_cache ~cache))
        env_extension

  and print_of_kind_fabricated ~cache ppf (o : of_kind_fabricated) =
    match o with
    | Tag tag_map ->
      Format.fprintf ppf "@[(Tags@ %a)@]"
        (Tag.Map.print (print_tag_case ~cache)) tag_map
    | Set_of_closures set -> print_set_of_closures ~cache ppf set
    | Closure closure -> print_closure ~cache ppf closure

  and print_ty_fabricated_with_cache ~cache ppf (ty : ty_fabricated) =
    print_ty_generic (print_of_kind_fabricated ~cache) ppf ty

  and print_ty_fabricated ppf (ty : ty_fabricated) =
    print_ty_fabricated_with_cache ~cache:(Printing_cache.create ()) ppf ty

  and print_descr ~cache ppf (descr : descr) =
    match descr with
    | Value ty ->
      Format.fprintf ppf "@[(Value@ (%a))@]"
        (print_ty_value_with_cache ~cache) ty
    | Naked_number (ty, _kind) ->
      Format.fprintf ppf "@[(Naked_number@ (%a))@]" print_ty_naked_number ty
    | Fabricated ty ->
      Format.fprintf ppf "@[(Fabricated@ (%a))@]"
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
           levels_to_names; existentials; existential_freshening; } as env) =
    if Name.Map.is_empty names_to_types then
      Format.pp_print_string ppf "Empty"
    else
      Printing_cache.with_cache cache ppf "env" env (fun ppf () ->
        let print_scope_level_and_type ppf (_scope_level, ty) =
          print_with_cache ~cache ppf ty
        in
        Format.fprintf ppf
          "@[((names_to_types@ %a)@ \
              (levels_to_names@ %a)@ \
              (existentials@ %a)@ \
              (existential_freshening@ %a)@ \
              (canonical_names_to_aliases@ %a))@]"
          (Name.Map.print print_scope_level_and_type) names_to_types
          (Scope_level.Map.print Name.Set.print) levels_to_names
          Name.Set.print existentials
          Freshening.print existential_freshening
          (Name.Map.print Name.Set.print) canonical_names_to_aliases)

  let print_typing_environment ppf env =
    print_typing_environment_with_cache ~cache:(Printing_cache.create ())
      ppf env

  let free_names_or_alias free_names_contents (or_alias : _ or_alias) acc =
    match or_alias with
    | No_alias contents -> free_names_contents contents acc
    | Type _export_id -> acc
    | Type_of name -> Name.Set.add name acc

  let free_names_unknown_or_join free_names_contents (o : _ unknown_or_join)
        acc =
    match o with
    | Unknown -> acc
    | Join contents_list ->
      List.fold_left (fun free_names contents ->
          free_names_contents contents free_names)
        Name.Set.empty
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
          Tag.Map.fold (fun _tag ((Join { by_length; }) : block_cases) acc ->
              Targetint.OCaml.Map.fold
                (fun _length (singleton : singleton_block) acc ->
                  let acc =
                    free_names_of_env_extension singleton.env_extension acc
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
              free_names_of_env_extension case.env_extension acc)
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
    | Tag tag_map ->
      Tag.Map.fold (fun _tag ({ env_extension; } : tag_case) acc ->
          free_names_of_env_extension env_extension acc)
        tag_map
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
      let acc =
        List.fold_left (fun acc t ->
          free_names t acc)
          acc
          decl.result
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

  and free_names_of_typing_environment (env : typing_environment) acc =
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

  and free_names_of_env_extension env_extension acc =
    match env_extension with
    | None -> acc
    | Some env_extension -> free_names_of_typing_environment env_extension acc

  let free_names_set t =
    free_names t Name.Set.empty

  let free_names t =
    Name_occurrences.create_from_set_in_types (free_names_set t)

  let create_typing_environment ~resolver =
    let existential_freshening = Freshening.activate Freshening.empty in
    { resolver;
      canonical_names_to_aliases = Name.Map.empty;
      names_to_types = Name.Map.empty;
      levels_to_names = Scope_level.Map.empty;
      existentials = Name.Set.empty;
      existential_freshening;
    }

  let add_alias_typing_environment env ~canonical_name ~alias =
    if not (Name.Map.mem canonical_name env.names_to_types) then begin
      Misc.fatal_errorf "Cannot add alias %a of canonical name %a: the \
          canonical name is not bound in the environment"
        Name.print alias
        Name.print canonical_name
    end;
    if not (Name.Map.mem canonical_name env.names_to_types) then begin
      Misc.fatal_errorf "Cannot add alias %a of canonical name %a: the \
          alias is not bound in the environment"
        Name.print alias
        Name.print canonical_name
    end;
    let canonical_names_to_aliases =
      Name.Map.update canonical_name (function
          | None -> Some (Name.Set.singleton alias)
          | Some aliases -> Some (Name.Set.add alias aliases))
        env.canonical_names_to_aliases
    in
    { env with
      canonical_names_to_aliases;
    }

  let aliases_typing_environment env ~canonical_name =
    match Name.Map.find canonical_name env.canonical_names_to_aliases with
    | exception Not_found ->
      Misc.fatal_errorf "Cannot find aliases of canonical name %a which is \
          not bound in the environment"
        Name.print canonical_name
    | aliases -> aliases

  let add_or_replace_typing_environment' env name scope_level t =
    let names_to_types =
      Name.Map.add name (scope_level, t) env.names_to_types
    in
    let levels_to_names =
      Scope_level.Map.update scope_level
        (function
           | None -> Some (Name.Set.singleton name)
           | Some names -> Some (Name.Set.add name names))
        env.levels_to_names
    in
    { env with
      names_to_types;
      levels_to_names;
    }

  let add_or_replace_typing_environment env name scope_level t =
    (* CR mshinwell: We should add a comment here explaining where this can
       be used and what it cannot be used for (e.g. changing a name's scope
       level) *)
(*
if Scope_level.to_int scope_level = 2
  && not (Name.Map.mem name t.names_to_types)
then begin
  Format.eprintf "AoR for %a:@ %s\n%!"
    Name.print name
    (Printexc.raw_backtrace_to_string (Printexc.get_callstack 20))
end;
*)
    if !Clflags.flambda_invariant_checks then begin
      let free_names = free_names_set t in
      let domain = Name.Map.keys env.names_to_types in
      if not (Name.Set.subset free_names domain) then begin
        Misc.fatal_errorf "Adding binding of %a to@ %a@ would cause typing \
            environment to not be closed: %a"
          Name.print name
          print t
          print_typing_environment env
      end;
      if Name.Set.mem name free_names then begin
        Misc.fatal_errorf "Adding binding of %a to@ %a@ would cause a \
            direct circularity in a type.  Environment: %a"
          Name.print name
          print t
          print_typing_environment env
      end
    end;
    add_or_replace_typing_environment' env name scope_level t

  type binding_type = Normal | Existential

  let find_typing_environment env name =
    match Name.Map.find name env.names_to_types with
    | exception Not_found ->
      Misc.fatal_errorf "Cannot find %a in environment:@ %a"
        Name.print name
        print_typing_environment env
    | _scope_level, ty ->
      let binding_type =
        if Name.Set.mem name env.existentials then Existential
        else Normal
      in
      match binding_type with
      | Normal -> ty, Normal
      | Existential ->
   (* XXX     let ty = rename_variables t freshening in *)
        ty, Existential

  let scope_level_typing_environment env name =
    match Name.Map.find name env.names_to_types with
    | exception Not_found ->
      Misc.fatal_errorf "Cannot find %a in environment:@ %a"
        Name.print name
        print_typing_environment env
    | scope_level, _ty -> scope_level

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

  module Name_or_export_id = struct
    type t =
      | Name of Name.t
      | Export_id of Export_id.t

    include Identifiable.Make (struct
      type nonrec t = t

      let compare t1 t2 =
        match t1, t2 with
        | Name _, Export_id _ -> -1
        | Export_id _, Name _ -> 1
        | Name name1, Name name2 -> Name.compare name1 name2
        | Export_id id1, Export_id id2 -> Export_id.compare id1 id2

      let equal t1 t2 =
        compare t1 t2 = 0
 
      let hash t =
        match t with
        | Name name -> Hashtbl.hash (0, Name.hash name)
        | Export_id id -> Hashtbl.hash (1, Export_id.hash id)

      let print ppf t =
        match t with
        | Name name -> Name.print ppf name
        | Export_id id -> Export_id.print ppf id
    end)
  end

  type 'a type_accessor = typing_environment -> 'a

  let alias_type_of (kind : K.t) name : t =
    match kind with
    | Value ->
      { descr = Value (Type_of name);
        phantom = None;
      }
    | Naked_number Naked_immediate ->
      { descr = Naked_number (Type_of name, K.Naked_number.Naked_immediate);
        phantom = None;
      }
    | Naked_number Naked_float ->
      { descr = Naked_number (Type_of name, K.Naked_number.Naked_float);
        phantom = None;
      }
    | Naked_number Naked_int32 ->
      { descr = Naked_number (Type_of name, K.Naked_number.Naked_int32);
        phantom = None;
      }
    | Naked_number Naked_int64 ->
      { descr = Naked_number (Type_of name, K.Naked_number.Naked_int64);
        phantom = None;
      }
    | Naked_number Naked_nativeint ->
      { descr = Naked_number (Type_of name, K.Naked_number.Naked_nativeint);
        phantom = None;
      }
    | Fabricated ->
      { descr = Fabricated (Type_of name);
        phantom = None;
      }
    | Phantom (occs, phantom_kind) ->
      let descr : descr =
        match phantom_kind with
        | Value -> Value (Type_of name)
        | Naked_number Naked_immediate ->
          Naked_number (Type_of name, K.Naked_number.Naked_immediate)
        | Naked_number Naked_float ->
          Naked_number (Type_of name, K.Naked_number.Naked_float)
        | Naked_number Naked_int32 ->
          Naked_number (Type_of name, K.Naked_number.Naked_int32)
        | Naked_number Naked_int64 ->
          Naked_number (Type_of name, K.Naked_number.Naked_int64)
        | Naked_number Naked_nativeint ->
          Naked_number (Type_of name, K.Naked_number.Naked_nativeint)
        | Fabricated -> Fabricated (Type_of name)
      in
      { descr;
        phantom = Some occs;
      }

  let alias_type_of_as_ty_value name : ty_value = Type_of name

  let alias_type_of_as_ty_fabricated name : ty_fabricated = Type_of name

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
              { env_extension = None;
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
        Immediate.Map.map (fun env_extension : immediate_case ->
            { env_extension = Some env_extension;
            })
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

  let these_tags_as_ty_fabricated tags_to_env_extensions : ty_fabricated =
    let tag_map =
      Tag.Map.map (fun env : tag_case ->
          { env_extension = Some env; })
        tags_to_env_extensions
    in
    No_alias (Join [Tag tag_map])

  let these_tags tags_to_env_extensions : t =
    { descr = Fabricated (these_tags_as_ty_fabricated tags_to_env_extensions);
      phantom = None;
    }

  let this_tag_as_ty_fabricated tag =
    let tags_to_env_extensions = Tag.Map.add tag None Tag.Map.empty in
    let tag_map =
      Tag.Map.map (fun env : tag_case ->
          { env_extension = env; })
        tags_to_env_extensions
    in
    No_alias (Join [Tag tag_map])

  let this_tag tag : t =
    { descr = Fabricated (this_tag_as_ty_fabricated tag);
      phantom = None;
    }

  let any_tag_as_ty_fabricated () : ty_fabricated =
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
      { env_extension = None;
        fields;
      }
    in
    let by_length =
      Targetint.OCaml.Map.add size singleton_block
        Targetint.OCaml.Map.empty
    in
    let block_cases : block_cases = Join { by_length; } in
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
        { env_extension = None;
          fields;
        }
      in
      let by_length =
        Targetint.OCaml.Map.add length singleton_block
          Targetint.OCaml.Map.empty
      in
      let block_cases : block_cases = Join { by_length; } in
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
        { env_extension = None;
          fields;
        }
      in
      let by_length =
        Targetint.OCaml.Map.add length singleton_block
          Targetint.OCaml.Map.empty
      in
      let block_cases : block_cases = Join { by_length; } in
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
        { env_extension = None;
          fields;
        }
      in
      let by_length =
        Targetint.OCaml.Map.add length singleton_block
          Targetint.OCaml.Map.empty
      in
      let block_cases : block_cases = Join { by_length; } in
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

  type still_unresolved =
    | Resolved
    | Still_unresolved

  let resolve_aliases_on_ty0 (type a) env ~force_to_kind
        (ty : a ty) : (a ty) * (Name.t option) * still_unresolved =
    let rec resolve_aliases names_seen ~canonical_name (ty : a ty) =
      let resolve (name : Name_or_export_id.t) : _ * _ * still_unresolved =
        if Name_or_export_id.Set.mem name names_seen then begin
          Misc.fatal_errorf "Loop on %a whilst resolving aliases"
            Name_or_export_id.print name
        end;
        let continue_resolving t ~canonical_name =
          let names_seen = Name_or_export_id.Set.add name names_seen in
          let ty = force_to_kind t in
          resolve_aliases names_seen ~canonical_name ty
        in
        match name with
        | Name name ->
          let t, _binding_type = find_typing_environment env name in
          continue_resolving t ~canonical_name:(Some name)
        | Export_id export_id ->
          match env.resolver export_id with
          | Some t -> continue_resolving t ~canonical_name
          | None -> ty, None, Still_unresolved
      in
      match ty with
      | No_alias _ -> ty, canonical_name, Resolved
      | Type export_id -> resolve (Name_or_export_id.Export_id export_id)
      | Type_of name ->
(*
        Format.eprintf "recursing on %a, seen %a\n%!" Name.print name
          Name_or_export_id.Set.print names_seen;
*)
        resolve (Name_or_export_id.Name name)
    in
    resolve_aliases Name_or_export_id.Set.empty ~canonical_name:None ty

  let resolve_aliases_on_ty env ~force_to_kind ty =
    let t, canonical_name, _still_unresolved =
      resolve_aliases_on_ty0 env ~force_to_kind ty
    in
    t, canonical_name

  let resolve_aliases_and_squash_unresolved_names_on_ty env ~kind:_
        ~force_to_kind ~unknown ty =
    let ty, canonical_name, still_unresolved =
      resolve_aliases_on_ty0 env ~force_to_kind ty
    in
    match still_unresolved with
    | Resolved -> ty, canonical_name
    | Still_unresolved -> unknown, canonical_name

  (* CR mshinwell: choose this function or the one above *)
  let resolve_aliases_and_squash_unresolved_names_on_ty' env ~kind:_
        ~print_ty ~force_to_kind ~unknown:_ ty
        : _ unknown_or_join * (Name.t option) =
    let ty, canonical_name, _still_unresolved =
      try resolve_aliases_on_ty0 env ~force_to_kind ty
      with Misc.Fatal_error -> begin
        Format.eprintf "\n%sContext is: \
            resolve_aliases_and_squash_unresolved_names_on_ty':%s\
            @ %a@ Environment:@ %a\n"
          (Misc_color.bold_red ())
          (Misc_color.reset ())
          print_ty ty
          print_typing_environment env;
        raise Misc.Fatal_error
      end
    in
    match ty with
    | No_alias uoj -> uoj, canonical_name
    | Type _ | Type_of _ -> Unknown, canonical_name

  (* CR mshinwell: this should return not just the canonical name but all
     other aliases encountered, so the meet functions can add judgements
     for those. *)
  let resolve_aliases (env, t) : t * (Name.t option) =
    match t.descr with
    | Value ty ->
      let force_to_kind = force_to_kind_value in
      let ty, canonical_name =
        resolve_aliases_on_ty env ~force_to_kind ty
      in
      { t with descr = Value ty; }, canonical_name
    | Naked_number (ty, kind) ->
      let force_to_kind = force_to_kind_naked_number kind in
      let ty, canonical_name = resolve_aliases_on_ty env ~force_to_kind ty in
      { t with descr = Naked_number (ty, kind); }, canonical_name
    | Fabricated ty ->
      let force_to_kind = force_to_kind_fabricated in
      let ty, canonical_name = resolve_aliases_on_ty env ~force_to_kind ty in
      { t with descr = Fabricated ty; }, canonical_name

  let _resolve_aliases_and_squash_unresolved_names (env, t)
        : t * (Name.t option) =
    let kind = kind t in
    match t.descr with
    | Value ty ->
      let unknown : ty_value = No_alias Unknown in
      let force_to_kind = force_to_kind_value in
      let ty, canonical_name =
        resolve_aliases_and_squash_unresolved_names_on_ty env ~kind
          ~force_to_kind ~unknown ty
      in
      { t with descr = Value ty; }, canonical_name
    | Naked_number (ty, kind) ->
      let unknown : _ ty_naked_number = No_alias Unknown in
      let force_to_kind = force_to_kind_naked_number kind in
      let ty, canonical_name =
        resolve_aliases_and_squash_unresolved_names_on_ty env ~kind
          ~force_to_kind ~unknown ty
      in
      { t with descr = Naked_number (ty, kind); }, canonical_name
    | Fabricated ty ->
      let force_to_kind = force_to_kind_fabricated in
      let unknown : ty_fabricated = No_alias Unknown in
      let ty, canonical_name =
        resolve_aliases_and_squash_unresolved_names_on_ty env
          ~kind ~force_to_kind ~unknown ty
      in
      { t with descr = Fabricated ty; }, canonical_name

  let create_inlinable_function_declaration ~is_classic_mode ~closure_origin
        ~continuation_param ~exn_continuation_param
        ~params ~body ~result ~result_env_extension ~stub ~dbg ~inline
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
      result_env_extension;
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
        ~result_env_extension ~direct_call_surrogate
        : function_declarations =
    let decl : non_inlinable_function_declarations =
      { params;
        result;
        result_env_extension;
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

(*
  let judgements_holding_now ~type_of_name t =
    let t, _canonical_name =
      resolve_aliases_and_squash_unresolved_names ~type_of_name t
    in
    match t.descr with
    | Value (No_alias (Join [Blocks_and_tagged_immediates blocks_imms])) ->
      begin match blocks_imms.immediates with
      | Unknown -> create_typing_environment ()
      | Known imms ->
        if not (Immediate.Map.is_empty imms) then create_typing_environment ()
        else
          begin match Tag.Map.get_singleton blocks_imms.blocks with
          | None -> create_typing_environment ()
          | Some (_, Join { by_length; }) ->
            match Targetint.OCaml.Map.get_singleton by_length with
            | None -> create_typing_environment ()
            | Some (_, singleton_block) -> singleton_block.env_extension
          end
      end
    | Naked_number _ -> create_typing_environment ()
    | Fabricated (No_alias (Join [Tag map])) ->
      begin match Tag.Map.get_singleton map with
      | None -> create_typing_environment ()
      | Some (_, tag_case) -> tag_case.env_extension
      end
    | _ -> create_typing_environment ()
*)

  type judgements_from_meet = (Name.t * Scope_level.t * t) list

  let judgements_of_typing_environment (env : typing_environment) =
    Name.Map.fold (fun name (level, t) judgements ->
        if Name.Set.mem name env.existentials then judgements
        else (name, level, t) :: judgements)
      env.names_to_types
      []

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
      -> (of_kind_foo * judgements_from_meet) Or_bottom.t

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

    (* Greatest lower bound of two types of a particular kind.
       The computation of such may yield new judgements. *)
    val meet_ty
       : typing_environment
      -> typing_environment
      -> of_kind_foo ty
      -> of_kind_foo ty
      -> of_kind_foo ty * judgements_from_meet
  end

  (* CR mshinwell: Work out which properties we need to prove, e.g.
     Distributivity of meet over join:
       X n (X' u Y') == (X n X') u (X n Y'). *)
  module Make_meet_and_join (S : Meet_and_join_spec) : sig
    include Meet_and_join
      with type of_kind_foo := S.of_kind_foo
  end = struct
    let unknown_or_join_is_bottom (uj : _ unknown_or_join) =
      match uj with
      | Join [] -> true
      | Unknown | Join _ -> false

    let rec join_on_unknown_or_join env1 env2
          (uj1 : S.of_kind_foo unknown_or_join)
          (uj2 : S.of_kind_foo unknown_or_join)
          : S.of_kind_foo unknown_or_join =
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
      let unknown_or_join1, canonical_name1 =
        resolve_aliases_and_squash_unresolved_names_on_ty' env1
          ~kind:S.kind
          ~force_to_kind:S.force_to_kind
          ~unknown:(No_alias Unknown)
          ~print_ty:S.print_ty
          or_alias1
      in
      let unknown_or_join2, canonical_name2 =
        resolve_aliases_and_squash_unresolved_names_on_ty' env2
          ~kind:S.kind
          ~force_to_kind:S.force_to_kind
          ~unknown:(No_alias Unknown)
          ~print_ty:S.print_ty
          or_alias2
      in
      match canonical_name1, canonical_name2 with
      | Some name1, Some name2 when Name.equal name1 name2 ->
        Type_of name1
      | Some name1, _ when unknown_or_join_is_bottom unknown_or_join2 ->
        Type_of name1
      | _, Some name2 when unknown_or_join_is_bottom unknown_or_join1 ->
        Type_of name2
      | _, _ ->
        let unknown_or_join =
          join_on_unknown_or_join env1 env2 unknown_or_join1 unknown_or_join2
        in
        No_alias unknown_or_join

    let rec meet_on_unknown_or_join env1 env2
          (ou1 : S.of_kind_foo unknown_or_join)
          (ou2 : S.of_kind_foo unknown_or_join)
          : S.of_kind_foo unknown_or_join * judgements_from_meet =
      match ou1, ou2 with
      | Unknown, ou2 -> ou2, []
      | ou1, Unknown -> ou1, []
      | Join of_kind_foos1, Join of_kind_foos2 ->
        let of_kind_foos, judgements =
          List.fold_left (fun (of_kind_foos, judgements) of_kind_foo ->
              let new_judgements = ref [] in
              let of_kind_foos =
                Misc.Stdlib.List.filter_map (fun of_kind_foo' ->
                    let meet =
                      S.meet_of_kind_foo env1 env2 of_kind_foo of_kind_foo'
                    in
                    match meet with
                    | Ok (of_kind_foo, new_judgements') ->
                      new_judgements := new_judgements' @ !new_judgements;
                      Some of_kind_foo
                    | Bottom -> None)
                  of_kind_foos
              in
              of_kind_foos, !new_judgements @ judgements)
            (of_kind_foos2, [])
            of_kind_foos1
        in
        Join of_kind_foos, judgements

    and meet_ty env1 env2
          (or_alias1 : S.of_kind_foo ty)
          (or_alias2 : S.of_kind_foo ty)
          : S.of_kind_foo ty * judgements_from_meet =
      let unknown_or_join1, canonical_name1 =
        resolve_aliases_and_squash_unresolved_names_on_ty' env1
          ~kind:S.kind
          ~force_to_kind:S.force_to_kind
          ~unknown:(No_alias Unknown)
          ~print_ty:S.print_ty
          or_alias1
      in
      let unknown_or_join2, canonical_name2 =
        resolve_aliases_and_squash_unresolved_names_on_ty' env2
          ~kind:S.kind
          ~force_to_kind:S.force_to_kind
          ~unknown:(No_alias Unknown)
          ~print_ty:S.print_ty
          or_alias2
      in
      let normal_case ~first_name_to_bind ~first_name_to_bind_level
            ~names_to_bind =
        let unknown_or_join, new_judgements =
          meet_on_unknown_or_join env1 env2
            unknown_or_join1 unknown_or_join2
        in
        let new_judgement =
          first_name_to_bind, first_name_to_bind_level,
            S.to_type (No_alias unknown_or_join)
        in
        let new_judgements' =
          List.map (fun (name, level) ->
              name, level, S.to_type (Type_of first_name_to_bind))
            names_to_bind
        in
        Type_of first_name_to_bind,
          new_judgements @ (new_judgement :: new_judgements')
      in
      let normal_case ~names_to_bind =
        match names_to_bind with
        | [] ->
          let unknown_or_join, new_judgements =
            meet_on_unknown_or_join env1 env2
              unknown_or_join1 unknown_or_join2
          in
          No_alias unknown_or_join, new_judgements
        | (first_name_to_bind, first_name_to_bind_level)::names_to_bind ->
          normal_case ~first_name_to_bind ~first_name_to_bind_level
            ~names_to_bind
      in
      match canonical_name1, canonical_name2 with
      | Some name1, Some name2 when Name.equal name1 name2 ->
        Type_of name1, []
      | Some name1, Some name2 ->
        (* N.B. This needs to respect the [bias_towards] argument on the
           [meet] function exposed in the interface (below). *)
        let level1 = scope_level_typing_environment env1 name1 in
        let level2 = scope_level_typing_environment env2 name2 in
        normal_case ~names_to_bind:[name1, level1; name2, level2]
      | Some name1, None ->
        let level1 = scope_level_typing_environment env1 name1 in
        normal_case ~names_to_bind:[name1, level1]
      | None, Some name2 ->
        let level2 = scope_level_typing_environment env2 name2 in
        normal_case ~names_to_bind:[name2, level2]
      | None, None -> normal_case ~names_to_bind:[]
  end

  module rec Meet_and_join_value : sig
    include Meet_and_join
      with type of_kind_foo := of_kind_value
  end = Make_meet_and_join (struct
    type of_kind_foo = of_kind_value

    let kind = K.value ()

    let to_type ty : t = { descr = Value ty; phantom = None; }
    let force_to_kind = force_to_kind_value
    let print_ty = print_ty_value

    let meet_immediate_case _env1 _env2
          ({ env_extension = env_extension1; } : immediate_case)
          ({ env_extension = env_extension2; } : immediate_case)
          : immediate_case =
      let env_extension =
        match env_extension1, env_extension2 with
        | None, None -> None
        | Some env_extension, None | None, Some env_extension ->
          Some env_extension
        | Some env_extension1, Some env_extension2 ->
          let env_extension =
            Meet_and_join.meet_typing_environment env_extension1 env_extension2
          in
          Some env_extension
      in
      { env_extension; }

    let join_immediate_case _env1 _env2
          ({ env_extension = env_extension1; } : immediate_case)
          ({ env_extension = env_extension2; } : immediate_case)
          : immediate_case =
      (* CR mshinwell: share with the meet function above *)
      let env_extension =
        match env_extension1, env_extension2 with
        | None, None -> None
        | Some env_extension, None | None, Some env_extension ->
          Some env_extension
        | Some env_extension1, Some env_extension2 ->
          let env_extension =
            Meet_and_join.join_typing_environment env_extension1 env_extension2
          in
          Some env_extension
      in
      { env_extension; }

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
          ({ env_extension = env_extension1;
             fields = fields1;
           } : singleton_block)
          ({ env_extension = env_extension2;
             fields = fields2;
           } : singleton_block) : singleton_block * judgements_from_meet =
      let env_extension =
        match env_extension1, env_extension2 with
        | None, None -> None
        | Some env_extension, None | None, Some env_extension ->
          Some env_extension
        | Some env_extension1, Some env_extension2 ->
          let env_extension =
            Meet_and_join.meet_typing_environment env_extension1 env_extension2
          in
          Some env_extension
      in
      assert (Array.length fields1 = Array.length fields2);
      let judgements = ref [] in
      let fields =
        Array.map2
          (fun (field1 : _ mutable_or_immutable)
               (field2 : _ mutable_or_immutable) : _ mutable_or_immutable ->
            match field1, field2 with
            | Mutable, _ | _, Mutable -> Mutable
            | Immutable field1, Immutable field2 ->
              let field, new_judgements =
                Meet_and_join.meet (env1, field1) (env2, field2)
              in
              judgements := new_judgements @ !judgements;
              Immutable field)
          fields1
          fields2
      in
      { env_extension;
        fields;
      }, !judgements

    let join_singleton_block env1 env2
          ({ env_extension = env_extension1;
             fields = fields1;
           } : singleton_block)
          ({ env_extension = env_extension2;
             fields = fields2;
           } : singleton_block) : singleton_block =
      let env_extension =
        (* CR mshinwell: factor this little bit out *)
        match env_extension1, env_extension2 with
        | None, None -> None
        | Some env_extension, None | None, Some env_extension ->
          Some env_extension
        | Some env_extension1, Some env_extension2 ->
          let env_extension =
            Meet_and_join.join_typing_environment env_extension1 env_extension2
          in
          Some env_extension
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
      { env_extension;
        fields;
      }

    let meet_block_cases env1 env2
          ((Join { by_length = singleton_blocks1; }) : block_cases)
          ((Join { by_length = singleton_blocks2; }) : block_cases)
          : (block_cases * judgements_from_meet) Or_bottom.t =
      let judgements = ref [] in
      let by_length =
        Targetint.OCaml.Map.inter_merge
          (fun singleton_block1 singleton_block2 ->
            let singleton_block, new_judgements =
              meet_singleton_block env1 env2
                singleton_block1 singleton_block2
            in
            judgements := new_judgements @ !judgements;
            singleton_block)
          singleton_blocks1
          singleton_blocks2
      in
      if Targetint.OCaml.Map.is_empty by_length then Bottom
      else Ok (((Join { by_length; }) : block_cases), !judgements)

    let join_block_cases env1 env2
          ((Join { by_length = singleton_blocks1; }) : block_cases)
          ((Join { by_length = singleton_blocks2; }) : block_cases)
          : block_cases =
      let by_length =
        Targetint.OCaml.Map.union_merge
          (fun singleton_block1 singleton_block2 ->
            join_singleton_block env1 env2
              singleton_block1 singleton_block2)
          singleton_blocks1
          singleton_blocks2
      in
      Join { by_length; }

    let meet_blocks env1 env2 blocks1 blocks2 : _ Or_bottom.t =
      let judgements = ref [] in
      let blocks =
        Tag.Map.inter (fun block_cases1 block_cases2 ->
            match meet_block_cases env1 env2 block_cases1 block_cases2 with
            | Ok (block_cases, new_judgements) ->
              judgements := new_judgements @ !judgements;
              Some block_cases
            | Bottom -> None)
          blocks1
          blocks2
      in
      if Tag.Map.is_empty blocks then Bottom
      else Ok (blocks, !judgements)

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
          : (blocks_and_tagged_immediates * judgements_from_meet) Or_bottom.t =
      let (blocks : _ or_unknown), judgements =
        match blocks1, blocks2 with
        | Unknown, _ -> blocks2, []
        | _, Unknown -> blocks1, []
        | Known blocks1, Known blocks2 ->
          match meet_blocks env1 env2 blocks1 blocks2 with
          | Bottom -> Known Tag.Map.empty, []
          | Ok (blocks, judgements) -> Known blocks, judgements
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
        let judgements =
          match immediates with
          | Unknown -> judgements
          | Known imms ->
            if not (Immediate.Map.is_empty imms) then judgements
            else  (* CR mshinwell: This should maybe meet across all blocks *)
              match blocks with
              | Unknown -> judgements
              | Known blocks ->
                match Tag.Map.get_singleton blocks with
                | None -> judgements
                | Some (_, Join { by_length; }) ->
                  match Targetint.OCaml.Map.get_singleton by_length with
                  | None -> judgements
                  | Some (_, singleton_block) ->
                    let new_judgements =
                      match singleton_block.env_extension with
                      | None -> []
                      | Some env_extension ->
                        judgements_of_typing_environment env_extension
                    in
                    new_judgements @ judgements
        in
        Ok ({ blocks; immediates; is_int; get_tag; }, judgements)

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
          : (of_kind_value * judgements_from_meet) Or_bottom.t =
      match of_kind1, of_kind2 with
      | Blocks_and_tagged_immediates blocks_imms1,
          Blocks_and_tagged_immediates blocks_imms2 ->
        let blocks_imms =
          meet_blocks_and_tagged_immediates env1 env2
            blocks_imms1 blocks_imms2
        in
        begin match blocks_imms with
        | Ok (blocks_imms, judgements) ->
          Ok (Blocks_and_tagged_immediates blocks_imms, judgements)
        | Bottom -> Bottom
        end
      | Boxed_number (Boxed_float n1),
          Boxed_number (Boxed_float n2) ->
        let (n : _ ty_naked_number), judgements =
          Meet_and_join_naked_float.meet_ty env1 env2 n1 n2
        in
        Ok (Boxed_number (Boxed_float n), judgements)
      | Boxed_number (Boxed_int32 n1),
          Boxed_number (Boxed_int32 n2) ->
        let (n : _ ty_naked_number), judgements =
          Meet_and_join_naked_int32.meet_ty env1 env2 n1 n2
        in
        Ok (Boxed_number (Boxed_int32 n), judgements)
      | Boxed_number (Boxed_int64 n1),
          Boxed_number (Boxed_int64 n2) ->
        let (n : _ ty_naked_number), judgements =
          Meet_and_join_naked_int64.meet_ty env1 env2 n1 n2
        in
        Ok (Boxed_number (Boxed_int64 n), judgements)
      | Boxed_number (Boxed_nativeint n1),
          Boxed_number (Boxed_nativeint n2) ->
        let (n : _ ty_naked_number), judgements =
          Meet_and_join_naked_nativeint.meet_ty env1 env2 n1 n2
        in
        Ok (Boxed_number (Boxed_nativeint n), judgements)
      | Closures closures1, Closures closures2 ->
        let judgements = ref [] in
        let closures =
          Closure_id.Map.inter
            (fun (closures_entry1 : closures_entry)
                 (closures_entry2 : closures_entry) : closures_entry option ->
              let set1 = closures_entry1.set_of_closures in
              let set2 = closures_entry2.set_of_closures in
              let set, new_judgements =
                Meet_and_join_fabricated.meet_ty env1 env2 set1 set2
              in
              if ty_is_obviously_bottom set then begin
                None
              end else begin
                judgements := new_judgements @ !judgements;
                Some { set_of_closures = set; }
              end)
            closures1
            closures2
        in
        if Closure_id.Map.is_empty closures then Bottom
        else Ok (Closures closures, !judgements)
      | String strs1, String strs2 ->
        let strs = String_info.Set.inter strs1 strs2 in
        if String_info.Set.is_empty strs then Bottom
        else Ok (String strs, [])
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
          : (Immediate.Set.t of_kind_naked_number * judgements_from_meet)
              Or_bottom.t =
      match of_kind1, of_kind2 with
      | Immediate fs1, Immediate fs2 ->
        let fs = Immediate.Set.inter fs1 fs2 in
        if Immediate.Set.is_empty fs then Bottom
        else Ok (Immediate fs, [])
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
              * judgements_from_meet) Or_bottom.t =
      match of_kind1, of_kind2 with
      | Float fs1, Float fs2 ->
        let fs = Float_by_bit_pattern.Set.inter fs1 fs2 in
        if Float_by_bit_pattern.Set.is_empty fs then Bottom
        else Ok (Float fs, [])
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
          : (Int32.Set.t of_kind_naked_number * judgements_from_meet)
              Or_bottom.t =
      match of_kind1, of_kind2 with
      | Int32 is1, Int32 is2 ->
        let is = Int32.Set.inter is1 is2 in
        if Int32.Set.is_empty is then Bottom
        else Ok (Int32 is, [])
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
          : (Int64.Set.t of_kind_naked_number * judgements_from_meet)
              Or_bottom.t =
      match of_kind1, of_kind2 with
      | Int64 is1, Int64 is2 ->
        let is = Int64.Set.inter is1 is2 in
        if Int64.Set.is_empty is then Bottom
        else Ok (Int64 is, [])
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
          : (Targetint.Set.t of_kind_naked_number * judgements_from_meet)
              Or_bottom.t =
      match of_kind1, of_kind2 with
      | Nativeint is1, Nativeint is2 ->
        let is = Targetint.Set.inter is1 is2 in
        if Targetint.Set.is_empty is then Bottom
        else Ok (Nativeint is, [])
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
          : (closure * judgements_from_meet) Or_bottom.t =
      let cannot_prove_different ~params1 ~params2 ~result1 ~result2
            ~result_env_extension1 ~result_env_extension2 : _ Or_bottom.t =
        let same_arity =
          List.compare_lengths params1 params2 = 0
        in
        let same_num_results =
          List.compare_lengths result1 result2 = 0
        in
        let result_env_extension =
          match result_env_extension1, result_env_extension2 with
          | None, None -> None
          | Some env_extension, None | None, Some env_extension ->
            Some env_extension
          | Some env_extension1, Some env_extension2 ->
            let env_extension =
              Meet_and_join.meet_typing_environment env_extension1
                env_extension2
            in
            Some env_extension
        in
        let judgements = ref [] in
        let has_bottom params =
          List.exists is_obviously_bottom params
        in
        let params : _ Or_bottom.t =
          if not same_arity then Bottom
          else
            let params =
              List.map2 (fun t1 t2 ->
                  let t, new_judgements =
                    Meet_and_join.meet (env1, t1) (env2, t2)
                  in
                  judgements := new_judgements @ !judgements;
                  t)
                params1
                params2
            in
            if has_bottom params then Bottom
            else Ok params
        in
        let result : _ Or_bottom.t =
          if not same_num_results then Bottom
          else
            let result =
              List.map2 (fun t1 t2 ->
                  let result_env_extension1 =
                    match result_env_extension1 with
                    | None -> env1
                    | Some env -> env
                  in
                  let result_env_extension2 =
                    match result_env_extension2 with
                    | None -> env2
                    | Some env -> env
                  in
                  let t, new_judgements =
                    Meet_and_join.meet (result_env_extension1, t1)
                      (result_env_extension2, t2)
                  in
                  judgements := new_judgements @ !judgements;
                  t)
                result1
                result2
            in
            if has_bottom result then Bottom
            else Ok result
        in
        match params, result with
        | Ok params, Ok result ->
          Ok (params, result, result_env_extension, !judgements)
        | _, _ -> Bottom
      in
      let function_decls : _ Or_bottom.t =
        match closure1.function_decls, closure2.function_decls with
        | Inlinable inlinable1, Inlinable inlinable2 ->
          let params1 = List.map snd inlinable1.params in
          let params2 = List.map snd inlinable2.params in
          let result =
            cannot_prove_different ~params1 ~params2
              ~result1:inlinable1.result
              ~result2:inlinable2.result
              ~result_env_extension1:inlinable1.result_env_extension
              ~result_env_extension2:inlinable2.result_env_extension
          in
          begin match result with
          | Ok (params, result, result_env_extension, judgements) ->
            (* [closure1.function_decls] and [closure2.function_decls] may be
               different, but we cannot prove it.  We arbitrarily pick
               [closure1.function_decls] to return, with parameter and result
               types refined. *)
            let params =
              List.map2 (fun (param, _old_ty) new_ty -> param, new_ty)
                inlinable1.params
                params
            in
            let inlinable_function_decl =
              { inlinable1 with
                params;
                result;
                result_env_extension;
              }
            in
            Ok (Inlinable inlinable_function_decl, judgements)
          | Bottom ->
            (* [closure1] and [closure2] are definitely different. *)
            Bottom
          end
        | Non_inlinable None, Non_inlinable None -> Ok (Non_inlinable None, [])
        | Non_inlinable (Some non_inlinable), Non_inlinable None
        | Non_inlinable None, Non_inlinable (Some non_inlinable) ->
          (* We can arbitrarily pick one side or the other: we choose the
             side which gives a more precise type. *)
          Ok (Non_inlinable (Some non_inlinable), [])
        | Non_inlinable None, Inlinable inlinable
        | Inlinable inlinable, Non_inlinable None ->
          (* Likewise. *)
          Ok (Inlinable inlinable, [])
        | Non_inlinable (Some non_inlinable1),
            Non_inlinable (Some non_inlinable2) ->
          let result =
            cannot_prove_different
              ~params1:non_inlinable1.params
              ~params2:non_inlinable2.params
              ~result1:non_inlinable1.result
              ~result2:non_inlinable2.result
              ~result_env_extension1:non_inlinable1.result_env_extension
              ~result_env_extension2:non_inlinable2.result_env_extension
          in
          begin match result with
          | Ok (params, result, result_env_extension, judgements) ->
            let non_inlinable_function_decl =
              { non_inlinable1 with
                params;
                result;
                result_env_extension;
              }
            in
            Ok (Non_inlinable (Some non_inlinable_function_decl), judgements)
          | Bottom ->
            Bottom
          end
        | Non_inlinable (Some non_inlinable), Inlinable inlinable
        | Inlinable inlinable, Non_inlinable (Some non_inlinable) ->
          let params1 = List.map snd inlinable.params in
          let result =
            cannot_prove_different
              ~params1
              ~params2:non_inlinable.params
              ~result1:inlinable.result
              ~result2:non_inlinable.result
              ~result_env_extension1:inlinable.result_env_extension
              ~result_env_extension2:non_inlinable.result_env_extension
          in
          begin match result with
          | Ok (params, result, result_env_extension, judgements) ->
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
                result_env_extension;
              }
            in
            Ok (Inlinable inlinable_function_decl, judgements)
          | Bottom ->
            Bottom
          end
      in
      match function_decls with
      | Bottom -> Bottom
      | Ok (function_decls, judgements) ->
        Ok (({ function_decls; } : closure), judgements)

    let join_closure env1 env2
          (closure1 : closure) (closure2 : closure)
          : closure =
      let produce_non_inlinable ~params1 ~params2 ~result1 ~result2
            ~result_env_extension1 ~result_env_extension2
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
          let result =
            let result_env_extension1 =
              match result_env_extension1 with
              | None -> env1
              | Some env -> env
            in
            let result_env_extension2 =
              match result_env_extension2 with
              | None -> env2
              | Some env -> env
            in
            List.map2 (fun t1 t2 ->
                Meet_and_join.join (result_env_extension1, t1)
                  (result_env_extension2, t2))
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
          let result_env_extension =
            match result_env_extension1, result_env_extension2 with
            | None, None -> None
            | Some env_extension, None | None, Some env_extension ->
              Some env_extension
            | Some env_extension1, Some env_extension2 ->
              let env_extension =
                Meet_and_join.join_typing_environment env_extension1
                  env_extension2
              in
              Some env_extension
          in
          let non_inlinable : non_inlinable_function_declarations =
            { params;
              result;
              result_env_extension;
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
            ~result_env_extension1:non_inlinable1.result_env_extension
            ~result_env_extension2:non_inlinable2.result_env_extension
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
            ~result_env_extension1:inlinable.result_env_extension
            ~result_env_extension2:non_inlinable.result_env_extension
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
              ~result_env_extension1:inlinable1.result_env_extension
              ~result_env_extension2:inlinable2.result_env_extension
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
            let result_env_extension =
              match inlinable1.result_env_extension,
                inlinable2.result_env_extension
              with
              | None, None -> None
              | Some env_extension, None | None, Some env_extension ->
                Some env_extension
              | Some env_extension1, Some env_extension2 ->
                let env_extension =
                  Meet_and_join.join_typing_environment env_extension1
                    env_extension2
                in
                Some env_extension
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
              (* CR mshinwell: must share with above *)
              let result_env_extension1 =
                match inlinable1.result_env_extension with
                | None -> env1
                | Some env -> env
              in
              let result_env_extension2 =
                match inlinable2.result_env_extension with
                | None -> env2
                | Some env -> env
              in
              List.map2 (fun t1 t2 ->
                  Meet_and_join.join (result_env_extension1, t1)
                    (result_env_extension2, t2))
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
              result_env_extension;
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

    let meet_set_of_closures env1 env2
          (set1 : set_of_closures) (set2 : set_of_closures)
          : (set_of_closures * judgements_from_meet) Or_bottom.t =
      let judgements = ref [] in
      (* CR mshinwell: Try to refactor this code to shorten it. *)
      let closures : _ extensibility =
        match set1.closures, set2.closures with
        | Exactly closures1, Exactly closures2 ->
          let closures =
            Closure_id.Map.inter (fun ty_fabricated1 ty_fabricated2 ->
                let ty_fabricated, new_judgements =
                  Meet_and_join_fabricated.meet_ty env1 env2
                    ty_fabricated1 ty_fabricated2
                in
                if ty_is_obviously_bottom ty_fabricated then begin
                  None
                end else begin
                  judgements := new_judgements @ !judgements;
                  Some ty_fabricated
                end)
              closures1
              closures2
          in
          Exactly closures
        | Exactly closures1, Open closures2
        | Open closures2, Exactly closures1 ->
          let closures =
            Closure_id.Map.filter_map closures1 ~f:(fun closure_id ty1 ->
              match Closure_id.Map.find closure_id closures2 with
              | exception Not_found -> Some ty1
              | ty2 ->
                let ty_fabricated, new_judgements =
                  Meet_and_join_fabricated.meet_ty env1 env2 ty1 ty2
                in
                if ty_is_obviously_bottom ty_fabricated then begin
                  None
                end else begin
                  judgements := new_judgements @ !judgements;
                  Some ty_fabricated
                end)
          in
          Exactly closures
        | Open closures1, Open closures2 ->
          let closures =
            Closure_id.Map.union_merge (fun ty_fabricated1 ty_fabricated2 ->
                let ty_fabricated, new_judgements =
                  Meet_and_join_fabricated.meet_ty env1 env2
                    ty_fabricated1 ty_fabricated2
                in
                if ty_is_obviously_bottom ty_fabricated then begin
                  bottom_as_ty_fabricated ()
                end else begin
                  judgements := new_judgements @ !judgements;
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
                let ty_value, new_judgements =
                  Meet_and_join_value.meet_ty env1 env2
                    ty_value1 ty_value2
                in
                if ty_is_obviously_bottom ty_value then begin
                  None
                end else begin
                  judgements := new_judgements @ !judgements;
                  Some ty_value
                end)
              closure_elements1
              closure_elements2
          in
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
                  let ty_value, new_judgements =
                    Meet_and_join_value.meet_ty env1 env2 ty1 ty2
                  in
                  if ty_is_obviously_bottom ty_value then begin
                    None
                  end else begin
                    judgements := new_judgements @ !judgements;
                    Some ty_value
                  end)
          in
          Exactly closure_elements
        | Open closure_elements1, Open closure_elements2 ->
          let closure_elements =
            Var_within_closure.Map.union_merge (fun ty_value1 ty_value2 ->
                let ty_value, new_judgements =
                  Meet_and_join_value.meet_ty env1 env2
                    ty_value1 ty_value2
                in
                if ty_is_obviously_bottom ty_value then begin
                  bottom_as_ty_value ()
                end else begin
                  judgements := new_judgements @ !judgements;
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
        let set : set_of_closures =
          { closures;
            closure_elements;
          }
        in
        Ok (set, !judgements)

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
      { closures;
        closure_elements;
      }

    let meet_of_kind_foo env1 env2
          (of_kind1 : of_kind_fabricated) (of_kind2 : of_kind_fabricated)
          : (of_kind_fabricated * judgements_from_meet) Or_bottom.t =
      match of_kind1, of_kind2 with
      | Tag tags1, Tag tags2 ->
        let tags =
          Tag.Map.inter_merge
            (fun ({ env_extension = env_extension1; } : tag_case)
                  ({ env_extension = env_extension2; } : tag_case)
                  : tag_case ->
              let env_extension =
                match env_extension1, env_extension2 with
                | None, None -> None
                | Some env_extension, None | None, Some env_extension ->
                  Some env_extension
                | Some env_extension1, Some env_extension2 ->
                  let env_extension =
                    Meet_and_join.meet_typing_environment env_extension1
                      env_extension2
                  in
                  Some env_extension
              in
              (* CR mshinwell: Do we ever flip back to [Bottom] here? *)
              { env_extension; })
            tags1
            tags2
        in
        let judgements =
          match Tag.Map.get_singleton tags with
          | None -> []
          | Some (_, tag_case) ->
            match tag_case.env_extension with
            | None -> []
            | Some env_extension ->
              judgements_of_typing_environment env_extension
        in
        Ok (Tag tags, judgements)
      | Set_of_closures set1, Set_of_closures set2 ->
        begin match meet_set_of_closures env1 env2 set1 set2 with
        | Ok (set_of_closures, judgements) ->
          Ok (Set_of_closures set_of_closures, judgements)
        | Bottom -> Bottom
        end
      | Closure closure1, Closure closure2 ->
        begin match meet_closure env1 env2 closure1 closure2 with
        | Ok (closure, judgements) -> Ok (Closure closure, judgements)
        | Bottom -> Bottom
        end
      | (Tag _ | Set_of_closures _ | Closure _), _ -> Bottom

    let join_of_kind_foo env1 env2
          (of_kind1 : of_kind_fabricated) (of_kind2 : of_kind_fabricated)
          : of_kind_fabricated or_unknown =
      match of_kind1, of_kind2 with
      | Tag tags1, Tag tags2 ->
        let tags =
          Tag.Map.union_merge
            (fun ({ env_extension = env_extension1; } : tag_case)
                  ({ env_extension = env_extension2; } : tag_case)
                  : tag_case ->
              let env_extension =
                match env_extension1, env_extension2 with
                | None, None -> None
                | Some env_extension, None | None, Some env_extension ->
                  Some env_extension
                | Some env_extension1, Some env_extension2 ->
                  let env_extension =
                    Meet_and_join.join_typing_environment env_extension1
                      env_extension2
                  in
                  Some env_extension
              in
              { env_extension; })
            tags1
            tags2
        in
        Known (Tag tags)
      | Set_of_closures set1, Set_of_closures set2 ->
        let set_of_closures = join_set_of_closures env1 env2 set1 set2 in
        Known (Set_of_closures set_of_closures)
      | Closure closure1, Closure closure2 ->
        let closure = join_closure env1 env2 closure1 closure2 in
        Known (Closure closure)
      | (Tag _ | Set_of_closures _ | Closure _), _ -> Unknown
  end) and Meet_and_join : sig
    val meet : t_in_context -> t_in_context -> t * judgements_from_meet

    val join : t_in_context -> t_in_context -> t

    val meet_typing_environment
       : typing_environment
      -> typing_environment
      -> typing_environment

    val join_typing_environment
       : typing_environment
      -> typing_environment
      -> typing_environment

    val replace_meet_typing_environment0
       : typing_environment
      -> Name.t
      -> scope_level:Scope_level.t
      -> existing_ty:t
      -> t_in_context
      -> typing_environment

    val replace_meet_typing_environment
       : typing_environment
      -> Name.t
      -> t_in_context
      -> typing_environment
  end = struct
    let meet (env1, (t1 : t)) (env2, (t2 : t)) : t * judgements_from_meet =
      ensure_phantomness_matches t1 t2 "kind mismatch upon meet";
      let descr, judgements =
        match t1.descr, t2.descr with
        | Value ty_value1, Value ty_value2 ->
          let ty_value, judgements =
            Meet_and_join_value.meet_ty env1 env2 ty_value1 ty_value2
          in
          Value ty_value, judgements
        | Naked_number (ty_naked_number1, kind1),
            Naked_number (ty_naked_number2, kind2) ->
          let module N = K.Naked_number in
          begin match kind1, kind2 with
          | N.Naked_immediate, N.Naked_immediate ->
            let ty_naked_number, judgements =
              Meet_and_join_naked_immediate.meet_ty env1 env2
                ty_naked_number1 ty_naked_number2
            in
            Naked_number (ty_naked_number, N.Naked_immediate), judgements
          | N.Naked_float, N.Naked_float ->
            let ty_naked_number, judgements =
              Meet_and_join_naked_float.meet_ty env1 env2
                ty_naked_number1 ty_naked_number2
            in
            Naked_number (ty_naked_number, N.Naked_float), judgements
          | N.Naked_int32, N.Naked_int32 ->
            let ty_naked_number, judgements =
              Meet_and_join_naked_int32.meet_ty env1 env2
                ty_naked_number1 ty_naked_number2
            in
            Naked_number (ty_naked_number, N.Naked_int32), judgements
          | N.Naked_int64, N.Naked_int64 ->
            let ty_naked_number, judgements =
              Meet_and_join_naked_int64.meet_ty env1 env2
                ty_naked_number1 ty_naked_number2
            in
            Naked_number (ty_naked_number, N.Naked_int64), judgements
          | N.Naked_nativeint, N.Naked_nativeint ->
            let ty_naked_number, judgements =
              Meet_and_join_naked_nativeint.meet_ty env1 env2
                ty_naked_number1 ty_naked_number2
            in
            Naked_number (ty_naked_number, N.Naked_nativeint), judgements
          | _, _ ->
            Misc.fatal_errorf "Kind mismatch upon meet:@ %a@ versus@ %a"
              print t1
              print t2
          end
        | Fabricated ty_fabricated1, Fabricated ty_fabricated2 ->
          let ty_fabricated, judgements =
            Meet_and_join_fabricated.meet_ty env1 env2
              ty_fabricated1 ty_fabricated2
          in
          Fabricated ty_fabricated, judgements
        | (Value _ | Naked_number _ | Fabricated _), _ ->
          Misc.fatal_errorf "Kind mismatch upon meet:@ %a@ versus@ %a"
            print t1
            print t2
      in
      let t = { t1 with descr; } in
      t, judgements

    let join (env1, (t1 : t)) (env2, (t2 : t)) =
      ensure_phantomness_matches t1 t2 "kind mismatch upon join";
      let descr =
        match t1.descr, t2.descr with
        | Value ty_value1, Value ty_value2 ->
          let ty_value =
            Meet_and_join_value.join_ty env1 env2 ty_value1 ty_value2
          in
          Value ty_value
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
          Fabricated ty_fabricated
        | (Value _ | Naked_number _ | Fabricated _), _ ->
          Misc.fatal_errorf "Kind mismatch upon join:@ %a@ versus %a"
            print t1
            print t2
      in
      { t1 with descr; }

    let join_typing_environment (env1 : typing_environment)
          (env2 : typing_environment) =
      let canonical_names_to_aliases =
        Name.Map.union_merge Name.Set.union
          env1.canonical_names_to_aliases
          env2.canonical_names_to_aliases
      in
      let names_to_types =
        Name.Map.union_merge (fun (level1, ty1) (level2, ty2) ->
            if not (Scope_level.equal level1 level2) then begin
              Misc.fatal_errorf "join_typing_environment: \
                  Scope levels differ for:@ %a@ and:@ %a"
                print ty1
                print ty2
            end;
            (* When joining (or meeting) environments, all free names in the
               types in such environments must be in those environments
               themselves, otherwise the well-formedness condition for
               environments being closed would not be respected. *)
            let ty = join (env1, ty1) (env2, ty2) in
            level1, ty)
          env1.names_to_types
          env2.names_to_types
      in
      let all_levels_to_names =
        Scope_level.Map.union_merge
          (fun names1 names2 -> Name.Set.union names1 names2)
          env1.levels_to_names
          env2.levels_to_names
      in
      let levels_to_names =
        Scope_level.Map.map (fun names ->
            Name.Set.filter (fun name ->
                Name.Map.mem name names_to_types)
              names)
          all_levels_to_names
      in
      let existentials =
        Name.Set.union env1.existentials env2.existentials
      in
      let existential_freshening =
        env1.existential_freshening (* XXX *)
      in
      { resolver = env1.resolver;
        canonical_names_to_aliases;
        names_to_types;
        levels_to_names;
        existentials;
        existential_freshening;
      }

    let print_judgements ppf judgements =
      Format.pp_print_list ~pp_sep:Format.pp_print_space
        (fun ppf (name, _level, ty) ->
          Format.fprintf ppf "@[(%s%a%s@ %a)@]"
            (Misc_color.bold_green ())
            Name.print name
            (Misc_color.reset ())
            print ty)
        ppf judgements

    (* CR mshinwell: Should the judgements come with environments? *)
    let rec meet_typing_environment_with_judgements ~num_iterations
          (env : typing_environment) ~(judgements : judgements_from_meet) =
      try
        if num_iterations >= 10 then env
        else begin
          match judgements with
          | [] -> env
          | (name, scope_level, ty)::judgements ->
            let scope_level, existing_ty =
              match Name.Map.find name env.names_to_types with
              | exception Not_found ->
                scope_level, unknown (kind ty)
              | existing_scope_level, existing_ty ->
                assert (Scope_level.equal scope_level existing_scope_level);
                scope_level, existing_ty
            in
            (* XXX I suspect we need an "add in parallel" operation on
               typing environments which does the additions then checks
               the closedness invariant. *)
            (* CR mshinwell: How do we know that [env] is the correct
               environment for [judgements]? *)
            let ty, new_judgements = meet (env, ty) (env, existing_ty) in
            let env =
              add_or_replace_typing_environment' env name scope_level ty
            in
            let judgements = new_judgements @ judgements in
            meet_typing_environment_with_judgements
              ~num_iterations:(num_iterations + 1) env ~judgements
        end
      with Misc.Fatal_error -> begin
        Format.eprintf "\n%sContext is: applying judgements:%s\
            @ %a\n"
          (Misc_color.bold_red ())
          (Misc_color.reset ())
          print_judgements judgements;
        raise Misc.Fatal_error
      end

    and replace_meet_typing_environment0 env name
          ~scope_level ~existing_ty ty_in_context =
      let ty, judgements = meet ty_in_context (env, existing_ty) in
      let env = add_or_replace_typing_environment env name scope_level ty in
      meet_typing_environment_with_judgements
        ~num_iterations:0 env ~judgements

    let meet_typing_environment (env1 : typing_environment)
          (env2 : typing_environment) =
      let judgements = ref [] in
      let canonical_names_to_aliases =
        Name.Map.union_merge Name.Set.union
          env1.canonical_names_to_aliases
          env2.canonical_names_to_aliases
      in
      let names_to_types =
        Name.Map.union_merge (fun (level1, ty1) (level2, ty2) ->
            if not (Scope_level.equal level1 level2) then begin
              Misc.fatal_errorf "meet_typing_environment: \
                  Scope levels differ for:@ %a@ and:@ %a@ levels1:@ %a@ \
                  levels2:@ %a@ env1:@ %a@ env2:@ %a"
                print ty1
                print ty2
                (Scope_level.Map.print Name.Set.print) env1.levels_to_names
                (Scope_level.Map.print Name.Set.print) env2.levels_to_names
                print_typing_environment env1
                print_typing_environment env2
            end;
(*
Format.eprintf "Meeting@ %a and@ %a ...\n%!" print ty1 print ty2;
*)
            let ty, new_judgements = meet (env1, ty1) (env2, ty2) in
(*
Format.eprintf "...giving %a\n%!" print ty;
*)
            judgements := new_judgements @ !judgements;
            level1, ty)
          env1.names_to_types
          env2.names_to_types
      in
      let all_levels_to_names =
        Scope_level.Map.union_merge
          (fun names1 names2 -> Name.Set.union names1 names2)
          env1.levels_to_names
          env2.levels_to_names
      in
      let levels_to_names =
        Scope_level.Map.map (fun names ->
            Name.Set.filter (fun name ->
                Name.Map.mem name names_to_types)
              names)
          all_levels_to_names
      in
      let existentials =
        Name.Set.union env1.existentials env2.existentials
      in
      let existential_freshening =
        env1.existential_freshening (* XXX *)
      in
      let env =
        { resolver = env1.resolver;
          canonical_names_to_aliases;
          names_to_types;
          levels_to_names;
          existentials;
          existential_freshening;
        }
      in
      try
        meet_typing_environment_with_judgements ~num_iterations:0
          env ~judgements:!judgements
      with Misc.Fatal_error -> begin
        Format.eprintf "\n%sContext is: meeting two typing environments:%s\
            @ %a\n\n%sand%s:@ %a\n"
          (Misc_color.bold_red ())
          (Misc_color.reset ())
          print_typing_environment env1
          (Misc_color.bold_red ())
          (Misc_color.reset ())
          print_typing_environment env2;
        raise Misc.Fatal_error
      end

    let replace_meet_typing_environment env name t_in_context =
      match Name.Map.find name env.names_to_types with
      | exception Not_found ->
        Misc.fatal_errorf "Cannot meet types for name %a which is unbound \
            in the environment: %a"
          Name.print name
          print_typing_environment env
      | scope_level, existing_ty ->
        try
          replace_meet_typing_environment0 env name
            ~scope_level ~existing_ty t_in_context
        with Misc.Fatal_error -> begin
          Format.eprintf "\n%sContext is: replace-meet on a member %a of a \
              typing environment:%s@ Existing type:@ %a@ \
              Refined type:@ %a@ Environment of refined type:@ %a\n"
            (Misc_color.bold_red ())
            Name.print name
            (Misc_color.reset ())
            print existing_ty
            print (snd t_in_context)
            print_typing_environment (fst t_in_context);
          raise Misc.Fatal_error
        end
  end

  let meet ~output_env tc1 tc2 =
    let t, judgements = Meet_and_join.meet tc1 tc2 in
    let output_env =
      List.fold_left (fun output_env (name, scope_level, ty) ->
          let env, _ty =
            (* We can choose either the environment from [tc1] or [tc2],
               since [t] is the meet of the type components of those. *)
            tc1
          in
          match Name.Map.find name output_env.names_to_types with
          | exception Not_found ->
            Meet_and_join.replace_meet_typing_environment0 output_env
              name ~scope_level ~existing_ty:(unknown (kind ty))
              (env, ty)
          | existing_scope_level, existing_ty ->
            assert (Scope_level.equal scope_level existing_scope_level);
            Meet_and_join.replace_meet_typing_environment0 output_env
              name ~scope_level ~existing_ty
              (env, ty))
        output_env
        judgements
    in
    output_env, t

  let meet ~output_env ~bias_towards tc2 =
    meet ~output_env bias_towards tc2

  let join = Meet_and_join.join

  let join_ty_value (env1, ty_value1) (env2, ty_value2) =
    Meet_and_join_value.join_ty env1 env2 ty_value1 ty_value2

  module Typing_environment = struct
    type t = typing_environment

    (* CR mshinwell: Add invariant check.  First one: symbols should never be
       existential *)

    let print = print_typing_environment
    let create = create_typing_environment

    let create_using_resolver_from t = create ~resolver:t.resolver

    let add_or_replace = add_or_replace_typing_environment

    let add t name scope_level ty =
      match Name.Map.find name t.names_to_types with
      | exception Not_found -> add_or_replace t name scope_level ty
      | _ty ->
        Misc.fatal_errorf "Cannot rebind %a in environment:@ %a"
          Name.print name
          print t

    let singleton ~resolver name scope_level ty =
      add (create ~resolver) name scope_level ty

    let find = find_typing_environment

    let scope_level t name =
      match Name.Map.find name t.names_to_types with
      | exception Not_found ->
        Misc.fatal_errorf "scope_level: Cannot find %a in environment:@ %a"
          Name.print name
          print t
      | scope_level, _ty -> scope_level

    (* CR mshinwell: improve efficiency *)
    let find_with_scope_level t name =
      let ty, binding_type = find t name in
      let scope_level = scope_level t name in
      ty, scope_level, binding_type

    let replace_meet = Meet_and_join.replace_meet_typing_environment

    let add_or_replace_meet t name scope_level ty =
      match Name.Map.find name t.names_to_types with
      | exception Not_found -> add t name scope_level ty
      | scope_level, existing_ty ->
        (* CR mshinwell: We need to think about this some more.  Is [ty]
           supposed to only have free names in [t]? *)
        Meet_and_join.replace_meet_typing_environment0 t name
          ~scope_level ~existing_ty (t, ty)

    let find_opt t name =
      match Name.Map.find name t.names_to_types with
      | exception Not_found -> None
      | _scope_level, ty ->
        let binding_type =
          if Name.Set.mem name t.existentials then Existential
          else Normal
        in
        match binding_type with
        | Normal -> Some (ty, Normal)
        | Existential ->
     (* XXX     let ty = rename_variables t freshening in *)
          Some (ty, Existential)

    let is_existential t name =
      let _ty, binding_type = find t name in
      match binding_type with
      | Normal -> false
      | Existential -> true

    let cut t ~existential_if_defined_at_or_later_than =
(*
Format.eprintf "Cutting environment at %a: %a\n%!"
  Scope_level.print existential_if_defined_at_or_later_than
  print_typing_environment t;
*)
      let new_existentials =
        Scope_level.Map.fold (fun scope_level names resulting_existentials ->
            let will_be_existential =
              Scope_level.(>=)
                scope_level existential_if_defined_at_or_later_than
            in
            if will_be_existential then
              let non_symbols = Name.variables_only names in
              Name.Set.union non_symbols resulting_existentials
            else
              resulting_existentials)
          t.levels_to_names
          Name.Set.empty
      in
      let existential_freshening =
        Name.Set.fold (fun (name : Name.t) freshening ->
            match name with
            | Symbol _ ->
              Misc.fatal_error "Symbols cannot be existentially bound"
            | Var var ->
              let _new_var, freshening =
                Freshening.add_variable freshening var
              in
              freshening)
          new_existentials
          t.existential_freshening
      in
let result =
      (* XXX we actually need to rename in the domain of [names_to_types] *)
      { resolver = t.resolver;
        canonical_names_to_aliases = t.canonical_names_to_aliases;
        names_to_types = t.names_to_types;
        levels_to_names = t.levels_to_names;
        existentials = Name.Set.union t.existentials new_existentials;
        existential_freshening;
      }
in
(*
Format.eprintf "Result is: %a\n%!"
  print_typing_environment result;
*)
      result

  let add_alias = add_alias_typing_environment
  let aliases = aliases_typing_environment

    let meet = Meet_and_join.meet_typing_environment
    let join = Meet_and_join.join_typing_environment

    let restrict_to_names0 t allowed =
      let names_to_types =
        Name.Map.filter (fun name _ty -> Name.Set.mem name allowed)
          t.names_to_types
      in
      let levels_to_names =
        Scope_level.Map.filter_map t.levels_to_names ~f:(fun _level names ->
          let names = Name.Set.inter names allowed in
          if Name.Set.is_empty names then None
          else Some names)
      in
      let existentials = Name.Set.inter t.existentials allowed in
      let existential_freshening =
        Freshening.restrict_to_names t.existential_freshening allowed
      in
      { resolver = t.resolver;
        canonical_names_to_aliases = t.canonical_names_to_aliases;
        names_to_types;
        levels_to_names;
        existentials;
        existential_freshening;
      }

    let restrict_to_names t allowed =
      let allowed = Name_occurrences.everything allowed in
      restrict_to_names0 t allowed

    let restrict_to_symbols t =
      let symbols = Name.symbols_only_map t.names_to_types in
      restrict_to_names0 t (Name.Map.keys symbols)

    let is_empty t = Name.Map.is_empty t.names_to_types

    let domain t =
      let domain =
        Name.Set.diff (Name.Map.keys t.names_to_types) t.existentials
      in
      Name_occurrences.create_from_set_in_terms domain

    let resolver t = t.resolver
  end

  let add_judgements (env, t) : t =
    let t, _canonical_name = resolve_aliases (env, t) in
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
                      (fun ((Join { by_length }) : block_cases) : block_cases ->
                        let by_length =
                          Targetint.OCaml.Map.map
                            (fun (block : singleton_block) : singleton_block ->
                              let env_extension =
                                match block.env_extension with
                                | None -> env
                                | Some env_extension ->
                                  Typing_environment.meet env_extension env
                              in
                              { block with env_extension = Some env_extension; })
                            by_length
                        in
                        Join { by_length; })
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
                      (fun ({ env_extension; } : immediate_case)
                            : immediate_case ->
                        let env_extension =
                          match env_extension with
                          | None -> env
                          | Some env_extension ->
                            Typing_environment.meet env_extension env
                        in
                        { env_extension = Some env_extension; })
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
            | Tag tag_map ->
              let tag_map =
                Tag.Map.map (fun ({ env_extension; } : tag_case) : tag_case ->
                    let env_extension =
                      match env_extension with
                      | None -> env
                      | Some env_extension ->
                        Typing_environment.meet env_extension env
                    in
                    { env_extension = Some env_extension; })
                  tag_map
              in
              Tag tag_map
            | Set_of_closures _
            | Closure _ -> of_kind_fabricated)
          of_kind_fabricateds
      in
      { t with
        descr = Fabricated (No_alias (Join of_kind_fabricateds));
      }
    | Value (Type _ | Type_of _ | No_alias Unknown)
    | Fabricated (Type _ | Type_of _ | No_alias Unknown) -> t
    | Naked_number _ -> t
end
