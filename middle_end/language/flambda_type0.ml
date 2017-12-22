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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Float_by_bit_pattern = Numbers.Float_by_bit_pattern
module Int32 = Numbers.Int32
module Int64 = Numbers.Int64

module K = Flambda_kind

module Make (Expr : sig
  type t
  val print : Format.formatter -> t -> unit
  val free_names : t -> Name.Set.t
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

  type 'a or_alias =
    | No_alias of 'a
    | Type of Export_id.t
    | Type_of of Name.t

  type 'a or_unknown_length =
    | Exactly of 'a
    | Unknown_length

  type t =
    | Value of ty_value
    | Naked_number :
        'kind ty_naked_number * 'kind K.Naked_number.t -> t
    | Fabricated of ty_fabricated
    | Phantom of ty_phantom

  and flambda_type = t

  and ty_value = (of_kind_value, K.Value_kind.t) ty
  and 'a ty_naked_number = ('a of_kind_naked_number, unit) ty
  and ty_fabricated = (of_kind_fabricated, K.Value_kind.t) ty
  and ty_phantom = (of_kind_phantom, K.Phantom_kind.t) ty

  and ('a, 'u) ty = ('a, 'u) or_unknown_or_bottom or_alias

  and ('a, 'u) or_unknown_or_bottom =
    | Unknown of 'u
    | Ok of 'a or_join
    | Bottom

  and 'a or_join =
    | Normal of 'a
    | Join of 'a or_join or_alias * 'a or_join or_alias

  and of_kind_value =
    | Blocks_and_tagged_immediates of blocks_and_tagged_immediates
    | Boxed_number : _ of_kind_value_boxed_number -> of_kind_value
    | Closure of closures
    | String of String_info.Set.t

  and immediate_case = {
    env_extension : typing_environment;
  }
 
  and singleton_block = {
    env_extension : typing_environment;
    first_fields : t array or_unknown_length;
  }

  and block_case =
    | Join of singleton_block list

  and blocks_and_tagged_immediates = {
    immediates : immediate_case Immediate.Or_unknown.Map.t;
    blocks : block_case Tag.Scannable.Map.t;
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

  and closures = {
    set_of_closures : ty_fabricated;
    closure_id : Closure_id.t;
  }

  and inlinable_function_declaration = {
    closure_origin : Closure_origin.t;
    continuation_param : Continuation.t;
    is_classic_mode : bool;
    params : (Parameter.t * t) list;
    body : expr;
    free_names_in_body : Name.Set.t;
    result : t list;
    stub : bool;
    dbg : Debuginfo.t;
    inline : inline_attribute;
    specialise : specialise_attribute;
    is_a_functor : bool;
    invariant_params : Variable.Set.t lazy_t;
    size : int option lazy_t;
    direct_call_surrogate : Closure_id.t option;
  }

  and non_inlinable_function_declaration = {
    result : t list;
    direct_call_surrogate : Closure_id.t option;
  }

  and function_declaration =
    | Non_inlinable of non_inlinable_function_declaration
    | Inlinable of inlinable_function_declaration

  and set_of_closures = {
    set_of_closures_id : Set_of_closures_id.t;
    set_of_closures_origin : Set_of_closures_origin.t;
    function_decls : function_declaration Closure_id.Map.t;
    closure_elements : ty_value Var_within_closure.Map.t;
  }

  and 'a of_kind_naked_number =
    | Immediate : Immediate.Set.t -> Immediate.Set.t of_kind_naked_number
    | Float : Numbers.Float_by_bit_pattern.Set.t
        -> Numbers.Float_by_bit_pattern.Set.t of_kind_naked_number
    | Int32 : Int32.Set.t -> Int32.Set.t of_kind_naked_number
    | Int64 : Int64.Set.t -> Int64.Set.t of_kind_naked_number
    | Nativeint : Targetint.Set.t -> Targetint.Set.t of_kind_naked_number

  and tag_case = {
    env_extension : typing_environment;
  }

  and of_kind_fabricated =
    | Tag of tag_case Tag.Map.t
    | Set_of_closures of set_of_closures

  and of_kind_phantom =
    | Value of ty_value
    | Naked_number
         : 'kind ty_naked_number * 'kind K.Naked_number.t
        -> of_kind_phantom
    | Fabricated of ty_fabricated

  and typing_environment = {
    names_to_types : t Name.Map.t;
    levels_to_names : Name.Set.t Scope_level.Map.t;
    existentials : Name.Set.t;
    existential_freshening : Freshening.t;
  }

  let print_or_alias print_descr ppf (or_alias : _ or_alias) =
    match or_alias with
    | No_alias descr -> print_descr ppf descr
    | Type_of name ->
      Format.fprintf ppf "@[(= type_of %a)@]" Name.print name
    | Type export_id ->
      Format.fprintf ppf "@[(= %a)@]" Export_id.print export_id

  let print_or_unknown_length f ppf (unk : _ or_unknown_length) =
    match unk with
    | Exactly contents -> f ppf contents
    | Unknown_length -> Format.pp_print_string ppf "<unknown length>"

  let print_or_unknown_or_bottom print_contents print_unknown_payload ppf
        (o : _ or_unknown_or_bottom) =
    match o with
    | Unknown payload ->
      Format.fprintf ppf "(Unknown %a)" print_unknown_payload payload
    | Ok contents -> print_contents ppf contents
    | Bottom -> Format.fprintf ppf "Bottom"

  let rec print_or_join print_contents ppf or_join =
    match or_join with
    | Normal contents -> print_contents ppf contents
    | Join (or_alias1, or_alias2) ->
      let print_part ppf w =
        print_or_alias (print_or_join print_contents) ppf w
      in
      Format.fprintf ppf "@[(Join@ @[(%a)@]@ @[(%a)@])@]"
        print_part or_alias1
        print_part or_alias2

  let print_ty_generic print_contents print_unknown_payload ppf ty =
    (print_or_alias
        (print_or_unknown_or_bottom
          (print_or_join print_contents)
          print_unknown_payload))
      ppf ty

  let print_of_kind_naked_number (type n) ppf (n : n of_kind_naked_number) =
    match n with
    | Immediate i ->
      Format.fprintf ppf "@[(Naked_immediates (%a))@]"
        Immediate.Set.print i
    | Float f ->
      Format.fprintf ppf "@[(Naked_floats (%a))@]"
        Float_by_bit_pattern.Set.print f
    | Int32 i ->
      Format.fprintf ppf "@[(Naked_int32s (%a))@]"
        Int32.Set.print i
    | Int64 i ->
      Format.fprintf ppf "@[(Naked_int64s (%a))@]"
        Int64.Set.print i
    | Nativeint i ->
      Format.fprintf ppf "@[(Naked_nativeints (%a))@]"
        Targetint.Set.print i

  let print_ty_naked_number (type n) ppf (ty : n ty_naked_number) =
    print_ty_generic print_of_kind_naked_number (fun _ () -> ()) ppf ty

  let print_of_kind_value_boxed_number (type n)
        ppf (n : n of_kind_value_boxed_number) =
    match n with
    | Boxed_float f ->
      Format.fprintf ppf "@[(Boxed_float (%a))@]"
        print_ty_naked_number f
    | Boxed_int32 i ->
      Format.fprintf ppf "@[(Boxed_int32 (%a))@]"
        print_ty_naked_number i
    | Boxed_int64 i ->
      Format.fprintf ppf "@[(Boxed_int64 (%a))@]"
        print_ty_naked_number i
    | Boxed_nativeint i ->
      Format.fprintf ppf "@[(Boxed_nativeint (%a))@]"
        print_ty_naked_number i

  let rec print_immediate_case ppf ({ env_extension; } : immediate_case) =
    Format.fprintf ppf "@[(env_extension %a)@]"
      print_typing_environment env_extension

  and print_first_fields ppf (fields : t array or_unknown_length) =
    (print_or_unknown_length print_array) ppf fields

  and print_singleton_block ppf { env_extension; first_fields; } =
    Format.fprintf ppf "@[((env_extension %a) (first_fields %a))@]"
      print_typing_environment env_extension
      print_first_fields first_fields

  and print_block_case ppf ((Join singleton_blocks) : block_case) =
    match singleton_blocks with
    | [] -> Format.pp_print_string ppf "<empty join>"
    | [block] -> print_singleton_block ppf block
    | blocks ->
      Format.fprintf ppf "(Join %a)"
        (Format.pp_print_list print_singleton_block) blocks

  and print_immediates ppf cases =
    Immediate.Or_unknown.Map.print print_immediate_case ppf cases

  and print_blocks ppf cases =
    Tag.Scannable.Map.print print_block_case ppf cases

  and print_of_kind_value ppf (of_kind_value : of_kind_value) =
    match of_kind_value with
    | Blocks_and_tagged_immediates { blocks; immediates; } ->
      Format.fprintf ppf
        "@[(Blocks_and_immediates@ @[(blocks %a)@]@ @[(immediates %a)@])@]"
        print_blocks blocks
        print_immediates immediates
    | Boxed_number n ->
      Format.fprintf ppf "[@(Boxed_number %a)@]"
        print_of_kind_value_boxed_number n
    | Closure closure -> print_closure ppf closure
    | String str_infos ->
      Format.fprintf ppf "@[(Strings (%a))@]" String_info.Set.print str_infos

  and print_ty_value ppf (ty : ty_value) =
    print_ty_generic print_of_kind_value K.Value_kind.print ppf ty

  and print_ty_value_array ppf ty_values =
    Format.fprintf ppf "@[[| %a |]@]"
      (Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ";@ ")
        print_ty_value)
      (Array.to_list ty_values)

  and _unused = Expr.print

  and print_closure ppf ({ closure_id; set_of_closures; } : closures) =
    Format.fprintf ppf "@[(Closures@ @[<2>[@ %a @[<2>from@ %a@];@ ]@])@]"
      Closure_id.print closure_id
      print_ty_fabricated set_of_closures

  and print_inlinable_function_declaration ppf
        (decl : inlinable_function_declaration) =
    Format.fprintf ppf
      "@[(inlinable@ \
        @[(closure_origin %a)@]@,\
        @[(continuation_param %a)@]@,\
        @[(is_classic_mode %b)@]@,\
        @[(params (%a))@]@,\
        @[(body <elided>)@]@,\
        @[(free_names_in_body %a)@]@,\
        @[(result (%a))@]@,\
        @[(stub %b)@]@,\
        @[(dbg %a)@]@,\
        @[(inline %a)@]@,\
        @[(specialise %a)@]@,\
        @[(is_a_functor %b)@]@,\
        @[(invariant_params %a)@]@,\
        @[(size %a)@]@,\
        @[(direct_call_surrogate %a)@])@]"
      Closure_origin.print decl.closure_origin
      Continuation.print decl.continuation_param
      decl.is_classic_mode
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
        (fun ppf (param, ty) ->
          Format.fprintf ppf "@[(%a@ :@ %a)@]"
            Parameter.print param
            print ty)) decl.params
      Name.Set.print decl.free_names_in_body
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
        (fun ppf ty ->
          Format.fprintf ppf "%a"
            print ty)) decl.result
      decl.stub
      Debuginfo.print_compact decl.dbg
      print_inline_attribute decl.inline
      print_specialise_attribute decl.specialise
      decl.is_a_functor
      Variable.Set.print (Lazy.force decl.invariant_params)
      (Misc.Stdlib.Option.print Format.pp_print_int) (Lazy.force decl.size)
      (Misc.Stdlib.Option.print Closure_id.print) decl.direct_call_surrogate

  and print_non_inlinable_function_declaration ppf
        (decl : non_inlinable_function_declaration) =
    Format.fprintf ppf
      "@[(non_inlinable@ \
        @[(result (%a))@]@,\
        @[(direct_call_surrogate %a)@])@]"
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
        (fun ppf ty ->
          Format.fprintf ppf "%a"
            print ty)) decl.result
      (Misc.Stdlib.Option.print Closure_id.print) decl.direct_call_surrogate

  and print_function_declaration ppf (decl : function_declaration) =
    match decl with
    | Inlinable decl -> print_inlinable_function_declaration ppf decl
    | Non_inlinable decl -> print_non_inlinable_function_declaration ppf decl

  and print_function_declarations ppf function_decls =
    Format.fprintf ppf "%a"
      (Closure_id.Map.print print_function_declaration)
      function_decls

  and print_set_of_closures ppf set =
    Format.fprintf ppf
      "@[(@[(set_of_closures_id@ %a)@]@,\
          @[(set_of_closures_origin@ %a)@]@,\
          @[(function_decls@ %a)@]@,\
          @[(closure_elements@ %a)@])@]"
      Set_of_closures_id.print set.set_of_closures_id
      Set_of_closures_origin.print set.set_of_closures_origin
      print_function_declarations set.function_decls
      (Var_within_closure.Map.print print_ty_value) set.closure_elements

  and print_tag_case ppf ({ env_extension; } : tag_case) =
    Format.fprintf ppf "@[(env_extension %a)@]"
      print_typing_environment env_extension

  and print_of_kind_fabricated ppf (o : of_kind_fabricated) =
    match o with
    | Tag tag_map ->
      Format.fprintf ppf "@[(Tags %a)@]" (Tag.Map.print print_tag_case) tag_map
    | Set_of_closures set -> print_set_of_closures ppf set

  and print_ty_fabricated ppf (ty : ty_fabricated) =
    print_ty_generic print_of_kind_fabricated K.Value_kind.print ppf ty

  and print_of_kind_phantom ppf (o : of_kind_phantom) =
    match o with
    | Value ty_value ->
      Format.fprintf ppf "[@(Phantom %a)@]"
        print_ty_value ty_value
    | Naked_number (ty_naked_number, _kind) ->
      Format.fprintf ppf "[@(Phantom %a)@]"
        print_ty_naked_number ty_naked_number
    | Fabricated ty_fabricated ->
      Format.fprintf ppf "[@(Fabricated %a)@]"
        print_ty_fabricated ty_fabricated

  and print_ty_phantom ppf (ty : ty_phantom) =
    print_ty_generic print_of_kind_phantom K.Phantom_kind.print ppf ty

  and print ppf (t : t) =
    match t with
    | Value ty ->
      Format.fprintf ppf "(Value (%a))" print_ty_value ty
    | Naked_number (ty, _kind) ->
      Format.fprintf ppf "(Naked_number (%a))" print_ty_naked_number ty
    | Fabricated ty ->
      Format.fprintf ppf "(Fabricated (%a))" print_ty_fabricated ty
    | Phantom ty ->
      Format.fprintf ppf "(Phantom (%a))" print_ty_phantom ty

  and print_array ppf (ts : t array) =
    Format.fprintf ppf "@[[| %a |]@]"
      (Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ";@ ")
        print)
      (Array.to_list ts)

  and print_typing_environment ppf { names_to_types; levels_to_names;
        existentials; existential_freshening; } =
    Format.fprintf ppf
      "@[((names_to_types %a)@ \
          (levels_to_names %a)@ \
          (existentials %a)@ \
          (existential_freshening %a))@]"
      (Name.Map.print print) names_to_types
      (Scope_level.Map.print Name.Set.print) levels_to_names
      Name.Set.print existentials
      Freshening.print existential_freshening

  let put_ok_under_or_alias (or_alias : _ or_alias)
        : _ or_unknown_or_bottom or_alias =
    match or_alias with
    | No_alias contents -> No_alias (Ok contents)
    | Type export_id -> Type export_id
    | Type_of name -> Type_of name

  let free_names_or_alias free_names_contents (or_alias : _ or_alias) acc =
    match or_alias with
    | No_alias contents -> free_names_contents contents acc
    | Type _export_id -> acc
    | Type_of name -> Name.Set.add name acc

  let rec free_names_or_join free_names_contents (or_join : _ or_join) acc =
    match or_join with
    | Normal contents -> free_names_contents contents acc
    | Join (or_alias1, or_alias2) ->
      let acc =
        free_names_or_alias (free_names_or_join free_names_contents)
          or_alias1 acc
      in
      free_names_or_alias (free_names_or_join free_names_contents)
        or_alias2 acc

  let free_names_or_unknown_or_bottom free_names_contents free_names_unk
        (o : _ or_unknown_or_bottom) acc =
    match o with
    | Unknown unk -> free_names_unk unk acc
    | Ok or_join -> free_names_or_join free_names_contents or_join acc
    | Bottom -> acc

  let free_names_ty free_names_contents ty acc =
    let free_names_unk _unk acc = acc in
    free_names_or_alias
      (free_names_or_unknown_or_bottom free_names_contents free_names_unk)
      ty
      acc

  let free_names_of_kind_naked_number (type n) (_ty : n of_kind_naked_number)
        acc =
    acc

  let rec free_names (t : t) acc =
    match t with
    | Value ty -> free_names_ty free_names_of_kind_value ty acc
    | Naked_number (ty, _kind) ->
      free_names_ty free_names_of_kind_naked_number ty acc
    | Fabricated ty -> free_names_ty free_names_of_kind_fabricated ty acc
    | Phantom ty ->
      (* CR mshinwell: We need to think more about this.  There may be a need
         for a normal name / phantom name split. *)
      free_names_ty free_names_of_kind_phantom ty acc

  and free_names_of_kind_value (of_kind : of_kind_value) acc =
    match of_kind with
    | Blocks_and_tagged_immediates { blocks; immediates; } ->
      let acc =
        Tag.Scannable.Map.fold (fun _tag ((Join singletons) : block_case) acc ->
            List.fold_left (fun acc (singleton : singleton_block) ->
                let acc =
                  free_names_of_typing_environment singleton.env_extension acc
                in
                match singleton.first_fields with
                | Unknown_length -> acc
                | Exactly first_fields ->
                  Array.fold_left (fun acc t -> free_names t acc)
                    acc first_fields)
              acc
              singletons)
          blocks
          acc
      in
      Immediate.Or_unknown.Map.fold (fun _imm (case : immediate_case) acc ->
          free_names_of_typing_environment case.env_extension acc)
        immediates
        acc
    | Boxed_number (Boxed_float n) ->
      free_names_ty free_names_of_kind_naked_number n acc
    | Boxed_number (Boxed_int32 n) ->
      free_names_ty free_names_of_kind_naked_number n acc
    | Boxed_number (Boxed_int64 n) ->
      free_names_ty free_names_of_kind_naked_number n acc
    | Boxed_number (Boxed_nativeint n) ->
      free_names_ty free_names_of_kind_naked_number n acc
    | Closure { set_of_closures; closure_id = _; } ->
      free_names_ty free_names_of_kind_fabricated set_of_closures acc
    | String _ -> acc

  and free_names_of_kind_fabricated (of_kind : of_kind_fabricated) acc =
    match of_kind with
    | Tag tag_map ->
      Tag.Map.fold (fun _tag ({ env_extension; } : tag_case) acc ->
          free_names_of_typing_environment env_extension acc)
        tag_map
        acc
    | Set_of_closures set ->
      let acc =
        Var_within_closure.Map.fold (fun _var ty_value acc ->
            free_names_ty free_names_of_kind_value ty_value acc)
          set.closure_elements acc
      in
      Closure_id.Map.fold
        (fun _closure_id (decl : function_declaration) acc ->
          match decl with
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
          | Non_inlinable decl ->
            List.fold_left (fun acc t ->
              free_names t acc)
              acc
              decl.result)
        set.function_decls
        acc

  and free_names_of_kind_phantom (of_kind : of_kind_phantom) acc =
    match of_kind with
    | Value ty_value ->
      free_names_ty free_names_of_kind_value ty_value acc
    | Naked_number (ty_naked_number, _kind) ->
      free_names_ty free_names_of_kind_naked_number ty_naked_number acc
    | Fabricated ty_fabricated ->
      free_names_ty free_names_of_kind_fabricated ty_fabricated acc

  and free_names_of_typing_environment (env : typing_environment) acc =
    let all_names =
      Name.Map.fold (fun _bound_name t all_names ->
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

  let free_names t = free_names t Name.Set.empty

  module Typing_environment = struct
    type t = typing_environment

    let print = print_typing_environment

    let create () =
      let existential_freshening = Freshening.activate Freshening.empty in
      { names_to_types = Name.Map.empty;
        levels_to_names = Scope_level.Map.empty;
        existentials = Name.Set.empty;
        existential_freshening;
      }
  end

  (* CR-someday mshinwell: Functions such as [alias] and [bottom] could be
     simplified if [K.t] were a GADT. *)

  let phantomize t : t =
    match t with
    | Value ty_value ->
      let of_kind_phantom : of_kind_phantom =
        Value ty_value
      in
      Phantom (No_alias (Ok (Normal of_kind_phantom)))
    | Naked_number (ty_naked_number, kind) ->
      let of_kind_phantom : of_kind_phantom =
        Naked_number (ty_naked_number, kind)
      in
      Phantom (No_alias (Ok (Normal of_kind_phantom)))
    | Fabricated ty_fabricated ->
      let of_kind_phantom : of_kind_phantom =
        Fabricated ty_fabricated
      in
      Phantom (No_alias (Ok (Normal of_kind_phantom)))
    | Phantom _ -> t

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

  type 'a type_accessor = type_of_name:(Name_or_export_id.t -> t option) -> 'a

  let alias_type_of (kind : K.t) name : t =
    match kind with
    | Value _ -> Value (Type_of name)
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
    | Fabricated _ -> Fabricated (Type_of name)
    | Phantom _ -> Phantom (Type_of name)

  let alias_type (kind : K.t) export_id : t =
    match kind with
    | Value _ -> Value (Type export_id)
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
    | Fabricated _ -> Fabricated (Type export_id)
    | Phantom _ -> Phantom (Type export_id)

  let bottom (kind : K.t) : t =
    match kind with
    | Value _ -> Value (No_alias Bottom)
    | Naked_number Naked_immediate ->
      Naked_number (No_alias Bottom, K.Naked_number.Naked_immediate)
    | Naked_number Naked_float ->
      Naked_number (No_alias Bottom, K.Naked_number.Naked_float)
    | Naked_number Naked_int32 ->
      Naked_number (No_alias Bottom, K.Naked_number.Naked_int32)
    | Naked_number Naked_int64 ->
      Naked_number (No_alias Bottom, K.Naked_number.Naked_int64)
    | Naked_number Naked_nativeint ->
      Naked_number (No_alias Bottom, K.Naked_number.Naked_nativeint)
    | Fabricated _ -> Fabricated (No_alias Bottom)
    | Phantom _ -> Phantom (No_alias Bottom)

  let these_naked_immediates (is : Immediate.Set.t) : t =
    let of_kind : _ of_kind_naked_number = Immediate is in
    Naked_number (No_alias (Ok (Normal of_kind)),
      K.Naked_number.Naked_immediate)

  let these_naked_floats (is : Float_by_bit_pattern.Set.t) : t =
    let of_kind : _ of_kind_naked_number = Float is in
    Naked_number (No_alias (Ok (Normal of_kind)),
      K.Naked_number.Naked_float)

  let these_naked_int32s (is : Int32.Set.t) : t =
    let of_kind : _ of_kind_naked_number = Int32 is in
    Naked_number (No_alias (Ok (Normal of_kind)),
      K.Naked_number.Naked_int32)

  let these_naked_int64s (is : Int64.Set.t) : t =
    let of_kind : _ of_kind_naked_number = Int64 is in
    Naked_number (No_alias (Ok (Normal of_kind)),
      K.Naked_number.Naked_int64)

  let these_naked_nativeints (is : Targetint.Set.t) : t =
    let of_kind : _ of_kind_naked_number = Nativeint is in
    Naked_number (No_alias (Ok (Normal of_kind)),
      K.Naked_number.Naked_nativeint)

  let this_naked_immediate i =
    these_naked_immediates (Immediate.Set.singleton i)

  let this_naked_float i =
    these_naked_floats (Float_by_bit_pattern.Set.singleton i)

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

  let box_float (t : t) : t =
    match t with
    | Naked_number (ty_naked_float, K.Naked_number.Naked_float) ->
      Value (No_alias (Ok (Normal (
        Boxed_number (Boxed_float ty_naked_float)))))
    | Value _
    | Naked_number _
    | Fabricated _
    | Phantom _ ->
      Misc.fatal_errorf "Type of wrong kind for [box_float]: %a"
        print t

  let box_int32 (t : t) : t =
    match t with
    | Naked_number (ty_naked_int32, K.Naked_number.Naked_int32) ->
      Value (No_alias (Ok (Normal (
        Boxed_number (Boxed_int32 ty_naked_int32)))))
    | Value _
    | Naked_number _
    | Fabricated _
    | Phantom _ ->
      Misc.fatal_errorf "Type of wrong kind for [box_int32]: %a"
        print t

  let box_int64 (t : t) : t =
    match t with
    | Naked_number (ty_naked_int64, K.Naked_number.Naked_int64) ->
      Value (No_alias (Ok (Normal (
        Boxed_number (Boxed_int64 ty_naked_int64)))))
    | Value _
    | Naked_number _
    | Fabricated _
    | Phantom _ ->
      Misc.fatal_errorf "Type of wrong kind for [box_int64]: %a"
        print t

  let box_nativeint (t : t) : t =
    match t with
    | Naked_number (ty_naked_nativeint, K.Naked_number.Naked_nativeint) ->
      Value (No_alias (Ok (Normal (
        Boxed_number (Boxed_nativeint ty_naked_nativeint)))))
    | Value _
    | Naked_number _
    | Fabricated _
    | Phantom _ ->
      Misc.fatal_errorf "Type of wrong kind for [box_nativeint]: %a"
        print t

  let these_tagged_immediates imms : t =
    let immediates =
      Immediate.Set.fold (fun imm map ->
          let case : immediate_case =
            { env_extension = Typing_environment.create ();
            }
          in
          let key = Immediate.Or_unknown.ok imm in
          Immediate.Or_unknown.Map.add key case map)
        imms
        Immediate.Or_unknown.Map.empty
    in
    let blocks_and_tagged_immediates : blocks_and_tagged_immediates =
      { immediates;
        blocks = Tag.Scannable.Map.empty;
      }
    in
    Value (No_alias (Ok (Normal (Blocks_and_tagged_immediates
      blocks_and_tagged_immediates))))

  let this_tagged_immediate imm =
    these_tagged_immediates (Immediate.Set.singleton imm)

  let this_boxed_float f = box_float (this_naked_float f)
  let this_boxed_int32 f = box_int32 (this_naked_int32 f)
  let this_boxed_int64 f = box_int64 (this_naked_int64 f)
  let this_boxed_nativeint f = box_nativeint (this_naked_nativeint f)

  let these_boxed_floats f = box_float (these_naked_floats f)
  let these_boxed_int32s f = box_int32 (these_naked_int32s f)
  let these_boxed_int64s f = box_int64 (these_naked_int64s f)
  let these_boxed_nativeints f = box_nativeint (these_naked_nativeints f)

  let these_tags tags_to_env_extensions : t =
    let tag_map =
      Tag.Map.map (fun env : tag_case ->
          { env_extension = env; })
        tags_to_env_extensions
    in
    Fabricated (No_alias (Ok (Normal (Tag tag_map))))

(*

  let this_tagged_immediate i : t =
    let i : ty_naked_immediate =
      let i : of_kind_naked_immediate = Naked_immediate i in
      No_alias (Resolved (Ok (No_alias i)))
    in
    Value (No_alias (Resolved (Ok (No_alias (Tagged_immediate i)))))

*)

  let this_immutable_string_as_ty_value str : ty_value =
    let str : String_info.t =
      { contents = Contents str;
        (* CR mshinwell: Possibility for exception? *)
        size = Targetint.OCaml.of_int (String.length str);
      }
    in
    let str = String_info.Set.singleton str in
    No_alias (Ok (Normal (String str)))

  let this_immutable_string str : t =
    Value (this_immutable_string_as_ty_value str)

  let immutable_string_as_ty_value ~size : ty_value =
    let str : String_info.t =
      { contents = Unknown_or_mutable;
        size;
      }
    in
    let str = String_info.Set.singleton str in
    No_alias (Ok (Normal (String str)))

  let immutable_string ~size : t =
    Value (immutable_string_as_ty_value ~size)

  let mutable_string ~size : t =
    let str : String_info.t =
      { contents = Unknown_or_mutable;
        size;
      }
    in
    let str = String_info.Set.singleton str in
    Value (No_alias (Ok (Normal (String str))))

(*

  (* CR mshinwell: We need to think about these float array functions in
     conjunction with the 4.06 feature for disabling the float array
     optimisation *)

  let this_immutable_float_array fields : t =
    let make_field f : ty_naked_float =
      let f : of_kind_naked_float = Naked_float f in
      No_alias (Resolved (Ok (No_alias f)))
    in
    let fields = Array.map make_field fields in
    Value (No_alias (Resolved (Ok (No_alias (Float_array fields)))))

  let immutable_float_array fields : t =
(*
    let fields =
      Array.map (fun (field : t) ->
          match field with
          | Naked_float ty_naked_float -> ty_naked_float
          | Value _ | Naked_immediate _ | Naked_int32 _ | Naked_int64 _
          | Naked_nativeint _ ->
            Misc.fatal_errorf "Can only form [Float_array] types with fields \
                of kind [Naked_float].  Wrong field type: %a"
              print field)
        fields
    in
*)
    Value (No_alias (Resolved (Ok (No_alias (Float_array fields)))))

  let mutable_float_array0 ~size : _ singleton_or_combination =
    let make_field () : ty_naked_float =
      No_alias (Resolved (Unknown (Other, ())))
    in
    (* CR mshinwell: dubious for cross compilation *)
    let size = Targetint.OCaml.to_int size in
    let fields = Array.init size (fun _ -> make_field ()) in
    No_alias (Float_array fields)

  let mutable_float_array ~size : t =
    let ty = mutable_float_array0 ~size in
    Value (No_alias (Resolved (Ok ty)))

  let block tag fields : t =
(*
    let fields =
      Array.map (fun (field : t) ->
          match field with
          | Value ty_value -> ty_value
          | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
          | Naked_nativeint _ ->
            Misc.fatal_errorf "Can only form [Block] types with fields of \
                kind [Value].  Wrong field type: %a"
              print field)
        fields
    in
*)
    Value (No_alias (Resolved (Ok (No_alias (Block (tag, fields))))))


*)

(*

  let any_naked_float_as_ty_naked_float () : ty_naked_float =
    No_alias (Resolved (Unknown (Other, ())))
*)

  let any_value_as_ty_value value_kind : ty_value =
    No_alias (Unknown value_kind)

  let any_value value_kind : t =
    Value (any_value_as_ty_value value_kind)

  let any_naked_immediate () : t =
    Naked_number (No_alias (Unknown ()), K.Naked_number.Naked_immediate)

  let any_naked_float () : t =
    Naked_number (No_alias (Unknown ()), K.Naked_number.Naked_float)

  let any_naked_int32 () : t =
    Naked_number (No_alias (Unknown ()), K.Naked_number.Naked_int32)

  let any_naked_int64 () : t =
    Naked_number (No_alias (Unknown ()), K.Naked_number.Naked_int64)

  let any_naked_nativeint () : t =
    Naked_number (No_alias (Unknown ()), K.Naked_number.Naked_nativeint)

  let any_fabricated () : t =
    Fabricated (No_alias (Unknown K.Value_kind.Unknown))

  let any_phantom () : t =
    Phantom (No_alias (Unknown K.Phantom_kind.Unknown))

  let unknown (kind : K.t) =
    match kind with
    | Value value_kind -> any_value value_kind
    | Naked_number Naked_immediate -> any_naked_immediate ()
    | Naked_number Naked_float -> any_naked_float ()
    | Naked_number Naked_int32 -> any_naked_int32 ()
    | Naked_number Naked_int64 -> any_naked_int64 ()
    | Naked_number Naked_nativeint -> any_naked_nativeint ()
    | Fabricated _ -> any_fabricated ()
    | Phantom _ -> any_phantom ()

(*

  let any_tagged_immediate () : t =
    let i : ty_naked_immediate = No_alias (Resolved (Unknown (Other, ()))) in
    Value (No_alias (Resolved (Ok (No_alias (Tagged_immediate i)))))

*)

  let any_boxed_float () = box_float (any_naked_float ())
  let any_boxed_int32 () = box_int32 (any_naked_int32 ())
  let any_boxed_int64 () = box_int64 (any_naked_int64 ())
  let any_boxed_nativeint () = box_nativeint (any_naked_nativeint ())


(*

  (* CR mshinwell: Check this is being used correctly *)
  let resolved_ty_value_for_predefined_exception ~name : resolved_ty_value =
    let fields =
      [| this_immutable_string_as_ty_value name;
         unknown_as_ty_value Other Must_scan;
      |]
    in
    No_alias (Ok (No_alias (Block (Tag.Scannable.object_tag, fields))))
*)

  let force_to_kind_value t =
    match t with
    | Value ty_value -> ty_value
    | Naked_number _
    | Fabricated _
    | Phantom _ ->
      Misc.fatal_errorf "Type has wrong kind (expected [Value]): %a"
        print t

  let force_to_kind_naked_number (type n) (kind : n K.Naked_number.t) (t : t)
        : n ty_naked_number =
    match t, kind with
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
    | Value _, _
    | Phantom _, _ ->
      Misc.fatal_errorf "Type has wrong kind (expected [Naked_number %a]): %a"
        K.Naked_number.print kind
        print t

  let force_to_kind_fabricated t =
    match t with
    | Fabricated ty_fabricated -> ty_fabricated
    | Value _
    | Naked_number _
    | Phantom _ ->
      Misc.fatal_errorf "Type has wrong kind (expected [Fabricated]): %a"
        print t

  let force_to_kind_phantom t =
    match t with
    | Phantom ty_phantom -> ty_phantom
    | Value _
    | Naked_number _
    | Fabricated _ ->
      Misc.fatal_errorf "Type has wrong kind (expected [Phantom]): %a"
        print t

(*
  let t_of_ty_value (ty : ty_value) : t = Value ty

  let t_of_ty_naked_float (ty : ty_naked_float) : t = Naked_float ty

*)

  let resolve_aliases_on_ty (type a)
        ~(type_of_name : Name_or_export_id.t -> t option)
        ~(force_to_kind : t -> (a, _) ty)
        (ty : (a, _) ty)
        : (a, _) ty * (Name.t option) =
    let rec resolve_aliases names_seen ~canonical_name (ty : (a, _) ty) =
      let resolve (name : Name_or_export_id.t) =
        if Name_or_export_id.Set.mem name names_seen then begin
          (* CR-soon mshinwell: Improve message -- but this means passing the
             printing functions to this function. *)
          Misc.fatal_errorf "Loop on %a whilst resolving aliases"
            Name_or_export_id.print name
        end;
        let canonical_name =
          match name with
          | Name name -> Some name
          | Export_id _ -> None
        in
        begin match type_of_name name with
        | None ->
          (* The type could not be obtained but we still wish to keep the
             name (in case for example a .cmx file subsequently becomes
             available). *)
          ty, canonical_name
        | Some t ->
          let names_seen = Name_or_export_id.Set.add name names_seen in
          let ty = force_to_kind t in
          resolve_aliases names_seen ~canonical_name ty
        end
      in
      match ty with
      | No_alias _ -> ty, canonical_name
      | Type export_id -> resolve (Name_or_export_id.Export_id export_id)
      | Type_of name -> resolve (Name_or_export_id.Name name)
    in
    resolve_aliases Name_or_export_id.Set.empty ~canonical_name:None ty

  let resolve_aliases_and_squash_unresolved_names_on_ty ~type_of_name
        ~force_to_kind ~unknown_payload ty =
    let ty, canonical_name =
      resolve_aliases_on_ty ~force_to_kind ~type_of_name ty
    in
    let ty =
      match ty with
      | No_alias ty -> ty
      | Type _ | Type_of _ -> Unknown unknown_payload
    in
    ty, canonical_name

  let resolve_aliases ~type_of_name t : t * (Name.t option) =
    match t with
    | Value ty ->
      let force_to_kind = force_to_kind_value in
      let ty, canonical_name =
        resolve_aliases_on_ty ~force_to_kind ~type_of_name ty
      in
      Value ty, canonical_name
    | Naked_number (ty, kind) ->
      let force_to_kind = force_to_kind_naked_number kind in
      let ty, canonical_name =
        resolve_aliases_on_ty ~force_to_kind ~type_of_name ty
      in
      Naked_number (ty, kind), canonical_name
    | Fabricated ty ->
      let force_to_kind = force_to_kind_fabricated in
      let ty, canonical_name =
        resolve_aliases_on_ty ~force_to_kind ~type_of_name ty
      in
      Fabricated ty, canonical_name
    | Phantom ty ->
      let force_to_kind = force_to_kind_phantom in
      let ty, canonical_name =
        resolve_aliases_on_ty ~force_to_kind ~type_of_name ty
      in
      Phantom ty, canonical_name

  let resolve_aliases_and_squash_unresolved_names ~type_of_name t
        : t * (Name.t option) =
    match t with
    | Value ty ->
      let force_to_kind = force_to_kind_value in
      let ty, canonical_name =
        resolve_aliases_and_squash_unresolved_names_on_ty ~force_to_kind
          ~type_of_name ~unknown_payload:K.Value_kind.Unknown ty
      in
      Value (No_alias ty), canonical_name
    | Naked_number (ty, kind) ->
      let force_to_kind
          = force_to_kind_naked_number kind in
      let ty, canonical_name =
        resolve_aliases_and_squash_unresolved_names_on_ty ~force_to_kind
          ~type_of_name ~unknown_payload:() ty
      in
      Naked_number (No_alias ty, kind), canonical_name
    | Fabricated ty ->
      let force_to_kind
          = force_to_kind_fabricated in
      let ty, canonical_name =
        resolve_aliases_and_squash_unresolved_names_on_ty ~force_to_kind
          ~type_of_name ~unknown_payload:K.Value_kind.Unknown ty
      in
      Fabricated (No_alias ty), canonical_name
    | Phantom ty ->
      let force_to_kind
          = force_to_kind_phantom in
      let ty, canonical_name =
        resolve_aliases_and_squash_unresolved_names_on_ty ~force_to_kind
          ~type_of_name ~unknown_payload:K.Phantom_kind.Unknown ty
      in
      Phantom (No_alias ty), canonical_name

  let value_kind_ty_value ~type_of_name ty =
    let rec value_kind_ty_value (ty : ty_value) : K.Value_kind.t =
      let (ty : _ or_unknown_or_bottom), _canonical_name =
        resolve_aliases_and_squash_unresolved_names_on_ty
          ~force_to_kind:force_to_kind_value
          ~type_of_name
          ~unknown_payload:K.Value_kind.Unknown
          ty
      in
      match ty with
      | Unknown value_kind -> value_kind
      | Ok (Normal of_kind_value) ->
        begin match of_kind_value with
        | Blocks_and_tagged_immediates { blocks; immediates = _; } ->
          if Tag.Scannable.Map.is_empty blocks then Definitely_immediate
          else Unknown
        | Boxed_number _ | Closure _ | String _ -> Definitely_pointer
        end
      | Ok (Join (ty1, ty2)) ->
        let ty1 = put_ok_under_or_alias ty1 in
        let ty2 = put_ok_under_or_alias ty2 in
        K.Value_kind.join (value_kind_ty_value ty1)
          (value_kind_ty_value ty2)
      | Bottom -> Bottom
    in
    value_kind_ty_value ty

  let kind_ty_value ~type_of_name (ty : ty_value) =
    let value_kind =
      value_kind_ty_value ~type_of_name ty
    in
    K.value value_kind

  let value_kind_ty_fabricated ~type_of_name ty =
    let rec value_kind_ty_fabricated (ty : ty_fabricated)
          : K.Value_kind.t =
      let (ty : _ or_unknown_or_bottom), _canonical_name =
        resolve_aliases_and_squash_unresolved_names_on_ty
          ~force_to_kind:force_to_kind_fabricated
          ~type_of_name
          ~unknown_payload:K.Value_kind.Unknown
          ty
      in
      match ty with
      | Unknown value_kind -> value_kind
      | Ok (Normal of_kind_fabricated) ->
        begin match of_kind_fabricated with
        | Tag _ -> K.Value_kind.Definitely_immediate
        | Set_of_closures _ -> K.Value_kind.Definitely_pointer
        end
      | Ok (Join (ty1, ty2)) ->
        let ty1 = put_ok_under_or_alias ty1 in
        let ty2 = put_ok_under_or_alias ty2 in
        K.Value_kind.join (value_kind_ty_fabricated ty1)
          (value_kind_ty_fabricated ty2)
      | Bottom -> Bottom
    in
    value_kind_ty_fabricated ty

  let kind_ty_fabricated ~type_of_name ty =
    let value_kind = value_kind_ty_fabricated ~type_of_name ty in
    K.fabricated value_kind

  let phantom_kind_ty_phantom ~type_of_name ty =
    let rec phantom_kind_ty_phantom (ty : ty_phantom)
          : K.Phantom_kind.t =
      let (ty : _ or_unknown_or_bottom), _canonical_name =
        resolve_aliases_and_squash_unresolved_names_on_ty
          ~force_to_kind:force_to_kind_phantom
          ~type_of_name
          ~unknown_payload:K.Phantom_kind.Unknown
          ty
      in
      match ty with
      | Unknown phantom_kind -> phantom_kind
      | Ok (Normal of_kind_phantom) ->
        begin match of_kind_phantom with
        | Value ty_value ->
          let value_kind =
            value_kind_ty_value ~type_of_name ty_value
          in
          Value value_kind
        | Naked_number (_, K.Naked_number.Naked_immediate) ->
          Naked_number Naked_immediate
        | Naked_number (_, K.Naked_number.Naked_float) ->
          Naked_number Naked_float
        | Naked_number (_, K.Naked_number.Naked_int32) ->
          Naked_number Naked_int32
        | Naked_number (_, K.Naked_number.Naked_int64) ->
          Naked_number Naked_int64
        | Naked_number (_, K.Naked_number.Naked_nativeint) ->
          Naked_number Naked_nativeint
        | Fabricated ty_fabricated ->
          let value_kind =
            value_kind_ty_fabricated ~type_of_name ty_fabricated
          in
          Fabricated value_kind
        end
      | Ok (Join (ty1, ty2)) ->
        let ty1 = put_ok_under_or_alias ty1 in
        let ty2 = put_ok_under_or_alias ty2 in
        K.Phantom_kind.join (phantom_kind_ty_phantom ty1)
          (phantom_kind_ty_phantom ty2)
      | Bottom -> Bottom
    in
    phantom_kind_ty_phantom ty

  let kind_ty_phantom ~type_of_name ty =
    let phantom_kind = phantom_kind_ty_phantom ~type_of_name ty in
    K.phantom phantom_kind

  let kind ~type_of_name (t : t) =
    match t with
    | Value ty -> kind_ty_value ~type_of_name ty
    | Naked_number (_, K.Naked_number.Naked_immediate) ->
      K.naked_immediate ()
    | Naked_number (_, K.Naked_number.Naked_float) ->
      K.naked_float ()
    | Naked_number (_, K.Naked_number.Naked_int32) ->
      K.naked_int32 ()
    | Naked_number (_, K.Naked_number.Naked_int64) ->
      K.naked_int64 ()
    | Naked_number (_, K.Naked_number.Naked_nativeint) ->
      K.naked_nativeint ()
    | Fabricated ty -> kind_ty_fabricated ~type_of_name ty
    | Phantom ty -> kind_ty_phantom ~type_of_name ty

  let create_inlinable_function_declaration ~is_classic_mode ~closure_origin
        ~continuation_param ~params ~body ~result ~stub ~dbg ~inline
        ~specialise ~is_a_functor ~invariant_params ~size ~direct_call_surrogate
        : inlinable_function_declaration =
    { closure_origin;
      continuation_param;
      is_classic_mode;
      params;
      body;
      free_names_in_body = Expr.free_names body;
      result;
      stub;
      dbg;
      inline;
      specialise;
      is_a_functor;
      invariant_params;
      size;
      direct_call_surrogate;
    }

  let create_non_inlinable_function_declaration ~result ~direct_call_surrogate
        : non_inlinable_function_declaration =
    { result;
      direct_call_surrogate;
    }

  let closure ~set_of_closures closure_id : t =
    (* CR mshinwell: pass a description to the "force" functions *)
    let set_of_closures = force_to_kind_fabricated set_of_closures in
    Value (No_alias (Ok (Normal (Closure { set_of_closures; closure_id; }))))

  let create_set_of_closures ~set_of_closures_id ~set_of_closures_origin
        ~function_decls ~closure_elements : set_of_closures =
    { set_of_closures_id;
      set_of_closures_origin;
      function_decls;
      closure_elements;
    }

  let set_of_closures ~set_of_closures_id ~set_of_closures_origin
        ~function_decls ~closure_elements =
    let set_of_closures =
      create_set_of_closures ~set_of_closures_id ~set_of_closures_origin
        ~function_decls ~closure_elements
    in
    Fabricated (No_alias (Ok (Normal (Set_of_closures set_of_closures))))


(*
  (* CR mshinwell: We need tests to check that [clean] matches up with
     [free_variables]. *)
(*
  type cleaning_spec =
    | Available
    | Available_different_name of Variable.t
    | Unavailable

  let rec clean t classify =
    let clean_var var =
      match classify var with
      | Available -> Some var
      | Available_different_name new_var -> Some new_var
      | Unavailable -> None
    in
    let clean_var_opt var_opt =
      match var_opt with
      | None -> None
      | Some var ->
        match clean_var var with
        | None -> None
        | (Some var') as var_opt' ->
          if var == var' then var_opt
          else var_opt'
    in
    clean_t t clean_var_opt

  and clean_t (t : t) clean_var_opt : t =
    match t with
    | Value ty ->
      Value (clean_ty_value ty clean_var_opt)
    | Naked_immediate ty ->
      Naked_immediate (clean_ty_naked_immediate ty clean_var_opt)
    | Naked_float ty ->
      Naked_float (clean_ty_naked_float ty clean_var_opt)
    | Naked_int32 ty ->
      Naked_int32 (clean_ty_naked_int32 ty clean_var_opt)
    | Naked_int64 ty ->
      Naked_int64 (clean_ty_naked_int64 ty clean_var_opt)
    | Naked_nativeint ty ->
      Naked_nativeint (clean_ty_naked_nativeint ty clean_var_opt)

  and clean_ty_value ty_value clean_var_opt : ty_value =
    let module I = (val importer : Importer) in
    let ty_value = I.import_value_type_as_resolved_ty_value ty_value in
    let var = clean_var_opt ty_value.var in
    let descr : (of_kind_value, _) or_unknown_or_bottom =
      match ty_value.descr with
      | (Unknown _) | Bottom -> ty_value.descr
      | Ok of_kind_value ->
        Ok (clean_of_kind_value of_kind_value clean_var_opt)
    in
    { var;
      symbol = ty_value.symbol;
      descr = Ok descr;
    }

  and clean_resolved_ty_set_of_closures
        (resolved_ty_set_of_closures : resolved_ty_set_of_closures)
        clean_var_opt
        : resolved_ty_set_of_closures =
    let var = clean_var_opt resolved_ty_set_of_closures.var in
    let descr : (set_of_closures, _) or_unknown_or_bottom =
      match resolved_ty_set_of_closures.descr with
      | (Unknown _) | Bottom -> resolved_ty_set_of_closures.descr
      | Ok set_of_closures ->
        Ok (clean_set_of_closures set_of_closures clean_var_opt)
    in
    { var;
      symbol = resolved_ty_set_of_closures.symbol;
      descr = descr;
    }

  and clean_ty_naked_immediate ty_naked_immediate clean_var_opt
        : ty_naked_immediate =
    let module I = (val importer : Importer) in
    let ty_naked_immediate =
      I.import_naked_immediate_type_as_resolved_ty_naked_immediate
        ty_naked_immediate
    in
    let var = clean_var_opt ty_naked_immediate.var in
    { var;
      symbol = ty_naked_immediate.symbol;
      descr = Ok ty_naked_immediate.descr;
    }

  and clean_ty_naked_float ty_naked_float clean_var_opt
        : ty_naked_float =
    let module I = (val importer : Importer) in
    let ty_naked_float =
      I.import_naked_float_type_as_resolved_ty_naked_float ty_naked_float
    in
    let var = clean_var_opt ty_naked_float.var in
    { var;
      symbol = ty_naked_float.symbol;
      descr = Ok ty_naked_float.descr;
    }

  and clean_ty_naked_int32 ty_naked_int32 clean_var_opt
        : ty_naked_int32 =
    let module I = (val importer : Importer) in
    let ty_naked_int32 =
      I.import_naked_int32_type_as_resolved_ty_naked_int32 ty_naked_int32
    in
    let var = clean_var_opt ty_naked_int32.var in
    { var;
      symbol = ty_naked_int32.symbol;
      descr = Ok ty_naked_int32.descr;
    }

  and clean_ty_naked_int64 ty_naked_int64 clean_var_opt
        : ty_naked_int64 =
    let module I = (val importer : Importer) in
    let ty_naked_int64 =
      I.import_naked_int64_type_as_resolved_ty_naked_int64 ty_naked_int64
    in
    let var = clean_var_opt ty_naked_int64.var in
    { var;
      symbol = ty_naked_int64.symbol;
      descr = Ok ty_naked_int64.descr;
    }

  and clean_ty_naked_nativeint ty_naked_nativeint clean_var_opt
        : ty_naked_nativeint =
    let module I = (val importer : Importer) in
    let ty_naked_nativeint =
      I.import_naked_nativeint_type_as_resolved_ty_naked_nativeint
        ty_naked_nativeint
    in
    let var = clean_var_opt ty_naked_nativeint.var in
    { var;
      symbol = ty_naked_nativeint.symbol;
      descr = Ok ty_naked_nativeint.descr;
    }

  and clean_set_of_closures set_of_closures clean_var_opt =
    let closure_elements =
      Var_within_closure.Map.map (fun t ->
          clean_ty_value t clean_var_opt)
        set_of_closures.closure_elements
    in
    let function_decls =
      Closure_id.Map.map
        (fun (decl : function_declaration) : function_declaration ->
          match decl with
          | Inlinable decl ->
            let params =
              List.map (fun (param, t) ->
                  param, clean_t t clean_var_opt)
                decl.params
            in
            let result =
              List.map (fun ty ->
                clean_t ty clean_var_opt)
                decl.result
            in
            Inlinable { decl with params; result; }
          | Non_inlinable decl ->
            let result =
              List.map (fun ty ->
                clean_t ty clean_var_opt)
                decl.result
            in
            Non_inlinable { decl with result; })
        set_of_closures.function_decls
    in
    { set_of_closures with
      function_decls;
      closure_elements;
    }

  and clean_of_kind_value (o : of_kind_value) clean_var_opt
        : of_kind_value =
    match o with
    | No_alias singleton ->
      let singleton : of_kind_value_singleton =
        match singleton with
        | Tagged_immediate i ->
          Tagged_immediate (clean_ty_naked_immediate i clean_var_opt)
        | Boxed_float f ->
          Boxed_float (clean_ty_naked_float f clean_var_opt)
        | Boxed_int32 n ->
          Boxed_int32 (clean_ty_naked_int32 n clean_var_opt)
        | Boxed_int64 n ->
          Boxed_int64 (clean_ty_naked_int64 n clean_var_opt)
        | Boxed_nativeint n ->
          Boxed_nativeint (clean_ty_naked_nativeint n clean_var_opt)
        | Block (tag, fields) ->
          let fields =
            Array.map (fun t -> clean_ty_value t clean_var_opt)
              fields
          in
          Block (tag, fields)
        | Set_of_closures set_of_closures ->
          Set_of_closures
            (clean_set_of_closures set_of_closures clean_var_opt)
        | Closure { set_of_closures; closure_id; } ->
          let set_of_closures =
            clean_resolved_ty_set_of_closures set_of_closures
              clean_var_opt
          in
          Closure { set_of_closures; closure_id; }
        | String _ -> singleton
        | Float_array fields ->
          let fields =
            Array.map (fun field ->
                clean_ty_naked_float field clean_var_opt)
              fields
          in
          Float_array fields
      in
      No_alias singleton
    | Join (w1, w2) ->
      let w1 =
        { var = clean_var_opt w1.var;
          symbol = w1.symbol;
          descr = clean_of_kind_value w1.descr clean_var_opt;
        }
      in
      let w2 =
        { var = clean_var_opt w2.var;
          symbol = w2.symbol;
          descr = clean_of_kind_value w2.descr clean_var_opt;
        }
      in
      Join (w1, w2)
*)

  module Join_or_meet (P : sig
    val description : string
    val combining_op : combining_op
  end) = struct
    let combine_unknown_payload_for_value ~type_of_name
          _ty_value1 value_kind1 ty_value2 value_kind2_opt =
      let value_kind2 : K.Value_kind.t =
        match value_kind2_opt with
        | Some value_kind2 -> value_kind2
        | None ->
          value_kind_ty_value ~type_of_name
            (No_alias ((Resolved ty_value2) : _ maybe_unresolved))
      in
      match P.combining_op with
      | Join -> K.Value_kind.join value_kind1 value_kind2
      | Meet ->
        (* CR mshinwell: Same comment as above re. Definitely_immediate *)
        begin match K.Value_kind.meet value_kind1 value_kind2 with
        | Ok value_kind -> value_kind
        | Bottom -> K.Value_kind.Definitely_immediate
        end

    let combine_unknown_payload_for_non_value _ty1 () _ty2 (_ : unit option) =
      ()

    type 'a or_combine =
      | Exactly of 'a
      | Combine

    let combine_singleton_or_combination ty1 ty2
          ~combine_of_kind : _ or_unknown_or_bottom =
      let combine () : _ or_unknown_or_bottom =
        Ok (Combination (P.combining_op, No_alias ty1, No_alias ty2))
      in
      match ty1, ty2 with
      | No_alias s1, No_alias s2 ->
        begin match combine_of_kind s1 s2 with
        | Exactly result -> result
        | Combine -> combine ()
        end
      | No_alias _, Combination _
      | Combination _, No_alias _
      | Combination _, Combination _ -> combine ()

    let combine_ty (type a) (type u):__this_kind
          ~(force_to_kind : t -> (a, u) ty)
          ~(type_of_name : Name.t -> t option)
          unknown_payload_top
          combine_contents combine_unknown_payload
          (ty1 : (a, u) ty) (ty2 : (a, u) ty) : (a, u) ty =
      (* CR mshinwell: Should something be happening here with the canonical
         names? *)
      let ty1, _canonical_name1 =
        resolve_aliases_on_ty_this_kind ~force_to_kind
          ~type_of_name ty1
      in
      let ty2, _canonical_name2 =
        resolve_aliases_on_ty_this_kind ~force_to_kind
          ~type_of_name ty2
      in
      match ty1, ty2 with
      | Alias name1, Alias name2 when Name.equal name1 name2 -> Alias name1
      | _, _ ->
        let unresolved_var_or_symbol_to_unknown (ty : _ resolved_ty)
              : _ or_unknown_or_bottom =
          match ty with
          | No_alias ty -> ty
          | Alias _ -> Unknown (Other, unknown_payload_top)
        in
        let ty1 = unresolved_var_or_symbol_to_unknown ty1 in
        let ty2 = unresolved_var_or_symbol_to_unknown ty2 in
        let ty =
          (* Care: we need to handle the payloads of [Unknown]. *)
          match ty1, ty2 with
          | Unknown (reason1, payload1), Unknown (reason2, payload2) ->
            Unknown (combine_unknown_because_of reason1 reason2,
              combine_unknown_payload ty1 payload1
                ty2 (Some payload2))
          | Ok ty1, Ok ty2 ->
            combine_singleton_or_combination
              ~combine_of_kind:combine_contents
              ty1 ty2
          | Unknown (reason, payload), _ ->
            begin match P.combining_op with
            | Join ->
              Unknown (reason, combine_unknown_payload ty1 payload ty2 None)
            | Meet -> ty2
            end
          | _, Unknown (reason, payload) ->
            begin match P.combining_op with
            | Join ->
              Unknown (reason, combine_unknown_payload ty2 payload ty1 None)
            | Meet -> ty1
            end
          | Bottom, _ ->
            begin match P.combining_op with
            | Join -> ty2
            | Meet -> Bottom
            end
          | _, Bottom ->
            begin match P.combining_op with
            | Join -> ty1
            | Meet -> Bottom
            end
        in
        No_alias ((Resolved ty) : _ maybe_unresolved)

    let rec combine_of_kind_value ~type_of_name
          (t1 : of_kind_value) t2
          : (of_kind_value, K.Value_kind.t) or_unknown_or_bottom or_combine =
      let singleton s : _ or_combine =
        Exactly ((Ok (No_alias s)) : _ or_unknown_or_bottom)
      in
      match t1, t2 with
      | Tagged_immediate ty1, Tagged_immediate ty2 ->
        singleton (Tagged_immediate (
          combine_ty_naked_immediate ~type_of_name
            ty1 ty2))
      | Boxed_float ty1, Boxed_float ty2 ->
        singleton (Boxed_float (
          combine_ty_naked_float ~type_of_name
            ty1 ty2))
      | Boxed_int32 ty1, Boxed_int32 ty2 ->
        singleton (Boxed_int32 (
          combine_ty_naked_int32 ~type_of_name
            ty1 ty2))
      | Boxed_int64 ty1, Boxed_int64 ty2 ->
        singleton (Boxed_int64 (
          combine_ty_naked_int64 ~type_of_name
            ty1 ty2))
      | Boxed_nativeint ty1, Boxed_nativeint ty2 ->
        singleton (Boxed_nativeint (
          combine_ty_naked_nativeint ~type_of_name
            ty1 ty2))
      | Block (tag1, fields1), Block (tag2, fields2)
          when Tag.Scannable.equal tag1 tag2
            && Array.length fields1 = Array.length fields2 ->
        let fields =
          Array.map2 (fun ty1 ty2 ->
              combine_ty_value ~type_of_name
                ty1 ty2)
            fields1 fields2
        in
        singleton (Block (tag1, fields))
      | String { contents = Contents str1; _ },
          String { contents = Contents str2; _ }
          when String.equal str1 str2 ->
        singleton t1
      | Float_array fields1, Float_array fields2
          when Array.length fields1 = Array.length fields2 ->
        let fields =
          Array.map2 (fun ty1 ty2 ->
              combine_ty_naked_float ~type_of_name
                ty1 ty2)
            fields1 fields2
        in
        singleton (Float_array fields)
      | _, _ -> Combine

    and combine_of_kind_naked_immediate
          (t1 : of_kind_naked_immediate)
          (t2 : of_kind_naked_immediate)
          : (of_kind_naked_immediate, _) or_unknown_or_bottom or_combine =
      match t1, t2 with
      | Naked_immediate i1, Naked_immediate i2 ->
        if not (Immediate.equal i1 i2) then
          Combine
        else
          Exactly (Ok (
            No_alias ((Naked_immediate i1) : of_kind_naked_immediate)))

    and combine_of_kind_naked_float
          (t1 : of_kind_naked_float) (t2 : of_kind_naked_float)
          : (of_kind_naked_float, _) or_unknown_or_bottom or_combine =
      match t1, t2 with
      | Naked_float i1, Naked_float i2 ->
        if not (Numbers.Float_by_bit_pattern.equal i1 i2) then
          Combine
        else
          Exactly (Ok (No_alias ((Naked_float i1) : of_kind_naked_float)))

    and combine_of_kind_naked_int32
          (t1 : of_kind_naked_int32) (t2 : of_kind_naked_int32)
          : (of_kind_naked_int32, _) or_unknown_or_bottom or_combine =
      match t1, t2 with
      | Naked_int32 i1, Naked_int32 i2 ->
        if not (Int32.equal i1 i2) then
          Combine
        else
          Exactly (Ok (No_alias ((Naked_int32 i1) : of_kind_naked_int32)))

    and combine_of_kind_naked_int64
          (t1 : of_kind_naked_int64) (t2 : of_kind_naked_int64)
          : (of_kind_naked_int64, _) or_unknown_or_bottom or_combine =
      match t1, t2 with
      | Naked_int64 i1, Naked_int64 i2 ->
        if not (Int64.equal i1 i2) then
          Combine
        else
          Exactly (Ok (No_alias ((Naked_int64 i1) : of_kind_naked_int64)))

    and combine_of_kind_naked_nativeint
          (t1 : of_kind_naked_nativeint) (t2 : of_kind_naked_nativeint)
          : (of_kind_naked_nativeint, _) or_unknown_or_bottom or_combine =
      match t1, t2 with
      | Naked_nativeint i1, Naked_nativeint i2 ->
        if not (Targetint.equal i1 i2) then
          Combine
        else
          Exactly (Ok (
            No_alias ((Naked_nativeint i1) : of_kind_naked_nativeint)))

    and combine_ty_value ~type_of_name
          (ty1 : ty_value) (ty2 : ty_value) : ty_value =
      let module I = (val importer : Importer) in
      combine_ty ~type_of_name
       _this_kind:I.import_value_type_as_resolved_ty_value
        ~force_to_kind:force_to_kind_value
        K.Value_kind.Unknown
        (combine_of_kind_value ~type_of_name)
        (combine_unknown_payload_for_value ~type_of_name)
        ty1 ty2

    and combine_ty_naked_immediate ~type_of_name ty1 ty2 =
      let module I = (val importer : Importer) in
      combine_ty ~type_of_name 
       _this_kind:
          I.import_naked_immediate_type_as_resolved_ty_naked_immediate
        ~force_to_kind:force_to_kind_naked_immediate
        ()
        combine_of_kind_naked_immediate
        combine_unknown_payload_for_non_value
        ty1 ty2

    and combine_ty_naked_float ~type_of_name ty1 ty2 =
      let module I = (val importer : Importer) in
      combine_ty ~type_of_name 
       _this_kind:I.import_naked_float_type_as_resolved_ty_naked_float
        ~force_to_kind:force_to_kind_naked_float
        ()
        combine_of_kind_naked_float
        combine_unknown_payload_for_non_value
        ty1 ty2

    and combine_ty_naked_int32 ~type_of_name ty1 ty2 =
      let module I = (val importer : Importer) in
      combine_ty ~type_of_name 
       _this_kind:I.import_naked_int32_type_as_resolved_ty_naked_int32
        ~force_to_kind:force_to_kind_naked_int32
        ()
        combine_of_kind_naked_int32
        combine_unknown_payload_for_non_value
        ty1 ty2

    and combine_ty_naked_int64 ~type_of_name ty1 ty2 =
      let module I = (val importer : Importer) in
      combine_ty ~type_of_name 
       _this_kind:I.import_naked_int64_type_as_resolved_ty_naked_int64
        ~force_to_kind:force_to_kind_naked_int64
        ()
        combine_of_kind_naked_int64
        combine_unknown_payload_for_non_value
        ty1 ty2

    and combine_ty_naked_nativeint ~type_of_name ty1 ty2 =
      let module I = (val importer : Importer) in
      combine_ty ~type_of_name 
       _this_kind:
          I.import_naked_nativeint_type_as_resolved_ty_naked_nativeint
        ~force_to_kind:force_to_kind_naked_nativeint
        ()
        combine_of_kind_naked_nativeint
        combine_unknown_payload_for_non_value
        ty1 ty2

    let combine ~type_of_name (t1 : t) (t2 : t) : t =
      if t1 == t2 then t1
      else
        match t1, t2 with
        | Value ty1, Value ty2 ->
          Value (combine_ty_value
            ~type_of_name ty1 ty2)
        | Naked_immediate ty1, Naked_immediate ty2 ->
          Naked_immediate (combine_ty_naked_immediate
            ~type_of_name ty1 ty2)
        | Naked_float ty1, Naked_float ty2 ->
          Naked_float (combine_ty_naked_float
            ~type_of_name ty1 ty2)
        | Naked_int32 ty1, Naked_int32 ty2 ->
          Naked_int32 (combine_ty_naked_int32
            ~type_of_name ty1 ty2)
        | Naked_int64 ty1, Naked_int64 ty2 ->
          Naked_int64 (combine_ty_naked_int64
            ~type_of_name ty1 ty2)
        | Naked_nativeint ty1, Naked_nativeint ty2 ->
          Naked_nativeint (combine_ty_naked_nativeint
            ~type_of_name ty1 ty2)
        | _, _ ->
          Misc.fatal_errorf "Cannot take the %s of two types with different \
              kinds: %a and %a"
            P.description
            print t1
            print t2
  end

  module Join = Join_or_meet (struct
    let description = "join"
    let combining_op = Join
  end)

  module Meet = Join_or_meet (struct
    let description = "meet"
    let combining_op = Meet
  end)

  let join = Join.combine
  let join_ty_value = Join.combine_ty_value
  let join_ty_naked_float = Join.combine_ty_naked_float
  let join_ty_naked_int32 = Join.combine_ty_naked_int32
  let join_ty_naked_int64 = Join.combine_ty_naked_int64
  let join_ty_naked_nativeint = Join.combine_ty_naked_nativeint

  let join_list ~type_of_name kind ts =
    match ts with
    | [] -> bottom kind
    | t::ts ->
      List.fold_left (fun result t -> join ~type_of_name result t)
        t
        ts

  let meet = Meet.combine
  let meet_ty_value = Meet.combine_ty_value
  let meet_ty_naked_float = Meet.combine_ty_naked_float
  let meet_ty_naked_int32 = Meet.combine_ty_naked_int32
  let meet_ty_naked_int64 = Meet.combine_ty_naked_int64
  let meet_ty_naked_nativeint = Meet.combine_ty_naked_nativeint

  let meet_list ~type_of_name kind ts =
    match ts with
    | [] -> bottom kind
    | t::ts ->
      List.fold_left (fun result t -> meet ~type_of_name result t)
        t
        ts

  type 'a or_bottom =
    | Ok of 'a
    | Bottom

  let generic_meet_list ~meet ~type_of_name ts t =
    Misc.Stdlib.List.filter_map (fun t' ->
        match meet ~type_of_name t t' with
        | Ok meet -> Some meet
        | Bottom -> None)
      ts

  let generic_meet_lists ~meet ~type_of_name ts1 ts2 =
    List.fold_left (fun result t1 ->
        generic_meet_list ~type_of_name ~meet result t1)
      ts2
      ts1

  module Closure = struct
    type t = closure

    let meet ~type_of_name (t1 : t) (t2 : t) : t or_bottom =
      if not (Closure_id.equal t1.closure_id t2.closure_id) then
        Bottom
      else
        let set_of_closures =
          meet_ty_value ~type_of_name
            t1.set_of_closures t2.set_of_closures
        in
        Ok {
          set_of_closures;
          closure_id = t1.closure_id;
        }

    let meet_lists = generic_meet_lists ~meet

    let print = print_closure
  end

  module Set_of_closures = struct
    type t = set_of_closures

    let meet ~type_of_name (t1 : t) (t2 : t) : t or_bottom =
      let same_set =
        Set_of_closures_id.equal t1.set_of_closures_id t2.set_of_closures_id
          && Set_of_closures_origin.equal t1.set_of_closures_origin
            t2.set_of_closures_origin
      in
      if not same_set then Bottom
      else
        let closure_elements =
          Var_within_closure.Map.inter_merge (fun elt1 elt2 ->
              join_ty_value ~type_of_name elt1 elt2)
            t1.closure_elements
            t2.closure_elements
        in
        Ok {
          set_of_closures_id = t1.set_of_closures_id;
          set_of_closures_origin = t1.set_of_closures_origin;
          function_decls = t1.function_decls;
          closure_elements;
        }

    let meet_lists = generic_meet_lists ~meet

    let print = print_set_of_closures
  end

  let mutable_float_arrays_of_various_sizes ~sizes : t =
    let tys =
      List.map (fun size -> mutable_float_array ~size)
        (Targetint.OCaml.Set.elements sizes)
    in
    (with_null_importer join_list) (K.value Definitely_pointer) tys

  let combination_component_to_ty (type a)
        (ty : a singleton_or_combination or_alias)
        : (a, _) ty =
    match ty with
    | Alias alias -> Alias alias
    | No_alias s_or_c -> No_alias (Resolved (Ok s_or_c))

    let print ppf { names_to_types; levels_to_names;
          existentials; existential_freshening; } =
      Format.fprintf ppf
        "@[((names_to_types %a)@ \
            (levels_to_names %a)@ \
            (existentials %a)@ \
            (existential_freshening %a))@]"
        Name.Map.print print names_to_types
        Scope_level.Map.print Name.Set.print levels_to_names
        Name.Set.print existentials
        Freshening.print existential_freshening

    let create () =
      let existential_freshening = Freshening.activate Freshening.empty in
      { names_to_types = Name.Map.empty;
        levels_to_names : Scope_level.Map.empty;
        existentials : Name.Set.empty;
        existential_freshening;
      }

    let add t name scope_level ty =
      match Name.Map.find name t.names_to_types with
      | exception Not_found ->
        let names = Name.Map.add name ty t.names_to_types in
        let levels_to_names =
          Scope_level.Map.update scope_level
            (function
               | None -> Name.Set.singleton name
               | Some names -> Name.Set.add name names)
        in
        { t with
          names;
          levels_to_names;
        }
      | _ty ->
        Misc.fatal_errorf "Cannot rebind %a in environment: %a"
          Name.print name
          print t

    type binding_type = No_alias | Existential

    let find t name =
      match Name.Map.find name t.names_to_types with
      | exception Not_found ->
        Misc.fatal_errorf "Cannot find %a in environment: %a"
          Name.print name
          print t
      | ty ->
        let binding_type =
          if Name.Map.mem name t.existentials then Existential
          else No_alias
        in
        match binding_type with
        | No_alias -> ty, No_alias
        | Existential ->
          let ty = rename_variables t freshening in
          ty, Existential

    let cut t ~minimum_scope_level_to_be_existential =
      let existentials =
        Scope_level.Map.fold (fun scope_level names resulting_existentials ->
            let will_be_existential =
              Scope_level.(>=) scope_level minimum_scope_level_to_be_existential
            in
            if will_be_existential then
              Name.Set.union names resulting_existentials
            else
              resulting_existentials)
          t.levels_to_names
          Name.Set.empty
      in
      let existential_freshening =
        Name.Set.fold (fun (name : Name.t) freshening ->
            match name with
            | Symbol _ -> freshening
            | Var var ->
              let new_var = Variable.rename var in
              Freshening.add_variable freshening var new_var)
          t.existential_freshening
      in
      (* XXX we actually need to rename in the domain of [names_to_types] *)
      { names_to_types = t.names_to_types;
        levels_to_names = t.levels_to_names;
        existentials;
        existential_freshening;
      }

    let join ~type_of_name t1 t2 =
      let names_to_types =
        Name.Map.inter (fun ty1 ty2 ->
            join ~type_of_name t1 t2)
          t1.names_to_types
          t2.names_to_types
      in
      let all_levels_to_names =
        Scope_level.Map.union
          (fun names1 names2 -> Name.Set.union names1 names2)
          t1.levels_to_names
          t2.levels_to_names
      in
      let levels_to_names =
        Scope_level.Map.filter (fun _scope_level name ->
            Name.Map.mem name names_to_types)
          all_levels_to_names
      in
      let existentials =
        (* XXX care if a name is non-existential in one and existential
           in the other *)
        Name.Set.inter t1.existentials t2.existentials
      in
      let existential_freshening =
        ...
      in
      { names_to_types;
        types_to_levels;
        existentials;
        existential_freshening;
      }

    let meet ~type_of_name t1 t2 =
      let names_to_types =
        Name.Map.union (fun ty1 ty2 ->
            meet ~type_of_name t1 t2)
          t1.names_to_types
          t2.names_to_types
      in
      let all_levels_to_names =
        Scope_level.Map.union
          (fun names1 names2 -> Name.Set.union names1 names2)
          t1.levels_to_names
          t2.levels_to_names
      in
      let levels_to_names =
        Scope_level.Map.filter (fun _scope_level name ->
            Name.Map.mem name names_to_types)
          all_levels_to_names
      in
      let existentials =
        Name.Set.union t1.existentials t2.existentials
      in
      let existential_freshening =
        ...
      in
      { names_to_types;
        types_to_levels;
        existentials;
        existential_freshening;
      }

  end



*)

  let join_or_alias (type contents) ~join_contents
        (or_alias1 : contents or_alias) (or_alias2 : contents or_alias) =


  let rec join_on_or_join (type ty) (type unk) ~type_of_name
        (oj1 : ty or_join) (oj2 : ty or_join)
        ~(join_ty : (ty -> ty -> (ty, unk) or_unknown_or_bottom) type_accessor)
        ~(join_unk : unk -> unk -> unk)
        ~(ty_of_t : t -> ty)
        : (ty, unk) or_unknown_or_bottom =
    match oj1, oj2 with
    | Normal s1, Normal s2 ->
      join_ty ~type_of_name s1 s2
    | ((Normal _ | Join _) as other_side), Join (or_alias1, or_alias2)
    | Join (or_alias1, or_alias2), ((Normal _ | Join _) as other_side) ->
      let join_left = put_ok_under_or_alias or_alias1 in
      let join_right = put_ok_under_or_alias or_alias2 in
      let other_side_join_join_left =
        join_on_or_unknown_or_bottom ~type_of_name
          (Ok other_side) join_left
          ~join_ty ~join_unk ~ty_of_t
      in
      let other_side_join_join_right =
        join_on_or_unknown_or_bottom ~type_of_name
          (Ok other_side) join_right
          ~join_ty ~join_unk ~ty_of_t
      in
      join_or_unknown_or_bottom ~type_of_name
        other_side_join_join_left other_side_join_join_right

  and join_on_or_unknown_or_bottom (type ty) (type unk) ~type_of_name
        (ou1 : (ty, unk) or_unknown_or_bottom)
        (ou2 : (ty, unk) or_unknown_or_bottom)
        ~(join_ty : (ty -> ty -> (ty, unk) or_unknown_or_bottom) type_accessor)
        ~(join_unk : unk -> unk -> unk)
        ~(ty_of_t : t -> ty)
        : (ty, unk) or_unknown_or_bottom =
    match ou1, ou2 with
    | Unknown unk_left, Unknown unk_right ->
      Unknown (join_unk unk_left unk_right)
    | Unknown unk, _ | _, Unknown unk -> Unknown unk
    | Bottom, _ -> ou2
    | _, Bottom -> ou1
    | Ok or_join1, Ok or_join2 ->
      join_on_or_join ~type_of_name or_join1 or_join2
        ~join_ty ~join_unk ~ty_of_t

  let resolve_aliases_on_ty (type a)
        ~(type_of_name : Name_or_export_id.t -> t option)
        ~(force_to_kind : t -> (a, _) ty)
        (ty : (a, _) ty)
        : (a, _) ty * (Name.t option) =
  let resolve_aliases_and_squash_unresolved_names_on_ty ~type_of_name
        ~force_to_kind ~unknown_payload ty =

  module type Meet_or_join_spec = sig
    type of_kind_foo
    type unk

    val force_to_kind : t -> (of_kind_foo, unk) ty

    val unknown_payload : unk

    val meet_of_kind_foo
       : (of_kind_foo
      -> of_kind_foo
      -> (of_kind_foo, unk) or_unknown_or_bottom) type_accessor

    val meet_unk : unk -> unk -> unk

    val join_of_kind_foo
       : (of_kind_foo
      -> of_kind_foo
      -> of_kind_foo or_join) type_accessor

    val join_unk : unk -> unk -> unk
  end

  module Meet_or_join = sig
    type of_kind_foo
    type unk

    val meet_ty
       : ((of_kind_foo, unk) ty
      -> (of_kind_foo, unk) ty
      -> (of_kind_foo, unk) ty) type_accessor
  end

  module Make_meet_and_join (S : Meet_or_join_spec) : sig
    include Meet_or_join
      with type of_kind_foo = S.of_kind_foo
      with type unk = S.unk
  end = struct
    let meet_ty (type a) (type unk) ~type_of_name
          (or_alias1 : (a, unk) ty) (or_alias2 : (a, unk) ty)
          : (a, unk) ty =
      let or_unknown_or_bottom1, canonical_name1 =
        resolve_aliases_and_squash_unresolved_names_on_ty ~type_of_name
          ~force_to_kind
          ~unknown_payload
          or_alias1
      in
      let or_unknown_or_bottom2, canonical_name2 =
        resolve_aliases_and_squash_unresolved_names_on_ty ~type_of_name
          ~force_to_kind
          ~unknown_payload
          or_alias2
      in
      match canonical_name1, canonical_name2 with
      | Some name1, Some name2 when Name.equal name1 name2 ->
        alias_type_of kind name1
      | _, _ ->
        let or_unknown_or_bottom =
          meet_on_or_unknown_or_bottom ~type_of_name
            or_unknown_or_bottom1 or_unknown_or_bottom2
            ~force_to_kind
            ~meet_of_kind_foo ~meet_unk
            ~join_of_kind_foo ~join_unk
            ~ty_of_t
        in


      match or_alias1, or_alias2 with
      | No_alias contents1, No_alias contents2 ->
        No_alias (meet_contents contents1 contents2)
      | No_alias contents, (Type export_id | Type_of name) ->


      | (Type export_id | Type_of name), No_alias contents ->

      | Type export_id1, Type export_id2 ->

      | Type_of name1, 

    let rec meet_on_or_join (type ty) (type unk) (type of_kind_foo) ~type_of_name
          (oj1 : ty or_join) (oj2 : ty or_join)
          ~(meet_of_kind_foo : (of_kind_foo -> of_kind_foo
            -> (of_kind_foo, unk) or_unknown_or_bottom) type_accessor)
          ~(meet_unk : unk -> unk -> unk)
          ~(join_of_kind_foo : (of_kind_foo -> of_kind_foo
            -> (of_kind_foo, unk) or_unknown_or_bottom) type_accessor)
          ~(join_unk : unk -> unk -> unk)
          ~(ty_of_t : t -> ty)
          : (ty, unk) or_unknown_or_bottom =
      match oj1, oj2 with
      | Normal s1, Normal s2 ->
        meet_of_kind_foo ~type_of_name s1 s2
      | ((Normal _ | Join _) as other_side), Join (or_alias1, or_alias2)
      | Join (or_alias1, or_alias2), ((Normal _ | Join _) as other_side) ->
        (* CR mshinwell: We should maybe be returning equations when we
           meet types equipped with alias information. *)
        let join_left =
          put_ok_under_or_alias ~type_of_name ~ty_of_t or_alias1
        in
        let join_right =
          put_ok_under_or_alias ~type_of_name ~ty_of_t or_alias2
        in
        let other_side_meet_join_left =
          meet_on_or_unknown_or_bottom ~type_of_name
            (Ok other_side) join_left
            ~meet_of_kind_foo ~meet_unk ~join_of_kind_foo ~join_unk ~ty_of_t
        in
        let other_side_meet_join_right =
          meet_on_or_unknown_or_bottom ~type_of_name
            (Ok other_side) join_right
            ~meet_of_kind_foo ~meet_unk ~join_of_kind_foo ~join_unk ~ty_of_t
        in
        join_on_or_unknown_or_bottom ~type_of_name
          other_side_meet_join_left other_side_meet_join_right

    and meet_on_or_unknown_or_bottom (type ty) (type unk) ~type_of_name
          (ou1 : (ty, unk) or_unknown_or_bottom)
          (ou2 : (ty, unk) or_unknown_or_bottom)
          : (ty, unk) or_unknown_or_bottom =
      match ou1, ou2 with
      | Bottom, _ | _, Bottom -> Bottom
      | Unknown unk1, Unknown unk2 -> Unknown (meet_unk unk1 unk2)
      | Unknown _, ou2 -> ou2
      | ou1, Unknown _ -> ou1
      | Ok or_join1, Ok or_join2 ->
        meet_on_or_join ~type_of_name or_join1 or_join2
          ~meet_of_kind_foo ~meet_unk
          ~join_of_kind_foo ~join_unk
          ~ty_of_t
  end

  module rec Meet_or_join_value : sig
    include Meet_or_join
      with type of_kind_foo = of_kind_value
      with type unk = K.Value_kind.t
  end = Make_meet_or_join (struct
    type of_kind_foo = of_kind_value
    type unk = K.Value_kind.t

    let force_to_kind = force_to_kind_value

    let unknown_payload = K.Value_kind.Unknown

    let meet_of_kind_foo ~type_of_name
          (of_kind1 : of_kind_value) (of_kind2 : of_kind_value)
          : (of_kind_value, unk) or_unknown_or_bottom =
      match of_kind1, of_kind2 with
      | Blocks_and_tagged_immediates blocks_imms1,
          Blocks_and_tagged_immediates blocks_imms2 ->
        let blocks_imms =
          meet_blocks_and_tagged_immediates ~type_of_name
            blocks_imms1 blocks_imms2
        in
        begin match blocks_imms with
        | Ok blocks_imms ->
          Ok (Normal (Blocks_and_tagged_immediates blocks_imms))
        | Bottom -> Bottom
        end
      | Boxed_number ((Boxed_float _) as n1),
          Boxed_number ((Boxed_float _) as n2) ->
        let n : _ ty_naked_number ... =
          Meet_or_join_naked_number.meet_ty ~type_of_name n1 n2
        in
        Normal (Boxed_number (Boxed_float n))

      | Closure closures1, Closure closures2 ->

      | String strs1, String strs2 ->
        let strs = String_info.Set.inter strs1 strs2 in
        if String_info.Set.is_empty strs then Bottom
        else Ok (Normal (String strs))
      | (Blocks_and_tagged_immediates _
          | Boxed_number _
          | Closure _
          | String _), _ ->
        Bottom

    let meet_unk value_kind1 value_kind2 =
      K.Value_kind.meet value_kind1 value_kind2

    let join_of_kind_foo ~type_of_name
          (of_kind1 : of_kind_value) (of_kind2 : of_kind_value)
          : of_kind_value or_join =
      match of_kind1, of_kind2 with
      | Blocks_and_tagged_immediates blocks_imms1,
          Blocks_and_tagged_immediates blocks_imms2 ->
        let blocks_imms =
          join_blocks_and_tagged_immediates ~type_of_name
            blocks_imms1 blocks_imms2
        in
        Normal (Blocks_and_tagged_immediates blocks_imms)
      | Boxed_number ((Boxed_float _) as n1),
          Boxed_number ((Boxed_float _) as n2) ->
        let n : _ ty_naked_number =
          Meet_or_join_naked_number.join_ty ~type_of_name n1 n2
        in
        Normal (Boxed_number (Boxed_float n))
      | Boxed_number ((Boxed_int32 _) as n1),
          Boxed_number ((Boxed_int32 _) as n2) ->
        let n : _ ty_naked_number =
          Meet_or_join_naked_number.join_ty ~type_of_name n1 n2
        in
        Normal (Boxed_number (Boxed_int32 n))
      | Boxed_number ((Boxed_int64 _) as n1),
          Boxed_number ((Boxed_int64 _) as n2) ->
        let n : _ ty_naked_number =
          Meet_or_join_naked_number.join_ty ~type_of_name n1 n2
        in
        Normal (Boxed_number (Boxed_int64 n))
      | Boxed_number ((Boxed_nativeint _) as n1),
          Boxed_number ((Boxed_nativeint _) as n2) ->
        let n : _ ty_naked_number =
          Meet_or_join_naked_number.join_ty ~type_of_name n1 n2
        in
        Normal (Boxed_number (Boxed_nativeint n))
      | Closure closures1, Closure closures2 ->

      | String strs1, String strs2 ->
        let strs = String_info.Set.union strs1 strs2 in
        Normal (String strs)
      | (Blocks_and_tagged_immediates _
          | Boxed_number _
          | Closure _
          | String _), _ ->
        (* CR mshinwell: Hmm, there is never any alias here. *)
        let left = No_alias (Normal of_kind1) in
        let right = No_alias (Normal of_kind2) in
        Join (left, right)

    let join_unk value_kind1 value_kind2 =
      K.Value_kind.join value_kind1 value_kind2
  end

(*
  let meet_immediate ~type_of_name
        ({ env_extension = env_extension1; } : immediate)
        ({ env_extension = env_extension2; } : immediate) : immediate =
    let env_extension =
      meet_typing_environment ~type_of_name env_extension1 env_extension2
    in
    { env_extension; }

  let join_immediate ~type_of_name
        ({ env_extension = env_extension1; } : immediate)
        ({ env_extension = env_extension2; } : immediate) : immediate =
    let env_extension =
      join_typing_environment ~type_of_name env_extension1 env_extension2
    in
    { env_extension; }

  let meet_singleton_block ~type_of_name
        ({ env_extension = env_extension1;
           first_fields = first_fields1;
         } : singleton_block)
        ({ env_extension = env_extension2;
           first_fields = first_fields2;
         } : singleton_block) : singleton_block =
    let env_extension =
      meet_typing_environment ~type_of_name env_extension1 env_extension2
    in
    let first_fields =
      match first_fields1, first_fields2 with
      | Exactly fields, Unknown_length
      | Unknown_length, Exactly fields -> Exactly fields
      | Unknown_length, Unknown_length -> Unknown_length
      | Exactly fields1, Exactly fields2 ->
        if Array.length fields1 = Array.length fields2 then
          Array.map2 (fun field1 field2 ->
              meet ~type_of_name field1 field2)
            fields1 fields2
        else
          ...
    in
    { env_extension;
      first_fields;
    }

  let join_singleton_block ~type_of_name
        ({ env_extension = env_extension1;
           first_fields = first_fields1;
         } : singleton_block)
        ({ env_extension = env_extension2;
           first_fields = first_fields2;
         } : singleton_block) : singleton_block list =
    let env_extension =
      meet_typing_environment ~type_of_name env_extension1 env_extension2
    in
    let first_fields =
      match first_fields1, first_fields2 with
      | Exactly fields, Unknown_length ->
      | Unknown_length, Exactly fields ->
      | Unknown_length, Unknown_length ->
      | Exactly fields1, Exactly fields2 ->
    in
    { env_extension;
      first_fields;
    }

  let meet_block ~type_of_name ((Join singleton_blocks) : block) =


  let join_block ~type_of_name ((Join singleton_blocks) : block) =


  let meet_blocks_and_immediates ~type_of_name
        { immediates = immediates1; blocks = blocks1; }
        { immediates = immediates2; blocks = blocks2; }
        : blocks_and_immediates =


  let join_blocks_and_immediates ~type_of_name
        { immediates = immediates1; blocks = blocks1; }
        { immediates = immediates2; blocks = blocks2; }
        : blocks_and_immediates =

*)

end
