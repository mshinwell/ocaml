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

[@@@ocaml.warning "+a-30-40-41-42"]

module TEE = Typing_env_extension

module T_V = Type_of_kind_value
module T_Nf = Type_of_kind_naked_float
module T_N32 = Type_of_kind_naked_int32
module T_N64 = Type_of_kind_naked_int64
module T_NN = Type_of_kind_naked_nativeint
module T_F = Type_of_kind_fabricated

type t =
  | Value of T_V.t
  | Naked_float of T_Nf.t
  | Naked_int32 of T_N32.t
  | Naked_int64 of T_N64.t
  | Naked_nativeint of T_NN.t
  | Fabricated of T_F.t

let print_with_cache ~cache ppf (t : Type_grammar.t) =
  match t with
  | Value ty ->
    Format.fprintf ppf "@[<hov 1>(Val@ %a)@]"
      (T_V.print_with_cache ~cache) ty
  | Naked_float ty ->
    Format.fprintf ppf "@[<hov 1>(Naked_float@ %a)@]"
      (T_Nf.print_with_cache ~cache) ty
  | Naked_int32 ty ->
    Format.fprintf ppf "@[<hov 1>(Naked_int32@ %a)@]"
      (T_N32.print_with_cache ~cache) ty
  | Naked_int64 ty ->
    Format.fprintf ppf "@[<hov 1>(Naked_int64@ %a)@]"
      (T_N64.print_with_cache ~cache) ty
  | Naked_nativeint ty ->
    Format.fprintf ppf "@[<hov 1>(Naked_nativeint@ %a)@]"
      (T_NN.print_with_cache ~cache) ty
  | Fabricated ty ->
    Format.fprintf ppf "@[<hov 1>(Fab@ %a)@]"
      (T_F.print_with_cache ~cache) ty

let print ppf t =
  let cache : Printing_cache.t = Printing_cache.create () in
  print_with_cache ~cache ppf t

let force_to_kind_value t =
  match t with
  | Value ty -> ty
  | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Fabricated _ ->
    Misc.fatal_errorf "Type has wrong kind (expected [Value]):@ %a" print t

let force_to_kind_naked_float t =
  match t with
  | Naked_float ty -> ty
  | Value _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Fabricated _ ->
    Misc.fatal_errorf
      "Type has wrong kind (expected [Naked_float]):@ %a"
      print t

let force_to_kind_naked_int32 t =
  match t with
  | Naked_int32 ty -> ty
  | Value _ | Naked_float _ | Naked_int64 _
  | Naked_nativeint _ | Fabricated _ ->
    Misc.fatal_errorf
      "Type has wrong kind (expected [Naked_int32]):@ %a"
      print t

let force_to_kind_naked_int64 t =
  match t with
  | Naked_int64 ty -> ty
  | Value _ | Naked_float _ | Naked_int32 _
  | Naked_nativeint _ | Fabricated _ ->
    Misc.fatal_errorf
      "Type has wrong kind (expected [Naked_number Int64]):@ %a"
      print t

let force_to_kind_naked_nativeint t =
  match t with
  | Naked_nativeint ty -> ty
  | Value _ | Naked_float _ | Naked_int32 _
  | Naked_int64 _ | Fabricated _ ->
    Misc.fatal_errorf
      "Type has wrong kind (expected [Naked_number Nativeint]):@ %a"
      print t

let force_to_kind_fabricated t =
  match t with
  | Fabricated ty -> ty
  | Value _ | Naked_float _ | Naked_int32 _
  | Naked_int64 _ | Naked_nativeint _ ->
    Misc.fatal_errorf "Type has wrong kind (expected [Fabricated]):@ %a"
      print t

let apply_name_permutation t perm =
  match t with
  | Value ty ->
    let ty' = T_V.apply_name_permutation ty perm in
    if ty == ty' then t
    else Value ty'
  | Naked_float ty ->
    let ty' = T_Nf.apply_name_permutation ty perm in
    if ty == ty' then t
    else Naked_float ty'
  | Naked_int32 ty ->
    let ty' = T_N32.apply_name_permutation ty perm in
    if ty == ty' then t
    else Naked_int32 ty'
  | Naked_int64 ty ->
    let ty' = T_N64.apply_name_permutation ty perm in
    if ty == ty' then t
    else Naked_int64 ty'
  | Naked_nativeint ty ->
    let ty' = T_NN.apply_name_permutation ty perm in
    if ty == ty' then t
    else Naked_nativeint ty'
  | Fabricated ty ->
    let ty' = T_F.apply_name_permutation ty perm in
    if ty == ty' then t
    else Fabricated ty'

let free_names t =
  match t with
  | Value ty -> T_V.free_names ty
  | Naked_float ty -> T_Nf.free_names ty
  | Naked_int32 ty -> T_N32.free_names ty
  | Naked_int64 ty -> T_N64.free_names ty
  | Naked_nativeint ty -> T_NN.free_names ty
  | Fabricated ty -> T_F.free_names ty

let apply_rec_info t rec_info : _ Or_bottom.t =
  match t with
  | Value ty ->
    begin match T_V.apply_rec_info ty rec_info with
    | Ok ty -> Ok (Value ty)
    | Bottom -> Bottom
    end
  | Naked_float ty ->
    begin match T_Nf.apply_rec_info ty rec_info with
    | Ok ty -> Ok (Naked_float ty)
    | Bottom -> Bottom
    end
  | Naked_int32 ty ->
    begin match T_N32.apply_rec_info ty rec_info with
    | Ok ty -> Ok (Naked_int32 ty)
    | Bottom -> Bottom
    end
  | Naked_int64 ty ->
    begin match T_N64.apply_rec_info ty rec_info with
    | Ok ty -> Ok (Naked_int64 ty)
    | Bottom -> Bottom
    end
  | Naked_nativeint ty ->
    begin match T_NN.apply_rec_info ty rec_info with
    | Ok ty -> Ok (Naked_nativeint ty)
    | Bottom -> Bottom
    end
  | Fabricated ty ->
    begin match T_F.apply_rec_info ty rec_info with
    | Ok ty -> Ok (Fabricated ty)
    | Bottom -> Bottom
    end

let kind t =
  match t with
  | Value _ -> K.value
  | Naked_float _ -> K.naked_float
  | Naked_int32 _ -> K.naked_int32
  | Naked_int64 _ -> K.naked_int64
  | Naked_nativeint _ -> K.naked_nativeint
  | Fabricated _ -> K.fabricated

let get_alias t =
  match t with
  | Value ty -> T_V.get_alias ty
  | Naked_float ty -> T_Nf.get_alias ty
  | Naked_int32 ty -> T_N32.get_alias ty
  | Naked_int64 ty -> T_N64.get_alias ty
  | Naked_nativeint ty -> T_NN.get_alias ty
  | Fabricated ty -> T_F.get_alias ty

(* CR mshinwell: We should have transformations and invariant checks to
   enforce that, when a type can be expressed just using [Equals] (e.g. to
   a tagged immediate [Simple]), then it should be.  In the tagged immediate
   case this would mean forbidding Blocks_and_tagged_immediates with only
   a single immediate.  Although this state needs to exist during [meet]
   or whenever heads are expanded. *)

let is_obviously_bottom (t : t) =
  match t with
  | Value ty -> T_V.is_obviously_bottom ty
  | Naked_float ty -> T_Nf.is_obviously_bottom ty
  | Naked_int32 ty -> T_N32.is_obviously_bottom ty
  | Naked_int64 ty -> T_N64.is_obviously_bottom ty
  | Naked_nativeint ty -> T_NN.is_obviously_bottom ty
  | Fabricated ty -> T_F.is_obviously_bottom ty

let alias_type_of (kind : K.t) name : t =
  match kind with
  | Value -> Value (T_V.create_equals name)
  | Naked_number Naked_float -> Naked_float (T_Nf.create_equals name)
  | Naked_number Naked_int32 -> Naked_int32 (T_N32.create_equals name)
  | Naked_number Naked_int64 -> Naked_int64 (T_N64.create_equals name)
  | Naked_number Naked_nativeint -> Naked_nativeint (T_NN.create_equals name)
  | Fabricated -> Fabricated (T_F.create_equals name)

let bottom_value () = Value (T_V.bottom ())
let bottom_naked_float () = Naked_float (T_Nf.bottom ())
let bottom_naked_int32 () = Naked_int32 (T_N32.bottom ())
let bottom_naked_int64 () = Naked_int64 (T_N64.bottom ())
let bottom_naked_nativeint () = Naked_nativeint (T_NN.bottom ())
let bottom_fabricated () = Fabricated (T_F.bottom ())

let bottom (kind : K.t) =
  match kind with
  | Value -> bottom_value ()
  | Naked_number Naked_float -> bottom_naked_float ()
  | Naked_number Naked_int32 -> bottom_naked_int32 ()
  | Naked_number Naked_int64 -> bottom_naked_int64 ()
  | Naked_number Naked_nativeint -> bottom_naked_nativeint ()
  | Fabricated -> bottom_fabricated ()

let bottom_like t = bottom (kind t)

let any_value () = Value (T_V.unknown ())
let any_naked_float () = Naked_float (T_Nf.unknown ())
let any_naked_int32 () = Naked_int32 (T_N32.unknown ())
let any_naked_int64 () = Naked_int64 (T_N64.unknown ())
let any_naked_nativeint () = Naked_nativeint (T_NN.unknown ())
let any_fabricated () = Fabricated (T_F.unknown ())

let unknown (kind : K.t) =
  match kind with
  | Value -> any_value ()
  | Naked_number Naked_float -> any_naked_float ()
  | Naked_number Naked_int32 -> any_naked_int32 ()
  | Naked_number Naked_int64 -> any_naked_int64 ()
  | Naked_number Naked_nativeint -> any_naked_nativeint ()
  | Fabricated -> any_fabricated ()

let unknown_like t = unknown (kind t)

let this_naked_float f : t =
  Naked_float (T_Nf.create_equals (Simple.const (Naked_float f)))

let this_naked_int32 i : t =
  Naked_int32 (T_N32.create_equals (Simple.const (Naked_int32 i)))

let this_naked_int64 i : t =
  Naked_int64 (T_N64.create_equals (Simple.const (Naked_int64 i)))

let this_naked_nativeint i : t =
  Naked_nativeint (T_NN.create_equals (Simple.const (Naked_nativeint i)))

let these_naked_floats0 ~no_alias fs =
  match Float.Set.get_singleton fs with
  | Some f when not no_alias -> this_naked_float f
  | _ ->
    if Float.Set.is_empty fs then bottom K.naked_float
    else Naked_float (T_Nf.create_no_alias (Ok fs))

let these_naked_int32s0 ~no_alias is =
  match Int32.Set.get_singleton is with
  | Some i when not no_alias -> this_naked_int32 i
  | _ ->
    if Int32.Set.is_empty is then bottom K.naked_int32
    else Naked_int32 (T_N32.create_no_alias (Ok is))

let these_naked_int64s0 ~no_alias is =
  match Int64.Set.get_singleton is with
  | Some i when not no_alias -> this_naked_int64 i
  | _ ->
    if Int64.Set.is_empty is then bottom K.naked_int64
    else Naked_int64 (T_N64.create_no_alias (Ok is))

let these_naked_nativeints0 ~no_alias is =
  match Targetint.Set.get_singleton is with
  | Some i when not no_alias -> this_naked_nativeint i
  | _ ->
    if Targetint.Set.is_empty is then bottom K.naked_nativeint
    else Naked_nativeint (T_NN.create_no_alias (Ok is))

let this_naked_float_without_alias f =
  these_naked_floats0 ~no_alias:true (Float.Set.singleton f)

let this_naked_int32_without_alias i =
  these_naked_int32s0 ~no_alias:true (Int32.Set.singleton i)

let this_naked_int64_without_alias i =
  these_naked_int64s0 ~no_alias:true (Int64.Set.singleton i)

let this_naked_nativeint_without_alias i =
  these_naked_nativeints0 ~no_alias:true (Targetint.Set.singleton i)

let these_naked_floats fs = these_naked_floats0 ~no_alias:false fs
let these_naked_int32s is = these_naked_int32s0 ~no_alias:false is
let these_naked_int64s is = these_naked_int64s0 ~no_alias:false is
let these_naked_nativeints is = these_naked_nativeints0 ~no_alias:false is

let box_float (t : t) : t =
  match t with
  | Naked_float _ -> Value (T_V.create (Boxed_float t))
  | Value _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Fabricated _ ->
    Misc.fatal_errorf "Type of wrong kind for [box_float]: %a"
      print t

let box_int32 (t : t) : t =
  match t with
  | Naked_int32 _ -> Value (T_V.create (Boxed_int32 t))
  | Value _ | Naked_float _ | Naked_int64 _
  | Naked_nativeint _ | Fabricated _ ->
    Misc.fatal_errorf "Type of wrong kind for [box_int32]: %a"
      print t

let box_int64 (t : t) : t =
  match t with
  | Naked_int64 _ -> Value (T_V.create (Boxed_int64 t))
  | Value _ | Naked_float _ | Naked_int32 _
  | Naked_nativeint _ | Fabricated _ ->
    Misc.fatal_errorf "Type of wrong kind for [box_int64]: %a"
      print t

let box_nativeint (t : t) : t =
  match t with
  | Naked_nativeint _ -> Value (T_V.create (Boxed_nativeint t))
  | Value _ | Naked_float _ | Naked_int32 _
  | Naked_int64 _ | Fabricated _ ->
    Misc.fatal_errorf "Type of wrong kind for [box_nativeint]: %a"
      print t

let any_tagged_immediate () : t =
  Value (T_V.create_no_alias (Ok (Blocks_and_tagged_immediates {
    immediates = Unknown;
    blocks = Known (Row_like.For_blocks.create_bottom ());
  })))

let this_tagged_immediate imm : t =
  Value (T_V.create_equals (Simple.const (Tagged_immediate imm)))

let these_tagged_immediates0 ~no_alias imms : t =
  match Immediate.Set.get_singleton imms with
  | Some imm when not no_alias -> this_tagged_immediate imm
  | _ ->
    if Immediate.Set.is_empty imms then bottom K.value
    else
      let immediates = Row_like.For_immediates.create imms in
      Value (T_V.create_no_alias (
        Ok (Blocks_and_tagged_immediates {
          immediates = Known immediates;
          blocks = Known (Row_like.For_blocks.create_bottom ());
        })))

let these_tagged_immediates imms =
  these_tagged_immediates0 ~no_alias:false imms

let this_tagged_immediate_without_alias imm =
  these_tagged_immediates0 ~no_alias:true (Immediate.Set.singleton imm)

let this_discriminant discr : t =
  Fabricated (T_F.create_equals (Simple.discriminant discr))

(* CR mshinwell: Same code pattern as the tagged immediates cases above,
   factor out. *)
let these_discriminants0 ~no_alias discrs : t =
  match Discriminant.Set.get_singleton discrs with
  | Some discr when not no_alias -> this_discriminant discr
  | _ ->
    if Discriminant.Set.is_empty discrs then bottom K.fabricated
    else
      let discrs = Row_like.For_discriminants.create discrs in
      Fabricated (T_F.create (Discriminants discrs))

let these_discriminants discrs =
  these_discriminants0 ~no_alias:false discrs

let this_discriminant_without_alias discr : t =
  these_discriminants0 ~no_alias:true (Discriminant.Set.singleton discr)

let any_tagged_bool () = these_tagged_immediates Immediate.all_bools

let this_boxed_float f = box_float (this_naked_float f)
let this_boxed_int32 i = box_int32 (this_naked_int32 i)
let this_boxed_int64 i = box_int64 (this_naked_int64 i)
let this_boxed_nativeint i = box_nativeint (this_naked_nativeint i)

let these_boxed_floats fs = box_float (these_naked_floats fs)
let these_boxed_int32s is = box_int32 (these_naked_int32s is)
let these_boxed_int64s is = box_int64 (these_naked_int64s is)
let these_boxed_nativeints is = box_nativeint (these_naked_nativeints is)

let boxed_float_alias_to ~naked_float =
  box_float (Naked_float (T_Nf.create_equals (Simple.var naked_float)))

let boxed_int32_alias_to ~naked_int32 =
  box_int32 (Naked_int32 (T_N32.create_equals (Simple.var naked_int32)))

let boxed_int64_alias_to ~naked_int64 =
  box_int64 (Naked_int64 (T_N64.create_equals (Simple.var naked_int64)))

let boxed_nativeint_alias_to ~naked_nativeint =
  box_nativeint (Naked_nativeint (
    T_NN.create_equals (Simple.var naked_nativeint)))

let immutable_block tag ~fields =
  (* CR mshinwell: We should check the field kinds against the tag. *)
  match Targetint.OCaml.of_int_option (List.length fields) with
  | None ->
    (* CR mshinwell: This should be a special kind of error. *)
    Misc.fatal_error "Block too long for target"
  | Some _size ->
    Value (T_V.create_no_alias (Ok (
      Blocks_and_tagged_immediates {
        immediates = Known (Row_like.For_immediates.create_bottom ());
        blocks = Known (Row_like.For_blocks.create ~field_tys:fields (Closed tag));
      })))

let immutable_block_with_size_at_least ~n ~field_n_minus_one =
  let n = Targetint.OCaml.to_int n in
  let field_tys =
    List.init n (fun index ->
        if index < n - 1 then any_value ()
        else alias_type_of K.value (Simple.var field_n_minus_one))
  in
  Value (T_V.create_no_alias (Ok (
    Blocks_and_tagged_immediates {
      immediates = Known (Row_like.For_immediates.create_bottom ());
      blocks = Known (Row_like.For_blocks.create ~field_tys Open);
    })))

let this_immutable_string str =
  (* CR mshinwell: Use "length" not "size" for strings *)
  let size = Targetint.OCaml.of_int (String.length str) in
  let string_info =
    String_info.Set.singleton
      (String_info.create ~contents:(Contents str) ~size)
  in
  Value (T_V.create (String string_info))

let mutable_string ~size =
  let size = Targetint.OCaml.of_int size in
  let string_info =
    String_info.Set.singleton
      (String_info.create ~contents:Unknown_or_mutable ~size)
  in
  Value (T_V.create (String string_info))

let any_boxed_float () = box_float (any_naked_float ())
let any_boxed_int32 () = box_int32 (any_naked_int32 ())
let any_boxed_int64 () = box_int64 (any_naked_int64 ())
let any_boxed_nativeint () = box_nativeint (any_naked_nativeint ())

let create_inlinable_function_declaration function_decl rec_info
      : Function_declaration_type.t =
  Inlinable {
    function_decl;
    rec_info;
  }

let create_non_inlinable_function_declaration ~param_arity ~result_arity
      ~recursive : Function_declaration_type.t =
  Non_inlinable {
    param_arity;
    result_arity;
    recursive;
  }

let exactly_this_closure closure_id ~all_function_decls_in_set:function_decls
      ~all_closures_in_set:closure_types
      ~all_closure_vars_in_set:closure_var_types =
  let closure_types = Product.Closure_id_indexed.create closure_types in
  let closures_entry =
    let closure_var_types =
      Product.Var_within_closure_indexed.create closure_var_types
    in
    let function_decls =
      Closure_id.Map.map (fun func_decl -> Or_unknown.Known func_decl)
        function_decls
    in
    Closures_entry.create ~function_decls ~closure_types ~closure_var_types
  in
  let by_closure_id =
    let set_of_closures_contents =
      Set_of_closures_contents.create (Closure_id.Map.keys function_decls)
        (Var_within_closure.Map.keys closure_var_types)
    in
    let set_of_closures_contents_to_closures_entry =
      Set_of_closures_contents.With_closure_id.Map.singleton
        (closure_id, set_of_closures_contents)
        closures_entry
    in
    Row_like.For_closures_entry_by_set_of_closures_contents.
      create_exactly_multiple
        set_of_closures_contents_to_closures_entry
  in
  Value (T_V.create (Closures { by_closure_id; }))

let at_least_the_closures_with_ids ~this_closure closure_ids_and_bindings =
  let closure_ids_and_types =
    Closure_id.Map.map (fun bound_to -> alias_type_of K.value bound_to)
      closure_ids_and_bindings
  in
  let function_decls =
    Closure_id.Map.map (fun _ -> Or_unknown.Unknown)
      closure_ids_and_bindings
  in
  let closure_types = Product.Closure_id_indexed.create closure_ids_and_types in
  let closures_entry =
    Closures_entry.create ~function_decls ~closure_types
      ~closure_var_types:(Product.Var_within_closure_indexed.create_bottom ())
  in
  let by_closure_id =
    let set_of_closures_contents =
      Set_of_closures_contents.create
        (Closure_id.Map.keys closure_ids_and_types)
        Var_within_closure.Set.empty
    in
    let set_of_closures_contents_to_closures_entry =
      Set_of_closures_contents.With_closure_id_or_unknown.Map.singleton
        (Known this_closure, set_of_closures_contents)
        closures_entry
    in
    Row_like.For_closures_entry_by_set_of_closures_contents.
      create_at_least_multiple
        set_of_closures_contents_to_closures_entry
  in
  Value (T_V.create (Closures { by_closure_id; }))

let closure_with_at_least_this_closure_var closure_var ~closure_element_var =
  let closure_var_types =
    let closure_var_type =
      alias_type_of K.value (Simple.var closure_element_var)
    in
    Product.Var_within_closure_indexed.create
      (Var_within_closure.Map.singleton closure_var closure_var_type)
  in
  let closures_entry =
    Closures_entry.create ~function_decls:Closure_id.Map.empty
      ~closure_types:(Product.Closure_id_indexed.create_bottom ())
      ~closure_var_types
  in
  let by_closure_id =
    let set_of_closures_contents =
      Set_of_closures_contents.create
        Closure_id.Set.empty
        (Var_within_closure.Set.singleton closure_var)
    in
    let set_of_closures_contents_to_closures_entry =
      Set_of_closures_contents.With_closure_id_or_unknown.Map.singleton
        (Unknown, set_of_closures_contents)
        closures_entry
    in
    Row_like.For_closures_entry_by_set_of_closures_contents.
      create_at_least_multiple
        set_of_closures_contents_to_closures_entry
  in
  Value (T_V.create (Closures { by_closure_id; }))

let array_of_length ~length =
  Value (T_V.create (Array { length; }))

let type_for_const (const : Simple.Const.t) =
  match const with
  | Tagged_immediate i -> this_tagged_immediate i
  | Naked_float f -> this_naked_float f
  | Naked_int32 n -> this_naked_int32 n
  | Naked_int64 n -> this_naked_int64 n
  | Naked_nativeint n -> this_naked_nativeint n

let kind_for_const const = kind (type_for_const const)

(* CR mshinwell: Having to have this here is a bit of a nuisance. *)
let make_suitable_for_environment0 t env ~suitable_for level =
  match t with
  | Value ty ->
    let level, ty' =
      T_V.make_suitable_for_environment0 ty env ~suitable_for level
    in
    if ty == ty' then level, t
    else level, Value ty'
  | Naked_float ty ->
    let level, ty' =
      T_Nf.make_suitable_for_environment0 ty env ~suitable_for level
    in
    if ty == ty' then level, t
    else level, Naked_float ty'
  | Naked_int32 ty ->
    let level, ty' =
      T_N32.make_suitable_for_environment0 ty env ~suitable_for level
    in
    if ty == ty' then level, t
    else level, Naked_int32 ty'
  | Naked_int64 ty ->
    let level, ty' =
      T_N64.make_suitable_for_environment0 ty env ~suitable_for level
    in
    if ty == ty' then level, t
    else level, Naked_int64 ty'
  | Naked_nativeint ty ->
    let level, ty' =
      T_NN.make_suitable_for_environment0 ty env ~suitable_for level
    in
    if ty == ty' then level, t
    else level, Naked_nativeint ty'
  | Fabricated ty ->
    let level, ty' =
      T_F.make_suitable_for_environment0 ty env ~suitable_for level
    in
    if ty == ty' then level, t
    else level, Fabricated ty'

let make_suitable_for_environment t env ~suitable_for ~bind_to =
  match t with
  | Value ty ->
    T_V.make_suitable_for_environment ty env ~suitable_for ~bind_to
      ~to_type:(fun ty -> Value ty)
  | Naked_float ty ->
    T_Nf.make_suitable_for_environment ty env ~suitable_for ~bind_to
      ~to_type:(fun ty -> Naked_float ty)
  | Naked_int32 ty ->
    T_N32.make_suitable_for_environment ty env ~suitable_for ~bind_to
      ~to_type:(fun ty -> Naked_int32 ty)
  | Naked_int64 ty ->
    T_N64.make_suitable_for_environment ty env ~suitable_for ~bind_to
      ~to_type:(fun ty -> Naked_int64 ty)
  | Naked_nativeint ty ->
    T_NN.make_suitable_for_environment ty env ~suitable_for ~bind_to
      ~to_type:(fun ty -> Naked_nativeint ty)
  | Fabricated ty ->
    T_F.make_suitable_for_environment ty env ~suitable_for ~bind_to
      ~to_type:(fun ty -> Fabricated ty)

let expand_head t env : Resolved_type.t =
  match t with
  | Value ty ->
    let head =
      T_V.expand_head ~force_to_kind:force_to_kind_value ty env
    in
    Resolved (Value head)
  | Naked_float ty ->
    let head =
      T_Nf.expand_head ~force_to_kind:force_to_kind_naked_float ty env
    in
    Resolved (Naked_float head)
  | Naked_int32 ty ->
    let head =
      T_N32.expand_head ~force_to_kind:force_to_kind_naked_int32 ty env
    in
    Resolved (Naked_int32 head)
  | Naked_int64 ty ->
    let head =
      T_N64.expand_head ~force_to_kind:force_to_kind_naked_int64 ty env
    in
    Resolved (Naked_int64 head)
  | Naked_nativeint ty ->
    let head =
      T_NN.expand_head ~force_to_kind:force_to_kind_naked_nativeint ty env
    in
    Resolved (Naked_nativeint head)
  | Fabricated ty ->
    let head =
      T_F.expand_head ~force_to_kind:force_to_kind_fabricated ty env
    in
    Resolved (Fabricated head)

module Make_meet_or_join
  (E : Lattice_ops_intf.S
   with type meet_env := Meet_env.t
   with type typing_env := Typing_env.t
   with type typing_env_extension := Typing_env_extension.t) =
struct
  let meet_or_join env t1 t2 =
    match t1, t2 with
    | Value ty1, Value ty2 ->
      (* CR mshinwell: Try to lift out these functor applications (or ditch
         the functors entirely). *)
      let module T_V_meet_or_join = T_V.Make_meet_or_join (E) in
      begin match
        T_V_meet_or_join.meet_or_join env ty1 ty2
          ~force_to_kind:force_to_kind_value
          ~to_type:(fun ty -> Value ty)
      with
      | Ok (ty, env_extension) -> Value ty, env_extension
      | Bottom -> bottom_value (), TEE.empty ()
      end
    | Naked_float ty1, Naked_float ty2 ->
      let module T_Nf_meet_or_join = T_Nf.Make_meet_or_join (E) in
      begin match
        T_Nf_meet_or_join.meet_or_join env ty1 ty2
          ~force_to_kind:force_to_kind_naked_float
          ~to_type:(fun ty -> Naked_float ty)
      with
      | Ok (ty, env_extension) -> Naked_float ty, env_extension
      | Bottom -> bottom_naked_float (), TEE.empty ()
      end
    | Naked_int32 ty1, Naked_int32 ty2 ->
      let module T_N32_meet_or_join = T_N32.Make_meet_or_join (E) in
      begin match
        T_N32_meet_or_join.meet_or_join env ty1 ty2
          ~force_to_kind:force_to_kind_naked_int32
          ~to_type:(fun ty -> Naked_int32 ty)
      with
      | Ok (ty, env_extension) -> Naked_int32 ty, env_extension
      | Bottom -> bottom_naked_int32 (), TEE.empty ()
      end
    | Naked_int64 ty1, Naked_int64 ty2 ->
      let module T_N64_meet_or_join = T_N64.Make_meet_or_join (E) in
      begin match
        T_N64_meet_or_join.meet_or_join env ty1 ty2
          ~force_to_kind:force_to_kind_naked_int64
          ~to_type:(fun ty -> Naked_int64 ty)
      with
      | Ok (ty, env_extension) -> Naked_int64 ty, env_extension
      | Bottom -> bottom_naked_int64 (), TEE.empty ()
      end
    | Naked_nativeint ty1, Naked_nativeint ty2 ->
      let module T_NN_meet_or_join = T_NN.Make_meet_or_join (E) in
      begin match
        T_NN_meet_or_join.meet_or_join env ty1 ty2
          ~force_to_kind:force_to_kind_naked_nativeint
          ~to_type:(fun ty -> Naked_nativeint ty)
      with
      | Ok (ty, env_extension) -> Naked_nativeint ty, env_extension
      | Bottom -> bottom_naked_nativeint (), TEE.empty ()
      end
    | Fabricated ty1, Fabricated ty2 ->
      let module T_F_meet_or_join = T_F.Make_meet_or_join (E) in
      begin match
        T_F_meet_or_join.meet_or_join env ty1 ty2
          ~force_to_kind:force_to_kind_fabricated
          ~to_type:(fun ty -> Fabricated ty)
      with
      | Ok (ty, env_extension) -> Fabricated ty, env_extension
      | Bottom -> bottom_fabricated (), TEE.empty ()
      end
    | (Value _ | Naked_float _ | Naked_int32 _
        | Naked_int64 _ | Naked_nativeint _ | Fabricated _), _ ->
      Misc.fatal_errorf "Kind mismatch upon %s:@ %a@ versus@ %a"
        (E.name ())
        print t1
        print t2
end

module Meet = Make_meet_or_join (Lattice_ops.For_meet)
module Join = Make_meet_or_join (Lattice_ops.For_join)

let meet' env t1 t2 = Meet.meet_or_join env t1 t2

let meet env t1 t2 : _ Or_bottom.t =
  let ty, env_extension = meet' env t1 t2 in
  if is_obviously_bottom ty then Bottom
  else Ok (ty, env_extension)

let join env t1 t2 =
  let env = Meet_env.create env in
  let joined, env_extension = Join.meet_or_join env t1 t2 in
  if not (TEE.is_empty env_extension) then begin
    Misc.fatal_errorf "Non-empty environment extension produced from a \
        [join] operation:@ %a"
      TEE.print env_extension
  end;
  joined
