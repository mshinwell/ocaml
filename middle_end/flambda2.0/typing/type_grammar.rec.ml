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

type t =
  | Value of Type_of_kind_value.t
  | Naked_number of Type_of_kind_naked_number.t
  | Fabricated of Type_of_kind_fabricated.t

let print_with_cache ~cache ppf (t : Type_grammar.t) =
  match t with
  | Value ty ->
    Format.fprintf ppf "@[<hov 1>(Val@ %a)@]"
      (Type_of_kind_value.print_with_cache ~cache) ty
  | Naked_number ty ->
    Format.fprintf ppf "@[<hov 1>(Naked@ %a)@]"
      (Type_of_kind_naked_number.print_with_cache ~cache) ty
  | Fabricated ty ->
    Format.fprintf ppf "@[<hov 1>(Fab@ %a)@]"
      (Type_of_kind_fabricated.print_with_cache ~cache) ty

let print ppf t =
  let cache : Printing_cache.t = Printing_cache.create () in
  print_with_cache ~cache ppf t

let apply_name_permutation t perm =
  match t with
  | Value ty ->
    let ty' = Type_of_kind_value.apply_name_permutation ty perm in
    if ty == ty' then t
    else Value ty'
  | Naked_number ty ->
    let ty' = Type_of_kind_naked_number.apply_name_permutation ty perm in
    if ty == ty' then t
    else Naked_number ty'
  | Fabricated ty ->
    let ty' = Type_of_kind_fabricated.apply_name_permutation ty perm in
    if ty == ty' then t
    else Fabricated ty'

let free_names t =
  match t with
  | Value ty -> Type_of_kind_value.free_names ty
  | Naked_number ty -> Type_of_kind_naked_number.free_names ty
  | Fabricated ty -> Type_of_kind_fabricated.free_names ty

let erase_aliases t ~allowed =
  match t with
  | Value ty ->
    Value (Type_of_kind_value.erase_aliases ty ~allowed)
  | Naked_number ty ->
    Naked_number (Type_of_kind_naked_number.erase_aliases ty ~allowed)
  | Fabricated ty ->
    Fabricated (Type_of_kind_fabricated.erase_aliases ty ~allowed)

let apply_rec_info t rec_info : _ Or_bottom.t =
  match t with
  | Value ty ->
    begin match Type_of_kind_value.apply_rec_info ty rec_info with
    | Ok ty -> Ok (Value ty)
    | Bottom -> Bottom
    end
  | Naked_number (ty_naked_number, kind) ->
    begin match Type_of_kind_naked_number.apply_rec_info ty rec_info with
    | Ok ty -> Ok (Naked_number ty)
    | Bottom -> Bottom
    end
  | Fabricated ty ->
    begin match Type_of_kind_fabricated.apply_rec_info ty rec_info with
    | Ok ty -> Ok (Fabricated ty)
    | Bottom -> Bottom
    end

(* CR mshinwell: We should have transformations and invariant checks to
   enforce that, when a type can be expressed just using [Equals] (e.g. to
   a tagged immediate [Simple]), then it should be.  In the tagged immediate
   case this would mean forbidding Blocks_and_tagged_immediates with only
   a single immediate.  Although this state needs to exist during [meet]
   or whenever heads are expanded. *)

let force_to_kind_value (t : t) =
  match t with
  | Value ty -> ty
  | Naked_number _
  | Fabricated _ ->
    Misc.fatal_errorf "Type has wrong kind (expected [Value]):@ %a" print t

let force_to_kind_naked_immediate (t : t) : K.naked_immediate ty_naked_number =
  match t with
  | Naked_number (ty_naked_number, K.Naked_number.Naked_immediate) ->
    ty_naked_number
  | Naked_number _
  | Fabricated _
  | Value _ ->
    Misc.fatal_errorf
      "Type has wrong kind (expected [Naked_number Immediate]):@ %a"
      print t

let force_to_kind_naked_float (t : t) : K.naked_float ty_naked_number =
  match t with
  | Naked_number (ty_naked_number, K.Naked_number.Naked_float) ->
    ty_naked_number
  | Naked_number _
  | Fabricated _
  | Value _ ->
    Misc.fatal_errorf
      "Type has wrong kind (expected [Naked_number Float]):@ %a"
      print t

let force_to_kind_naked_int32 (t : t) : K.naked_int32 ty_naked_number =
  match t with
  | Naked_number (ty_naked_number, K.Naked_number.Naked_int32) ->
    ty_naked_number
  | Naked_number _
  | Fabricated _
  | Value _ ->
    Misc.fatal_errorf
      "Type has wrong kind (expected [Naked_number Int32]):@ %a"
      print t

let force_to_kind_naked_int64 (t : t) : K.naked_int64 ty_naked_number =
  match t with
  | Naked_number (ty_naked_number, K.Naked_number.Naked_int64) ->
    ty_naked_number
  | Naked_number _
  | Fabricated _
  | Value _ ->
    Misc.fatal_errorf
      "Type has wrong kind (expected [Naked_number Int64]):@ %a"
      print t

let force_to_kind_naked_nativeint (t : t) : K.naked_nativeint ty_naked_number =
  match t with
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
  | Value _, _ ->
    Misc.fatal_errorf "Type has wrong kind (expected \
        [Naked_number %a]):@ %a"
      K.Naked_number.print kind
      print t

let force_to_kind_fabricated (t : t) =
  match t with
  | Fabricated ty_fabricated -> ty_fabricated
  | Value _
  | Naked_number _ ->
    Misc.fatal_errorf "Type has wrong kind (expected [Fabricated]):@ %a"
      print t

let ty_is_obviously_bottom (ty : _ ty) =
  match ty with
  | No_alias Bottom -> true
  | _ -> false

let is_obviously_bottom (t : t) =
  match t with
  | Value ty -> ty_is_obviously_bottom ty
  | Naked_number (ty, _) -> ty_is_obviously_bottom ty
  | Fabricated ty -> ty_is_obviously_bottom ty

let alias_type_of (kind : K.t) name : t =
  match kind with
  | Value ->
    Value (Equals name)
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
  | Fabricated ->
    Fabricated (Equals name)

let alias_type_of_as_ty_value name : ty_value = Equals name

let alias_type_of_as_ty_fabricated name : ty_fabricated = Equals name

let bottom (kind : K.t) : t =
  match kind with
  | Value ->
    Value (No_alias Bottom)
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
  | Fabricated ->
    Fabricated (No_alias Bottom)

let any_value_as_ty_value () : ty_value =
  No_alias Unknown

let any_value () : t =
  Value (any_value_as_ty_value ())

let any_tagged_immediate () : t =
  Value (No_alias (Ok (Blocks_and_tagged_immediates {
    immediates = Unknown;
    (* CR mshinwell: Again, here, should this allow [Bottom] as well as
       [Unknown]? *)
    blocks = Known (Blocks.create_bottom ());
  })))

let any_naked_immediate () : t =
  Naked_number (No_alias Unknown, K.Naked_number.Naked_immediate)

let any_naked_float () : t =
  Naked_number (No_alias Unknown, K.Naked_number.Naked_float)

let any_naked_int32 () : t =
  Naked_number (No_alias Unknown, K.Naked_number.Naked_int32)

let any_naked_int64 () : t =
  Naked_number (No_alias Unknown, K.Naked_number.Naked_int64)

let any_naked_nativeint () : t =
  Naked_number (No_alias Unknown, K.Naked_number.Naked_nativeint)

let any_fabricated () : t =
  Fabricated (No_alias Unknown)

let unknown (kind : K.t) : t =
  match kind with
  | Value ->
    Value (No_alias Unknown)
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
  | Fabricated ->
    Fabricated (No_alias Unknown)

let unknown_as_ty_fabricated () : ty_fabricated = No_alias Unknown

let this_naked_immediate i : t =
  Naked_number (Equals (Simple.const (Naked_immediate i)),
    K.Naked_number.Naked_immediate)

let this_naked_float f : t =
  Naked_number (Equals (Simple.const (Naked_float f)),
    K.Naked_number.Naked_float)

let this_naked_int32 i : t =
  Naked_number (Equals (Simple.const (Naked_int32 i)),
    K.Naked_number.Naked_int32)

let this_naked_int64 i : t =
  Naked_number (Equals (Simple.const (Naked_int64 i)),
    K.Naked_number.Naked_int64)

let this_naked_nativeint i : t =
  Naked_number (Equals (Simple.const (Naked_nativeint i)),
    K.Naked_number.Naked_nativeint)

let these_naked_immediates0 ~no_alias (is : Immediate.Set.t) : t =
  match Immediate.Set.get_singleton is with
  | Some i when not no_alias -> this_naked_immediate i
  | _ ->
    if Immediate.Set.is_empty is then bottom K.naked_immediate
    else
      let of_kind : _ of_kind_naked_number = Immediate is in
      Naked_number (No_alias (Ok of_kind), K.Naked_number.Naked_immediate)

let these_naked_floats0 ~no_alias (fs : Float.Set.t) : t =
  match Float.Set.get_singleton fs with
  | Some f when not no_alias -> this_naked_float f
  | _ ->
    if Float.Set.is_empty fs then bottom K.naked_float
    else
      let of_kind : _ of_kind_naked_number = Float fs in
      Naked_number (No_alias (Ok of_kind), K.Naked_number.Naked_float)

let these_naked_int32s0 ~no_alias (is : Int32.Set.t) : t =
  match Int32.Set.get_singleton is with
  | Some i when not no_alias -> this_naked_int32 i
  | _ ->
    if Int32.Set.is_empty is then bottom K.naked_int32
    else
      let of_kind : _ of_kind_naked_number = Int32 is in
      Naked_number (No_alias (Ok of_kind), K.Naked_number.Naked_int32)

let these_naked_int64s0 ~no_alias (is : Int64.Set.t) : t =
  match Int64.Set.get_singleton is with
  | Some i when not no_alias -> this_naked_int64 i
  | _ ->
    if Int64.Set.is_empty is then bottom K.naked_int64
    else
      let of_kind : _ of_kind_naked_number = Int64 is in
      Naked_number (No_alias (Ok of_kind), K.Naked_number.Naked_int64)

let these_naked_nativeints0 ~no_alias (is : Targetint.Set.t) : t =
  match Targetint.Set.get_singleton is with
  | Some i when not no_alias -> this_naked_nativeint i
  | _ ->
    if Targetint.Set.is_empty is then bottom K.naked_nativeint
    else
      let of_kind : _ of_kind_naked_number = Nativeint is in
      Naked_number (No_alias (Ok of_kind), K.Naked_number.Naked_nativeint)

let this_naked_immediate_without_alias i =
  these_naked_immediates0 ~no_alias:true (Immediate.Set.singleton i)

let this_naked_float_without_alias f =
  these_naked_floats0 ~no_alias:true (Float.Set.singleton f)

let this_naked_int32_without_alias i =
  these_naked_int32s0 ~no_alias:true (Int32.Set.singleton i)

let this_naked_int64_without_alias i =
  these_naked_int64s0 ~no_alias:true (Int64.Set.singleton i)

let this_naked_nativeint_without_alias i =
  these_naked_nativeints0 ~no_alias:true (Targetint.Set.singleton i)

let these_naked_immediates is =
  these_naked_immediates0 ~no_alias:false is

let these_naked_floats fs =
  these_naked_floats0 ~no_alias:false fs

let these_naked_int32s is =
  these_naked_int32s0 ~no_alias:false is

let these_naked_int64s is =
  these_naked_int64s0 ~no_alias:false is

let these_naked_nativeints is =
  these_naked_nativeints0 ~no_alias:false is

let box_float (t : t) : t =
  match t with
  | Naked_number (ty_naked_float, K.Naked_number.Naked_float) ->
    Value (No_alias (Ok (Boxed_number (Boxed_float ty_naked_float))))
  | Value _
  | Naked_number _
  | Fabricated _ ->
    Misc.fatal_errorf "Type of wrong kind for [box_float]: %a"
      print t

let box_int32 (t : t) : t =
  match t with
  | Naked_number (ty_naked_int32, K.Naked_number.Naked_int32) ->
    Value (No_alias (Ok (Boxed_number (Boxed_int32 ty_naked_int32))))
  | Value _
  | Naked_number _
  | Fabricated _ ->
    Misc.fatal_errorf "Type of wrong kind for [box_int32]: %a"
      print t

let box_int64 (t : t) : t =
  match t with
  | Naked_number (ty_naked_int64, K.Naked_number.Naked_int64) ->
    Value (No_alias (Ok (Boxed_number (Boxed_int64 ty_naked_int64))))
  | Value _
  | Naked_number _
  | Fabricated _ ->
    Misc.fatal_errorf "Type of wrong kind for [box_int64]: %a"
      print t

let box_nativeint (t : t) : t =
  match t with
  | Naked_number (ty_naked_nativeint, K.Naked_number.Naked_nativeint) ->
    Value (No_alias (Ok (Boxed_number (Boxed_nativeint ty_naked_nativeint))))
  | Value _
  | Naked_number _
  | Fabricated _ ->
    Misc.fatal_errorf "Type of wrong kind for [box_nativeint]: %a"
      print t

let this_tagged_immediate imm : t =
  Value (Equals (Simple.const (Tagged_immediate imm)))

let these_tagged_immediates0 ~no_alias imms : t =
  match Immediate.Set.get_singleton imms with
  | Some imm when not no_alias -> this_tagged_immediate imm
  | _ ->
    if Immediate.Set.is_empty imms then bottom K.value
    else
      let immediates = Immediates.create imms in
      let blocks_and_tagged_immediates : blocks_and_tagged_immediates =
        { immediates = Known immediates;
          blocks = Known (Blocks.create_bottom ());
        }
      in
      Value (No_alias (Ok (
        Blocks_and_tagged_immediates blocks_and_tagged_immediates)))

let these_tagged_immediates imms =
  these_tagged_immediates0 ~no_alias:false imms

let this_tagged_immediate_without_alias imm =
  these_tagged_immediates0 ~no_alias:true (Immediate.Set.singleton imm)

let this_discriminant_as_ty_fabricated discr : ty_fabricated =
  Equals (Simple.discriminant discr)

let this_discriminant discr : t =
  Fabricated (this_discriminant_as_ty_fabricated discr)

(* CR mshinwell: Same code pattern as the tagged immediates cases above,
   factor out. *)
let these_discriminants0 ~no_alias discrs : t =
  match Discriminant.Set.get_singleton discrs with
  | Some discr when not no_alias -> this_discriminant discr
  | _ ->
    if Discriminant.Set.is_empty discrs then bottom K.fabricated
    else
      let discrs = Discriminants.create discrs in
      Fabricated (No_alias (Ok (Discriminants discrs)))

let these_discriminants discrs =
  these_discriminants0 ~no_alias:false discrs

let this_discriminant_without_alias discr : t =
  these_discriminants0 ~no_alias:true (Discriminant.Set.singleton discr)

let any_tagged_bool () =
  let bools =
    Immediate.Set.add Immediate.bool_false
      (Immediate.Set.add Immediate.bool_true Immediate.Set.empty)
  in
  these_tagged_immediates bools

let this_boxed_float f = box_float (this_naked_float f)
let this_boxed_int32 i = box_int32 (this_naked_int32 i)
let this_boxed_int64 i = box_int64 (this_naked_int64 i)
let this_boxed_nativeint i = box_nativeint (this_naked_nativeint i)

let these_boxed_floats fs = box_float (these_naked_floats fs)
let these_boxed_int32s is = box_int32 (these_naked_int32s is)
let these_boxed_int64s is = box_int64 (these_naked_int64s is)
let these_boxed_nativeints is = box_nativeint (these_naked_nativeints is)

let boxed_float_alias_to ~naked_float =
  box_float (Naked_number (Equals (Simple.var naked_float), Naked_float))

let boxed_int32_alias_to ~naked_int32 =
  box_int32 (Naked_number (Equals (Simple.var naked_int32), Naked_int32))

let boxed_int64_alias_to ~naked_int64 =
  box_int64 (Naked_number (Equals (Simple.var naked_int64), Naked_int64))

let boxed_nativeint_alias_to ~naked_nativeint =
  box_nativeint (Naked_number (Equals (Simple.var naked_nativeint),
    Naked_nativeint))

let kind (t : t) =
  match t with
  | Value _ -> K.value
  | Naked_number (_, K.Naked_number.Naked_immediate) -> K.naked_immediate
  | Naked_number (_, K.Naked_number.Naked_float) -> K.naked_float
  | Naked_number (_, K.Naked_number.Naked_int32) -> K.naked_int32
  | Naked_number (_, K.Naked_number.Naked_int64) -> K.naked_int64
  | Naked_number (_, K.Naked_number.Naked_nativeint) -> K.naked_nativeint
  | Fabricated _ -> K.fabricated

let immutable_block tag ~(fields : t list) : t =
  (* CR mshinwell: We should check the field kinds against the tag. *)
  match Targetint.OCaml.of_int_option (List.length fields) with
  | None ->
    Misc.fatal_error "Block too long for target"
  | Some _size ->
    let blocks = Blocks.create ~field_tys:fields (Closed tag) in
    let blocks_imms : blocks_and_tagged_immediates =
      { immediates = Known (Immediates.create_bottom ());
        blocks = Known blocks;
      }
    in
    Value (No_alias (Ok (Blocks_and_tagged_immediates blocks_imms)))

let immutable_block_of_values tag ~fields =
  let fields = List.map (fun ty_value : t -> Value ty_value) fields in
  immutable_block tag ~fields

let immutable_block_with_size_at_least ~n ~field_n_minus_one : t =
  let n = Targetint.OCaml.to_int n in
  let field_tys =
    List.init n (fun index ->
        if index < n - 1 then any_value ()
        else alias_type_of K.value (Simple.var field_n_minus_one))
  in
  let blocks = Blocks.create ~field_tys Open in
  let blocks_imms : blocks_and_tagged_immediates =
    { immediates = Known (Immediates.create_bottom ());
      blocks = Known blocks;
    }
  in
  Value (No_alias (Ok (Blocks_and_tagged_immediates blocks_imms)))

let this_immutable_string str =
  (* CR mshinwell: Use "length" not "size" for strings *)
  let size = Targetint.OCaml.of_int (String.length str) in
  let string_info =
    String_info.Set.singleton
      (String_info.create ~contents:(Contents str) ~size)
  in
  Value (No_alias (Ok (String string_info)))

let mutable_string ~size =
  let size = Targetint.OCaml.of_int size in
  let string_info =
    String_info.Set.singleton
      (String_info.create ~contents:Unknown_or_mutable ~size)
  in
  Value (No_alias (Ok (String string_info)))

let any_boxed_float () = box_float (any_naked_float ())
let any_boxed_int32 () = box_int32 (any_naked_int32 ())
let any_boxed_int64 () = box_int64 (any_naked_int64 ())
let any_boxed_nativeint () = box_nativeint (any_naked_nativeint ())

let bottom_like t = bottom (kind t)
let unknown_like t = unknown (kind t)

let create_inlinable_function_declaration function_decl rec_info
      : function_declaration =
  Inlinable {
    function_decl;
    rec_info;
  }

let create_non_inlinable_function_declaration ~param_arity ~result_arity
      ~recursive : function_declaration =
  Non_inlinable {
    param_arity;
    result_arity;
    recursive;
  }

let exactly_this_closure closure_id ~all_function_decls_in_set:function_decls
      ~all_closures_in_set:closure_types
      ~all_closure_vars_in_set:closure_var_types
      : t =
  let closure_types = Types_by_closure_id.create closure_types in
  let closures_entry : closures_entry =
    let closure_var_types =
      Types_by_var_within_closure.create
        (Var_within_closure.Map.map (fun ty_value : t -> Value ty_value)
          closure_var_types)
    in
    let function_decls =
      Closure_id.Map.map (fun func_decl -> Or_unknown.Known func_decl)
        function_decls
    in
    { function_decls;
      closure_types;
      closure_var_types;
    }
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
    Closures_entry_by_set_of_closures_contents.create_exactly_multiple
      set_of_closures_contents_to_closures_entry
  in
  let closures : closures =
    { by_closure_id;
    }
  in
  Value (No_alias (Ok (Closures closures)))

let at_least_the_closures_with_ids ~this_closure closure_ids_and_bindings : t =
  let closure_ids_and_types =
    Closure_id.Map.map (fun bound_to -> alias_type_of K.value bound_to)
      closure_ids_and_bindings
  in
  let function_decls =
    Closure_id.Map.map (fun _ -> Or_unknown.Unknown)
      closure_ids_and_bindings
  in
  let closure_types = Types_by_closure_id.create closure_ids_and_types in
  let closures_entry : closures_entry =
    { function_decls;
      closure_types;
      closure_var_types = Types_by_var_within_closure.bottom;
    }
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
    Closures_entry_by_set_of_closures_contents.create_at_least_multiple
      set_of_closures_contents_to_closures_entry
  in
  let closures : closures =
    { by_closure_id;
    }
  in
  Value (No_alias (Ok (Closures closures)))

let closure_with_at_least_this_closure_var closure_var ~closure_element_var
      : t =
  let closure_var_types =
    let closure_var_type =
      alias_type_of K.value (Simple.var closure_element_var)
    in
    Types_by_var_within_closure.create
      (Var_within_closure.Map.singleton closure_var closure_var_type)
  in
  let closures_entry : closures_entry =
    { function_decls = Closure_id.Map.empty;
      closure_types = Types_by_closure_id.bottom;
      closure_var_types;
    }
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
    Closures_entry_by_set_of_closures_contents.create_at_least_multiple
      set_of_closures_contents_to_closures_entry
  in
  let closures : closures =
    { by_closure_id;
    }
  in
  Value (No_alias (Ok (Closures closures)))

let array_of_length ~length =
  Value (No_alias (Ok (Array { length; })))

let type_for_const (const : Simple.Const.t) =
  match const with
  | Naked_immediate i -> this_naked_immediate i
  | Tagged_immediate i -> this_tagged_immediate i
  | Naked_float f -> this_naked_float f
  | Naked_int32 n -> this_naked_int32 n
  | Naked_int64 n -> this_naked_int64 n
  | Naked_nativeint n -> this_naked_nativeint n

let kind_for_const const = kind (type_for_const const)

let get_alias t =
  match t with
  | Value (Equals simple) -> Some simple
  | Value _ -> None
  | Naked_number (Equals simple, _) -> Some simple
  | Naked_number _ -> None
  | Fabricated (Equals simple) -> Some simple
  | Fabricated _ -> None

let get_alias_ty ty =
  match ty with
  | Equals simple -> Some simple
  | _ -> None

(* The following was Meet_or_join: *)

(* First specialise generic meet-and-join code to either meet or join. *)
module Of_kind_value = Meet_and_join_value.Make (E)
module Of_kind_naked_immediate = Meet_and_join_naked_immediate.Make (E)
module Of_kind_naked_float = Meet_and_join_naked_float.Make (E)
module Of_kind_naked_int32 = Meet_and_join_naked_int32.Make (E)
module Of_kind_naked_int64 = Meet_and_join_naked_int64.Make (E)
module Of_kind_naked_nativeint = Meet_and_join_naked_nativeint.Make (E)
module Of_kind_fabricated = Meet_and_join_fabricated.Make (E)

(* Next lift the meet or join operations from "of_kind_..." to "ty_...". *)
module Value = KI.Make (E) (Of_kind_value)
module Naked_immediate = KI.Make (E) (Of_kind_naked_immediate)
module Naked_float = KI.Make (E) (Of_kind_naked_float)
module Naked_int32 = KI.Make (E) (Of_kind_naked_int32)
module Naked_int64 = KI.Make (E) (Of_kind_naked_int64)
module Naked_nativeint = KI.Make (E) (Of_kind_naked_nativeint)
module Fabricated = KI.Make (E) (Of_kind_fabricated)

(* This function then lifts the meet or join operation from "ty_..." to
   Flambda types. *)
let meet_or_join ?bound_name env (t1 : T.t) (t2 : T.t) : T.t * _ =
  match t1, t2 with
  | Value ty1, Value ty2 ->
    begin match Value.meet_or_join_ty ?bound_name env ty1 ty2 with
    | Ok (ty, env_extension) -> Value ty, env_extension
    | Bottom -> Basic_type_ops.bottom K.value, TEE.empty ()
    end
  | Naked_number (ty1, Naked_immediate),
      Naked_number (ty2, Naked_immediate) ->
    begin match Naked_immediate.meet_or_join_ty ?bound_name env ty1 ty2 with
    | Ok (ty, env_extension) ->
      Naked_number (ty, Naked_immediate), env_extension
    | Bottom -> Basic_type_ops.bottom K.naked_immediate, TEE.empty ()
    end
  | Naked_number (ty1, Naked_float), Naked_number (ty2, Naked_float) ->
    begin match Naked_float.meet_or_join_ty ?bound_name env ty1 ty2 with
    | Ok (ty, env_extension) ->
      Naked_number (ty, Naked_float), env_extension
    | Bottom -> Basic_type_ops.bottom K.naked_float, TEE.empty ()
    end
  | Naked_number (ty1, Naked_int32), Naked_number (ty2, Naked_int32) ->
    begin match Naked_int32.meet_or_join_ty ?bound_name env ty1 ty2 with
    | Ok (ty, env_extension) ->
      Naked_number (ty, Naked_int32), env_extension
    | Bottom -> Basic_type_ops.bottom K.naked_int32, TEE.empty ()
    end
  | Naked_number (ty1, Naked_int64), Naked_number (ty2, Naked_int64) ->
    begin match Naked_int64.meet_or_join_ty ?bound_name env ty1 ty2 with
    | Ok (ty, env_extension) ->
      Naked_number (ty, Naked_int64), env_extension
    | Bottom -> Basic_type_ops.bottom K.naked_int64, TEE.empty ()
    end
  | Naked_number (ty1, Naked_nativeint),
      Naked_number (ty2, Naked_nativeint) ->
    begin match Naked_nativeint.meet_or_join_ty ?bound_name env ty1 ty2 with
    | Ok (ty, env_extension) ->
      Naked_number (ty, Naked_nativeint), env_extension
    | Bottom -> Basic_type_ops.bottom K.naked_nativeint, TEE.empty ()
    end
  | Fabricated ty1, Fabricated ty2 ->
    begin match Fabricated.meet_or_join_ty ?bound_name env ty1 ty2 with
    | Ok (ty, env_extension) -> Fabricated ty, env_extension
    | Bottom -> Basic_type_ops.bottom K.fabricated, TEE.empty ()
    end
  | (Value _ | Naked_number _ | Fabricated _), _ ->
    Misc.fatal_errorf "Kind mismatch upon %s:@ %a@ versus@ %a"
      (E.name ())
      Type_printers.print t1
      Type_printers.print t2

(* ---- *)

module Meet = Meet_or_join.Make (Lattice_ops.For_meet)
module Join = Meet_or_join.Make (Lattice_ops.For_join)

let meet env t1 t2 =
let ty, ext =
  Meet.meet_or_join env t1 t2
in
(*
Format.eprintf "meet:@ %a@ and@ %a@ =@ %a@ with extension:@ %a\n%!"
  Type_printers.print t1
  Type_printers.print t2
  Type_printers.print ty
  TEE.print ext;
*)
ty, ext

let join ?bound_name env t1 t2 =
  let env = Meet_env.create env in
  let joined, env_extension = Join.meet_or_join ?bound_name env t1 t2 in
  if not (TEE.is_empty env_extension) then begin
    Misc.fatal_errorf "Non-empty environment extension produced from a \
        [join] operation:@ %a"
      TEE.print env_extension
  end;
  joined
