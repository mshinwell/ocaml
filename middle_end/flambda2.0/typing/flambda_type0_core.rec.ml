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

open Flambda_types

(* CR mshinwell: We should have transformations and invariant checks to
   enforce that, when a type can be expressed just using [Equals] (e.g. to
   a tagged immediate [Simple]), then it should be.  In the tagged immediate
   case this would mean forbidding Blocks_and_tagged_immediates with only
   a single immediate.  Maybe Blocks_and_tagged_immediates should be made
   abstract *)

let force_to_kind_value (t : t) =
  match t with
  | Value ty_value -> ty_value
  | Naked_number _
  | Fabricated _ ->
    Misc.fatal_errorf "Type has wrong kind (expected [Value]):@ %a"
      Type_printers.print t

let force_to_kind_naked_immediate (t : t) : K.naked_immediate ty_naked_number =
  match t with
  | Naked_number (ty_naked_number, K.Naked_number.Naked_immediate) ->
    ty_naked_number
  | Naked_number _
  | Fabricated _
  | Value _ ->
    Misc.fatal_errorf
      "Type has wrong kind (expected [Naked_number Immediate]):@ %a"
      Type_printers.print t

let force_to_kind_naked_float (t : t) : K.naked_float ty_naked_number =
  match t with
  | Naked_number (ty_naked_number, K.Naked_number.Naked_float) ->
    ty_naked_number
  | Naked_number _
  | Fabricated _
  | Value _ ->
    Misc.fatal_errorf
      "Type has wrong kind (expected [Naked_number Float]):@ %a"
      Type_printers.print t

let force_to_kind_naked_int32 (t : t) : K.naked_int32 ty_naked_number =
  match t with
  | Naked_number (ty_naked_number, K.Naked_number.Naked_int32) ->
    ty_naked_number
  | Naked_number _
  | Fabricated _
  | Value _ ->
    Misc.fatal_errorf
      "Type has wrong kind (expected [Naked_number Int32]):@ %a"
      Type_printers.print t

let force_to_kind_naked_int64 (t : t) : K.naked_int64 ty_naked_number =
  match t with
  | Naked_number (ty_naked_number, K.Naked_number.Naked_int64) ->
    ty_naked_number
  | Naked_number _
  | Fabricated _
  | Value _ ->
    Misc.fatal_errorf
      "Type has wrong kind (expected [Naked_number Int64]):@ %a"
      Type_printers.print t

let force_to_kind_naked_nativeint (t : t) : K.naked_nativeint ty_naked_number =
  match t with
  | Naked_number (ty_naked_number, K.Naked_number.Naked_nativeint) ->
    ty_naked_number
  | Naked_number _
  | Fabricated _
  | Value _ ->
    Misc.fatal_errorf
      "Type has wrong kind (expected [Naked_number Nativeint]):@ %a"
      Type_printers.print t

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
      Type_printers.print t

let force_to_kind_fabricated (t : t) =
  match t with
  | Fabricated ty_fabricated -> ty_fabricated
  | Value _
  | Naked_number _ ->
    Misc.fatal_errorf "Type has wrong kind (expected [Fabricated]):@ %a"
      Type_printers.print t

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

let bottom_as_ty_fabricated () : ty_fabricated =
  No_alias Bottom

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

let any_fabricated_as_ty_fabricated () : ty_fabricated =
  No_alias Unknown

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

let these_naked_immediates ~no_alias (is : Immediate.Set.t) : t =
  match Immediate.Set.get_singleton is with
  | Some i when not no_alias -> this_naked_immediate i
  | _ ->
    if Immediate.Set.is_empty is then bottom K.naked_immediate
    else
      let of_kind : _ of_kind_naked_number = Immediate is in
      Naked_number (No_alias (Ok of_kind), K.Naked_number.Naked_immediate)

let these_naked_floats ~no_alias (fs : Float.Set.t) : t =
  match Float.Set.get_singleton fs with
  | Some f when not no_alias -> this_naked_float f
  | _ ->
    if Float.Set.is_empty fs then bottom K.naked_float
    else
      let of_kind : _ of_kind_naked_number = Float fs in
      Naked_number (No_alias (Ok of_kind), K.Naked_number.Naked_float)

let these_naked_int32s ~no_alias (is : Int32.Set.t) : t =
  match Int32.Set.get_singleton is with
  | Some i when not no_alias -> this_naked_int32 i
  | _ ->
    if Int32.Set.is_empty is then bottom K.naked_int32
    else
      let of_kind : _ of_kind_naked_number = Int32 is in
      Naked_number (No_alias (Ok of_kind), K.Naked_number.Naked_int32)

let these_naked_int64s ~no_alias (is : Int64.Set.t) : t =
  match Int64.Set.get_singleton is with
  | Some i when not no_alias -> this_naked_int64 i
  | _ ->
    if Int64.Set.is_empty is then bottom K.naked_int64
    else
      let of_kind : _ of_kind_naked_number = Int64 is in
      Naked_number (No_alias (Ok of_kind), K.Naked_number.Naked_int64)

let these_naked_nativeints ~no_alias (is : Targetint.Set.t) : t =
  match Targetint.Set.get_singleton is with
  | Some i when not no_alias -> this_naked_nativeint i
  | _ ->
    if Targetint.Set.is_empty is then bottom K.naked_nativeint
    else
      let of_kind : _ of_kind_naked_number = Nativeint is in
      Naked_number (No_alias (Ok of_kind), K.Naked_number.Naked_nativeint)

let this_naked_immediate_without_alias i =
  these_naked_immediates ~no_alias:true (Immediate.Set.singleton i)

let this_naked_float_without_alias f =
  these_naked_floats ~no_alias:true (Float.Set.singleton f)

let this_naked_int32_without_alias i =
  these_naked_int32s ~no_alias:true (Int32.Set.singleton i)

let this_naked_int64_without_alias i =
  these_naked_int64s ~no_alias:true (Int64.Set.singleton i)

let this_naked_nativeint_without_alias i =
  these_naked_nativeints ~no_alias:true (Targetint.Set.singleton i)

let box_float (t : t) : t =
  match t with
  | Naked_number (ty_naked_float, K.Naked_number.Naked_float) ->
    Value (No_alias (Ok (Boxed_number (Boxed_float ty_naked_float))))
  | Value _
  | Naked_number _
  | Fabricated _ ->
    Misc.fatal_errorf "Type of wrong kind for [box_float]: %a"
      Type_printers.print t

let box_int32 (t : t) : t =
  match t with
  | Naked_number (ty_naked_int32, K.Naked_number.Naked_int32) ->
    Value (No_alias (Ok (Boxed_number (Boxed_int32 ty_naked_int32))))
  | Value _
  | Naked_number _
  | Fabricated _ ->
    Misc.fatal_errorf "Type of wrong kind for [box_int32]: %a"
      Type_printers.print t

let box_int64 (t : t) : t =
  match t with
  | Naked_number (ty_naked_int64, K.Naked_number.Naked_int64) ->
    Value (No_alias (Ok (Boxed_number (Boxed_int64 ty_naked_int64))))
  | Value _
  | Naked_number _
  | Fabricated _ ->
    Misc.fatal_errorf "Type of wrong kind for [box_int64]: %a"
      Type_printers.print t

let box_nativeint (t : t) : t =
  match t with
  | Naked_number (ty_naked_nativeint, K.Naked_number.Naked_nativeint) ->
    Value (No_alias (Ok (Boxed_number (Boxed_nativeint ty_naked_nativeint))))
  | Value _
  | Naked_number _
  | Fabricated _ ->
    Misc.fatal_errorf "Type of wrong kind for [box_nativeint]: %a"
      Type_printers.print t

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

let boxed_int32_alias_to ~naked_int32 =
  box_int32 (Naked_number (Equals (Simple.var naked_int32), Naked_int32))

let boxed_int64_alias_to ~naked_int64 =
  box_int64 (Naked_number (Equals (Simple.var naked_int64), Naked_int64))

let this_discriminant_as_ty_fabricated discr : ty_fabricated =
  Equals (Simple.discriminant discr)

let this_discriminant discr : t =
  Fabricated (this_discriminant_as_ty_fabricated discr)

let this_discriminant_without_alias discr : t =
  let discriminants =
    Discriminants.create (Discriminant.Set.singleton discr)
  in
  Fabricated (No_alias (Ok (Discriminants discriminants)))

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

let set_of_closures_containing_exactly_this_closure closure_id function_decl
      closure_var_types ~bound_to : t =
  let closure_type = alias_type_of_as_ty_value (Name.var bound_to) in
  let closure_types =
    Types_by_closure_id.create
      (Closure_id.Map.singleton closure_id closure_type)
  in
  let closure_var_types =
    Types_by_var_within_closure.create
      (Var_within_closure.Map.map (fun ty_value : t -> Value ty_value)
        closure_var_types)
  in
  let closures_entry : closures_entry =
    { function_decl = Known function_decl;
      closure_types;
      closure_var_types;
    }
  in
  let by_closure_id =
    let set_of_closures_contents =
      Set_of_closures_contents.create (Closure_id.Set.singleton closure_id)
        (Var_within_closure.Map.keys closure_elements)
    in
    Closures_entry_by_set_of_closures_contents.create_exactly_multiple
      set_of_closures_contents closures_entry
  in
  let closures : closures =
    { by_closure_id;
    }
  in
  Value (No_alias (Ok (Closures closures)))

let set_of_closures_containing_exactly_these_closures ~closures : t =
  if Closure_id.Map.is_empty closures then bottom K.value
  else
    let all_closures = Closure_id.Map.keys closures in
    let by_closure_id = Types_by_closure_id.create closures in
    let set_of_closures_entry : set_of_closures_entry = { by_closure_id; } in
    let closures =
      Closure_ids.create
        (Closure_id_set.Map.singleton all_closures set_of_closures_entry)
        Closed
    in
    Fabricated (No_alias (Ok (Set_of_closures { closures; })))

let set_of_closures_containing_at_least_this_closure closure_id ~closure_var =
  let all_closures = Closure_id.Set.singleton closure_id in
  let by_closure_id =
    Types_by_closure_id.create
      (Closure_id.Map.singleton closure_id
        (alias_type_of K.value (Simple.var closure_var)))
  in
  let set_of_closures_entry : set_of_closures_entry = { by_closure_id; } in
  let closures =
    Closure_ids.create
      (Closure_id_set.Map.singleton all_closures set_of_closures_entry)
      Open
  in
  Fabricated (No_alias (Ok (Set_of_closures { closures; })))

let closure_containing_at_least_this_closure_var var_within_closure
      ~closure_element_var =
  let ty_value = alias_type_of_as_ty_value (Simple.var closure_element_var) in
  let closure_elements =
    Var_within_closure.Map.singleton var_within_closure (Value ty_value)
  in
  let closure_elements = Closure_elements.create closure_elements in
  let closures_entry : closures_entry =
    { function_decl = Unknown;
      closure_elements;
      set_of_closures = any_fabricated_as_ty_fabricated ();
    }
  in
  let by_closure_id =
    Closures_entry_by_closure_id.create_at_least_multiple
      (Var_within_closure_set.Map.singleton
        (Var_within_closure.Set.singleton var_within_closure)
        closures_entry)
  in
  let closures : closures =
    { by_closure_id;
    }
  in
  Value (No_alias (Ok (Closures closures)))

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

let rec apply_rec_info (t : Flambda_types.t) rec_info : t Or_bottom.t =
  match t with
  | Value (Equals simple) ->
    let simple = Simple.merge_rec_info simple ~newer_rec_info in
    Ok (Value (Equals simple))
  | Value (Type _) -> Misc.fatal_error "Not yet implemented"
  | Value (No_alias Unknown) -> Ok t
  | Value (No_alias Bottom) -> Bottom
  | Value (No_alias ty_value) ->
    begin match apply_rec_info_ty_value ty_value rec_info with
    | Ok ty_value -> Ok (Value (No_alias ty_value))
    | Bottom -> Bottom
    end
  | Naked_number _ | Fabricated _ -> Bottom

and apply_rec_info_ty_value (ty_value : Flambda_types.ty_value) rec_info
      : Flambda_types.ty_value Or_bottom.t =
  match ty_value with
  | Closures closures ->
    let by_closure_id =
      Closures_entry_by_set_of_closures_contents.map_closure_types by_closure_id
        ~f:(fun (closure_type : Flambda_types.t) ->
          apply_rec_info closure_type rec_info)
    in
    begin match by_closure_id with
    | Ok by_closure_id -> Ok (Closures { by_closure_id; })
    | Bottom -> Bottom
    end
  | Blocks_and_tagged_immediates _
  | Boxed_number _
  | String _ -> Bottom
