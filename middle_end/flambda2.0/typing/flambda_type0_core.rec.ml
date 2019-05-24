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

let force_to_kind_value t =
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

let force_to_kind_fabricated t =
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

let of_ty_value ty_value : t =
  Value ty_value

let of_ty_naked_number (type n) (ty_naked_number : n ty_naked_number)
      (kind : n K.Naked_number.t) : t =
  Naked_number (ty_naked_number, kind)

let of_ty_fabricated ty_fabricated : t =
  Fabricated ty_fabricated

(* CR-someday mshinwell: Functions such as [alias] and [bottom] could be
   simplified if [K.t] were a GADT. *)

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

let alias_type (type k) (kind : k K.t) export_id : t =
  match kind with
  | Value ->
    Value (Type export_id)
  | Naked_number naked_number_witness ->
    Naked_number (Type export_id, naked_number_witness)
  | Fabricated ->
    Fabricated (Type export_id)

let bottom_as_ty_value () : ty_value =
  No_alias Bottom

let bottom_as_ty_fabricated () : ty_fabricated =
  No_alias Bottom

let bottom (type k) (kind : k K.t) : t =
  match kind with
  | Value ->
    Value (No_alias Bottom)
  | Naked_number naked_number_witness ->
    Naked_number (No_alias Bottom, naked_number_witness)
  | Fabricated ->
    Fabricated (No_alias Bottom)

let any_value_as_ty_value () : ty_value =
  No_alias Unknown

let any_fabricated_as_ty_fabricated () : ty_fabricated =
  No_alias Unknown

let any_naked_float_as_ty_naked_float () : _ ty_naked_number =
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

let unknown (kind : K.t) =
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

let these_naked_immediates (is : Immediate.Set.t) : t =
  let of_kind : _ of_kind_naked_number = Immediate is in
  Naked_number (No_alias (Ok of_kind), K.Naked_number.Naked_immediate)

let these_naked_floats (is : Float.Set.t) : t =
  let of_kind : _ of_kind_naked_number = Float is in
  Naked_number (No_alias (Ok of_kind), K.Naked_number.Naked_float)

let these_naked_int32s (is : Int32.Set.t) : t =
  let of_kind : _ of_kind_naked_number = Int32 is in
  Naked_number (No_alias (Ok of_kind), K.Naked_number.Naked_int32)

let these_naked_int64s (is : Int64.Set.t) : t =
  let of_kind : _ of_kind_naked_number = Int64 is in
  Naked_number (No_alias (Ok of_kind), K.Naked_number.Naked_int64)

let these_naked_nativeints (is : Targetint.Set.t) : t =
  let of_kind : _ of_kind_naked_number = Nativeint is in
  Naked_number (No_alias (Ok of_kind), K.Naked_number.Naked_nativeint)

let this_naked_immediate i =
  these_naked_immediates (Immediate.Set.singleton i)

let this_naked_float f =
  these_naked_floats (Float.Set.singleton f)

let this_naked_float_as_ty_naked_float f =
  let fs = Float.Set.singleton f in
  let of_kind : _ of_kind_naked_number = Float fs in
  No_alias (Ok of_kind)

let this_naked_int32 i =
  these_naked_int32s (Int32.Set.singleton i)

let this_naked_int64 i =
  these_naked_int64s (Int64.Set.singleton i)

let this_naked_nativeint i =
  these_naked_nativeints (Targetint.Set.singleton i)

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

let these_tagged_immediates imms : t =
  if Immediate.Set.is_empty imms then
    bottom K.value
  else
    let immediates = Immediates.create imms in
    let blocks_and_tagged_immediates : blocks_and_tagged_immediates =
      { immediates = Known immediates;
        blocks = Known (Blocks.create_bottom ());
      }
    in
    Value (No_alias (Ok (
      Blocks_and_tagged_immediates blocks_and_tagged_immediates)))

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

let these_discriminants_as_ty_fabricated discriminants : ty_fabricated =
  let discriminants = Discriminants.create discriminants in
  No_alias (Ok (Discriminants discriminants))

let these_discriminants discriminants : t =
  Fabricated (these_discriminants_as_ty_fabricated discriminants)

let this_discriminant_as_ty_fabricated discriminant : ty_fabricated =
  let discriminants =
    Discriminants.create (Discriminant.Set.singleton discriminant)
  in
  No_alias (Ok (Discriminants discriminants))

let this_discriminant discriminant : t =
  Fabricated (this_discriminant_as_ty_fabricated discriminant)

let any_discriminant_as_ty_fabricated () : ty_fabricated =
  No_alias Unknown

let this_immutable_string_as_ty_value str : ty_value =
  let str =
    String_info.create ~contents:(Contents str)
      ~size:(Targetint.OCaml.of_int (String.length str))
  in
  let str = String_info.Set.singleton str in
  No_alias (Ok (String str))

let this_immutable_string str : t =
  Value (this_immutable_string_as_ty_value str)

let immutable_string_as_ty_value ~size : ty_value =
  let str = String_info.create ~contents:Unknown_or_mutable ~size in
  let str = String_info.Set.singleton str in
  No_alias (Ok (String str))

let immutable_string ~size : t =
  Value (immutable_string_as_ty_value ~size)

let mutable_string ~size : t =
  let str = String_info.create ~contents:Unknown_or_mutable ~size in
  let str = String_info.Set.singleton str in
  Value (No_alias (Ok (String str)))

let kind (t : t) =
  match t with
  | Value _ -> K.value
  | Naked_number (_, K.Naked_number.Naked_immediate) -> K.naked_immediate
  | Naked_number (_, K.Naked_number.Naked_float) -> K.naked_float
  | Naked_number (_, K.Naked_number.Naked_int32) -> K.naked_int32
  | Naked_number (_, K.Naked_number.Naked_int64) -> K.naked_int64
  | Naked_number (_, K.Naked_number.Naked_nativeint) -> K.naked_nativeint
  | Fabricated _ -> K.fabricated

let block tag ~(fields : t list) ~field_kind:_ =
  (* CR mshinwell: We should check the field kinds against the tag; and
     check the kinds of the [fields] against [field_kind]. *)
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

(* CR mshinwell: bad name *)
let block_of_values tag ~(fields : ty_value list) =
  block tag ~fields:(List.map (fun field : t -> Value field) fields)
    ~field_kind:K.value

(* CR mshinwell: Decide what to do about [field_kind] *)
let block_with_size_at_least ~n ~field_n_minus_one ~field_kind:_ =
  let type_of_field_n_minus_one =
    alias_type_of K.value (Simple.var field_n_minus_one)
  in
  let field_tys =
    List.init n (fun index ->
      if index = n - 1 then type_of_field_n_minus_one
      else any_value ())
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

let check_of_kind t (expected_kind : K.t) =
  let actual_kind = kind t in
  if not (K.equal actual_kind expected_kind) then begin
    Misc.fatal_errorf "Type has wrong kind: have %a but expected %a"
      K.print actual_kind
      K.print expected_kind
  end

let bottom_like t = bottom (kind t)
let unknown_like t = unknown (kind t)

let create_inlinable_function_declaration function_decl : function_declaration =
  Inlinable {
    function_decl;
  }

let create_non_inlinable_function_declaration () : function_declaration =
  Non_inlinable

let closure closure_id function_decl closure_elements ~set_of_closures =
  let closure_elements' =
    let closure_elements =
      Var_within_closure.Map.map (fun ty_value : t -> Value ty_value)
        closure_elements
    in
    Closure_elements.create closure_elements
  in
  let closures_entry : closures_entry =
    { function_decl;
      closure_elements = closure_elements';
      set_of_closures;
    }
  in
  let by_closure_id =
    Closures_entry_by_closure_id.create_exactly_multiple
      (Closure_id_and_var_within_closure_set.Map.singleton
        (closure_id, Var_within_closure.Map.keys closure_elements)
        closures_entry)
  in
  let closures : closures =
    { by_closure_id;
    }
  in
  Value (No_alias (Ok (Closures closures)))

let closure_containing_at_least var_within_closure =
  let ty_value = any_value_as_ty_value () in
  let closure_elements =
    Var_within_closure.Map.singleton var_within_closure (Value ty_value)
  in
  let closure_elements = Closure_elements.create closure_elements in
  let closures_entry : closures_entry =
    { function_decl = Non_inlinable;
      closure_elements;
      set_of_closures = any_fabricated_as_ty_fabricated ()
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

let set_of_closures ~closures =
  if Closure_id.Map.is_empty closures then bottom K.value
  else
    let all_closures = Closure_id.Map.keys closures in
    let by_closure_id = Types_by_closure_id.create closures in
    let set_of_closures_entry : set_of_closures_entry =
       { by_closure_id; }
    in
    let closures =
      Closure_ids.create
        (Closure_id_set.Map.singleton all_closures set_of_closures_entry)
        Closed
    in
    Fabricated (No_alias (Ok (Set_of_closures { closures; })))

let set_of_closures_containing_at_least closure_id =
  let by_closure_id =
    Types_by_closure_id.create
      (Closure_id.Map.singleton closure_id (any_value ()))
  in
  let set_of_closures_entry : set_of_closures_entry = { by_closure_id; } in
  let closure_id = Closure_id.Set.singleton closure_id in
  let closures =
    Closure_ids.create
      (Closure_id_set.Map.singleton closure_id set_of_closures_entry)
      Open
  in
  Fabricated (No_alias (Ok (Set_of_closures { closures; })))

let type_for_const (const : Simple.Const.t) =
  match const with
  | Naked_immediate i -> this_naked_immediate i
  | Tagged_immediate i -> this_tagged_immediate i
  | Naked_float f -> this_naked_float f
  | Naked_int32 n -> this_naked_int32 n
  | Naked_int64 n -> this_naked_int64 n
  | Naked_nativeint n -> this_naked_nativeint n

let get_alias t =
  match t with
  | Value (Equals simple) -> Some simple
  | Value _ -> None
  | Naked_number (Equals simple, _) -> Some simple
  | Naked_number _ -> None
  | Fabricated (Equals simple) -> Some simple
  | Fabricated _ -> None

module Set_of_closures_entry = struct
  type t = set_of_closures_entry

  let bottom () : t =
    { by_closure_id = Types_by_closure_id.bottom;
    }

  let print_with_cache ~cache ppf { by_closure_id; } =
    Format.fprintf ppf
      "@[<hov 1>(@\
        @[<hov 1>(by_closure_id@ %a)@])@]"
      (Types_by_closure_id.print_with_cache ~cache) by_closure_id

  let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let equal = Type_equality.equal_set_of_closures_entry

  let widen t ~to_match:_ = t  (* XXX Think about this *)
(*
  let meet = Api_meet_and_join.meet_set_of_closures_entry
  let join = Api_meet_and_join.join_set_of_closures_entry
*)
end

module Closures_entry = struct
  type t = closures_entry

  let bottom () : t =
    { function_decl = Non_inlinable;
      closure_elements = Closure_elements.bottom;
      set_of_closures = bottom_as_ty_fabricated ();
    }

  let print_with_cache ~cache ppf
        { function_decl; closure_elements; set_of_closures; } =
    Format.fprintf ppf
      "@[<hov 1>(@\
        @[<hov 1>(function_decl@ %a)@]@ \
        @[<hov 1>(closure_elements@ %a)@]@ \
        @[<hov 1>(set_of_closures@ %a)@])@]"
      (Type_printers.print_function_declaration_with_cache ~cache)
        function_decl
      (Closure_elements.print_with_cache ~cache) closure_elements
      (Type_printers.print_ty_fabricated_with_cache ~cache) set_of_closures

  let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let equal = Type_equality.equal_closures_entry

  let widen t ~to_match:_ = t  (* XXX Think about this *)

(*
  let meet = Api_meet_and_join.meet_closures_entry
  let join = Api_meet_and_join.join_closures_entry
*)
end
