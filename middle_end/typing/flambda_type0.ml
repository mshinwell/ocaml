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

(* CR mshinwell: Add invariant check that one-case discriminants don't have
   any equations (for all cases with equations) *)

module Float_by_bit_pattern = Numbers.Float_by_bit_pattern
module Int32 = Numbers.Int32
module Int64 = Numbers.Int64

module K = Flambda_kind

module Outer_namespace = struct
  module Join_env = Join_env
  module Meet_and_join_value = Meet_and_join_value
  module Meet_and_join_naked_number = Meet_and_join_naked_number
  module Meet_and_join_fabricated = Meet_and_join_fabricated
  module Parameters = Parameters
  module Typing_env = Typing_env
  module Typing_env_extension = Typing_env_extension
  module Type_equality = Type_equality
  module Type_free_names = Type_free_names
  module Type_printers = Type_printers
end

module Make (Expr : Expr_intf.S) = struct
  module rec T : Flambda_type0_internal_intf.S
    with type expr := Expr.t
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension
    with module Join_env := Join_env
    with module Parameters := Parameters
    with module Blocks := Blocks
    with module Closure_elements := Closure_elements
  = struct
    include Flambda_type0_internal_intf.S_impl (Expr) (Typing_env)
      (Typing_env_extension) (Join_env) (Parameters) (Blocks)
      (Closure_elements)

    let print = Type_printers.print
    let print_with_cache = Type_printers.print_with_cache

    let free_names = Type_free_names.free_names

    let force_to_kind_value t =
      match t.descr with
      | Value ty_value -> ty_value
      | Naked_number _
      | Fabricated _ ->
        Misc.fatal_errorf "Type has wrong kind (expected [Value]):@ %a"
          print t

    let force_to_kind_naked_immediate (t : t)
          : Immediate.Set.t ty_naked_number =
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

    let force_to_kind_naked_nativeint (t : t)
          : Targetint.Set.t ty_naked_number =
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
        Misc.fatal_errorf "Type has wrong kind (expected \
            [Naked_number %a]):@ %a"
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

    (* CR mshinwell: Share with typing_env_extension.ml *)
    let empty_env_extension : env_extension =
      { first_definitions = [];
        at_or_after_cut_point = Scope_level.Map.empty;
        last_equations_rev = [];
        cse = Flambda_primitive.With_fixed_value.Map.empty;
      }

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

    let no_blocks : blocks =
      { known_tags_and_sizes = Tag_and_size.Map.empty;
        size_at_least_n = Targetint.OCaml.Map.empty;
      }

    let any_tagged_immediate () : t =
      { descr =
          Value (No_alias (Join [Blocks_and_tagged_immediates {
            immediates = Unknown;
            blocks = Known no_blocks;
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
                { env_extension = empty_env_extension;
                }
              in
              Immediate.Map.add imm case map)
            imms
            Immediate.Map.empty
        in
        let blocks_and_tagged_immediates : blocks_and_tagged_immediates =
          { immediates = Known immediates;
            blocks = Known no_blocks;
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
              { env_extension; })
            env_map
        in
        let blocks_and_tagged_immediates : blocks_and_tagged_immediates =
          { immediates = Known immediates;
            blocks = Known no_blocks;
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

    let these_discriminants_as_ty_fabricated discriminants_to_env_extension
          : ty_fabricated =
      let discriminant_map =
        Discriminant.Map.map (fun env_extension : discriminant_case ->
            { env_extension; })
          discriminants_to_env_extension
      in
      No_alias (Join [Discriminant discriminant_map])

    let these_discriminants discriminants_to_env_extension : t =
      { descr = Fabricated (
          these_discriminants_as_ty_fabricated discriminants_to_env_extension);
        phantom = None;
      }

    let this_discriminant_as_ty_fabricated discriminant =
      let discriminant_map =
        Discriminant.Map.singleton discriminant
          ({ env_extension = empty_env_extension; } : discriminant_case)
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

    let kind (t : t) =
      match t.phantom with
      | None ->
        begin match t.descr with
        | Value _ -> K.value ()
        | Naked_number (_, K.Naked_number.Naked_immediate) ->
          K.naked_immediate ()
        | Naked_number (_, K.Naked_number.Naked_float) -> K.naked_float ()
        | Naked_number (_, K.Naked_number.Naked_int32) -> K.naked_int32 ()
        | Naked_number (_, K.Naked_number.Naked_int64) -> K.naked_int64 ()
        | Naked_number (_, K.Naked_number.Naked_nativeint) ->
          K.naked_nativeint ()
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

    let create_parameters_from_types ts : parameters =
      let params =
        List.mapi (fun index t ->
            let kind = kind t in
            let var = Variable.create (Format.sprintf "param%d" index) in
            let parameter = Parameter.wrap var in
            Kinded_parameter.create parameter kind)
          ts
      in
      { params;
        env_extension = empty_env_extension;
      }

    let create_blocks ~tag ~size ~field_tys : blocks =
      let fields = create_parameters_from_types field_tys in
      let known_tags_and_sizes =
        Tag_and_size.Map.singleton (Tag_and_size.create tag size) fields
      in
      { known_tags_and_sizes;
        size_at_least_n = Targetint.OCaml.Map.empty;
      }

    let mutable_float_array ~size : t =
      match Targetint.OCaml.to_int_option size with
      | None ->
        (* CR mshinwell: Here and below, this should be a normal compilation
          error, not a fatal error. *)
        Misc.fatal_error "Mutable float array too long for host"
      | Some size ->
        let field_tys = List.init size (fun _index -> any_naked_float ()) in
        let size = Targetint.OCaml.of_int size in
        let blocks = create_blocks ~tag:Tag.double_array_tag ~size ~field_tys in
        let blocks_imms : blocks_and_tagged_immediates =
          { immediates = Known Immediate.Map.empty;
            blocks = Known blocks;
          }
        in
        { descr =
            Value (No_alias (Join [Blocks_and_tagged_immediates blocks_imms]));
        }

    let immutable_float_array fields : t =
      match Targetint.OCaml.of_int_option (Array.length fields) with
      | None ->
        Misc.fatal_error "Immutable float array too long for target"
      | Some size ->
        let field_tys =
          Array.map (fun ty_naked_number : t ->
              { descr =
                  Naked_number (ty_naked_number, K.Naked_number.Naked_float);
                phantom = None;
              })
            fields
        in
        let blocks =
          create_blocks ~tag:Tag.double_array_tag ~size
            ~field_tys:(Array.to_list field_tys)
        in
        let blocks_imms : blocks_and_tagged_immediates =
          { immediates = Known Immediate.Map.empty;
            blocks = Known blocks;
          }
        in
        { descr =
            Value (No_alias (Join [Blocks_and_tagged_immediates blocks_imms]));
        }

    let this_immutable_float_array fields : t =
      let make_field f : _ ty_naked_number =
        No_alias (Join [Float (Float_by_bit_pattern.Set.singleton f)])
      in
      let fields = Array.map make_field fields in
      immutable_float_array fields

    let block tag ~(fields : t mutable_or_immutable array) =
      (* CR mshinwell: We should check the field kinds against the tag. *)
      match Targetint.OCaml.of_int_option (Array.length fields) with
      | None ->
        Misc.fatal_error "Block too long for target"
      | Some size ->
        let field_tys =
          Array.map
            (fun (field : _ mutable_or_immutable) : t ->
              match field with
              | Immutable t -> t
              | Mutable -> any_value ())
            fields
        in
        let blocks =
          create_blocks ~tag ~size ~field_tys:(Array.to_list field_tys)
        in
        let blocks_imms : blocks_and_tagged_immediates =
          { immediates = Known Immediate.Map.empty;
            blocks = Known blocks;
          }
        in
        { descr =
            Value (No_alias (Join [Blocks_and_tagged_immediates blocks_imms]));
          phantom = None;
        }

    (* CR mshinwell: bad name *)
    let block_of_values tag ~(fields : ty_value mutable_or_immutable array) =
      let fields =
        Array.map
          (fun (field : _ mutable_or_immutable) : t mutable_or_immutable ->
            match field with
            | Immutable ty_value ->
              Immutable {
                descr = Value ty_value;
              }
            | Mutable -> Mutable)
          fields
      in
      block tag ~fields

    let block_of_unknown_values tag ~size =
      let fields =
        Array.init size (fun _index : _ mutable_or_immutable ->
          Immutable (any_value_as_ty_value ()))
      in
      block_of_values tag ~fields

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

    let create_inlinable_function_declaration ~is_classic_mode ~closure_origin
          ~continuation_param ~exn_continuation_param ~params ~body ~code_id
          ~results ~stub ~dbg ~inline ~specialise ~is_a_functor
          ~invariant_params ~size
          ~direct_call_surrogate ~my_closure : function_declarations =
      Inlinable [({
        closure_origin;
        continuation_param;
        exn_continuation_param;
        is_classic_mode;
        params;
        body;
        code_id;
        free_names_in_body = Expr.free_names body;
        results;
        stub;
        dbg;
        inline;
        specialise;
        is_a_functor;
        invariant_params;
        size;
        direct_call_surrogate;
        my_closure;
      } : inlinable_function_declaration)]

    let create_non_inlinable_function_declaration ~direct_call_surrogate
          : function_declarations =
      let decl : non_inlinable_function_declarations =
        { direct_call_surrogate;
        }
      in
      Non_inlinable decl

    let closure function_decls : ty_fabricated =
      No_alias (Join [Closure { function_decls; }])

    let closures_entry ~set_of_closures : closures_entry =
      { set_of_closures; }

    let closures ty by_closure_id : t =
      { descr = Value (No_alias (Join [Closures { ty; by_closure_id; }]));
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
      }

    let apply_name_permutation_unknown_or_join unknown_or_join perm =
      match unknown_or_join with
      | Unknown -> unknown_or_join
      | Join of_kind_foos ->
        let something_changed = ref false in
        let of_kind_foos =
          List.map (fun (of_kind_foo, existing_perm) ->
              let new_perm = Name_permutation.compose existing_perm new_perm in
              if not (new_perm == existing_perm) then begin
                something_changed := true
              end;
              of_kind_foo, new_perm)
            of_kind_foos
        in
        if not !something_changed then unknown_or_join
        else Join of_kind_foos

    let apply_name_permutation_ty ty perm =
      match ty with
      | No_alias unknown_or_join ->
        let unknown_or_join' =
          apply_name_permutation_unknown_or_join unknown_or_join perm
        in
        if unknown_or_join == unknown_or_join' then ty
        else No_alias unknown_or_join'
      | Type _ -> ty
      | Equals simple ->
        let simple' = Name_permutation.apply_simple perm simple in
        if simple == simple' then ty
        else Equals simple'

    let apply_name_permutation_descr descr perm =
      match descr with
      | Value ty_value ->
        let ty_value' = apply_name_permutation_ty ty_value perm in
        if ty_value == ty_value' then descr
        else Value ty_value'
      | Naked_number (ty_naked_number, kind) ->
        let ty_naked_number' = apply_name_permutation_ty ty_naked_number perm in
        if ty_naked_number == ty_naked_number' then descr
        else Naked_number (ty_naked_number', kind)
      | Fabricated ty_fabricated ->
        let ty_fabricated' = apply_name_permutation_ty ty_fabricated perm in
        if ty_fabricated == ty_fabricated' then descr
        else Fabricated ty_fabricated'

    let apply_name_permutation ({ descr; } as t) perm =
      let descr' = apply_name_permutation_descr descr perm in
      if descr == descr' then t
      else { descr = descr'; }

    let get_alias t =
      match t.descr with
      | Value (Equals simple) -> Some simple
      | Value _ -> None
      | Naked_number (Equals simple, _) -> Some simple
      | Naked_number _ -> None
      | Fabricated (Equals simple) -> Some simple
      | Fabricated _ -> None

  (*
    (* CR mshinwell: Add comment that this forms an equivalence relation *)
    let function_declarations_compatible
          (decl1 : function_declaration)
          (decl2 : function_declaration) =
      let check (params1 : parameters) (params2 : parameters) =
        let arity1 = Kinded_parameter.arity params1.params in
        let arity2 = Kinded_parameter.arity params2.params in
        Flambda_arity.equal arity1 arity2
          || (Flambda_arity.all_values arity1 && Flambda_arity.all_values arity2)
      in
      check decl1.ty.params decl2.ty.params
        && check decl1.ty.result decl2.ty.result
  *)

    let meet = Both_meet_and_join.meet
    let join = Both_meet_and_join.join
    let as_or_more_precise = Both_meet_and_join.as_or_more_precise
    let strictly_more_precise = Both_meet_and_join.strictly_more_precise
    let fast_equal = Type_equality.fast_equal
    let equal = Type_equality.equal
  end and Make_meet_or_join : functor
    (E : Either_meet_or_join_intf.S with module T := T)
      ->
      sig
        module Meet_and_join : Meet_and_join_intf.S_for_types
          with module T := T
      end
  = functor
    (E : Either_meet_or_join_intf.S with module T := T)
  -> struct
    (* CR mshinwell: Work out which properties we need to prove, e.g.
       Distributivity of meet over join:
         X n (X' u Y') == (X n X') u (X n Y'). *)
    module rec Make_meet_and_join : functor
      (S : Meet_and_join_spec_intf.S with module T := T)
        ->
        Meet_and_join_intf.S
          with module T := T
          with type of_kind_foo = S.of_kind_foo
      =
    functor (S : Meet_and_join_spec_intf.S with module T := T) ->
    struct
      type of_kind_foo = S.of_kind_foo

      let unknown_or_join_is_bottom (uj : _ unknown_or_join) =
        match uj with
        | Join [] -> true
        | Unknown | Join _ -> false

      let unknown_or_join_is_unknown (uj : _ unknown_or_join) =
        match uj with
        | Join _ -> false
        | Unknown -> true

      let rec join_on_unknown_or_join env
            (uj1 : S.of_kind_foo unknown_or_join)
            (uj2 : S.of_kind_foo unknown_or_join)
            : S.of_kind_foo unknown_or_join =
        if uj1 == uj2 then uj1
        else
          match uj1, uj2 with
          | Unknown, _ | _, Unknown -> Unknown
          | Join [], Join [] -> Join []
          | Join of_kind_foos1, Join of_kind_foos2 ->
            (* We rely on the invariant in flambda_type0_intf.ml.
               Everything in [of_kind_foos1] is mutually incompatible with each
               other; likewise in [of_kind_foos2]. *)
            let of_kind_foos =
              List.fold_left (fun of_kind_foos (of_kind_foo, perm) ->
                  (* [of_kind_foo] can be compatible with at most one of the
                     elements of [of_kind_foos]. *)
                  let found_one = ref false in
                  let joined =
                    List.map (fun (of_kind_foo', perm') ->
                        let join =
                          (* N.B. If we are here, [S.meet_or_join_of_kind_foo]
                             must be a "join" operation. *)
                          S.meet_or_join_of_kind_foo env perm perm'
                            of_kind_foo of_kind_foo'
                        in
                        match join with
                        | Ok (of_kind_foo, _env_extension) ->
                          if !found_one then begin
                            (* CR mshinwell: Add detail showing what was
                               wrong. *)
                            Misc.fatal_errorf "Invariant broken for [Join]"
                          end;
                          found_one := true;
                          of_kind_foo, Name_permutation.create ()
                        | Absorbing -> of_kind_foo', perm')
                      of_kind_foos
                  in
                  if not !found_one then (of_kind_foo, perm) :: of_kind_foos
                  else joined)
                of_kind_foos2
                of_kind_foos1
            in
            Join of_kind_foos

      and join_ty env
            (or_alias1 : S.of_kind_foo ty) (or_alias2 : S.of_kind_foo ty)
            : S.of_kind_foo ty =
        if Join_env.fast_check_extensions_same_both_sides env
          && or_alias1 == or_alias2
        then or_alias1
        else
          let unknown_or_join1, canonical_simple1 =
            Typing_env.resolve_aliases_and_squash_unresolved_names_on_ty'
              (Join_env.environment_on_left env)
              ~force_to_kind:S.force_to_kind
              ~print_ty:S.print_ty
              or_alias1
          in
          let unknown_or_join2, canonical_simple2 =
            Typing_env.resolve_aliases_and_squash_unresolved_names_on_ty'
              (Join_env.environment_on_right env)
              ~force_to_kind:S.force_to_kind
              ~print_ty:S.print_ty
              or_alias2
          in
          let all_aliases1 =
            match canonical_simple1 with
            | None -> Name.Set.empty
            | Some canonical_simple ->
              Typing_env.aliases_of_simple (Join_env.environment_on_left env)
                canonical_simple
          in
          let all_aliases2 =
            match canonical_simple2 with
            | None -> Name.Set.empty
            | Some canonical_simple ->
              Typing_env.aliases_of_simple (Join_env.environment_on_right env)
                canonical_simple
          in
          let all_aliases = Name.Set.inter all_aliases1 all_aliases2 in
          let alias_both_sides = Name.Set.choose_opt all_aliases in
          match alias_both_sides with
          | Some name -> Equals (Simple.name name)
          | None ->
            let alias1 = Name.Set.choose_opt all_aliases1 in
            let alias2 = Name.Set.choose_opt all_aliases2 in
            match alias1, alias2 with
            | Some name1, _ when unknown_or_join_is_bottom unknown_or_join2 ->
              Equals (Simple.name name1)
            | _, Some name2 when unknown_or_join_is_bottom unknown_or_join1 ->
              Equals (Simple.name name2)
            | None, None ->
              let unknown_or_join =
                join_on_unknown_or_join env perm1 perm2
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
                join_on_unknown_or_join env perm1 perm2
                  unknown_or_join1 unknown_or_join2
              in
              No_alias unknown_or_join

      let rec meet_on_unknown_or_join env perm1 perm2
            (ou1 : S.of_kind_foo unknown_or_join)
            (ou2 : S.of_kind_foo unknown_or_join)
            : S.of_kind_foo unknown_or_join * env_extension =
        if ou1 == ou2 then
          ou1, Typing_env_extension.empty
        else
          match ou1, ou2 with
          | Unknown, ou2 -> ou2, Typing_env_extension.empty
          | ou1, Unknown -> ou1, Typing_env_extension.empty
          | Join of_kind_foos1, Join of_kind_foos2 ->
            let of_kind_foos, env_extension_from_meet =
              List.fold_left
                (fun (of_kind_foos, env_extension_from_meet) of_kind_foo ->
                  let new_env_extension_from_meet =
                    ref (Typing_env_extension.empty)
                  in
                  let of_kind_foos =
                    Misc.Stdlib.List.filter_map (fun of_kind_foo' ->
                        let meet =
                          let env = Join_env.create env in
                          S.meet_or_join_of_kind_foo env perm1 perm2
                            of_kind_foo of_kind_foo'
                        in
                        match meet with
                        | Ok (of_kind_foo, new_env_extension_from_meet') ->
                          new_env_extension_from_meet :=
                            Typing_env_extension.meet env
                              new_env_extension_from_meet'
                                !new_env_extension_from_meet;
                          Some of_kind_foo
                        | Absorbing -> None)
                      of_kind_foos
                  in
                  let env_extension_from_meet =
                    Typing_env_extension.meet env
                      env_extension_from_meet !new_env_extension_from_meet;
                  in
                  of_kind_foos, env_extension_from_meet)
                (of_kind_foos2, Typing_env_extension.empty)
                of_kind_foos1
            in
            let same_as input_of_kind_foos =
              List.compare_lengths input_of_kind_foos of_kind_foos = 0
                && List.for_all2 (fun input_of_kind_foo of_kind_foo ->
                       input_of_kind_foo == of_kind_foo)
                     input_of_kind_foos of_kind_foos
            in
            if same_as of_kind_foos1 then ou1, env_extension_from_meet
            else if same_as of_kind_foos2 then ou2, env_extension_from_meet
            else Join of_kind_foos, env_extension_from_meet

      and meet_ty env perm1 perm2
            (or_alias1 : S.of_kind_foo ty)
            (or_alias2 : S.of_kind_foo ty)
            : S.of_kind_foo ty * env_extension =
        if or_alias1 == or_alias2 then begin
          or_alias1, Typing_env_extension.empty
        end else begin
          let unknown_or_join1, canonical_simple1 =
            Typing_env.resolve_aliases_and_squash_unresolved_names_on_ty' env
              ~force_to_kind:S.force_to_kind
              ~print_ty:S.print_ty
              or_alias1
          in
          let unknown_or_join2, canonical_simple2 =
            Typing_env.resolve_aliases_and_squash_unresolved_names_on_ty' env
              ~force_to_kind:S.force_to_kind
              ~print_ty:S.print_ty
              or_alias2
          in
          let add_equation_if_on_a_name env_extension (simple : Simple.t) ty =
            match simple with
            | Name name ->
              Typing_env_extension.add_equation env_extension name ty
            | Const _ | Discriminant _ -> env_extension
          in
          match canonical_simple1, canonical_simple2 with
          | Some simple1, Some simple2 when Simple.equal simple1 simple2 ->
            Equals simple1, Typing_env_extension.empty
          | Some simple1, _ when unknown_or_join_is_unknown unknown_or_join2 ->
            Equals simple1, Typing_env_extension.empty
          | _, Some simple2 when unknown_or_join_is_unknown unknown_or_join1 ->
            Equals simple2, Typing_env_extension.empty
          | Some simple1, Some simple2 ->
            let meet_unknown_or_join, env_extension_from_meet =
              meet_on_unknown_or_join env perm1 perm2
                unknown_or_join1 unknown_or_join2
            in
            let meet_ty = S.to_type (No_alias meet_unknown_or_join) in
            let env_extension_from_meet =
              add_equation_if_on_a_name env_extension_from_meet
                simple1 meet_ty
            in
            let env_extension_from_meet =
              add_equation_if_on_a_name env_extension_from_meet
                simple2 (S.to_type (Equals simple1))
            in
            Equals simple1, env_extension_from_meet
          | Some simple1, None ->
            let meet_unknown_or_join, env_extension_from_meet =
              meet_on_unknown_or_join env perm1 perm2
                unknown_or_join1 unknown_or_join2
            in
            let meet_ty = S.to_type (No_alias meet_unknown_or_join) in
            let env_extension_from_meet =
              add_equation_if_on_a_name env_extension_from_meet
                simple1 meet_ty
            in
            Equals simple1, env_extension_from_meet
          | None, Some simple2 ->
            let meet_unknown_or_join, env_extension_from_meet =
              meet_on_unknown_or_join env perm1 perm2
                unknown_or_join1 unknown_or_join2
            in
            let meet_ty = S.to_type (No_alias meet_unknown_or_join) in
            let env_extension_from_meet =
              add_equation_if_on_a_name env_extension_from_meet
                simple2 meet_ty
            in
            Equals simple2, env_extension_from_meet
          | None, None ->
            let unknown_or_join, env_extension_from_meet =
              meet_on_unknown_or_join env perm1 perm2
                unknown_or_join1 unknown_or_join2
            in
            if unknown_or_join == unknown_or_join1 then begin
              assert (match or_alias1 with No_alias _ -> true | _ -> false);
              or_alias1, env_extension_from_meet
            end else if unknown_or_join == unknown_or_join2 then begin
              assert (match or_alias2 with No_alias _ -> true | _ -> false);
              or_alias2, env_extension_from_meet
            end else begin
              No_alias unknown_or_join, env_extension_from_meet
            end
        end

      let meet_or_join_ty env or_alias1 or_alias2 =
        E.switch meet_ty join_ty env or_alias1 or_alias2
    end and Meet_and_join : sig
      include Meet_and_join_intf.S_for_types with module T := T
    end = struct
      let meet_or_join env t1 t2 : t * env_extension =
        if Join_env.fast_check_extensions_same_both_sides env
          && Type_equality.fast_equal t1 t2
        then t1, Typing_env_extension.empty
        else begin
          Join_env.invariant env;
          ensure_phantomness_matches t1 t2
            (Printf.sprintf "kind mismatch upon %s" E.name);
          let descr, equations =
            match t1.descr, t2.descr with
            | Value ty_value1, Value ty_value2 ->
              let ty_value, equations =
                Meet_and_join_value.meet_or_join_ty env perm1 perm2
                  ty_value1 ty_value2
              in
              if ty_value == ty_value1 then t1.descr, equations
              else if ty_value == ty_value2 then t2.descr, equations
              else Value ty_value, equations
            | Naked_number (ty_naked_number1, kind1),
                Naked_number (ty_naked_number2, kind2) ->
              let module N = K.Naked_number in
              begin match kind1, kind2 with
              | N.Naked_immediate, N.Naked_immediate ->
                let ty_naked_number, equations =
                  Meet_and_join_naked_immediate.meet_or_join_ty env perm1 perm2
                    ty_naked_number1 ty_naked_number2
                in
                Naked_number (ty_naked_number, N.Naked_immediate), equations
              | N.Naked_float, N.Naked_float ->
                let ty_naked_number, equations =
                  Meet_and_join_naked_float.meet_or_join_ty env perm1 perm2
                    ty_naked_number1 ty_naked_number2
                in
                Naked_number (ty_naked_number, N.Naked_float), equations
              | N.Naked_int32, N.Naked_int32 ->
                let ty_naked_number, equations =
                  Meet_and_join_naked_int32.meet_or_join_ty env perm1 perm2
                    ty_naked_number1 ty_naked_number2
                in
                Naked_number (ty_naked_number, N.Naked_int32), equations
              | N.Naked_int64, N.Naked_int64 ->
                let ty_naked_number, equations =
                  Meet_and_join_naked_int64.meet_or_join_ty env perm1 perm2
                    ty_naked_number1 ty_naked_number2
                in
                Naked_number (ty_naked_number, N.Naked_int64), equations
              | N.Naked_nativeint, N.Naked_nativeint ->
                let ty_naked_number, equations =
                  Meet_and_join_naked_nativeint.meet_or_join_ty env perm1 perm2
                    ty_naked_number1 ty_naked_number2
                in
                Naked_number (ty_naked_number, N.Naked_nativeint), equations
              | _, _ ->
                Misc.fatal_errorf "Kind mismatch upon %s:@ %a@ versus@ %a"
                  E.name
                  print t1
                  print t2
              end
            | Fabricated ty_fabricated1, Fabricated ty_fabricated2 ->
              let ty_fabricated, equations =
                Meet_and_join_fabricated.meet_or_join_ty env perm1 perm2
                  ty_fabricated1 ty_fabricated2
              in
              if ty_fabricated == ty_fabricated1 then
                t1.descr, equations
              else if ty_fabricated == ty_fabricated2 then
                t2.descr, equations
              else
                Fabricated ty_fabricated, equations
            | (Value _ | Naked_number _ | Fabricated _), _ ->
              Misc.fatal_errorf "Kind mismatch upon %s:@ %a@ versus@ %a"
                E.name
                print t1
                print t2
          in
          let t =
            if t1.descr == descr then t1
            else if t2.descr == descr then t2
            else {
              descr;
            }
          in
          t, equations
        end
    end and Meet_and_join_value :
      Meet_and_join_intf.S
        with module T := T
        with type of_kind_foo = of_kind_value
      = Outer_namespace.Meet_and_join_value.Make (T1)
          (Make_meet_and_join) (Meet_and_join_naked_immediate)
          (Meet_and_join_naked_float) (Meet_and_join_naked_int32)
          (Meet_and_join_naked_int64) (Meet_and_join_naked_nativeint)
          (Meet_and_join_fabricated) (Both_meet_and_join)
          (Typing_env) (Typing_env_extension) (Join_env) (Parameters) (E)
    and Meet_and_join_naked_number : sig
      (* CR mshinwell: Deal with this signature somehow *)
      module Naked_immediate :
        Meet_and_join_intf.S
          with module T := T
          with type of_kind_foo = Immediate.Set.t of_kind_naked_number
      module Naked_float :
        Meet_and_join_intf.S
          with module T := T
          with type of_kind_foo =
            Numbers.Float_by_bit_pattern.Set.t of_kind_naked_number
      module Naked_int32 :
        Meet_and_join_intf.S
          with module T := T
          with type of_kind_foo = Numbers.Int32.Set.t of_kind_naked_number
      module Naked_int64 :
        Meet_and_join_intf.S
          with module T := T
          with type of_kind_foo = Numbers.Int64.Set.t of_kind_naked_number
      module Naked_nativeint :
        Meet_and_join_intf.S
          with module T := T
          with type of_kind_foo = Targetint.Set.t of_kind_naked_number
    end = Outer_namespace.Meet_and_join_naked_number.Make
      (T1) (Make_meet_and_join) (Typing_env) (Typing_env_extension) (E)
    and Meet_and_join_naked_immediate :
      Meet_and_join_intf.S
        with module T := T
        with type of_kind_foo = Immediate.Set.t of_kind_naked_number
      = Meet_and_join_naked_number.Naked_immediate
    and Meet_and_join_naked_float :
      (* CR mshinwell: See if we can abstract these naked number cases some
         more? *)
      Meet_and_join_intf.S
        with module T := T
        with type of_kind_foo = Float_by_bit_pattern.Set.t of_kind_naked_number
      = Meet_and_join_naked_number.Naked_float
    and Meet_and_join_naked_int32 :
      Meet_and_join_intf.S
        with module T := T
        with type of_kind_foo = Int32.Set.t of_kind_naked_number
      = Meet_and_join_naked_number.Naked_int32
    and Meet_and_join_naked_int64 :
      Meet_and_join_intf.S
        with module T := T
        with type of_kind_foo = Int64.Set.t of_kind_naked_number
      = Meet_and_join_naked_number.Naked_int64
    and Meet_and_join_naked_nativeint :
      Meet_and_join_intf.S
        with module T := T
        with type of_kind_foo = Targetint.Set.t of_kind_naked_number
      = Meet_and_join_naked_number.Naked_nativeint
    and Meet_and_join_fabricated :
      Meet_and_join_intf.S
        with module T := T
        with type of_kind_foo = of_kind_fabricated
      = Outer_namespace.Meet_and_join_fabricated.Make
          (T1) (Make_meet_and_join) (Meet_and_join_value) (Both_meet_and_join)
          (Typing_env) (Typing_env_extension) (Join_env) (E)
  end and Meet : sig
    module Meet_and_join : Meet_and_join_intf.S_for_types with module T := T
  end = Make_meet_or_join (For_meet)
  and Join : sig
    module Meet_and_join : Meet_and_join_intf.S_for_types with module T := T
  end = Make_meet_or_join (For_join)
  and Both_meet_and_join : Meet_and_join_intf.S_both with module T := T
    = struct
      module T = T

      let meet env t1 t2 =
        Meet.Meet_and_join.meet_or_join (Join_env.create env) t1 t2

      let join env t1 t2 =
        let join_ty, _env_extension =
          Join.Meet_and_join.meet_or_join env t1 t2
        in
        join_ty

      let as_or_more_precise env t1 ~than:t2 =
        if Type_equality.fast_equal t1 t2 then true
        else
          let meet_t, _env_extension = meet env t1 t2 in
          Type_equality.equal meet_t t1

      let strictly_more_precise env t1 ~than:t2 =
        if Type_equality.fast_equal t1 t2 then false
        else
          let meet_t, _env_extension = meet env t1 t2 in
          Type_equality.equal meet_t t1
            && not (Type_equality.equal meet_t t2)
    end
  and Typing_env :
    Typing_env_intf.S with module T := T
      = Outer_namespace.Typing_env.Make (T1)
          (Typing_env_extension) (Both_meet_and_join) (Type_equality)
  and Typing_env_extension :
    Typing_env_extension_intf.S with module T := T
      = Outer_namespace.Typing_env_extension.Make (T1)
          (Typing_env) (Both_meet_and_join) (Type_equality) (Join_env)
  and Type_equality :
    Type_equality_intf.S with module T := T
      = Outer_namespace.Type_equality.Make (T1) (Expr) (Typing_env_extension)
  and Join_env :
    Join_env_intf.S with module T := T
    = Outer_namespace.Join_env.Make (T1) (Typing_env) (Typing_env_extension)
  and Parameters :
    Parameters_intf.S with module T := T
    = Outer_namespace.Parameters.Make (T1) (Typing_env) (Typing_env_extension)
        (Both_meet_and_join) (Join_env)
  and For_meet : Either_meet_or_join_intf.S with module T := T
  = struct
    let name = "meet"

    type meet_or_join = Meet | Join

    let op : meet_or_join = Meet
    let (_ : meet_or_join) = Join

    let unknown_is_identity = true
    let unknown_is_absorbing = false

    (* CR mshinwell: Write functors to generate these patterns *)
    module Immediate = struct
      module Set = struct
        type t = Immediate.Set.t
        let union_or_inter = Immediate.Set.inter
      end

      module Map = struct
        type 'a t = 'a Immediate.Map.t

        let union_or_inter = Immediate.Map.inter
      end
    end

    module Float_by_bit_pattern = struct
      module Set = struct
        type t = Float_by_bit_pattern.Set.t
        let union_or_inter = Float_by_bit_pattern.Set.inter
      end
    end

    module Int32 = struct
      module Set = struct
        type t = Int32.Set.t
        let union_or_inter = Int32.Set.inter
      end
    end

    module Int64 = struct
      module Set = struct
        type t = Int64.Set.t
        let union_or_inter = Int64.Set.inter
      end
    end

    module Targetint = struct
      module Set = struct
        type t = Targetint.Set.t
        let union_or_inter = Targetint.Set.inter
      end

      module OCaml = struct
        module Map = struct
          type 'a t = 'a Targetint.OCaml.Map.t

          let union_or_inter_both ~in_left_only:_ ~in_right_only:_ ~in_both
                t1 t2 =
            Targetint.OCaml.Map.inter in_both t1 t2
        end
      end
    end

    module Closure_id = struct
      module Map = struct
        type 'a t = 'a Closure_id.Map.t

        let union_or_inter = Closure_id.Map.inter

        (* CR mshinwell: implement these *)
        let union_or_inter_and_left _f _t1 _t2 = assert false
      end
    end

    module Var_within_closure = struct
      module Map = struct
        type 'a t = 'a Var_within_closure.Map.t

        let union_or_inter = Var_within_closure.Map.inter

        let union_or_inter_and_left _f _t1 _t2 = assert false
      end
    end

    module Tag = struct
      module Map = struct
        type 'a t = 'a Tag.Map.t

        let union_or_inter_both ~in_left_only:_ ~in_right_only:_ ~in_both
              t1 t2 =
          Tag.Map.inter in_both t1 t2
      end
    end

    module Discriminant = struct
      module Map = struct
        type 'a t = 'a Discriminant.Map.t

        let union_or_inter_both ~in_left_only:_ ~in_right_only:_ ~in_both
              t1 t2 =
          Discriminant.Map.inter in_both t1 t2
      end
    end

    let switch meet _join join_env thing1 thing2 =
      meet (Join_env.central_environment join_env) thing1 thing2

    let switch' meet _join join_env thing1 thing2 =
      meet (Join_env.central_environment join_env) thing1 thing2
  end and For_join : Either_meet_or_join_intf.S with module T := T
  = struct
    let name = "join"

    type meet_or_join = Meet | Join

    let op : meet_or_join = Join
    let (_ : meet_or_join) = Meet

    let unknown_is_identity = false
    let unknown_is_absorbing = true

    module Immediate = struct
      module Set = struct
        type t = Immediate.Set.t
        let union_or_inter = Immediate.Set.union
      end

      module Map = struct
        type 'a t = 'a Immediate.Map.t

        let union_or_inter = Immediate.Map.union
      end
    end

    module Float_by_bit_pattern = struct
      module Set = struct
        type t = Float_by_bit_pattern.Set.t
        let union_or_inter = Float_by_bit_pattern.Set.union
      end
    end

    module Int32 = struct
      module Set = struct
        type t = Int32.Set.t
        let union_or_inter = Int32.Set.union
      end
    end

    module Int64 = struct
      module Set = struct
        type t = Int64.Set.t
        let union_or_inter = Int64.Set.union
      end
    end

    module Targetint = struct
      module Set = struct
        type t = Targetint.Set.t
        let union_or_inter = Targetint.Set.union
      end

      module OCaml = struct
        module Map = struct
          type 'a t = 'a Targetint.OCaml.Map.t

          let union_or_inter_both = Targetint.OCaml.Map.union_both
        end
      end
    end

    module Closure_id = struct
      module Map = struct
        type 'a t = 'a Closure_id.Map.t

        let union_or_inter = Closure_id.Map.union

        let union_or_inter_and_left f t1 t2 =
          Closure_id.Map.union f t1 t2
      end
    end

    module Var_within_closure = struct
      module Map = struct
        type 'a t = 'a Var_within_closure.Map.t

        let union_or_inter = Var_within_closure.Map.union

        let union_or_inter_and_left f t1 t2 =
          Var_within_closure.Map.union f t1 t2
      end
    end

    module Tag = struct
      module Map = struct
        type 'a t = 'a Tag.Map.t

        let union_or_inter_both = Tag.Map.union_both
      end
    end

    module Discriminant = struct
      module Map = struct
        type 'a t = 'a Discriminant.Map.t

        let union_or_inter_both = Discriminant.Map.union_both
      end
    end

    let switch _meet join join_env thing1 thing2 =
      join join_env thing1 thing2, Typing_env_extension.empty

    let switch' _meet join join_env thing1 thing2 =
      join join_env thing1 thing2
  end
  and Type_printers : Type_printers_intf.S with module T := T
    = Outer_namespace.Type_printers.Make (T)
  and Type_free_names : Type_free_names_intf.S with module T := T
    = Outer_namespace.Type_free_names.Make (T)
end
