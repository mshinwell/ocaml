(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module K = Flambda_kind

module Float_by_bit_pattern = Numbers.Float_by_bit_pattern
module Int32 = Numbers.Int32
module Int64 = Numbers.Int64

module Make (T : sig
  include Flambda_type0_internal_intf.S

  val print_ty_naked_number
     : Format.formatter
    -> 'a ty_naked_number
    -> unit

  val force_to_kind_naked_immediate
     : t
    -> Immediate.Set.t of_kind_naked_number ty

  val force_to_kind_naked_float
     : t
    -> Numbers.Float_by_bit_pattern.Set.t of_kind_naked_number ty

  val force_to_kind_naked_int32
     : t
    -> Numbers.Int32.Set.t of_kind_naked_number ty

  val force_to_kind_naked_int64
     : t
    -> Numbers.Int64.Set.t of_kind_naked_number ty

  val force_to_kind_naked_nativeint
     : t
    -> Targetint.Set.t of_kind_naked_number ty
end) (Make_meet_and_join : functor
    (S : sig
      include Meet_and_join_spec_intf.S
        with type flambda_type := T.flambda_type
        with type typing_environment := T.typing_environment
        with type env_extension := T.env_extension
        with type 'a ty := 'a T.ty
     end)
  -> sig
       include Meet_and_join_intf.S
         with type of_kind_foo := S.of_kind_foo
         with type typing_environment := T.typing_environment
         with type env_extension := T.env_extension
         with type 'a ty := 'a T.ty
    end) (Meet_and_join : sig
      include Meet_and_join_intf.S_for_types
        with type t_in_context := T.t_in_context
        with type env_extension := T.env_extension
        with type flambda_type := T.flambda_type
    end) (Typing_env0 : sig
      include Typing_env0_intf.S
        with type typing_environment := T.typing_environment
        with type env_extension := T.env_extension
        with type flambda_type := T.flambda_type
        with type t_in_context := T.t_in_context
        with type 'a ty := 'a T.ty
        with type 'a unknown_or_join := 'a T.unknown_or_join
    end) (Typing_env_extension : sig
      include Typing_env_extension_intf.S
        with type env_extension := T.env_extension
        with type typing_environment := T.typing_environment
        with type flambda_type := T.flambda_type
    end) =
struct
  open T

  module Naked_immediate = Make_meet_and_join (struct
    type env_extension = T.env_extension

    type of_kind_foo = Immediate.Set.t of_kind_naked_number

    let kind = K.naked_immediate ()

    let to_type ty : t =
      { descr = Naked_number (ty, Naked_immediate);
        phantom = None;
      }

    let force_to_kind = force_to_kind_naked_immediate

    let print_ty = print_ty_naked_number

    let meet_of_kind_foo _env
          (of_kind1 : Immediate.Set.t of_kind_naked_number)
          (of_kind2 : Immediate.Set.t of_kind_naked_number)
          : (Immediate.Set.t of_kind_naked_number * env_extension)
              Or_bottom.t =
      match of_kind1, of_kind2 with
      | Immediate fs1, Immediate fs2 ->
        let fs = Immediate.Set.inter fs1 fs2 in
        if Immediate.Set.is_empty fs then Bottom
        else Ok (Immediate fs, Typing_env_extension.create ())
      | _, _ -> Bottom

    let join_of_kind_foo _env _env_extension1 _env_extension2
          (of_kind1 : Immediate.Set.t of_kind_naked_number)
          (of_kind2 : Immediate.Set.t of_kind_naked_number)
          : Immediate.Set.t of_kind_naked_number Or_unknown.t =
      match of_kind1, of_kind2 with
      | Immediate fs1, Immediate fs2 ->
        let fs = Immediate.Set.union fs1 fs2 in
        Known (Immediate fs)
      | _, _ -> Unknown
  end)

  module Naked_float = Make_meet_and_join (struct
    type of_kind_foo = Float_by_bit_pattern.Set.t of_kind_naked_number

    let kind = K.naked_float ()

    let to_type ty =
      { descr = Naked_number (ty, Naked_float);
        phantom = None;
      }

    let force_to_kind = force_to_kind_naked_float
    let print_ty = print_ty_naked_number

    let meet_of_kind_foo _env
          (of_kind1 : Float_by_bit_pattern.Set.t of_kind_naked_number)
          (of_kind2 : Float_by_bit_pattern.Set.t of_kind_naked_number)
          : (Float_by_bit_pattern.Set.t of_kind_naked_number
              * env_extension) Or_bottom.t =
      match of_kind1, of_kind2 with
      | Float fs1, Float fs2 ->
        let fs = Float_by_bit_pattern.Set.inter fs1 fs2 in
        if Float_by_bit_pattern.Set.is_empty fs then Bottom
        else Ok (Float fs, Typing_env_extension.create ())
      | _, _ -> Bottom

    let join_of_kind_foo _env _env_extension1 _env_extension2
          (of_kind1 : Float_by_bit_pattern.Set.t of_kind_naked_number)
          (of_kind2 : Float_by_bit_pattern.Set.t of_kind_naked_number)
          : Float_by_bit_pattern.Set.t of_kind_naked_number Or_unknown.t =
      match of_kind1, of_kind2 with
      | Float fs1, Float fs2 ->
        let fs = Float_by_bit_pattern.Set.union fs1 fs2 in
        Known (Float fs)
      | _, _ -> Unknown
  end)

  module Naked_int32 = Make_meet_and_join (struct
    type of_kind_foo = Int32.Set.t of_kind_naked_number

    let kind = K.naked_int32 ()

    let to_type ty : t =
      { descr = Naked_number (ty, Naked_int32);
        phantom = None;
      }

    let force_to_kind = force_to_kind_naked_int32
    let print_ty = print_ty_naked_number

    let meet_of_kind_foo _env
          (of_kind1 : Int32.Set.t of_kind_naked_number)
          (of_kind2 : Int32.Set.t of_kind_naked_number)
          : (Int32.Set.t of_kind_naked_number * env_extension) Or_bottom.t =
      match of_kind1, of_kind2 with
      | Int32 is1, Int32 is2 ->
        let is = Int32.Set.inter is1 is2 in
        if Int32.Set.is_empty is then Bottom
        else Ok (Int32 is, Typing_env_extension.create ())
      | _, _ -> Bottom

    let join_of_kind_foo _env _env_extension1 _env_extension2
          (of_kind1 : Int32.Set.t of_kind_naked_number)
          (of_kind2 : Int32.Set.t of_kind_naked_number)
          : Int32.Set.t of_kind_naked_number Or_unknown.t =
      match of_kind1, of_kind2 with
      | Int32 is1, Int32 is2 ->
        let is = Int32.Set.union is1 is2 in
        Known (Int32 is)
      | _, _ -> Unknown
  end)

  module Naked_int64 = Make_meet_and_join (struct
    type of_kind_foo = Int64.Set.t of_kind_naked_number

    let kind = K.naked_int64 ()

    let to_type ty : t =
      { descr = Naked_number (ty, Naked_int64);
        phantom = None;
      }

    let force_to_kind = force_to_kind_naked_int64
    let print_ty = print_ty_naked_number

    let meet_of_kind_foo _env
          (of_kind1 : Int64.Set.t of_kind_naked_number)
          (of_kind2 : Int64.Set.t of_kind_naked_number)
          : (Int64.Set.t of_kind_naked_number * env_extension) Or_bottom.t =
      match of_kind1, of_kind2 with
      | Int64 is1, Int64 is2 ->
        let is = Int64.Set.inter is1 is2 in
        if Int64.Set.is_empty is then Bottom
        else Ok (Int64 is, Typing_env_extension.create ())
      | _, _ -> Bottom

    let join_of_kind_foo _env _env_extension1 _env_extension2
          (of_kind1 : Int64.Set.t of_kind_naked_number)
          (of_kind2 : Int64.Set.t of_kind_naked_number)
          : Int64.Set.t of_kind_naked_number Or_unknown.t =
      match of_kind1, of_kind2 with
      | Int64 is1, Int64 is2 ->
        let is = Int64.Set.union is1 is2 in
        Known (Int64 is)
      | _, _ -> Unknown
  end)

  module Naked_nativeint = Make_meet_and_join (struct
    type of_kind_foo = Targetint.Set.t of_kind_naked_number

    let kind = K.naked_nativeint ()

    let to_type ty : t =
      { descr = Naked_number (ty, Naked_nativeint);
        phantom = None;
      }

    let force_to_kind = force_to_kind_naked_nativeint
    let print_ty = print_ty_naked_number

    let meet_of_kind_foo _env
          (of_kind1 : Targetint.Set.t of_kind_naked_number)
          (of_kind2 : Targetint.Set.t of_kind_naked_number)
          : (Targetint.Set.t of_kind_naked_number * env_extension) Or_bottom.t =
      match of_kind1, of_kind2 with
      | Nativeint is1, Nativeint is2 ->
        let is = Targetint.Set.inter is1 is2 in
        if Targetint.Set.is_empty is then Bottom
        else Ok (Nativeint is, Typing_env_extension.create ())
      | _, _ -> Bottom

    let join_of_kind_foo _env _env_extension1 _env_extension2
          (of_kind1 : Targetint.Set.t of_kind_naked_number)
          (of_kind2 : Targetint.Set.t of_kind_naked_number)
          : Targetint.Set.t of_kind_naked_number Or_unknown.t =
      match of_kind1, of_kind2 with
      | Nativeint is1, Nativeint is2 ->
        let is = Targetint.Set.union is1 is2 in
        Known (Nativeint is)
      | _, _ -> Unknown
  end)
end
