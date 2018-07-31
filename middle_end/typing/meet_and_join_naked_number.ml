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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module K = Flambda_kind

module Float = Numbers.Float_by_bit_pattern
module Int32 = Numbers.Int32
module Int64 = Numbers.Int64

module Make (W : Typing_world.S)
  (E : Either_meet_or_join_intf.S with module T := T) =
struct
  module TEE = W.Typing_env_extension
  module T = W.T

  module Naked_immediate = Make_meet_and_join (struct
    type of_kind_foo = Immediate.Set.t T.of_kind_naked_number

    let kind = K.naked_immediate ()

    let to_type ty : T.t =
      { descr = Naked_number (ty, Naked_immediate);
      }

    let force_to_kind = T.force_to_kind_naked_immediate
    let print_ty = T.print_ty_naked_number

    let meet_or_join_of_kind_foo _meet_or_join_env _perm1 _perm2
          (of_kind1 : Immediate.Set.t T.of_kind_naked_number)
          (of_kind2 : Immediate.Set.t T.of_kind_naked_number)
          : (Immediate.Set.t T.of_kind_naked_number * TEE.t)
              Or_absorbing.t =
      match of_kind1, of_kind2 with
      | Immediate fs1, Immediate fs2 ->
        let fs = E.Immediate.Set.union_or_inter fs1 fs2 in
        if Immediate.Set.is_empty fs then Absorbing
        else Ok (Immediate fs, TEE.empty)
      | _, _ -> Absorbing
  end)

  module Naked_float = Make_meet_and_join (struct
    type of_kind_foo = Float.Set.t T.of_kind_naked_number

    let kind = K.naked_float ()

    let to_type ty : T.t =
      { descr = Naked_number (ty, Naked_float);
      }

    let force_to_kind = T.force_to_kind_naked_float
    let print_ty = T.print_ty_naked_number

    let meet_or_join_of_kind_foo _meet_or_join_env _perm1 _perm2
          (of_kind1 : Float.Set.t T.of_kind_naked_number)
          (of_kind2 : Float.Set.t T.of_kind_naked_number)
          : (Float.Set.t T.of_kind_naked_number * TEE.t) Or_absorbing.t =
      match of_kind1, of_kind2 with
      | Float fs1, Float fs2 ->
        let fs = E.Float.Set.union_or_inter fs1 fs2 in
        if Float.Set.is_empty fs then Absorbing
        else Ok (Float fs, TEE.t)
      | _, _ -> Absorbing
  end)

  module Naked_int32 = Make_meet_and_join (struct
    type of_kind_foo = Int32.Set.t of_kind_naked_number

    let kind = K.naked_int32 ()

    let to_type ty : T.t =
      { descr = Naked_number (ty, Naked_int32);
      }

    let force_to_kind = T.force_to_kind_naked_int32
    let print_ty = T.print_ty_naked_number

    let meet_or_join_of_kind_foo _meet_or_join_env _perm1 _perm2
          (of_kind1 : Int32.Set.t T.of_kind_naked_number)
          (of_kind2 : Int32.Set.t T.of_kind_naked_number)
          : (Int32.Set.t T.of_kind_naked_number * TEE.t) Or_absorbing.t =
      match of_kind1, of_kind2 with
      | Int32 is1, Int32 is2 ->
        let is = E.Int32.Set.union_or_inter is1 is2 in
        if Int32.Set.is_empty is then Absorbing
        else Ok (Int32 is, TEE.empty)
      | _, _ -> Absorbing
  end)

  module Naked_int64 = Make_meet_and_join (struct
    type of_kind_foo = Int64.Set.t of_kind_naked_number

    let kind = K.naked_int64 ()

    let to_type ty : T.t =
      { descr = Naked_number (ty, Naked_int64);
      }

    let force_to_kind = T.force_to_kind_naked_int64
    let print_ty = T.print_ty_naked_number

    let meet_or_join_of_kind_foo _meet_or_join_env _perm1 _perm2
          (of_kind1 : Int64.Set.t T.of_kind_naked_number)
          (of_kind2 : Int64.Set.t T.of_kind_naked_number)
          : (Int64.Set.t T.of_kind_naked_number * TEE.t) Or_absorbing.t =
      match of_kind1, of_kind2 with
      | Int64 is1, Int64 is2 ->
        let is = E.Int64.Set.union_or_inter is1 is2 in
        if Int64.Set.is_empty is then Absorbing
        else Ok (Int64 is, TEE.empty)
      | _, _ -> Absorbing
  end)

  module Naked_nativeint = Make_meet_and_join (struct
    type of_kind_foo = Targetint.Set.t of_kind_naked_number

    let kind = K.naked_nativeint ()

    let to_type ty : T.t =
      { descr = Naked_number (ty, Naked_nativeint);
      }

    let force_to_kind = T.force_to_kind_naked_nativeint
    let print_ty = T.print_ty_naked_number

    let meet_or_join_of_kind_foo _env _perm1 _perm2
          (of_kind1 : Targetint.Set.t of_kind_naked_number)
          (of_kind2 : Targetint.Set.t of_kind_naked_number)
          : (Targetint.Set.t T.of_kind_naked_number * TEE.t) Or_absorbing.t =
      match of_kind1, of_kind2 with
      | Nativeint is1, Nativeint is2 ->
        let is = E.Targetint.Set.union_or_inter is1 is2 in
        if Targetint.Set.is_empty is then Absorbing
        else Ok (Nativeint is, TEE.empty)
      | _, _ -> Absorbing
  end)
end
