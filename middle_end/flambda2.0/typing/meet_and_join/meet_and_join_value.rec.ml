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

module KI = Kind_independent_meet_or_join
module T = Flambda_types
module TEE = Typing_env_extension

module Make
  (E : Lattice_ops_intf.S
   with type meet_env := Meet_env.t
   with type typing_env := Typing_env.t
   with type typing_env_extension := Typing_env_extension.t) =
struct
  module Of_kind_naked_immediate = Meet_and_join_naked_immediate.Make (E)
  module Of_kind_naked_float = Meet_and_join_naked_float.Make (E)
  module Of_kind_naked_int32 = Meet_and_join_naked_int32.Make (E)
  module Of_kind_naked_int64 = Meet_and_join_naked_int64.Make (E)
  module Of_kind_naked_nativeint = Meet_and_join_naked_nativeint.Make (E)
  module Of_kind_fabricated = Meet_and_join_fabricated.Make (E)

  module Naked_immediate = KI.Make (E) (Of_kind_naked_immediate)
  module Naked_float = KI.Make (E) (Of_kind_naked_float)
  module Naked_int32 = KI.Make (E) (Of_kind_naked_int32)
  module Naked_int64 = KI.Make (E) (Of_kind_naked_int64)
  module Naked_nativeint = KI.Make (E) (Of_kind_naked_nativeint)
  module Fabricated = KI.Make (E) (Of_kind_fabricated)

  type of_kind_foo = T.of_kind_value

  let kind = K.value
  let to_type ty : T.t = Value ty
  let force_to_kind = Flambda_type0_core.force_to_kind_value
  let print_ty = Type_printers.print_ty_value_with_cache
  let apply_rec_info = Flambda_type0_core.apply_rec_info_of_kind_value

  (* CR mshinwell: These next two could go in a separate file. *)
  let meet_unknown meet_contents env
      (or_unknown1 : _ Or_unknown.t) (or_unknown2 : _ Or_unknown.t)
      : ((_ Or_unknown.t) * TEE.t) Or_bottom.t =
    match or_unknown1, or_unknown2 with
    | Unknown, Unknown -> Ok (Unknown, TEE.empty ())
    | _, Unknown -> Ok (or_unknown1, TEE.empty ())
    | Unknown, _ -> Ok (or_unknown2, TEE.empty ())
    | Known contents1, Known contents2 ->
      Or_bottom.map (meet_contents env contents1 contents2)
        ~f:(fun (contents, env_extension) ->
          Or_unknown.Known contents, env_extension)

  let join_unknown join_contents env
      (or_unknown1 : _ Or_unknown.t) (or_unknown2 : _ Or_unknown.t)
      : _ Or_unknown.t =
    match or_unknown1, or_unknown2 with
    | Unknown, Unknown
    | _, Unknown
    | Unknown, _ -> Unknown
    | Known contents1, Known contents2 ->
      Known (join_contents env contents1 contents2)

  let meet_or_join_blocks_and_tagged_immediates env
        ({ blocks = blocks1; immediates = imms1; }
          : T.blocks_and_tagged_immediates)
        ({ blocks = blocks2; immediates = imms2; }
          : T.blocks_and_tagged_immediates) : _ Or_bottom.t =
    let blocks =
      E.switch (meet_unknown Blocks.meet) (join_unknown Blocks.join)
        env blocks1 blocks2
    in
    let imms =
      E.switch (meet_unknown Immediates.meet) (join_unknown Immediates.join)
        env imms1 imms2
    in
    match blocks, imms with
    | Bottom, Bottom -> Bottom
    | Ok (blocks, env_extension), Bottom ->
      let blocks_and_imms : T.blocks_and_tagged_immediates =
        { blocks;
          immediates = Known (Immediates.create_bottom ());
        }
      in
      Ok (blocks_and_imms, env_extension)
    | Bottom, Ok (immediates, env_extension) ->
      let blocks_and_imms : T.blocks_and_tagged_immediates =
        { blocks = Known (Blocks.create_bottom ());
          immediates;
        }
      in
      Ok (blocks_and_imms, env_extension)
    | Ok (blocks, env_extension1), Ok (immediates, env_extension2) ->
      let blocks_and_imms : T.blocks_and_tagged_immediates =
        { blocks;
          immediates;
        }
      in
      let env_extension =
        (* XXX *)
        let left_env = Meet_env.env env in
        let right_env = Meet_env.env env in
        E.switch0 TEE.meet (TEE.join ~left_env ~right_env) env
          env_extension1 env_extension2
      in
      Ok (blocks_and_imms, env_extension)

  let meet_or_join_closures_entry env
        ({ function_decl = function_decl1;
           closure_elements = closure_elements1;
           set_of_closures = set_of_closures1;
         } : T.closures_entry)
        ({ function_decl = function_decl2;
           closure_elements = closure_elements2;
           set_of_closures = set_of_closures2;
         } : T.closures_entry) =
    let function_decl : T.function_declaration Or_unknown.t =
      match function_decl1, function_decl2 with
      | Unknown, Unknown -> Unknown
      | Known _, Unknown ->
        begin match E.op () with
        | Join -> Unknown
        | Meet -> function_decl1
        end
      | Unknown, Known _ ->
        begin match E.op () with
        | Join -> Unknown
        | Meet -> function_decl2
        end
      | Known decl1, Known decl2 ->
        match decl1, decl2 with
        | Non_inlinable {
            param_arity = param_arity1; result_arity = result_arity1;
            recursive = recursive1;
          }, Non_inlinable {
            param_arity = param_arity2; result_arity = result_arity2;
            recursive = recursive2;
          } ->
          (* CR mshinwell: Are fatal errors right here?  Given the arbitrary
             choice below, it would seem so, but unsure.  Also, the error
             message is currently poor. *)
          if Flambda_arity.equal param_arity1 param_arity2
            && Flambda_arity.equal result_arity1 result_arity2
            && Recursive.equal recursive1 recursive2
          then
            Known decl1
          else
            Misc.fatal_error "Mismatched Non_inlinable arities"
        | Non_inlinable _ , Inlinable _
        | Inlinable _, Non_inlinable _ ->
          (* CR mshinwell: This should presumably return [Non_inlinable] if
             the arities match. *)
          Unknown
        | Inlinable { function_decl = decl1; rec_info = _rec_info1; },
            Inlinable { function_decl = decl2; rec_info = _rec_info2; } ->
          (* CR mshinwell: Assertions about other properties of
             [decl1] versus [decl2]? *)
          (* CR mshinwell: What about [rec_info]? *)
          let module TFD = Term_language_function_declaration in
          match E.op () with
          | Join ->
            (* CR mshinwell: As mentioned in [Function_declaration], [Code_id]
               is a misnomer at present. *)
            if Code_id.equal (TFD.code_id decl1) (TFD.code_id decl2)
            then function_decl1
            else Unknown
          | Meet ->
            (* We can arbitrarily pick one of the functions, since they must
               both behave in the same way, even if we cannot prove it. *)
            function_decl1
    in
    let closure_elements =
      E.switch Closure_elements.meet Closure_elements.join env
        closure_elements1 closure_elements2
    in
    let set_of_closures =
      Fabricated.meet_or_join_ty env set_of_closures1 set_of_closures2
    in
    Or_bottom.both closure_elements set_of_closures
      ~f:(fun (closure_elements, env_extension1)
              (set_of_closures, env_extension2) ->
        let closures_entry : T.closures_entry =
          { function_decl;
            closure_elements;
            set_of_closures;
          }
        in
        let env_extension =
          (* XXX This should use the proper environments from both sides, no?
             See if we can avoid needing that *)
          let left_env = Meet_env.env env in
          let right_env = Meet_env.env env in
          E.switch0 TEE.meet (TEE.join ~left_env ~right_env) env
            env_extension1 env_extension2
        in
        closures_entry, env_extension)

  let meet_or_join_naked_number env n1 n2 meet_or_join_ty box =
    Or_bottom_or_absorbing.of_or_bottom (meet_or_join_ty env n1 n2)
      ~f:(fun (n, env_extension) -> T.Boxed_number (box n), env_extension)

  let meet_or_join_of_kind_foo env ~meet_or_join_ty
        (of_kind1 : T.of_kind_value) (of_kind2 : T.of_kind_value)
        : (T.of_kind_value * TEE.t) Or_bottom_or_absorbing.t =
    match of_kind1, of_kind2 with
    | Blocks_and_tagged_immediates blocks_imms1,
        Blocks_and_tagged_immediates blocks_imms2 ->
      Or_bottom_or_absorbing.of_or_bottom
        (meet_or_join_blocks_and_tagged_immediates env
          blocks_imms1 blocks_imms2)
        ~f:(fun (blocks_imms, env_extension) ->
          T.Blocks_and_tagged_immediates blocks_imms, env_extension)
    | Boxed_number (Boxed_float n1), Boxed_number (Boxed_float n2) ->
      meet_or_join_naked_number env n1 n2
        (* CR mshinwell: Sort out this [bound_name] stuff *)
        (Naked_float.meet_or_join_ty ?bound_name:None)
        (fun n -> T.Boxed_float n)
    | Boxed_number (Boxed_int32 n1), Boxed_number (Boxed_int32 n2) ->
      meet_or_join_naked_number env n1 n2
        (Naked_int32.meet_or_join_ty ?bound_name:None)
        (fun n -> T.Boxed_int32 n)
    | Boxed_number (Boxed_int64 n1), Boxed_number (Boxed_int64 n2) ->
      meet_or_join_naked_number env n1 n2
        (Naked_int64.meet_or_join_ty ?bound_name:None)
        (fun n -> T.Boxed_int64 n)
    | Boxed_number (Boxed_nativeint n1), Boxed_number (Boxed_nativeint n2) ->
      meet_or_join_naked_number env n1 n2
        (Naked_nativeint.meet_or_join_ty ?bound_name:None)
        (fun n -> T.Boxed_nativeint n)
    | Closures { by_closure_id = by_closure_id1; },
        Closures { by_closure_id = by_closure_id2; } ->
      let module C = Closures_entry_by_closure_id in
      Or_bottom_or_absorbing.of_or_bottom
        (E.switch C.meet C.join env by_closure_id1 by_closure_id2)
        ~f:(fun (by_closure_id, env_extension) ->
          T.Closures { by_closure_id; }, env_extension)
    | String strs1, String strs2 ->
      let strs = E.String_info.Set.union_or_inter strs1 strs2 in
      if String_info.Set.is_empty strs then Bottom
      else Or_bottom_or_absorbing.Ok (String strs, TEE.empty ())
    | Array { length = length1; }, Array { length = length2; } ->
      Or_bottom_or_absorbing.of_or_bottom
        (meet_or_join_ty ?bound_name:None env length1 length2)
        ~f:(fun (length, env_extension) -> T.Array { length }, env_extension)
    | (Blocks_and_tagged_immediates _
        | Boxed_number _
        | Closures _
        | String _
        | Array _), _ -> Absorbing
end
