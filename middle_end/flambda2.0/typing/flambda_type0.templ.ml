(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2019 OCamlPro SAS                                    *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR mshinwell: Decide on doc or non-doc comments in here.  There are some
   modules which aren't exposed in the interface but probably require
   documentation. *)

(* CR mshinwell: Remove when warning 60 fixed *)
[@@@ocaml.warning "-60"]

module Float = Numbers.Float_by_bit_pattern
module Int32 = Numbers.Int32
module Int64 = Numbers.Int64

module K = Flambda_kind

(* CR mshinwell: Should there be a different [Name_occurrences] used for
   types?  It would remove most of the "everything_must_only_be_names"
   stuff. *)

module Make
  (Term_language_function_declaration : Term_language_function_declaration.S)
= struct
  (* -- module rec binding here -- *)

  include Flambda_type0_core
  include Flambda_types

  let meet env t1 t2 = Api_meet_and_join.meet env t1 t2
  let join env t1 t2 = Api_meet_and_join.join env t1 t2

  let meet_shape env t ~shape ~result_var ~result_kind : _ Or_bottom.t =
    let result = Name_in_binding_pos.var result_var in
    let env = Typing_env.add_definition env result result_kind in
    let env = Meet_env.create env in
(*
Format.eprintf "Meeting: %a@ and@ %a\n%!"
  Type_printers.print t
  Type_printers.print shape;
*)
    let meet_ty, env_extension = meet env t shape in
(*
Format.eprintf "meet_ty: %a@ TEE: %a\n%!"
  Type_printers.print meet_ty
  Typing_env_extension.print env_extension;
*)
    if is_obviously_bottom meet_ty then Bottom
    else Ok env_extension

  let erase_aliases = Type_erase_aliases.erase_aliases
  let erase_aliases_ty_value = Type_erase_aliases.erase_aliases_ty_value

  let arity_of_list ts =
    Flambda_arity.create (List.map Flambda_type0_core.kind ts)

  type typing_env = Typing_env.t
  type typing_env_extension = Typing_env_extension.t

  let print = Type_printers.print
  let print_with_cache = Type_printers.print_with_cache

  let invariant _env _t = ()  (* CR mshinwell: implement *)

  type 'a type_accessor = Typing_env.t -> 'a

  let unknown_types_from_arity arity =
    List.map (fun kind -> unknown kind) arity

  let is_bottom env t =
    match snd (Typing_env.resolve_type env t) with
    | Resolved (Resolved_value Bottom)
    | Resolved (Resolved_naked_number (Bottom, _))
    | Resolved (Resolved_fabricated Bottom) -> true
    | Const _ | Discriminant _
    | Resolved (Resolved_value _)
    | Resolved (Resolved_naked_number _)
    | Resolved (Resolved_fabricated _) -> false

  type 'a proof =
    | Proved of 'a
    | Unknown
    | Invalid

  (* CR mshinwell: These next two functions could maybe share some code *)

  let prove_equals_single_tagged_immediate env t : _ proof =
    let original_kind = kind t in
    if not (K.equal original_kind K.value) then begin
      Misc.fatal_errorf "Type %a is not of kind value"
        Type_printers.print t
    end;
    match get_alias t with
    | None -> Unknown
    | Some simple ->
      match Simple.descr simple with
      | Const (Tagged_immediate imm) -> Proved imm
      | Const _ | Discriminant _ ->
        Misc.fatal_errorf "[Simple] %a in the [Equals] field has a kind \
            different from that returned by [kind] (%a):@ %a"
          Simple.print simple
          K.print original_kind
          Type_printers.print t
      | Name _ ->
        match
          Typing_env.get_canonical_simple env simple
            ~min_occurrence_kind:Name_occurrence_kind.normal
        with
        | Bottom, _ -> Invalid
        | Ok simple, ty ->
          let kind = Flambda_type0_core.kind ty in
          if not (K.equal kind K.value) then begin
            Misc.fatal_errorf "Canonical [Simple] (%a) has a kind (%a) \
                different from that returned by [kind] (%a):@ %a"
              Simple.print simple
              K.print kind
              K.print original_kind
              Type_printers.print t
          end;
          match Simple.descr simple with
          | Const (Tagged_immediate imm) -> Proved imm
          | Name _ -> Unknown
          | Const _ | Discriminant _ ->
            Misc.fatal_errorf "Kind returned by [get_canonical_simple] (%a) \
                doesn't match the kind of the returned [Simple] %a:@ %a"
              K.print kind
              Simple.print simple
              Type_printers.print t

  let prove_equals_to_symbol env t : _ proof =
    let original_kind = kind t in
    if not (K.equal original_kind K.value) then begin
      Misc.fatal_errorf "Type %a is not of kind value"
        Type_printers.print t
    end;
    (* CR mshinwell: The [get_alias] thing should presumably return the
       canonical one *)
    match get_alias t with
    | None -> Unknown
    | Some simple ->
      match Simple.descr simple with
      | Const _ | Discriminant _ ->
        Misc.fatal_errorf "[Simple] %a in the [Equals] field has a kind \
            different from that returned by [kind] (%a):@ %a"
          Simple.print simple
          K.print original_kind
          Type_printers.print t
      | Name _ ->
        match
          Typing_env.get_canonical_simple env simple
            ~min_occurrence_kind:Name_occurrence_kind.normal
        with
        | Bottom, _ -> Invalid
        | Ok simple, ty ->
          let kind = Flambda_type0_core.kind ty in
          if not (K.equal kind K.value) then begin
            Misc.fatal_errorf "Canonical [Simple] (%a) has a kind (%a) \
                different from that returned by [kind] (%a):@ %a"
              Simple.print simple
              K.print kind
              K.print original_kind
              Type_printers.print t
          end;
          match Simple.descr simple with
          | Name (Symbol sym) -> Proved sym
          | Name (Var _) -> Unknown
          | Const _ | Discriminant _ ->
            Misc.fatal_errorf "Kind returned by [get_canonical_simple] (%a) \
                doesn't match the kind of the returned [Simple] %a:@ %a"
              K.print kind
              Simple.print simple
              Type_printers.print t

  let prove_single_closures_entry env t : _ proof =
    let wrong_kind () = Misc.fatal_errorf "Type has wrong kind: %a" print t in
    match snd (Typing_env.resolve_type env t) with
    | Const _ | Discriminant _ -> Invalid
    | Resolved resolved ->
      match resolved with
      | Resolved_value Unknown -> Unknown
      | Resolved_value (Ok (Closures closures)) ->
        begin
          match Closures_entry_by_closure_id.get_singleton
            closures.by_closure_id
        with
        | None -> Unknown
        | Some ((closure_id, _var_within_closures), closures_entry) ->
          Proved (closure_id, closures_entry.function_decl)
        end
      | Resolved_value (Ok _) -> Invalid
      | Resolved_value Bottom -> Invalid
      | Resolved_naked_number _ -> wrong_kind ()
      | Resolved_fabricated _ -> wrong_kind ()

  type to_lift =
    | Immutable_block of Tag.Scannable.t * (Symbol.t list)
    | Boxed_float of Float.t
    | Boxed_int32 of Int32.t
    | Boxed_int64 of Int64.t
    | Boxed_nativeint of Targetint.t

  type reification_result =
    | Lift of to_lift
    | Simple of Simple.t
    | Cannot_reify
    | Invalid

  let reify env t : reification_result =
    match Typing_env.resolve_type env t with
    | None, (Const _ | Discriminant _) -> Cannot_reify
    | Some canonical_simple, (Const _ | Discriminant _) ->
      Simple canonical_simple
    | canonical_simple, Resolved resolved ->
      let try_canonical_simple () =
        match canonical_simple with
        | None -> Cannot_reify
        | Some canonical_simple -> Simple canonical_simple
      in
      match resolved with
      | Resolved_value (Ok (Blocks_and_tagged_immediates blocks_imms)) ->
        begin match blocks_imms.blocks, blocks_imms.immediates with
        | Known blocks, Known imms ->
          if Immediates.is_bottom imms then
            begin match Blocks.get_singleton blocks with
            | None -> try_canonical_simple ()
            | Some ((tag, size), field_types) ->
              assert (Targetint.OCaml.equal size
                (Blocks.Int_indexed_product.width field_types));
              (* CR mshinwell: Could recognise other things, e.g. tagged
                 immediates and float arrays, supported by [Static_part]. *)
              let field_types =
                Blocks.Int_indexed_product.components field_types
              in
              let symbols =
                List.filter_map (fun field_type ->
                    match prove_equals_to_symbol env field_type with
                    | Proved sym -> Some sym
                    (* CR mshinwell: [Invalid] should propagate up *)
                    | Unknown | Invalid -> None)
                  field_types
              in
              if List.compare_lengths field_types symbols = 0 then
                match Tag.Scannable.of_tag tag with
                | Some tag -> Lift (Immutable_block (tag, symbols))
                | None -> try_canonical_simple ()
              else
                try_canonical_simple ()
            end
          else
            try_canonical_simple ()
        | _, _ -> try_canonical_simple ()
        end
(*
      | Resolved_value (Ok (Boxed_number (Boxed_int64 ty_naked_int64))) ->
        let unknown_or_join, _canonical_simple =
          Typing_env.resolve_any_toplevel_alias_on_ty0 env
            ~force_to_kind:Flambda_type0_core.force_to_kind_naked_int64
            ~print_ty:Type_printers.print_ty_naked_int64
            ty_naked_int64
        in
        begin match unknown_or_join with
        | Ok (Int64 ints) ->
          begin match Int64.Set.get_singleton ints with
          | Some i -> Lift (Boxed_int64 i)
          | None -> Cannot_reify
          end
        | _ -> Cannot_reify
        end
*)
      | Resolved_value Bottom
      | Resolved_naked_number (Bottom, _)
      | Resolved_fabricated Bottom -> Invalid
      | _ -> try_canonical_simple ()
end
