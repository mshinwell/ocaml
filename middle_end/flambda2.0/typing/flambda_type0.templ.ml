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

  include Basic_type_ops
  include Type_grammar

  let meet env t1 t2 : _ Or_bottom.t =
    let meet_env = Meet_env.create env in
    (* CR mshinwell: We shouldn't need to do this Or_bottom.t recovery *)
    let meet_ty, env_extension = meet meet_env t1 t2 in
    if is_obviously_bottom meet_ty then Bottom
    else Ok (meet_ty, env_extension)

  let arity_of_list ts =
    Flambda_arity.create (List.map kind ts)

  type typing_env = Typing_env.t
  type typing_env_extension = Typing_env_extension.t

  let print = Type_printers.print
  let print_with_cache = Type_printers.print_with_cache

  let invariant _env _t = ()  (* CR mshinwell: implement *)

  type 'a type_accessor = Typing_env.t -> 'a

  let unknown_types_from_arity arity =
    List.map (fun kind -> unknown kind) arity

  let is_bottom env t =
    match Typing_env.expand_head env t with
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

  type 'a proof_allowing_kind_mismatch =
    | Proved of 'a
    | Unknown
    | Invalid
    | Wrong_kind

  type symbol_or_tagged_immediate =
    | Symbol of Symbol.t
    | Tagged_immediate of Immediate.t

  let prove_equals_to_symbol_or_tagged_immediate env t
        : symbol_or_tagged_immediate proof =
    let original_kind = kind t in
    if not (K.equal original_kind K.value) then begin
      Misc.fatal_errorf "Type %a is not of kind value"
        Type_printers.print t
    end;
    (* XXX This probably shouldn't be using [get_alias] *)
    match get_alias t with
    | None -> Unknown
    | Some simple ->
      match Simple.descr simple with
      | Const (Tagged_immediate imm) -> Proved (Tagged_immediate imm)
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
        | Bottom -> Invalid
        | Ok None -> Unknown
        | Ok (Some simple) ->
          (* CR mshinwell: Instead, get all aliases and find a Symbol,
             to avoid relying on the fact that if there is a Symbol alias then
             it will be canonical *)
          match Simple.descr simple with
          | Name (Symbol sym) -> Proved (Symbol sym)
          | Name (Var _) -> Unknown
          | Const (Tagged_immediate imm) -> Proved (Tagged_immediate imm)
          | Const _ | Discriminant _ ->
            let kind = kind t in
            Misc.fatal_errorf "Kind returned by [get_canonical_simple] (%a) \
                doesn't match the kind of the returned [Simple] %a:@ %a"
              K.print kind
              Simple.print simple
              Type_printers.print t

  let prove_single_closures_entry env t : _ proof =
    let wrong_kind () = Misc.fatal_errorf "Type has wrong kind: %a" print t in
    match Typing_env.expand_head env t with
    | Const _ | Discriminant _ -> Invalid
    | Resolved resolved ->
      match resolved with
      | Resolved_value (Ok (Closures closures)) ->
        begin
          match Closures_entry_by_set_of_closures_contents.get_singleton
            closures.by_closure_id
        with
        | None -> Unknown
        | Some ((closure_id, _set_of_closures_contents), closures_entry) ->
          let function_decl =
            Closures_entry.find_function_declaration closures_entry closure_id
          in
          Proved (closure_id, function_decl)
        end
      | Resolved_value (Ok _) -> Invalid
      | Resolved_value Unknown -> Unknown
      | Resolved_value Bottom -> Invalid
      | Resolved_naked_number _ -> wrong_kind ()
      | Resolved_fabricated _ -> wrong_kind ()

  let prove_equals_tagged_immediates env t : _ proof =
    let wrong_kind () =
      Misc.fatal_errorf "Kind error: expected [Value]:@ %a" print t
    in
    match Typing_env.expand_head env t with
    | Const (Tagged_immediate imm) -> Proved (Immediate.Set.singleton imm)
    | Const (Naked_immediate _ | Naked_float _ | Naked_int32 _
      | Naked_int64 _ | Naked_nativeint _)
    | Discriminant _ -> wrong_kind ()
    | Resolved resolved ->
      match resolved with
      | Resolved_value (Ok (Blocks_and_tagged_immediates blocks_imms)) ->
        begin match blocks_imms.blocks, blocks_imms.immediates with
        | Unknown, Unknown | Unknown, Known _ | Known _, Unknown -> Unknown
        | Known blocks, Known imms ->
          if Blocks.is_bottom blocks then
            match Immediates.all imms with
            | Known imms -> Proved imms
            | Unknown -> Unknown
          else
            Invalid
        end
      | Resolved_value (Ok _) -> Invalid
      | Resolved_value Unknown -> Unknown
      | Resolved_value Bottom -> Invalid
      | Resolved_naked_number _ | Resolved_fabricated _ -> wrong_kind ()

  let prove_equals_single_tagged_immediate env t : _ proof =
    match prove_equals_tagged_immediates env t with
    | Proved imms ->
      begin match Immediate.Set.get_singleton imms with
      | Some imm -> Proved imm
      | None -> Unknown
      end
    | Unknown -> Unknown
    | Invalid -> Invalid

  (* CR mshinwell: Try to functorise or otherwise factor out across the
     various number kinds. *)
  let prove_naked_floats env t : _ proof =
    let wrong_kind () =
      Misc.fatal_errorf "Kind error: expected [Naked_float]:@ %a" print t
    in
    match Typing_env.expand_head env t with
    | Const (Naked_float f) -> Proved (Float.Set.singleton f)
    | Const (Tagged_immediate _ | Naked_immediate _ | Naked_int32 _
      | Naked_int64 _ | Naked_nativeint _)
    | Discriminant _ -> wrong_kind ()
    | Resolved resolved ->
      match resolved with
      | Resolved_naked_number (Ok (Float fs), Naked_float) -> Proved fs
      | Resolved_naked_number (Unknown, Naked_float) -> Unknown
      | Resolved_naked_number (Bottom, Naked_float) -> Invalid
      | Resolved_value _ | Resolved_naked_number _
      | Resolved_fabricated _ -> wrong_kind ()

  let prove_naked_int32s env t : _ proof =
    let wrong_kind () =
      Misc.fatal_errorf "Kind error: expected [Naked_int32]:@ %a" print t
    in
    match Typing_env.expand_head env t with
    | Const (Naked_int32 i) -> Proved (Int32.Set.singleton i)
    | Const (Tagged_immediate _ | Naked_immediate _ | Naked_float _
      | Naked_int64 _ | Naked_nativeint _)
    | Discriminant _ -> wrong_kind ()
    | Resolved resolved ->
      match resolved with
      | Resolved_naked_number (Ok (Int32 is), Naked_int32) -> Proved is
      | Resolved_naked_number (Unknown, Naked_int32) -> Unknown
      | Resolved_naked_number (Bottom, Naked_int32) -> Invalid
      | Resolved_value _ | Resolved_naked_number _
      | Resolved_fabricated _ -> wrong_kind ()

  let prove_naked_int64s env t : _ proof =
    let wrong_kind () =
      Misc.fatal_errorf "Kind error: expected [Naked_int64]:@ %a" print t
    in
    match Typing_env.expand_head env t with
    | Const (Naked_int64 i) -> Proved (Int64.Set.singleton i)
    | Const (Tagged_immediate _ | Naked_immediate _ | Naked_float _
      | Naked_int32 _ | Naked_nativeint _)
    | Discriminant _ -> wrong_kind ()
    | Resolved resolved ->
      match resolved with
      | Resolved_naked_number (Ok (Int64 is), Naked_int64) -> Proved is
      | Resolved_naked_number (Unknown, Naked_int64) -> Unknown
      | Resolved_naked_number (Bottom, Naked_int64) -> Invalid
      | Resolved_value _ | Resolved_naked_number _
      | Resolved_fabricated _ -> wrong_kind ()

  let prove_naked_nativeints env t : _ proof =
    let wrong_kind () =
      Misc.fatal_errorf "Kind error: expected [Naked_int64]:@ %a" print t
    in
    match Typing_env.expand_head env t with
    | Const (Naked_nativeint i) -> Proved (Targetint.Set.singleton i)
    | Const (Tagged_immediate _ | Naked_immediate _ | Naked_float _
      | Naked_int32 _ | Naked_int64 _)
    | Discriminant _ -> wrong_kind ()
    | Resolved resolved ->
      match resolved with
      | Resolved_naked_number (Ok (Nativeint is), Naked_nativeint) -> Proved is
      | Resolved_naked_number (Unknown, Naked_nativeint) -> Unknown
      | Resolved_naked_number (Bottom, Naked_nativeint) -> Invalid
      | Resolved_value _ | Resolved_naked_number _
      | Resolved_fabricated _ -> wrong_kind ()

  let prove_is_int env t : bool proof =
    let wrong_kind () =
      Misc.fatal_errorf "Kind error: expected [Value]:@ %a" print t
    in
    match Typing_env.expand_head env t with
    | Const (Tagged_immediate _) -> Proved true
    | Const (Naked_immediate _ | Naked_float _ | Naked_int32 _
      | Naked_int64 _ | Naked_nativeint _)
    | Discriminant _ -> wrong_kind ()
    | Resolved resolved ->
      match resolved with
      | Resolved_value (Ok (Blocks_and_tagged_immediates blocks_imms)) ->
        begin match blocks_imms.blocks, blocks_imms.immediates with
        | Unknown, Unknown | Unknown, Known _ | Known _, Unknown -> Unknown
        | Known blocks, Known imms ->
          (* CR mshinwell: Should we tighten things up by causing fatal errors
             in cases such as [blocks] and [imms] both being bottom? *)
          if Blocks.is_bottom blocks then
            if Immediates.is_bottom imms then Invalid
            else Proved true
          else
            if Immediates.is_bottom imms then Proved false
            else Unknown
        end
      | Resolved_value (Ok _) -> Invalid
      | Resolved_value Unknown -> Unknown
      | Resolved_value Bottom -> Invalid
      | Resolved_naked_number _ | Resolved_fabricated _ -> wrong_kind ()

  let prove_tags_and_sizes env t : Targetint.OCaml.t Tag.Map.t proof =
    let wrong_kind () =
      Misc.fatal_errorf "Kind error: expected [Value]:@ %a" print t
    in
    match Typing_env.expand_head env t with
    | Const (Tagged_immediate _) -> Unknown
    | Const (Naked_immediate _ | Naked_float _ | Naked_int32 _
      | Naked_int64 _ | Naked_nativeint _)
    | Discriminant _ -> wrong_kind ()
    | Resolved resolved ->
      match resolved with
      | Resolved_value (Ok (Blocks_and_tagged_immediates blocks_imms)) ->
        begin match blocks_imms.immediates with
        | Unknown -> Unknown
        | Known _ ->
          match blocks_imms.blocks with
          | Unknown -> Unknown
          | Known blocks ->
            match Blocks.all_tags_and_sizes blocks with
            | Unknown -> Unknown
            | Known tags_and_sizes -> Proved tags_and_sizes
        end
      | Resolved_value (Ok _) -> Invalid
      | Resolved_value Unknown -> Unknown
      | Resolved_value Bottom -> Invalid
      | Resolved_naked_number _ | Resolved_fabricated _ -> wrong_kind ()

  let prove_unique_tag_and_size env t
       : (Tag.t * Targetint.OCaml.t) proof_allowing_kind_mismatch =
    if not (Flambda_kind.equal (kind t) Flambda_kind.value) then
      Wrong_kind
    else
      match prove_tags_and_sizes env t with
      | Invalid -> Invalid
      | Unknown -> Unknown
      | Proved tags_to_sizes ->
        match Tag.Map.get_singleton tags_to_sizes with
        | None -> Unknown
        | Some (tag, size) -> Proved (tag, size)

  let prove_is_a_boxed_float env t : _ proof_allowing_kind_mismatch =
    match Typing_env.expand_head env t with
    | Const _ | Discriminant _ -> Wrong_kind
    | Resolved resolved ->
      match resolved with
      | Resolved_value Unknown -> Unknown
      | Resolved_value (Ok (Boxed_number (Boxed_float _))) -> Proved ()
      | Resolved_value _ -> Invalid
      | _ -> Wrong_kind

  let prove_is_a_boxed_int32 env t : _ proof_allowing_kind_mismatch =
    match Typing_env.expand_head env t with
    | Const _ | Discriminant _ -> Wrong_kind
    | Resolved resolved ->
      match resolved with
      | Resolved_value Unknown -> Unknown
      | Resolved_value (Ok (Boxed_number (Boxed_int32 _))) -> Proved ()
      | Resolved_value _ -> Invalid
      | _ -> Wrong_kind

  let prove_is_a_boxed_int64 env t : _ proof_allowing_kind_mismatch =
    match Typing_env.expand_head env t with
    | Const _ | Discriminant _ -> Wrong_kind
    | Resolved resolved ->
      match resolved with
      | Resolved_value Unknown -> Unknown
      | Resolved_value (Ok (Boxed_number (Boxed_int64 _))) -> Proved ()
      | Resolved_value _ -> Invalid
      | _ -> Wrong_kind

  let prove_is_a_boxed_nativeint env t : _ proof_allowing_kind_mismatch =
    match Typing_env.expand_head env t with
    | Const _ | Discriminant _ -> Wrong_kind
    | Resolved resolved ->
      match resolved with
      | Resolved_value Unknown -> Unknown
      | Resolved_value (Ok (Boxed_number (Boxed_nativeint _))) -> Proved ()
      | Resolved_value _ -> Invalid
      | _ -> Wrong_kind

  (* CR mshinwell: Factor out code from the following. *)

  let prove_boxed_floats env t : _ proof =
    let result_var = Variable.create "result" in
    let result_var' =
      Var_in_binding_pos.create result_var Name_occurrence_kind.normal
    in
    let result_simple = Simple.var result_var in
    let result_kind = K.naked_float in
    let shape = box_float (alias_type_of result_kind result_simple) in
Format.eprintf "shape for boxed float proof:@ %a\n%!"
  Type_printers.print shape;
    match meet_shape env t ~shape ~result_var:result_var' ~result_kind with
    | Bottom -> Invalid
    | Ok env_extension ->
      let env =
        Typing_env.add_definition env
          (Name_in_binding_pos.create (Name.var result_var)
            Name_occurrence_kind.normal)
          result_kind
      in
      let env = Typing_env.add_env_extension env env_extension in
      let t = Typing_env.find env (Name.var result_var) in
Format.eprintf "result type for boxed float proof:@ %a\n%!"
  Type_printers.print t;
      prove_naked_floats env t

  let prove_boxed_int32s env t : _ proof =
    let result_var = Variable.create "result" in
    let result_var' =
      Var_in_binding_pos.create result_var Name_occurrence_kind.normal
    in
    let result_simple = Simple.var result_var in
    let result_kind = K.naked_int32 in
    let shape = box_int32 (alias_type_of result_kind result_simple) in
    match meet_shape env t ~shape ~result_var:result_var' ~result_kind with
    | Bottom -> Invalid
    | Ok env_extension ->
      let env =
        Typing_env.add_definition env
          (Name_in_binding_pos.create (Name.var result_var)
            Name_occurrence_kind.normal)
          result_kind
      in
      let env = Typing_env.add_env_extension env env_extension in
      let t = Typing_env.find env (Name.var result_var) in
      prove_naked_int32s env t

  let prove_boxed_int64s env t : _ proof =
    let result_var = Variable.create "result" in
    let result_var' =
      Var_in_binding_pos.create result_var Name_occurrence_kind.normal
    in
    let result_simple = Simple.var result_var in
    let result_kind = K.naked_int64 in
    let shape = box_int64 (alias_type_of result_kind result_simple) in
    match meet_shape env t ~shape ~result_var:result_var' ~result_kind with
    | Bottom -> Invalid
    | Ok env_extension ->
      let env =
        Typing_env.add_definition env
          (Name_in_binding_pos.create (Name.var result_var)
            Name_occurrence_kind.normal)
          result_kind
      in
      let env = Typing_env.add_env_extension env env_extension in
      let t = Typing_env.find env (Name.var result_var) in
      prove_naked_int64s env t

  let prove_boxed_nativeints env t : _ proof =
    let result_var = Variable.create "result" in
    let result_var' =
      Var_in_binding_pos.create result_var Name_occurrence_kind.normal
    in
    let result_simple = Simple.var result_var in
    let result_kind = K.naked_nativeint in
    let shape = box_nativeint (alias_type_of result_kind result_simple) in
    match meet_shape env t ~shape ~result_var:result_var' ~result_kind with
    | Bottom -> Invalid
    | Ok env_extension ->
      let env =
        Typing_env.add_definition env
          (Name_in_binding_pos.create (Name.var result_var)
            Name_occurrence_kind.normal)
          result_kind
      in
      let env = Typing_env.add_env_extension env env_extension in
      let t = Typing_env.find env (Name.var result_var) in
      prove_naked_nativeints env t

  let prove_equals_discriminants env t : Discriminant.Set.t proof =
    let wrong_kind () =
      Misc.fatal_errorf "Kind error: expected [Fabricated]:@ %a" print t
    in
    match Typing_env.expand_head env t with
    | Const _ -> wrong_kind ()
    | Discriminant discr -> Proved (Discriminant.Set.singleton discr)
    | Resolved resolved ->
      match resolved with
      | Resolved_value _ | Resolved_naked_number _ -> wrong_kind ()
      | Resolved_fabricated (Ok (Discriminants discrs)) ->
        begin match Discriminants.all discrs with
        | Known discrs -> Proved discrs
        | Unknown -> Unknown
        end
      | Resolved_fabricated Unknown -> Unknown
      | Resolved_fabricated Bottom -> Invalid

  let prove_strings env t : String_info.Set.t proof =
    let wrong_kind () =
      Misc.fatal_errorf "Kind error: expected [Value]:@ %a" print t
    in
    match Typing_env.expand_head env t with
    | Const _ ->
      if K.equal (kind t) K.value then Invalid
      else wrong_kind ()
    | Discriminant _ -> wrong_kind ()
    | Resolved resolved ->
      match resolved with
      | Resolved_value (Ok (String strs)) -> Proved strs      
      | Resolved_value (Ok _) -> Invalid
      | Resolved_value Unknown -> Unknown
      | Resolved_value Bottom -> Invalid
      | Resolved_naked_number _
      | Resolved_fabricated _ -> wrong_kind ()

  type to_lift =
    | Immutable_block of Tag.Scannable.t * (symbol_or_tagged_immediate list)
    | Boxed_float of Float.t
    | Boxed_int32 of Int32.t
    | Boxed_int64 of Int64.t
    | Boxed_nativeint of Targetint.t

  type reification_result =
    | Lift of to_lift
    | Simple of Simple.t
    | Cannot_reify
    | Invalid

  let reify env ~min_occurrence_kind t : reification_result =
(*
Format.eprintf "reifying %a\n%!" Type_printers.print t;
*)
    match
      Typing_env.get_alias_then_canonical_simple env ~min_occurrence_kind t
    with
    | Bottom -> Invalid
    | Ok (Some canonical_simple)
        when begin match Simple.descr canonical_simple with
        | Name (Symbol _) -> true
        | _ -> false
        end ->
      (* Don't lift things that are already bound to symbols.  Apart from
         anything else, this could cause aliases between symbols, which are
         currently forbidden (every symbol has the same binding time). *)
      Cannot_reify
    | Ok canonical_simple_opt ->
      match Typing_env.expand_head env t with
      | Const const -> Simple (Simple.const const)
      | Discriminant discr -> Simple (Simple.discriminant discr)
      | Resolved resolved ->
        let try_canonical_simple () =
          match canonical_simple_opt with
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
                let symbols_or_tagged_immediates =
                  List.filter_map
                    (fun field_type : symbol_or_tagged_immediate option ->
                      match
                        prove_equals_to_symbol_or_tagged_immediate env
                          field_type
                      with
                      | Proved (Symbol sym) -> Some (Symbol sym)
                      | Proved (Tagged_immediate sym) ->
                        Some (Tagged_immediate sym)
                      (* CR mshinwell: [Invalid] should propagate up *)
                      | Unknown | Invalid -> None)
                    field_types
                in
                if List.compare_lengths field_types symbols_or_tagged_immediates
                  = 0
                then
                  match Tag.Scannable.of_tag tag with
                  | Some tag ->
                    Lift (Immutable_block (tag, symbols_or_tagged_immediates))
                  | None -> try_canonical_simple ()
                else
                  try_canonical_simple ()
              end
            else
              try_canonical_simple ()
          | _, _ -> try_canonical_simple ()
          end
        | Resolved_value Bottom
        | Resolved_naked_number (Bottom, _)
        | Resolved_fabricated Bottom -> Invalid
        | _ -> try_canonical_simple ()
end
