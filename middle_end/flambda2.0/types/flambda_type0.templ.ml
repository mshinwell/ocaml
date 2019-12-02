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

module Make
  (Term_language_function_declaration : Term_language_function_declaration.S)
= struct
  (* -- module rec binding here -- *)

  include Type_grammar

  type flambda_type = t

  let meet env t1 t2 : _ Or_bottom.t =
    let meet_env = Meet_env.create env in
    meet meet_env t1 t2

  let meet_shape env t ~shape ~result_var ~result_kind : _ Or_bottom.t =
    let result = Name_in_binding_pos.var result_var in
    let env = Typing_env.add_definition env result result_kind in
    match meet env t shape with
    | Bottom -> Bottom
    | Ok (_meet_ty, env_extension) -> Ok env_extension

  let arity_of_list ts =
    Flambda_arity.create (List.map kind ts)

  type typing_env = Typing_env.t
  type typing_env_extension = Typing_env_extension.t

  let invariant _env _t = ()  (* CR mshinwell: implement *)

  type 'a type_accessor = Typing_env.t -> 'a

  let unknown_types_from_arity arity =
    List.map (fun kind -> unknown kind) arity

  let bottom_types_from_arity arity =
    List.map (fun kind -> bottom kind) arity

  let is_bottom env t =
    match expand_head t env with
    | Resolved (Value Bottom)
    | Resolved (Naked_immediate Bottom)
    | Resolved (Naked_float Bottom)
    | Resolved (Naked_int32 Bottom)
    | Resolved (Naked_int64 Bottom)
    | Resolved (Naked_nativeint Bottom) -> true
    | Const _
    | Resolved (Value _)
    | Resolved (Naked_immediate _)
    | Resolved (Naked_float _)
    | Resolved (Naked_int32 _)
    | Resolved (Naked_int64 _)
    | Resolved (Naked_nativeint _) -> false

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
        print t
    end;
    (* XXX This probably shouldn't be using [get_alias] *)
    (* XXX This needs to match the equal-to-tagged-immediates function below *)
    match get_alias t with
    | None -> Unknown
    | Some simple ->
      match Simple.descr simple with
      | Const (Tagged_immediate imm) -> Proved (Tagged_immediate imm)
      | Const _ ->
        Misc.fatal_errorf "[Simple] %a in the [Equals] field has a kind \
            different from that returned by [kind] (%a):@ %a"
          Simple.print simple
          K.print original_kind
          print t
      | Name _ ->
        match
          Typing_env.get_canonical_simple env simple
            ~min_name_mode:Name_mode.normal
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
          | Const _ ->
            let kind = kind t in
            Misc.fatal_errorf "Kind returned by [get_canonical_simple] (%a) \
                doesn't match the kind of the returned [Simple] %a:@ %a"
              K.print kind
              Simple.print simple
              print t

  let prove_single_closures_entry env t : _ proof =
    let wrong_kind () = Misc.fatal_errorf "Type has wrong kind: %a" print t in
    match expand_head t env with
    | Const _ -> Invalid
    | Resolved resolved ->
      match resolved with
      | Value (Ok (Closures closures)) ->
        begin match
          Row_like.For_closures_entry_by_set_of_closures_contents.
            get_singleton closures.by_closure_id
        with
        | None -> Unknown
        | Some ((closure_id, _set_of_closures_contents), closures_entry) ->
          let function_decl =
            Closures_entry.find_function_declaration closures_entry closure_id
          in
          Proved (closure_id, function_decl)
        end
      | Value (Ok _) -> Invalid
      | Value Unknown -> Unknown
      | Value Bottom -> Invalid
      | Naked_immediate _ -> wrong_kind ()
      | Naked_float _ -> wrong_kind ()
      | Naked_int32 _ -> wrong_kind ()
      | Naked_int64 _ -> wrong_kind ()
      | Naked_nativeint _ -> wrong_kind ()

  (* CR mshinwell: Try to functorise or otherwise factor out across the
     various number kinds. *)
  let prove_naked_floats env t : _ proof =
    let wrong_kind () =
      Misc.fatal_errorf "Kind error: expected [Naked_float]:@ %a" print t
    in
    match expand_head t env with
    | Const (Naked_float f) -> Proved (Float.Set.singleton f)
    | Const (Naked_immediate _ | Tagged_immediate _ | Naked_int32 _
      | Naked_int64 _ | Naked_nativeint _) -> wrong_kind ()
    | Resolved resolved ->
      match resolved with
      | Naked_float (Ok fs) ->
        if Float.Set.is_empty fs then Invalid
        else Proved fs
      | Naked_float Unknown -> Unknown
      | Naked_float Bottom -> Invalid
      | Value _ -> wrong_kind ()
      | Naked_immediate _ -> wrong_kind ()
      | Naked_int32 _ -> wrong_kind ()
      | Naked_int64 _ -> wrong_kind ()
      | Naked_nativeint _ -> wrong_kind ()

  let prove_naked_int32s env t : _ proof =
    let wrong_kind () =
      Misc.fatal_errorf "Kind error: expected [Naked_int32]:@ %a" print t
    in
    match expand_head t env with
    | Const (Naked_int32 i) -> Proved (Int32.Set.singleton i)
    | Const (Naked_immediate _ | Tagged_immediate _ | Naked_float _
      | Naked_int64 _ | Naked_nativeint _) -> wrong_kind ()
    | Resolved resolved ->
      match resolved with
      | Naked_int32 (Ok is) ->
        if Int32.Set.is_empty is then Invalid
        else Proved is
      | Naked_int32 Unknown -> Unknown
      | Naked_int32 Bottom -> Invalid
      | Value _ -> wrong_kind ()
      | Naked_immediate _ -> wrong_kind ()
      | Naked_float _ -> wrong_kind ()
      | Naked_int64 _ -> wrong_kind ()
      | Naked_nativeint _ -> wrong_kind ()

  let prove_naked_int64s env t : _ proof =
    let wrong_kind () =
      Misc.fatal_errorf "Kind error: expected [Naked_int64]:@ %a" print t
    in
    match expand_head t env with
    | Const (Naked_int64 i) -> Proved (Int64.Set.singleton i)
    | Const (Naked_immediate _ | Tagged_immediate _ | Naked_float _
      | Naked_int32 _ | Naked_nativeint _) -> wrong_kind ()
    | Resolved resolved ->
      match resolved with
      | Naked_int64 (Ok is) ->
        if Int64.Set.is_empty is then Invalid
        else Proved is
      | Naked_int64 Unknown -> Unknown
      | Naked_int64 Bottom -> Invalid
      | Value _ -> wrong_kind ()
      | Naked_immediate _ -> wrong_kind ()
      | Naked_float _ -> wrong_kind ()
      | Naked_int32 _ -> wrong_kind ()
      | Naked_nativeint _ -> wrong_kind ()

  let prove_naked_nativeints env t : _ proof =
    let wrong_kind () =
      Misc.fatal_errorf "Kind error: expected [Naked_nativeint]:@ %a" print t
    in
    match expand_head t env with
    | Const (Naked_nativeint i) -> Proved (Targetint.Set.singleton i)
    | Const (Naked_immediate _ | Tagged_immediate _ | Naked_float _
      | Naked_int32 _ | Naked_int64 _) -> wrong_kind ()
    | Resolved resolved ->
      match resolved with
      | Naked_nativeint (Ok is) ->
        if Targetint.Set.is_empty is then Invalid
        else Proved is
      | Naked_nativeint Unknown -> Unknown
      | Naked_nativeint Bottom -> Invalid
      | Value _ -> wrong_kind ()
      | Naked_immediate _ -> wrong_kind ()
      | Naked_float _ -> wrong_kind ()
      | Naked_int32 _ -> wrong_kind ()
      | Naked_int64 _ -> wrong_kind ()

  let prove_is_int env t : bool proof =
    let wrong_kind () =
      Misc.fatal_errorf "Kind error: expected [Value]:@ %a" print t
    in
    match expand_head t env with
    | Const (Tagged_immediate _) -> Proved true
    | Const _ -> wrong_kind ()
    | Resolved resolved ->
      match resolved with
      | Value (Ok (Variant blocks_imms)) ->
        begin match blocks_imms.blocks, blocks_imms.immediates with
        | Unknown, Unknown | Unknown, Known _ | Known _, Unknown -> Unknown
        | Known blocks, Known imms ->
          (* CR mshinwell: Should we tighten things up by causing fatal errors
             in cases such as [blocks] and [imms] both being bottom? *)
          if Row_like.For_blocks.is_bottom blocks then
            if is_bottom env imms then Invalid
            else Proved true
          else
            if is_bottom env imms then Proved false
            else Unknown
        end
      | Value (Ok _) -> Invalid
      | Value Unknown -> Unknown
      | Value Bottom -> Invalid
      | Naked_immediate _ -> wrong_kind ()
      | Naked_float _ -> wrong_kind ()
      | Naked_int32 _ -> wrong_kind ()
      | Naked_int64 _ -> wrong_kind ()
      | Naked_nativeint _ -> wrong_kind ()

  let prove_tags_must_be_a_block env t : Tag.Set.t proof =
    let wrong_kind () =
      Misc.fatal_errorf "Kind error: expected [Value]:@ %a" print t
    in
    match expand_head t env with
    | Const (Tagged_immediate _) -> Unknown
    | Const _ -> wrong_kind ()
    | Resolved resolved ->
      match resolved with
      | Value (Ok (Variant blocks_imms)) ->
        begin match blocks_imms.immediates with
        | Unknown -> Unknown
        | Known imms ->
          if not (is_bottom env imms) then
            Invalid
          else
            match blocks_imms.blocks with
            | Unknown -> Unknown
            | Known blocks ->
              (* CR mshinwell: maybe [all_tags] should return the [Invalid]
                 case directly? *)
              match Row_like.For_blocks.all_tags blocks with
              | Unknown -> Unknown
              | Known tags ->
                if Tag.Set.is_empty tags then Invalid
                else Proved tags
        end
      | Value (Ok _) -> Invalid
      | Value Unknown -> Unknown
      | Value Bottom -> Invalid
      (* CR mshinwell: Here and elsewhere, use or-patterns. *)
      | Naked_immediate _ -> wrong_kind ()
      | Naked_float _ -> wrong_kind ()
      | Naked_int32 _ -> wrong_kind ()
      | Naked_int64 _ -> wrong_kind ()
      | Naked_nativeint _ -> wrong_kind ()

  let prove_naked_immediates env t : Immediate.Set.t proof =
    let wrong_kind () =
      Misc.fatal_errorf "Kind error: expected [Naked_immediate]:@ %a" print t
    in
    match expand_head t env with
    | Const (Naked_immediate i) -> Proved (Immediate.Set.singleton i)
    | Const (Tagged_immediate _ | Naked_float _ | Naked_int32 _
      | Naked_int64 _ | Naked_nativeint _) -> wrong_kind ()
    | Resolved resolved ->
      match resolved with
      | Naked_immediate (Ok (Naked_immediates is)) ->
        (* CR mshinwell: As noted elsewhere, add abstraction to avoid the need
           for these checks *)
        if Immediate.Set.is_empty is then Invalid
        else Proved is
      | Naked_immediate (Ok (Is_int scrutinee_ty)) ->
        begin match prove_is_int env scrutinee_ty with
        | Proved true -> Proved (Immediate.Set.singleton Immediate.bool_true)
        | Proved false -> Proved (Immediate.Set.singleton Immediate.bool_false)
        | Unknown -> Unknown
        | Invalid -> Invalid
        end
      | Naked_immediate (Ok (Get_tag block_ty)) ->
        begin match prove_tags_must_be_a_block env block_ty with
        | Proved tags ->
          let is =
            Tag.Set.fold (fun tag is ->
                Immediate.Set.add (Immediate.tag tag) is)
              tags
              Immediate.Set.empty
          in
          Proved is
        | Unknown -> Unknown
        | Invalid -> Invalid
        end
      | Naked_immediate Unknown -> Unknown
      | Naked_immediate Bottom -> Invalid
      | Value _ -> wrong_kind ()
      | Naked_float _ -> wrong_kind ()
      | Naked_int32 _ -> wrong_kind ()
      | Naked_int64 _ -> wrong_kind ()
      | Naked_nativeint _ -> wrong_kind ()

  let prove_equals_tagged_immediates env t : Immediate.Set.t proof =
    let wrong_kind () =
      Misc.fatal_errorf "Kind error: expected [Value]:@ %a" print t
    in
    match expand_head t env with
    | Const (Tagged_immediate imm) -> Proved (Immediate.Set.singleton imm)
    | Const (Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
      | Naked_nativeint _) -> wrong_kind ()
    | Resolved resolved ->
      match resolved with
      | Value (Ok (Variant blocks_imms)) ->
        begin match blocks_imms.blocks, blocks_imms.immediates with
        | Unknown, Unknown | Unknown, Known _ | Known _, Unknown -> Unknown
        | Known blocks, Known imms ->
          (* CR mshinwell: Check this.  Again it depends on the context; is
             this a context where variants are ok? *)
          if not (Row_like.For_blocks.is_bottom blocks)
          then Unknown
          else prove_naked_immediates env imms
        end
      | Value (Ok _) -> Invalid
      | Value Unknown -> Unknown
      | Value Bottom -> Invalid
      | Naked_immediate _ -> wrong_kind ()
      | Naked_float _ -> wrong_kind ()
      | Naked_int32 _ -> wrong_kind ()
      | Naked_int64 _ -> wrong_kind ()
      | Naked_nativeint _ -> wrong_kind ()

  let prove_equals_single_tagged_immediate env t : _ proof =
    match prove_equals_tagged_immediates env t with
    | Proved imms ->
      begin match Immediate.Set.get_singleton imms with
      | Some imm -> Proved imm
      | None -> Unknown
      end
    | Unknown -> Unknown
    | Invalid -> Invalid

  let prove_tags_and_sizes env t : Targetint.OCaml.t Tag.Map.t proof =
    let wrong_kind () =
      Misc.fatal_errorf "Kind error: expected [Value]:@ %a" print t
    in
    match expand_head t env with
    | Const (Tagged_immediate _) -> Unknown
    | Const _ -> wrong_kind ()
    | Resolved resolved ->
      match resolved with
      | Value (Ok (Variant blocks_imms)) ->
        begin match blocks_imms.immediates with
        (* CR mshinwell: Care.  Should this return [Unknown] or [Invalid] if
           there is the possibility of the type representing a tagged
           immediate? *)
        | Unknown -> Unknown
        | Known immediates ->
          if is_bottom env immediates then
            match blocks_imms.blocks with
            | Unknown -> Unknown
            | Known blocks ->
              match Row_like.For_blocks.all_tags_and_sizes blocks with
              | Unknown -> Unknown
              | Known tags_and_sizes -> Proved tags_and_sizes
          else
            Unknown
        end
      | Value (Ok _) -> Invalid
      | Value Unknown -> Unknown
      | Value Bottom -> Invalid
      (* CR mshinwell: Here and elsewhere, use or-patterns. *)
      | Naked_immediate _ -> wrong_kind ()
      | Naked_float _ -> wrong_kind ()
      | Naked_int32 _ -> wrong_kind ()
      | Naked_int64 _ -> wrong_kind ()
      | Naked_nativeint _ -> wrong_kind ()

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

  let prove_is_a_tagged_immediate env t : _ proof_allowing_kind_mismatch =
    match expand_head t env with
    | Const (Tagged_immediate _) -> Proved ()
    | Const _ -> Wrong_kind
    | Resolved resolved ->
      match resolved with
      | Value Unknown -> Unknown
      | Value (Ok (Variant { blocks; immediates; })) ->
        begin match blocks, immediates with
        | Unknown, Unknown | Unknown, Known _ | Known _, Unknown -> Unknown
        | Known blocks, Known imms ->
          if Row_like.For_blocks.is_bottom blocks && not (is_bottom env imms)
          then Proved ()
          else Invalid
        end
      | Value _ -> Invalid
      | _ -> Wrong_kind

  let prove_is_a_boxed_float env t : _ proof_allowing_kind_mismatch =
    match expand_head t env with
    | Const _ -> Wrong_kind
    | Resolved resolved ->
      match resolved with
      | Value Unknown -> Unknown
      | Value (Ok (Boxed_float _)) -> Proved ()
      | Value _ -> Invalid
      | _ -> Wrong_kind

  let prove_is_a_boxed_int32 env t : _ proof_allowing_kind_mismatch =
    match expand_head t env with
    | Const _ -> Wrong_kind
    | Resolved resolved ->
      match resolved with
      | Value Unknown -> Unknown
      | Value (Ok (Boxed_int32 _)) -> Proved ()
      | Value _ -> Invalid
      | _ -> Wrong_kind

  let prove_is_a_boxed_int64 env t : _ proof_allowing_kind_mismatch =
    match expand_head t env with
    | Const _ -> Wrong_kind
    | Resolved resolved ->
      match resolved with
      | Value Unknown -> Unknown
      | Value (Ok (Boxed_int64 _)) -> Proved ()
      | Value _ -> Invalid
      | _ -> Wrong_kind

  let prove_is_a_boxed_nativeint env t : _ proof_allowing_kind_mismatch =
    match expand_head t env with
    | Const _ -> Wrong_kind
    | Resolved resolved ->
      match resolved with
      | Value Unknown -> Unknown
      | Value (Ok (Boxed_nativeint _)) -> Proved ()
      | Value _ -> Invalid
      | _ -> Wrong_kind

  (* CR mshinwell: Factor out code from the following. *)

  let prove_boxed_floats env t : _ proof =
    let result_var = Variable.create "result" in
    let result_var' =
      Var_in_binding_pos.create result_var Name_mode.normal
    in
    let result_simple = Simple.var result_var in
    let result_kind = K.naked_float in
    let shape = box_float (alias_type_of result_kind result_simple) in
    match meet_shape env t ~shape ~result_var:result_var' ~result_kind with
    | Bottom -> Invalid
    | Ok env_extension ->
      let env =
        Typing_env.add_definition env
          (Name_in_binding_pos.create (Name.var result_var)
            Name_mode.normal)
          result_kind
      in
      let env = Typing_env.add_env_extension env ~env_extension in
      let t = Typing_env.find env (Name.var result_var) in
      prove_naked_floats env t

  let prove_boxed_int32s env t : _ proof =
    let result_var = Variable.create "result" in
    let result_var' =
      Var_in_binding_pos.create result_var Name_mode.normal
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
            Name_mode.normal)
          result_kind
      in
      let env = Typing_env.add_env_extension env ~env_extension in
      let t = Typing_env.find env (Name.var result_var) in
      prove_naked_int32s env t

  let prove_boxed_int64s env t : _ proof =
    let result_var = Variable.create "result" in
    let result_var' =
      Var_in_binding_pos.create result_var Name_mode.normal
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
            Name_mode.normal)
          result_kind
      in
      let env = Typing_env.add_env_extension env ~env_extension in
      let t = Typing_env.find env (Name.var result_var) in
      prove_naked_int64s env t

  let prove_boxed_nativeints env t : _ proof =
    let result_var = Variable.create "result" in
    let result_var' =
      Var_in_binding_pos.create result_var Name_mode.normal
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
            Name_mode.normal)
          result_kind
      in
      let env = Typing_env.add_env_extension env ~env_extension in
      let t = Typing_env.find env (Name.var result_var) in
      prove_naked_nativeints env t

  let prove_strings env t : String_info.Set.t proof =
    let wrong_kind () =
      Misc.fatal_errorf "Kind error: expected [Value]:@ %a" print t
    in
    match expand_head t env with
    | Const _ ->
      if K.equal (kind t) K.value then Invalid
      else wrong_kind ()
    | Resolved resolved ->
      match resolved with
      | Value (Ok (String strs)) -> Proved strs      
      | Value (Ok _) -> Invalid
      | Value Unknown -> Unknown
      | Value Bottom -> Invalid
      | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
      | Naked_nativeint _ -> wrong_kind ()

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

  (* CR mshinwell: Think more to identify all the cases that should be
     in this function. *)
  let reify env ~min_name_mode t : reification_result =
(*
Format.eprintf "reifying %a\n%!" print t;
*)
    match
      Typing_env.get_alias_then_canonical_simple env ~min_name_mode t
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
      match expand_head t env with
      | Const const -> Simple (Simple.const const)
      | Resolved resolved ->
        let try_canonical_simple () =
          match canonical_simple_opt with
          | None -> Cannot_reify
          | Some canonical_simple -> Simple canonical_simple
        in
        match resolved with
        | Value (Ok (Variant blocks_imms)) ->
          begin match blocks_imms.blocks, blocks_imms.immediates with
          | Known blocks, Known imms ->
            if is_bottom env imms then
              begin match Row_like.For_blocks.get_singleton blocks with
              | None -> try_canonical_simple ()
              | Some ((tag, size), field_types) ->
                assert (Targetint.OCaml.equal size
                  (Product.Int_indexed.width field_types));
                (* CR mshinwell: Could recognise other things, e.g. tagged
                   immediates and float arrays, supported by [Static_part]. *)
                let field_types =
                  Product.Int_indexed.components field_types
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
            else if Row_like.For_blocks.is_bottom blocks then
              match prove_naked_immediates env imms with
              | Proved imms ->
                begin match Immediate.Set.get_singleton imms with
                | None -> try_canonical_simple ()
                | Some imm ->
                (*
                  Format.eprintf "Type %a reifies to tagged imm %a\n%!"
                    print t Immediate.print imm;
                    *)
                  Simple (Simple.const (Tagged_immediate imm))
                end
              | Unknown -> try_canonical_simple ()
              | Invalid -> Invalid
            else
              try_canonical_simple ()
          | _, _ -> try_canonical_simple ()
          end
        (* CR mshinwell: share code with [prove_equals_tagged_immediates],
           above *)
        | Naked_immediate (Ok (Is_int scrutinee_ty)) ->
          begin match prove_is_int env scrutinee_ty with
          | Proved true -> Simple Simple.untagged_const_true
          | Proved false -> Simple Simple.untagged_const_false
          | Unknown -> try_canonical_simple ()
          | Invalid -> Invalid
          end
        | Naked_immediate (Ok (Get_tag block_ty)) ->
          begin match prove_tags_must_be_a_block env block_ty with
          | Proved tags ->
            let is =
              Tag.Set.fold (fun tag is ->
                  Immediate.Set.add (Immediate.tag tag) is)
                tags
                Immediate.Set.empty
            in
            begin match Immediate.Set.get_singleton is with
            | None -> try_canonical_simple ()
            | Some i -> Simple (Simple.const (Naked_immediate i))
            end
          | Unknown -> try_canonical_simple ()
          | Invalid -> Invalid
          end
        | Value Bottom
        | Naked_immediate Bottom | Naked_float Bottom
        | Naked_int32 Bottom | Naked_int64 Bottom | Naked_nativeint Bottom ->
          Invalid
        | _ -> try_canonical_simple ()
end
