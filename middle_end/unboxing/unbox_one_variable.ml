(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017--2018 OCamlPro SAS                                    *)
(*   Copyright 2017--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module E = Simplify_env_and_result.Env
module T = Flambda_type

module How_to_unbox = struct
  type t = {
    unboxee_to_wrapper_params_unboxee : Variable.t Variable.Map.t;
    add_bindings_in_wrapper : Flambda.Expr.t -> Flambda.Expr.t;
    new_arguments_for_call_in_wrapper : Variable.t list;
    new_params : Flambda.Typed_parameter.t list;
    new_unboxee_types : (Variable.t * Flambda_type.t) list;
    build_boxed_value_from_new_params :
      (Flambda.Typed_parameter.t * (Flambda.Expr.t -> Flambda.Expr.t)) list;
  }

  let create () =
    { unboxee_to_wrapper_params_unboxee = Variable.Map.empty;
      add_bindings_in_wrapper = (fun expr -> expr);
      new_arguments_for_call_in_wrapper = [];
      new_params = [];
      new_unboxee_types = [];
      build_boxed_value_from_new_params = [];
    }

  let merge t1 t2 =
    { unboxee_to_wrapper_params_unboxee =
        Variable.Map.union (fun _ param1 param2 ->
            assert (Variable.equal param1 param2);
            Some param1)
          t1.unboxee_to_wrapper_params_unboxee
          t2.unboxee_to_wrapper_params_unboxee;
      add_bindings_in_wrapper = (fun expr ->
        t2.add_bindings_in_wrapper (
          t1.add_bindings_in_wrapper expr));
      new_arguments_for_call_in_wrapper =
        t1.new_arguments_for_call_in_wrapper
          @ t2.new_arguments_for_call_in_wrapper;
      new_params = t1.new_params @ t2.new_params;
      new_unboxee_types = t1.new_unboxee_types @ t2.new_unboxee_types;
      build_boxed_value_from_new_params =
        t1.build_boxed_value_from_new_params
          @ t2.build_boxed_value_from_new_params;
    }

  let merge_variable_map t_map =
    Variable.Map.fold (fun _param t1 t2 -> merge t1 t2) t_map (create ())
end

module type Unboxing_spec = sig
  val unboxed_kind : Flambda_kind.t

end

module Unboxing_spec_variant : Unboxing_spec = struct
  let unboxed_kind = K.value ()



  let is_int = Variable.rename ~append:"_is_int" unboxee in
  let is_int_in_wrapper = Variable.rename is_int in
  let is_int_known_value =
    if no_constant_ctors then Some (Simple Simple.zero : Flambda.Named.t)
    else None
  in
  (* CR-soon mshinwell: On [discriminant] add information that tells us
     about the individual unboxed field parameters _given that_ we are
     in some particular case of a match on [discriminant] (GADT-style). *)
  let discriminant = Variable.rename ~append:"_discr" unboxee in
  let discriminant_in_wrapper = Variable.rename discriminant in
  let discriminant_known_value =
    let discriminant_possible_values =
      let all_tags =
        Tag.Map.fold (fun tag _ all_tags ->
            Numbers.Int.Set.add (Tag.to_int tag) all_tags)
          blocks
          Numbers.Int.Set.empty
      in
      Numbers.Int.Set.union constant_ctors all_tags
    in
    match Numbers.Int.Set.elements discriminant_possible_values with
    | [] -> assert false  (* see the bottom of [how_to_unbox], below *)
    | [tag] -> Some (Simple (Simple.const_int tag) : Flambda.Named.t)
    | _tags -> None
  in
  let needs_discriminant =
    match discriminant_known_value with
    | None -> true
    | Some _ -> false
  in
  let is_int_in_wrapper' = Variable.rename is_int_in_wrapper in
  let discriminant_in_wrapper' = Variable.rename discriminant_in_wrapper in
  let new_arguments_for_call_in_wrapper =
    let is_int =
      if no_constant_ctors then [] else [is_int_in_wrapper']
    in
    let discriminant =
      if not needs_discriminant then [] else [discriminant_in_wrapper']
    in
    is_int @ discriminant @ field_arguments_for_call_in_wrapper
  in



  (* for unbox_returns: *)
  let boxing_is_int_cont = Continuation.create () in
  let boxing_is_block_cont = Continuation.create () in
  let boxing_is_int_switch =
    let arms =
      Targetint.OCaml.Map.of_list [
        Targetint.OCaml.zero, boxing_is_block_cont;
        Targetint.OCaml.one, boxing_is_int_cont;
      ]
    in
    Flambda.Expr.create_int_switch ~scrutinee:is_int ~arms
  in
  let boxing_switch =
    let arms =
      Tag.Map.map (fun (_size, boxing_cont) -> boxing_cont)
        tags_to_sizes_and_boxing_conts
    in
    Flambda.Expr.create_tag_switch ~scrutinee:discriminant ~arms
  in


  let unboxee_ty =
    let unboxee_discriminants =
      T.variant_whose_discriminants_are ~is_int ~get_tag
    in
    (E.type_accessor env T.join) unboxee_ty unboxee_discriminants
  in
  let is_int =
(* same as below.
    if no_constant_ctors then []
    else
*)
      let is_int_ty =
        let by_constant_ctor_index =
          Targetint.OCaml.Set.fold (fun ctor_index by_constant_ctor_index ->
              let tag = Tag.of_targetint_ocaml ctor_index in
              let env = T.Typing_environment.create () in
              Tag.Map.add tag env by_constant_ctor_index)
            constant_ctors
            Tag.Map.empty
        in
        let by_tag =
          Tag.Map.map (fun size ->
              let env = ref (T.Typing_environment.create ()) in
              for field = 0 to size - 1 do
                let field, kind = fields_with_kinds0.(field) in
                let field_scope_level = E.continuation_scope_level env in
                (* CR mshinwell: We could refine the types of the actual fields
                   themselves according to the tag. *)
                let field_ty = T.unknown kind in
                env := T.Typing_environment.add !env field field_scope_level
                  field_ty
              done;
              !env)
            tags_to_sizes
        in
        let discriminant_env_is_int =
          T.Typing_environment.add discriminant
            (E.continuation_scope_level env)
            (T.these_tags by_constant_ctor_index)
        in
        let discriminant_env_is_block =
          T.Typing_environment.add discriminant
            (E.continuation_scope_level env)
            (T.these_tags by_tag)
        in
        let by_is_int_result =
          Immediate.Map.of_list [
            Immediate.const_true, discriminant_env_is_int;
            Immediate.const_false, discriminant_env_is_block;
          ]
        in
        T.these_tagged_immediates_with_envs by_is_int_result
      in
      let is_int = Parameter.wrap is_int in
      [Flambda.Typed_parameter.create is_int is_int_ty]
  in
  let discriminant =
(* We probably still need this now in all cases to get the extra type info,
   or else we need another special case in here to refine the type of
   [unboxee] unilaterally.
    if not needs_discriminant then []
    else [discriminant, Projection.Prim (Pgettag, [unboxee])] *)
    let discriminant = Parameter.wrap discriminant in
    Flambda.Typed_parameter.create discriminant discriminant_ty
  in
end

module Unboxing_spec_naked_number (N : sig

end) : Unboxing_spec = struct

end

module Unboxing_spec_float = Unboxing_spec_naked_number (struct

end

module Unboxing_spec_int32 = Unboxing_spec_naked_number (struct

end

module Unboxing_spec_int64 = Unboxing_spec_naked_number (struct

end

module Unboxing_spec_nativeint = Unboxing_spec_naked_number (struct

end

let how_to_unbox_core ~type_of_name ~env ~constant_ctors ~blocks ~unboxee
      ~unboxee_ty ~unbox_returns : How_to_unbox.t =
  let dbg = Debuginfo.none in
  let num_constant_ctors = Numbers.Int.Set.cardinal constant_ctors in
  assert (num_constant_ctors >= 0);
  (* CR mshinwell: We need to think about this more.
     Suppose we have code that deconstructs an "int option".  That code uses
     Pisint.  However we know that the thing is only ever going to be
     "Some x" and try to elide the "_is_int" parameter.  However that means
     we don't know that Pisint foo_option = false.  For the moment we don't
     elide the "_is_int".  Note that for Unbox_continuation_params the
     extra argument isn't really a problem---it will be removed---but for
     Unbox_returns we really don't want to generate an extra return value
     if it isn't needed.
     Follow-up: think this might be ok for Unbox_returns only, since we don't
     need the Pisint = false judgements etc.
  *)
  let no_constant_ctors = 
    if unbox_returns then num_constant_ctors = 0
    else false
  in
  let num_tags = Tag.Map.cardinal blocks in
  assert (num_tags >= 1);  (* see below *)
  let wrapper_param_unboxee = Variable.rename unboxee in
  let unboxee_to_wrapper_params_unboxee =
    Variable.Map.add unboxee wrapper_param_unboxee
      Variable.Map.empty
  in
  let max_size =
    Tag.Map.fold (fun _tag fields max_size ->
        max (Array.length fields) max_size)
      blocks 0
  in
  let field_arguments_for_call_in_wrapper =
    Array.to_list (Array.init max_size (fun index ->
      Variable.create (Printf.sprintf "field%d" index)))
  in
  let new_arguments_for_call_in_wrapper =
    S.new_arguments_for_call_in_wrapper @ field_arguments_for_call_in_wrapper
  in
  let new_arguments_for_call_in_wrapper_with_types =
    (* CR mshinwell: should be able to do better for the type here *)
    List.map (fun arg -> Parameter.wrap arg, T.any_value ())
      new_arguments_for_call_in_wrapper
  in
  let tags_to_sizes = Tag.Map.map (fun fields -> Array.length fields) blocks in
  let all_tags = Tag.Map.keys blocks in
  let sizes_to_filler_conts =
    List.fold_left (fun sizes_to_filler_conts size ->
        Numbers.Int.Map.add size (Continuation.create ()) sizes_to_filler_conts)
      Numbers.Int.Map.empty
      (Tag.Map.data tags_to_sizes)
  in
  let tags_to_sizes_and_boxing_conts =
    Tag.Map.map (fun size -> size, Continuation.create ()) tags_to_sizes
  in
  let all_units = Array.to_list (Array.init max_size (fun _ -> Simple.unit)) in
  let add_bindings_in_wrapper expr =
    let is_int_cont = Continuation.create () in
    let is_block_cont = Continuation.create () in
    let join_cont = Continuation.create () in
    let tag = Variable.create "tag" in
    let is_int_switch =
      let arms =
        Targetint.OCaml.Map.of_list [
          Targetint.OCaml.zero, is_block_cont;
          Targetint.OCaml.one, is_int_cont;
        ]
      in
      Flambda.Expr.create_int_switch ~scrutinee:is_int_in_wrapper ~arms
    in
    let add_fill_fields_conts expr =
      Numbers.Int.Map.fold (fun size filler_cont expr : Flambda.Expr.t ->
          let fields =
            Array.init max_size (fun index ->
              if index < size then
                let name = Printf.sprintf "_field%d" index in
                index, Some (Variable.rename ~append:name unboxee)
              else
                index, None)
          in
          let fields_for_apply =
            List.map (fun (_index, var_opt) ->
                match var_opt with
                | None -> Simple.const_unit
                | Some var -> Simple.var var)
              (Array.to_list fields)
          in
          let filler : Flambda.Expr.t =
            let filler : Flambda.Expr.t =
              let is_int_in_wrapper =
                if no_constant_ctors then [] else [is_int_in_wrapper]
              in
              let tag = if not needs_discriminant then [] else [tag] in
              Apply_cont (join_cont, None,
                is_int_in_wrapper @ tag @ fields_for_apply)
            in
            Array.fold_right (fun (index, var_opt) filler ->
                match var_opt with
                | None -> filler
                | Some var ->
                  (* CR mshinwell: We should be able to do better than
                     [Unknown], based on the type of the unboxee. *)
                  Flambda.Expr.create_let var
                    (Prim (Binary (Block_load (Value Unknown, Immutable),
                      wrapper_param_unboxee, index), dbg))
                    filler)
              fields
              filler
          in
          Let_cont {
            body = expr;
            handlers = Non_recursive {
              name = filler_cont;
              handler = {
                params = [];
                (* CR mshinwell: All of the "stub" settings in this file are
                   "true" so we don't try to unbox their arguments over and
                   over.  Maybe instead we should have a "kind" field which
                   could include the stub, is_exn_handler, etc data plus
                   something saying not to unbox *)
                stub = true;
                is_exn_handler = false;
                handler = filler;
              };
            }
          })
        sizes_to_filler_conts
        expr      
    in
    let fill_fields_switch =
      let all_possible_values =
        Tag.Set.fold (fun tag all_possible_values ->
            Numbers.Int.Set.add (Tag.to_int tag) all_possible_values)
          all_tags
          Numbers.Int.Set.empty
      in
      let arms =
        Numbers.Int.Map.fold (fun size filler_cont consts ->
            Tag.Map.fold (fun tag fields consts ->
                if Array.length fields = size then
                  (Tag.to_int tag, filler_cont) :: consts
                else
                  consts)
              blocks
              consts)
          sizes_to_filler_conts
          []
      in
      Flambda.Expr.create_switch ~scrutinee:tag
        ~all_possible_values
        ~arms
        ~default:None
    in
    Flambda.Expr.create_let is_int_in_wrapper
      (if no_constant_ctors then Simple Simple.const_zero
       else Prim (Unary (Is_int, wrapper_param_unboxee), dbg))
      (Let_cont {
        body = Let_cont {
          body = Let_cont {
            body = is_int_switch;
            handlers = Non_recursive {
              name = is_int_cont;
              handler = {
                params = [];
                handler =
                  (let is_int_in_wrapper =
                    if no_constant_ctors then [] else [is_int_in_wrapper]
                  in
                  let wrapper_param_unboxee =
                    if not needs_discriminant then []
                    else [wrapper_param_unboxee]
                  in
                  Apply_cont (join_cont, None,
                    is_int_in_wrapper @ wrapper_param_unboxee
                      @ all_units));
                stub = true;
                is_exn_handler = false;
              };
            };
          };
          handlers = Non_recursive {
            name = is_block_cont;
            handler = {
              params = [];
              handler =
                Flambda.Expr.create_let tag
                  (match discriminant_known_value with
                   | Some known -> known
                   | None ->
                     Prim (Unary (Get_tag, wrapper_param_unboxee), dbg))
                  (add_fill_fields_conts fill_fields_switch);
              stub = true;
              is_exn_handler = false;
            };
          };
        };
        handlers = Non_recursive {
          name = join_cont;
          handler = {
            params = Flambda.Typed_parameter.List.create
              new_arguments_for_call_in_wrapper_with_types;
            handler = expr;
            stub = true;
            is_exn_handler = false;
          };
        }
      })
  in
  let fields_with_kinds0 =
    Array.init max_size (fun index ->
      let append = string_of_int index in
      let var = Variable.rename ~append unboxee in
      var, field_kind)
  in
  let fields_with_kinds = Array.to_list fields_with_kinds0 in
  (* CR mshinwell: This next section is only needed for [Unbox_returns] at
     present; we shouldn't run it unless required. *)
  let build_boxed_value_from_new_params =
    let boxed = Variable.rename ~append:"_boxed" unboxee in
    let boxed_param = Parameter.wrap boxed in
    (* CR mshinwell: Should be able to do better for [boxed_ty] *)
    let boxed_ty = T.any_value () in
    let join_cont = Continuation.create () in
    let build (expr : Flambda.Expr.t) : Flambda.Expr.t =
      let arms =
        Numbers.Int.Set.fold (fun ctor_index arms ->
            let tag = Tag.create_exn ctor_index in
            let cont = Continuation.create () in
            Tag.Map.add tag cont arms)
          constant_ctors
          Tag.Map.empty
      in
      let constant_ctor_switch =
        Flambda.Expr.create_int_switch ~scrutinee:discriminant ~arms
      in
      let add_constant_ctor_conts expr =
        List.fold_left (fun expr (ctor_index, cont) ->
            let ctor_index_var = Variable.create "ctor_index" in
            Flambda.Expr.create_let ctor_index_var
              (Simple (Simple.int ctor_index))
              (Let_cont {
                body = expr;
                handlers = Non_recursive {
                  name = cont;
                  handler = {
                    handler = Apply_cont (
                      join_cont, None, [Simple.var ctor_index_var]);
                    params = [];
                    stub = true;
                    is_exn_handler = false;
                  };
                };
              }))
          expr
          consts
      in
      let add_boxing_conts expr =
        Tag.Map.fold (fun tag (size, boxing_cont) expr : Flambda.Expr.t ->
            let boxed = Variable.rename boxed in
            let fields =
              let fields, _index =
                List.fold_left (fun (fields, index) field ->
                    if index >= size then fields, index + 1
                    else (field :: fields), index + 1)
                  ([], 0)
                  fields_with_kinds
              in
              List.rev fields
            in
            let handler : Flambda.Expr.t =
              Flambda.Expr.create_let boxed
                (Prim (Pmakeblock (Tag.to_int tag, Immutable, None),
                  fields, dbg))
                (Flambda.Apply_cont (join_cont, None, [boxed]))
            in
            Let_cont {
              body = expr;
              handlers = Non_recursive {
                name = boxing_cont;
                handler = {
                  params = [];
                  handler;
                  stub = true;
                  is_exn_handler = false;
                };
              };
            })
          tags_to_sizes_and_boxing_conts
          expr
      in
      let body : Flambda.Expr.t =
        Let_cont {
          body = Let_cont {
            body = Let_cont {
              body = boxing_is_int_switch;
              handlers = Non_recursive {
                name = boxing_is_block_cont;
                handler = {
                  params = [];
                  handler = add_boxing_conts boxing_switch;
                  stub = true;
                  is_exn_handler = false;
                };
              };
            };
            handlers = Non_recursive {
              name = boxing_is_int_cont;
              handler = {
                params = [];
                (* We could just call [join_cont] with [discriminant] as the
                   argument, but that wouldn't pass on the knowledge to the
                   place in which this stub gets inlined that [discriminant]
                   is an integer. *)
                (* CR-someday mshinwell: Maybe adding some kind of support for
                   coercions would help here.  Perhaps another approach would be
                   to do CSE on "Pisint discriminant" (which would rewrite to
                   the "is_int" variable returned from the callee).  This would
                   require propagation of the projection information from the
                   stub function generated by Unbox_returns to the place it's
                   being inlined. *)
                handler = add_constant_ctor_conts constant_ctor_switch;
                stub = true;
                is_exn_handler = false;
              };
            };
          };
          handlers = Non_recursive {
            name = join_cont;
            handler = {
              params = [Flambda.Typed_parameter.create boxed_param boxed_ty];
              handler = expr;
              stub = true;
              is_exn_handler = false;
            };
          };
        }
      in
      let body =
        match is_int_known_value with
        | None -> body
        | Some named -> Flambda.Expr.create_let is_int named body
      in
      match discriminant_known_value with
      | None -> body
      | Some named -> Flambda.Expr.create_let discriminant named body
    in
    [boxed, build]
  in
  let unboxee_ty =
    let unboxee_discriminants =
      T.variant_whose_discriminants_are ~is_int ~get_tag
    in
    (E.type_accessor env T.join) unboxee_ty unboxee_discriminants
  in
  let is_int =
(* same as below.
    if no_constant_ctors then []
    else
*)
      let is_int_ty =
        let by_constant_ctor_index =
          Targetint.OCaml.Set.fold (fun ctor_index by_constant_ctor_index ->
              let tag = Tag.of_targetint_ocaml ctor_index in
              let env = T.Typing_environment.create () in
              Tag.Map.add tag env by_constant_ctor_index)
            constant_ctors
            Tag.Map.empty
        in
        let by_tag =
          Tag.Map.map (fun size ->
              let env = ref (T.Typing_environment.create ()) in
              for field = 0 to size - 1 do
                let field, kind = fields_with_kinds0.(field) in
                let field_scope_level = E.continuation_scope_level env in
                (* CR mshinwell: We could refine the types of the actual fields
                   themselves according to the tag. *)
                let field_ty = T.unknown kind in
                env := T.Typing_environment.add !env field field_scope_level
                  field_ty
              done;
              !env)
            tags_to_sizes
        in
        let discriminant_env_is_int =
          T.Typing_environment.add discriminant
            (E.continuation_scope_level env)
            (T.these_tags by_constant_ctor_index)
        in
        let discriminant_env_is_block =
          T.Typing_environment.add discriminant
            (E.continuation_scope_level env)
            (T.these_tags by_tag)
        in
        let by_is_int_result =
          Immediate.Map.of_list [
            Immediate.const_true, discriminant_env_is_int;
            Immediate.const_false, discriminant_env_is_block;
          ]
        in
        T.these_tagged_immediates_with_envs by_is_int_result
      in
      let is_int = Parameter.wrap is_int in
      [Flambda.Typed_parameter.create is_int is_int_ty]
  in
  let discriminant =
(* We probably still need this now in all cases to get the extra type info,
   or else we need another special case in here to refine the type of
   [unboxee] unilaterally.
    if not needs_discriminant then []
    else [discriminant, Projection.Prim (Pgettag, [unboxee])] *)
    let discriminant = Parameter.wrap discriminant in
    Flambda.Typed_parameter.create discriminant discriminant_ty
  in
  let fields =
    List.map (fun (field, kind) ->
        let field = Parameter.wrap field in
        Flambda.Typed_parameter.create field KIND)
      fields_with_kinds
  in
  let 
  { unboxee_to_wrapper_params_unboxee;
    add_bindings_in_wrapper;
    new_arguments_for_call_in_wrapper;
    new_type_for_unboxees = [unboxee, unboxee_ty];
    new_params = is_int @ discriminant @ fields;
    new_unboxee_types = [unboxee, unboxee_ty];
    build_boxed_value_from_new_params;
  }

let how_to_unbox ~type_of_name ~env ~unboxee ~unboxee_ty ~unbox_returns =
  match T.prove_blocks_and_immediates unboxee_ty with
  | Wrong -> None
  | Ok blocks_and_immediates ->
    (* Don't unbox a given variable more than once. *)
    match blocks_and_immediates.is_int with
    | Some _ -> None
    | None ->
      let constant_ctors =
        match approx with
        | Blocks _ -> Numbers.Int.Set.empty
        | Blocks_and_immediates (_, imms) | Immediates imms ->
          let module I = T.Unionable.Immediate in
          I.Set.fold (fun (approx : I.t) ctor_indexes ->
              let ctor_index =
                match approx with
                | Int i -> i
                | Char c -> Char.code c
                | Constptr p -> p
              in
              Numbers.Int.Set.add ctor_index ctor_indexes)
            imms
            Numbers.Int.Set.empty
      in
      let blocks =
        match approx with
        | Blocks blocks | Blocks_and_immediates (blocks, _) -> blocks
        | Immediates _ -> Tag.Map.empty
      in
      (* CR mshinwell: This is sometimes returning "new_params" being empty;
         this should be an error presumably *)
      if Tag.Map.is_empty blocks then None
      else
        Some (how_to_unbox_core ~constant_ctors ~blocks ~unboxee
          ~unbox_returns)

(* Some new ideas
module Unboxable = struct
  type immediate_valued =
    | Yes of { unique_known_value : Immediate.t option; }
    | No

  module encoded_or_boxed = struct
    type how_to_create =
      | Call_external of { function_name : string; }
      | Allocate of {
          sizes_by_tag : int Tag.Map.t;
          max_size : int;
        }

    type t = {
      how_to_create : how_to_create;
      arity : Flambda_kind.t list;
      projection : (field:int -> Projection.t);
      projection_code : (field:int -> Flambda0.Named.t);
    }

    let how_to_create t = t.how_to_create
    let arity t = t.arity
    let projection t ~field = t.projection ~field
    let projection_code t ~field = t.projection_code ~field
  end

  type encoded_or_boxed =
    | Yes of encoded_or_boxed.t
    | No

  type t = {
    immediate_valued : immediate_valued;
    encoded_or_boxed : encoded_or_boxed;
  }

  let immediate_valued t = t.immediate_valued
  let encoded_or_boxed t = t.encoded_or_boxed

  let check_field_within_range ~field ~max_size =
    if field < 0 || field >= max_size then begin
      Misc.fatal_errorf "Field index %d out of range when forming \
          [Unboxable.t]"
        field
    end

  let create_blocks_internal ~immediate_valued ~sizes_by_tag : t =
    if Tag.Map.cardinal sizes_by_tag < 1 then begin
      Misc.fatal_error "create_blocks_internal: empty [sizes_by_tag]"
    end;
    let max_size = Tag.Set.max_elt (Tag.Map.keys sizes_by_tag) in
    { immediate_valued = No;
      encoded_or_boxed = {
        how_to_create = Allocate_empty { sizes_by_tag; };
        arity = Array.to_list (Array.create max_size (Flambda_kind.value ()));
        projection = (fun ~unboxee ~field : Projection.t ->
          (* This bounds check isn't completely watertight (any particular
             constructor may have fewer arguments than [max_size]), but it's
             better than nothing. *)
          check_field_within_range ~field ~max_size;
          Field (unboxee, field));
        projection_code = (fun ~unboxee ~field dbg : Flambda0.Named.t ->
          check_field_within_range ~field ~max_size;
          Prim (Pfield field, [unboxee], dbg));
      };
    }

  let create_blocks_and_immediates ~unique_immediate_value ~sizes_by_tag =
    create_blocks_internal
      ~immediate_valued:(Yes { unique_known_value = unique_immediate_value; })
      ~tag ~sizes_by_tag

  let create_blocks ~sizes_by_tag =
    create_blocks_internal ~immediate_valued:No ~sizes_by_tag

  let create_boxed_float () : t =
    match Flambda_kind.naked_float () with
    | None -> None
    | Some naked_float_kind ->
      { immediate_valued = No;
        encoded_or_boxed = {
          how_to_create = Allocate_and_fill Pbox_float;
          arity = [naked_float_kind];
          projection = (fun ~unboxee ~field : Projection.t ->
            check_field_within_range ~field ~max_size:1;
            Prim (Punbox_float, [unboxee]));
          projection_code = (fun ~unboxee ~field dbg : Flambda0.Named.t ->
            check_field_within_range ~field ~max_size:1;
            Prim (Punbox_float, [unboxee], dbg));
        };
      }

  let create_boxed_int32 () : t =
    { immediate_valued = No;
      encoded_or_boxed = {
        how_to_create = Allocate_and_fill Pbox_int32;
        arity = [Flambda_kind.naked_int32 ()];
        projection = (fun ~unboxee ~field : Projection.t ->
          check_field_within_range ~field ~max_size:1;
          Prim (Punbox_int32, [unboxee]));
        projection_code = (fun ~unboxee ~field dbg : Flambda0.Named.t ->
          check_field_within_range ~field ~max_size:1;
          Prim (Punbox_int32 field, [unboxee], dbg));
      };
    }

  let create_boxed_int64 () : t =
    match Flambda_kind.naked_int64 () with
    | None -> None
    | Some naked_int64_kind ->
      { immediate_valued = No;
        encoded_or_boxed = {
          how_to_create = Allocate_and_fill Pbox_int64;
          arity = [naked_int64_kind];
          projection = (fun ~unboxee ~field : Projection.t ->
            check_field_within_range ~field ~max_size:1;
            Prim (Punbox_int64, [unboxee]));
          projection_code = (fun ~unboxee ~field dbg : Flambda0.Named.t ->
            check_field_within_range ~field ~max_size:1;
            Prim (Punbox_int64 field, [unboxee], dbg));
        };
      }

  let create_boxed_nativeint () : t =
    { immediate_valued = No;
      encoded_or_boxed = {
        how_to_create = Allocate_and_fill Pbox_nativeint;
        arity = [Flambda_kind.naked_nativeint ()];
        projection = (fun ~unboxee ~field : Projection.t ->
          check_field_within_range ~field ~max_size:1;
          Prim (Punbox_nativeint, [unboxee]));
        projection_code = (fun ~unboxee ~field dbg : Flambda0.Named.t ->
          check_field_within_range ~field ~max_size:1;
          Prim (Punbox_nativeint, [unboxee], dbg));
      };
    }

  let create_tagged_immediate () : t =
    { immediate_valued = No;
      encoded_or_boxed = {
        how_to_create = Allocate_and_fill Ptag_int;
        arity = [Flambda_kind.naked_immediate ()];
        projection = (fun ~unboxee ~field : Projection.t ->
          check_field_within_range ~field ~max_size:1;
          Prim (Puntag_immediate, [unboxee]));
        projection_code = (fun ~unboxee ~field dbg : Flambda0.Named.t ->
          check_field_within_range ~field ~max_size:1;
          Prim (Puntag_immediate, [unboxee], dbg));
      };
    }
end

module Unboxable_or_untaggable : sig
  (** Witness that values of a particular Flambda type may be unboxed or
      untagged.  We call the contents of such values the "constitutuents"
      of the value.  (For example, each boxed float value has a naked
      float constitutent; each tagged immediate has a naked immediate
      constituent; a pair has two constituents of kind [Value].)  Constituents
      of values are ordered (following field numbers for blocks) starting at
      zero.

      The functions in this module provide a basic abstraction over unboxing
      and untagging which can be built on to perform unboxing transformations
      (cf. [Unbox_one_variable]).
  *)

  type how_to_create = private
    | Allocate_and_fill of Flambda_primitive.t
    (** The boxed or encoded value is to be completely constructed using the
        given primitive.  The constituents of the value are specified as the
        usual [Variable.t]s in the [Prim] term (cf. [Flambda0.Named.t]). *)
    | Allocate_empty of {
        sizes_by_tag : int Tag.Map.t;
      }
    (** The value is to be allocated, according to the desired tag, using
        [Pmakeblock]---but the caller is responsible for filling it. *)

  (** For each constituent of the value, in order, which value kind is required
      to represent that component.  When unboxing variants the arity
      corresponds to the maximum number of fields across all possible
      tags. *)
  val arity : t -> Flambda_kind.t list

  (** Values of variant type with mixed constant and non-constant
      constructors take on immediate values in addition to boxed values.
      Such immediate values are returned by this function.  (Note that this
      is unrelated to immediate values that might be taken on by a variable
      that always holds tagged immediates and is being untagged.  That case
      is one of those for which this function returns [None].) *)
  val forms_union_with_immediates : t -> Immediate.Set.t option

  (** The [Projection.t] value that describes the given projection out of
      the block. *)
  val projection
     : t
    -> unboxee:Variable.t
    -> field:int
    -> Projection.t

  (** The code required to perform the given projection out of the block. *)
  val projection_code
     : t
    -> unboxee:Variable.t
    -> field:int
    -> Debuginfo.t
    -> Flambda0.Named.t
end
*)


(* We'll want this at some point
    let num_words_allocated_excluding_header t =
      let custom_block_size = 2 in
      match t with
      | Encoded Tagged_int -> 0
      | Boxed Float ->
        begin match Targetint.num_bits with
        | Thirty_two -> 2
        | Sixty_four -> 1
        end
      | Boxed Int32 -> custom_block_size + 1
      | Boxed Int64 ->
        begin match Targetint.num_bits with
        | Thirty_two -> custom_block_size + 2
        | Sixty_four -> custom_block_size + 1
        end
      | Boxed Nativeint -> custom_block_size + 1
*)
