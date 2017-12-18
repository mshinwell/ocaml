(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module B = Inlining_cost.Benefit
module E = Simplify_env_and_result.Env
module K = Flambda_kind
module R = Simplify_env_and_result.Result
module T = Flambda_type

module Float_by_bit_pattern = Numbers.Float_by_bit_pattern
module Int = Numbers.Int
module Named = Flambda.Named
module Reachable = Flambda.Reachable

type named_simplifier =
  (Variable.t * Named.t) list * Reachable.t
    * Flambda_type.t * R.t

(* To fix with Pierre

(* Simplify an expression that takes a set of closures and projects an
   individual closure from it. *)
let simplify_project_closure env r
      ~(project_closure : Projection.Project_closure.t) : named_simplifier =
  let set_of_closures, set_of_closures_ty =
    freshen_and_squash_aliases env project_closure.set_of_closures
  in
  let closure_id = project_closure.closure_id in
  let importer = E.importer env in
  match T.prove_set_of_closures ~importer set_of_closures_ty with
  | Wrong ->
    let ty = Flambda_type.bottom (K.value Must_scan) in
    let term = Reachable.invalid () in
    [], ty, term
  | Unknown ->
    let ty = Flambda_type.bottom (K.value Must_scan) in
    let term =
      Reachable.reachable (Project_closure {
        set_of_closures;
        closure_id;
      })
    in
    [], ty, term
  | Known set ->
(*
    begin match Closure_id.Set.elements closure_id with
      | _ :: _ :: _ ->
        Format.printf "Set of closures type is not a singleton \
            in project closure@ %a@ %a@."
          T.print set_of_closures_type
          Projection.print_project_closure project_closure
      | [] ->
        Format.printf "Set of closures type is empty in project \
            closure@ %a@ %a@."
          T.print set_of_closures_type
          Projection.print_project_closure project_closure
      | _ ->
        ()
    end;
*)
    let projecting_from =
      match Flambda_type.Set_of_closures.set_of_closures_var set with
      | None -> None
      | Some set_of_closures_var ->
        let projection : Projection.t =
          Project_closure {
            set_of_closures = set_of_closures_var;
            closure_id;
          }
        in
        match E.find_projection env ~projection with
        | None -> None
        | Some var -> Some (var, projection)
    in
    match projecting_from with
    | Some (var, projection) ->
      let var, var_ty = freshen_and_squash_aliases env var in
      let r = R.map_benefit r (B.remove_projection projection) in
      if Flambda_type.is_bottom ~importer var_ty then
        [], Reachable.invalid (), r
      else
        [], Reachable.reachable (Var var), r
    | None ->
      assert false
(* XXX for pchambart to fix: 
      let if_not_reference_recursive_function_directly ()
        : (Variable.t * Named.t) list * Named.t_reachable
            * R.t =
        let set_of_closures_var =
          match set_of_closures_var with
          | Some set_of_closures_var' when E.mem env set_of_closures_var' ->
            set_of_closures_var
          | Some _ | None -> None
        in
        let ty =
          T.closure ?set_of_closures_var
            (Closure_id.Map.of_set (fun _ -> value_set_of_closures)
                closure_id)
        in
        [], Reachable (Project_closure { set_of_closures; closure_id; }),
          ty
      in
      match Closure_id.Set.get_singleton closure_id with
      | None ->
        if_not_reference_recursive_function_directly ()
      | Some closure_id ->
        match reference_recursive_function_directly env closure_id with
        | Some (flam, ty) -> [], Reachable flam, ty
        | None ->
          if_not_reference_recursive_function_directly ()
*)

(* Simplify an expression that, given one closure within some set of
   closures, returns another closure (possibly the same one) within the
   same set. *)
let simplify_move_within_set_of_closures env r
      ~(move_within_set_of_closures : Projection.Move_within_set_of_closures.t)
      : named_simplifier =
  let closure, closure_ty =
    freshen_and_squash_aliases env move_within_set_of_closures.closure
  in
  match T.prove_closure_allowing_unresolved closure_ty with
  | Wrong ->
    Misc.fatal_errorf "Wrong Flambda type when moving within set of \
        closures.  Flambda type: %a  Term: %a"
      T.print closure_ty
      Flambda.print_move_within_set_of_closures move_within_set_of_closures
  | Unresolved sym ->
    [], Reachable (Move_within_set_of_closures {
        closure;
        move = move_within_set_of_closures.move;
      }),
      ret r (T.unresolved_symbol sym)
  | Unknown ->
    [], Reachable (Move_within_set_of_closures {
        closure;
        move = move_within_set_of_closures.move;
      }),
      ret r (T.unknown Value Other)
  | Unknown_because_of_unresolved_value value ->
    (* For example: a move upon a (move upon a closure whose .cmx file
        is missing). *)
    [], Reachable (Move_within_set_of_closures {
        closure;
        move = move_within_set_of_closures.move;
      }),
      ret r (T.unknown Value (Unresolved_value value))
  | Ok (value_closures, set_of_closures_var, set_of_closures_symbol) ->
    let () =
      match Closure_id.Map.bindings value_closures with
      | _ :: _ :: _ ->
        Format.printf "Closure type is not a singleton in \
            move@ %a@ %a@."
          T.print closure_ty
          Projection.print_move_within_set_of_closures
          move_within_set_of_closures
      | [] ->
        Format.printf "Closure type is empty in move@ %a@ %a@."
          T.print closure_ty
          Projection.print_move_within_set_of_closures
          move_within_set_of_closures
      | _ ->
        ()
    in
    (* Freshening of the move. *)
    let move, type_map =
      Closure_id.Map.fold
        (fun closure_id_in_type
              (value_set_of_closures:T.set_of_closures)
              (move, type_map) ->
          (* Pas efficace: on refait le freshening de tout pour ne
             garder que la partie pertinente, mais n'est pas très
             grave parce que ces map sont petites (normalement) *)
          let freshened_move =
            Freshening.freshen_move_within_set_of_closures
              ~closure_freshening:value_set_of_closures.freshening
              move_within_set_of_closures.move
          in
          let start_from = closure_id_in_type in
          let move_to =
            try Closure_id.Map.find start_from freshened_move with
            | Not_found ->
              Misc.fatal_errorf "Move %a freshened to %a does not contain \
                                  projection for %a@.  Type is:@ %a@.\
                                  Environment:@ %a@."
                Projection.print_move_within_set_of_closures
                  move_within_set_of_closures
                (Closure_id.Map.print Closure_id.print) freshened_move
                Closure_id.print start_from
                (Closure_id.Map.print T.print_value_set_of_closures)
                  value_closures
                E.print env
          in
          assert(not (Closure_id.Map.mem start_from move));
          Closure_id.Map.add start_from move_to move,
          Closure_id.Map.add move_to value_set_of_closures type_map)
        value_closures (Closure_id.Map.empty, Closure_id.Map.empty)
    in
    let projection : Projection.t =
      Move_within_set_of_closures {
        closure;
        move;
      }
    in
    match Closure_id.Map.get_singleton value_closures,
          Closure_id.Map.get_singleton move with
    | None, Some _ | Some _, None ->
      (* After the freshening, move and value_closures have the same
         cardinality *)
      assert false
    | None, None ->
      let ty = T.closure ty_map in
      let move_within : Projection.Move_within_set_of_closures.t =
        { closure; move; }
      in
      [], Reachable (Move_within_set_of_closures move_within), ty
    | Some (_start_from, value_set_of_closures),
      Some (start_from, move_to) ->
      match E.find_projection env ~projection with
      | Some var ->
        freshen_and_squash_aliases_named env var ~f:(fun _env var var_ty ->
          let r = R.map_benefit r (B.remove_projection projection) in
          [], Reachable (Var var), var_ty)
      | None ->
        match reference_recursive_function_directly env move_to with
        | Some (flam, ty) -> [], Reachable flam, ty
        | None ->
          if Closure_id.equal start_from move_to then
            (* Moving from one closure to itself is a no-op.  We can return an
               [Var] since we already have a variable bound to the closure. *)
            [], Reachable (Var closure), closure_ty
          else
            match set_of_closures_var with
            | Some set_of_closures_var when E.mem env set_of_closures_var ->
              (* A variable bound to the set of closures is in scope,
                 meaning we can rewrite the [Move_within_set_of_closures] to a
                 [Project_closure]. *)
              let project_closure : Projection.Project_closure.t =
                { set_of_closures = set_of_closures_var;
                  closure_id = Closure_id.Set.singleton move_to;
                }
              in
              let ty =
                T.closure ~set_of_closures_var
                  (Closure_id.Map.singleton move_to value_set_of_closures)
              in
              [], Reachable (Project_closure project_closure), ty
            | Some _ | None ->
              match set_of_closures_symbol with
              | Some set_of_closures_symbol ->
                let set_of_closures_var = Variable.create "symbol" in
                let project_closure : Projection.Project_closure.t =
                  { set_of_closures = set_of_closures_var;
                    closure_id = Closure_id.Set.singleton move_to;
                  }
                in
                let ty =
                  T.closure ~set_of_closures_var ~set_of_closures_symbol
                    (Closure_id.Map.singleton move_to value_set_of_closures)
                in
                let bindings : (Variable.t * Named.t) list = [
                  set_of_closures_var, Symbol set_of_closures_symbol;
                ]
                in
                bindings, Reachable (Project_closure project_closure),
                  ty
              | None ->
                (* The set of closures is not available in scope, and we
                  have no other information by which to simplify the move. *)
                let move_within : Projection.Move_within_set_of_closures.t =
                  { closure; move; }
                in
                let ty = T.closure ty_map in
                [], Reachable (Move_within_set_of_closures move_within), ty

let rec simplify_project_var env r ~(project_var : Projection.Project_var.t)
      : named_simplifier =
  let closure, closure_ty =
    freshen_and_squash_aliases env project_var.closure
  in
  match T.prove_closure_allowing_unresolved closure_ty with
  | Ok (value_closures, _set_of_closures_var, _set_of_closures_symbol) ->
(*
    let () =
      match Closure_id.Map.bindings value_closures with
        | _ :: _ :: _ ->
          Format.printf "Closure type is not a singleton in \
              project@ %a@ %a@."
            T.print ty
            Projection.print_project_var project_var
        | [] ->
          Format.printf "Closure type is empty in project@ %a@ %a@."
            T.print ty
            Projection.print_project_var project_var
        | _ ->
          ()
    in
*)
    (* Freshening of the projection *)
    let project_var_var, ty =
      Closure_id.Map.fold
        (fun closure_id_in_type
          (value_set_of_closures : T.set_of_closures)
          (project_var_var, set_type) ->
          let freshened_var =
            Freshening.freshen_project_var project_var.var
              ~closure_freshening:value_set_of_closures.freshening
          in
          let closure_id = closure_id_in_type in
          let var =
            try Closure_id.Map.find closure_id_in_type freshened_var with
            | Not_found ->
              Misc.fatal_errorf "When simplifying [Project_var], the \
                closure ID %a in the type of the set of closures \
                did not match any closure ID in the [Project_var] term %a \
                freshened to %a. \
                Type: %a@."
                Closure_id.print closure_id_in_type
                Projection.print_project_var project_var
                (Closure_id.Map.print Var_within_closure.print) freshened_var
                Flambda_type.print ty
          in
          let set_type =
            let ty = T.type_for_bound_var value_set_of_closures var in
            let really_import_type = E.really_import_type env in
            T.join ~really_import_type ty set_type
          in
          Closure_id.Map.add closure_id var project_var_var,
          set_type)
        value_closures (Closure_id.Map.empty, T.bottom)
    in
    let projection : Projection.t =
      Project_var {
        closure;
        var = project_var_var;
      }
    in
    begin match E.find_projection env ~projection with
    | Some var ->
      freshen_and_squash_aliases_named env var ~f:(fun _env var var_ty ->
        let r = R.map_benefit r (B.remove_projection projection) in
        [], Reachable (Var var), var_ty)
    | None ->
      let expr : Named.t =
        Project_var { closure; var = project_var_var; }
      in
      let expr =
        match Closure_id.Map.get_singleton project_var_var with
        | None ->
          expr
        | Some (_closure_id, var) ->
          let unwrapped = Var_within_closure.unwrap var in
          if E.mem env unwrapped then
            Flambda.Var unwrapped
          else
            expr
      in
      simpler_equivalent_term env r expr ty
    end
  | Unknown ->
    [], Reachable.reachable (Project_var { project_var with closure }),
      r
  | Wrong ->
    [], Reachable.invalid (), r

*)

let simplify_duplicate_block env r prim arg dbg
      (kind : Flambda_primitive.make_block_kind)
      ~source_mutability:_ ~destination_mutability =
  let arg, ty = S.simplify_simple env arg in
  let original_term () : Named.t = Prim (Unary (prim, arg), dbg) in
  let kind_of_block = K.value Definitely_pointer in
  let full_of_values ~template =
    let proof = (E.type_accessor env T.prove_block) ty in
    match proof with
    | Proved blocks ->
      let new_block_tys =
        T.Blocks.fold blocks
          ~init:[]
          ~f:(fun new_block_tys block ->
            let module B = T.Blocks.Block in
            let tag = B.tag block in
            if not (Tag.Scannable.equal tag new_tag) then
              new_block_tys
            else
              let ty =
                match destination_mutability with
                | Mutable ->
                  let field_tys = B.fields block in
                  T.block tag (T.unknown_like_array field_tys)
                | Immutable ->
                  B.to_type block
              in
              let ty = (E.type_accessor env T.meet_of_kind_value) ty template in
              ty :: new_block_tys)
      in
      let type_of_new_block = (E.type_accessor env T.join) new_block_tys in
      Reachable.reachable (original_term ()), type_of_new_block
    | Unknown ->
      let type_of_new_block =
        let ty =
          match destination_mutability with
          | Mutable -> T.unknown kind_of_block Other
          | Immutable -> ty
        in
        (E.type_accessor env T.meet_of_kind_value) ty template
      in
      Reachable.reachable (original_term ()), type_of_new_block
    | Invalid ->
      Reachable.invalid (), T.bottom kind_of_block
  in
  let full_of_naked_floats ~template =
    let proof = (E.type_accessor env T.prove_float_array) ty in
    match proof with
    | Proved arrays ->
      let new_block_tys =
        Targetint.OCaml.Set.fold (fun fields new_block_tys ->
            let size = Array.length fields in
            let ty =
              match destination_mutability with
              | Mutable -> T.mutable_float_array ~size
              | Immutable -> T.immutable_float_array fields
            in
            let ty =
              (E.type_accessor env T.meet_of_kind_naked_float) ty template
            in
            ty :: new_block_tys)
          arrays
          []
      in
      let type_of_new_block = (E.type_accessor env T.join) new_block_tys in
      Reachable.reachable (original_term ()), type_of_new_block
    | Unknown ->
      let type_of_new_block =
        let ty =
          match destination_mutability with
          | Mutable -> T.unknown kind_of_block Other
          | Immutable -> ty
        in
        (E.type_accessor env T.meet_of_kind_value) ty template
      in
      Reachable.reachable (original_term ()), type_of_new_block
    | Invalid ->
      Reachable.invalid (), T.bottom kind_of_block
  in
  let term, ty =
    match kind with
    | Full_of_values_known_length (new_tag, new_value_kinds) ->
      let unknown_type_of_new_block =
        T.constraint_block new_tag new_value_kinds
      in
      full_of_values ~template:(Some unknown_type_of_new_block)
    | Full_of_values_unknown_length (new_tag, new_value_kind) ->
      full_of_values ~template:None
    | Full_of_naked_floats { length; } ->
      let size_constraint =
        match length with
        | None -> None
        | Some size -> Some (T.constraint_float_array ~size)
      in
      full_of_naked_floats ~template:size_constraint
    | Generic_array _ -> assert false
      (* To finish later.  (Also, evict to [Simplify_generic_array].) *)
  in
  term, ty, r

let simplify_is_int env r prim arg dbg =
  let arg, ty = S.simplify_simple env arg in
  let original_term () : Named.t = Prim (Unary (prim, arg), dbg) in
  let proof = (E.type_accessor env T.prove_is_tagged_immediate) ty in
  let term, ty =
    match proof with
    | Proved is_tagged_immediate ->
      let simple = Simple.const_bool is_tagged_immediate in
      let imm = Immediate.const_bool is_tagged_immediate in
      Reachable.reachable (Simple simple), T.this_tagged_immediate imm
    | Unknown ->
      Reachable.reachable (original_term ()),
        T.these_tagged_immediates Immediate.all_bools
    | Invalid -> 
      Reachable.invalid (), T.bottom (K.value Definitely_immediate)
  in
  term, ty, r

let simplify_get_tag env r ~result_var prim ~tags_to_sizes ~block dbg =
  let block, block_type = S.simplify_simple env block in
  let inferred_tags = (E.type_accessor env T.possible_tags) ty in
  let possible_tags = Tag.Map.keys tags_to_sizes in
  match inferred_tags with
  | Exactly inferred_tags ->
    let tags = Tag.Set.inter inferred_tags possible_tags in
    let r = R.map_benefit (B.remove_primitive Get_tag) r in
    if Tag.Set.is_empty tags then
      Reachable.invalid (), T.bottom (K.fabricated Definitely_immediate), r
    else
      let tags_to_sizes =
        Tag.Map.filter (fun tag -> Tag.Set.mem inferred_tags) tags_to_sizes
      in
      assert (not (Tag.Map.is_empty tags_to_sizes));
      let prim : Flambda_primitive.unary_primitive = Get_tag tags_to_sizes in
      let term : Named.t = Prim (Unary (prim, block), dbg) in
      term, T.these_tags tags, r
  | Unknown ->
    let block_field_kind = K.value Unknown in
    let tags_to_block_cases =
      Tag.Map.fold (fun tag size tags_to_block_cases ->
          let block_case =
            T.block_case ~env:(T.Type_environment.create ())
              ~fields:(T.this_many_unknowns size block_field_kind)
          in
          Tag.Map.add tag block_case tags_to_block_cases)
        tags_to_sizes
        Tag.Map.empty
    in
    let block_type_refinement =
      T.blocks ~tag:(Var result_var) ~tags_to_block_cases
    in
    let block_type =
      (E.type_accessor env T.meet) block_type block_type_refinement
    in
    let r = R.add_typing_judgement r block block_type in
    let prim : Flambda_primitive.unary_primitive = Get_tag tags_to_sizes in
    let term : Named.t = Prim (Unary (prim, block), dbg) in
    term, T.these_tags possible_tags, r

module type For_standard_ints = sig
  module Num : sig
    include Identifiable.S

    val swap_byte_endianness : t -> t

    val to_const : t -> Simple.Const.t

    val to_tagged_immediate : t -> Immediate.t
    val to_naked_int32 : t -> Int32.t
    val to_naked_int64 : t -> Int64.t
    val to_naked_nativeint : t -> Targetint.t
  end

  val kind : K.Standard_int.t
  val prover : (T.t -> Num.Set.t proof) T.type_accessor
  val this : Num.t -> T.t
  val these : Num.Set.t -> T.t
end

module Make_simplify_swap_byte_endianness (P : For_standard_ints) = struct
  let simplify env r prim arg dbg =
    let arg, ty = S.simplify_simple env arg in
    let original_term () : Named.t = Prim (Unary (prim, arg), dbg) in
    let proof = (E.type_accessor env P.prover) ty in
    let kind = K.Standard_int.to_kind P.kind in
    let term, ty =
      match proof with
      | Proved nums ->
        let nums =
          P.Set.map (fun imm -> P.swap_byte_endianness imm) nums
        in
        Reachable.reachable (original_term ()), P.these nums
      | Proved Not_all_values_known ->
        Reachable.reachable (original_term ()), T.unknown kind Other
      | Invalid ->
        Reachable.invalid (), T.bottom kind
    in
    term, ty, r
end

module For_standard_ints_tagged_immediate : For_standard_ints = struct
  module Num = struct
    include Immediate

    let swap_byte_endianness t =
      Immediate.map (fun i ->
          Targetint.get_least_significant_16_bits_then_byte_swap i)
        t

    let to_const t = Simple.Const.Tagged_immediate t

    let to_tagged_immediate t = t

    let to_naked_int32 t = Targetint.to_int32 (Immediate.to_targetint t)
    let to_naked_int64 t = Targetint.to_int64 (Immediate.to_targetint t)
    let to_naked_nativeint t = Immediate.to_targetint t

    (* It seems as if the various [float_of_int] functions never raise
       an exception even in the case of NaN or infinity. *)
    (* CR mshinwell: We should be sure this semantics is reasonable. *)
    let to_naked_float t =
      Float_by_bit_pattern.of_bits (to_naked_int64 t)
  end

  let kind : K.Standard_int.t = Tagged_immediate
  let or_float_kind : K.Standard_int_or_float.t = Tagged_immediate
  let prover = T.prove_tagged_immediate
  let this = T.this_tagged_immediate
  let these = T.these_tagged_immediates
end

module For_standard_ints_naked_int32 : For_standard_ints = struct
  module Num = struct
    include Int32

    let to_const t = Simple.Const.Naked_int32 t

    let to_tagged_immediate t =
      Immediate.int (Targetint.treat_as_int (Targetint.of_int32 t))
    let to_naked_int32 t = t
    let to_naked_int64 t = Int64.of_int32 t
    let to_naked_nativeint t = Targetint.of_int64 t
  end

  let kind : K.Standard_int.t = Naked_int32
  let or_float_kind : K.Standard_int_or_float.t = Naked_int32
  let prover = T.prove_naked_int32
  let this = T.this_naked_int32
  let these = T.these_naked_int32s
end

module For_standard_ints_naked_int64 : For_standard_ints = struct
  module Num = struct
    include Int64

    let to_const t = Simple.Const.Naked_int64 t

    let to_tagged_immediate t =
      Immediate.int (Targetint.treat_as_int (Targetint.of_int64 t))
    let to_naked_int32 t = Int64.to_int32 t
    let to_naked_int64 t = t
    let to_naked_nativeint t = Targetint.of_int64 t
  end

  let kind : K.Standard_int.t = Naked_int64
  let or_float_kind : K.Standard_int_or_float.t = Naked_int64
  let prover = T.prove_naked_int64
  let this = T.this_naked_int64
  let these = T.these_naked_int64s
end

module For_standard_ints_naked_nativeint : For_standard_ints = struct
  module Num = struct
    include Targetint

    let to_const t = Simple.Const.Naked_nativeint t

    let to_tagged_immediate t = Immediate.int (Targetint.treat_as_int t)
    let to_naked_int32 t = Targetint.to_int32
    let to_naked_int64 t = Targetint.to_int64
    let to_naked_nativeint t = t
  end

  let kind : K.Standard_int.t = Naked_nativeint
  let or_float_kind : K.Standard_int_or_float.t = Naked_nativeint
  let prover = T.prove_naked_nativeint
  let this = T.this_naked_nativeint
  let these = T.these_naked_nativeints
end

module Simplify_swap_byte_endianness_tagged_immediate =
  Make_simplify_swap_byte_endianness (For_standard_ints_tagged_immediate)

module Simplify_swap_byte_endianness_naked_int32 =
  Make_simplify_swap_byte_endianness (For_standard_ints_naked_int32)

module Simplify_swap_byte_endianness_naked_int64 =
  Make_simplify_swap_byte_endianness (For_standard_ints_naked_int64)

module Simplify_swap_byte_endianness_naked_nativeint =
  Make_simplify_swap_byte_endianness (For_standard_ints_naked_nativeint)

module type For_unboxable_ints = sig
  module Num : sig
    include Identifiable.S

    val swap_byte_endianness : t -> t
    val to_const : t -> Simple.Const.t
  end

  val kind : K.Boxable_number.t
  val prover : (T.t -> Num.Set.t proof) T.type_accessor

  val this : Num.t -> T.t
  val these : Num.Set.t -> T.t
  val box : T.t -> T.t
end

module Make_simplify_unbox_number (P : For_unboxable_ints) = struct
  let simplify env r prim arg dbg =
    let arg, ty = S.simplify_simple env arg in
    let original_term () : Named.t = Prim (Unary (prim, arg), dbg) in
    let proof = (E.type_accessor env P.boxed_prover) ty in
    let kind = K.Boxable_number.to_kind P.kind in
    let term, ty =
      match proof with
      | Proved unboxed_ty ->
        let proof = (E.type_accessor env P.unboxed_prover) unboxed_ty in
        begin match proof with
        | Proved nums ->
          Reachable.reachable (original_term ()), P.these nums
        | Unknown ->
          Reachable.reachable (original_term ()), unboxed_ty
        | Invalid ->
          Reachable.invalid (), T.bottom kind
        end
      | Invalid -> 
        Reachable.invalid (), T.bottom kind
    in
    term, ty, r
end

module Simplify_unbox_number_float =
  Make_simplify_unbox_number (struct
    module Num = struct
      include Float
      let to_const t = Simple.Const.Naked_float t
    end

    let kind : K.Boxable_number.t = Naked_float
    let boxed_prover = T.prove_boxed_float
    let unboxed_prover = T.prove_naked_float
    let this = T.this_naked_float
    let these = T.these_naked_floats
  end)

module Simplify_unbox_number_int32 =
  Make_simplify_unbox_number (struct
    module Num = struct
      include Int32
      let to_const t = Simple.Const.Naked_int32 t
    end

    let kind : K.Boxable_number.t = Naked_int32
    let boxed_prover = T.prove_boxed_int32
    let unboxed_prover = T.prove_naked_int32
    let this = T.this_naked_int32
    let these = T.these_naked_int32s
  end)

module Simplify_unbox_number_int64 =
  Make_simplify_unbox_number (struct
    module Num = struct
      include Int64
      let to_const t = Simple.Const.Naked_int64 t
    end

    let kind : K.Boxable_number.t = Naked_int64
    let boxed_prover = T.prove_boxed_int64
    let unboxed_prover = T.prove_naked_int64
    let this = T.this_naked_int64
    let these = T.these_naked_int64s
  end)

module Simplify_unbox_number_nativeint =
  Make_simplify_unbox_number (struct
    module Num = struct
      include Nativeint
      let to_const t = Simple.Const.Naked_nativeint t
    end

    let kind : K.Boxable_number.t = Naked_nativeint
    let boxed_prover = T.prove_boxed_nativeint
    let unboxed_prover = T.prove_naked_nativeint
    let this = T.this_naked_nativeint
    let these = T.these_naked_nativeints
  end)

(* CR mshinwell:
   1. Overlap with Lift_constants?
   2. Work out how to make use of the projection information, e.g. for
      boxing/unboxing.
   3. Where there are aliases e.g. x = 3 and x = 5, we don't emit a term
      "x" even if we emit "3 union 5" as the type.
*)

module Make_simplify_box_number (P : For_unboxable_ints) = struct
  let simplify env r prim arg dbg =
    (* CR mshinwell: If [arg] is already a [Const] we shouldn't have to do
       much work... *)
    let arg, ty = S.simplify_simple env arg in
    let original_term () : Named.t = Prim (Unary (prim, arg), dbg) in
    let proof = (E.type_accessor env P.unboxed_prover) ty in
    let kind = K.Boxable_number.to_kind P.kind in
    match proof with
    | Proved nums ->
      begin match P.Num.Set.get_singleton nums with
      | Some n ->
        let symbol, r = R.new_lifted_constant r env (Boxed_float (Const n)) in
        let r = R.map_benefit r (Inlining_cost.Benefit.remove_box P.kind) in
        let named : Named.t = Simple (Simple.name (Name.symbol symbol)) in
        Reachable.reachable named, P.this n, r
      | None ->
        assert (not (P.Num.Set.is_empty nums));
        Reachable.reachable (original_term ()), P.these nums, r
      end
    | Proved Not_all_values_known ->
      Reachable.reachable (original_term ()), P.box ty, r
    | Invalid -> 
      Reachable.invalid (), T.bottom kind, r
end

module Simplify_box_number_float =
  Make_simplify_box_number (struct
    module Num = struct
      include Numbers.Float_by_bit_pattern
      let to_const t = Simple.Const.Naked_float t
    end

    let kind : K.Boxable_number.t = Naked_float
    let prover = T.prove_naked_float
    let this = T.this_boxed_float
    let these = T.these_boxed_floats
    let box = T.box_float
  end)

module Simplify_box_number_int32 =
  Make_simplify_box_number (struct
    module Num = struct
      include Int32
      let to_const t = Simple.Const.Naked_int32 t
    end

    let kind : K.Boxable_number.t = Naked_int32
    let prover = T.prove_naked_int32
    let this = T.this_boxed_int32
    let these = T.these_boxed_int32s
    let box = T.box_int32
  end)

module Simplify_box_number_int64 =
  Make_simplify_box_number (struct
    module Num = struct
      include Int64
      let to_const t = Simple.Const.Naked_int64 t
    end

    let kind : K.Boxable_number.t = Naked_int64
    let prover = T.prove_naked_int64
    let this = T.this_boxed_int64
    let these = T.these_boxed_int64s
    let box = T.box_int64
  end)

module Simplify_box_number_nativeint =
  Make_simplify_box_number (struct
    module Num = struct
      include Nativeint
      let to_const t = Simple.Const.Naked_nativeint t
    end

    let kind : K.Boxable_number.t = Naked_nativeint
    let prover = T.prove_naked_nativeint
    let this = T.this_boxed_nativeint
    let these = T.these_boxed_nativeints
    let box = T.box_nativeint
  end)

module Unary_int_arith (I : sig
  type t

  val kind : K.Standard_int.t
  val term : t -> Named.t

  val neg : t -> t

  include Identifiable.S with type t := t

  val this : t -> flambda_type
  val these : Set.t -> flambda_type
end) = struct
  let simplify env r prim dbg (op : Flambda_primitive.unary_int_arith_op) arg =
    let arg, ty = S.simplify_simple env arg in
    let proof = (E.type_accessor env T.prove_tagged_immediate) arg in
    let original_term () : Named.t = Prim (Unary (prim, arg), dbg) in
    let result_unknown () =
      (* One might imagine doing something complicated to [ty] to reflect
         the operation that has happened, but we don't.  As such we cannot
         propagate [ty] and must return "unknown". *)
      Reachable.reachable (original_term ()),
        T.unknown (K.Standard_int.to_kind I.kind) Other
    in
    let result_invalid () =
      Reachable.invalid (),
        T.bottom (K.Standard_int.to_kind I.kind)
    in
    let term, ty =
      match proof with
      | Proved (Exactly ints) ->
        assert (not (I.Set.is_empty ints));
        begin match op with
        | Neg ->
          let possible_results = I.Set.map (fun i -> I.neg i) ints in
          Reachable.reachable (original_term ()), I.these possible_results
        end
      | Proved Not_all_values_known -> result_unknown ()
      | Invalid -> result_invalid ()
    in
    term, ty, r
end

module Unary_int_arith_tagged_immediate = Unary_int_arith (Targetint)
module Unary_int_arith_naked_int32 = Unary_int_arith (Int32)
module Unary_int_arith_naked_int64 = Unary_int_arith (Int64)
module Unary_int_arith_naked_nativeint = Unary_int_arith (Targetint)

module type For_standard_int_or_float = sig
  module Num : sig
    include Identifiable.S

    val to_const : t -> Simple.Const.t

    val to_tagged_immediate : t -> Immediate.t
    val to_naked_float : t -> Float_by_bit_pattern.t
    val to_naked_int32 : t -> Int32.t
    val to_naked_int64 : t -> Int64.t
    val to_naked_nativeint : t -> Targetint.t
  end

  val or_float_kind : K.Standard_int_or_float.t
  val prover : (T.t -> Num.Set.t proof) T.type_accessor
  val this : Num.t -> T.t
  val these : Num.Set.t -> T.t
end

module For_standard_ints_naked_float : For_standard_int_or_float = struct
  module Num = struct
    include Float_by_bit_pattern

    let to_const t = Simple.Const.Naked_float t

    (* CR mshinwell: We need to validate that the backend compiles
       the [Int_of_float] primitive in the same way as
       [Targetint.of_float].  Ditto for [Float_of_int].  (For the record,
       [Pervasives.int_of_float] and [Nativeint.of_float] on [nan] produce
       wildly different results). *)
    let to_tagged_immediate t =
      let f = Float_by_bit_pattern.to_float t in
      Immediate.int (Targetint.OCaml.of_float f)

    let to_naked_float t = t

    let to_naked_int32 t = Int32.of_float (Float_by_bit_pattern.to_float t)

    let to_naked_int64 t = Int64.of_float (Float_by_bit_pattern.to_float t)

    let to_naked_nativeint t =
      Targetint.of_float (Float_by_bit_pattern.to_float t)
  end

  let or_float_kind : K.Standard_int_or_float.t = Naked_float
  let prover = T.prove_naked_float
  let this = T.this_naked_float
  let these = T.these_naked_floats
end

module Make_simplify_int_conv (N : For_standard_int_or_float) = struct
  module F = Float_by_bit_pattern

  let simplify env r prim arg ~(dst : K.Standard_int_or_float.t) dbg =
    let arg, ty = S.simplify_simple env arg in
    if K.Standard_int_or_float.equal N.kind dst then
      if T.is_bottom ty then
        Reachable.invalid (),
          T.bottom (K.Standard_int_or_float.to_kind dst), r
      else
        Reachable.reachable (Flambda.Named.Simple arg), ty, r
    else
      let proof = (E.type_accessor env N.prover) arg in
      let original_term () : Named.t = Prim (Unary (prim, arg), dbg) in
      let module N = N.Num in
      let term, ty =
        match proof with
        | Proved (Exactly is) ->
          assert (N.Set.length is > 0);
          begin match dst with
          | Tagged_immediate ->
            let imms =
              N.Set.fold (fun i imms ->
                  Immediate.Set.add (N.to_tagged_immediate i) imms)
                is
                Immediate.Set.empty
            in
            Reachable.reachable (original_term ()),
              T.these_tagged_immediates imms
          | Naked_float ->
            let is =
              N.Set.fold (fun i is ->
                  F.Set.add (N.to_naked_float i) is)
                is
                F.Set.empty
            in
            Reachable.reachable (original_term ()), T.these_naked_floats is
          | Naked_int32 ->
            let is =
              N.Set.fold (fun i is ->
                  Naked_int32.Set.add (N.to_naked_int32 i) is)
                is
                Naked_int32.Set.empty
            in
            Reachable.reachable (original_term ()), T.these_naked_int32s is
          | Naked_int64 ->
            let is =
              N.Set.fold (fun i is ->
                  Naked_int64.Set.add (N.to_naked_int64 i) is)
                is
                Naked_int64.Set.empty
            in
            Reachable.reachable (original_term ()), T.these_naked_int64s is
          | Naked_nativeint ->
            let is =
              N.Set.fold (fun i is ->
                  Naked_nativeint.Set.add (N.to_naked_nativeint i) is)
                is
                Naked_nativeint.Set.empty
            in
            Reachable.reachable (original_term ()), T.these_naked_nativeints is
          end
        | Proved Not_all_values_known ->
          Reachable.reachable (original_term ()),
            T.unknown (K.Standard_int.to_kind dst) Other
        | Invalid ->
          Reachable.invalid (), T.bottom (K.Standard_int.to_kind dst)
      in
      term, ty, r
end

module Simplify_int_conv_tagged_immediate =
  Make_simplify_int_conv (For_standard_ints_tagged_immediate)
module Simplify_int_conv_naked_float =
  Make_simplify_int_conv (For_standard_ints_naked_float)
module Simplify_int_conv_naked_int32 =
  Make_simplify_int_conv (For_standard_ints_naked_int32)
module Simplify_int_conv_naked_int64 =
  Make_simplify_int_conv (For_standard_ints_naked_int64)
module Simplify_int_conv_naked_nativeint =
  Make_simplify_int_conv (For_standard_ints_naked_nativeint)

let simplify_unary_float_arith_op env r prim
      (op : Flambda_primitive.unary_float_arith_op) arg dbg =
  let module F = Numbers.Float_by_bit_pattern in
  let arg, ty = S.simplify_simple env arg in
  let proof = (E.type_accessor env T.prove_naked_float) arg in
  let original_term () : Named.t = Prim (Unary (prim, arg), dbg) in
  let result_unknown () =
    Reachable.reachable (original_term ()), T.unknown (K.naked_float ()) Other
  in
  let result_invalid () = Reachable.invalid (), T.bottom (K.naked_float ()) in
  let term, ty =
    match proof with
    | Proved (Exactly fs) when E.const_float_prop env ->
      assert (not (F.Set.is_empty fs));
      let possible_results =
        match op with
        | Abs -> F.Set.map (fun f -> F.abs f) fs
        | Neg -> F.Set.map (fun f -> F.neg f) fs
      in
      Reachable.reachable (original_term ()),
        T.these_naked_floats possible_results
    | Proved (Exactly _ | Not_all_values_known) -> result_unknown ()
    | Invalid -> result_invalid ()
  in
  term, ty, r

let simplify_string_length env r prim arg dbg =
  let arg, ty = S.simplify_simple env arg in
  let proof = (E.type_accessor env T.prove_string) arg in
  let original_term () : Named.t = Prim (Unary (prim, arg), dbg) in
  let result_kind = K.value Definitely_immediate in
  let result_invalid () = Reachable.invalid (), T.bottom result_kind in
  let term, ty =
    match proof with
    | Proved (Exactly strs) ->
      assert (T.String_info.Set.cardinal strs > 0);
      let lengths =
        T.String_info.Set.fold (fun str lengths ->
            let size = Immediate.of_int str.size in
            Immediate.Set.add size lengths)
          strs
          Immediate.Set.empty
      in
      Reachable (original_term ()), T.these_tagged_immediates lengths
    | Proved Not_all_values_known ->
      Reachable (original_term ()), T.unknown result_kind Other
    | Invalid -> result_invalid ()
  in
  term, ty, r

(* CR mshinwell: Factorize out together with [simplify_string_length] *)
let simplify_array_length env r prim arg ~array_kind dbg =
  let arg, ty = S.simplify_simple env arg in
  (* XXX this may be wrong: for 32-bit platforms, arrays of floats have
     lengths differing from the lengths of the blocks *)
  let proof = (E.type_accessor env T.lengths_of_arrays_or_blocks) arg in
  let original_term () : Named.t = Prim (Unary (prim, arg), dbg) in
  let result_kind = K.value Definitely_immediate in
  let result_invalid () = Reachable.invalid (), T.bottom result_kind in
  let term, ty =
    match proof with
    | Proved (Exactly lengths) ->
      assert (Targetint.Set.cardinal lengths > 0);
      let lengths =
        Targetint.Set.fold (fun length lengths ->
            let length = Immediate.of_int length in
            Immediate.Set.add length lengths)
          lengths
          Immediate.Set.empty
      in
      Reachable (original_term ()), T.these_tagged_immediates lengths
    | Proved Not_all_values_known ->
      Reachable (original_term ()), T.unknown result_kind Other
    | Invalid -> result_invalid ()
  in
  term, ty, r

let simplify_bigarray_length env r prim bigarray ~dimension dbg =
  let bigarray, ty = S.simplify_simple env bigarray in
  let result_kind = K.value Definitely_immediate in
  let bigarray_proof =
    (E.type_accessor env T.prove_of_kind_value_with_expected_value_kind
      Definitely_pointer) bigarray
  in
  match proof with
  | Proved _ ->
    let named : Named.t = Prim (Unary (prim, bigarray), dbg) in
    named, T.unknown result_kind Other
  | Invalid ->
    Reachable.invalid (), T.bottom result_kind

let simplify_unary_primitive env r ~result_var prim arg dbg =
  match prim with
  | Block_load (field_index, field_kind, field_is_mutable) ->
    simplify_block_load env r prim arg dbg ~field_index ~field_kind
      ~field_is_mutable
  | Duplicate_block { kind; source_mutability;
      destination_mutability; } ->
    simplify_duplicate_block env r prim arg dbg ~kind
      ~source_mutability ~destination_mutability
  | Is_int -> simplify_is_int env r prim arg dbg
  | Get_tag { tags_to_sizes; } ->
    simplify_get_tag env r ~result_var prim ~tags_to_sizes ~block:arg dbg
  | String_length _string_or_bytes ->
    simplify_string_length env r prim arg dbg
  | Swap_byte_endianness Tagged_immediate ->
    Simplify_swap_byte_endianness_tagged_immediate.simplify env r prim arg dbg
  | Swap_byte_endianness Naked_int32 ->
    Simplify_swap_byte_endianness_naked_int32.simplify env r prim arg dbg
  | Swap_byte_endianness Naked_int64 ->
    Simplify_swap_byte_endianness_naked_int64.simplify env r prim arg dbg
  | Swap_byte_endianness Naked_nativeint ->
    Simplify_swap_byte_endianness_naked_nativeint.simplify env r prim arg dbg
  | Int_as_pointer ->
    let arg, _ty = S.simplify_simple env arg in
    (* There is no check on the kind of [arg]: a fatal error resulting from
       such could potentially be triggered by wrong user code. *)
    (* CR mshinwell: think about this some more *)
    Prim (Unary (prim, arg), dbg), T.unknown K.naked_immediate Other
  | Opaque_identity ->
    let arg, ty = S.simplify_simple env arg in
    let kind = (E.type_accessor env T.kind) ty in
    Prim (Unary (prim, arg), dbg), T.unknown kind Other
  | Int_arith (kind, op) ->
    begin match kind with
    | Tagged_immediate ->
      Unary_int_arith_tagged_immediate.simplify env r op arg
    | Naked_int32 ->
      Unary_int_arith_naked_int32.simplify env r op arg
    | Naked_int64 ->
      Unary_int_arith_naked_int64.simplify env r op arg
    | Naked_nativeint ->
      Unary_int_arith_naked_nativeint.simplify env r op arg
    end
  | Num_conv { src = Tagged_immediate; dst; } ->
    Simplify_int_conv_tagged_immediate.simplify env r prim arg ~dst dbg
  | Num_conv { src = Naked_int32; dst; } ->
    Simplify_int_conv_naked_int32.simplify env r prim arg ~dst dbg
  | Num_conv { src = Naked_int64; dst; } ->
    Simplify_int_conv_naked_int64.simplify env r prim arg ~dst dbg
  | Num_conv { src = Naked_nativeint; dst; } ->
    Simplify_int_conv_naked_nativeint.simplify env r prim arg ~dst dbg
  | Float_arith op -> simplify_unary_float_arith_op env r prim op arg dbg
  | Array_length array_kind ->
    simplify_array_length env r prim arg ~array_kind dbg
  | Bigarray_length { dimension : int; } ->
    simplify_bigarray_length env r prim arg ~dimension dbg
  | Unbox_number Naked_float ->
    Simplify_unbox_number_float.simplify env r prim arg dbg
  | Unbox_number Naked_int32 ->
    Simplify_unbox_number_int32.simplify env r prim arg dbg
  | Unbox_number Naked_int64 ->
    Simplify_unbox_number_int64.simplify env r prim arg dbg
  | Unbox_number Naked_nativeint ->
    Simplify_unbox_number_nativeint.simplify env r prim arg dbg
  | Box_number Naked_float ->
    Simplify_box_number_float.simplify env r prim arg dbg
  | Box_number Naked_int32 ->
    Simplify_box_number_int32.simplify env r prim arg dbg
  | Box_number Naked_int64 ->
    Simplify_box_number_int64.simplify env r prim arg dbg
  | Box_number Naked_nativeint ->
    Simplify_box_number_nativeint.simplify env r prim arg dbg
  | Project_closure _closures ->
    assert false
(*
    simplify_project_closure env r closures
*)
  | Move_within_set_of_closures _by_closure_id ->
    assert false
(*
    simplify_move_within_set_of_closures env r by_closure_id
*)
  | Project_var _by_closure_id ->
    assert false
(*
    simplify_project_var env r by_closure_id
*)
