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
    let env =
      Typing_env.add_definition env (Name.var result_var) result_kind
    in
    let env = Meet_env.create env in
    let meet_ty, env_extension = meet env t shape in
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

  let unknown_types_from_arity t =
    List.map (fun kind -> unknown kind) t

  let is_bottom env t =
    let t, _simple = Typing_env.resolve_any_toplevel_alias env t in
    is_obviously_bottom t

  let term_language_function_declaration (decl : function_declaration) =
    match decl with
    | Non_inlinable -> None
    | Inlinable inlinable -> Some (inlinable.function_decl)

  type 'a proof =
    | Proved of 'a
    | Unknown
    | Invalid

  let prove_single_closures_entry env t : _ proof =
    let wrong_kind () = Misc.fatal_errorf "Type has wrong kind: %a" print t in
    let resolved, _simple = Typing_env.resolve_type env t in
    match resolved with
    | Resolved_value Unknown -> Unknown
    | Resolved_value (Ok (Closures closures)) ->
      begin
        match Closures_entry_by_closure_id.get_singleton closures.by_closure_id
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
    | Boxed_float of Float.t
    | Boxed_int32 of Int32.t
    | Boxed_int64 of Int64.t
    | Boxed_nativeint of Targetint.t

  type reification_result =
    | Term of Simple.t * t
    | Lift of to_lift
    | Cannot_reify
    | Invalid

  (* CR mshinwell: Move into submodule *)
  let resolved_type_is_bottom (resolved_t : resolved_t) =
    match resolved_t with
    | Resolved_value Bottom -> true
    | Resolved_naked_number (Bottom, _) -> true
    | Resolved_fabricated Bottom -> true
    | Resolved_value _ -> false
    | Resolved_naked_number _ -> false
    | Resolved_fabricated _ -> false

  let reify env ~allow_free_variables t : reification_result =
    let resolved, canonical_simple = Typing_env.resolve_type env t in
    (* CR mshinwell: We should probably also resolve aliases throughout the
       type to try to get them to symbols. *)
    let _can_lift =
      Name_occurrences.only_contains_symbols (Type_free_names.free_names t)
    in
    if resolved_type_is_bottom resolved then Invalid
    else
      let result, canonical_var =
        match canonical_simple with
        | Some ((Name (Symbol _) | Const _ | Discriminant _) as simple) ->
          Some (Term (simple, alias_type_of (kind t) simple)), None
        | Some ((Name ((Var _) as _name)) as simple) ->
          let all_aliases = Typing_env.aliases_of_simple env simple in
(*Format.eprintf "all_aliases %a\n%!" Name.Set.print all_aliases;*)
          let all_symbol_aliases = Name.set_to_symbol_set all_aliases in
          begin match Symbol.Set.get_singleton all_symbol_aliases with
          | Some symbol ->
(*Format.eprintf "using symbol %a\n%!" Symbol.print symbol;*)
            let simple = Simple.symbol symbol in
            Some (Term (simple, alias_type_of (kind t) simple)), None
          | None ->
            if allow_free_variables
            then None, Some simple
            else None, None
          end
        | None -> None, None
      in
      match result with
      | Some result -> result
      | None ->
        let try_canonical_var () : reification_result =
          match canonical_var with
          | Some simple -> Term (simple, alias_type_of (kind t) simple)
          | None -> Cannot_reify
        in
        match resolved with
        | _ -> try_canonical_var ()
end
