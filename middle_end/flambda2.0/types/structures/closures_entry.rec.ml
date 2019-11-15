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

module TEE = Typing_env_extension

type t = {
  function_decls : Function_declaration_type.t Or_unknown.t Closure_id.Map.t;
  closure_types : Product.Closure_id_indexed.t;
  closure_var_types : Product.Var_within_closure_indexed.t;
}

let create ~function_decls ~closure_types ~closure_var_types =
  { function_decls;
    closure_types;
    closure_var_types;
  }

let create_bottom () =
  { function_decls = Closure_id.Map.empty;
    closure_types = Product.Closure_id_indexed.create_bottom ();
    closure_var_types = Product.Var_within_closure_indexed.create_bottom ();
  }

let print_with_cache ~cache ppf
      { function_decls; closure_types; closure_var_types; } =
  Format.fprintf ppf
    "@[<hov 1>(\
      @[<hov 1>(function_decls@ %a)@]@ \
      @[<hov 1>(closure_types@ %a)@]@ \
      @[<hov 1>(closure_var_types@ %a)@]\
      )@]"
    (Closure_id.Map.print
      (Or_unknown.print
        (Function_declaration_type.print_with_cache ~cache)))
    function_decls
    (Product.Closure_id_indexed.print_with_cache ~cache) closure_types
    (Product.Var_within_closure_indexed.print_with_cache ~cache) closure_var_types

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let widen t ~to_match =
  let missing_function_decls =
    Closure_id.Set.diff (Closure_id.Map.keys to_match.function_decls)
      (Closure_id.Map.keys t.function_decls)
  in
  let function_decls =
    Closure_id.Set.fold (fun closure_id function_decls ->
        Closure_id.Map.add closure_id Or_unknown.Unknown function_decls)
      missing_function_decls
      t.function_decls
  in
  let closure_types =
    Product.Closure_id_indexed.widen t.closure_types
      ~to_match:to_match.closure_types
  in
  let closure_var_types =
    Product.Var_within_closure_indexed.widen t.closure_var_types
      ~to_match:to_match.closure_var_types
  in
  { function_decls;
    closure_types;
    closure_var_types;
  }

module Make_meet_or_join
  (E : Lattice_ops_intf.S
   with type meet_env := Meet_env.t
   with type typing_env := Typing_env.t
   with type typing_env_extension := Typing_env_extension.t) =
struct
  let meet_or_join env
        { function_decls = function_decls1;
          closure_types = closure_types1;
          closure_var_types = closure_var_types1;
        }
        { function_decls = function_decls2;
          closure_types = closure_types2;
          closure_var_types = closure_var_types2;
        } =
    let function_decls =
      Closure_id.Map.merge (fun _closure_id func_decl1 func_decl2 ->
          match func_decl1, func_decl2 with
          | None, None | Some _, None | None, Some _ -> None
          | Some func_decl1, Some func_decl2 ->
            let func_decl, env_extension =
              Function_declaration_type.meet_or_join_unknown
                func_decl1 func_decl2
            in
            Some func_decl)
        function_decls1 function_decls2
    in
    let closure_types =
      E.switch Product.Closure_id_indexed.meet
        Product.Closure_id_indexed.join
        env closure_types1 closure_types2
    in
    let closure_var_types =
      E.switch Product.Var_within_closure_indexed.meet
        Product.Var_within_closure_indexed.join
        env closure_var_types1 closure_var_types2
    in
    Or_bottom.both closure_types closure_var_types
      ~f:(fun (closure_types, env_extension1)
              (closure_var_types, env_extension2) ->
        let closures_entry =
          { function_decls;
            closure_types;
            closure_var_types;
          }
        in
        let env_extension =
          (* XXX This should use the proper environments from both sides, no?
             See if we can avoid needing that *)
          let left_env = Meet_env.env env in
          let right_env = Meet_env.env env in
          (* CR mshinwell: Move to [TEE].  Also used in [Type_of_kind_value0] *)
          let join_extensions env ext1 ext2 =
            let env_extension, _ =
              TEE.n_way_join ~initial_env_at_join:env [
                left_env, Apply_cont_rewrite_id.create (), Non_inlinable,
                  Variable.Set.empty, ext1;
                right_env, Apply_cont_rewrite_id.create (), Non_inlinable,
                  Variable.Set.empty, ext2;
              ]
            in
            env_extension
          in
          E.switch0 TEE.meet join_extensions env
            env_extension1 env_extension2
        in
        closures_entry, env_extension)
end

module Meet = Make_meet_or_join (Lattice_ops.For_meet)
module Join = Make_meet_or_join (Lattice_ops.For_join)

let meet env t1 t2 : _ Or_bottom.t =
  (* CR mshinwell: Move the code to here *)
  Meet.meet_or_join env t1 t2

let join env t1 t2 =
  let env = Meet_env.create env in
  match Join.meet_or_join env t1 t2 with
  | Ok (t, _env_extension) -> t
  | Bottom -> create_bottom ()

let apply_name_permutation
      { function_decls; closure_types; closure_var_types; } perm =
  { function_decls; (* XXX is this correct? *)
    closure_types =
      Product.Closure_id_indexed.apply_name_permutation closure_types perm;
    closure_var_types =
      Product.Var_within_closure_indexed.apply_name_permutation closure_var_types perm;
  }

let free_names { function_decls = _; closure_types; closure_var_types; } =
  Name_occurrences.union
    (Product.Closure_id_indexed.free_names closure_types)
    (Product.Var_within_closure_indexed.free_names closure_var_types)

let map_function_decl_types
      { function_decls; closure_types; closure_var_types; }
      ~(f : Function_declaration_type.t
        -> Function_declaration_type.t Or_bottom.t)
      : _ Or_bottom.t =
  (* CR mshinwell: This needs to deal with [closure_types] too.
     Deferring until new approach for [Rec_info] is sorted out. *)
  let bottom = ref false in
  let function_decls =
    Closure_id.Map.map (fun (function_decl : _ Or_unknown.t) : _ Or_unknown.t ->
        match function_decl with
        | Unknown -> Unknown
        | Known function_decl ->
          match f function_decl with
          | Bottom ->
            bottom := true;
            Unknown
          | Ok function_decl ->
            Known function_decl)
      function_decls
  in
  if !bottom then Bottom
  else
    let t =
      { function_decls;
        closure_types;
        closure_var_types;
      }
    in
    Ok t

let closure_var_types t =
  Product.Var_within_closure_indexed.to_map t.closure_var_types

let find_function_declaration t closure_id =
  match Closure_id.Map.find closure_id t.function_decls with
  | exception Not_found ->
    (* CR mshinwell: Add [invariant] check. *)
    Misc.fatal_errorf "[Closures_entry] doesn't contain function declaration \
        for closure ID %a:@ %a"
      Closure_id.print closure_id
      print t
  | func_decl -> func_decl
