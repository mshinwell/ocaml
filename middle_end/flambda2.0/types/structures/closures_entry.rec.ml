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

module FDT = Function_declaration_type
module PC = Product.Closure_id_indexed
module PV = Product.Var_within_closure_indexed
module TEE = Typing_env_extension

type t = {
  function_decls : FDT.t Closure_id.Map.t;
  closure_types : PC.t;
  closure_var_types : PV.t;
}

let create ~function_decls ~closure_types ~closure_var_types =
  { function_decls;
    closure_types;
    closure_var_types;
  }

let create_bottom () =
  { function_decls = Closure_id.Map.empty;
    closure_types = PC.create_bottom ();
    closure_var_types = PV.create_bottom ();
  }

let is_bottom { function_decls; closure_types; closure_var_types; } =
  Closure_id.Map.is_empty function_decls
    && PC.is_bottom closure_types
    && PV.is_bottom closure_var_types

let print_with_cache ~cache ppf
      { function_decls; closure_types; closure_var_types; } =
  Format.fprintf ppf
    "@[<hov 1>(\
      @[<hov 1>(function_decls@ %a)@]@ \
      @[<hov 1>(closure_types@ %a)@]@ \
      @[<hov 1>(closure_var_types@ %a)@]\
      )@]"
    (Closure_id.Map.print (FDT.print_with_cache ~cache)) function_decls
    (PC.print_with_cache ~cache) closure_types
    (PV.print_with_cache ~cache) closure_var_types

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

module Make_meet_or_join
  (E : Lattice_ops_intf.S
   with type meet_env := Meet_env.t
   with type meet_or_join_env := Meet_or_join_env.t
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
        } : _ Or_bottom.t =
    let env_extensions = ref [] in
    let function_decls =
      Closure_id.Map.merge (fun _closure_id func_decl1 func_decl2 ->
          match func_decl1, func_decl2 with
          | None, None | Some _, None | None, Some _ -> None
          | Some func_decl1, Some func_decl2 ->
            match E.switch FDT.meet FDT.join env func_decl1 func_decl2 with
            | Bottom -> None
            | Ok (func_decl, env_extension) ->
              env_extensions := env_extension :: !env_extensions;
              Some func_decl)
        function_decls1 function_decls2
    in
    if Closure_id.Map.is_empty function_decls then Bottom
    else
      Or_bottom.both
        (E.switch PC.meet PC.join env closure_types1 closure_types2)
        (E.switch PV.meet PV.join env closure_var_types1 closure_var_types2)
        ~f:(fun (closure_types, env_extension1)
                (closure_var_types, env_extension2) ->
          let closures_entry =
            { function_decls;
              closure_types;
              closure_var_types;
            }
          in
          let env_extension =
            match E.op () with
            | Join -> TEE.empty ()
            | Meet ->
              TEE.n_way_meet (Meet_or_join_env.meet_env env)
                (env_extension1 :: env_extension2 :: !env_extensions)
          in
          closures_entry, env_extension)
end

module Meet = Make_meet_or_join (Lattice_ops.For_meet)
module Join = Make_meet_or_join (Lattice_ops.For_join)

let meet env t1 t2 : _ Or_bottom.t =
  let env = Meet_or_join_env.create_for_meet env in
  match Meet.meet_or_join env t1 t2 with
  | Bottom -> Bottom
  | Ok (t, env_extension) ->
    if is_bottom t then Bottom
    else Ok (t, env_extension)

let join env t1 t2 =
  match Join.meet_or_join env t1 t2 with
  | Ok (t, env_extension) ->
    assert (TEE.is_empty env_extension);
    t
  | Bottom -> create_bottom ()

let apply_name_permutation
      { function_decls; closure_types; closure_var_types; } perm =
  { function_decls =
      Closure_id.Map.map_sharing (fun function_decl ->
          FDT.apply_name_permutation function_decl perm)
        function_decls;
    closure_types = PC.apply_name_permutation closure_types perm;
    closure_var_types = PV.apply_name_permutation closure_var_types perm;
  }

let free_names { function_decls; closure_types; closure_var_types; } =
  let function_decls_free_names =
    Closure_id.Map.fold (fun _closure_id function_decl free_names ->
        Name_occurrences.union free_names (FDT.free_names function_decl))
      function_decls
      Name_occurrences.empty
  in
  Name_occurrences.union function_decls_free_names
    (Name_occurrences.union (PC.free_names closure_types)
      (PV.free_names closure_var_types))

let function_decl_types t = t.function_decls
let closure_types t = PC.to_map t.closure_types
let closure_var_types t = PV.to_map t.closure_var_types

let find_function_declaration t closure_id : _ Or_bottom.t =
  match Closure_id.Map.find closure_id t.function_decls with
  | exception Not_found -> Bottom
  | func_decl -> Ok func_decl

let map_function_decl_types
      { function_decls; closure_types; closure_var_types; }
      ~(f : FDT.t -> FDT.t Or_bottom.t) : _ Or_bottom.t =
  (* CR mshinwell: This needs to deal with [closure_types] too.
     Deferring until new approach for [Rec_info] is sorted out. *)
  let bottom = ref false in
  let function_decls =
    Closure_id.Map.map (fun function_decl ->
        match f function_decl with
        | Ok function_decl -> function_decl
        | Bottom ->
          bottom := true;
          function_decl)
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
