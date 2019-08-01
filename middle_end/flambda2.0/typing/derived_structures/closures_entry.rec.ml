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

(* CR mshinwell: Move the definition of type [t] here then remove all the
   " : t" type annotations. *)
type t = Type_grammar.closures_entry

let create_bottom () : t =
  { function_decl = Unknown;
    closure_types = Types_by_closure_id.bottom;
    closure_var_types = Types_by_var_within_closure.bottom;
  }

let print_with_cache ~cache ppf
      ({ function_decl; closure_types; closure_var_types; } : t) =
  Format.fprintf ppf
    "@[<hov 1>(\
      @[<hov 1>(function_decl@ %a)@]@ \
      @[<hov 1>(closure_types@ %a)@]@ \
      @[<hov 1>(closure_var_types@ %a)@]@\
      )@]"
    (Or_unknown.print
      (Type_printers.print_function_declaration_with_cache ~cache))
    function_decl
    (Types_by_closure_id.print_with_cache ~cache) closure_types
    (Types_by_var_within_closure.print_with_cache ~cache) closure_var_types

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let equal _ _ = Misc.fatal_error "Not yet implemented"

let widen (t : t) ~(to_match : t) : t =
  let closure_types =
    Types_by_closure_id.widen t.closure_types
      ~to_match:to_match.closure_types
  in
  let closure_var_types =
    Types_by_var_within_closure.widen t.closure_var_types
      ~to_match:to_match.closure_var_types
  in
  { function_decl = t.function_decl;
    closure_types;
    closure_var_types;
  }

module Meet_value = Meet_and_join_value.Make (Lattice_ops.For_meet)
module Join_value = Meet_and_join_value.Make (Lattice_ops.For_join)

let meet env t1 t2 : _ Or_bottom.t =
  (* CR mshinwell: Move the code to here *)
  Meet_value.meet_or_join_closures_entry env t1 t2

let join env t1 t2 =
  let env = Meet_env.create env in
  match Join_value.meet_or_join_closures_entry env t1 t2 with
  | Ok (t, _env_extension) -> t
  | Bottom -> create_bottom ()

let erase_aliases ({ function_decl; closure_types; closure_var_types; } : t)
      env ~already_seen ~allowed : t =
  { function_decl;
    closure_types =
      Types_by_closure_id.erase_aliases closure_types
        env ~already_seen ~allowed;
    closure_var_types =
      Types_by_var_within_closure.erase_aliases closure_var_types
        env ~already_seen ~allowed;
  }

let apply_name_permutation
      ({ function_decl; closure_types; closure_var_types; } : t) perm : t =
  { function_decl;
    closure_types =
      Types_by_closure_id.apply_name_permutation closure_types perm;
    closure_var_types =
      Types_by_var_within_closure.apply_name_permutation closure_var_types perm;
  }

let free_names ({ function_decl = _; closure_types; closure_var_types; } : t) =
  Name_occurrences.union
    (Types_by_closure_id.free_names closure_types)
    (Types_by_var_within_closure.free_names closure_var_types)

let map_function_decl_types
      (({ function_decl; closure_types; closure_var_types; } : t) as t)
      ~(f : Type_grammar.function_declaration
        -> Type_grammar.function_declaration Or_bottom.t)
      : _ Or_bottom.t =
  (* CR mshinwell: This needs to deal with [closure_types] too.
     Deferring until new approach for [Rec_info] is sorted out. *)
  match function_decl with
  | Unknown -> Ok t
  | Known function_decl ->
    Or_bottom.map (f function_decl) ~f:(fun function_decl : t ->
      { function_decl = Known function_decl;
        closure_types;
        closure_var_types;
      })
