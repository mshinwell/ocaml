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
    closure_elements = Closure_elements.bottom;
    set_of_closures = Basic_type_ops.bottom_as_ty_fabricated ();
  }

let print_with_cache ~cache ppf
      ({ function_decl; closure_elements; set_of_closures; } : t) =
  Format.fprintf ppf
    "@[<hov 1>(\
      @[<hov 1>(function_decl@ %a)@]@ \
      @[<hov 1>(closure_elements@ %a)@]@ \
      @[<hov 1>(set_of_closures@ %a)@]\
      )@]"
    (Or_unknown.print
      (Type_printers.print_function_declaration_with_cache ~cache))
    function_decl
    (Closure_elements.print_with_cache ~cache) closure_elements
    (Type_printers.print_ty_fabricated_with_cache ~cache) set_of_closures

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let equal _ _ = Misc.fatal_error "Not yet implemented"

let widen (t : t) ~to_match : t =
  let closure_elements =
    Closure_elements.widen t.closure_elements ~to_match
  in
  { function_decl = t.function_decl;
    closure_elements;
    set_of_closures = t.set_of_closures;
  }

module Meet_value = Meet_and_join_value.Make (Lattice_ops.For_meet)
module Join_value = Meet_and_join_value.Make (Lattice_ops.For_join)

let meet env t1 t2 : _ Or_bottom.t =
  Meet_value.meet_or_join_closures_entry env t1 t2

let join env t1 t2 =
  let env = Meet_env.create env in
  match Join_value.meet_or_join_closures_entry env t1 t2 with
  | Ok (t, _env_extension) -> t
  | Bottom -> create_bottom ()

let erase_aliases ({ function_decl; closure_elements; set_of_closures; } : t)
      env ~already_seen ~allowed : t =
  { function_decl;
    closure_elements =
      Closure_elements.erase_aliases closure_elements
        env ~already_seen ~allowed;
    set_of_closures =
      Type_erase_aliases.erase_aliases_ty_fabricated env
        ~bound_name:None ~already_seen ~allowed set_of_closures;
  }

let apply_name_permutation
      ({ function_decl; closure_elements; set_of_closures; } : t) perm : t =
  { function_decl;
    closure_elements =
      Closure_elements.apply_name_permutation closure_elements perm;
    set_of_closures =
      Basic_type_ops.apply_name_permutation_ty_fabricated
        set_of_closures perm;
  }

let free_names ({ function_decl = _; closure_elements; set_of_closures; } : t) =
  Name_occurrences.union
    (Closure_elements.free_names closure_elements)
    (Type_free_names.free_names_of_ty_fabricated set_of_closures)

let map_function_decl_types
      (({ function_decl; closure_elements; set_of_closures; } : t) as t)
      ~(f : Type_grammar.function_declaration
        -> Type_grammar.function_declaration Or_bottom.t)
      : _ Or_bottom.t =
  match function_decl with
  | Unknown -> Ok t
  | Known function_decl ->
    Or_bottom.map (f function_decl) ~f:(fun function_decl : t ->
      { function_decl = Known function_decl;
        closure_elements;
        set_of_closures;
      })
