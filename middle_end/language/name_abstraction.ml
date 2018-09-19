(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type Term = sig
  include Contains_names.S
  val print : Format.formatter -> t -> unit
  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit
end

type printing_style =
  | Normal
  | Brackets
  | Existential

let before_binding_position style =
  match style with
  | Normal -> "\u{0418}"
  | Brackets -> "["
  | Existential -> "\u{2203}"

let after_binding_position style =
  match style with
  | Normal -> "."
  | Brackets -> "]"
  | Existential -> "."

module type Common = sig
  type t

  val print : ?style:printing_style -> Format.formatter -> t -> unit

  val print_with_cache
     : ?style:printing_style
    -> cache:Printing_cache.t
    -> Format.formatter
    -> t
    -> unit
end

module Make (Term : Term) = struct
  type t = Bindable_name.t * Term.t

  let create name term = name, term

  let pattern_match (name, term) ~f =
    let fresh_name = Bindable_name.rename name in
    let perm =
      Name_permutation.add_bindable_name_exn (Name_permutation.create ())
        name fresh_name
    in
    let fresh_term = Term.apply_name_permutation term perm in
    f fresh_name fresh_term

  let print ?(style = Brackets) ppf t =
    pattern_match t ~f:(fun name term ->
      Format.fprintf ppf "@[<hov 1>%s@<1>%s%s%a%s@<1>%s%s@,%a@]"
        (Misc_color.bold_cyan ())
        (before_binding_position style)
        (Misc_color.reset ())
        Bindable_name.print name
        (Misc_color.bold_cyan ())
        (after_binding_position style)
        (Misc_color.reset ())
        Term.print term)

  let print_with_cache ?(style = Brackets) ~cache ppf t =
    pattern_match t ~f:(fun name term ->
      Format.fprintf ppf "@[<hov 1>%s@<1>%s%s%a%s@<1>%s%s@,%a@]"
        (Misc_color.bold_cyan ())
        (before_binding_position style)
        (Misc_color.reset ())
        Bindable_name.print name
        (Misc_color.bold_cyan ())
        (after_binding_position style)
        (Misc_color.reset ())
        (Term.print_with_cache ~cache) term)

  let pattern_match_mapi t ~f =
    pattern_match t ~f:(fun fresh_name fresh_term ->
      let new_term = f fresh_name fresh_term in
      fresh_name, new_term)

  let pattern_match_map t ~f =
    pattern_match_mapi t ~f:(fun _fresh_name fresh_term -> f fresh_term)

  let pattern_match_pair (name0, term0) (name1, term1) ~f =
    let fresh_name = Bindable_name.rename name0 in
    let perm0 =
      Name_permutation.add_bindable_name_exn (Name_permutation.create ())
        name0 fresh_name
    in
    let perm1 =
      Name_permutation.add_bindable_name_exn (Name_permutation.create ())
        name1 fresh_name
    in
    let fresh_term0 = Term.apply_name_permutation term0 perm0 in
    let fresh_term1 = Term.apply_name_permutation term1 perm1 in
    f fresh_name fresh_term0 fresh_term1

  let apply_name_permutation (name, term) perm =
    let name = Name_permutation.apply_bindable_name perm name in
    let term = Term.apply_name_permutation term perm in
    name, term

  let free_names (name, term) =
    let in_binding_position = Name_occurrences.singleton_in_terms name in
    let free_in_term = Term.free_names term in
    Name_occurrences.diff free_in_term in_binding_position
end

module Make_list (Term : Term) = struct
  type t = Bindable_name.t list * Term.t

  let create names term =
    let names_set = Bindable_name.Set.of_list names in
    if List.length names <> Bindable_name.Set.cardinal names_set then begin
      Misc.fatal_errorf "Cannot create generalised abstraction value with \
          non-disjoint names in binding position: %a"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
          Bindable_name.print) names
    end;
    names, term

  let pattern_match (names, term) ~f =
    let fresh_names_rev, perm =
      List.fold_left (fun (fresh_names_rev, perm) stale_name ->
          let fresh_name = Bindable_name.rename stale_name in
          let perm =
            Name_permutation.add_bindable_name_exn perm fresh_name stale_name
          in
          fresh_name :: fresh_names_rev, perm)
        ([], Name_permutation.create ())
        names
    in
    let fresh_names = List.rev fresh_names_rev in
    let fresh_term = Term.apply_name_permutation term perm in
    f fresh_names fresh_term

  let print_bindable_name_list style ppf bns =
    match bns with
    | [] -> ()
    | _ ->
      Format.fprintf ppf "@<1>%s%s@<1>%s%a@<1>%s%s@<1>%s"
        (Misc_color.bold_cyan ())
        (before_binding_position style)
        (Misc_color.reset ())
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
          Bindable_name.print) bns
        (Misc_color.bold_cyan ())
        (after_binding_position style)
        (Misc_color.reset ())

  let print ?(style = Brackets) ppf t =
    pattern_match t ~f:(fun names term ->
      Format.fprintf ppf "@[%a@,%a@]"
        (print_bindable_name_list style) names
        Term.print term)

  let print_with_cache ?(style = Brackets) ~cache ppf t =
    pattern_match t ~f:(fun names term ->
      Format.fprintf ppf "@[%a@,%a@]"
        (print_bindable_name_list style) names
        (Term.print_with_cache ~cache) term)

  let pattern_match_mapi t ~f =
    pattern_match t ~f:(fun fresh_names fresh_term ->
      let new_term = f fresh_names fresh_term in
      fresh_names, new_term)

  let pattern_match_map t ~f =
    pattern_match_mapi t ~f:(fun _name term -> f term)

  let pattern_match_pair ((names0, term0) as t1) ((names1, term1) as t2) ~f =
    if List.compare_lengths names0 names1 <> 0 then begin
      let print ppf t : unit = print ppf t in
      Misc.fatal_errorf "Cannot concrete a pair of generalised abstractions \
          unless they have the same number of names in binding position:@ \
          %a@ and@ %a"
        print t1
        print t2
    end;
    let fresh_names_rev, perm0, perm1 =
      List.fold_left2
        (fun (fresh_names_rev, perm0, perm1) stale_name0 stale_name1 ->
          let fresh_name = Bindable_name.rename stale_name0 in
          let perm0 =
            Name_permutation.add_bindable_name_exn perm0 fresh_name stale_name0
          in
          let perm1 =
            Name_permutation.add_bindable_name_exn perm1 fresh_name stale_name1
          in
          fresh_name :: fresh_names_rev, perm0, perm1)
        ([], Name_permutation.create (), Name_permutation.create ())
        names0 names1
    in
    let fresh_names = List.rev fresh_names_rev in
    let fresh_term0 = Term.apply_name_permutation term0 perm0 in
    let fresh_term1 = Term.apply_name_permutation term1 perm1 in
    f fresh_names fresh_term0 fresh_term1

  let apply_name_permutation (names, term) perm =
    let names = Name_permutation.apply_bindable_name_list perm names in
    let term = Term.apply_name_permutation term perm in
    names, term

  let free_names (names, term) =
    let in_binding_position =
      List.fold_left (fun in_binding_position name ->
          Name_occurrences.add in_binding_position name In_terms)
        (Name_occurrences.create ())
        names
    in
    let free_in_term = Term.free_names term in
    Name_occurrences.diff free_in_term in_binding_position
end
