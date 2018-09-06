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

module type Name = sig
  include Contains_names.S
  val print : Format.formatter -> t -> unit
  val rename : t -> t
  val permutation_to_swap : t -> t -> Name_permutation.t
end

module type Term = sig
  include Contains_names.S
  val print : Format.formatter -> t -> unit
  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit
end

type printing_style =
  | Normal
  | Brackets
  | Existential

module Make (Name : Name) (Term : Term) = struct
  type t = Name.t * Term.t

  let create name term = name, term

  let pattern_match (name, term) ~f =
    let fresh_name = Name.rename name in
    let perm = Name.permutation_to_swap name fresh_name in
    let fresh_term = Term.apply_name_permutation term perm in
    f fresh_name fresh_term

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

  let print ?(style = Normal) ppf t =
    pattern_match t ~f:(fun name term ->
      Format.fprintf ppf "@[<hov 1>%s@<1>%s%s%a%s@<1>%s%s@,%a@]"
        (Misc_color.bold_cyan ())
        (before_binding_position style)
        (Misc_color.reset ())
        Name.print name
        (Misc_color.bold_cyan ())
        (after_binding_position style)
        (Misc_color.reset ())
        Term.print term)

  let print_with_cache ?(style = Normal) ~cache ppf t =
    pattern_match t ~f:(fun name term ->
      Format.fprintf ppf "@[<hov 1>%s@<1>%s%s%a%s@<1>%s%s@,%a@]"
        (Misc_color.bold_cyan ())
        (before_binding_position style)
        (Misc_color.reset ())
        Name.print name
        (Misc_color.bold_cyan ())
        (after_binding_position style)
        (Misc_color.reset ())
        (Term.print_with_cache ~cache) term)

  let pattern_match_mapi (name, term) ~f =
    let fresh_name = Name.rename name in
    let perm = Name.permutation_to_swap name fresh_name in
    let fresh_term = Term.apply_name_permutation term perm in
    let new_term = f fresh_name fresh_term in
    fresh_name, new_term

  let pattern_match_map t ~f =
    pattern_match_mapi t ~f:(fun _name term -> f term)

  let pattern_match_pair (name0, term0) (name1, term1) ~f =
    let fresh_name = Name.rename name0 in
    let perm0 = Name.permutation_to_swap name0 fresh_name in
    let perm1 = Name.permutation_to_swap name1 fresh_name in
    let fresh_term0 = Term.apply_name_permutation term0 perm0 in
    let fresh_term1 = Term.apply_name_permutation term1 perm1 in
    f fresh_name fresh_term0 fresh_term1

  let apply_name_permutation (name, term) perm =
    let name = Name.apply_name_permutation name perm in
    let term = Term.apply_name_permutation term perm in
    name, term

  let free_names (name, term) =
    let bound = Name.free_names name in
    let free_in_term = Term.free_names term in
    Name_occurrences.diff free_in_term bound
end

module Make2 (Name0 : Name) (Name1 : Name) (Term : Term) = struct
  type t = Name0.t * Name1.t * Term.t

  let create name0 name1 term = name0, name1, term

  let pattern_match (name0, name1, term) ~f =
    let fresh_name0 = Name0.rename name0 in
    let perm0 = Name0.permutation_to_swap name0 fresh_name0 in
    let fresh_name1 = Name1.rename name1 in
    let perm1 = Name1.permutation_to_swap name1 fresh_name1 in
    let perm = Name_permutation.compose ~first:perm0 ~second:perm1 in
    let fresh_term = Term.apply_name_permutation term perm in
    f fresh_name0 fresh_name1 fresh_term

  let print ppf t =
    pattern_match t ~f:(fun name0 name1 term ->
      Format.fprintf ppf "@[[%a, %a]@,%a@]"
        Name0.print name0
        Name1.print name1
        Term.print term)

  let print_with_cache ~cache ppf t =
    pattern_match t ~f:(fun name0 name1 term ->
      Format.fprintf ppf "@[[%a, %a]@,%a@]"
        Name0.print name0
        Name1.print name1
        (Term.print_with_cache ~cache) term)

  let pattern_match_pair (name0, name1, term) (name0', name1', term') ~f =
    let fresh_name0 = Name0.rename name0 in
    let perm0 = Name0.permutation_to_swap name0 fresh_name0 in
    let perm0' = Name0.permutation_to_swap name0' fresh_name0 in
    let fresh_name1 = Name1.rename name1 in
    let perm1 = Name1.permutation_to_swap name1 fresh_name1 in
    let perm1' = Name1.permutation_to_swap name1' fresh_name1 in
    let perm = Name_permutation.compose ~first:perm0 ~second:perm1 in
    let perm' = Name_permutation.compose ~first:perm0' ~second:perm1' in
    let fresh_term = Term.apply_name_permutation term perm in
    let fresh_term' = Term.apply_name_permutation term' perm' in
    f fresh_name0 fresh_name1 fresh_term fresh_term'

  let apply_name_permutation (name0, name1, term) perm =
    let name0 = Name0.apply_name_permutation name0 perm in
    let name1 = Name1.apply_name_permutation name1 perm in
    let term = Term.apply_name_permutation term perm in
    name0, name1, term

  let free_names (name0, name1, term) =
    let bound =
      Name_occurrences.union (Name0.free_names name0) (Name1.free_names name1)
    in
    let free_in_term = Term.free_names term in
    Name_occurrences.diff free_in_term bound
end
