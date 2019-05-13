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

type t = {
  typing_env_left : Typing_env.t;
  typing_env_right : Typing_env.t;
  perm_left : Name_permutation.t;
  perm_right : Name_permutation.t;
  existentials : Name.Set.t;
}

let print ppf { typing_env_left; typing_env_right;
                perm_left; perm_right; existentials; } =
  Format.fprintf ppf
    "@[<hov 1>(\
      @[<hov 1>(typing_env_left@ %a)@]@ \
      @[<hov 1>(typing_env_right@ %a)@]@ \
      @[<hov 1>(perm_left@ %a)@]@ \
      @[<hov 1>(perm_right@ %a)@]@ \
      @[<hov 1>(existentials@ %a)@])@]"
    Typing_env.print typing_env_left
    Typing_env.print typing_env_right
    Name_permutation.print perm_left
    Name_permutation.print perm_right
    Name.Set.print existentials

let create ~typing_env_left ~typing_env_right ~perm_left ~perm_right =
  { typing_env_left;
    typing_env_right;
    perm_left;
    perm_right;
    existentials = Name.Set.empty;
  }

let empty ~typing_env_left ~typing_env_right =
  create ~typing_env_left ~typing_env_right
    ~perm_left:(Name_permutation.create ())
    ~perm_right:(Name_permutation.create ())

let typing_env_left t = t.typing_env_left
let typing_env_right t = t.typing_env_right
let perm_left t = t.perm_left
let perm_right t = t.perm_right
let existentials t = t.existentials

let entering_scope_of_existentials t names =
  if not (Name.Set.is_empty (Name.Set.inter names t.existentials))
  then begin
    Misc.fatal_errorf "Already in scope of one or more of the \
        existential(s) %a:@ %a"
      Name.Set.print names
      print t
  end;
  { t with
    existentials = Name.Set.union t.existentials names;
  }

let replace_typing_environments t ~left:typing_env_left
      ~right:typing_env_right =
   { t with
     typing_env_left;
     typing_env_right;
   }

let add_definition_typing_env_left t name kind =
  let typing_env_left =
    Typing_env.add_definition t.typing_env_left name kind
  in
  { t with typing_env_left; }

let add_definition_typing_env_right t name kind =
  let typing_env_right =
    Typing_env.add_definition t.typing_env_right name kind
  in
  { t with typing_env_right; }

let _shortcut_precondition t =
  t.typing_env_left == t.typing_env_right
    && t.perm_left == t.perm_right

let compose_name_permutations t ~perm_left ~perm_right =
  { typing_env_left = t.typing_env_left;
    typing_env_right = t.typing_env_right;
    perm_left =
      Name_permutation.compose ~first:t.perm_left ~second:perm_left;
    perm_right =
      Name_permutation.compose ~first:t.perm_right ~second:perm_right;
    existentials = t.existentials;
  }
