(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR mshinwell: Delete >= 4.08 *)
[@@@ocaml.warning "-60"]
module Typing_env = struct end

module Simple_pair = Hashtbl.Make_with_map_pair (Simple) (Simple)

module Make (W : Typing_world.S) = struct
  open! W

  type t = {
    env : Typing_env.t;
    perm_left : Name_permutation.t;
    perm_right : Name_permutation.t;
    already_meeting : Simple_pair.Set.t;
  }

  let print ppf { env; perm_left; perm_right; already_meeting; } =
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(env@ %a)@]@ \
        @[<hov 1>(perm_left@ %a)@]@ \
        @[<hov 1>(perm_right@ %a)@]@ \
        @[<hov 1>(already_meeting@ %a)@])@]"
      Typing_env.print env
      Name_permutation.print perm_left
      Name_permutation.print perm_right
      Simple_pair.Set.print already_meeting

  let create env ~perm_left ~perm_right =
    { env;
      perm_left;
      perm_right;
      already_meeting = Simple_pair.Set.empty;
    }

  let env t = t.env

  let perm_left t = t.perm_left
  let perm_right t = t.perm_right

  let fast_check_name_permutations_same_both_sides t =
    t.perm_left == t.perm_right

  let already_meeting t simple1 simple2 =
    Simple_pair.Set.mem (simple1, simple2) t.already_meeting
      || Simple_pair.Set.mem (simple2, simple1) t.already_meeting

  let now_meeting t simple1 simple2 =
    if already_meeting t simple1 simple2 then begin
      Misc.fatal_errorf "Already meeting %a and %a:@ %a"
        Simple.print simple1
        Simple.print simple2
        print t
    end;
    let already_meeting =
      Simple_pair.Set.add (simple1, simple2) t.already_meeting
    in
    { t with
      already_meeting;
    }

  let shortcut_precondition t =
    fast_check_name_permutations_same_both_sides t

  let with_env t f =
    { t with env = f t.env; }

  let clear_name_permutations t =
    { t with
      perm_left = Name_permutation.create ();
      perm_right = Name_permutation.create ();
    }

  module Typing_env = W.Typing_env
end
