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

[@@@ocaml.warning "+a-30-40-41-42"]

(** Types of kind [Fabricated].  No source-level values are of this kind. *)

type t =
  (* CR mshinwell: Work out how to do the dependent-discriminant thing
     properly. *)
  | Discriminants of Discriminants.t
    (* CR mshinwell: update comment.  It's now either a tag, or the
       result of Pisint *)
    (** A discriminant is either:
        - a block tag, as returned by the [Get_tag] primitive; or
        - a constant constructor which has undergone a kind-cast to kind
          [Fabricated] using the [Discriminant_of_int] primitive. *)

include Type_head_intf.S
  with type t := t
  with type type_grammar := Type_grammar.t
  with type typing_env := Typing_env.t
  with type typing_env_extension := Typing_env_extension.t
  with type meet_env := Meet_env.t
