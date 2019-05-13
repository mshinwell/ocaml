(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module K = Flambda_kind

type t = {
  kind : K.t;
  tys : Flambda_types.t list;
}

let create tys kind =
  let kinds = List.map Flambda_type0_core.kind tys in
  match kinds with
  | [] ->
    { kind;
      ty;
    }
  | kinds ->
    let kind_set = K.Set.of_list kinds in
    match K.Set.get_singleton kind_set with
    | Some kind' ->
      if not (K.equal kind kind') then begin
        Misc.fatal_errorf "[kind] argument (%a) to [Product.create] \
            doesn't match kind(s) of supplied [tys] (%a)"
          K.print kind
          K.print kind'
      end;
      { tys;
        kind;
      }
    | None ->
      Misc.fatal_error "Supplied types to [Product.create] differ: %a"
        K.Set.print kind_set

let create_bottom ~arity kind =
  let tys = List.init arity (fun _ -> Flambda_type0_core.bottom kind) in
  create tys kind

let widen t ~to_match =
  let t_length = List.length t in
  let to_match_length = List.length to_match in
  if t_length >= to_match_length then t
  else
    let extra_length = to_match_length - t_length in
    let extra = create_bottom ~arity:extra_length t.kind in
    create (t.tys @ extra.tys) t.kind
