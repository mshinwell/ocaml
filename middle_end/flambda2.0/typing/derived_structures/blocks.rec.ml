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

module Tag_and_targetint_ocaml = struct
  type t = Tag.t * Targetint.OCaml.t
  include Identifiable.Make_pair (Tag) (Targetint.OCaml)
end

module RL =
  Row_like.Make (Tag) (Targetint.OCaml) (Tag_and_targetint_ocaml) (Product)

include RL

type open_or_closed = Open | Closed of Tag.t

let create ~field_tys kind open_or_closed : t =
  let product = Product.create field_tys kind in
  let size = Targetint.OCaml.of_int (List.length field_tys) in
  match open_or_closed with
  | Open -> create_at_least size product
  | Closed tag -> create_exactly tag size product
