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

module Int = Numbers.Int

module Int_indexed_product = Product.Make (Int)

module Index = struct
  include Targetint.OCaml
  let subset t1 t2 = Stdlib.(<=) (compare t1 t2) 0
end

include Row_like.Make (Tag) (Index) (Tag_and_size)
  (Tag_or_unknown_and_size) (Int_indexed_product)

type open_or_closed = Open | Closed of Tag.t

let create ~field_tys (open_or_closed : open_or_closed) =
  let fields = List.mapi (fun index ty -> index, ty) field_tys in
  let product = Int_indexed_product.create (Int.Map.of_list fields) in
  let size = Targetint.OCaml.of_int (List.length field_tys) in
  match open_or_closed with
  | Open -> create_at_least Unknown size product
  | Closed tag -> create_exactly tag size product

let all_tags_and_sizes t : _ Or_unknown.t =
  match all_tags_and_indexes t with
  | Unknown -> Unknown
  | Known tags_and_indexes ->
    let by_tag =
      Tag_and_size.Set.fold (fun (tag, size) all_tags ->
          match Tag.Map.find tag all_tags with
          | exception Not_found -> Tag.Map.add tag size all_tags
          | _ ->
            Misc.fatal_errorf "More than one size for the same tag:@ %a"
              print t)
        tags_and_indexes
        Tag.Map.empty
    in
    Known by_tag
