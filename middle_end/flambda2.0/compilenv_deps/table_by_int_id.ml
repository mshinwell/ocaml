(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module Int = Numbers.Int

module Id = struct
  include Int

  let num_empty_bottom_bits = 2
  let mask_selecting_top_bits = (-1) lsl num_empty_bottom_bits
  let mask_selecting_bottom_bits = lnot mask_selecting_top_bits

  let flags_size_in_bits = num_empty_bottom_bits

  let flags t = t land mask_selecting_bottom_bits

  let [@inline always] with_flags t flags =
    if flags < 0 || flags > mask_selecting_bottom_bits then begin
      Misc.fatal_errorf "Flags value 0x%x out of range" flags
    end;
    t lor flags

  let without_flags t = t land mask_selecting_top_bits

  let hash t = t
end

module Make (E : sig
  type t

  val print : Format.formatter -> t -> unit
  val hash : t -> int
  val equal : t -> t -> bool
end) = struct
  module HT = Hashtbl.Make (Int)

  type t = E.t HT.t

  let create () = HT.create 10_000

  exception Can_add of int
  exception Already_added of int

  let add t elt =
    let hash = (E.hash elt) land Id.mask_selecting_top_bits in
    match HT.find t hash with
    | exception Not_found ->
      HT.add t hash elt;
      hash
    | existing_elt ->
      if E.equal elt existing_elt then begin
        hash
      end else begin
        try
          let starting_hash = hash in
          let hash = ref (starting_hash + 1) in
          while !hash <> starting_hash do
            while !hash land Id.mask_selecting_bottom_bits <> 0 do
              incr hash;
            done;
            match HT.find t !hash with
            | exception Not_found -> raise (Can_add !hash)
            | existing_elt ->
              if E.equal elt existing_elt then raise (Already_added !hash)
              else incr hash
          done;
          Misc.fatal_errorf "No hash values left for@ %a" E.print elt
        with (Can_add hash) | (Already_added hash) -> begin
          HT.add t hash elt;
          hash
        end
      end

  let find t id =
    assert (id land Id.mask_selecting_bottom_bits = 0);
    HT.find t id
end
