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

module Id = struct
  include Numbers.Int

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

  let create () = HT.create ()

  exception Can_add of int

  let min_positive_hash = 1 lsl Id.num_empty_bottom_bits
  let max_negative_hash = -(min_positive_hash + 1)

  let add t elt =
    let hash = (E.hash elt) land Id.mask_selecting_top_bits in
    if not (HT.mem t hash) then begin
      HT.add t hash elt;
      hash
    end else begin
      try
        let starting_hash = hash in
        let hash = ref (starting_hash + 1) in
        while !hash <> starting_hash do
          if !hash > max_negative_hash && !hash < min_positive_hash then begin
            hash := min_positive_hash + 1
          end;
          if not (HT.mem t !hash) then begin
            raise (Can_add !hash)
          end;
          incr hash
        done;
        Misc.fatal_errorf "No hash values left for@ %a" E.print elt
      with (Can_add hash) -> begin
        HT.add t hash elt;
        hash
      end
    end

  let find t id = HT.find t id
end
