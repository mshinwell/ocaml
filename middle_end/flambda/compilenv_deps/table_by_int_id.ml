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

  let num_empty_bottom_bits = 3
  let mask_selecting_top_bits = (-1) lsl num_empty_bottom_bits
  let mask_selecting_bottom_bits = lnot mask_selecting_top_bits

  let flags_size_in_bits = num_empty_bottom_bits

  let flags t = t land mask_selecting_bottom_bits

  let without_flags t = t land mask_selecting_top_bits

  let [@inline always] with_flags t flags =
    if flags < 0 || flags >= (1 lsl (flags_size_in_bits + 1)) then begin
      Misc.fatal_errorf "Flags value 0x%x out of range" flags
    end;
    (without_flags t) lor flags
end

module Make (E : sig
  type t

  val flags : int

  val print : Format.formatter -> t -> unit
  val hash : t -> int
  val equal : t -> t -> bool
end) = struct
  module HT = Hashtbl.Make (struct
    type t = int
    (* CR mshinwell: maybe this should be a proper hash function *)
    let hash (t : t) = Hashtbl.hash t
    let equal t1 t2 = (t1 == t2)
  end)

  let () = assert (E.flags land Id.mask_selecting_top_bits = 0)

  type t = E.t HT.t

  let create () = HT.create 20_000

  exception Can_add of int
  exception Already_added of int

  let add t elt ~extra_flags =
    if E.flags land extra_flags <> 0 then begin
      Misc.fatal_errorf "Clash on flags: E.flags = %d, extra_flags = %d"
        E.flags extra_flags
    end;
    if extra_flags land Id.mask_selecting_top_bits <> 0 then begin
      Misc.fatal_errorf "Invalid extra_flags: %d" extra_flags
    end;
    let flags = E.flags lor extra_flags in
    let id = Id.with_flags (E.hash elt) flags in
    match HT.find t id with
    | exception Not_found ->
      Format.eprintf "adding %x (case 1) fl %d\n%!" id E.flags;
      HT.add t id elt;
      id
    | existing_elt ->
      if E.equal elt existing_elt then begin
        id
      end else begin
        try
          let starting_id = id in
          let id = ref (starting_id + 1) in
          (* If there is a collision, we search for another slot, but take
             care not to alter the flags bits. *)
          while !id <> starting_id do
            (* CR mshinwell: performance could be improved *)
            while Id.flags !id <> flags do
              incr id;
            done;
            match HT.find t !id with
            | exception Not_found -> raise (Can_add !id)
            | existing_elt ->
              if E.equal elt existing_elt then raise (Already_added !id)
              else incr id
          done;
          Misc.fatal_errorf "No hash values left for@ %a" E.print elt
        with
        | Can_add id ->
          HT.add t id elt;
          Format.eprintf "adding %x (case 2) fl %d\n%!" id E.flags;
          assert (Id.flags id = flags);
          id
        | Already_added id ->
          Format.eprintf "adding %x (case 3) fl %d\n%!" id E.flags;
          assert (Id.flags id = flags);
          id
      end

  let find t id =
    assert (Id.flags id land E.flags = E.flags);
    Printf.eprintf "E.flags = %d, finding %x\n%!" E.flags id;
    HT.find t id
end
