(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module L = Linearize

let rec filter_inplace (insn : L.instruction) =
  match insn.desc with
  | Lend -> ()
  | _ ->
    insn.available_before
      <- Reg_availability_set.canonicalise insn.available_before;
    filter_inplace insn.next

let fundecl (decl : L.fundecl) =
  if not !Clflags.debug then decl
  else begin
    filter_inplace decl.fun_body;
    decl
  end
