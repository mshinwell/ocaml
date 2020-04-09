(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2020 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

let join_points () = !Clflags.Flambda_2.join_points
let unbox_along_intra_function_control_flow () =
  !Clflags.Flambda_2.unbox_along_intra_function_control_flow
let lift_inconstants () = !Clflags.Flambda_2.lift_inconstants
let backend_cse_at_toplevel () = !Clflags.Flambda_2.backend_cse_at_toplevel

module Expert = struct
  let denest_at_toplevel () = !Clflags.Flambda_2.Expert.denest_at_toplevel
  let code_id_and_symbol_scoping_checks () =
    !Clflags.Flambda_2.Expert.code_id_and_symbol_scoping_checks
end