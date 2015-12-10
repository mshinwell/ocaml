(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(** Assign numerical offsets, within closure blocks, for code pointers and
    environment entries. *)

type result = private {
  function_offsets : int Closure_id.Map.t;
  free_variable_offsets : int Var_within_closure.Map.t;
}

val compute : Flambda.program -> result

(** If compilation unit [C] references [B], which contains functions inlined
    from another compilation unit [A], then we may need to know the layout of
    closures inside (or constructed by code inside) a.cmx in order to
    compile c.cmx.  Unfortunately a.cmx is permitted to be absent during such
    compilation; c.cmx will be compiled using just b.cmx.  As such, when
    building the .cmx export information for a given compilation unit, we
    also include information about the layout of any closures that it depends
    on from other compilation units.  This means that when situations as just
    describe arise, we always have access to the necessary closure offsets. *)
val compute_reexported_offsets
   : Flambda.program
  -> get_fun_offset:(Closure_id.t -> int)
  -> get_fv_offset:(Var_within_closure.t -> int)
  -> int Closure_id.Map.t * int Var_within_closure.Map.t
