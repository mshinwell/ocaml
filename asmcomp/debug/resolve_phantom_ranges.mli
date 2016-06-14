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

(** Resolve transitive references to identifiers in the defining
    expressions of phantom lets. *)

val run
   : (Clambda.ulet_provenance * Clambda.uphantom_defining_expr) Ident.Map.t
  -> (Clambda.ulet_provenance * Mach.phantom_defining_expr) Ident.Map.t
