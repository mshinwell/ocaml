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

type t

(** Printing, invariant checks, name manipulation, etc. *)
include Expr_std.S with type t := t

(** Create a set of closures given the code for its functions and the
    closure variables. *)
val create
   : function_decls:Function_declarations.t
  -> closure_elements:Simple.t Var_within_closure.Map.t
  -> t

(** The function declarations associated with the set of closures. *)
val function_decls : t -> Function_declarations.t

(** The map from the closure's environment entries to their values. *)
val closure_elements : t -> Simple.t Var_within_closure.Map.t

(** Returns true iff the given set of closures has an empty environment. *)
val has_empty_environment : t -> bool
