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

(** The representation of the application of an OCaml function, OCaml method
    or external call to a list of arguments. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

(** Printing, invariant checks, name manipulation, etc. *)
include Expr_std.S with type t := t

(** Create an application expression. *)
val create
   : callee:Name.t
  -> continuation:Continuation.t
  -> exn_continuation:Exn_continuation.t
  -> args:Simple.t list
  -> call_kind:Call_kind.t
  -> dbg:Debuginfo.t
  -> inline:Inline_attribute.t
  -> specialise:Specialise_attribute.t
  -> t

(** The function or method being applied. *)
val callee : t -> Name.t

(** The arguments of the function or method being applied. *)
val args : t -> Simple.t list

(** Information about what kind of call is involved (direct function call,
    method call, etc). *)
val call_kind : t -> Call_kind.t

(** Where to send the result of the application. *)
val continuation : t -> Continuation.t

(** Where to jump to upon the application raising an exception. *)
val exn_continuation : t -> Exn_continuation.t

(** Debugging information attached to the application. *)
val dbg : t -> Debuginfo.t

(** Instructions from the source code as to whether the callee should be
    inlined. *)
val inline : t -> Inline_attribute.t

(** Instructions from the source code as to whether the callee should be
    specialised. *)
val specialise : t -> Specialise_attribute.t

(** Change the return continuation of an application. *)
val with_continuation : t -> Continuation.t -> t
