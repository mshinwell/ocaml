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

(** Compact printer for use when printing types. *)
val print_compact : Format.formatter -> t -> unit

(** Create a function declaration. *)
val create
   : params_and_body:Function_params_and_body.t
  -> result_arity:Flambda_arity.t
  -> stub:bool
  -> dbg:Debuginfo.t
  -> inline:Inline_attribute.t
  -> is_a_functor:bool
  -> recursive:Recursive.t
  -> t

(** The alpha-equivalence class of the function's continuations and
    parameters bound over the code of the function. *)
val params_and_body : t -> Function_params_and_body.t

(** An identifier to provide fast (conservative) equality checking for
    function bodies. *)
val code_id : t -> Code_id.t

(* CR mshinwell: Be consistent: "param_arity" or "params_arity" throughout. *)
val params_arity : t -> Flambda_arity.t

(** The arity of the return continuation of the function.  This provides the
    number of results that the function produces and their kinds. *)
(* CR mshinwell: Be consistent everywhere as regards "result" vs "return"
   arity. *)
val result_arity : t -> Flambda_arity.t

(** A stub function is a generated function used to prepare arguments or
    return values to allow indirect calls to functions with a special
    calling convention.  For instance indirect calls to tuplified functions
    must go through a stub.  Stubs will be unconditionally inlined. *)
val stub : t -> bool

(** Debug info for the function declaration. *)
val dbg : t -> Debuginfo.t

(** Inlining requirements from the source code. *)
val inline : t -> Inline_attribute.t

(** Whether the function is known definitively to be a functor. *)
val is_a_functor : t -> bool

(** Change the parameters and code of a function declaration. *)
val update_params_and_body : t -> Function_params_and_body.t -> t

(** Whether the function is recursive, in the sense of the syntactic analysis
    conducted during closure conversion. *)
val recursive : t -> Recursive.t
