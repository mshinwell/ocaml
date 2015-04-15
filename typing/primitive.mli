(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Description of primitive functions *)

type unbox =
  | Do_not_unbox
  | Unbox_float
  | Unbox_int32
  | Unbox_int64
  | Unbox_nativeint

type description = private
  { prim_name: string;         (* Name of primitive  or C function *)
    prim_arity: int;           (* Number of arguments *)
    prim_alloc: bool;          (* Does it allocates or raise? *)
    prim_native_name: string;  (* Name of C function for the nat. code gen. *)
    prim_native_unbox_args: unbox list;
    prim_native_unbox_res: unbox }

val simple
  :  name:string
  -> arity:int
  -> alloc:bool
  -> description

val parse_declaration
  :  Parsetree.value_description
  -> native_unbox_args:unbox list
  -> native_unbox_res:unbox
  -> description

val description_list: description -> string list

val native_name: description -> string
val byte_name: description -> string

type error =
  | Float_with_unbox_attribute

exception Error of Location.t * error
