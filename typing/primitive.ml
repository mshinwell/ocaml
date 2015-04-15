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

open Misc
open Parsetree

type unbox =
  | Do_not_unbox
  | Unbox_float
  | Unbox_int32
  | Unbox_int64
  | Unbox_nativeint

type description =
  { prim_name: string;         (* Name of primitive  or C function *)
    prim_arity: int;           (* Number of arguments *)
    prim_alloc: bool;          (* Does it allocates or raise? *)
    prim_native_name: string;  (* Name of C function for the nat. code gen. *)
    prim_native_unbox_args: unbox list;
    prim_native_unbox_res: unbox }

type error =
  | Float_with_unbox_attribute

exception Error of Location.t * error

let is_unbox = function
  | Do_not_unbox -> false
  | Unbox_float
  | Unbox_int32
  | Unbox_int64
  | Unbox_nativeint -> true

let rec make_unbox_args arity x =
  if arity = 0 then
    []
  else
    x :: make_unbox_args (arity - 1) x

let simple ~name ~arity ~alloc =
  {prim_name = name;
   prim_arity = arity;
   prim_alloc = alloc;
   prim_native_name = "";
   prim_native_unbox_args = make_unbox_args arity Do_not_unbox;
   prim_native_unbox_res = Do_not_unbox}

let parse_declaration valdecl ~native_unbox_args ~native_unbox_res =
  let arity = List.length native_unbox_args in
  let name, native_name, noalloc, float =
    match valdecl.pval_prim with
    | name :: "noalloc" :: name2 :: "float" :: _ -> (name, name2, true, true)
    | name :: "noalloc" :: name2 :: _ -> (name, name2, true, false)
    | name :: name2 :: "float" :: _ -> (name, name2, false, true)
    | name :: "noalloc" :: _ -> (name, "", true, false)
    | name :: name2 :: _ -> (name, name2, false, false)
    | name :: _ -> (name, "", false, false)
    | [] ->
        fatal_error "Primitive.parse_declaration"
  in
  (* The compiler used to assume "noalloc" with "float", we just make this
     explicit now: *)
  let noalloc = noalloc || float in
  if float &&
     (List.exists is_unbox native_unbox_args || is_unbox native_unbox_res) then
    raise (Error (valdecl.pval_loc, Float_with_unbox_attribute));
  let native_unbox_args, native_unbox_res =
    if float then
      (make_unbox_args arity Unbox_float, Unbox_float)
    else
      (native_unbox_args, native_unbox_res)
  in
  {prim_name = name;
   prim_arity = arity;
   prim_alloc = not noalloc;
   prim_native_name = native_name;
   prim_native_unbox_args = native_unbox_args;
   prim_native_unbox_res = native_unbox_res}

let description_list p =
  let list = [p.prim_name] in
  let list = if not p.prim_alloc then "noalloc" :: list else list in
  let list =
    if p.prim_native_name <> "" then p.prim_native_name :: list else list
  in
  let list =
    let is_unbox_float x = x = Unbox_float in
    if List.for_all is_unbox_float p.prim_native_unbox_args &&
       is_unbox_float p.prim_native_unbox_res then
      "float" :: list
    else
      list
  in
  List.rev list

let native_name p =
  if p.prim_native_name <> ""
  then p.prim_native_name
  else p.prim_name

let byte_name p =
  p.prim_name

let report_error ppf err =
  let open Format in
  match err with
  | Float_with_unbox_attribute ->
      fprintf ppf "Cannot use \"float\" in conjunction with [@unbox ]"

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
        Some (Location.error_of_printer loc report_error err)
      | _ ->
        None
    )
