(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type t =
  | Boxed_float of float
  | Boxed_int32 of Int32.t
  | Boxed_int64 of Int64.t
  | Boxed_nativeint of Targetint.t
  | Mutable_float_array of { initial_value : float list; }
  | Immutable_float_array of float list
  | Mutable_string of { initial_value : string; }
  | Immutable_string of string

let tag t =
  match t with
  | Boxed_float _ -> Tag.double_tag
  | Boxed_int32 _
  | Boxed_int64 _
  | Boxed_nativeint _ -> Tag.custom_tag
  | Mutable_float_array _
  | Immutable_float_array _ -> Tag.double_array_tag
  | Mutable_string _
  | Immutable_string _ -> Tag.string_tag

let compare (x : t) (y : t) =
  let compare_floats x1 x2 =
    (* It is important to compare the bit patterns here, so as not to
       be subject to bugs such as GPR#295. *)
    Int64.compare (Int64.bits_of_float x1) (Int64.bits_of_float x2)
  in
  let rec compare_float_lists l1 l2 =
    match l1, l2 with
    | [], [] -> 0
    | [], _::_ -> -1
    | _::_, [] -> 1
    | h1::t1, h2::t2 ->
      let c = compare_floats h1 h2 in
      if c <> 0 then c else compare_float_lists t1 t2
  in
  match x, y with
  | Boxed_float x, Boxed_float y -> compare_floats x y
  | Boxed_int32 x, Boxed_int32 y -> Int32.compare x y
  | Boxed_int64 x, Boxed_int64 y -> Int64.compare x y
  | Boxed_nativeint x, Boxed_nativeint y -> Targetint.compare x y
  | Mutable_float_array { initial_value = x; },
    Mutable_float_array { initial_value = y; } -> compare_float_lists x y
  | Immutable_float_array x, Immutable_float_array y -> compare_float_lists x y
  | Mutable_string { initial_value = x; },
    Mutable_string { initial_value = y; } -> compare x y
  | Immutable_string x, Immutable_string y -> compare x y
  | Boxed_float _, _ -> -1
  | _, Boxed_float _ -> 1
  | Boxed_int32 _, _ -> -1
  | _, Boxed_int32 _ -> 1
  | Boxed_int64 _, _ -> -1
  | _, Boxed_int64 _ -> 1
  | Boxed_nativeint _, _ -> -1
  | _, Boxed_nativeint _ -> 1
  | Mutable_float_array _, _ -> -1
  | _, Mutable_float_array _ -> 1
  | Immutable_float_array _, _ -> -1
  | _, Immutable_float_array _ -> 1
  | Mutable_string _, _ -> -1
  | _, Mutable_string _ -> 1

let print ppf (t : t) =
  let fprintf = Format.fprintf in
  let floats ppf fl =
    List.iter (fun f -> fprintf ppf "@ %f" f) fl
  in
  match t with
  | Mutable_string { initial_value = s; } -> fprintf ppf "%S" s
  | Immutable_string s -> fprintf ppf "#%S" s
  | Boxed_int32 n -> fprintf ppf "%lil" n
  | Boxed_int64 n -> fprintf ppf "%LiL" n
  | Boxed_nativeint n -> fprintf ppf "%an" Targetint.print n
  | Boxed_float f -> fprintf ppf "%f" f
  | Mutable_float_array { initial_value = []; } -> fprintf ppf "[| |]"
  | Mutable_float_array { initial_value = (f1 :: fl); } ->
    fprintf ppf "@[<1>[|@[%f%a@]|]@]" f1 floats fl
  | Immutable_float_array [] -> fprintf ppf "[|# |]"
  | Immutable_float_array (f1 :: fl) ->
    fprintf ppf "@[<1>[|# @[%f%a@]|]@]" f1 floats fl
