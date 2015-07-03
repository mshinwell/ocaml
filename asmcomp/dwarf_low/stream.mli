(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2015, Jane Street Holding                                *)
(*                                                                     *)
(*  Licensed under the Apache License, Version 2.0 (the "License");    *)
(*  you may not use this file except in compliance with the License.   *)
(*  You may obtain a copy of the License at                            *)
(*                                                                     *)
(*      http://www.apache.org/licenses/LICENSE-2.0                     *)
(*                                                                     *)
(*  Unless required by applicable law or agreed to in writing,         *)
(*  software distributed under the License is distributed on an        *)
(*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,       *)
(*  either express or implied.  See the License for the specific       *)
(*  language governing permissions and limitations under the License.  *)
(*                                                                     *)
(***********************************************************************)

(** Basic stream-based input from files. *)

type t

val open_file : filename:string -> t
val close : t -> unit

val read_int8 : t -> Int8.t Or_error.t
val read_int8_as_int : t -> int Or_error.t
val read_int8_as_int16 : t -> Int32.t Or_error.t
val read_int8_as_int32 : t -> Int64.t Or_error.t

val read_int16 : t -> Int16.t Or_error.t
val read_int32 : t -> Int32.t Or_error.t
val read_int64 : t -> Int64.t Or_error.t

val read_null_terminated_string : t -> string Or_error.t
