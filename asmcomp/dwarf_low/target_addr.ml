(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2015, Jane Street Group, LLC                             *)
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

(* CR-someday mshinwell: if proper cross compilation support is
   implemented, change this as appropriate. *)
include Nativeint

let parse t ~stream =
  let open Or_error.Monad_infix in
  match Sys.word_size with
  | 8 ->
    Stream.read_int64 stream
    >>= fun addr ->
    Ok (Int64.to_nativeint addr)
  | 4 ->
    Stream.read_int32 stream
    >>= fun addr ->
    Ok (Nativeint.of_int32 addr)
  | size ->
    Error (Printf.sprintf "bad system word size %d for Target_addr" size)
