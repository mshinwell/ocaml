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

(* Executable and Linking Format (ELF) file manipulation. *)

(* For a file descriptor open for reading on an [executable], determine
   the offset from the start of the file to the start of the data for
   [section_name].  [None] is returned in the case of an error (including the
   section being absent).  This function does not make any assumptions about
   the position of the file pointer on the fd, but may change that
   position. *)
val byte_offset_of_section
   : executable:Unix.file_descr
  -> section_name:string
  -> int option
