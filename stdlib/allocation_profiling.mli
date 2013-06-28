(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2013, Jane Street Holding                                *)
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

(* Access to the allocation profiler from OCaml programs. *)

module Source_location : sig
  (* A value of type [t] identifies a point in a program's source code. *)
  type t

  val filename : t -> string option
  val function_name : t -> string option
  val line_number : t -> int option

  val to_string : t -> string
end

type t = [
  | `Not_boxed
  | `Unknown
  | `At_address of Int64.t
  | `At_source_location of Source_location.t
  | `Between_source_locations of
    Source_location.t option * Int64.t * Source_location.t option
]

(* CR mshinwell: enhance functionality to permit discovery of _when_
   a value was allocated. *)

(* [where_was_value_allocated v] attempts to determine the source
   location at which the value [v] was allocated.  If the source location
   cannot be determined, but the virtual memory address of the allocation
   point is known, that address will be returned instead.  The location
   of some values may be completely unknown.  If [v] is not a boxed
   (allocated) value then [`Not_boxed] will be returned.

   This function can resolve source locations even for values allocated
   outside OCaml code (for example in C bindings).

   Upon the first call to this function it will block for a short time
   whilst it reads part of the current program's executable from disk.
   The executable should not be changed on disk whilst the program is
   running if it makes use of this function.

   This function is thread safe, although, if it is called from another
   thread whilst in progress reading the executable file then it may
   return [`At_address] instead of [`At_source_location] or
   [`Between_source_locations] during that period.
*)
val where_was_value_allocated : 'a -> t

(* [where_was_value_allocated_address_only] is like
   [where_was_value_allocated], except that it does not access the
   program's executable on disk, and does not resolve addresses to
   source locations.  It runs in (short) constant time.  It is
   thread safe. *)
val where_was_value_allocated_address_only : 'a
  -> [ `Not_boxed | `Unknown | `At_address of Int64.t ]

(* [to_string t] produces a human-readable representation of [t]. *)
val to_string : t -> string

(* [forget_where_values_were_allocated] erases allocation profiling
   information on all values.  This is useful at the start of some
   benchmarking period---for example to exclude allocations associated
   with one-time startup costs from a profile.

   This function causes a minor garbage collection with the usual
   associated slice of major collection.
*)
val forget_where_values_were_allocated : unit -> unit

val dump_allocators_of_major_heap_blocks : filename:string -> unit

val dump_heapgraph : node_filename:string
  -> edge_filename:string
  -> unit

(* The following is only for the internal use of the OCaml system.
   User code should use the functions provided above. *)
module Source_location_map : sig
  type t
  val create_from_dwarf_then_stuff_into_elf_section_exn : executable:string
    -> run_command:(string -> unit)
    -> unit
  val create_from_elf_section : executable:string -> t option
  val resolve : t
    -> instr_pointer:Int64.t
    -> [ `Exact of Source_location.t
       | `Between of Source_location.t option * Source_location.t option] option
end
