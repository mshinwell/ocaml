(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2013--2015, Jane Street Group, LLC                       *)
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

(* [dump_allocators_of_major_heap_blocks] writes a file that may be
   decoded using tools/allocation-profiling/decode-major-heap.sh
   in order to show, for each block in the major heap, where it was
   allocated.  For example:
     decode-major-heap.sh profile-output-file executable-file

   This function performs a full major GC.

   [dump_num_unaccounted_for] should be set to a positive number if it
   is suspected that some values have not been correctly instrumented.
   This will cause more detailed information to be printed about some
   number of values (up to the specified limit) in the output file.
*)
val dump_allocators_of_major_heap_blocks
   : ?dump_num_unaccounted_for:int
  -> filename:string
  -> unit
  -> unit

(* [dump_heapgraph] writes two files that may be decoded using
   tools/allocation-profiling/decode-heapgraph.sh in order to show
   the graph of values in the heap quotiented by the equivalence
   relation that identifies two values iff they were allocated at
   the same source location.  This enables judgements such as
   "a lot of values were allocated at foo.ml line 36 and they were
   pointed at by values allocated at bar.ml line 42".
   Example script invocation:
     decode-heapgraph.sh node-filename edge-filename executable-file
   This function performs a full major GC.
*)
val dump_heapgraph : node_filename:string
  -> edge_filename:string
  -> unit

(* [erase_profiling_annotations] erases allocation profiling
   information on all values.  This is useful at the start of some
   benchmarking period---for example to exclude allocations associated
   with one-time startup costs from a profile.  It is also important in
   conjunction with [annotate_values_with_given_integer] (see below).
*)
val erase_profiling_annotations : unit -> unit

(* Going forward, annotate all allocated values with their allocation location
   (the default). *)
val annotate_values_with_allocation_location : unit -> unit

(* Going forward, annotate all allocated values with the given integer.
   [`Out_of_range] is returned if the supplied integer is negative or larger
   than [max_annotation_value ()].
   It is not sensible to mix this kind of annotation with the location
   annotation provided by the OCaml system.  To this end, before using
   [annotate_values_with_given_integer], it is recommended to call
   [erase_profiling_annotations] first.
*)
val annotate_values_with_given_integer : int -> [ `Ok | `Out_of_range ]

(* Returns the largest value that may be used for profiling annotations. *)
val max_annotation_value : unit -> int

(* Returns the profiling annotation on a given value.  This is only sensible
   to call after [annotate_values_with_given_integer]. *)
val annotation_of_value : 'a -> int

(*
val dump_backtraces_to_file : filename:string -> unit
*)

val marshal_trie : out_channel -> unit

type trie
val unmarshal_trie : in_channel -> trie

val debug : unit -> unit
