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

module Gc_stats : sig
  type t

  val minor_words : t -> int
  val promoted_words : t -> int
  val major_words : t -> int
  val minor_collections : t -> int
  val major_colections : t -> int
  val heap_words : t -> int
  val heap_chunks : t -> int
  val compactions : t -> int
  val top_heap_words : t -> int
end = struct
  type t = {
    minor_words : int;
    promoted_words : int;
    major_words : int;
    minor_collections : int;
    major_colections : int;
    heap_words : int;
    heap_chunks : int;
    compactions : int;
    top_heap_words : int;
  }

  let minor_words t = t.minor_words
  let promoted_words t = t.promoted_words
  let major_words t = t.major_words
  let minor_collections t = t.minor_collections
  let major_colections t = t.major_collections
  let heap_words t = t.heap_words
  let heap_chunks t = t.heap_chunks
  let compactions t = t.compactions
  let top_heap_words t = t.top_heap_words
end

module Snapshot_entries : sig
  type t

  val length : t -> int

  val num_blocks : t -> index:int -> int
  val num_words_including_headers : t -> index:int -> int
end = struct
  type t = int array

  let length t =
    let length = Array.length t in
    assert (length mod 2 = 0);
    length / 2

  let num_blocks t ~index =
    if index < 0 || index >= length t then begin
      invalid_arg "Allocation_profiling.Snapshot_entries.num_blocks"
    end;
    t.(index * 2)

  let num_words_including_headers t ~index =
    if index < 0 || index >= length t then begin
      invalid_arg
        "Allocation_profiling.Snapshot_entries.num_words_including_headers"
    end;
    t.(index*2 + 1)
end

module Snapshot : sig
  type t

  val gc_stats : t -> Gc_stats.t
  val entries : t -> Snapshot_entries.t
end = struct
  type t = {
    gc_stats : Gc_stats.t;
    entries : Snapshot_entries.t;
  }

  let gc_stats t = t.gc_stats
  let entries t = t.entries
end

module Program_counter : sig
  type t
end = struct

end

module Profinfo : sig
  type t
end = struct

end

module Trie : sig
  type t
  type node

  module Allocation_point : sig
    type t

    val location : t -> Program_counter.t
    val profinfo : t -> Profinfo.t
  end = struct

  end

  module Call_point : sig
    type t

    val call_site : t -> Program_counter.t
    val callee : t -> Program_counter.t option
    val child_node : t -> node option

    val is_tail : t -> bool
    val is_indirect : t -> bool
  end = struct

  end

  module Node : sig
    type t = node

    val node_identifier : t -> int
    val tail_chain : t -> t list

    val allocation_points : t -> Allocation_point.t list
    val call_points : t -> Call_point.t list
  end
end = struct

end

module Profile : sig
  type t

  val snapshots : t -> Snapshot.t list
  val trie : t -> Trie.t
end = struct

end

(* CR mshinwell: must ensure -allocation-profiling does not appear as a
   compiler option if WITH_ALLOCATION_PROFILING was not set *)

external annotation_of_value : 'a -> int
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_allocation_profiling_get_profinfo"

external dump_allocators_of_major_heap_blocks
   : filename:string
  -> dump_num_unaccounted_for:int
  -> unit
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_dump_allocators_of_major_heap_blocks_from_ocaml"

let dump_allocators_of_major_heap_blocks ?(dump_num_unaccounted_for = 0)
      ~filename () =
  dump_allocators_of_major_heap_blocks ~filename ~dump_num_unaccounted_for

external erase_profiling_annotations : unit -> unit
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_forget_where_values_were_allocated"

external dump_heapgraph : node_filename:string
  -> edge_filename:string
  -> unit
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_dump_heapgraph_from_ocaml"

external annotate_values_with_allocation_location : unit -> unit
  = "%identity" (*"caml_do_not_override_profinfo" "noalloc"*)

external annotate_values_with_given_integer : int -> bool
= "%identity"
(*
    = "caml_set_override_profinfo" "noalloc"
*)

let annotate_values_with_given_integer i =
  if i < 0 then `Out_of_range
  else if annotate_values_with_given_integer i then `Ok
  else `Out_of_range

external max_annotation_value : unit -> int
= "%identity"
(*
  = "caml_allocation_profiling_max_override_profinfo" "noalloc"
*)

(*
external dump_backtraces_to_file : filename:string -> unit
  = "caml_allocation_profiling_dump_backtraces_to_file"
*)

type profinfo = int

external min_override_profinfo : unit -> profinfo
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_allocation_profiling_min_override_profinfo" "noalloc"

external max_override_profinfo : unit -> profinfo
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_allocation_profiling_max_override_profinfo" "noalloc"

external profinfo_none : unit -> profinfo
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_allocation_profiling_profinfo_none" "noalloc"

external profinfo_overflow : unit -> profinfo
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_allocation_profiling_profinfo_none" "noalloc"

external get_profinfo : 'a -> profinfo
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_allocation_profiling_get_profinfo" "noalloc"

external trie_is_initialized : unit -> bool
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_allocation_profiling_trie_is_initialized" "noalloc"

external get_trie_root : unit -> Obj.t
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_allocation_profiling_get_trie_root" "noalloc"

(*
let debug () =
  Printf.printf "trie initialized? %b\n%!" (trie_is_initialized ())
*)

external debug : unit -> unit
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_allocation_profiling_debug" "noalloc"

external marshal_trie : out_channel -> unit
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_allocation_profiling_marshal_trie"

type trie_node = Obj.t array

type trie =
  | Empty
  | Non_empty of trie_node

let unmarshal_trie in_channel =
  let trie = Marshal.from_channel in_channel in
  if trie = () then
    Empty
  else
    Non_empty ((Obj.magic trie) : trie_node)
