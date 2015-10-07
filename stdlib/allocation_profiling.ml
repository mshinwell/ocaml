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
  val major_collections : t -> int
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
    major_collections : int;
    heap_words : int;
    heap_chunks : int;
    compactions : int;
    top_heap_words : int;
  }

  let minor_words t = t.minor_words
  let promoted_words t = t.promoted_words
  let major_words t = t.major_words
  let minor_collections t = t.minor_collections
  let major_collections t = t.major_collections
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

module Trace = struct
  type node
  type ocaml_node
  type c_node

  type t = node option

  external marshal_global : out_channel -> unit
    = "caml_allocation_profiling_only_works_for_native_code"
      "caml_allocation_profiling_marshal_trie"

  let unmarshal in_channel =
    let trace = Marshal.from_channel in_channel in
    if trace = () then
      None
    else
      Some ((Obj.magic trace) : node)

  let node_is_null (node : node) =
    phys_equal () ((Obj.magic node) : unit)

  external node_num_header_words : unit -> int
    = "caml_allocation_profiling_node_num_header_words" "noalloc"

  let num_header_words = node_num_header_words ()

  module OCaml_node = struct
    type t = ocaml_node

    let (=) = phys_equal

    external function_identifier : t -> Function_identifier.t
      = "caml_allocation_profiling_ocaml_function_identifier"

    external next_in_tail_call_chain : t -> t
      = "caml_allocation_profiling_ocaml_tail_chain" "noalloc"

    type field_iterator = {
      node : t;
      offset : int;
    }

    module Allocation_point = struct
      type t = field_iterator

      external program_counter : ocaml_node -> int -> Program_counter.t
        = "caml_allocation_profiling_ocaml_allocation_point_program_counter"

      let program_counter t = program_counter t.node t.offset

      external annotation : ocaml_node -> int -> Annotation.t
        = "caml_allocation_profiling_ocaml_allocation_point_annotation"

      let annotation t = annotation t.node t.offset
    end

    module Direct_call_point_in_ocaml_code = struct
      type _ t = field_iterator

      external call_site : ocaml_node -> int -> Program_counter.t
        = "caml_allocation_profiling_ocaml_direct_call_point_call_site"

      let call_site t = call_site t.node t.offset

      external callee : ocaml_node -> int -> Function_entry_point.t
        = "caml_allocation_profiling_ocaml_direct_call_point_callee"

      let callee t = callee t.node t.offset

      external callee_node : ocaml_node -> int -> 'target
        = "caml_allocation_profiling_ocaml_direct_call_point_callee_node"

      let callee_node (type target) (t : target t) : target =
        callee_node t.node t.offset
    end

    module Indirect_call_point_in_ocaml_code = struct
      type t = field_iterator

      external call_site : ocaml_node -> int -> Program_counter.t
        = "caml_allocation_profiling_ocaml_indirect_call_point_call_site"

      let call_site t = call_site t.node t.offset

      module Callee_iterator = struct
        type t

        let is_null = node_is_null

        external callee : t -> Function_entry_point.t
          = "caml_allocation_profiling_c_node_callee"

        external callee_node : t -> node
          = "caml_allocation_profiling_c_node_callee_node" "noalloc"

        external next : t -> node
          = "caml_allocation_profiling_c_node_next" "noalloc"

        let next t =
          let next = next t in
          if node_is_null next then None
          else Some next
      end

      external callees : ocaml_node -> int -> Callee_iterator.t
        = "caml_allocation_profiling_ocaml_indirect_call_point_callees"
          "noalloc"

      let callees t =
        let callees = callees t.node t.offset in
        if Callee_iterator.is_null callees then None
        else Some callees
    end

    module Field_iterator = struct
      type t = field_iterator

      type direct_call_point =
        | To_ocaml of ocaml_node Direct_call_point_in_ocaml_code.t
        | To_c of c_node Direct_call_point_in_ocaml_code.t
        | To_uninstrumented of
            uninstrumented_node Direct_call_point_in_ocaml_code.t

      type classification =
        | Allocation_point of Allocation_point.t
        | Direct_call_point of direct_call_point
        | Indirect_call_point of Indirect_call_point_in_ocaml_code.t

      external classify : ocaml_node -> int
        = "caml_allocation_profiling_ocaml_classify_field" "noalloc"

      let classify t =
        match classify t with
        | 0 -> Allocation_point t
        | 1 -> Direct_call_point (To_uninstrumented t)
        | 2 -> Direct_call_point (To_ocaml t)
        | 3 -> Direct_call_point (To_c t)
        | 4 -> Indirect_call_point t
        | _ -> assert false

      external next : ocaml_node -> int -> int
        = "caml_allocation_profiling_ocaml_node_next" "noalloc"

      let next t =
        let offset = next t.node t.offset in
        if offset < 0 then None
        else Some { t with offset; }
    end

    let fields t =
      let start =
        { node = t;
          offset = node_num_header_words;
        }
      in
      (* We need to skip to the first populated field. *)
      Field_iterator.next start
  end

  module C_node = struct
    type t = c_node

    module Allocation_point = struct
      type t = c_node

      external program_counter : t -> Program_counter.t
        (* This is not a mistake; the same C function works. *)
        = "caml_allocation_profiling_c_node_call_site"

      external annotation : t -> Annotation.t
        = "caml_allocation_profiling_c_node_profinfo"
    end

    module Call_point = struct
      type t = c_node

      external call_site : t -> Program_counter.t
        = "caml_allocation_profiling_c_node_call_site"

      external callee_node : t -> node
        = "caml_allocation_profiling_c_node_callee_node" "noalloc"
    end

    module Field_iterator = struct
      type t = c_node

      type classification =
        | Allocation_point of Allocation_point.t
        | Call_point of Call_point.t

      external is_call : t -> bool
        = "caml_allocation_profiling_c_node_is_call" "noalloc"

      let classify t =
        if is_call t then Call_point t
        else Allocation_point t

      external next : t -> t
        = "caml_allocation_profiling_c_node_next" "noalloc"

      let next t =
        let next = next t in
        if node_is_null t then None
        else Some t
    end

    let fields t =
      if node_is_null t then None
      else Some t
  end

  module Node = struct
    type t = node

    type classification = private
      | OCaml of OCaml_node.t
      | C of C_node.t

    external is_ocaml_node : t -> bool
      = "caml_allocation_profiling_is_ocaml_node" "noalloc"

    let classify t =
      if is_ocaml_node t then OCaml t
      else C t
  end

  let root t = t

  external debug : unit -> unit
    = "caml_allocation_profiling_only_works_for_native_code"
      "caml_allocation_profiling_debug" "noalloc"
end

(* CR mshinwell: must ensure -allocation-profiling does not appear as a
   compiler option if WITH_ALLOCATION_PROFILING was not set *)

external annotation_of_value : 'a -> int
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_allocation_profiling_get_profinfo"

external erase_profiling_annotations : unit -> unit
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_forget_where_values_were_allocated"

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

module Frame_table = struct
  type t = (Program_counter.t, Frame_descriptor.t) Hashtbl.t

  external num_frame_descriptors : unit -> int
    = "caml_allocation_profiling_num_frame_descriptors" "noalloc"

  external get_frame_descriptor : int -> Printexc.raw_backtrace_slot option
    = "caml_allocation_profiling_get_frame_descriptor"

  external return_address_of_frame_descriptor
     : Printexc.raw_backtrace_slot
    -> Int64.t
    = "caml_allocation_profiling_return_address_of_frame_descriptor"

  let get () =
    let num = num_frame_descriptors () in
    let table = Hashtbl.create num in
    for index = 0 to num - 1 do
      match get_frame_descriptor index with
      | None -> ()
      | Some descr ->
        let return_addr = return_address_of_frame_descriptor descr in
        assert (not (Hashtbl.mem table return_addr));
        Hashtbl.add table return_addr
          (Printexc.convert_raw_backtrace_slot descr)
    done;
    table
end
