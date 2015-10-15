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

module Program_counter = struct
  type t = Int64.t

  let to_int64 t = t
end

module Function_identifier = struct
  type t = Int64.t

  let to_int64 t = t
end

module Function_entry_point = struct
  type t = Int64.t

  let to_int64 t = t
end

module Call_site = struct
  type t = Int64.t

  let to_int64 t = t
end

module Frame_table = struct
  type t = (Program_counter.t, Printexc.Slot.t) Hashtbl.t
end

module Annotation = struct
  type t = int

  external lowest_allowable : unit -> t
    = "caml_allocation_profiling_only_works_for_native_code"
      "caml_allocation_profiling_min_override_profinfo" "noalloc"

  let lowest_allowable = lazy (lowest_allowable ())

  external highest_allowable : unit -> t
    = "caml_allocation_profiling_only_works_for_native_code"
      "caml_allocation_profiling_max_override_profinfo" "noalloc"

  let highest_allowable = lazy (highest_allowable ())

  let of_int t =
    if t >= Lazy.force lowest_allowable
      && t <= Lazy.force highest_allowable
    then Some t
    else None

  let to_int t = t
end

module Trace = struct
  type node
  type ocaml_node
  type c_node
  type uninstrumented_node

  type t = node option

  (* This function unmarshals into malloc blocks, which mean that we
     obtain a straightforward means of writing [compare] on [node]s. *)
  external unmarshal : in_channel -> 'a
    = "caml_allocation_profiling_only_works_for_native_code"
      "caml_allocation_profiling_unmarshal_trie"

  let unmarshal in_channel =
    let trace = unmarshal in_channel in
    if trace = () then
      None
    else
      Some ((Obj.magic trace) : node)

  let node_is_null (node : node) =
    ((Obj.magic node) : unit) == ()

  let c_node_is_null (node : c_node) =
    ((Obj.magic node) : unit) == ()

  external node_num_header_words : unit -> int
    = "caml_allocation_profiling_only_works_for_native_code"
      "caml_allocation_profiling_node_num_header_words" "noalloc"

  let num_header_words = lazy (node_num_header_words ())

  module OCaml_node = struct
    type t = ocaml_node

    external function_identifier : t -> Function_identifier.t
      = "caml_allocation_profiling_only_works_for_native_code"
        "caml_allocation_profiling_ocaml_function_identifier"

    external next_in_tail_call_chain : t -> t
      = "caml_allocation_profiling_only_works_for_native_code"
        "caml_allocation_profiling_ocaml_tail_chain" "noalloc"

    type field_iterator = {
      node : t;
      offset : int;
    }

    module Allocation_point = struct
      type t = field_iterator

      external program_counter : ocaml_node -> int -> Program_counter.t
        = "caml_allocation_profiling_only_works_for_native_code"
          "caml_allocation_profiling_ocaml_allocation_point_program_counter"

      let program_counter t = program_counter t.node t.offset

      external annotation : ocaml_node -> int -> Annotation.t
        = "caml_allocation_profiling_only_works_for_native_code"
          "caml_allocation_profiling_ocaml_allocation_point_annotation"
          "noalloc"

      let annotation t = annotation t.node t.offset
    end

    module Direct_call_point_in_ocaml_code = struct
      type _ t = field_iterator

      external call_site : ocaml_node -> int -> Program_counter.t
        = "caml_allocation_profiling_only_works_for_native_code"
          "caml_allocation_profiling_ocaml_direct_call_point_call_site"

      let call_site t = call_site t.node t.offset

      external callee : ocaml_node -> int -> Function_entry_point.t
        = "caml_allocation_profiling_only_works_for_native_code"
          "caml_allocation_profiling_ocaml_direct_call_point_callee"

      let callee t = callee t.node t.offset

      external callee_node : ocaml_node -> int -> 'target
        = "caml_allocation_profiling_only_works_for_native_code"
          "caml_allocation_profiling_ocaml_direct_call_point_callee_node"

      let callee_node (type target) (t : target t) : target =
        callee_node t.node t.offset
    end

    module Indirect_call_point_in_ocaml_code = struct
      type t = field_iterator

      external call_site : ocaml_node -> int -> Program_counter.t
        = "caml_allocation_profiling_only_works_for_native_code"
          "caml_allocation_profiling_ocaml_indirect_call_point_call_site"

      let call_site t = call_site t.node t.offset

      module Callee_iterator = struct
        type t = c_node

        let is_null = c_node_is_null

        (* CR mshinwell: maybe rename ...c_node_call_site -> c_node_pc,
           since it isn't a call site in this case. *)
        external callee : t -> Function_entry_point.t
          = "caml_allocation_profiling_only_works_for_native_code"
            "caml_allocation_profiling_c_node_call_site"

        (* XXX this might return an unused tail call site *)
        (* is this possible now?  OCaml -> C is never tail, and
           all other code should be instrumented *)
        external callee_node : t -> node
          = "caml_allocation_profiling_only_works_for_native_code"
            "caml_allocation_profiling_c_node_callee_node" "noalloc"

        external next : t -> c_node
          = "caml_allocation_profiling_only_works_for_native_code"
            "caml_allocation_profiling_c_node_next" "noalloc"

        let next t =
          let next = next t in
          if c_node_is_null next then None
          else Some next
      end

      external callees : ocaml_node -> int -> Callee_iterator.t
        = "caml_allocation_profiling_only_works_for_native_code"
          "caml_allocation_profiling_ocaml_indirect_call_point_callees"
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
        = "caml_allocation_profiling_only_works_for_native_code"
          "caml_allocation_profiling_ocaml_classify_field" "noalloc"

      let classify t =
        match classify t.node with
        | 0 -> Allocation_point t
        | 1 -> Direct_call_point (To_uninstrumented t)
        | 2 -> Direct_call_point (To_ocaml t)
        | 3 -> Direct_call_point (To_c t)
        | 4 -> Indirect_call_point t
        | _ -> assert false

      external next : ocaml_node -> int -> int
        = "caml_allocation_profiling_only_works_for_native_code"
          "caml_allocation_profiling_ocaml_node_next" "noalloc"

      let next t =
        let offset = next t.node t.offset in
        if offset < 0 then None
        else Some { t with offset; }
    end

    let fields t =
      let start =
        { node = t;
          offset = Lazy.force num_header_words;
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
        = "caml_allocation_profiling_only_works_for_native_code"
          "caml_allocation_profiling_c_node_call_site"

      external annotation : t -> Annotation.t
        = "caml_allocation_profiling_only_works_for_native_code"
          "caml_allocation_profiling_c_node_profinfo" "noalloc"
    end

    module Call_point = struct
      type t = c_node

      external call_site : t -> Program_counter.t
        = "caml_allocation_profiling_only_works_for_native_code"
          "caml_allocation_profiling_c_node_call_site"

      external callee_node : t -> node
        = "caml_allocation_profiling_only_works_for_native_code"
          "caml_allocation_profiling_c_node_callee_node" "noalloc"
    end

    module Field_iterator = struct
      type t = c_node

      type classification =
        | Allocation_point of Allocation_point.t
        | Call_point of Call_point.t

      external is_call : t -> bool
        = "caml_allocation_profiling_only_works_for_native_code"
          "caml_allocation_profiling_c_node_is_call" "noalloc"

      let classify t =
        if is_call t then Call_point t
        else Allocation_point t

      external next : t -> t
        = "caml_allocation_profiling_only_works_for_native_code"
          "caml_allocation_profiling_c_node_next" "noalloc"

      let next t =
        let next = next t in
        if c_node_is_null next then None
        else Some next
    end

    let fields t =
      if c_node_is_null t then None
      else Some t
  end

  module Node = struct
    module T = struct
      type t = node

      external compare : t -> t -> int
        = "caml_allocation_profiling_only_works_for_native_code"
          "caml_allocation_profiling_compare_node" "noalloc"
    end

    include T

    type classification =
      | OCaml of OCaml_node.t
      | C of C_node.t

    external is_ocaml_node : t -> bool
      = "caml_allocation_profiling_only_works_for_native_code"
        "caml_allocation_profiling_is_ocaml_node" "noalloc"

    let classify t =
      if is_ocaml_node t then OCaml ((Obj.magic t) : ocaml_node)
      else C ((Obj.magic t) : c_node)

    let of_ocaml_node (node : ocaml_node) : t = Obj.magic node
    let of_c_node (node : c_node) : t = Obj.magic node

    module Map = Map.Make (T)
    module Set = Set.Make (T)
  end

  let root t = t

  let debug_ocaml t =
    let next_id = ref 0 in
    let visited = ref Node.Map.empty in
    let rec print_node node =
      match Node.Map.find node !visited with
      | id -> Printf.printf "Node %d visited before.\n" id
      | exception Not_found ->
        let id = !next_id in
        incr next_id;
        visited := Node.Map.add node id !visited;
        match Node.classify node with
        | Node.OCaml node ->
          Printf.printf "Node %d (OCaml node):\n" id;
          let module O = OCaml_node in
          let fun_id = O.function_identifier node in
          Printf.printf "Function identifier for node: %Lx\n"
            (Function_identifier.to_int64 fun_id);
          Printf.printf "Tail chain for node:\n";
          let rec print_tail_chain node' =
            if Node.compare (Node.of_ocaml_node node)
                (Node.of_ocaml_node node') = 0
            then ()
            else begin
              let id =
                match Node.Map.find (Node.of_ocaml_node node') !visited with
                | id -> id
                | exception Not_found ->
                  let id = !next_id in
                  incr next_id;
                  visited :=
                    Node.Map.add (Node.of_ocaml_node node') id !visited;
                  id
              in
              Printf.printf "  Node %d\n" id;
              print_tail_chain (O.next_in_tail_call_chain node')
            end
          in
          print_tail_chain (O.next_in_tail_call_chain node);
          let rec iter_fields index = function
            | None -> ()
            | Some field ->
              Printf.printf "Node %d field %d:\n" id index;
              let module F = O.Field_iterator in
              begin match F.classify field with
              | F.Allocation_point alloc ->
                let pc = O.Allocation_point.program_counter alloc in
                let annot = O.Allocation_point.annotation alloc in
                Printf.printf "Allocation point, pc=%Lx annot=%d\n"
                  (Program_counter.to_int64 pc)
                  (Annotation.to_int annot)
              | F.Direct_call_point (F.To_ocaml direct) ->
                let module D = O.Direct_call_point_in_ocaml_code in
                let call_site = D.call_site direct in
                let callee = D.callee direct in
                let callee_node = D.callee_node direct in
                Printf.printf "Direct OCaml -> OCaml call point, pc=%Lx, \
                    callee=%Lx.  Callee node is:\n"
                  (Program_counter.to_int64 call_site)
                  (Function_entry_point.to_int64 callee);
                print_node (Node.of_ocaml_node callee_node);
                Printf.printf "End of call point\n"
              | F.Direct_call_point (F.To_c direct) ->
                let module D = O.Direct_call_point_in_ocaml_code in
                let call_site = D.call_site direct in
                let callee = D.callee direct in
                let callee_node = D.callee_node direct in
                Printf.printf "Direct OCaml -> C call point, pc=%Lx, \
                    callee=%Lx.  Callee node is:\n"
                  (Program_counter.to_int64 call_site)
                  (Function_entry_point.to_int64 callee);
                print_node (Node.of_c_node callee_node);
                Printf.printf "End of call point\n"
              | F.Direct_call_point (F.To_uninstrumented direct) ->
                let module D = O.Direct_call_point_in_ocaml_code in
                let call_site = D.call_site direct in
                let callee = D.callee direct in
                Printf.printf "Direct OCaml -> uninstrumented call point, \
                    pc=%Lx, callee=%Lx.\n"
                  (Program_counter.to_int64 call_site)
                  (Function_entry_point.to_int64 callee)
              | F.Indirect_call_point indirect ->
                let module I = O.Indirect_call_point_in_ocaml_code in
                let call_site = I.call_site indirect in
                let callees = I.callees indirect in
                let rec iter_callees index = function
                  | None ->
                    Printf.printf "End of callees for indirect call point.\n"
                  | Some callee_iterator ->
                    let module C = I.Callee_iterator in
                    let callee = C.callee callee_iterator in
                    let callee_node = C.callee_node callee_iterator in
                    Printf.printf "Indirect call point in OCaml code, \
                        pc=%Lx callee=%Lx.  Callee node is:\n"
                      (Program_counter.to_int64 call_site)
                      (Function_entry_point.to_int64 callee);
                    print_node callee_node;
                    iter_callees (index + 1) (C.next callee_iterator)
                in
                iter_callees 0 callees
              end;
              iter_fields (index + 1) (F.next field)
          in
          iter_fields 0 (O.fields node);
          Printf.printf "End of node %d.\n" id
        | Node.C node ->
          Printf.printf "Node %d (C node):\n" id;
          let rec iter_fields index = function
            | None -> ()
            | Some field ->
              Printf.printf "Node %d field %d:\n" id index;
              let module F = C_node.Field_iterator in
              begin match F.classify field with
              | F.Allocation_point alloc ->
                let pc = C_node.Allocation_point.program_counter alloc in
                let annot = C_node.Allocation_point.annotation alloc in
                Printf.printf "Allocation point, pc=%Lx annot=%d\n"
                  (Program_counter.to_int64 pc)
                  (Annotation.to_int annot)
              | F.Call_point call ->
                let call_site = C_node.Call_point.call_site call in
                let callee_node = C_node.Call_point.callee_node call in
                Printf.printf "Call point, pc=%Lx.  Callee node is:\n"
                  (Program_counter.to_int64 call_site);
                print_node callee_node;
                Printf.printf "End of call point\n"
              end;
              iter_fields (index + 1) (F.next field)
          in
          iter_fields 0 (C_node.fields node);
          Printf.printf "End of node %d.\n" id
    in
    match root t with
    | None -> Printf.printf "Trace is empty.\n"
    | Some node -> print_node node
end

module Heap_snapshot = struct
  let pathname_suffix_trace = "trace"

  type raw_entries = int array  (* == "struct snapshot_entries" *)

  type raw_snapshot = {
    raw_timestamp : float;
    raw_gc_stats : Gc_stats.t;
    raw_entries : raw_entries;
  }

  let num_raw_entries (raw_entries : raw_entries) =
    let length = Array.length raw_entries in
    assert (length mod 3 = 0);
    length / 3

  let raw_entry_annotation raw_entries entry = raw_entries.(entry*3)
  let raw_entry_num_blocks raw_entries entry = raw_entries.(entry*3 + 1)
  let raw_entry_num_words raw_entries entry = raw_entries.(entry*3 + 2)

  module Entry = struct
    type t = {
      num_blocks : int;
      num_words : int;
    }

    let num_blocks t = t.num_blocks
    let num_words_including_headers t = t.num_words
  end

  module Entries = struct
    type t = (Annotation.t, Entry.t) Hashtbl.t
  end

  type t = {
    timestamp : float;
    gc_stats : Gc_stats.t;
    entries : Entries.t;
  }

  type heap_snapshot = t

  let timestamp t = t.timestamp
  let gc_stats t = t.gc_stats
  let entries t = t.entries

  let transform_raw_snapshot raw_snapshot =
    let raw_entries = raw_snapshot.raw_entries in
    let num_entries = num_raw_entries raw_entries in
    Printf.printf "transform_raw_snapshot: num_entries %d\n%!" num_entries;
    let entries = Hashtbl.create 42 in
    for entry = 0 to num_entries - 1 do
      let annotation = raw_entry_annotation raw_entries entry in
      let entry_record : Entry.t =
        { Entry.
          num_blocks = raw_entry_num_blocks raw_entries entry;
          num_words = raw_entry_num_words raw_entries entry;
        }
      in
      Printf.printf ">>>>>>>>> entry=%d annotation=%d blocks=%d words=%d\n%!"
        entry annotation entry_record.Entry.num_blocks
        entry_record.Entry.num_words;
      assert (not (Hashtbl.mem entries annotation));
      Hashtbl.add entries annotation entry_record
    done;
    { timestamp = raw_snapshot.raw_timestamp;
      gc_stats = raw_snapshot.raw_gc_stats;
      entries;
    }

  module Series = struct
    type t = {
      num_snapshots : int;
      time_of_writer_close : float;
      frame_table : Frame_table.t;
      trace : Trace.t;
      snapshots : heap_snapshot array;
    }

    let read ~pathname_prefix =
      let pathname_prefix = pathname_prefix ^ "." in
      let chn = open_in (pathname_prefix ^ pathname_suffix_trace) in
      let num_snapshots : int = Marshal.from_channel chn in
      let time_of_writer_close : float = Marshal.from_channel chn in
      let frame_table : Frame_table.t = Marshal.from_channel chn in
      let trace = Trace.unmarshal chn in
      close_in chn;
      let snapshots =
        Array.init num_snapshots (fun index ->
          let chn = open_in (pathname_prefix ^ (string_of_int index)) in
          let raw : raw_snapshot = Marshal.from_channel chn in
          close_in chn;
          transform_raw_snapshot raw)
      in
      { num_snapshots;
        time_of_writer_close;
        frame_table;
        trace;
        snapshots;
      }

    let trace t = t.trace
    let num_snapshots t = t.num_snapshots
    let snapshot t ~index = t.snapshots.(index)
    let frame_table t = t.frame_table
    let time_of_writer_close t = t.time_of_writer_close
  end
end
