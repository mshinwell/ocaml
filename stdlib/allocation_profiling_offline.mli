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

(** Access to the information recorded by the [Allocation_profiling]
    module.  (It is intended that this module will be used by
    post-processors rather than users wishing to understand their
    programs.)
    For 64-bit targets only.
    This module may be used from any program, not just one compiled
    with -allocation-profiling. *)

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
end

module Annotation : sig
  (** An annotation written into a value's header.  These may be looked up
      in a [Trace.t] (see below). *)
  type t

  (* CR-someday mshinwell: consider using tag and size to increase the
     available space of annotations.  Need to be careful of [Obj.truncate].
     Could also randomise the tags on records.
  *)

  val of_int : int -> t option
  val to_int : t -> int

  val lowest_allowable : t Lazy.t
  val highest_allowable : t Lazy.t
end

(* CR mshinwell: rationalise this next part.  OCaml/C PC types. *)

module Program_counter : sig
  type t

  val to_int64 : t -> Int64.t
end

module Frame_table : sig
  (* CR-someday mshinwell: move to [Gc] if dependencies permit? *)
  (** A value of type [t] corresponds to the frame table of a running
      OCaml program.  The table is indexed by return address. *)
  (* XXX do these exactly match with the call site addresses? ... *)
  type t = private (Program_counter.t, Printexc.Slot.t) Hashtbl.t
end

module Function_entry_point : sig
  type t

  val to_int64 : t -> Int64.t
end

module Function_identifier : sig
  (* CR mshinwell: check if this is actually [Function_entry_point.t] now *)
  type t

  val to_int64 : t -> Int64.t
end

module Call_site : sig
  type t = private Int64.t
end

module Trace : sig
  (** A value of type [t] holds the dynamic call structure of the program
      (i.e. which functions have called which other functions) together with
      information required to decode profiling annotations written into
      values' headers. *)
  type t

  type node
  type ocaml_node
  type c_node
  type uninstrumented_node

  module OCaml_node : sig
    (** A node corresponding to an invocation of a function written in
        OCaml. *)
    type t = ocaml_node

    (** A unique identifier for the function corresponding to this node. *)
    val function_identifier : t -> Function_identifier.t

    (** This function traverses a circular list. *)
    val next_in_tail_call_chain : t -> t

    module Allocation_point : sig
      (** A value of type [t] corresponds to an allocation point in OCaml
          code. *)
      type t

      (** The program counter at (or close to) the allocation site. *)
      val program_counter : t -> Program_counter.t

      (** The annotation written into the headers of boxed values allocated
          at the given allocation site. *)
      val annotation : t -> Annotation.t
    end

    module Direct_call_point_in_ocaml_code : sig
      (** A value of type ['target t] corresponds to a direct (i.e. known
          at compile time) call point in OCaml code.  ['target] is the type
          of the node corresponding to the callee. *)
      type 'target t

      (** The program counter at (or close to) the call site. *)
      val call_site : _ t -> Call_site.t

      (** The address of the first instruction of the callee. *)
      val callee : _ t -> Function_entry_point.t

      (** The node corresponding to the callee. *)
      val callee_node : 'target t -> 'target
    end

    module Indirect_call_point_in_ocaml_code : sig
      (** A value of type [t] corresponds to an indirect call point in OCaml
          code.  Each such value contains a list of callees to which the
          call point has branched. *)
      type t

      (** The program counter at (or close to) the call site. *)
      val call_site : t -> Call_site.t

      module Callee_iterator : sig
        type t

        (** The address of the first instruction of the callee. *)
        val callee : t -> Function_entry_point.t

        (** The node corresponding to the callee. *)
        val callee_node : t -> node

        (** Move to the next callee to which this call point has branched.
            [None] is returned when the end of the list is reached. *)
        val next : t -> t option
      end

      (** The list of callees to which this indirect call point has
          branched. *)
      val callees : t -> Callee_iterator.t option
    end

    module Field_iterator : sig
      (** A value of type [t] enables iteration through the contents ("fields")
          of an OCaml node. *)
      type t

      type direct_call_point =
        | To_ocaml of ocaml_node Direct_call_point_in_ocaml_code.t
        | To_c of c_node Direct_call_point_in_ocaml_code.t
        (* CR mshinwell: once everything's finished, "uninstrumented" should
           be able to go away. *)
        | To_uninstrumented of
            uninstrumented_node Direct_call_point_in_ocaml_code.t

      type classification =
        | Allocation_point of Allocation_point.t
        | Direct_call_point of direct_call_point
        | Indirect_call_point of Indirect_call_point_in_ocaml_code.t

      val classify : t -> classification
      val next : t -> t option
    end

    val fields : t -> Field_iterator.t option
  end

  module C_node : sig
    (** A node corresponding to an invocation of a function written in C
        (or any other language that is not OCaml). *)
    type t = c_node

    module Allocation_point : sig
      (** A value of type [t] corresponds to an allocation point in non-OCaml
          code. *)
      type t

      val program_counter : t -> Program_counter.t
      val annotation : t -> Annotation.t
    end

    module Call_point : sig
      (** A value of type [t] corresponds to a call point from non-OCaml
          code (to either non-OCaml code, or OCaml code via the usual
          assembly veneer). *)
      type t

      (** N.B. The address of the callee (of type [Function_entry_point.t]) is
          not available.  It must be recovered during post-processing. *)
      val call_site : t -> Call_site.t
      val callee_node : t -> node
    end

    (* CR mshinwell: consider removing "_iterator" suffixes *)
    module Field_iterator : sig
      (** A value of type [t] enables iteration through the contents ("fields")
          of a C node. *)
      type t

      type classification = private
        | Allocation_point of Allocation_point.t
        | Call_point of Call_point.t

      val classify : t -> classification
      val next : t -> t option
    end

    val fields : t -> Field_iterator.t option
  end

  module Node : sig
    (** Either an OCaml or a C node; or an indication that this is a branch
        of the graph corresponding to uninstrumented code. *)
    type t = node

    val compare : t -> t -> int

    type classification = private
      | OCaml of OCaml_node.t
      | C of C_node.t

    val classify : t -> classification

    val of_ocaml_node : OCaml_node.t -> t
    val of_c_node : C_node.t -> t

    module Set : Set.S with type elt = t
    module Map : Map.S with type key = t
  end

  (** Obtains the root of the graph for traversal.  [None] is returned if
      the graph is empty. *)
  val root : t -> Node.t option

  (** Dump an unmarshalled trace to stdout (version written in OCaml). *)
  val debug_ocaml : t -> unit
end

module Heap_snapshot : sig
  type t
  type heap_snapshot = t

  module Entry : sig
    (** A value of type [t] provides the total number of blocks (= boxed
        values) and the total number of words occupied by such blocks
        (including their headers) for some given profiling annotation in
        the heap. *)
    type t

    val num_blocks : t -> int
    val num_words_including_headers : t -> int
  end

  module Entries : sig
    (** Numbers of blocks and words, as above, indexed by profiling
        annotation. *)
    type t = private (Annotation.t, Entry.t) Hashtbl.t
  end

  (** The timestamp of a snapshot.  The units are as for [Sys.time]. *)
  val timestamp : t -> float

  val gc_stats : t -> Gc_stats.t
  val entries : t -> Entries.t

  module Series : sig
    type t

    (** At present, the [Trace.t] associated with a [Series.t] cannot be
        garbage collected or freed.  This should not be a problem, since
        the intention is that a post-processor reads the trace and outputs
        another format. *)
    val read : pathname_prefix:string -> t

    val time_of_writer_close : t -> float
    val trace : t -> Trace.t
    val frame_table : t -> Frame_table.t
    val num_snapshots : t -> int
    val snapshot : t -> index:int -> heap_snapshot
  end
end
