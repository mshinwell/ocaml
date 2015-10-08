/***********************************************************************/
/*                                                                     */
/*                               OCaml                                 */
/*                                                                     */
/*           Mark Shinwell and Leo White, Jane Street Europe           */
/*                                                                     */
/*  Copyright 2013--2015, Jane Street Group, LLC                       */
/*                                                                     */
/*  Licensed under the Apache License, Version 2.0 (the "License");    */
/*  you may not use this file except in compliance with the License.   */
/*  You may obtain a copy of the License at                            */
/*                                                                     */
/*      http://www.apache.org/licenses/LICENSE-2.0                     */
/*                                                                     */
/*  Unless required by applicable law or agreed to in writing,         */
/*  software distributed under the License is distributed on an        */
/*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,       */
/*  either express or implied.  See the License for the specific       */
/*  language governing permissions and limitations under the License.  */
/*                                                                     */
/***********************************************************************/

/* Runtime support for allocation profiling. */

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <limits.h>
#include <math.h>
#include <sys/resource.h>

#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/gc.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/roots.h"
#include "caml/signals.h"
#include "alloc_profiling.h"
#include "stack.h"

#include "../config/s.h"

#define HAS_LIBUNWIND

#ifdef HAS_LIBUNWIND
#include "libunwind.h"
#endif

#pragma GCC optimize ("-O0")

int caml_allocation_profiling = 1;

/* All .cmxs files loaded using natdynlink.  This will be available on any
   platform supporting natdynlink, which might not be the case for the
   memory map information (used to resolve program counters in shared
   libraries that were not compiled by ocamlopt). */
static struct ext_table ocaml_dynamic_libraries;
typedef struct {
  const char* filename;
  void* address_of_code_begin;
} ocaml_dynamic_library;

void caml_allocation_profiling_register_dynamic_library(
  const char* filename, void* address_of_code_begin)
{
  ocaml_dynamic_library* lib;

  lib = caml_stat_alloc(sizeof(ocaml_dynamic_library));
  lib->filename = filename;
  lib->address_of_code_begin = address_of_code_begin;

  caml_ext_table_add(&ocaml_dynamic_libraries, lib);
}

/* The following structures must match the type definitions in the
   [Allocation_profiling] module. */

typedef struct {
  /* (GC header here.) */
  value minor_words;
  value promoted_words;
  value major_words;
  value minor_collections;
  value major_collections;
  value heap_words;
  value heap_chunks;
  value compactions;
  value top_heap_words;
} gc_stats;

typedef struct {
  value profinfo;
  value num_blocks;
  value num_words_including_headers;
} snapshot_entry;

typedef struct {
  /* (GC header here.) */
  snapshot_entry entries[0];
} snapshot_entries;

typedef struct {
  /* (GC header here.) */
  value gc_stats;
  value entries;
} snapshot;

static const uintnat profinfo_none = (uintnat) 0;
static const uintnat profinfo_overflow = (uintnat) 1;
static const uintnat profinfo_lowest = (uintnat) 2;
uintnat caml_allocation_profiling_profinfo = (uintnat) 2;

static value take_gc_stats(void)
{
  gc_stats* stats;
  mlsize_t size;

  stats = caml_stat_alloc(sizeof(header_t) + sizeof(gc_stats));

  Hd_val((value) stats) =
    Make_header(sizeof(gc_stats) / sizeof(value), 0, Caml_black);

  stats->minor_words = (uintnat) caml_stat_minor_words;
  stats->promoted_words = (uintnat) caml_stat_promoted_words;
  stats->major_words =
    ((uintnat) caml_stat_major_words) + ((uintnat) caml_allocated_words);
  stats->minor_collections = (uintnat) caml_stat_minor_collections;
  stats->major_collections = (uintnat) caml_stat_major_collections;
  stats->heap_words = (uintnat) caml_stat_heap_size / sizeof(value);
  stats->heap_chunks = (uintnat) caml_stat_heap_chunks;
  stats->compactions = (uintnat) caml_stat_compactions;
  stats->top_heap_words = (uintnat) caml_stat_top_heap_size / sizeof(value);

  return (value) stats;
}

CAMLprim value caml_allocation_profiling_take_heap_snapshot(void)
{
  snapshot* snapshot;
  snapshot_entry* temp_entries;
  char* chunk;
  value gc_stats;
  value entries;
  uintnat index;
  uintnat target_index;
  uintnat size_in_bytes;
  uintnat largest_profinfo;
  uintnat num_distinct_profinfos = 0;

  temp_entries = (snapshot_entry*) caml_stat_alloc(
    sizeof(header_t) + (PROFINFO_MASK + 1)*sizeof(snapshot_entry));
  for (index = 0; index <= PROFINFO_MASK; index++) {
    temp_entries[index].profinfo = 0;
    temp_entries[index].num_blocks = 0;
    temp_entries[index].num_words_including_headers = 0;
  }

  /* Perform a full major collection so only live data remains and the
     minor heap is empty. */
  caml_minor_collection();
  caml_finish_major_cycle();

  largest_profinfo = profinfo_lowest;

  while (chunk != NULL) {
    char* hp;
    char* limit;

    hp = chunk;
    limit = chunk + Chunk_size (chunk);

    while (hp < limit) {
      header_t hd = Hd_hp (hp);
      switch (Color_hd(hd)) {
        case Caml_blue:
          break;

        default: {
          uint64_t profinfo = Profinfo_hd(hd);

          if (profinfo >= profinfo_lowest && profinfo <= PROFINFO_MASK) {
            if (profinfo > largest_profinfo) {
              largest_profinfo = profinfo;
            }
            if (temp_entries[profinfo].num_blocks == 0) {
              num_distinct_profinfos++;
            }
            temp_entries[profinfo].num_blocks++;
            temp_entries[profinfo].num_words_including_headers +=
              Whsize_hd(hd);
          }
          break;
        }
      }
      hp += Bhsize_hd (hd);
      Assert (hp <= limit);
    }

    chunk = Chunk_next (chunk);
  }

  snapshot = (snapshot*) caml_stat_alloc(sizeof(header_t) + sizeof(snapshot));
  *(Hp_val(snapshot)) =
    Make_header(sizeof(snapshot) / sizeof(value), 0, Caml_black);
  snapshot->gc_stats = gc_stats;
  snapshot->entries = entries;

  return snapshot;
}

CAMLprim value caml_allocation_profiling_free_heap_snapshot(value snapshot)
{
  caml_stat_free(Hp_val(snapshot));
  return Val_unit;
}

CAMLprim value
caml_allocation_profiling_num_frame_descriptors(value unit)
{
  assert(unit == Val_unit);

  if (caml_frame_descriptors == NULL) {
    caml_init_frame_descriptors();
  }

  return Val_long(caml_frame_descriptors_mask + 1);
}

CAMLprim value
caml_allocation_profiling_get_frame_descriptor(value v_index)
{
  uintnat index;
  value v_result;
  frame_descr* descr;

  assert(!Is_block(v_index));
  index = Long_val(v_index);
  if (index > caml_frame_descriptors_mask) {
    caml_failwith("caml_allocation_profiling_get_frametable: bad index");
  }

  if (caml_frame_descriptors == NULL) {
    caml_init_frame_descriptors();
  }
  
  descr = caml_frame_descriptors[index];

  if (descr == NULL) {
    return Val_long(0 /* None */);
  }

  v_result = caml_alloc_small(1, 1 /* Some */);
  Field(v_result, 0) = Val_Descrptr(descr);

  return v_result;
}

CAMLprim value
caml_allocation_profiling_return_address_of_frame_descriptor(value v_descr)
{
  frame_descr* descr;

  descr = Descrptr_val(v_descr);
  assert(descr != NULL);

  return caml_copy_int64(descr->retaddr);
}

CAMLprim value
caml_forget_where_values_were_allocated (value v_unit)
{
  char* chunk;

  assert(v_unit == Val_unit);

  caml_minor_collection();

  chunk = caml_heap_start;

  while (chunk != NULL) {
    char* hp;
    char* limit;

    hp = chunk;
    limit = chunk + Chunk_size (chunk);

    while (hp < limit) {
      header_t hd = Hd_hp (hp);
      Hd_hp (hp) = Make_header (Wosize_hd(hd), Tag_hd(hd), Color_hd(hd));
      hp += Bhsize_hd (hd);
      Assert (hp <= limit);
    }

    chunk = Chunk_next (chunk);
  }

  return v_unit;
}

value caml_alloc_profiling_trie_root = Val_unit;
value* caml_alloc_profiling_trie_node_ptr = &caml_alloc_profiling_trie_root;

value caml_allocation_profiling_use_override_profinfo = Val_false;
uintnat caml_allocation_profiling_override_profinfo;

void caml_allocation_profiling_initialize (void)
{
  caml_ext_table_init(&ocaml_dynamic_libraries, 42);
}

CAMLprim value caml_allocation_profiling_trie_is_initialized (value v_unit)
{
  return (caml_alloc_profiling_trie_root == Val_unit) ? Val_false : Val_true;
}

CAMLprim value caml_allocation_profiling_get_trie_root (value v_unit)
{
  return caml_alloc_profiling_trie_root;
}

CAMLprim value caml_allocation_profiling_do_not_override_profinfo (value v_unit)
{
  v_unit = v_unit;
  caml_allocation_profiling_use_override_profinfo = Val_false;
  return Val_unit;
}

CAMLprim value caml_allocation_profiling_set_override_profinfo (value v_override)
{
  uintnat override = (uintnat) Long_val (v_override);
  if (override == profinfo_none
      || override == profinfo_overflow
      || override > PROFINFO_MASK) {
    return Val_false;
  }
  caml_allocation_profiling_use_override_profinfo = Val_true;
  caml_allocation_profiling_override_profinfo = override;
  return Val_true;
}

CAMLprim value caml_allocation_profiling_min_override_profinfo (value v_unit)
{
  return Val_long(profinfo_lowest);
}

CAMLprim value caml_allocation_profiling_max_override_profinfo (value v_unit)
{
  return Val_long(PROFINFO_MASK);
}

CAMLprim value caml_allocation_profiling_get_profinfo (value v)
{
  return Val_long(Profinfo_val(v));
}

CAMLprim value caml_allocation_profiling_profinfo_none (value v_unit)
{
  return Val_long(profinfo_none);
}

CAMLprim value caml_allocation_profiling_profinfo_overflow (value v_unit)
{
  return Val_long(profinfo_overflow);
}

typedef enum {
  CALL,
  ALLOCATION
} c_node_type;

/* Layout of static nodes:

   OCaml GC header with tag zero
   Tail call words:
   1. PC value at the start of the function corresponding to this node,
      shifted left by 1, with bottom bit then set.
   2. Pointer forming a cyclic list through the nodes involved in any tail
      call chain.
   A sequence of:
   - An allocation point (two words):
     1. PC value, shifted left by 2, with bottom bit then set.  Bit 1 being
        clear enables allocation points to be distinguished from call points.
     2. Profinfo value [that gets written into the value's header]
   - A direct OCaml -> OCaml call point (three words):
     1. Call site PC value, shifted left by 2, with bits 0 and 1 then set
     2. Callee's PC value, shifted left by 2, with bit 0 set
     3. Pointer to callee's node, which will always be a static node.
   - An indirect OCaml -> OCaml call point (two words):
     1. Call site PC value, shifted left by 2, with bits 0 and 1 then set
     2. Pointer to dynamic node.  Note that this dynamic node is really
        part of the static node that points to it.  This pointer not having
        its bottom bit set enables it to be distinguished from the second word
        of a direct call point.  The dynamic node will only contain CALL
        entries, pointing at the callee(s).
   XXX what about indirect OCaml -> C?  Same as indirect OCaml -> OCaml.
   - A direct OCaml -> C call point (three words):
     1. Call site PC value, shifted left by 2, with bits 0 and 1 then set
     2. Callee's PC value, shifted left by 2, with bit 0 set
     3. Pointer to callee's node, which will always be a dynamic node.

   All pointers between nodes point at the word immediately after the
   GC headers, and everything is traversable using the normal OCaml rules.
   Any direct call entries for tail calls must come before any other call
   point or allocation point words.  This is to make them easier to
   initialize.

   Layout of dynamic nodes, which consist of >= 1 part(s) in a linked list:

   OCaml GC header with tag one
   PC value, shifted left by 2, with bottom bit then set.  Bit 1 then
   indicates:
     - bit 1 set => this is a call point
     - bit 1 clear => this is an allocation point
   XXX this next part is wrong for indirect dynamic nodes.  They have
   the callee address.
   The PC is either the PC of an allocation point or a *call site*, never the
     address of a callee.  This means that more conflation between nodes may
     occur than for OCaml parts of the trie.  This can be recovered afterwards
     by checking which function every PC value inside a C node corresponds to,
     and making more trie nodes if required.
   Pointer to callee's node (for a call point), or profinfo value.
   Pointer to the next part of the current node in the linked list, or
     [Val_unit] if this is the last part.

   On entry to an OCaml function:
   If the node hole pointer register has the bottom bit set, then the function
   is being tail called:
   - If the node hole is empty, the callee must create a new node and link
     it into the tail chain.  The node hole pointer will point at the tail
     chain.
   - Otherwise the node should be used as normal.
   Otherwise (not a tail call):
   - If the node hole is empty, the callee must create a new node, but the
     tail chain is untouched.
   - Otherwise the node should be used as normal.
*/

/* Classification of nodes (OCaml or C) with corresponding GC tags. */
#define Is_ocaml_node(node) (Is_block(node) && Tag_val(node) == 0)
#define OCaml_node_tag 0
#define C_node_tag 1

/* The header words are:
   1. The node program counter.
   2. The tail link. */
#define Node_num_header_words 2

/* The "node program counter" at the start of an OCaml node. */
#define Node_pc(node) (Field(node, 0))
#define Encode_node_pc(pc) (((value) pc) | 1)
#define Decode_node_pc(encoded_pc) ((void*) (encoded_pc & ~1))

/* The circular linked list of tail-called functions within OCaml nodes. */
#define Tail_link(node) (Field(node, 1))

/* The convention for pointers from OCaml nodes to other nodes.  There are
   two special cases:
   1. [Val_unit] means "uninitialized", and further, that this is not a
      tail call point.  (Tail call points are pre-initialized, as in case 2.)
   2. If the bottom bit is set, and the value is not [Val_unit], this is a
      tail call point. */
#define Encode_tail_caller_node(node) ((node) | 1)
#define Decode_tail_caller_node(node) ((node) & ~1)
#define Is_tail_caller_node_encoded(node) (((node) & 1) == 1)

/* Classification as to whether an encoded PC value at the start of a group
   of words within a node is either:
   (a) a direct or an indirect call point; or
   (b) an allocation point. */
#define Call_or_allocation_point(node, offset) \
  (((Field(node, offset) & 3) == 1) ? ALLOCATION : CALL)

/* Allocation points within OCaml nodes. */
#define Encode_alloc_point_pc(pc) ((((value) pc) << 2) | 1)
#define Decode_alloc_point_pc(pc) ((void*) (((value) pc) >> 2))
#define Encode_alloc_point_profinfo(profinfo) (Val_long(profinfo))
#define Decode_alloc_point_profinfo(profinfo) (Long_val(profinfo))
#define Alloc_point_pc(node, offset) (Field(node, offset))
#define Alloc_point_profinfo(node, offset) (Field(node, (offset) + 1))

/* Direct call points (tail or non-tail) within OCaml nodes.
   They hold the PC of the call site, the PC upon entry to the callee and
   a pointer to the child node. */
#define Direct_num_fields 3
#define Direct_pc_call_site(node,offset) (Field(node, offset))
#define Direct_pc_callee(node,offset) (Field(node, (offset) + 1))
#define Direct_callee_node(node,offset) (Field(node, (offset) + 2))
/* The following two are used for indirect call points too. */
#define Encode_call_point_pc(pc) ((((value) pc) << 2) | 3)
#define Decode_call_point_pc(pc) ((void*) (((value) pc) >> 2))

/* Indirect call points (tail or non-tail) within OCaml nodes.
   They hold the PC of the call site and a linked list of (PC upon entry
   to the callee, pointer to child node) pairs.  The linked list is encoded
   using C nodes and should be thought of as part of the OCaml node itself. */
#define Indirect_num_fields 2
#define Indirect_pc_call_site(node,offset) (Field(node, offset))
#define Indirect_pc_linked_list(node,offset) (Field(node, (offset) + 1))

/* Encodings of the program counter value within a C node. */
#define Encode_c_node_pc_for_call(pc) ((((value) pc) << 2) | 3)
#define Encode_c_node_pc_for_alloc_point(pc) ((((value) pc) << 2) | 1)
#define Decode_c_node_pc(pc) ((void*) ((pc) >> 2))

typedef struct {
  uintnat gc_header;
  uintnat pc;           /* always has bit 0 set.  Bit 1 set => CALL. */
  union {
    value callee_node;  /* for CALL */
    uintnat profinfo;   /* for ALLOCATION */
  } data;
  value next;           /* [Val_unit] for the end of the list */
} c_node; /* CR mshinwell: rename to dynamic_node */

static c_node_type classify_c_node(c_node* node)
{
  return (node->pc & 2) ? CALL : ALLOCATION;
}

static c_node* c_node_of_stored_pointer(value node_stored)
{
  return (node_stored == Val_unit) ? NULL : (c_node*) Hp_val(node_stored);
}

static c_node* c_node_of_stored_pointer_not_null(value node_stored)
{
  assert(node_stored != Val_unit);
  return (c_node*) Hp_val(node_stored);
}

static value stored_pointer_to_c_node(c_node* node)
{
  assert(node != NULL);
  return Val_hp(node);
}

static int pc_inside_c_node_matches(c_node* node, void* pc)
{
  return Decode_c_node_pc(node->pc) == pc;
}

CAMLprim value caml_allocation_profiling_node_num_header_words(value unit)
{
  unit = unit;
  return Val_long(Node_num_header_words);
}

CAMLprim value caml_allocation_profiling_is_ocaml_node(value node)
{
  return Val_bool(Is_ocaml_node(node));
}

CAMLprim value caml_allocation_profiling_ocaml_function_identifier(value node)
{
  return caml_copy_int64(Decode_node_pc(Node_pc(node)));
}

CAMLprim value caml_allocation_profiling_ocaml_tail_chain(value node)
{
  return Tail_link(node);
}

CAMLprim value caml_allocation_profiling_ocaml_classify_field(value node,
      value offset)
{
  /* Note that [offset] should always point at an initialized call or
     allocation point, by virtue of the behaviour of the function
     [caml_allocation_profiling_ocaml_node_next], below. */

  assert(Is_ocaml_node(node));
  assert(field >= Node_num_header_words);
  assert(field < Wosize_val(node));

  assert(Field(node, field) != Val_unit);

  switch (Call_or_allocation_point(node, field)) {
    case CALL: {
      value callee_node;
      value second_word;
      assert(field < Wosize_val(node) - 1);
      second_word = Indirect_pc_linked_list(node, field);
      assert(second_word != Val_unit);
      if (Is_block(second_word)) {
        return Val_long(4);  /* indirect call point */
      }
      callee_node = Direct_callee_node(node, field);
      if (callee_node == Val_unit) {
        return Val_long(1);  /* direct call point to uninstrumented code */
      } else if (Is_ocaml_node(callee_node)) {
        return Val_long(2);  /* direct call point to OCaml code */
      } else {
        return Val_long(3);  /* direct call point to non-OCaml code */
      }
    }

    case ALLOCATION:
      assert(field < Wosize_val(node) - 1);
      return Val_long(0);
  }

  assert(0);
}

CAMLprim value caml_allocation_profiling_ocaml_node_next(value node,
      value offset)
{
  uintnat field = Long_val(offset);

  assert(Is_ocaml_node(node));
  assert(field >= Node_num_header_words);
  assert(field < Wosize_val(node));

  /* This code follows [print_trie_node], below. */
  for (/* nothing */; field < Wosize_val(node); field++) {
    value entry;

    entry = Field(node, field);

    if (entry == Val_unit) {
      continue;
    }

    if (entry == Encode_tail_caller_node(node)) {
      /* Uninitialized tail call point. */
      assert (field >= Node_num_header_words + 2);
      continue;
    }

    switch (Call_or_allocation_point(node, field)) {
      case CALL: {
        value second_word;
        assert(field < Wosize_val(node) - 1);
        second_word = Indirect_pc_linked_list(node, field);
        assert(second_word != Val_unit);
        if (Is_block(second_word)) {
          /* This is an indirect call point. */
          field++;
        }
        else {
          /* This is a direct call point. */
          assert(field < Wosize_val(node) - 2);
          field += 2;
        }
        break;
      }

      case ALLOCATION:
        assert(field < Wosize_val(node) - 1);
        field++;
        break;

      default:
        assert(0);
    }
  }

  if (field < Wosize_val(node)) {
    return Val_long(field);
  }

  return Val_long(-1);
}

CAMLprim value caml_allocation_profiling_ocaml_allocation_point_program_counter
      (value node, value offset)
{
  return caml_copy_int64(Alloc_point_pc(node, Long_val(offset)));
}

CAMLprim value caml_allocation_profiling_ocaml_allocation_point_annotation
      (value node, value offset)
{
  return caml_copy_int64(Alloc_point_profinfo(node, Long_val(offset)));
}

CAMLprim value caml_allocation_profiling_ocaml_direct_call_point_call_site
      (value node, value offset)
{
  return caml_copy_int64(Direct_pc_call_site(node, Long_val(offset)));
}

CAMLprim value caml_allocation_profiling_ocaml_direct_call_point_callee
      (value node, value offset)
{
  return caml_copy_int64(Direct_pc_callee(node, Long_val(offset)));
}

CAMLprim value caml_allocation_profiling_ocaml_direct_call_point_callee_node
      (value node, value offset)
{
  return caml_copy_int64(Direct_callee_node(node, Long_val(offset)));
}

CAMLprim value caml_allocation_profiling_ocaml_indirect_call_point_call_site
      (value node, value offset)
{
  return caml_copy_int64(Indirect_pc_call_site(node, offset));
}

CAMLprim value caml_allocation_profiling_ocaml_indirect_call_point_callees
      (value node, value offset)
{
  return Indirect_pc_linked_list(node, offset);
}

CAMLprim value caml_allocation_profiling_c_node_is_call(value node)
{
  assert(!Is_ocaml_node(node));
  switch (classify_c_node(c_node_of_stored_pointer_not_null(node))) {
    case CALL: return Val_true;
    case ALLOCATION: return Val_false;
  }
  assert(0);
}

CAMLprim value caml_allocation_profiling_c_node_next(value node)
{
  assert(!Is_ocaml_node(node));
  return c_node_of_stored_pointer_not_null(node)->next;
}

CAMLprim value caml_allocation_profiling_c_node_call_site(value node)
{
  c_node* c_node;
  assert(!Is_ocaml_node(node));
  c_node = c_node_of_stored_pointer_not_null(node);
  return caml_copy_int64(Decode_c_node_pc(c_node->pc));
}

CAMLprim value caml_allocation_profiling_c_node_callee_node(value node)
{
  c_node* c_node;
  assert(!Is_ocaml_node(node));
  c_node = c_node_of_stored_pointer_not_null(node);
  assert(classify_c_node(c_node) == CALL);
  return c_node->data.callee_node;
}

CAMLprim value caml_allocation_profiling_c_node_profinfo(value node)
{
  c_node* c_node;
  assert(!Is_ocaml_node(node));
  c_node = c_node_of_stored_pointer_not_null(node);
  assert(classify_c_node(c_node) == ALLOCATION);
  return caml_copy_int64(Decode_c_node_pc(c_node->data.profinfo));
}

static value allocate_uninitialized_ocaml_node(int size_including_header)
{
  void* node;
  assert(size_including_header >= 3);
  node = caml_stat_alloc(sizeof(uintnat) * size_including_header);
  /* We don't currently rely on [uintnat] alignment, but we do need some
     alignment, so just be sure. */
  assert (((uintnat) node) % sizeof(uintnat) == 0);
  return Val_hp(node);
}

static value find_tail_node(value node, void* callee)
{
  /* Search the tail chain within [node] (which corresponds to an invocation
     of a caller of [callee]) to determine whether it contains a tail node
     corresponding to [callee].  Returns any such node, or [Val_unit] if no
     such node exists. */

  value starting_node;
  value pc;
  value found = Val_unit;

  printf("find_tail_node with callee %p\n", callee);
  starting_node = node;
  pc = Encode_node_pc(callee);

  do {
    assert(Is_ocaml_node(node));
    printf("find_tail_node comparing %p with %p\n",
      (void*) Node_pc(node), (void*) pc);
    if (Node_pc(node) == pc) {
      found = node;
    }
    else {
      node = Tail_link(node);
    }
  } while (found == Val_unit && starting_node != node);

  printf("find_tail_node returns value pointer %p\n", (void*) found);

  return found;
}

CAMLprim value caml_allocation_profiling_allocate_node(
      int size_including_header, void* pc, value* node_hole)
{
  int field;
  value node;
  value caller_node = Val_unit;

  node = *node_hole;
  /* The node hole should either contain [Val_unit], indicating that this
     function was not tail called and we have not been to this point in the
     trie before; or it should contain a value encoded using
     [Encoded_tail_caller_node] that points at the node of a caller
     that tail called the current function. */
  assert(Is_tail_caller_node_encoded(node));

  if (node != Val_unit) {
    value tail_node;
    /* The callee was tail called.  Find whether there already exists a node
       for it in the tail call chain within the caller's node.  The caller's
       node must always be an OCaml node. */
    caller_node = Decode_tail_caller_node(node);
    tail_node = find_tail_node(caller_node, pc);
    if (tail_node != Val_unit) {
      /* This tail calling sequence has happened before; just fill the hole
         with the existing node and return. */
      *node_hole = tail_node;
      return 0;  /* indicates an existing node was returned */
    }
  }

  node = allocate_uninitialized_ocaml_node(size_including_header);
  Hd_val(node) =
    Make_header(size_including_header - 1, OCaml_node_tag, Caml_black);
  Node_pc(node) = Encode_node_pc(pc);
  /* If the callee was tail called, then the tail link field will link this
     new node into an existing tail chain.  Otherwise, it is initialized with
     the empty tail chain, i.e. the one pointing directly at [node]. */
  if (caller_node == Val_unit) {
    Tail_link(node) = node;
  }
  else {
    Tail_link(node) = Tail_link(caller_node);
    Tail_link(caller_node) = node;
  }

  /* The callee node pointers for direct tail call points are
     initialized from code emitted by the OCaml compiler.  This is done to
     avoid having to pass this function a description of which nodes are
     direct tail call points.  (We cannot just count them and put them at the
     beginning of the node because we need the indexes of elements within the
     node during instruction selection before we have found all call points.) */

  for (field = Node_num_header_words; field < size_including_header - 1;
       field++) {
    Field(node, field) = Val_unit;
  }

  *node_hole = node;
  return 1;  /* indicates a new node was created */
}

static c_node* allocate_c_node(void)
{
  c_node* node;

  node = (c_node*) malloc(sizeof(c_node));
  if (!node) {
    abort();
  }

  assert((sizeof(c_node) % sizeof(uintnat)) == 0);
  node->gc_header =
    Make_header(sizeof(c_node) / sizeof(uintnat), C_node_tag, Caml_black);
  node->data.callee_node = Val_unit;
  node->next = Val_unit;

  return node;
}

CAMLprim value* caml_allocation_profiling_indirect_node_hole_ptr
      (void* callee, value* node_hole, value caller_node)
{
  /* Find the address of the node hole for an indirect call to [callee].
     If [node] is not [Val_unit], it is a pointer to the (caller's) node,
     and indicates that this is a tail call site. */

  c_node* c_node;
  int found = 0;

  /* On entry, the node hole pointer is over the call site address slot,
     so we must advance it to reach the linked list slot. */
  node_hole++;

  printf("indirect node hole ptr for callee %p starting at %p contains %p\n",
    callee, (void*) node_hole, *(void**) node_hole);
  while (!found && *node_hole != Val_unit) {
    c_node = c_node_of_stored_pointer(*node_hole);
    assert(c_node != NULL);
    switch (classify_c_node(c_node)) {
      case CALL:
        if (pc_inside_c_node_matches(c_node, callee)) {
          found = 1;
        }
        else {
          node_hole = &c_node->next;
        }
        break;

      case ALLOCATION:
        fprintf(stderr, "Node at %p wrongly marked as ALLOCATION\n", c_node);
        abort();

      default:
        assert(0);
    }
  }

  if (!found) {
    assert(*node_hole == Val_unit);
    c_node = allocate_c_node();
    c_node->pc = Encode_c_node_pc_for_call(callee);

    if (caller_node != Val_unit) {
      /* This is a tail call site.
         Perform the initialization equivalent to that emitted by
         [Alloc_profiling.code_for_function_prologue] for direct tail call
         sites. */
      c_node->data.callee_node = caller_node;
    }

    *node_hole = stored_pointer_to_c_node(c_node);
  }

  assert(*node_hole != Val_unit);
  printf("indirect node hole ptr for callee %p starting at %p is %p\n",
    callee, (void*) node_hole,
    (void*) &(c_node->data.callee_node));
  return &(c_node->data.callee_node);
}

static c_node* find_trie_node_from_libunwind(void)
{
#ifdef HAS_LIBUNWIND
  /* Given that [caml_last_return_address] is the most recent call site in
     OCaml code, and that we are now in C (or other) code called from that
     site, obtain a backtrace using libunwind and graft the most recent
     portion (everything back to but not including [caml_last_return_address])
     onto the trie.  See the important comment below regarding the fact that
     call site, and not callee, addresses are recorded during this process. */

  unw_cursor_t cur;
  unw_context_t ctx;
  int ret;
  int stop;
  int frame;
  struct ext_table frames;
  value* node_hole;
  c_node* node = NULL;

  caml_ext_table_init(&frames, 42);

  unw_getcontext(&ctx);
  unw_init_local(&cur, &ctx);

  stop = 0;
  while (!stop && (ret = unw_step(&cur)) > 0) {
    unw_word_t ip;
    unw_get_reg(&cur, UNW_REG_IP, &ip);
    if (caml_last_return_address == (uintnat) ip) {
      stop = 1;
    }
    else {
      caml_ext_table_add(&frames, (void*) ip);
    }
  }

  node_hole = caml_alloc_profiling_trie_node_ptr;
  printf("*** find_trie_node_from_libunwind: starting at %p\n",
    (void*) *node_hole);
/*  caml_allocation_profiling_debug(Val_unit);*/
  /* Note that if [node_hole] is filled, then it must point to a C node,
     since it is not possible for there to be a call point in an OCaml
     function that sometimes calls C and sometimes calls OCaml. */

  for (frame = frames.size - 1; frame >= 0; frame--) {
    c_node_type expected_type;
    void* pc = frames.contents[frame];
    assert (pc != (void*) caml_last_return_address);

    expected_type = (frame > 0 ? CALL : ALLOCATION);

    if (*node_hole == Val_unit) {
      node = allocate_c_node();
      printf("making new node %p\n", node);
      /* Note: for CALL nodes, the PC is the program counter at each call
         site.  We do not store program counter addresses of the start of
         callees, unlike for OCaml nodes.  This means that some trie nodes
         will become conflated.  These can be split during post-processing by
         working out which function each call site was in. */
      node->pc = (frame > 0 ? Encode_c_node_pc_for_call(pc)
        : Encode_c_node_pc_for_alloc_point(pc));
      *node_hole = stored_pointer_to_c_node(node);
    }
    else {
      c_node* prev;
      int found = 0;

      node = c_node_of_stored_pointer(*node_hole);
      printf("using existing node %p (size %lld)\n", (void*) node,
        (unsigned long long) Wosize_val(*node_hole));
      assert(node != NULL);

      prev = NULL;

      while (!found && node != NULL) {
        printf("...linked list entry pc=%p: ", (void*) node->pc);
        if (classify_c_node(node) == expected_type
            && pc_inside_c_node_matches(node, pc)) {
          printf("found\n");
          found = 1;
        }
        else {
          printf("doesn't match\n");
          prev = node;
          node = c_node_of_stored_pointer(node->next);
        }
      }
      if (!found) {
        assert(prev != NULL);
        node = allocate_c_node();
        node->pc = (frame > 0 ? Encode_c_node_pc_for_call(pc)
          : Encode_c_node_pc_for_alloc_point(pc));
        prev->next = stored_pointer_to_c_node(node);
      }
    }

    assert(node != NULL);

    assert(classify_c_node(node) == expected_type);
    assert(pc_inside_c_node_matches(node, pc));
    node_hole = &node->data.callee_node;

    printf("find_trie_node, frame=%d, ra=%p\n", frame, pc);
  }

  assert(classify_c_node(node) == ALLOCATION);
  assert(c_node_of_stored_pointer(node->next) != node);

  return node;
#else
  return NULL;
#endif
}

static uintnat generate_profinfo(void)
{
  uintnat profinfo;

  if (caml_allocation_profiling_use_override_profinfo == Val_true) {
    return caml_allocation_profiling_override_profinfo;
  }

  profinfo = caml_allocation_profiling_profinfo++;
  if (caml_allocation_profiling_profinfo > PROFINFO_MASK) {
    /* Profiling counter overflow. */
    profinfo = profinfo_overflow;
  }

  return profinfo;
}

CAMLprim uintnat caml_alloc_profiling_generate_profinfo (void* pc,
    void* profinfo_words)
{
  value node;
  uintnat offset;
  uintnat profinfo = generate_profinfo();

  /* [node] isn't really a node; it points into the middle of
     one---specifically to the "profinfo" word of an allocation point pair of
     words  It's done like this to avoid re-calculating the place in the node
     (which already has to be done in the OCaml-generated code run before
     this function). */
  node = (value) (((uintnat*) profinfo_words) - 1);
  offset = 0;

  assert(Alloc_point_pc(node, offset) == Val_unit);
  assert(Alloc_point_profinfo(node, offset) == Val_unit);

  Alloc_point_pc(node, offset) = Encode_alloc_point_pc(pc);
  Alloc_point_profinfo(node, offset) = Encode_alloc_point_profinfo(profinfo);

  return profinfo << PROFINFO_SHIFT;
}

uintnat caml_allocation_profiling_my_profinfo (void)
{
  /* Return the profinfo value that should be written into a value's header
     during an allocation from C.  This may necessitate extending the trie
     with information obtained from libunwind. */

  uint64_t profinfo;

  if (!caml_allocation_profiling) {
    profinfo = 0ull;
  }
  else if (caml_allocation_profiling_use_override_profinfo != Val_false) {
/*
    profinfo = caml_allocation_profiling_override_profinfo;
*/
    c_node* node = find_trie_node_from_libunwind ();
    profinfo = generate_profinfo();
    node->data.profinfo = profinfo;
  }
  else {
    profinfo = 0ull;
  }

/*  caml_allocation_profiling_debug(Val_unit);*/

  return profinfo;
}

extern int caml_extern_allow_out_of_heap;
extern value caml_output_value(value vchan, value v, value flags);

CAMLprim value caml_allocation_profiling_marshal_trie (value v_channel)
{
  /* Marshal the entire trie to an [out_channel].  This can be done by
     using the extern.c code as usual, since the trie looks like standard
     OCaml values; but we must allow it to traverse outside the heap. */

  caml_extern_allow_out_of_heap = 1;
  caml_output_value(v_channel, caml_alloc_profiling_trie_root, Val_long(0));
  caml_extern_allow_out_of_heap = 0;

  return Val_unit;
}

static void print_tail_chain(value node)
{
  value starting_node;

  assert(Is_ocaml_node(node));
  starting_node = node;

  if (Tail_link(node) == node) {
    printf("Tail chain is empty.\n");
  }
  else {
    printf("Tail chain:\n");
    do {
      node = Tail_link(node);
      printf("  Node %p (identifying PC=%p)\n", (void*) node,
        Decode_node_pc(Node_pc(node)));
    } while (node != starting_node);
  }
}

static void print_node_header(value node)
{
  printf("Node %p: tag %d, size %d, identifying PC=%p\n",
    (void*) node, Tag_val(node), (int) Wosize_val(node),
    Decode_node_pc(Node_pc(node)));
  print_tail_chain(node);
}

static void print_trie_node(value node, int inside_indirect_node)
{
  print_node_header(node);

  if (Color_val(node) != Caml_black) {
    printf("Node %p visited before\n", (void*) node);
  }
  else {
    int field;
    int alloc_point;
    int direct_call_point;
    int indirect_call_point;

    alloc_point = 0;
    direct_call_point = 0;
    indirect_call_point = 0;

    Hd_val(node) = Whitehd_hd(Hd_val(node));

    /* CR mshinwell: remove some of the hard-coded offsets below and use the
       macros */

    if (Is_ocaml_node(node)) {
      for (field = Node_num_header_words; field < Wosize_val(node); field++) {
        value entry;

        entry = Field(node, field);

        /* Even though indirect call points have a different size from
           direct call points and allocation points, it is still safe to just
           skip until we don't see [Val_unit] any more. */
        if (entry == Val_unit) {
          continue;
        }

        /* We may now be in the middle of an uninitialized direct call point
           for a tail call.  This can be detected by seeing if the pointer
           is an encoded pointer to the current node. */
        if (entry == Encode_tail_caller_node(node)) {
          /* The pointer should be the third in a group of three words. */
          assert (field >= Node_num_header_words + 2);
          printf("(Reached uninitialized tail call point.)\n");
          continue;
        }

        /* At this point we should have an encoded program counter value.
           First distinguish between:
           (a) a direct or an indirect call point;
           (b) an allocation point.
        */
        switch (Call_or_allocation_point(node, field)) {
          case CALL: {
            /* Determine whether this is a direct or an indirect call
               point by examining the second word in the group.  This will be
               an immediate encoded PC value for a direct call point, but a
               pointer for an indirect call point.  It should never be
               [Val_unit] in either case. */
            value second_word;
            assert(field < Wosize_val(node) - 1);
            /* CR mshinwell: this assumes that the list coincides with
               the callee slot... */
            second_word = Indirect_pc_linked_list(node, field);
            assert(second_word != Val_unit);
            /* CR mshinwell: consider using a macro */
            if (Is_block(second_word)) {
              /* This is an indirect call point. */
              int i = indirect_call_point;
              printf("Indirect call point %d: %p calls...\n",
                indirect_call_point,
                Decode_call_point_pc(Indirect_pc_call_site(node, field)));
              assert(!Is_ocaml_node(second_word));
              print_trie_node(second_word, 1);
              field++;
              printf("Indirect call point %d ends.\n", i);
            }
            else {
              /* This is a direct call point. */
              value child;
              int i = direct_call_point;
              assert(field < Wosize_val(node) - 2);
              child = Direct_callee_node(node, field);
              /* Catch tail call points that have been wrongly initialized. */
              if (child != Val_unit && ((child & 1) == 1)) {
                printf("Direct call point %d (at %p): %p calls %p\n",
                  direct_call_point,
                  (void*) &Direct_pc_call_site(node, field),
                  Decode_call_point_pc(Direct_pc_call_site(node, field)),
                  Decode_call_point_pc(Direct_pc_callee(node, field)));
                printf("This looks like a tail call point (child=%p) that\n",
                  (void*) child);
                printf("  has not been correctly initialized\n");
                assert(0);
              }
              printf("Direct call point %d: %p calls %p, ",
                direct_call_point,
                Decode_call_point_pc(Direct_pc_call_site(node, field)),
                Decode_call_point_pc(Direct_pc_callee(node, field)));
              if (child == Val_unit) {
                printf("callee was not instrumented\n");
              } else {
                printf("child node=%p\n", (void*) child);
              }
              direct_call_point++;
              if (child != Val_unit) {
                print_trie_node(child, 0);
              }
              field += 2;
              printf("Direct call point %d ends\n", i);
            }
            break;
          }

          case ALLOCATION:
            assert(field < Wosize_val(node) - 1);
            printf("Allocation point %d: pc=%p, profinfo=%lld\n", alloc_point,
              Decode_alloc_point_pc(Alloc_point_pc(node, field)),
              (unsigned long long)
                Decode_alloc_point_profinfo(Alloc_point_profinfo(node, field)));
            alloc_point++;
            field++;
            break;

          default:
            assert(0);
        }
      }
    } else {
      c_node* c_node = c_node_of_stored_pointer(node);
      assert (c_node != NULL);
      while (c_node != NULL) {
        printf("(Debug: about to classify node %p)\n", (void*) c_node);
        switch (classify_c_node(c_node)) {
          case CALL:
            printf("%s %p: child node=%p\n",
              inside_indirect_node ? "..." : "Call site in non-OCaml code ",
              (void*) (c_node->pc >> 2),
              (void*) c_node->data.callee_node);
            print_trie_node(c_node->data.callee_node, 0);
            break;

          case ALLOCATION:
            printf("Allocation point in non-OCaml code at %p: profinfo=%lld\n",
              (void*) (c_node->pc >> 2),
              (unsigned long long) c_node->data.profinfo);
            break;

          default:
            abort();
        }
        printf("(Debug: before 'about to classify node %p' with %p)\n",
          (void*) (c_node_of_stored_pointer(c_node->next)),
          (void*) c_node);
        c_node = c_node_of_stored_pointer(c_node->next);
      }
    }
    printf("End of node %p\n", (void*) node);
  }
}

static void mark_trie_node_black(value node)
{
  int field;

  if (Color_val(node) == Caml_black) {
    return;
  }
  Hd_val(node) = Blackhd_hd(Hd_val(node));

  if (Is_ocaml_node(node)) {
    for (field = Node_num_header_words; field < Wosize_val(node); field++) {
      value entry;

      entry = Field(node, field);

      if (entry == Val_unit) {
        continue;
      }

      if (entry == Encode_tail_caller_node(node)) {
        continue;
      }

      switch (Call_or_allocation_point(node, field)) {
        case CALL: {
          value second_word;
          second_word = Indirect_pc_linked_list(node, field);
          if (Is_block(second_word)) {
            assert(!Is_ocaml_node(second_word));
            mark_trie_node_black(second_word);
            field++;
          }
          else {
            value child;
            child = Direct_callee_node(node, field);
            if (child != Val_unit) {
              mark_trie_node_black(child);
            }
            field += 2;
          }
          break;
        }

        case ALLOCATION:
          field++;
          break;

        default:
          assert(0);
      }
    }
  } else {
    c_node* c_node = c_node_of_stored_pointer(node);
    assert (c_node != NULL);
    while (c_node != NULL) {
      switch (classify_c_node(c_node)) {
        case CALL:
          mark_trie_node_black(c_node->data.callee_node);
          break;

        default:
          break;
      }
      c_node = c_node_of_stored_pointer(c_node->next);
    }
  }
}

CAMLprim value caml_allocation_profiling_debug(value v_unit)
{
  value trie_node = caml_alloc_profiling_trie_root;

  if (trie_node == Val_unit) {
    printf("Allocation profiling trie is empty\n");
  }
  else {
    print_trie_node(trie_node, 0);
    mark_trie_node_black(trie_node);
    printf("End of trie dump.\n");
  }

  fflush(stdout);

  return Val_unit;
}
