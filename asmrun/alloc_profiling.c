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
#include "caml/signals.h"
#include "alloc_profiling.h"
#include "stack.h"

#include "../config/s.h"

#define HAS_LIBUNWIND

#ifdef HAS_LIBUNWIND
#include "libunwind.h"
#endif

int caml_allocation_profiling = 1;

void
caml_dump_allocators_of_major_heap_blocks (const char* output_file,
                                           int sample_strings)
{
  char* chunk;
  FILE* fp;
  uint64_t blue;
  uint64_t accounted_for;
  uint64_t unaccounted_for = 0ull;
  uint64_t unaccounted_for_by_tag[256];
  int tag;

  blue = 0ull;
  accounted_for = 0ull;

  /* XXX make this traverse from roots, and not do a GC. */

  for (tag = 0; tag < 256; tag++) {
    unaccounted_for_by_tag[tag] = 0ull;
  }

  fp = fopen(output_file, "w");
  if (fp == NULL) {
    fprintf(stderr, "couldn't open file '%s' for heap block dump\n", output_file);
    return;
  }

  /* To avoid having to traverse the minor heap, just empty it. */
  caml_minor_collection();

  /* Perform a full major collection so no white blocks remain. */
  caml_finish_major_cycle();

  chunk = caml_heap_start;

  while (chunk != NULL) {
    char* hp;
    char* limit;

    hp = chunk;
    limit = chunk + Chunk_size (chunk);

    while (hp < limit) {
      header_t hd = Hd_hp (hp);
      switch (Color_hd(hd)) {
        case Caml_blue:
          blue += Whsize_hd(hd);
          break;

        default: {
          uint64_t profinfo = Profinfo_hd(hd);

          if (profinfo != 0ull) {
            uint64_t size_in_words_including_header;

            size_in_words_including_header = Whsize_hd(hd);
            accounted_for += Whsize_hd(hd);

            fprintf(fp, "%p %lld\n", (void*) profinfo,
                    (unsigned long long) size_in_words_including_header);
          }
          else {
            unaccounted_for += Whsize_hd(hd);
            unaccounted_for_by_tag[Tag_hd(hd)]++;
            if (sample_strings > 0) {
              fprintf(fp, "tag %d with no profiling info: %p (field0: %p)\n",
                Tag_hd(hd), (void*) (Op_hp(hp)), (void*) *(Op_hp(hp)));
              sample_strings--;
            }
          }
          break;
        }
      }
      hp += Bhsize_hd (hd);
      Assert (hp <= limit);
    }

    chunk = Chunk_next (chunk);
  }

  fprintf(fp, "word size (incl headers) of non-blue blocks with profiling info: %lld\n", (unsigned long long) accounted_for);
  fprintf(fp, "word size (incl headers) of non-blue blocks with no profiling info: %lld\n  by tag: ", (unsigned long long) unaccounted_for);
  for (tag = 0; tag < 256; tag++) {
    if (unaccounted_for_by_tag[tag] > 0) {
      fprintf(fp, "tag(%d)=%lld ", tag, (unsigned long long) unaccounted_for_by_tag[tag]);
    }
  }
  fprintf(fp, "\n");
  fprintf(fp, "word size (incl headers) of blue blocks: %lld\n", (unsigned long long) blue);
  fprintf(fp, "word size (incl headers) of all blocks: %lld\n", (unsigned long long) (blue + accounted_for + unaccounted_for));
  fprintf(fp, "caml_stat_heap_size in words: %lld\n", (unsigned long long) caml_stat_heap_size / sizeof(value));

  fclose(fp);
}

CAMLprim value
caml_dump_allocators_of_major_heap_blocks_from_ocaml (value output_file,
                                                      value sample_strings)
{
  assert(Is_block(output_file) && Tag_val(output_file) == String_tag);
  caml_dump_allocators_of_major_heap_blocks(String_val(output_file),
    Int_val(sample_strings));
  return Val_unit;
}

/* XXX consider having this one not do a GC too */
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
      Hd_hp (hp) =
        Make_header_with_profinfo (Wosize_hd(hd), Tag_hd(hd), Color_hd(hd), 0);
      hp += Bhsize_hd (hd);
      Assert (hp <= limit);
    }

    chunk = Chunk_next (chunk);
  }

  return v_unit;
}

void
caml_dump_heapgraph(const char* node_output_file, const char* edge_output_file)
{
  char* chunk;
  FILE* node_fp;
  FILE* edge_fp;

  node_fp = fopen(node_output_file, "w");
  if (node_fp == NULL) {
    fprintf(stderr, "couldn't open file '%s' for heap graph dump\n", node_output_file);
    return;
  }

  edge_fp = fopen(edge_output_file, "w");
  if (edge_fp == NULL) {
    fprintf(stderr, "couldn't open file '%s' for heap graph dump\n", edge_output_file);
    return;
  }

  caml_minor_collection();
  caml_finish_major_cycle();

  chunk = caml_heap_start;

  while (chunk != NULL) {
    char* hp;
    char* limit;

    hp = chunk;
    limit = chunk + Chunk_size (chunk);

    while (hp < limit) {
      header_t hd_parent = Hd_hp (hp);
      switch (Color_hd(hd_parent)) {
        case Caml_blue:
          break;

        default: {
          uint64_t approx_instr_pointer_parent;

/* All wrong now, needs fixing */
          approx_instr_pointer_parent = Decode_profinfo_hd (hd_parent);

          if (approx_instr_pointer_parent != 0ull) {
            if (Tag_hd(hd_parent) < No_scan_tag) {
              mlsize_t field;
              value parent;

              parent = Val_hp(hp);
              assert(Is_block(parent));

              for (field = 0; field < Wosize_val(parent); field++) {
                value child;
                child = Field(parent, field);

                if (Is_block(child) && Is_in_value_area(child)) {
                  uint64_t approx_instr_pointer_child;
                  header_t hd_child;

                  hd_child = Hd_val(child);
                  approx_instr_pointer_child = Decode_profinfo_hd (hd_child);

                  if (approx_instr_pointer_child != 0ull) {
                    fprintf(edge_fp, "B %p,%d,%lld\n",
                      (void*) approx_instr_pointer_parent,
                      Tag_hd(hd_parent),
                      (unsigned long long) Wosize_hd(hd_parent));
                    fprintf(edge_fp, "B %p,%d,%lld\n",
                      (void*) approx_instr_pointer_child,
                      Tag_val(hd_child),
                      (unsigned long long) Wosize_hd(hd_child));
                    fprintf(edge_fp, "E %p,%p\n",
                            (void*) approx_instr_pointer_parent,
                            (void*) approx_instr_pointer_child);
                  }
                }
              }
            }
          }
          break;
        }
      }
      hp += Bhsize_hd (hd_parent);
      Assert (hp <= limit);
    }

    chunk = Chunk_next (chunk);
  }

  fclose(node_fp);
  fclose(edge_fp);
}

CAMLprim value
caml_dump_heapgraph_from_ocaml(value node_output_file, value edge_output_file)
{
  assert(Is_block(node_output_file) && Tag_val(node_output_file) == String_tag);
  assert(Is_block(edge_output_file) && Tag_val(edge_output_file) == String_tag);

  caml_dump_heapgraph(String_val(node_output_file), String_val(edge_output_file));

  return Val_unit;
}

#pragma GCC optimize ("-O3")

value caml_alloc_profiling_trie_root = Val_unit;
value* caml_alloc_profiling_trie_node_ptr = &caml_alloc_profiling_trie_root;

value caml_allocation_profiling_use_override_profinfo = Val_false;
uintnat caml_allocation_profiling_override_profinfo;

static const uintnat profinfo_none = (uintnat) 0;
static const uintnat profinfo_overflow = (uintnat) 1;
static const uintnat profinfo_lowest = (uintnat) 2;
uintnat caml_allocation_profiling_profinfo = (uintnat) 2;

void caml_allocation_profiling_initialize (void)
{

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
     2. Profinfo value
   - A direct OCaml -> OCaml call point (three words):
     1. Call site PC value, shifted left by 2, with bits 0 and 1 then set
     2. Callee's PC value, shifted left by 2, with bit 0 set
     3. Pointer to callee's node, which will always be a static node.
   - An indirect OCaml -> OCaml call point (two words):
     1. Call site PC value, shifted left by 2, with bits 0 and 1 then set
     2. Pointer to dynamic node.  Note that this dynamic node is really
        part of the static node that points to it.  This pointer not having
        its bottom bit set enables it to be distinguished from the other
        cases.  The dynamic node will only contain CALL entries, pointing
        at the callee(s).
   - A direct OCaml -> C call point (three words):
     1. Call site PC value, shifted left by 2, with bits 0 and 1 then set
     2. Callee's PC value, shifted left by 2, with bit 0 set
     3. Pointer to callee's node, which will always be a dynamic node.

   All pointers between nodes point at the word immediately after the
   GC headers.
   Any direct call entries for tail calls must come before any other call
   point or allocation point words.  This is to make them easier to
   initialize.

   Layout of dynamic nodes, which consist of >= 1 part(s) in a linked list:

   OCaml GC header with tag one
   PC value, shifted left by 2, with bottom bit then set.  Bit 1 then
   indicates:
     - bit 1 set => this is a call point
     - bit 1 clear => this is an allocation point
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

/* Allocation points within OCaml nodes. */
#define Encode_alloc_point_pc(pc) ((((value) pc) << 2) | 1)
#define Decode_alloc_point_pc(pc) (((value) pc) >> 2)
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
#define Decode_call_point_pc(pc) (((value) pc) >> 2)

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

static value allocate_uninitialized_ocaml_node(int size_including_header)
{
  void* node;
  assert(size_including_header >= 3);
  node = caml_stat_alloc(sizeof(uintnat) * size_including_header);
  return Val_hp(node);
}

static value* find_tail_node(value node, void* callee)
{
  /* Search the tail chain within [node] (which corresponds to an invocation
     of a caller of [callee]) to determine whether it contains a tail node
     corresponding to [callee].  Returns any such node, or [Val_unit] if no
     such node exists. */

  value starting_node;
  value pc;
  value found = Val_unit;

  starting_node = node;
  pc = Encode_node_pc(callee);

  do {
    assert(Is_ocaml_node(node));
    if (Node_pc(node) == pc) {
      found = 1;
    }
    else {
      node = Tail_link(node);
    }
  } while (found == Val_unit && starting_node != node);

  return found;
}

CAMLprim value caml_allocation_profiling_allocate_node (
      int size_including_header, void* pc, value* node_hole)
{
  int word;
  int direct_tail_call_point;
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
    /* The calling function was tail called.  Find whether there already
       exists a node for it in the tail call chain within the caller's
       node.  The caller's node must always be an OCaml node. */
    caller_node = Decode_tail_caller_node(node);
    tail_node = find_tail_node(caller_node, pc);
    if (tail_node != Val_unit) {
      /* This tail calling sequence has happened before; just fill the hole
         with the existing node and return. */
      *node_hole = tail_node;
      return;
    }
  }

  node = allocate_uninitialized_ocaml_node(size_including_header);
  Hd_val(node) =
    Make_header(size_including_header - 1, OCaml_node_tag, Caml_black);
  Node_pc(node) = Encode_node_pc(callee);
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
  return node;
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
    value node, uintnat offset)
{
  uintnat profinfo = generate_profinfo();

  assert(Alloc_point_pc(node, offset) == Val_unit);
  assert(Alloc_point_profinfo(node, offset) == Val_unit);

  Alloc_point_pc(node, offset) = Encode_alloc_point_pc(pc);
  Alloc_point_profinfo(node, offset) = Encode_alloc_point_profinfo(profinfo);

  return profinfo << PROFINFO_SHIFT;
}

typedef struct {
  uintnat gc_header;
  uintnat pc;           /* always has bit 0 set.  Bit 1 set => CALL. */
  union {
    value callee_node;  /* for CALL */
    uintnat profinfo;   /* for ALLOCATION */
  } data;
  value next;           /* [Val_unit] for the end of the list */
} c_node; /* CR mshinwell: rename to dynamic_node */

typedef enum {
  CALL,
  ALLOCATION
} c_node_type;

static c_node_type classify_c_node(c_node* node)
{
  return (node->pc & 2) ? CALL : ALLOCATION;
}

static c_node* c_node_of_stored_pointer(value node_stored)
{
  return (node_stored == Val_unit) ? NULL : (c_node*) Hp_val(node_stored);
}

static value stored_pointer_to_c_node(c_node* node)
{
  assert(node != NULL);
  return Val_hp(node);
}

static void print_node_header(value node)
{

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

static void print_trie_node(value node)
{
  if (Color_val(node) != Caml_black) {
    printf("Node %p visited before\n", (void*) node);
  }
  else {
    int field;
    int alloc_point;
    int direct_call_point;

    alloc_point = 0;
    direct_call_point = 0;

    Hd_val(node) = Whitehd_hd(Hd_val(node));

    printf("Node %p (%s) (size %d):\n", (void*) node,
      Tag_val(node) == 0 ? "OCaml node" : "C node", (int) Wosize_val(node));
    if (Tag_val(node) == 0) {
      for (field = 0; field < Wosize_val(node); field++) {
        value entry;
        int is_last;

        entry = Field(node, field);

        if (entry == Val_unit) {
          field++;
          continue;
        }

        is_last = (field == Wosize_val(node) - 1);

        switch (entry & 3) {
          case 1:
            if (is_last) {
              printf("Node is too short\n");
            }
            else {
              value pc = (entry & ~3) >> 2;
              printf("Allocation point %d: pc=%p, profinfo=%lld\n", alloc_point,
                (void*) pc,
                (unsigned long long) Field(node, field + 1));
              alloc_point++;
              field++;
            }
            break;

          case 3:
            if (is_last) {
              printf("Node is too short\n");
            }
            else {
              value pc = (entry & ~3) >> 2;
              value child = Field(node, field + 1);
              value is_self_tail = (child == node);
              printf("Direct call point %d: pc=%p, child node=%p%s\n",
                direct_call_point,
                (void*) pc,
                (void*) child,
                (is_self_tail ? " (self tail call)" : ""));
              direct_call_point++;
              if (child != Val_unit) {
                print_trie_node(child);
              }
              field++;
            }
            break;

          default:
            printf("Field %d = %p is malformed\n", field, (void*) entry);
            break;
        }
      }
    } else {
      c_node* c_node = c_node_of_stored_pointer(node);
      assert (c_node != NULL);
      while (c_node != NULL) {
        printf("(Debug: about to classify node %p)\n", (void*) c_node);
        switch (classify_c_node(c_node)) {
          case CALL:
            printf("Call point at %p: child node=%p\n",
              (void*) (c_node->pc >> 2),
              (void*) c_node->data.callee_node);
            print_trie_node(c_node->data.callee_node);
            break;

          case ALLOCATION:
            printf("Allocation point at %p: profinfo=%lld\n",
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

  if (Tag_val(node) == 0) {
    for (field = 0; field < Wosize_val(node); field += 2) {
      value entry;

      entry = Field(node, field);

      if (entry == Val_unit) {
        continue;
      }

      switch (entry & 3) {
        case 3: {
          value child = Field(node, field + 1);
          if (child != Val_unit) {
            mark_trie_node_black(child);
          }
          break;
        }

        default:
          break;
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
    print_trie_node(trie_node);
    mark_trie_node_black(trie_node);
    printf("End of trie dump.\n");
  }

  fflush(stdout);

  return Val_unit;
}

static int pc_inside_c_node_matches(c_node* node, void* pc)
{
  return Decode_c_node_pc(node->pc) == pc;
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
      (void* callee, value* node_hole, int is_tail)
{
  /* Find the address of the node hole for an indirect call to [callee]. */

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
  caml_allocation_profiling_debug(Val_unit);
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
}

uintnat caml_allocation_profiling_my_profinfo (void)
{
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

  caml_allocation_profiling_debug(Val_unit);

  return profinfo;
}

extern int caml_extern_allow_out_of_heap;
extern value caml_output_value(value vchan, value v, value flags);

CAMLprim value caml_allocation_profiling_marshal_trie (value v_channel)
{
  caml_extern_allow_out_of_heap = 1;
  caml_output_value(v_channel, caml_alloc_profiling_trie_root, Val_long(0));
  caml_extern_allow_out_of_heap = 0;

  return Val_unit;
}
