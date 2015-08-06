/***********************************************************************/
/*                                                                     */
/*                               OCaml                                 */
/*                                                                     */
/*                 Mark Shinwell, Jane Street Europe                   */
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
#include "caml/mlvalues.h"
#include "caml/signals.h"
#include "alloc_profiling.h"
#include "stack.h"

#include "../config/s.h"
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

#if 0

#ifdef HAS_LIBUNWIND
static int
capture_backtrace(backtrace_entry* backtrace, int depth)
{
  /* Capture a full backtrace using libunwind. */

  unw_cursor_t cur;
  unw_context_t ctx;
  int ret;

  unw_getcontext(&ctx);
  unw_init_local(&cur, &ctx);
  /* CR mshinwell: need [unw_step] here I think */
  if ((ret = unw_tdep_trace(&cur, (void**) backtrace, &depth)) < 0) {
    depth = 0;
    unw_getcontext(&ctx);
    unw_init_local(&cur, &ctx);
    while ((ret = unw_step(&cur)) > 0 && depth < MAX_BACKTRACE_DEPTH) {
      unw_word_t ip;
      unw_get_reg(&cur, UNW_REG_IP, &ip);
      backtrace[depth++].return_address = (void*) ip;
    }
  }
  return depth;
}
#else
static int
capture_backtrace(backtrace_entry* backtrace, int depth)
{
  /* In the absence of libunwind, try to just use our return address. */
  if (depth > 0) {
    backtrace[0].return_address = __builtin_return_address(0);
    return backtrace[0].return_address == NULL ? 0 : 1;
  }
  return 0;
}
#endif
#endif


/*

Upon entry to an OCaml function, a distinguished register holds the address of
the node hole.  If we've been here before then the hole will contain a pointer
to a trie node.  Otherwise, it will be NULL, and the callee must allocate a
new trie node.

The distinguished register is treated as containing immediates for the
purposes of GC.

Each OCaml callee will have knowledge, deduced at compile time, of any call
points it contains.

Trie nodes for OCaml functions hold:

- (pc, profinfo) pairs for each allocation point in the function;
- node holes for callees that are called directly in non-tail position;
- node holes for callees that are called directly in tail position (self tail
  calls are excluded);
- node holes for callees that are called indirectly.  Some of these might be
  in tail position.

We could in the future exclude the pc from the allocation point section,
but its presence makes decoding easier.

The indirect callee section is actually (node hole, branch target) pairs
and can be dynamically-extended; it lives at the end of the node.

Identification of a node hole at a call site, to pass to the callee, is
usually easy except in the case of an indirect call.  In this case we use
linear search.

The code sequence in the callee is something like:

- create a new trie node if one doesn't exist and fill the node hole
- change the distinguished register to hold the address of the current node
- immediately prior to any call, change the distinguished register to point
  at the node hole.  We will need to establish a convention as to what happens
  to this register upon return.
- at any allocation point, the code is straightforward (a new profinfo value
  may need to be assigned).

*/

value caml_alloc_profiling_trie_root = (value) 0;
value* caml_alloc_profiling_trie_node_ptr = &caml_alloc_profiling_trie_root;

value caml_allocation_profiling_use_override_profinfo = Val_false;
uintnat caml_allocation_profiling_override_profinfo;

static const uintnat profinfo_none = (uintnat) 0;
static const uintnat profinfo_overflow = (uintnat) 1;
static const uintnat profinfo_lowest = (uintnat) 2;
uintnat caml_allocation_profiling_profinfo = (uintnat) 2;

CAMLprim value caml_allocation_profiling_trie_is_initialized (value v_unit)
{
  return (caml_alloc_profiling_trie_root == (value) 0) ? Val_false : Val_true;
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

CAMLprim uintnat caml_alloc_profiling_generate_profinfo (uintnat pc,
    uintnat* alloc_point_within_node)
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

  Assert (alloc_point_within_node[0] == (uintnat) 0);
  Assert ((pc & 1) == 1);
  alloc_point_within_node[0] = pc;
  Assert (alloc_point_within_node[1] == profinfo_none);
  alloc_point_within_node[1] = profinfo;

  return profinfo << PROFINFO_SHIFT;
}

uintnat caml_allocation_profiling_my_profinfo (void)
{
  uint64_t profinfo;

  if (!caml_allocation_profiling) {
    profinfo = 0ull;
  }
  else if (caml_allocation_profiling_use_override_profinfo != Val_false) {
    profinfo = caml_allocation_profiling_override_profinfo;
  }
  else {
    profinfo = 0ull;
  }

  return profinfo;
}

#if 0

#define BACKTRACE_TABLE_SIZE 100000

/* Maximum depth for a captured backtrace.  This is not the maximum size of
   the backtrace stack. */
/* CR mshinwell: this comment isn't quite accurate any more */
#define MAX_BACKTRACE_DEPTH 16

#define MAX_LIBUNWIND_BACKTRACE_DEPTH 1024


/* Backtrace hash table, shared across all threads.
   As a point of information, libunwind-captured backtraces should never
   coincide with OCaml-captured backtraces, since a libunwind call never
   exists directly inside an OCaml function.
 */
typedef struct {
  void* pc;
  /* [profinfo] may be zero in the case where the counter overflowed.
     The [profinfo] value is already shifted by [PROFINFO_SHIFT], to save
     recomputation.
   */
  uint64_t profinfo;
} allocation_point;
typedef struct backtrace_table_bucket {
  struct backtrace_table_bucket* next;
  /* A word in [return_addrs_and_hashes] may be [END_OF_C_FRAMES].  These
     are used to delimit portions that do not contain any hash values
     (see below). */
  void* return_addrs_and_hashes[MAX_BACKTRACE_DEPTH];  /* most recent first */
  uint64_t num_allocation_points;
  /* [num_allocation_points] number of [allocation_point] structures follow
     here. */
  allocation_point profinfos[1];
} backtrace_table_bucket;
static backtrace_table_bucket* backtrace_table[BACKTRACE_TABLE_SIZE];

/* Backtrace stack layout.  There is one stack per thread; the stacks grow
   downwards in memory.

   Empty stack:
        ------------------------  <-- bottom of stack pointer
        |                      |
        |                      |
        |  zero-initialized    |
        |                      |
        |                      |
        ------------------------
        |  initial hash value  |
        ------------------------  <-- top of stack pointer

   Non-empty stack:
        ------------------------  <-- bottom of stack pointer
        |                      |
        |                      |
        |  zero-initialized    |
        |                      |
        |                      |
        ------------------------
        |  initial hash value  |
        ------------------------
        |  return address Rm   |
        ------------------------
        |  hash                |  (hash of initial value and return address N)
        ------------------------
        |        ...           |
        ------------------------
        |  hash                |
        ------------------------  ]
        |  END_OF_C_FRAMES     |  ]
        ------------------------  ]
        |  return address Cn   |  ]
        ------------------------  ]  captured by libunwind, no intervening
        |        ...           |  ]  hash values.  Corresponds to non-OCaml
        ------------------------  ]  frames on the system stack.
        |  return address C0   |  ]
        ------------------------  ]
        |  START_OF_C_FRAMES   |  ]
        ------------------------  ]
        |  hash                |
        ------------------------
        |  return address R0   |
        ------------------------
        |  hash                |
        ------------------------  <-- top of stack pointer

  Each hash value is the hash of all return addresses prior to it on the
  stack (whether or not from an OCaml frame).

  The hash word is always present even when there are no return addresses.

  The number of zero-initialized words is such that we can always capture a
  fixed-size backtrace without going above the bottom of stack pointer.
*/

/* Top, bottom and limit of the backtrace stack for the current thread. */
backtrace_entry* caml_allocation_profiling_top_of_backtrace_stack;
backtrace_entry* caml_allocation_profiling_bottom_of_backtrace_stack;
backtrace_entry* caml_allocation_profiling_limit_of_backtrace_stack;

/* Initial hash value. */
static uint64_t initial_hash_value = 5381;

#define DEFAULT_STACK_SIZE_IN_BYTES (10240 * 1024)

void
caml_allocation_profiling_create_backtrace_stack(backtrace_entry** top,
    backtrace_entry** bottom, backtrace_entry** limit)
{
  /* Create and initialize a new backtrace stack.  The highest address
     (bottom of stack) is written into [*bottom]; the current stack pointer
     into [*top]; and the limit into [*limit].  If possible, we use the
     system stack size as the default size of the stack. */

  struct rlimit rlim;
  uint64_t size_in_bytes;

  assert(sizeof(backtrace_entry) == sizeof(void*));

  if (getrlimit(RLIMIT_STACK, &rlim) != 0) {
    caml_failwith("Could not read stack rlimit to create backtrace stack");
  }

  size_in_bytes =
    (rlim.rlim_cur != RLIM_INFINITY
      ? rlim.rlim_cur
      : (rlim.rlim_max == RLIM_INFINITY
        ? rlim.rlim_max
        : DEFAULT_STACK_SIZE_IN_BYTES));

  /* XXX we must allocate the trap page */

  *limit = calloc(size_in_bytes, 1);
  if (*bottom == NULL) {
    caml_failwith("Could not allocate backtrace stack");
  }
  *bottom = *limit + (size_in_bytes / sizeof(void*));

  /* Leave empty space (see diagrams above). */
  *top = *bottom - (2 * MAX_BACKTRACE_DEPTH) + 1;

  /* Put initial hash value into place.  The top of stack pointer must end
     up pointing at this value upon return from this function. */
  (*top)--;
  (*top)[0].hash = initial_hash_value;

  /* Ensure we can always capture a fixed-size backtrace. */
  assert ((*bottom) - (*top) == 2 * MAX_BACKTRACE_DEPTH);
}

void
caml_allocation_profiling_initialize(void)
{
  uint64_t bucket;

  for (bucket = 0; bucket < BACKTRACE_TABLE_SIZE; bucket++) {
    backtrace_table[bucket] = NULL;
  }

  caml_allocation_profiling_create_backtrace_stack(
    &caml_allocation_profiling_top_of_backtrace_stack,
    &caml_allocation_profiling_bottom_of_backtrace_stack,
    &caml_allocation_profiling_limit_of_backtrace_stack);
}

static uint64_t
hash(uint64_t previous_hash, void* new_pointer)
{
  /* djb2 hash using XOR. */

  int i;
  for (i = 0; i < sizeof (void*); i++) {
    previous_hash = (previous_hash * 33) ^ (((char*) new_pointer)[i]);
  }
  return previous_hash;
}

static backtrace_table_bucket*
find_or_add_hash_bucket(uint64_t hash_of_all_frames,
                        backtrace_entry* backtrace,
                        /* [backtrace] is 2*MAX_BACKTRACE_DEPTH in size */
                        uint64_t num_allocation_points)
{
  /* CR mshinwell: rename backtrace -> return_addrs_and_hashes? */
  uint64_t bucket_index;
  uint64_t bucket_size;
  backtrace_table_bucket* bucket;
  backtrace_table_bucket** where_to_put_bucket;

  bucket_index = hash_of_all_frames % BACKTRACE_TABLE_SIZE;
  where_to_put_bucket = &backtrace_table[bucket_index];
  bucket = *where_to_put_bucket;

  if (bucket != NULL) {
    /* The bucket has a chain (possibly of length one). */

    where_to_put_bucket = &bucket->next;
    bucket = bucket->next;
    while (bucket != NULL) {
      /* Note that there are always sufficiently many NULL entries above a
         backtrace stack's hash slot that we can always read
         [MAX_BACKTRACE_DEPTH] words starting at one word above the hash
         slot (i.e. the most recent return address). */
      if (!memcmp(bucket->return_addrs_and_hashes, backtrace,
                  2 * MAX_BACKTRACE_DEPTH * sizeof(backtrace_entry))) {
        /* The backtrace matches [bucket]. */
        return bucket;
      }
      bucket = bucket->next;
    }
    /* End of the chain reached; we must allocate a new bucket. */
  }

  /* The backtrace bucket's size varies according to the number of
     allocation points, but currently we keep the depth constant. */
  bucket_size = sizeof(backtrace_table_bucket)
    - sizeof(bucket->profinfos)
    + (sizeof(allocation_point) * num_allocation_points);

  bucket = (backtrace_table_bucket*) malloc(bucket_size);
  if (!bucket) {
    fprintf(stderr, "Allocation profiling backtrace malloc failure");
    abort();
  }

  /* Put the bucket in the table (possibly on the end of a chain). */
  *where_to_put_bucket = bucket;

  bucket->num_allocation_points = num_allocation_points;
  bucket->next = NULL;
  /* Copy the backtrace, which includes the OCaml function's frame (our
     caller) and all previous frames, into the bucket. */
  memcpy(bucket->return_addrs_and_hashes, backtrace,
         2 * MAX_BACKTRACE_DEPTH * sizeof(void*));
  /* We zero-initialize the PC values in the [profinfos] member of the
     bucket so we know which allocation points have been hit. */
  memset(bucket->profinfos, '\0',
    bucket->num_allocation_points * sizeof(allocation_point));

  return bucket;
}

allocation_point*
caml_allocation_profiling_prologue(value num_allocation_points,
                                   backtrace_entry* backtrace_top_of_stack,
                                   void* return_address)
{
  /* This function is called at the top of every OCaml function that might
     allocate.  It finds the bucket (or allocates a new one) for the
     current backtrace---importantly, including the current function---and
     leaves space in that bucket ready for execution of the optimized code
     used at allocation points throughout the current function.
     (See asmcomp/alloc_profiling.ml).

     Upon entry to this function, the backtrace stack looks like this:

        |         ...          |
        ------------------------
        |  hash                |  (hash of all prior return addresses)
        ------------------------
        |  uninitialized       |
        ------------------------
        |  uninitialized       |
        ------------------------  <-- [backtrace_top_of_stack]

     The return address for the current frame is provided in
     [return_address].
  */

  uint64_t hash_of_previous_frames;
  uint64_t hash_of_all_frames;
  backtrace_table_bucket* bucket;

  /* CR-someday mshinwell: A possible test is to call libunwind in this
     function and make sure the backtrace matches the one on the backtrace
     stack. */

  hash_of_previous_frames = backtrace_top_of_stack[2].hash;
  hash_of_all_frames = hash(hash_of_previous_frames, return_address);

  /* If the stack overflows, we will write to the trap page, and fault. */
  backtrace_top_of_stack[1].return_address = return_address;
  backtrace_top_of_stack[0].hash = hash_of_all_frames;

  bucket = find_or_add_hash_bucket(hash_of_all_frames,
    backtrace_top_of_stack, Long_val(num_allocation_points));

  /* (We don't allocate any profinfo values here; they will be done at
     the allocation points throughout the OCaml function.) */

  return &bucket->profinfos[0];
}

void
caml_allocation_profiling_c_to_ocaml(void)
{
  /* This function is called whenever we transfer control from a C function
     to an OCaml function.  The current backtrace is captured using
     libunwind and written, after a marker word, into the backtrace stack,
     We then find out the most recent OCaml frame by looking at
     [caml_last_return_address] and graft the more recent portion of the
     backtrace onto the stack, computing the hash of all frames as we go.
     The hash value is written after a second marker word.

     As a speed optimization, the backtrace corresponding to the C frames is
     not interspersed with hash values; we never unwind into the middle of
     that stack.  The marker words enable the offline tool to work out where
     the libunwind backtrace stops.
  */

  backtrace_entry backtrace[MAX_LIBUNWIND_BACKTRACE_DEPTH];
  int frame;
  int depth;
  uint64_t hash_value;
  int last_return_address_frame = -1;

  caml_allocation_profiling_top_of_backtrace_stack--;
  caml_allocation_profiling_top_of_backtrace_stack[0].marker = END_OF_C_FRAMES;

  depth = capture_backtrace(backtrace, MAX_LIBUNWIND_BACKTRACE_DEPTH);

  hash_value = caml_allocation_profiling_top_of_backtrace_stack[0].hash;

  for (frame = 0; last_return_address_frame == -1 && frame < depth; frame++) {
    if (backtrace[frame].return_address == (void*) caml_last_return_address) {
      last_return_address_frame = frame;
    }
    else {  /* omit the most recent OCaml frame---already hashed. */
      hash_value = hash(hash_value, backtrace[depth].return_address);
    }
  }

  if (last_return_address_frame != -1) {
    depth = last_return_address_frame + 1;
  }
  /* If we couldn't find [caml_last_return_address], we use the whole
     backtrace (up to MAX_LIBUNWIND_BACKTRACE_DEPTH frames, anyway). */

  /* XXX: make sure the trap page is large enough */

  caml_allocation_profiling_top_of_backtrace_stack -= depth;
  memcpy(caml_allocation_profiling_top_of_backtrace_stack, backtrace,
    sizeof(backtrace_entry) * depth);
  caml_allocation_profiling_top_of_backtrace_stack--;
  caml_allocation_profiling_top_of_backtrace_stack[0].marker =
    START_OF_C_FRAMES;
  caml_allocation_profiling_top_of_backtrace_stack--;
  caml_allocation_profiling_top_of_backtrace_stack[0].hash = hash_value;
}

intnat
caml_allocation_profiling_my_profinfo(void)
{
  /* This function is called whenever a C function causes an allocation.
     The current backtrace is captured using libunwind and added to the
     hash table.  The backtrace stack is neither read nor written. */

  uint64_t profinfo;

  if (!caml_allocation_profiling) {
    profinfo = 0ull;
  }
  else if (caml_allocation_profiling_use_override_profinfo != Val_false) {
    profinfo = caml_allocation_profiling_override_profinfo;
  }
  else {
    backtrace_entry backtrace[MAX_BACKTRACE_DEPTH];
    int depth;
    backtrace_table_bucket* bucket;
    uint64_t hash_of_all_frames = initial_hash_value;

    memset((void*) backtrace, '\0',
      MAX_BACKTRACE_DEPTH * sizeof(backtrace_entry));

    depth = capture_backtrace(backtrace, MAX_BACKTRACE_DEPTH);

    for (/* empty */; depth >= 0; depth--) {
      hash_of_all_frames =
        hash(hash_of_all_frames, backtrace[depth].return_address);
    }

    bucket = find_or_add_hash_bucket(hash_of_all_frames, backtrace, 1);

    /* A full libunwind backtrace, such as we have in [backtrace], should
       never be the same as a backtrace from an OCaml function. */
    /* XXX consider making this not an assertion */
    assert (bucket->profinfos[0].pc == NULL);
    assert (bucket->num_allocation_points == 1);

    if (bucket->profinfos[0].profinfo != 0ull) {
      profinfo = bucket->profinfos[0].profinfo;
    }
    else {
      if (caml_allocation_profiling_profinfo == PROFINFO_MASK) {
        /* Counter overflow.  Using zero here is harmless. */
        profinfo = 0ull;
      }
      else {
        profinfo = caml_allocation_profiling_profinfo++;
      }
      bucket->profinfos[0].profinfo = profinfo << PROFINFO_SHIFT;
    }
  }

  return (intnat) profinfo;
}

/* XXX we must add a function to:
 - clear the hash table
 - clear the values' headers
*/

void
caml_allocation_profiling_dump_backtraces_to_file(char* filename)
{
#if 0
  int depth;
  int frame;
  uint64_t bucket_index;
  FILE* fp;

  fp = fopen(filename, "w");
  if (!fp) {
    fprintf(stderr, "Couldn't open %s for backtrace dump\n", filename);
    return;
  }

  for (bucket_index = 0; bucket_index < BACKTRACE_TABLE_SIZE;
       bucket_index++) {
    /* XXX this is out of date */
    backtrace_table_bucket* bucket = &table[bucket_index];
    while (bucket && bucket->profinfo != 0ull) {
      fprintf(stderr, "%llu ", (unsigned long long) bucket->profinfo);
      for (frame = 0; frame < depth; frame++) {
        fprintf(stderr, "%p",
          ((void**) &bucket->first_instr_ptr)[frame]);
        if (frame < (depth - 1)) {
          fprintf(stderr, " ");
        }
        else {
          fprintf(stderr, "\n");
        }
      }
      bucket = bucket->next;
    }
  }

  fclose(fp);
#endif
}

CAMLprim value
caml_allocation_profiling_dump_backtraces_to_file_from_ocaml(value v_filename)
{
  caml_allocation_profiling_dump_backtraces_to_file(String_val(v_filename));
  return Val_unit;
}
#endif
