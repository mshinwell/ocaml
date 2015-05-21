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
#include "caml/gc.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/mlvalues.h"
#include "caml/signals.h"

#include "config/s.h"
#ifdef HAS_LIBUNWIND
#include "libunwind.h"
#endif

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
                    (unsigned long long) size_in_words_including_header)
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
      Hd_hp (hp) = Make_header_with_profinfo (Wosize_hd(hd), Tag_hd(hd), Color_hd(hd), 0);
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

CAMLprim value
caml_do_not_override_profinfo (value v_unit)
{
  v_unit = v_unit;
  caml_override_profinfo = DO_NOT_OVERRIDE_PROFINFO;
  return Val_unit;
}

CAMLprim value
caml_set_override_profinfo (value v_override)
{
  uintnat override = Long_val (v_override);
  if (override == DO_NOT_OVERRIDE_PROFINFO || override > PROFINFO_MASK) {
    caml_invalid_argument ("Allocation profiling info override too large");
  }
  caml_override_profinfo = override;
  return Val_unit;
}

#pragma GCC optimize ("-O3")

static int
capture_backtrace(void** backtrace, int depth)
{
  /* Capture a full backtrace using libunwind. */

  unw_cursor_t cur;
  unw_context_t ctx;
  int ret;

  unw_getcontext(&ctx);
  unw_init_local(&cur, &ctx);
  /* CR mshinwell: need [unw_step] here I think */
  if ((ret = unw_tdep_trace(&cur, addrs, &depth)) < 0) {
    depth = 0;
    unw_getcontext(&ctx);
    unw_init_local(&cur, &ctx);
    while ((ret = unw_step(&cur)) > 0 && depth < MAX_BACKTRACE_DEPTH) {
      unw_word_t ip;
      unw_get_reg(&cur, UNW_REG_IP, &ip);
      backtrace[depth++] = (void*) ip;
    }
  }
  return depth;
}

#define BACKTRACE_TABLE_SIZE 100000

/* Maximum depth for a captured backtrace.  This is not the maximum size of
   the backtrace stack. */
#define MAX_BACKTRACE_DEPTH 16

/* The next profinfo value that will be generated.  Shared across all
   threads.  We do not use zero as a profinfo value; this is reserved to
   mean "none". */
uint64_t caml_allocation_profiling_profinfo = 1ull;

/* Backtrace hash table, shared across all threads.
   As a point of information, libunwind-captured backtraces should never
   coincide with OCaml-captured backtraces, since a libunwind call never
   exists directly inside an OCaml function.
 */
typedef struct {
  void* pc;
  uint64_t profinfo;
} allocation_point;
typedef struct {
  struct backtrace_table_bucket* next;
  void* return_addresses[MAX_BACKTRACE_DEPTH];  /* most recent first */
  uint64_t num_allocation_points;
  /* [num_allocation_points] number of [allocation_point] structures follow
     here. */
  allocation_point profinfos[];
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
        |  return address N    |
        ------------------------
        |        ...           |
        ------------------------
        |  return address 0    |
        ------------------------
        |  hash                |
        ------------------------  <-- top of stack pointer

  The hash value is the hash of the return addresses 0 through N inclusive.
  The hash word is always present even when there are no return addresses.

  The number of zero-initialized words is equal to the maximum length of
  an allocation profiling backtrace minus one.  These words ensure that we
  do not have garbage in the backtraces, which are captured as fixed-size.
*/

void* caml_allocation_profiling_top_of_backtrace_stack;
void* caml_allocation_profiling_bottom_of_backtrace_stack;

/* Limit of backtrace stack for the current thread. */
void* caml_allocation_profiling_limit_of_backtrace_stack;

/* Initial hash value. */
static uint64_t initial_hash_value = 5381;

void
caml_allocation_profiling_initialize(void)
{
  uint64_t bucket;

  for (bucket = 0; bucket < BACKTRACE_TABLE_SIZE; bucket++) {
    backtrace_table[bucket] = NULL;
  }
}

#define DEFAULT_STACK_SIZE_IN_BYTES (10240 * 1024)

void
caml_allocation_profiling_create_backtrace_stack(void** top,
    void** bottom, void** limit)
{
  /* Create and initialize a new backtrace stack.  The highest address
     (bottom of stack) is written into [*bottom]; the current stack pointer
     into [*top]; and the limit into [*limit].  If possible, we use the
     system stack size as the default size of the stack. */

  struct rlimit rlim;
  uint64_t size_in_bytes;

  if (getrlimit(RLIMIT_STACK, &rlim) != 0) {
    caml_failwith("Could not read stack rlimit to create backtrace stack");
  }

  size_in_bytes =
    (rlim.rlim_cur != RLIM_INFINITY
      ? rlim.rlim_cur
      : (rlim.rlim_max == RLIM_INFINITY
        ? rlim.rlim_max
        : DEFAULT_STACK_SIZE_IN_BYTES));

  *limit = calloc(size_in_bytes, 1);
  if (*bottom == NULL) {
    caml_failwith("Could not allocate backtrace stack");
  }
  *bottom = *limit + (size_in_bytes / sizeof(void*));

  /* Leave empty space (see diagrams above). */
  *top = *bottom - MAX_BACKTRACE_DEPTH;

  /* Put initial hash value into place.  The top of stack pointer must end
     up pointing at this value upon return from this function. */
  (*top)--;
  *((uint64_t*) *top) = initial_hash_value;
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
                        void** backtrace,  /* MAX_BACKTRACE_DEPTH in size */
                        uint64_t num_allocation_points)
{
  uint64_t bucket_index;
  uint64_t bucket_size;
  backtrace_table_bucket* bucket;
  backtrace_table_bucket** where_to_put_bucket;

  bucket_index = hash_of_all_frames % BACKTRACE_TABLE_SIZE;
  where_to_put_bucket = &(backtrace_table[bucket_index]);
  bucket = *where_to_put_bucket;

  if (bucket != NULL) {
    /* The bucket has a chain (possibly of length one). */

    where_to_put_bucket = &(bucket->next);
    bucket = bucket->next;
    while (bucket != NULL) {
      /* Note that there are always sufficiently many NULL entries above a
         backtrace stack's hash slot that we can always read
         [MAX_BACKTRACE_DEPTH] words starting at one word above the hash
         slot (i.e. the most recent return address). */
      if (!memcmp(bucket->return_addresses, backtrace,
                  MAX_BACKTRACE_DEPTH * sizeof(void*))) {
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
    - sizeof(backtrace_table_bucket.profinfos)
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
  memcpy(bucket->return_addresses, backtrace,
         MAX_BACKTRACE_DEPTH * sizeof(void*));
  /* We zero-initialize the PC values in the [profinfos] member of the
     bucket so we know which allocation points have been hit. */
  memset(bucket->profinfos, '\0',
    bucket->num_allocation_points * sizeof(allocation_point));

  return bucket;
}

void*
caml_allocation_profiling_prologue(value num_allocation_points,
                                   void* return_address)
{
  /* This function is called at the top of every OCaml function that might
     allocate.  It finds the bucket (or allocates a new one) for the
     current backtrace---importantly, including the current function---and
     leaves space in that bucket ready for execution of the optimized code
     used at allocation points throughout the current function.
     (See asmcomp/alloc_profiling.ml). */

  uint64_t hash_of_previous_frames;
  uint64_t hash_of_all_frames;
  uint64_t depth;

  hash_of_previous_frames =
    *(uint64_t *) caml_allocation_profiling_limit_of_backtrace_stack;

  /* If the stack would overflow, for the moment, we just fail. */
  if (caml_allocation_profiling_top_of_backtrace_stack
      <= caml_allocation_profiling_limit_of_backtrace_stack) {
    fprintf(stderr, "Allocation profiling backtrace stack overflow\n");
    abort();
  }

  hash_of_all_frames = hash(hash_of_previous_frames, return_address);

  caml_allocation_profiling_top_of_backtrace_stack[0] = hash_of_all_frames;
  caml_allocation_profiling_top_of_backtrace_stack[1] = return_address;

  bucket = find_or_add_hash_bucket(hash_of_all_frames,
    caml_allocation_profiling_top_of_backtrace_stack + 1,
    Long_val(num_allocation_points));

  /* (We don't allocate any profinfo values here; they will be done at
     the allocation points throughout the OCaml function.) */

  return bucket;
}

/* XXX maybe the constant depth thing doesn't work: on the stack it cannot
   be so, or we don't know how much to unwind it. */

void
caml_allocation_profiling_c_to_ocaml(void)
{
  /* This function is called whenever we transfer control from a C function
     to an OCaml function.  The current backtrace is captured using
     libunwind and written into the backtrace stack. */

  /* XXX this assumes the backtrace stack is always big enough */

  caml_allocation_profiling_top_of_backtrace_stack
    = caml_allocation_profiling_bottom_of_backtrace_stack
      - M

  capture_backtrace(caml_al, int depth)

}

/* XXX what happens when an exception is raised?  The trap frame probably
   needs to store the backtrace stack pointers to restore. */

/* XXX the return from caml_callback* needs thinking about.
   Also, upon C -> OCaml transition, it isn't clear how we know how to
   restore things (e.g. there might be another C call). */

intnat
caml_allocation_profiling_my_profinfo(void)
{
  /* This function is called whenever a C function causes an allocation.
     The current backtrace is captured using libunwind and added to the
     hash table.  The backtrace stack is untouched. */

#ifndef ARCH_SIXTYFOUR
  return (intnat) 0;  /* No room for profinfo in the header on 32 bit. */
#else
  uint64_t profinfo;

  if (!caml_allocation_profiling && !caml_lifetime_tracking) {
    profinfo = 0ull;
  }
  else if (caml_override_profinfo != DO_NOT_OVERRIDE_PROFINFO) {
    profinfo = caml_override_profinfo;
  }
  else {
    void* backtrace[MAX_BACKTRACE_DEPTH];
    int depth;
    uint64_t hash_of_all_frames = initial_hash_value;

    memset((void*) backtrace, '\0', MAX_BACKTRACE_DEPTH * sizeof(void*));

#ifndef HAS_LIBUNWIND
    /* In the absence of libunwind, just use our return address. */
    backtrace[0] = __builtin_return_address(0);
#else
    depth = capture_backtrace(backtrace, MAX_BACKTRACE_DEPTH);
#endif

    for (/* empty */; depth >= 0; depth--) {
      hash_of_all_frames = hash(hash_of_all_frames, backtrace[depth]);
    }

    bucket = find_or_add_hash_bucket(hash_of_all_frames, backtrace, 2);

    /* A full libunwind backtrace, such as we have in [backtrace], should
       never be the same as a backtrace from an OCaml function. */
    /* XXX consider making this not an assertion */
    assert (bucket->profinfos[0].pc == NULL);
    assert (bucket->num_allocation_points == 1);

    if (bucket->profinfos[0] != 0ull) {
      profinfo = bucket->profinfos[0];
    }
    /* XXX is profinfo shifted in the table? */
    profinfo = caml_allocation_profinfo++;
    bucket->profinfos[0].profinfo = profinfo;
  }

  return (intnat) ((profinfo & PROFINFO_MASK) << PROFINFO_SHIFT);
#endif
}

void
caml_allocation_profiling_dump_backtraces_to_file(char* filename)
{
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
}

CAMLprim value
caml_allocation_profiling_dump_backtraces_to_file_from_ocaml(value v_filename)
{
  caml_allocation_profiling_dump_backtraces_to_file_from_ocaml(
    String_val(v_filename));
  return Val_unit;
}
