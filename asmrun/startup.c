/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Start-up code */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include "caml/callback.h"
#include "caml/backtrace.h"
#include "caml/custom.h"
#include "caml/debugger.h"
#include "caml/fail.h"
#include "caml/freelist.h"
#include "caml/gc.h"
#include "caml/gc_ctrl.h"
#include "caml/intext.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/osdeps.h"
#include "caml/printexc.h"
#include "stack.h"
#include "caml/startup_aux.h"
#include "caml/sys.h"
#ifdef HAS_UI
#include "caml/ui.h"
#endif

extern int caml_parser_trace;
CAMLexport header_t caml_atom_table[256];
char * caml_code_area_start, * caml_code_area_end;
uintnat caml_lifetime_tracking = 0;
/* Corresponds to measuring block lifetimes in units of 8Mb.  With the 22 bits of
   allocation profiling information this should enable us to get a bit over 10^13 bytes
   as the maximum lifetime. */
/* was 22 = 8Mb
   was 19 = 1Mb
   now 16 = 128k
*/
uintnat caml_lifetime_shift = 16;
uintnat caml_allocation_profiling = 1;

/* Initialize the atom table and the static data and code area limits. */

struct segment { char * begin; char * end; };

static void init_static(void)
{
  extern struct segment caml_data_segments[], caml_code_segments[];
  int i;
  struct code_fragment * cf;

  caml_init_atom_table ();

  for (i = 0; caml_data_segments[i].begin != 0; i++) {
    /* PR#5509: we must include the zero word at end of data segment,
       because pointers equal to caml_data_segments[i].end are static data. */
    if (caml_page_table_add(In_static_data,
                            caml_data_segments[i].begin,
                            caml_data_segments[i].end + sizeof(value)) != 0)
      caml_fatal_error("Fatal error: not enough memory for initial page table");
  }

  caml_code_area_start = caml_code_segments[0].begin;
  caml_code_area_end = caml_code_segments[0].end;
  for (i = 1; caml_code_segments[i].begin != 0; i++) {
    if (caml_code_segments[i].begin < caml_code_area_start)
      caml_code_area_start = caml_code_segments[i].begin;
    if (caml_code_segments[i].end > caml_code_area_end)
      caml_code_area_end = caml_code_segments[i].end;
  }
  /* Register the code in the table of code fragments */
  cf = caml_stat_alloc(sizeof(struct code_fragment));
  cf->code_start = caml_code_area_start;
  cf->code_end = caml_code_area_end;
  cf->digest_computed = 0;
  caml_ext_table_init(&caml_code_fragments_table, 8);
  caml_ext_table_add(&caml_code_fragments_table, cf);
}

/* These are termination hooks used by the systhreads library */
struct longjmp_buffer caml_termination_jmpbuf;
void (*caml_termination_hook)(void *) = NULL;

extern value caml_start_program (void);
extern void caml_init_ieee_floats (void);
extern void caml_init_signals (void);

#ifdef _MSC_VER

/* PR 4887: avoid crash box of windows runtime on some system calls */
extern void caml_install_invalid_parameter_handler();
#endif
/*
size_t bytes_sufficient_for_code_section = 0;


static void
record_data_segment_limit(void)
{
  void* limit = sbrk(0);
  if (limit != (void*) -1) {
    bytes_sufficient_for_code_section = (uint64_t) limit;
  }
}
*/

uint64_t* caml_minor_allocation_profiling_array = NULL;
uint64_t* caml_minor_allocation_profiling_array_end = NULL;
uint64_t* caml_major_allocation_profiling_array = NULL;
uint64_t* caml_major_allocation_profiling_array_end = NULL;
void* caml_allocation_trace_caller = NULL;
/*
void (*__malloc_initialize_hook)(void) = record_data_segment_limit;

static void
initialize_allocation_profiling (void)
{
  if (caml_allocation_profiling && bytes_sufficient_for_code_section > 0) {
    caml_minor_allocation_profiling_array =
      (uint64_t*) calloc(bytes_sufficient_for_code_section, 1);
    if (!caml_minor_allocation_profiling_array) abort();
    caml_minor_allocation_profiling_array_end =
      caml_minor_allocation_profiling_array +
      (bytes_sufficient_for_code_section / sizeof(uint64_t));

    caml_major_allocation_profiling_array =
      (uint64_t*) calloc(bytes_sufficient_for_code_section, 1);
    if (!caml_major_allocation_profiling_array) abort();
    caml_major_allocation_profiling_array_end =
      caml_major_allocation_profiling_array +
      (bytes_sufficient_for_code_section / sizeof(uint64_t));
  }
  else {
    caml_allocation_profiling = 0;
  }
}*/


extern int ensure_alloc_profiling_dot_o_is_included;

void caml_main(char **argv)
{
  char * exe_name;
  static char proc_self_exe[256];
  value res;
  char tos;

  caml_init_frame_descriptors();
  ensure_alloc_profiling_dot_o_is_included++;
  caml_init_ieee_floats();
#ifdef _MSC_VER
  caml_install_invalid_parameter_handler();
#endif
  caml_init_custom_operations();
  caml_top_of_stack = &tos;
#ifdef DEBUG
  caml_verb_gc = 0x3F;
#endif
  caml_parse_ocamlrunparam();
/*  initialize_allocation_profiling();*/
#ifdef DEBUG
  caml_gc_message (-1, "### OCaml runtime: debug mode ###\n", 0);
#endif
  caml_init_gc (caml_init_minor_heap_wsz, caml_init_heap_wsz,
                caml_init_heap_chunk_sz, caml_init_percent_free,
                caml_init_max_percent_free, caml_init_major_window);
  init_static();
  caml_init_signals();
  caml_init_backtrace();
  caml_debugger_init (); /* force debugger.o stub to be linked */
  exe_name = argv[0];
  if (exe_name == NULL) exe_name = "";
  if (caml_executable_name(proc_self_exe, sizeof(proc_self_exe)) == 0)
    exe_name = proc_self_exe;
  else
    exe_name = caml_search_exe_in_path(exe_name);
  caml_sys_init(exe_name, argv);
  if (sigsetjmp(caml_termination_jmpbuf.buf, 0)) {
    if (caml_termination_hook != NULL) caml_termination_hook(NULL);
    return;
  }
  res = caml_start_program();
  if (Is_exception_result(res))
    caml_fatal_uncaught_exception(Extract_exception(res));
}

void caml_startup(char **argv)
{
  caml_main(argv);
}
