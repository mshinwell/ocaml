/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#if _MSC_VER >= 1400 && _MSC_VER < 1700
/* Microsoft introduced a regression in Visual Studio 2005 (technically it's
   not present in the Windows Server 2003 SDK which has a pre-release version)
   and the abort function ceased to be declared __declspec(noreturn). This was
   fixed in Visual Studio 2012. Trick stdlib.h into not defining abort (this
   means exit and _exit are not defined either, but they aren't required). */
#define _CRT_TERMINATE_DEFINED
__declspec(noreturn) void __cdecl abort(void);
#endif

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include "caml/config.h"
#include "caml/misc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/osdeps.h"
#include "caml/version.h"

caml_timing_hook caml_major_slice_begin_hook = NULL;
caml_timing_hook caml_major_slice_end_hook = NULL;
caml_timing_hook caml_minor_gc_begin_hook = NULL;
caml_timing_hook caml_minor_gc_end_hook = NULL;
caml_timing_hook caml_finalise_begin_hook = NULL;
caml_timing_hook caml_finalise_end_hook = NULL;

#ifdef DEBUG

void caml_failed_assert (char * expr, char_os * file_os, int line)
{
  char* file = caml_stat_strdup_of_os(file_os);
  fprintf (stderr, "file %s; line %d ### Assertion failed: %s\n",
           file, line, expr);
  fflush (stderr);
  caml_stat_free(file);
  abort();
}

void caml_set_fields (value v, uintnat start, uintnat filler)
{
  mlsize_t i;
  for (i = start; i < Wosize_val (v); i++){
    Field (v, i) = (value) filler;
  }
}

#endif /* DEBUG */

uintnat caml_verb_gc = 0;

void caml_gc_message (int level, char *msg, ...)
{
  if ((caml_verb_gc & level) != 0){
    va_list ap;
    va_start(ap, msg);
    vfprintf (stderr, msg, ap);
    va_end(ap);
    fflush (stderr);
  }
}

void (*caml_fatal_error_hook) (char *msg, va_list args) = NULL;

CAMLexport void caml_fatal_error (char *msg, ...)
{
  va_list ap;
  va_start(ap, msg);
  if(caml_fatal_error_hook != NULL) {
    caml_fatal_error_hook(msg, ap);
  } else {
    fprintf (stderr, "Fatal error: ");
    vfprintf (stderr, msg, ap);
    fprintf (stderr, "\n");
  }
  va_end(ap);
  abort();
}

/* If you change the caml_ext_table* functions, also update
   runtime/spacetime_nat.c:find_trie_node_from_libunwind. */

void caml_ext_table_init(struct ext_table * tbl, int init_capa)
{
  tbl->size = 0;
  tbl->capacity = init_capa;
  tbl->contents = caml_stat_alloc(sizeof(void *) * init_capa);
}

int caml_ext_table_add(struct ext_table * tbl, caml_stat_block data)
{
  int res;
  if (tbl->size >= tbl->capacity) {
    tbl->capacity *= 2;
    tbl->contents =
      caml_stat_resize(tbl->contents, sizeof(void *) * tbl->capacity);
  }
  res = tbl->size;
  tbl->contents[res] = data;
  tbl->size++;
  return res;
}

void caml_ext_table_remove(struct ext_table * tbl, caml_stat_block data)
{
  int i;
  for (i = 0; i < tbl->size; i++) {
    if (tbl->contents[i] == data) {
      caml_stat_free(tbl->contents[i]);
      memmove(&tbl->contents[i], &tbl->contents[i + 1],
              (tbl->size - i - 1) * sizeof(void *));
      tbl->size--;
    }
  }
}

void caml_ext_table_clear(struct ext_table * tbl, int free_entries)
{
  int i;
  if (free_entries) {
    for (i = 0; i < tbl->size; i++) caml_stat_free(tbl->contents[i]);
  }
  tbl->size = 0;
}

void caml_ext_table_free(struct ext_table * tbl, int free_entries)
{
  caml_ext_table_clear(tbl, free_entries);
  caml_stat_free(tbl->contents);
}

/* Integer arithmetic with overflow detection */

#if ! (__GNUC__ >= 5 || Caml_has_builtin(__builtin_mul_overflow))
CAMLexport int caml_umul_overflow(uintnat a, uintnat b, uintnat * res)
{
#define HALF_SIZE (sizeof(uintnat) * 4)
#define HALF_MASK (((uintnat)1 << HALF_SIZE) - 1)
#define LOW_HALF(x) ((x) & HALF_MASK)
#define HIGH_HALF(x) ((x) >> HALF_SIZE)
  /* Cut in half words */
  uintnat al = LOW_HALF(a);
  uintnat ah = HIGH_HALF(a);
  uintnat bl = LOW_HALF(b);
  uintnat bh = HIGH_HALF(b);
  /* Exact product is:
              al * bl
           +  ah * bl  << HALF_SIZE
           +  al * bh  << HALF_SIZE
           +  ah * bh  << 2*HALF_SIZE
     Overflow occurs if:
        ah * bh is not 0, i.e. ah != 0 and bh != 0
     OR ah * bl has high half != 0
     OR al * bh has high half != 0
     OR the sum al * bl + LOW_HALF(ah * bl) << HALF_SIZE
                        + LOW_HALF(al * bh) << HALF_SIZE overflows.
     This sum is equal to p = (a * b) modulo word size. */
  uintnat p = a * b;
  uintnat p1 = al * bh;
  uintnat p2 = ah * bl;
  *res = p;
  if (ah == 0 && bh == 0) return 0;
  if (ah != 0 && bh != 0) return 1;
  if (HIGH_HALF(p1) != 0 || HIGH_HALF(p2) != 0) return 1;
  p1 <<= HALF_SIZE;
  p2 <<= HALF_SIZE;
  p1 += p2;
  if (p < p1 || p1 < p2) return 1; /* overflow in sums */
  return 0;
#undef HALF_SIZE
#undef HALF_MASK
#undef LOW_HALF
#undef HIGH_HALF
}
#endif

/* Runtime warnings */

uintnat caml_runtime_warnings = 0;
static int caml_runtime_warnings_first = 1;

int caml_runtime_warnings_active(void)
{
  if (!caml_runtime_warnings) return 0;
  if (caml_runtime_warnings_first) {
    fprintf(stderr, "[ocaml] (use Sys.enable_runtime_warnings to control "
                    "these warnings)\n");
    caml_runtime_warnings_first = 0;
  }
  return 1;
}

#ifdef CAML_INSTR
/* Timers for profiling GC and allocation (experimental, Linux-only) */

#include <limits.h>
#include <sys/types.h>
#include <unistd.h>

struct caml_instr_block *caml_instr_log = NULL;
intnat caml_instr_starttime, caml_instr_stoptime;

#define Get_time(p,i) ((p)->ts[(i)].tv_nsec + 1000000000 * (p)->ts[(i)].tv_sec)

void caml_instr_init (void)
{
  char *s;

  caml_instr_starttime = 0;
  s = caml_secure_getenv ("OCAML_INSTR_START");
  if (s != NULL) caml_instr_starttime = atol (s);
  caml_instr_stoptime = LONG_MAX;
  s = caml_secure_getenv ("OCAML_INSTR_STOP");
  if (s != NULL) caml_instr_stoptime = atol (s);
}

void caml_instr_atexit (void)
{
  int i;
  struct caml_instr_block *p, *prev, *next;
  FILE *f = NULL;
  char *fname;

  fname = caml_secure_getenv ("OCAML_INSTR_FILE");
  if (fname != NULL){
    char *mode = "a";
    char buf [1000];
    char *name = fname;

    if (name[0] == '@'){
      snprintf (buf, sizeof(buf), "%s.%lld",
                name + 1, (long long) (getpid ()));
      name = buf;
    }
    if (name[0] == '+'){
      mode = "a";
      name = name + 1;
    }else if (name [0] == '>' || name[0] == '-'){
      mode = "w";
      name = name + 1;
    }
    f = fopen (name, mode);
  }

  if (f != NULL){
    /* reverse the list */
    prev = NULL;
    p = caml_instr_log;
    while (p != NULL){
      next = p->next;
      p->next = prev;
      prev = p;
      p = next;
    }
    caml_instr_log = prev;
    fprintf (f, "==== OCAML INSTRUMENTATION DATA %s\n", OCAML_VERSION_STRING);
    for (p = caml_instr_log; p != NULL; p = p->next){
      for (i = 0; i < p->index; i++){
        fprintf (f, "@@ %19ld %19ld %s\n",
                 (long) Get_time (p, i),
                 (long) Get_time(p, i+1),
                 p->tag[i+1]);
      }
      if (p->tag[0][0] != '\000'){
        fprintf (f, "@@ %19ld %19ld %s\n",
                 (long) Get_time (p, 0),
                 (long) Get_time(p, p->index),
                 p->tag[0]);
      }
    }
    fclose (f);
  }
}
#endif /* CAML_INSTR */

int caml_find_code_fragment(char *pc, int *index, struct code_fragment **cf)
{
  struct code_fragment *cfi;
  int i;

  for (i = 0; i < caml_code_fragments_table.size; i++) {
    cfi = (struct code_fragment *) caml_code_fragments_table.contents[i];
    if ((char*) pc >= cfi->code_start && (char*) pc < cfi->code_end) {
      if (index != NULL) *index = i;
      if (cf != NULL) *cf = cfi;
      return 1;
    }
  }
  return 0;
}

/* CR mshinwell: Probably move the following to its own file, or maybe ints.c */

#if defined(__GNUC__)
#if ARCH_INT32_TYPE == long
#define int32_clz __builtin_clzl
#else /* ARCH_INT32_TYPE == long */
#define int32_clz __builtin_clz
#endif /* ARCH_INT32_TYPE == long */

#define int64_clz __builtin_clzll

#else /* defined(__GNUC__) */
#ifdef _MSC_VER
#include <intrin.h>
#pragma intrinsic(_BitScanReverse)

static int naive_int64_clz(uint64_t v)
{
  unsigned long n;
#ifdef ARCH_SIXTYFOUR
  if (_BitScanReverse64(&n, v)) return 63-n;
  else return 64;
#else
  /* _BitScanReverse64 is not supported */
  if ((v >> 32) == 0)
    {
      if (_BitScanReverse(&n,v)) return 63-n;
      else return 64;
    }
  else
    {
      _BitScanReverse(&n,(v>>32));
      return 31-n;
    }
#endif
}

static int naive_int32_clz(uint32_t v)
{
  unsigned long n;
  if (_BitScanReverse(&n, v))
#ifdef ARCH_SIXTYFOUR
    return 63 - n;
#else
    return 31 - n;
#endif
  else return 32;
}


#define int32_clz naive_int32_clz
#define int64_clz naive_int64_clz
#endif /* _MSC_VER */
#endif /* defined(__GNUC__) */

static int wrap_int32_clz(uint32_t x)
{
  int res;
  /* builtin_clz on input 0 is undefined */
  if (x == 0) res = 32;
  else
    {
      res = int32_clz(x);
#ifdef ARCH_SIXTYFOUR
      res -= 32;
#endif
    }
  return res;
}

static int wrap_int64_clz(uint64_t x)
{
  int res;
  /* builtin_clz on input 0 is undefined */
  if (x == 0) res = 64;
  else res = int64_clz(x);
  return res;
}

CAMLprim value caml_stub_untagged_int_clz(value v1)
{
#ifdef ARCH_SIXTYFOUR
  return wrap_int64_clz((uint64_t)v1);
#else
  return wrap_int32_clz((uint32_t)v1);
#endif
}

CAMLprim value caml_stub_int_clz(value v1)
{
  /* Do not use Long_val(v1) conversion and preserve the tag. It
     guarantees that the input to builtin_clz is non-zero, to guard
     against versions of builtin_clz that are undefined for intput 0.
     The tag does not change the number of leading zeros.
   */
#ifdef ARCH_SIXTYFOUR
  return Val_long(int64_clz((uint64_t)v1));
#else
  return Val_long(int32_clz((uint32_t)v1));
#endif
}

CAMLprim int32_t caml_stub_int32_clz_unboxed(int32_t v)
{ return wrap_int32_clz((uint32_t) v); }

CAMLprim value caml_stub_int32_clz_unboxed_tag(int32_t v)
{ return Val_long(wrap_int32_clz((uint32_t) v)); }

CAMLprim value caml_stub_int32_clz(value v1)
{ return Val_long(wrap_int32_clz((uint32_t)Int32_val(v1))); }

CAMLprim int64_t caml_stub_int64_clz_unboxed(int64_t v)
{ return wrap_int64_clz((uint64_t) v); }

CAMLprim value caml_stub_int64_clz_unboxed_tag(int64_t v)
{ return Val_long(wrap_int64_clz((uint64_t) v)); }

CAMLprim value caml_stub_int64_clz(value v1)
{ return Val_long(wrap_int64_clz((uint64_t) Int64_val(v1))); }

CAMLprim intnat caml_stub_nativeint_clz_unboxed(intnat v)
{
#ifdef ARCH_SIXTYFOUR
  return wrap_int64_clz((uint64_t) v);
#else
  return wrap_int32_clz((uint32_t) v);
#endif
}

CAMLprim value caml_stub_nativeint_clz_unboxed_tag(intnat v)
{
#ifdef ARCH_SIXTYFOUR
  return Val_long(wrap_int64_clz((uint64_t) v));
#else
  return Val_long(wrap_int32_clz((uint32_t) v));
#endif
}

CAMLprim value caml_stub_nativeint_clz(value v1)
{
#ifdef ARCH_SIXTYFOUR
  return Val_long(wrap_int64_clz((uint64_t) Int64_val(v1)));
#else
  return Val_long(wrap_int32_clz((uint32_t) Int32_val(v1)));
#endif
}
