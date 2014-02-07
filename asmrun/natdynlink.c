/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Alain Frisch, projet Gallium, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2007 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

#include "misc.h"
#include "mlvalues.h"
#include "memory.h"
#include "stack.h"
#include "callback.h"
#include "alloc.h"
#include "intext.h"
#include "osdeps.h"
#include "fail.h"

#include <stdio.h>
#include <string.h>
#include <stdarg.h>

static void *getsym(void *handle, char *module, char *name){
  char *fullname = malloc(strlen(module) + strlen(name) + 5);
  void *sym;
  sprintf(fullname, "caml%s%s", module, name);
  sym = caml_dlsym (handle, fullname);
  /*  printf("%s => %lx\n", fullname, (uintnat) sym); */
  free(fullname);
  return sym;
}

extern char caml_globals_map[];

CAMLprim value caml_natdynlink_getmap(value unit)
{
  return (value)caml_globals_map;
}

CAMLprim value caml_natdynlink_globals_inited(value unit)
{
  return Val_int(caml_globals_inited);
}

CAMLprim value caml_natdynlink_open(value filename, value global)
{
  CAMLparam1 (filename);
  CAMLlocal1 (res);
  void *sym;
  void *handle;

  /* TODO: dlclose in case of error... */

  handle = caml_dlopen(String_val(filename), 1, Int_val(global));

  if (NULL == handle)
    CAMLreturn(caml_copy_string(caml_dlerror()));

  sym = caml_dlsym(handle, "caml_plugin_header");
  if (NULL == sym)
    CAMLreturn(caml_copy_string("not an OCaml plugin"));

  res = caml_alloc_tuple(2);
  Field(res, 0) = (value) handle;
  Field(res, 1) = (value) (sym);
  CAMLreturn(res);
}

CAMLprim value caml_natdynlink_run(void *handle, value symbol) {
  CAMLparam1 (symbol);
  CAMLlocal1 (result);
  void *sym,*sym2;
  struct code_fragment * cf;

#define optsym(n) getsym(handle,unit,n)
  char *unit;
  void (*entrypoint)(void);

  unit = String_val(symbol);

  sym = optsym("__frametable");
  if (NULL != sym) caml_register_frametable(sym);

  sym = optsym("");
  if (NULL != sym) caml_register_dyn_global(sym);

  sym = optsym("__data_begin");
  sym2 = optsym("__data_end");
  if (NULL != sym && NULL != sym2)
    caml_page_table_add(In_static_data, sym, sym2);

  sym = optsym("__code_begin");
  sym2 = optsym("__code_end");
  if (NULL != sym && NULL != sym2) {
    caml_page_table_add(In_code_area, sym, sym2);
    cf = caml_stat_alloc(sizeof(struct code_fragment));
    cf->code_start = (char *) sym;
    cf->code_end = (char *) sym2;
    cf->digest_computed = 0;
    caml_ext_table_add(&caml_code_fragments_table, cf);
  }

  entrypoint = optsym("__entry");
  if (NULL != entrypoint) result = caml_callback((value)(&entrypoint), 0);
  else result = Val_unit;

#undef optsym

  CAMLreturn (result);
}

CAMLprim value caml_natdynlink_run_toplevel(value filename, value symbol)
{
  CAMLparam2 (filename, symbol);
  CAMLlocal2 (res, v);
  void *handle;

  /* TODO: dlclose in case of error... */

  handle = caml_dlopen(String_val(filename), 1, 1);

  if (NULL == handle) {
    res = caml_alloc(1,1);
    v = caml_copy_string(caml_dlerror());
    Store_field(res, 0, v);
  } else {
    res = caml_alloc(1,0);
    v = caml_natdynlink_run(handle, symbol);
    Store_field(res, 0, v);
  }
  CAMLreturn(res);
}

int ensure_natdynlink_is_linked = 0;
int natdynlink_root_registered = 0;
value natdynlink_return_value = Val_unit;

CAMLprim value caml_natdynlink_gdb_set_result(value v)
{
  if (!natdynlink_root_registered) {
    caml_register_generational_global_root(&natdynlink_return_value);
    natdynlink_root_registered = 1;
  }

  natdynlink_return_value = v;

  return Val_unit;
}

static int gdb_num_vars;
static value* gdb_vars = NULL;

CAMLprim value caml_natdynlink_gdb_get_var(value arg_index)
{
  int arg_index_int = Int_val(arg_index);
  return gdb_vars[arg_index_int];
}

CAMLprim value caml_natdynlink_gdb_run(int num_args, ...)
{
  va_list ap;
  void* handle;
  char module_name[1000];
  char cmxs_name[1000];
  static int counter = 0;

  va_start(ap, num_args);

  if (num_args > 0) {
    int var;
    if (gdb_vars != NULL) {
      for (var = 0; var < gdb_num_vars; var++) {
        caml_remove_generational_global_root(&gdb_vars[var]);
      }
      free(gdb_vars);
    }
    gdb_vars = malloc(sizeof(value) * num_args);
    gdb_num_vars = num_args;
    for (var = 0; var < gdb_num_vars; var++) {
      gdb_vars[var] = va_arg(ap, value);
      caml_register_generational_global_root(&gdb_vars[var]);
    }
  }

  /* TODO: dlclose in case of error... */

  snprintf(cmxs_name, 1000, "/tmp/gdb_expr%d.cmxs", counter);
  handle = caml_dlopen(cmxs_name, 1, 0);

  if (NULL == handle) {
    fprintf(stderr, "Failed to load expression into target for evaluation: %s\n",
            caml_dlerror());
    return Val_unit;
  }

  /* CR mshinwell: fix counting nonsense */
  snprintf(module_name, 1000, "Gdb_expr%d", counter);

  (void) caml_natdynlink_run(handle, caml_copy_string(module_name));

  counter++;

  va_end(ap);

  return natdynlink_return_value;
}

CAMLprim value caml_natdynlink_loadsym(value symbol)
{
  CAMLparam1 (symbol);
  CAMLlocal1 (sym);

  sym = (value) caml_globalsym(String_val(symbol));
  if (!sym) caml_failwith(String_val(symbol));
  CAMLreturn(sym);
}
