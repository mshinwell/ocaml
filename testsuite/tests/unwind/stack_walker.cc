#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <caml/callback.h>
#include <caml/mlvalues.h>
#include <libunwind.h>

class Dummy_exn {};

extern "C"
value ml_func_with_10_params_native(value x1, value x2, value x3, value x4,
                                    value x5, value x6, value x7, value x8,
                                    value x9, value x10) {
    return Val_unit;
}

int perform_stack_walk(int dbg) {
    unw_context_t ctxt;
    unw_getcontext(&ctxt);

    unw_cursor_t cursor;
    {
        int result = unw_init_local(&cursor, &ctxt);
        if (result != 0) {
            if (dbg) printf("unw_init_local failed: %d\n", result);
            return -1;
        }
    }

    int reached_caml_start_program = 0;

    for (;;) {
        {
            char procname[256];
            unw_word_t ip_offset; // IP - start_of_proc
            int result = unw_get_proc_name(&cursor, procname, sizeof(procname),
                                           &ip_offset);
            if (result != 0) {
                if (dbg) printf("unw_get_proc_name failed: %d\n", result);
                return -1;
            }

            if (strcmp(procname, "caml_start_program") == 0)
                reached_caml_start_program = 1;
            if (dbg) printf("%s + %lld\n", procname, (long long int)ip_offset);
        }

        {
            int result = unw_step(&cursor);
            if (result == 0) break;
            if (result < 0) {
                if (dbg) printf("unw_step failed: %d\n", result);
                return -1;
            }
        }
    }

    if (dbg) printf("Reached end of stack.\n");
    if (!reached_caml_start_program) {
        if (dbg) printf("Failure: Did not reach caml_start_program.\n");
        return -1;
    }
    return 0;
}

extern "C"
value ml_perform_stack_walk() {
    /* Use C++ exception constructs to force clang to generate unwind tables. */
    try {
        if (perform_stack_walk(1) != 0) {
            printf("TEST FAILED\n");
            /* Re-run the test to produce a trace */
            perform_stack_walk(1);
            exit(1);
        }
    } catch (Dummy_exn exn) { throw exn; };
    return Val_unit;
}
