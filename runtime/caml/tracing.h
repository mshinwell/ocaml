#ifndef CAML_TRACING_H
#define CAML_TRACING_H

extern void caml_tracing_entering_function (uintnat pc);
extern void caml_tracing_leaving_function (void);
extern void caml_tracing_promotion (value old_block, value new_block);
extern void caml_tracing_swept (value block);

#endif
