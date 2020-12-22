#!/bin/sh

set -e

asm=no_builtin_attr.${asmext}

grep call ${asm} | grep caml_rdtsc_unboxed | wc -l
grep call ${asm} | grep caml_int_clz_untagged | wc -l
grep call ${asm} | grep caml_int_bsr_untagged | wc -l

