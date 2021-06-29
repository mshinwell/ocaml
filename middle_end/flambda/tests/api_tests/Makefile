#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                     Vincent Laviron, OCamlPro                          *
#*                                                                        *
#*   Copyright 2021 OCamlPro SAS                                          *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

ROOTDIR=../..

include $(ROOTDIR)/Makefile.common
# Copied from the root Makefile.
# The Makefile cannot be included as it contains paths relative
# to the root.

CAMLC=$(BOOT_OCAMLC) -g -nostdlib -I $(ROOTDIR)/boot -use-prims $(ROOTDIR)/runtime/primitives
CAMLOPT=$(CAMLRUN) $(ROOTDIR)/ocamlopt -g -nostdlib -I $(ROOTDIR)/stdlib
ROOTINCLUDES=-I utils -I parsing -I typing -I bytecomp -I file_formats \
        -I lambda -I middle_end -I middle_end/closure \
        -I middle_end/flambda/compilenv_deps \
        -I middle_end/flambda \
        -I middle_end/flambda/basic \
        -I middle_end/flambda/cmx \
        -I middle_end/flambda/compare \
        -I middle_end/flambda/from_lambda \
        -I middle_end/flambda/inlining \
        -I middle_end/flambda/lifting \
        -I middle_end/flambda/naming \
        -I middle_end/flambda/parser \
        -I middle_end/flambda/simplify \
        -I middle_end/flambda/simplify/basic \
        -I middle_end/flambda/simplify/env \
        -I middle_end/flambda/simplify/typing_helpers \
        -I middle_end/flambda/terms \
        -I middle_end/flambda/to_cmm \
        -I middle_end/flambda/types \
        -I middle_end/flambda/types/basic \
        -I middle_end/flambda/types/env \
        -I middle_end/flambda/types/kinds \
        -I middle_end/flambda/types/structures \
        -I middle_end/flambda/types/type_of_kind \
        -I middle_end/flambda/types/type_of_kind/boilerplate \
        -I middle_end/flambda/unboxing \
        -I asmcomp -I asmcomp/debug \
        -I driver -I toplevel
INCLUDES=$(subst -I$(SPACE), -I$(SPACE)$(ROOTDIR)/,$(ROOTINCLUDES))

COMPFLAGS=-strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48-66 \
	  -warn-error A \
          -bin-annot -safe-string -strict-formats $(INCLUDES)
LINKFLAGS=

CAMLDEP=$(CAMLRUN) $(ROOTDIR)/boot/ocamlc -depend
DEPFLAGS=-slash
DEPINCLUDES=$(INCLUDES)

SOURCES=extension_meet.ml import_test.ml

COMPILERLIBS=\
  $(ROOTDIR)/compilerlibs/ocamlcommon.cma \
  $(ROOTDIR)/compilerlibs/ocamloptcomp.cma

all:$(SOURCES:.ml=.byte)
allopt:$(SOURCES:.ml=.native)

# Work around a stupid feature of make that helpfully removes files it
# considers intermediate artefacts.
allobjects:$(SOURCES:.ml=.cmo) $(SOURCES:.ml=.cmx)

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) $(OPTCOMPFLAGS) -c $<

%.byte: $(COMPILERLIBS) %.cmo
	$(CAMLC) $(LINKFLAGS) -o $@ $^

%.native: $(COMPILERLIBS:.cma=.cmxa) %.cmx
	$(CAMLOPT) $(LINKFLAGS) -o $@ $^

EMPTY=
SPACE=$(EMPTY) $(EMPTY)

.PHONY: depend
depend:
	($(CAMLDEP) $(DEPFLAGS) $(DEPINCLUDES) \
          *.mli *.ml || exit) > .depend

.PHONY: clean
clean:
	rm -f *.cm[ioxt] *.cmti *.s *.o *.byte *.native
