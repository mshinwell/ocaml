#**************************************************************************
#*                                                                        *
#*                                OCaml                                   *
#*                                                                        *
#*                 Xavier Clerc, SED, INRIA Rocquencourt                  *
#*                   Mark Shinwell, Jane Street Europe                    *
#*                                                                        *
#*   Copyright 2010 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*   Copyright 2017 Jane Street Group LLC                                 *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

COMPFLAGS=-I $(OTOPDIR)/otherlibs/dynlink
LD_PATH=.:$(TOPDIR)/otherlibs/dynlink

.PHONY: default
default:
	@if ! $(SUPPORTS_SHARED_LIBRARIES); then \
	  echo ' ... skipped (shared libraries not available)'; \
	else \
    printf " ... testing with ocamlc: "; \
	  $(SET_LD_PATH) $(MAKE) compile run; \
		if $(BYTECODE_ONLY); then \
			echo ' ... testing with ocamlopt skipped'; \
		else \
			printf " ... testing with ocamlopt: "; \
			$(SET_LD_PATH) $(MAKE) compile_opt run_opt; \
		fi; \
	fi

.PHONY: compile
compile:
	@$(OCAMLC) -c sheep.mli
	@$(OCAMLC) -c sheep.ml
	@$(OCAMLC) -c test.ml
	@$(OCAMLC) -c plugin/sheep.mli
	@$(OCAMLC) -o plugin/sheep.cmo -c plugin/sheep.ml
	@$(OCAMLC) -o test.byte dynlink.cma sheep.cmo test.cmo

.PHONY: compile_opt
compile_opt:
	@$(OCAMLOPT) -c sheep.mli
	@$(OCAMLOPT) -c sheep.ml
	@$(OCAMLOPT) -c test.ml
	@$(OCAMLOPT) -c plugin/sheep.mli
	@$(OCAMLOPT) -o plugin/sheep.cmxs -shared plugin/sheep.ml
	@$(OCAMLOPT) -o test.exe dynlink.cmxa sheep.cmx test.cmx

.PHONY: run1
run:
	@./test.byte && echo " => passed" || echo " => failed"

.PHONY: run_opt
run_opt:
	@./test.exe && echo " => passed" || echo " => failed"

.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean
	@rm -f main main.native

include $(BASEDIR)/makefiles/Makefile.common
