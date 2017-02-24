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

COMPFLAGS=-I $(OTOPDIR)/otherlibs/dynlink -I $(OTOPDIR)/compilerlibs
LD_PATH=.:$(TOPDIR)/otherlibs/dynlink -I $(TOPDIR)/compilerlibs

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
compile: loader.byte plugin.cmo

.PHONY: compile_opt
compile_opt: loader.exe plugin.cmxs

loader.byte: loader.ml
	@$(OCAMLC) -I +compiler-libs ocamlcommon.cma dynlink.cma -linkall \
    loader.ml -o loader.byte

loader.exe: loader.ml
	@$(OCAMLOPT) -I +compiler-libs ocamlcommon.cmxa dynlink.cmxa -linkall \
    loader.ml -o loader.exe

plugin.cmo: b.ml config.ml
	@$(OCAMLC) -I . -a -o plugin.cma config.ml b.ml

plugin.cmxs: b.ml config.ml
	@$(OCAMLOPT) -I . -shared -o plugin.cmxs config.ml b.ml

.PHONY: run
run:
	@./loader.byte plugin.cma > byte.result 2>&1 || true
	@$(DIFF) byte.reference byte.result >/dev/null \
		&& echo " => passed" || echo " => failed"

.PHONY: run_opt
run_opt:
	@./loader.exe plugin.cmxs > native.result 2>&1 || true
	@$(DIFF) native.reference native.result >/dev/null \
		&& echo " => passed" || echo " => failed"

.PHONY: clean
clean:
	rm -f *.cm* *.o loader.exe loader.byte

include $(BASEDIR)/makefiles/Makefile.common
