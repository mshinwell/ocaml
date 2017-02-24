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
			echo 'testing with ocamlopt skipped'; \
		else \
			printf " ... testing with ocamlopt: "; \
			$(SET_LD_PATH) $(MAKE) compile_opt run_opt; \
		fi; \
	fi

.PHONY: compile
compile:
	@cd sub && $(OCAMLC) -c abstract.mli
	@cd sub && $(OCAMLC) -c abstract.ml
	@$(OCAMLC) -c abstract.mli
	@$(OCAMLC) -c abstract.ml
	@$(OCAMLC) -c static.ml
	@$(OCAMLC) -c client.ml
	@$(OCAMLC) -c main.ml
	@$(OCAMLC) -o main dynlink.cma abstract.cmo static.cmo main.cmo

.PHONY: compile_opt
compile_opt:
	@cd sub && $(OCAMLOPT) -c abstract.mli
	@cd sub && $(OCAMLOPT) -shared -o abstract.cmxs abstract.ml
	@$(OCAMLOPT) -c abstract.mli
	@$(OCAMLOPT) -c abstract.ml
	@$(OCAMLOPT) -c static.ml
	@$(OCAMLOPT) -shared -o client.cmxs client.ml
	@$(OCAMLOPT) -c main_native.ml
	@$(OCAMLOPT) -o main_native dynlink.cmxa abstract.cmx static.cmx \
    main_native.cmx

.PHONY: run
run:
	@./main > main.result 2>&1 || true
	@$(DIFF) main.reference main.result > /dev/null \
	  && echo " => passed" || echo " => failed"

.PHONY: run_opt
run_opt:
	@./main_native > main.native.result 2>&1 || true
	@$(DIFF) main.reference main.native.result > /dev/null \
	  && echo " => passed" || echo " => failed"

.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean
	@rm -f main main.native

include $(BASEDIR)/makefiles/Makefile.common
