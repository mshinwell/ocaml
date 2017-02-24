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
    printf " ... testing test1 with ocamlc: "; \
	  $(SET_LD_PATH) $(MAKE) compile; \
	  $(SET_LD_PATH) $(MAKE) run1; \
    printf " ... testing test2 with ocamlc: "; \
	  $(SET_LD_PATH) $(MAKE) run2; \
		if $(BYTECODE_ONLY); then \
			echo ' ... testing with ocamlopt skipped'; \
		else \
			printf " ... testing test1 with ocamlopt: "; \
			$(SET_LD_PATH) $(MAKE) compile_opt; \
			$(SET_LD_PATH) $(MAKE) run_opt1; \
			printf " ... testing test2 with ocamlopt: "; \
			$(SET_LD_PATH) $(MAKE) run_opt2; \
		fi; \
	fi

.PHONY: compile
compile:
	@$(OCAMLC) -c test1_main.ml
	@$(OCAMLC) -c test1_inited_second.ml
	@$(OCAMLC) -c test1_plugin.ml
	@$(OCAMLC) -o test1.byte dynlink.cma test1_main.cmo test1_inited_second.cmo
	@$(OCAMLC) -c test2_main.ml
	@$(OCAMLC) -c test2_plugin.ml
	@$(OCAMLC) -c test2_second_plugin.ml
	@$(OCAMLC) -o test2.byte dynlink.cma test2_main.cmo

.PHONY: compile_opt
compile_opt:
	@$(OCAMLOPT) -c test1_main.ml
	@$(OCAMLOPT) -c test1_inited_second.ml
	@$(OCAMLOPT) -shared -o test1_plugin.cmxs test1_plugin.ml
	@$(OCAMLOPT) -o test1.exe dynlink.cmxa test1_main.cmx test1_inited_second.cmx
	@$(OCAMLOPT) -c test2_main.ml
	@$(OCAMLOPT) -shared -o test2_plugin.cmxs test2_plugin.ml
	@$(OCAMLOPT) -shared -o test2_second_plugin.cmxs test2_second_plugin.ml
	@$(OCAMLOPT) -o test2.exe dynlink.cmxa test2_main.cmx

.PHONY: run1
run1:
	@./test1.byte && echo " => passed" || echo " => failed"

.PHONY: run2
run2:
	@./test2.byte && echo " => passed" || echo " => failed"

.PHONY: run_opt1
run_opt1:
	@./test1.exe && echo " => passed" || echo " => failed"

.PHONY: run_opt2
run_opt2:
	@./test2.exe && echo " => passed" || echo " => failed"

.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean
	@rm -f main main.native

include $(BASEDIR)/makefiles/Makefile.common
