#**************************************************************************
#*                                                                        *
#*                                OCaml                                   *
#*                                                                        *
#*                              Jun Furuse                                *
#*                 Xavier Clerc, SED, INRIA Rocquencourt                  *
#*                   Mark Shinwell, Jane Street Europe                    *
#*                                                                        *
#*   Copyright 2009 Jun Furuse                                            *
#*   Copyright 2010 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*   Copyright 2017 Jane Street Group LLC                                 *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

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
	@(cd host; make compile)
	@(cd plugin; make compile)
	@(cd plugin2; make compile)
	@(cd plugin3; make compile)
	@(cd plugin4; make compile)

.PHONY: compile_opt
compile_opt:
	@(cd host; make compile_opt)
	@(cd plugin; make compile_opt)
	@(cd plugin2; make compile_opt)
	@(cd plugin3; make compile_opt)
	@(cd plugin4; make compile_opt)

.PHONY: run
run:
	@(cd host; ./host.byt ../plugin/plugin.cmo > ../result.byte 2>&1)
	@(cd host; ./host.byt ../plugin2/plugin.cmo >> ../result.byte 2>&1)
	@(cd host; ./host.byt ../plugin3/plugin.cmo >> ../result.byte 2>&1)
	@(cd host; ./host.byt ../plugin4/plugin.cmo >> ../result.byte 2>&1)
	@$(DIFF) byte.reference result.byte >/dev/null \
		&& echo " => passed" || echo " => failed"

.PHONY: run_opt
run_opt:
	@(cd host; ./host.exe ../plugin/plugin.cmxs > ../result.native 2>&1)
	@(cd host; ./host.exe ../plugin2/plugin.cmxs >> ../result.native 2>&1)
	@(cd host; ./host.exe ../plugin3/plugin.cmxs >> ../result.native 2>&1)
	@(cd host; ./host.exe ../plugin4/plugin.cmxs >> ../result.native 2>&1)
	@$(DIFF) native.reference result.native >/dev/null \
		&& echo " => passed" || echo " => failed"

.PHONY: clean
clean:
	@(cd host; make clean)
	@(cd plugin; make clean)
	@(cd plugin2; make clean)
	@(cd plugin3; make clean)
	@(cd plugin4; make clean)

include $(BASEDIR)/makefiles/Makefile.common
