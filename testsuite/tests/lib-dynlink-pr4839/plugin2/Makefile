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

COMPFLAGS=-I $(OTOPDIR)/otherlibs/dynlink
LD_PATH=.:$(TOPDIR)/otherlibs/dynlink

.PHONY: compile
compile: plugin.cmo

.PHONY: compile_opt
compile_opt: plugin.cmxs

api.cmi: api.mli
	@$(OCAMLC) -c $<

api.cmx: api.ml api.cmi
	@$(OCAMLOPT) -c -for-pack Packed $<

api.cmo: api.ml api.cmi
	@$(OCAMLC) -c -for-pack Packed $<

packed.cmx: api.cmx
	@$(OCAMLOPT) -pack -o $@ $<

packed.cmo: api.cmo
	@$(OCAMLC) -pack -o $@ $<

plugin.cmo: packed.cmo plugin.ml
	@$(OCAMLC) -c plugin.ml

plugin.cmx: packed.cmo plugin.ml
	@$(OCAMLOPT) -c plugin.ml

plugin.cmxs: packed.cmx plugin.cmx
	@$(OCAMLOPT) -shared -o $@ plugin.cmx

.PHONY: clean
clean:
	rm -f *.cm* *.o *.exe

include $(BASEDIR)/makefiles/Makefile.common
