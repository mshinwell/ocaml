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
compile: host.byt

.PHONY: compile_opt
compile_opt: host.exe

host.exe: packed.cmx host.cmx
	@$(OCAMLOPT) -o $@ dynlink.cmxa packed.cmx host.cmx

host.byt: packed.cmo host.cmo
	@$(OCAMLC) -o $@ dynlink.cma packed.cmo host.cmo

packed.cmx: api.cmx
	@$(OCAMLOPT) -pack -o $@ $<

packed.cmo: api.cmo
	@$(OCAMLC) -pack -o $@ $<

api.cmi: api.mli
	@$(OCAMLC) -c $<

api.cmo: api.ml api.cmi
	@$(OCAMLC) -c -for-pack Packed $<

api.cmx: api.ml api.cmi
	@$(OCAMLOPT) -c -for-pack Packed $<

.PHONY: clean
clean:
	rm -f *.cm* *.o host.exe host.byt

include $(BASEDIR)/makefiles/Makefile.common
