#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU Library General Public License, with     #
#   the special exception on linking described in file ../LICENSE.      #
#                                                                       #
#########################################################################

include Makefile.shared

allopt:
	$(MAKE) stdlib.cmxa std_exit.cmx
	$(MAKE) allopt-$(PROFILING)
	$(MAKE) allopt-allocprof-$(WITH_ALLOCATION_PROFILING)

allopt-noprof:

allopt-prof: stdlib.p.cmxa std_exit.p.cmx
	rm -f std_exit.p.cmi

allopt-allocprof-false:

allopt-allocprof-true: stdlib.ap.cmxa std_exit.ap.cmx
	rm -f std_exit.ap.cmi

installopt: installopt-default installopt-$(PROFILING)

installopt-default:
	cp stdlib.cmxa stdlib.a std_exit.o *.cmx $(INSTALL_LIBDIR)
	cd $(INSTALL_LIBDIR); $(RANLIB) stdlib.a

installopt-noprof:
	rm -f $(INSTALL_LIBDIR)/stdlib.p.cmxa; \
	  ln -s stdlib.cmxa $(INSTALL_LIBDIR)/stdlib.p.cmxa
	rm -f $(INSTALL_LIBDIR)/stdlib.p.a; \
	  ln -s stdlib.a $(INSTALL_LIBDIR)/stdlib.p.a
	rm -f $(INSTALL_LIBDIR)/std_exit.p.cmx; \
	  ln -s std_exit.cmx $(INSTALL_LIBDIR)/std_exit.p.cmx
	rm -f $(INSTALL_LIBDIR)/std_exit.p.o; \
	  ln -s std_exit.o $(INSTALL_LIBDIR)/std_exit.p.o

installopt-prof:
	cp stdlib.p.cmxa stdlib.p.a std_exit.p.cmx std_exit.p.o $(INSTALL_LIBDIR)
	cd $(INSTALL_LIBDIR); $(RANLIB) stdlib.p.a

installopt-allocprof-false:
	rm -f $(INSTALL_LIBDIR)/stdlib.ap.cmxa
	rm -f $(INSTALL_LIBDIR)/stdlib.ap.a
	rm -f $(INSTALL_LIBDIR)/std_exit.ap.cmx
	rm -f $(INSTALL_LIBDIR)/std_exit.ap.o

installopt-allocprof-true:
	cp stdlib.ap.cmxa stdlib.ap.a std_exit.ap.cmx std_exit.ap.o $(INSTALL_LIBDIR)
	cd $(INSTALL_LIBDIR); $(RANLIB) stdlib.ap.a

stdlib.p.cmxa: $(OBJS:.cmo=.p.cmx)
	$(CAMLOPT) -a -o stdlib.p.cmxa $(OBJS:.cmo=.p.cmx)

stdlib.ap.cmxa: $(OBJS:.cmo=.ap.cmx)
	$(CAMLOPT) -allocation-profiling -a -o stdlib.ap.cmxa $(OBJS:.cmo=.ap.cmx)

camlheader target_camlheader camlheaderd target_camlheaderd camlheader_ur: \
  header.c ../config/Makefile
	if $(SHARPBANGSCRIPTS); then \
	  echo '#!$(BINDIR)/ocamlrun' > camlheader && \
	  echo '#!$(TARGET_BINDIR)/ocamlrun' > target_camlheader && \
	  echo '#!$(BINDIR)/ocamlrund' > camlheaderd && \
	  echo '#!$(TARGET_BINDIR)/ocamlrund' > target_camlheaderd && \
	  echo '#!' | tr -d '\012' > camlheader_ur; \
	else \
	  for suff in '' d; do \
	    $(BYTECC) $(BYTECCCOMPOPTS) $(BYTECCLINKOPTS) \
	              -DRUNTIME_NAME='"$(BINDIR)/ocamlrun'$$suff'"' \
	              header.c -o tmpheader$(EXE) && \
	    strip tmpheader$(EXE) && \
	    mv tmpheader$(EXE) camlheader$$suff && \
	    $(BYTECC) $(BYTECCCOMPOPTS) $(BYTECCLINKOPTS) \
	              -DRUNTIME_NAME='"$(TARGET_BINDIR)/ocamlrun'$$suff'"' \
	              header.c -o tmpheader$(EXE) && \
	    strip tmpheader$(EXE) && \
	    mv tmpheader$(EXE) target_camlheader$$suff; \
	  done && \
	  cp camlheader camlheader_ur; \
	fi

.PHONY: all allopt allopt-noprof allopt-prof install installopt
.PHONY: installopt-default installopt-noprof installopt-prof clean depend
