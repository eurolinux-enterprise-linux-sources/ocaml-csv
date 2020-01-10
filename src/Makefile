# $Id: Makefile,v 1.11 2007-05-14 16:20:10 rich Exp $

include Makefile.config

# To enable support for Extlib, also uncomment 'open ExtList' at the top
# of file csv.ml.

OCAMLCINCS	:=
#OCAMLCINCS	:= -package extlib
OCAMLCFLAGS	:= -g
OCAMLCLIBS	:=
#OCAMLCLIBS	:= -linkpkg

OCAMLOPTINCS	:= $(OCAMLCINCS)
OCAMLOPTFLAGS	:=
OCAMLOPTLIBS	:=
#OCAMLOPTLIBS	:= -linkpkg

OBJS		:= csv.cmo
XOBJS		:= $(OBJS:.cmo=.cmx)

OCAMLDOCFLAGS := -html -stars -sort $(OCAMLCINCS)

all: csv.cma csv.cmxa example csvtool

csv.cma: $(OBJS)
	ocamlfind ocamlc $(OCAMLCINCS) $(OCAMLCFLAGS) -a -o $@ $^

csv.cmxa: $(XOBJS)
	ocamlfind ocamlopt $(OCAMLOPTINCS) $(OCAMLOPTFLAGS) -a -o $@ $^

example: csv.cma example.ml
	ocamlfind ocamlc $(OCAMLCINCS) $(OCAMLCFLAGS) $(OCAMLCLIBS) $^ -o $@

test:	csv.cma test.ml
	ocamlfind ocamlc $^ -o $@
	./test

csvtool: csv.cmxa csvtool.ml
	ocamlfind ocamlopt $(OCAMLOPTINCS) $(OCAMLOPTFLAGS) $(OCAMLOPTLIBS) $^ -o $@

# Common rules for building OCaml objects.

.mli.cmi:
	ocamlfind ocamlc $(OCAMLCFLAGS) $(OCAMLCINCS) -c $<
.ml.cmo:
	ocamlfind ocamlc $(OCAMLCFLAGS) $(OCAMLCINCS) -c $<
.ml.cmx:
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) $(OCAMLOPTINCS) -c $<

.SUFFIXES:	.cmo .cmi .cmx .ml .mli

# Clean.

clean:
	rm -f *.cmi *.cmo *.cmx *.cma *.cmxa *.o *.a *~ core csvtool example

# Dependencies.

depend: .depend

.depend: $(wildcard *.mli) $(wildcard *.ml)
	rm -f .depend
	ocamldep $^ > $@

ifeq ($(wildcard .depend),.depend)
include .depend
endif

# Build a distribution.

install:
	ocamlfind install csv csv.{cmi,a,cmxa,cma,cmx} META

dist:
	$(MAKE) check-manifest
	rm -rf $(PACKAGE)-$(VERSION)
	mkdir $(PACKAGE)-$(VERSION)
	tar -cf - -T MANIFEST | tar -C $(PACKAGE)-$(VERSION) -xf -
	tar zcf $(PACKAGE)-$(VERSION).tar.gz $(PACKAGE)-$(VERSION)
	rm -rf $(PACKAGE)-$(VERSION)
	ls -l $(PACKAGE)-$(VERSION).tar.gz

check-manifest:
	@for d in `find -type d -name CVS | grep -v '^\./debian/'`; \
	do \
	b=`dirname $$d`/; \
	awk -F/ '$$1 != "D" {print $$2}' $$d/Entries | \
	sed -e "s|^|$$b|" -e "s|^\./||"; \
	done | sort > .check-manifest; \
	sort MANIFEST > .orig-manifest; \
	diff -u .orig-manifest .check-manifest; rv=$$?; \
	rm -f .orig-manifest .check-manifest; \
	exit $$rv

# Developer documentation (in html/ subdirectory).

doc:
	rm -rf html
	mkdir html
	-ocamlfind ocamldoc $(OCAMLDOCFLAGS) -d html csv.ml{i,}

.PHONY: depend dist check-manifest doc
