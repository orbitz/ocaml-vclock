CAMLP4=
OCAMLC_OPTS=
OCAMLOPT_OPTS=$(OCAMLC_OPTS)

cources=vclock.ml

sources=$(cources)

sources_mli=$(cources:%.ml=%.mli)

native_cmx=$(sources:%.ml=%.cmx)

byte_cmo=$(sources:%.ml=%.cmo)

native_cmi=$(sources_mli:%.mli=%.cmi)

.PHONY: all native byte native-code byte-code test clean

all: native byte

native-code: native

byte-code: byte

test: vclock_test
	./vclock_test

vclock_test: vclock.cmxa vclock_test.cmx
	ocamlfind ocamlopt $(OCAMLOPT_OPTS) -linkpkg -o $@ $^

vclock_test.cmx: vclock.cmxa

native: vclock.cmxa

vclock.cmxa: vclock.cmx

byte: vclock.cma

vclock.cma: vclock.cmo

clean:
	-rm *.cmxa *.cmi *.cmx *.cmo *.o .d

# Dependencies
include ../Ocamlrules.mk.in
