OUTDIR=$(shell pwd)/out

PROVE_MODE ?= all

prove_spark_ci:
	make -C .ci/lsc NO_SPARK=1 DESTDIR=$(OUTDIR)/lsc install
	gnatprove --mode=$(PROVE_MODE) -j0 --prover=z3,cvc4 --steps=1000 --memlimit=1000 --checks-as-errors --warnings=error -U -P .ci/cbe.gpr
