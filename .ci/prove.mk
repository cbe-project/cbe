OUTDIR=$(shell pwd)/out

prove_spark_ci:
	make -C .ci/lsc NO_SPARK=1 DESTDIR=$(OUTDIR)/lsc install
	gnatprove -j0 --prover=z3,cvc4 --steps=1000 --memlimit=1000 --checks-as-errors --warnings=error -U -P .ci/cbe.gpr
