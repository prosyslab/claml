DUNE=@dune
LN=@ln -sf

all:
	$(DUNE) build --auto-promote @fmt src
	$(DUNE) build test/test.exe
	$(DUNE) build test/rewriter/test.exe
	$(LN) _build/default/test/test.exe test-runner
	$(LN) _build/default/test/rewriter/test.exe rewriter-test-runner

test: all
	$(DUNE) test

fmt:
	$(DUNE) build @fmt --auto-promote

promote:
	$(DUNE) promote

clean:
	$(DUNE) clean
	rm -f test-runner rewriter-test-runner
