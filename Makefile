all:
	dune build src
	dune build test/test.exe
	dune build test/rewriter/test.exe
	ln -sf _build/default/test/test.exe test-runner
	ln -sf _build/default/test/rewriter/test.exe rewriter-test-runner

test: all
	dune test

promote:
	dune promote

clean:
	dune clean
	rm -f test-runner
