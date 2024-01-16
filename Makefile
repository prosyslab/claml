DUNE=@dune
LN=@ln -sf
RM=@rm -f
CLANG-FORMAT=@clang-format

all:
	$(CLANG-FORMAT) --dry-run -Werror -i src/*.cpp src/*.h # check that the code is formatted
	$(DUNE) build --auto-promote @fmt src
	$(DUNE) build test/test.exe
	$(DUNE) build test/rewriter/test.exe
	$(LN) _build/default/test/test.exe test-runner
	$(LN) _build/default/test/rewriter/test.exe rewriter-test-runner

test: all
	$(DUNE) test

fmt:
	$(DUNE) build @fmt --auto-promote src
	$(CLANG-FORMAT) -i src/*.cpp src/*.h

promote:
	$(DUNE) promote

clean:
	$(DUNE) clean
	$(RM) -f test-runner rewriter-test-runner
