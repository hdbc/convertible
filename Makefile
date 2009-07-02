all: setup
	@echo "Please use Cabal to build this package; not make."
	./setup configure
	./setup build

setup: Setup.lhs
	ghc --make -o setup Setup.lhs

install: setup
	./setup install

clean:
	-./setup clean
	-runghc Setup.lhs clean

.PHONY: test
test: test-ghc test-hugs
	@echo ""
	@echo "All tests pass."

test-hugs:
	@echo " ****** Running hugs tests"
	runhugs -98 +o -P$(PWD):$(PWD)/testsrc: testsrc/runtests.hs

test-ghc:
	@echo " ****** Building GHC tests"
	runghc Setup.lhs configure -f buildtests
	runghc Setup.lhs build
	@echo " ****** Running GHC tests"
	./dist/build/runtests/runtests
