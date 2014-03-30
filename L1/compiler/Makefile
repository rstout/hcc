all: l1c

l1c:
	cabal configure
	cabal build
	cp dist/build/hcc/hcc bin/hcc
	cp dist/build/testsuite/testsuite bin/testsuite
	cp bin/hcc bin/l1c

clean:
	rm -rf dist/
