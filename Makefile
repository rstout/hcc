all: hcc

configure:
	cabal configure --enable-tests

build:
	cabal build

copy_to_bin:
	cp dist/build/hcc/hcc bin/hcc
	cp dist/build/testsuite/testsuite bin/testsuite
	cp bin/hcc bin/l1c

hcc: configure build copy_to_bin

clean:
	rm -rf dist/
