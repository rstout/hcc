all: l1c

l1c:
	cabal configure 
	cabal build
	cp dist/build/c0c/c0c bin/c0c
	cp bin/c0c bin/l1c

clean:
	rm -rf dist/
