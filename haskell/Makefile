.PHONY: run configure build clean

build:
	cabal build

configure:
	cabal configure

clean:
	cabal clean

run:
	runhaskell Main.hs

rebuild: clean configure build
