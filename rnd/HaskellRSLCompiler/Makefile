.PHONY: test

all:
	ghc -o lslc --make Main.hs


clean:
	rm -rf *.hi *.o
	rm -rf slc

doc:
	haddoc

t:
	(cd test; make)
