all: install

install: build
	idris --install jason.ipkg

build: Jason/*.idr
	idris --build jason.ipkg

clean:
	idris --clean jason.ipkg
	rm -f tests/*.ibc
