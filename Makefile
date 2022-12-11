sources := $(wildcard *.idr)

build: $(sources)
	@idris2 --build violet.ipkg

run: build
	@./build/exec/violetc check example/test.vt
