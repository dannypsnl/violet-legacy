sources := $(shell find src -type f -name '*.idr')

.PHONY: build install run
build: $(sources)
	@idris2 --build violet.ipkg

install: build
	@idris2 --install violet.ipkg

run: build
	@./build/exec/violet check example/test.vt
