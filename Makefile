sources := $(shell find src -type f -name '*.idr')

.PHONY: build run
build: $(sources)
	@idris2 --build violet.ipkg

run: build
	@./build/exec/violet check example/test.vt
