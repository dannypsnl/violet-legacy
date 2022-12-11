SOURCE := Violet/Core.idr

build: ${SOURCE}
	@idris2 --build violet.ipkg

run: build
	@./build/exec/violetc check example/test.vt
