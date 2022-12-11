build:
	@idris2 --build violet.ipkg

run: build
	@./violetc check example/test.vt
