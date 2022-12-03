build:
	@idris --build violet.ipkg

run: build
	@./violetc check example/test.vt
