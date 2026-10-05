RBIN := C:/Program Files/R/R-4.6.1/bin

.PHONY: run sdl
sdl:
	build_sdl.bat

run: sdl
	"$(RBIN)/Rscript.exe" --vanilla main.R $(ARGS)
