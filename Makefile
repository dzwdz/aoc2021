bin/%: %.hs
	@mkdir -p bin
	ghc -outputdir bin -o $@ $^ -Wall
