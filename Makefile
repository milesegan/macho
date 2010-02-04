SOURCES=$(wildcard src/*.lisp)

all: macho.core

clean:
	@rm -f macho.core
	@find src lib -name \*.fasl -exec rm {} \;

macho.core: $(SOURCES)
	@echo compiling macho...
	@sbcl --noinform --load src/macho.lisp . false	
