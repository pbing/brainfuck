# Build files from Brainfuck sources
# $Id: Makefile,v 1.8 2010/10/17 16:30:05 bernd Exp $

CLISP = dx86cl64

# For non optimized code use
# make ... LLCFLAGS=-O0
LLCFLAGS = -O3

.PHONY: all clean

.SECONDARY: brainfuck.dx64fsl

all:
	for f in *.bf; do $(MAKE) `basename $$f .bf`; done

clean:
	rm -f *.dx64fsl *.ll *.bc *.s
	for f in *.bf; do rm -f `basename $$f .bf`; done

%.dx64fsl: %.lisp
	$(CLISP) --eval '(compile-file "$<")' --eval '(quit)'

%.ll: %.bf brainfuck.dx64fsl
	$(CLISP) --load brainfuck.dx64fsl --eval '(bf:compile-file "$<")' --eval '(quit)'

%.bc: %.ll
	llvm-as -f $<

%.s: %.bc
	llc $(LLCFLAGS) $<

%: %.s
	gcc -o $@ $<
