# Build files from Brainfuck sources
# $Id: Makefile,v 1.5 2009/12/24 22:06:54 bernd Exp bernd $

# For non optimized code use
# make ... LLCFLAGS=-O0
LLCFLAGS = -O3

.PHONY: all clean

all:
	for f in *.bf; do $(MAKE) `basename $$f .bf`; done

clean:
	rm -f *.ll *.bc *.s
	for f in *.bf; do rm -f `basename $$f .bf`; done

%.fasl: %.lisp
	sbcl --noinform --eval '(compile-file "$<")' --eval '(quit)'

%.ll: %.bf brainfuck.fasl
	sbcl --noinform --load brainfuck.fasl --eval '(bf:compile-file "$<")' --eval '(quit)'

%.bc: %.ll
	llvm-as -f $<

%.s: %.bc
	llc $(LLCFLAGS) -f $<

%: %.s
	gcc -o $@ $<
