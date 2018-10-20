# Build files from Brainfuck sources
# $Id: Makefile,v 1.8 2010/10/17 16:30:05 bernd Exp $

# For non optimized code use
# make ... LLCFLAGS=-O0
LLCFLAGS = -O3

.PHONY: all clean

.SECONDARY: brainfuck.fasl

all:
	for f in *.bf; do $(MAKE) `basename $$f .bf`; done

clean:
	rm -f *.fasl *.ll *.bc *.s
	for f in *.bf; do rm -f `basename $$f .bf`; done

%.fasl: %.lisp
	sbcl --noinform --eval '(compile-file "$<")' --eval '(quit)'

%.ll: %.bf brainfuck.fasl
	sbcl --noinform --load brainfuck.fasl --eval '(bf:compile-file "$<")' --eval '(quit)'

%.bc: %.ll
	llvm-as -f $<

%.s: %.bc
	llc $(LLCFLAGS) $<

%: %.s
	gcc -o $@ $<
