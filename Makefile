# Build files from Brainfuck sources
# $Id: Makefile,v 1.2 2009/12/21 22:12:07 bernd Exp bernd $

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
	llvm-as < $< | opt -O3 -f -o $@

%.s: %.bc
	llc -f $<

%: %.s
	gcc -o $@ $<
