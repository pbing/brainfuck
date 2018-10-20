# Simple brainfuck to LLVM compiler

Deliberately no optimizations -- let LLVM handle this.

Benchmarked with LLVM-7.0.0

## UNOPTIMIZED COMPILATION

```
rm -f mandelbrot

make mandelbrot LLCFLAGS=-O0

ls -l mandelbrot
-rwxr-xr-x  1 bernd  501  102776 20 Okt 13:48 mandelbrot

time ./mandelbrot > /dev/null
./mandelbrot > /dev/null  6,67s user 0,01s system 99% cpu 6,677 total
```

## OPTIMIZED COMPILATION

```
rm -f mandelbrot

make mandelbrot

ls -l mandelbrot
-rwxr-xr-x  1 bernd  501  29048 20 Okt 13:58 mandelbrot

time ./mandelbrot > /dev/null
./mandelbrot > /dev/null  1,04s user 0,00s system 99% cpu 1,039 total
```
