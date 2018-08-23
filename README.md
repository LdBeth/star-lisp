# \*LISP Emulator

Original source code and other useful information from [softwarepreservation.org](http://www.softwarepreservation.org/projects/LISP/parallel#Connection_Machine_*Lisp_(StarLisp)_)

This software is in public domain, by JP Massar.

You are free to do what you whant, including but not limited to distributing, modifying and copying.

# Intro

\*LISP is a superset of Common Lisp used to performe parallel computation on Connection Machine.

# About this repo

The original F20 runs on several ANSI Common Lisp implementations.
A few modifications are made to enable running this emulator on Clozure Common Lisp.
Also ASDF is used to replace the vintage make.lisp facility for loading.

For porters who plan to run this emulator on other implementations, i.e. SBCL,
README-UNIX.lisp can be used as a guide.

# A quickstart demo

```text
;;; cd to f20 and start ccl
CL-USER> (load "f20.asd")
#P"/Users/ldbeth/Downloads/f20/f20.asd"
CL-USER> (asdf:load-system "f20")


Thinking Machines Starlisp Simulator.  Version 20.0

T
;; You migth see many compiler warnings here.
;; But don't worry.
CL-USER> (in-package :*lisp)
#<Package "*SIM">
*SIM> (*defvar *hello* 0)
*HELL*
*SIM> (ppp *hello*)
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
NIL
*SIM> (ppp *hello* :mode :grid)


     DIMENSION 0 (X)  ----->

0 0 0 0 0 0 0 0 
0 0 0 0 0 0 0 0 
0 0 0 0 0 0 0 0 
0 0 0 0 0 0 0 0 
NIL
*SIM> (ppp (self-address!!) :mode :grid)

     DIMENSION 0 (X)  ----->

0 4 8 12 16 20 24 28 
1 5 9 13 17 21 25 29 
2 6 10 14 18 22 26 30 
3 7 11 15 19 23 27 31 
NIL
*SIM> (do-text-processing "This is good")

Processor 0. Length: 4.  Word: This                            
Processor 1. Length: 2.  Word: is                              
Processor 2. Length: 4.  Word: good                            
NIL
*SIM> 
```
A tutorial is in both PostScript and [EZ Word](https://en.wikipedia.org/wiki/EZ_Word) source format,
with other examples.


Have fun.
