;;; -*- Mode: LISP; Syntax: Common-lisp; Package: (DFS COMMON-LISP-GLOBAL); Base: 10; -*-

(IN-PACKAGE 'DFS)


(def-file-set (starlisp-simulator-f19 (:distribute-other t))

   #+symbolics
   "zwei-hacks"

   "make-packages"
   "def-starlisp"
   "external-symbols"
   ("import-export" :read)
   "help"
   "external-defvars"

   "destructuring-bind"
   "portable-proclaim"
   "defstruct-interface"
   "defstruct-parsing"
   "portable-geometry-ops"
   "starlisp-setf"

   "geometries"

   "simulator-specification"
   "unportable-proclaim"

   "no-bang-bang" ;;; remove need for most uses of !! operator

   "definitions"
   "pvars"
   "simulator-proclaim"

   "port"
   "starlisp-compiler-vars"
   "macros"

   "pvar-array-pool"
   "context"

   "vp-sets"
   "vp-sets-implementation"
   "pvar-allocation"
   "stardefvars"
   "cold-boot"

   "genfunctions"

   "pref"

   "defstruct-output"

   "trivfunctions"
   "functions"

   "sim"

   "internal-addressing"
   "addressing"
   "send"
   "get"
   "arrays"
   "address-objects"
   "address-object-smashing"

   "ppp"

   "hypergrid"
   "bitblt"
   "advanced-functions"
   "portable-rank-and-sort"

   ;; Experimental features

   "vector-functions"
   "sequence-ops-internal"
   "sequence-ops"
   "rational-scanning"

   ;; "defstruct-tests"
   ;; "array-tests"

   "patches"

   ("README-UNIX.lisp" :no-load)
   ("README-LISPM.lisp" :no-load)
   ("README-MAC.lisp" :no-load)
   ("starsim.1" :no-load)
   ("make.lisp" :no-load)
   ("defsys.f19" :no-load)
   ("how-to-make-tapes.lisp" :no-load)
   ("f19.sys" :no-load)
   ("Makefile.text" :no-load)

   ("examples-def-file-set.lisp" :no-load)
   ("text-processing-example.lisp" :no-load)
   ("very-long-addition-example.lisp" :no-load)
   ("canon-matrix-multiply-example.lisp" :no-load)
   ("combinations-example.lisp" :no-load)
   ("determinants-examples.lisp" :no-load)
   ("bitblt-example.lisp" :no-load)
   ("timing-example.lisp" :no-load)
   ("primes-example.lisp" :no-load)
   ("run-examples" :no-load)

   )
