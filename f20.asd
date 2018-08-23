;;; -*- Mode: LISP; Syntax: Common-lisp; Package: User; Base: 10 -*-

(in-package :asdf)

(defsystem f20
  :license "Public Domain"
  :components
  ((:static-file "tutorial.ez")
   (:static-file "tutorial.ps")
   (:module "source"
    :serial t
    :components
    ((:file "pre-package")
     (:file "make-packages")
     (:file "def-starlisp")
     (:file "external-symbols")
     (:file "import-export")
     (:file "help")
     (:file "external-defvars")
     (:file "starlisp-compiler-vars")
     (:file "dummy-functions")
     (:file "utility-macros")
     (:file "utilities")
     (:file "simulator-specification")
     (:file "definitions") (:file "pvars")
     (:file "context")
     (:file "geometries") (:file "vp-sets")
     (:file "type-system-basics") (:file "defstruct-interface")
     (:file "new-canonical-pvar-type") (:file "valid-pvar-type")
     (:file "type-system-internals") (:file "pvar-type-predicates")
     (:file "canonical-pvar-type") (:file "type-system-interface")
     (:file "type-system-deftypes") (:file "defstruct-parsing")
     (:file "defstruct-output") (:file "starlisp-setf")
     (:file "unportable-proclaim") (:file "no-bang-bang")
     (:file "simulator-proclaim") (:file "port") (:file "macros")
     (:file "macros-2") (:file "pvar-array-pool")
     (:file "handle-returning-pvar")
     (:file "pvar-allocation")
     (:file "stardefvar-internals") (:file "hypergrid")
     (:file "vp-sets-implementation")
     (:file "stardefvars") (:file "genfunctions")
     (:file "bang-bang") (:file "sim")
     (:file "trivfunctions") (:file "pref")
     (:file "address-object-defstruct")
     (:file "functions")
     (:file "internal-addressing")
     (:file "addressing") (:file "arrays")
     (:file "send") (:file "get") (:file "address-objects")
     (:file "address-object-smashing") (:file "ppp")
     (:file "cold-boot")
     (:file "bitblt") (:file "advanced-functions")
     (:file "portable-rank-and-sort")
     (:file "vector-functions")
     (:file "sequence-ops-internal")
     (:file "sequence-ops") (:file "rational-scanning")
     (:file "patches")))))

(defsystem star-lisp-examples
  :depends-on ("f20")
  :components
  ((:module "examples"
    :components
    ((:file "text-processing-example")
     (:file "bitblt-example")
     ;; (:file "ca-example")
     (:file "canon-mm-example")
     (:file "combinations-example")
     (:file "primes-example")
     ;; (:file "timing-example")
     (:file "very-long-addition-example")))))