;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-


(in-package #+LISPWORKS :user #-LISPWORKS :common-lisp-user)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.

;;;>
;;;> The Starlisp language was originally designed by Cliff Lasser and Steve
;;;> Omohundro, with help from many others at Thinking Machines.
;;;> 
;;;> The original Starlisp Interpreter was written by Cliff Lasser, with help
;;;> from George Robertson.
;;;> 
;;;> The Starlisp Simulator was written by JP Massar.
;;;> 
;;;> The Starlisp Compiler was designed and written by Jeff Mincy.
;;;> 
;;;> The Release 5 extensions to the Starlisp language were
;;;> designed by Cliff Lasser, JP Massar and Jeff Mincy, with help
;;;> and ideas from many others at Thinking Machines.
;;;>
;;;> The Release 5 Starlisp Interpreter was written by Cliff Lasser
;;;> and JP Massar, with help from Jeff Mincy.

;;;> Many of the ideas behind the Release 5 extensions to Starlisp,
;;;> including arrays, structures and vp sets, had their origin
;;;> in Jim Salem's New Types extensions to Starlisp.
;;;>


(defpackage "*SIM-I"
  (:use "COMMON-LISP")
  (:nicknames "*LISP-I")
  )

(defpackage-with-lists-symbols 
    "*SIM"
    (:shadow-list-symbol *common-lisp-shadowed-symbol-names*)
  (:use "COMMON-LISP")
  (:nicknames "*LISP")
  (:shadow :proclaim :declaim)
  )


(defpackage-with-lists-symbols 
    "*SIM-COMPILER"
    (:shadowing-import-from-package 
     "COMMON-LISP"
     :shadowing-import-from-list-symbol 
     *common-lisp-shadowed-symbol-names*
     )
  (:use "*SIM-I" "COMMON-LISP")
  (:nicknames "*LISP-COMPILER" "SLC")
  )


(defpackage-with-lists-symbols 
    "*SIM-USER"
    (:shadowing-import-from-package 
     "*SIM"
     :shadowing-import-from-list-symbol 
     *common-lisp-shadowed-symbol-names*
     )
  (:use "*SIM" "COMMON-LISP")
  (:nicknames "*USER" "*LISP-USER")
  )

(defparameter *lisp-i::*starlisp-package-name* :*SIM)
(defparameter *lisp-i::*starlisp-internal-package-name* :*SIM-I)
(defparameter *lisp-i::*starlisp-compiler-package-name :*SIM-COMPILER)


