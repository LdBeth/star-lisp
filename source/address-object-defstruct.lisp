;;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *SIM-i; MUSER: YES-*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


(eval-when (:compile-toplevel :load-toplevel :execute)
  #+*LISP-SIMULATOR
  (defparameter *max-cube-address-length* 28)
  (defconstant *maximum-number-of-dimensions* *max-cube-address-length*)
  )

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (*lisp::deftype max-cube-address-pvar-type () `(pvar (unsigned-byte #.*max-cube-address-length*)))
  (*lisp::deftype max-grid-address-pvar-type () `(pvar (signed-byte #.(1+ *max-cube-address-length*))))
  (*lisp::deftype grid-address-pvar-array-type (n) `(pvar (array (signed-byte #.(1+ *max-cube-address-length*)) (,n))))
  (*lisp::deftype geometry-id-pvar-type () `(pvar (unsigned-byte #.(integer-length (1- *maximum-geometries-allowed*)))))
  (*lisp::deftype geometry-rank-pvar-type () `(pvar (unsigned-byte #.(integer-length (1- *maximum-number-of-dimensions*)) )))
  
  )


(*defstruct address-object
  (geometry-id 0 :type fixnum :cm-type geometry-id-pvar-type :cm-uninitialized-p t)
  (cube-address 0 :type fixnum :cm-type max-cube-address-pvar-type :cm-uninitialized-p t)
  )

