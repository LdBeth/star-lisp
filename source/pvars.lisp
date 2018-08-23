;;; -*- Mode: LISP;  Syntax: COMMON-LISP; Package: (*SIM-I COMMON-LISP-GLOBAL); Base: 10; Muser: yes -*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


;;;; The Simulator's representation of a pvar.

;;; In some CLs, nasty circularities arise because Internal-Pvarp gets declared
;;; inline as being (Typep Foo 'Pvar), which gets optimized into a call to
;;; Internal-Pvarp thanks to the Pvar Deftype.  So we define Internal-Pvarp
;;; differently in this case.

 
#+:cormanlisp
(defstruct (pvar-struct (:predicate %internal-pvarp) 
                        (:print-function print-pvar)
                        (:conc-name pvar-)
                        (:constructor make-pvar))
  name
  location
  class
  canonical-pvar-type
  lvalue?
  constant?
  vp-set
  (plist nil)
  other-1
  other-2
  other-3
  other-4
  )

#+:cormanlisp
(defun internal-pvarp (x) 
  (and (common-lisp::structurep x) 
       (eq 'pvar-struct (common-lisp::struct-type x))))


#-:cormanlisp
(defstruct (pvar-struct (:predicate internal-pvarp) 
                        (:print-function print-pvar)
                        (:conc-name pvar-)
                        (:constructor make-pvar))
  name
  location
  class
  canonical-pvar-type
  lvalue?
  constant?
  vp-set
  (plist nil)
  other-1
  other-2
  other-3
  other-4
  )

(defun pvar-p (x) (internal-pvarp x))

(defun copy-pvar-slots (dest source)
  (setf (pvar-name dest) (pvar-name source))
  (setf (pvar-location dest) (pvar-location source))
  (setf (pvar-class dest) (pvar-class source))
  (setf (pvar-canonical-pvar-type dest) (pvar-canonical-pvar-type source))
  (setf (pvar-lvalue? dest) (pvar-lvalue? source))
  (setf (pvar-constant? dest) (pvar-constant? source))
  (setf (pvar-vp-set dest) (pvar-vp-set source))
  (setf (pvar-plist dest) (pvar-plist source))
  (setf (pvar-other-1 dest) (pvar-other-1 source))
  (setf (pvar-other-2 dest) (pvar-other-2 source))
  (setf (pvar-other-3 dest) (pvar-other-3 source))
  (setf (pvar-other-4 dest) (pvar-other-4 source))
  )

(defun is-pvar (x) (pvar-p x))

(defun pvar-type (pvar) (pvar-class pvar))
(defun pvar-data (pvar) (pvar-location pvar))

(defsetf pvar-type (pvar) (value) `(setf (pvar-class ,pvar) ,value))
(defsetf pvar-data (pvar) (value) `(setf (pvar-location ,pvar) ,value))

(defun pvar-array (pvar) (pvar-location pvar))
(defun pvar-array-dimensions (array-pvar) (pvar-other-1 array-pvar))
(defun pvar-array-canonical-element-type (array-pvar) (pvar-other-2 array-pvar))
(defun pvar-array-element-type (array-pvar) (pvar-other-2 array-pvar))
(defun pvar-array-displaced-array (array-pvar) (pvar-other-3 array-pvar))
(defun pvar-sideways-p (array-pvar) (pvar-other-4 array-pvar))

(defsetf pvar-array (pvar) (value) `(setf (pvar-location ,pvar) ,value))
(defsetf pvar-array-dimensions (pvar) (value) `(setf (pvar-other-1 ,pvar) ,value))
(defsetf pvar-array-canonical-element-type (pvar) (value) `(setf (pvar-other-2 ,pvar) ,value))
(defsetf pvar-array-element-type (pvar) (value) `(setf (pvar-other-2 ,pvar) ,value))
(defsetf pvar-array-displaced-array (pvar) (value) `(setf (pvar-other-3 ,pvar) ,value))
(defsetf pvar-sideways-p (pvar) (value) `(setf (pvar-other-4 ,pvar) ,value))

(defun pvar-structure (pvar) (pvar-location pvar))
(defun pvar-structure-name (pvar) (pvar-other-1 pvar))

(defun pvar-address-object-geometry-id (pvar) (pvar-other-2 pvar))

(defsetf pvar-structure (pvar) (value) `(setf (pvar-location ,pvar) ,value))
(defsetf pvar-structure-name (pvar) (value) `(setf (pvar-other-1 ,pvar) ,value))
(defsetf pvar-address-object-geometry-id (pvar) (value) `(setf (pvar-other-2 ,pvar) ,value))

(defun void-pvar-p (pvar) (eq :void (pvar-other-1 pvar)))
(defun make-void (pvar) (setf (pvar-other-1 pvar) :void))
(defun make-non-void (pvar) (setf (pvar-other-1 pvar) nil))

;(defun general-pvar-array-list (pvar) (pvar-other-1 pvar))
;(defun general-pvar-structure-list (pvar) (pvar-other-2 pvar))
;(defsetf general-pvar-array-list (pvar) (value) `(setf (pvar-other-1 ,pvar) ,value))
;(defsetf general-pvar-structure-list (pvar) (value) `(setf (pvar-other-2 ,pvar) ,value))

(defun pvar-constant-value (pvar) (pvar-plist pvar))
(defsetf pvar-constant-value (pvar) (value) `(setf (pvar-plist ,pvar) ,value))

(defun clear-non-essential-pvar-slots (pvar)
  (setf (pvar-plist pvar) nil)
  (setf (pvar-other-1 pvar) nil)
  (setf (pvar-other-2 pvar) nil)
  (setf (pvar-other-3 pvar) nil)
  (setf (pvar-other-4 pvar) nil)
  )
	
(defun general-pvar-class-label () :general)
(defun array-pvar-class-label () :array)
(defun structure-pvar-class-label () :structure)
(defun general-pvar-p (pvar) (eq (general-pvar-class-label) (pvar-class pvar)))
(defun array-pvar-p (pvar) (eq (array-pvar-class-label) (pvar-class pvar)))
(defun structure-pvar-p (pvar) (eq (structure-pvar-class-label) (pvar-class pvar)))
;(defun general-pvar-without-arrays-p (pvar)
;  (and (general-pvar-p pvar) (null (general-pvar-array-list pvar)))
;  )
;(defun general-pvar-without-structures-p (pvar)
;  (and (general-pvar-p pvar) (null (general-pvar-structure-list pvar)))
;  )
(defun simple-general-pvar-p (pvar)
  (and (general-pvar-p pvar)) ; (null (general-pvar-array-list pvar)) (null (general-pvar-structure-list pvar)))
  )
(defun scalar-pvar-p (pvar) (simple-general-pvar-p pvar))
(defun non-scalar-pvar-p (pvar) (or (array-pvar-p pvar) (structure-pvar-p pvar)))


(defun canonical-pvar-type-from-pvar (pvar) (pvar-canonical-pvar-type pvar))

(defun describe-pvar (pvar &optional (stream nil))
  
  (terpri stream)
  (let ((stream (if stream stream *standard-output*)))
    (terpri stream)
    (progn
      (format stream "Pvar name: ~S~%" (pvar-name pvar))
      (format stream "Lvalue?:   ~S~%" (pvar-lvalue? pvar))
      (format stream "Constant?: ~S~%" (pvar-constant? pvar))
      (format stream "Class:     ~S~%" (pvar-class pvar))
      (format stream "Canonical-pvar-type: ~S~%" (pvar-canonical-pvar-type pvar))
      (format stream "Vp Set:    ~S~%" (pvar-vp-set pvar))
      (when (pvar-constant-value pvar)
	(format stream "Constant value: ~S~%" (pvar-constant-value pvar))
	)
      (when (void-pvar-p pvar)
	(format stream "*** This pvar has not been initialized ***~%")
	)
      (when (array-pvar-p pvar)
	(format stream "Array dimensions: ~S~%" (pvar-array-dimensions pvar))
	(format stream "Canonical element pvar type: ~S~%" (pvar-array-canonical-element-type pvar))
	)
      (terpri stream)
      )))

(defun new-pvar-check (pvar function-name)
  (assert (is-pvar pvar) () "The object ~S, an argument to ~S, is not a pvar" pvar function-name)
  (assert (eq (pvar-vp-set pvar) *current-vp-set*) ()
	  "The pvar ~S,~% used in function ~S,~% does not belong to the current vp set, ~S"
	  pvar function-name *current-vp-set*
	  ))

(defun new-vp-pvar-check (pvar function-name) (new-pvar-check pvar function-name))

(defun new-pvar-check-no-vp-check (pvar function-name)
  (assert (is-pvar pvar) () "The object ~S, an argument to ~S, is not a pvar" pvar function-name)
  )

(defun new-pvar-check-lvalue (pvar function-name)
  (new-pvar-check pvar function-name)
  (unless (pvar-lvalue? pvar)
    (error "~S:  A destination pvar, ~S, is an expression, not an lvalue.~@
            You are probably trying to modify a temporary pvar.  Perhaps you want to use ALIAS!!."
	   function-name pvar
	   ))
  (when (pvar-constant? pvar)
    (error "Internal error in ~S.  Pvar ~S is both a constant and an lvalue ??" function-name pvar)
    ))

(defun new-pvar-check-lvalue-no-vp-check (pvar function-name)
  (assert (is-pvar pvar) () "The object ~S, an argument to ~S, is not a pvar" pvar function-name)
  (unless (pvar-lvalue? pvar)
    (error "~S:  A destination pvar, ~S, is an expression, not an lvalue." function-name pvar)
    )
  (when (pvar-constant? pvar)
    (error "Internal error in ~S.  Pvar ~S is both a constant and an lvalue ??" function-name pvar)
    ))


(defun new-two-pvar-check (pvar1 pvar2 function-name)
  (new-pvar-check pvar1 function-name)
  (new-pvar-check pvar2 function-name)
  )

(defun new-multiple-pvar-check (pvars function-name)
  (mapc
    #'(lambda (pvar) (new-pvar-check pvar function-name))
    pvars
    ))

(defun pvar-check-lvalue (pvar function-name)

  ;; make sure a pvar can be legitimately written to.

  (pvar-check pvar)
  (unless (pvar-lvalue? pvar)
    (error "~S:  Destination pvar is an expression, not an lvalue." function-name)
    )
  (when (pvar-constant? pvar)
    (error "Internal error in ~S.  Pvar ~S is both a constant and an lvalue ??" function-name pvar)
    )
  )

(defun portable-pvar-array-dimensions (pvar)
  (pvar-array-dimensions pvar)
  )

(defun portable-pvar-structure-name (pvar)
  (pvar-structure-name pvar)
  )

(defun portable-pvar-array-element-type (pvar)
  (pvar-array-canonical-element-type pvar)
  )

;;; The following functions are stubs for the real accessor
;;; functions into a PVAR structure.


(defun array-pvar-canonical-element-type (pvar-array)
  "Lowest level accessor function for this information."
  (pvar-array-canonical-element-type pvar-array)
  )


(defun array-pvar-dimensions (pvar-array)
  "Lowest level accessor function for this information"
  (pvar-array-dimensions pvar-array)
  )

(defun array-pvar-dimension (pvar-array dimension)
  "Lowest level accessor function for this information"
  (nth dimension (array-pvar-dimensions pvar-array))
  )

(defun array-pvar-rank (pvar-array)
  (length (array-pvar-dimensions pvar-array))
  )

(defun array-pvar-total-size (pvar-array)
  "Lowest level accessor function for this information"
  (apply #'* (array-pvar-dimensions pvar-array))
  )

(defun array-pvar-row-major-index (pvar-array &rest subscripts)
  (apply #'array-row-major-index (pvar-array pvar-array) subscripts)
  )


