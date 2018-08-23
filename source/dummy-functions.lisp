;;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *SIM-I; MUSER: YES -*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.

(defun list-of-slc-function-types () nil)

(defun get-function-type (function-symbol) (get function-symbol 'slc::function-type))
(defun set-function-type (function-symbol type)
  (setf (get function-symbol 'slc::function-type) type)
  )
(defun remove-function-type (function-symbol)
  (setf (get function-symbol 'slc::function-type) nil)
  )

(defun get-variable-descriptor (variable) (get variable 'slc::descriptor))
(defun set-variable-descriptor (variable value)
  (setf (get variable 'slc::descriptor) value)
  )
(defun remove-variable-descriptor (variable)
  (setf (get variable 'slc::descriptor) nil)
  )

(defun make-pvar-for-compiler (&rest args &key type reference read-only)
  (declare (ignore args type reference read-only))
  nil
  )

(defun make-descriptor-for-compiler (&rest args)
  (declare (ignore args))
  nil
  )

(defun find-compiler-keyword (keyword)
  (declare (ignore keyword))
  (error "I need a function which tells me if keyword is a legal compiler option.")
  )

(defun deal-with-compiler-option-value (option value keyword)
  (declare (ignore option value keyword))
  nil
  )

(defun compiler-warn (&rest args)
  (declare (ignore args))
  (error "Internal error.  This should never be called inside the simulator.")
  )

(defun expt2 (x) (expt 2 x))
(defun expt2-1- (x) (expt 2 (1- x)))

(defun expt2-symbol () 'expt2)
(defun expt2-1-symbol () 'expt2-1-)


(defun simplify-expression (&rest args)
  (copy-list args)
  )

(defvar *pvar-length-error-message*
	"The ~S of a pvar has no meaning with respect to the *Lisp language itself.  ~@
       It only has meaning with respect to Paris, the CM Assembly language, and the ~@
       *Lisp Simulator knows nothing about Paris.  Pure *Lisp code cannot use ~S."
  )

(defun pvar-length (pvar)
  pvar
  (error *pvar-length-error-message* 'length 'pvar-length)
  )

(defun pvar-mantissa-length (pvar)
  pvar
  (error *pvar-length-error-message* 'mantissa-length 'pvar-mantissa-length)
  )

(defun pvar-exponent-length (pvar)
  pvar
  (error *pvar-length-error-message* 'exponent-length 'pvar-exponent-length)
  )

(defun starlisp-form-not-to-macroexpand (form)
  (declare (ignore form))
  nil
  )

(defun call-starlisp-compiler (form environment)
  (declare (ignore environment))
  form
  )

