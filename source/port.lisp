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

;;;
;;; This file contains code which is implementation dependent.
;;; Aside from declarations, there should be no implementation dependent
;;; code anywhere in the Simulator besides here and in the
;;; specifications.lisp and the definitions.lisp and pref-setf.lisp files. (Ha!)
;;;

;;; Lispm's go very fast when vectors are declared as
;;; (sys:array-register ...).

(defmacro with-simple-vectors ((&rest vector-args) &body body)
  `(locally
     (declare (type simple-vector ,@vector-args))
     ,@body
     ))

(defmacro with-bit-vectors ((&rest vector-args) &body body)
  `(locally
     (declare (type bit-vector ,@vector-args))
     ,@body
     ))

(defmacro 1-d-array-declaration (&rest body)
  #+SYMBOLICS
  `(declare (sys:array-register ,@body))
  #-SYMBOLICS
  `(declare (type simple-vector ,@body))
  )

(defmacro bit-vector-array-declaration (&rest body)
  #+SYMBOLICS
  `(declare (sys:array-register ,@body))
  #-SYMBOLICS
  `(declare (type bit-vector ,@body))
  )

(defmacro declare-arglist (argument-list)
  #+SYMBOLICS
  `(declare (sys:arglist ,@argument-list))
  #+LUCID
  `(declare (sys::arglist ,argument-list))
  #-(OR SYMBOLICS LUCID)
  (declare (ignore argument-list))
  #-(OR SYMBOLICS LUCID)
  nil
  )

 
;;;
;;; This macro will iterate over the BODY with SYMBOL bound to selected processors.
;;;

(defmacro do-for-selected-processors ((symbol) &body body)
  (let ((css-symbol (gensym)))
    `(let ((,css-symbol *css*))
       (declare (type (vector bit) ,css-symbol))
       (dotimes (,symbol *number-of-processors-limit*)
         (declare (type fixnum ,symbol))
         (when (eql 1 (sbit ,css-symbol ,symbol))
           ,@body
           )))))

 

(defmacro with-all-errors-trapped (protected-form on-error-form)
  
  "Evaluates protected-form and returns the result of that evaluation
   (the form must return exactly one result).  If an error is signalled,
   however, on-error-form is evaluated and the result of evaluating that
   form is returned.
  "

  `(handler-case ,protected-form
     (error () ,on-error-form)
     ))
 
;;; Allow C-SH-A to work for *DEFUN functions on Lisp Machines
;;; and Lucid using TMC Gmacs hacks


(defun default-arglist-declaration-from-arglist (arglist)
  "Return a declaration form for the arg list of this function"
  #+SYMBOLICS
  `(declare (si:arglist ,@arglist))
  #+LCL3.0
  `(declare (sys::arglist ,arglist))
  #-(OR SYMBOLICS LCL3.0)
  (declare (ignore arglist))
  #-(OR SYMBOLICS LCL3.0)
  nil
  )

(defun hack-declarations-for-symbolics (declarations arglist)
  #-(OR SYMBOLICS LCL3.0)
  (declare (ignore arglist))
  #-(OR SYMBOLICS LCL3.0)
  declarations
  #+SYMBOLICS
  (append declarations
	  (and (not (find 'si:arglist declarations :key #'caadr))
	       (list (default-arglist-declaration-from-arglist arglist))
	       ))
  #+LCL3.0
  (append declarations
	  (and (not (find 'sys::arglist declarations :key #'caadr))
	       (list (default-arglist-declaration-from-arglist arglist))
	       ))
 )

(defmacro argument-list-declaration (&rest arglist)
  #+SYMBOLICS
  `(declare (zl:arglist ,@arglist))
  #+LCL3.0
  `(declare (sys::arglist ,arglist))
  #-(OR SYMBOLICS LCL3.0)
  (declare (ignore arglist))
  #-(OR SYMBOLICS LCL3.0)
  nil
 )


