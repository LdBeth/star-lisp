;;; -*- Mode: LISP; Syntax: Common-lisp; Package: (*SIM-I COMMON-LISP-GLOBAL); Base: 10 -*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


;;;; *****     WARNING WARNING WARNING WARNING WARNING WARNING      *****
;;;;
;;;; This code is shared between the Starlisp Interpreter and the
;;;; Starlisp Simulator.  DO NOT MAKE CHANGES IN THIS CODE UNLESS
;;;; YOU ARE ABSOLUTELY SURE THE CHANGES APPLY EQUALLY TO BOTH
;;;; SYSTEMS OR YOU ARE VERY CAREFUL TO CONDITIONALLY COMPILE!
;;;; VIOLATE THIS WARNING AT YOUR OWN RISK!
;;;;
;;;; *****     WARNING WARNING WARNING WARNING WARNING WARNING      *****


;;;; Interface functions for rest of Starlisp implementation


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *all-defstructs* nil "A list of the names of all *defstructs")
  )

(defun get-*defstruct (*defstruct-name) (get *defstruct-name '*defstruct-structure))

(defun set-*defstruct (*defstruct-name *defstruct-structure)
  (prog1
    (setf (get *defstruct-name '*defstruct-structure) *defstruct-structure)
    (pushnew *defstruct-name *all-defstructs*)
    ))


;;; This stores all the information about a *DEFSTRUCT obtained
;;; from parsing the *DEFSTRUCT form.  Other information is
;;; put on the property list of the *DEFSTRUCT name and the
;;; propertly list of each *DEFSTRUCT slot name.

;;; If you make a change to this structure, make sure you go
;;; through this file and make other changes where appropriate!!!!

(defstruct (*defstruct (:print-function *defstruct-printer))
  (original-form nil)
  (name nil)
  (constant-size? nil)
  (total-length-in-bits nil)
  (parallel-cm-predicate-name nil)
  (global-cm-predicate-name nil)
  (predicate-name nil)
  (copier-name nil)
  (cm-copier-name nil)
  (constructor-name nil)
  (cm-constructor-name nil)
  (slot-accessor-prefix-name nil)
  (included-*defstruct-name nil)
  (options-list nil)
  (all-slots-list nil)
  (defined-slots-list nil)
  (included-slots-list nil)
  (print-function-name nil)
  (type nil)
  (defstruct-form nil)
  (cm-uninitialized-p nil)
  (documentation nil)
  spare1
  spare2
  )

(defparameter *print-verbose* nil)

(defun *defstruct-printer (ds stream prindepth)
  (declare (ignore prindepth))
  (if *print-verbose*
      (format stream
	      "~%#<*DEFSTRUCT ~S,~%  Constant-size?: ~S, Total-length-in-bits: ~S~%~
             Parallel-cm-predicate-name: ~S, predicate-name: ~S~%~
             CM-copier-name: ~S, copier-name: ~S~%  CM-constructor-name: ~S, constructor-name: ~S~%~
             Slot-accessor-prefix-name: ~S, Included-*defstruct-name: ~S~%~
             print-function-name: ~S, type: ~S, cm-uninitialized-p: ~S~% >~%"
	      (*defstruct-name ds)
	      (*defstruct-constant-size? ds)
	      (*defstruct-total-length-in-bits ds)
	      (*defstruct-parallel-cm-predicate-name ds)
	      (*defstruct-predicate-name ds)
	      (*defstruct-cm-copier-name ds)
	      (*defstruct-copier-name ds)
	      (*defstruct-cm-constructor-name ds)
	      (*defstruct-constructor-name ds)
	      (*defstruct-slot-accessor-prefix-name ds)
	      (*defstruct-included-*defstruct-name ds)
	      (*defstruct-print-function-name ds)
	      (*defstruct-type ds)
	      (*defstruct-cm-uninitialized-p ds))
      (format stream "#<*defstruct ~S>" (*defstruct-name ds))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun modify-name (name prefix suffix)
    (intern (concatenate 'string (string prefix) (string name) (string suffix)))
    ))


(defun type-name (type)
  ;; Takes either a structure name or a canonical
  ;; pvar type and returns the structure name.
  (cond
    ((symbolp type) type)
    ((and (listp type) (eq (first type) 'pvar)) (structure-pvar-type-name type))
    (t (error "Internal error:  Unknown structure type: ~S" type))
    ))

  
;;;; To find out if a symbol is a *defstruct slot accessor
;;;; call the function *defstruct-slot-accessor-p.

;;;; To find out the offset in bits of an accessor
;;;; call the function *defstruct-slot-bit-offset

;;;; To find the type of an accessor call the function
;;;; *defstruct-slot-pvar-type.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter
    **defstruct-slot-accessor-properties*
    '(
      (*defstruct-slot-accessor-p *defstruct-accessor)
      #-*lisp-hardware
      (*defstruct-slot-alias!!-function *defstruct-alias!!-function)
      (*defstruct-slot-bit-offset *defstruct-offset)
      (*defstruct-slot-pvar-type *defstruct-slot-type)
      )
    "Data used to create interface functions which, given a possible slot accessor name,
     provide the required information.
    "
    ))


(defmacro create-interface-functions-for-*defstruct-slots ()
  (let ((properties nil))
    `(progn
       ,@(mapcar
	   #'(lambda (descriptor)
	       (let ((function-name (first descriptor)) (property (second descriptor)))
		 (push property properties)
		 `(defun ,function-name (symbol) (get symbol ',property))
		 ))
	   **defstruct-slot-accessor-properties*
	   )
       (defun delete-*defstruct-slot-properties (symbol)
	 (mapc #'(lambda (x) (remprop symbol x)) '(,@properties))
	 ))))

(create-interface-functions-for-*defstruct-slots)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter
    **defstruct-properties*
    '(
      (*defstruct-structure :PROPERTY structure)
      (known-*defstruct-type :PROPERTY defined)
      (!!-function :PROPERTY !!-function)
      (make-function :SLOT cm-constructor-name)
      (front-end-make-function :SLOT constructor-name)
      #-*lisp-hardware
      (pref-function :PROPERTY pref-function)
      #-*lisp-hardware
      (setf-pref-function :PROPERTY setf-pref-function)
      (global-predicate-function :SLOT global-cm-predicate-name)
      (parallel-predicate-function :SLOT parallel-cm-predicate-name)
      (slot-accessors :PROPERTY slot-accessors)
      (front-end-slot-accessors :PROPERTY front-end-slot-accessors)
      (constant-size? :SLOT constant-size?)
      (total-length-in-bits :SLOT total-length-in-bits)
      (front-end-copier-name :SLOT copier-name)
      (copier-name :SLOT cm-copier-name)
      (included-*defstruct-name :SLOT included-*defstruct-name)
      (front-end-print-function :SLOT print-function-name)
      (slot-accessor-prefix-name :SLOT slot-accessor-prefix-name)
      (cm-uninitialized-p :SLOT cm-uninitialized-p)
      )
    "Data used to create interface functions which, given a name,
     canonical pvar type, or a pvar, return the appropriate
     information.  For example, for the datum (!!-function :PROPERTY !!-function)
     the functions structure-pvar-type-!!-function and
     structure-pvar-!!-function are created.
    "
    ))

(defmacro create-interface-functions-for-*defstruct ()
  (let ((properties nil))
    `(progn
       ,@(mapcar
	   #'(lambda (descriptor)
	       (let ((name (first descriptor))
		     (structure-slot-or-property? (second descriptor))
		     (slot-or-property-name (third descriptor))
		     )
		 `(progn
		    (defun ,(modify-name name "STRUCTURE-PVAR-TYPE-" "") (type)
		      ,(ecase structure-slot-or-property?
			 ((:SLOT)
			  `(,(modify-name slot-or-property-name "*DEFSTRUCT-" "")
			    (structure-pvar-type-*defstruct-structure type))
			  )
			 ((:PROPERTY)
			  (let ((property-name (modify-name slot-or-property-name "*DEFSTRUCT-" "")))
			    (push property-name properties)
			    `(get (type-name type) ',property-name)
			    ))))
		    (defun ,(modify-name name "STRUCTURE-PVAR-" "") (pvar)
		      (,(modify-name name "STRUCTURE-PVAR-TYPE-" "")
		       (canonical-pvar-type-from-pvar pvar)
		       )))))
	   **defstruct-properties*
	   )
       (defun delete-*defstruct-properties (name)
	 (when (structure-pvar-type-*defstruct-structure name)
	   (mapc 'delete-*defstruct-slot-properties (structure-pvar-type-slot-accessors name))
	   )
	 (mapc #'(lambda (x) (remprop name x)) '(,@properties))
	 )
       )))

(create-interface-functions-for-*defstruct)

(defun front-end-slot-name-from-cm-slot-name (cm-slot-name)
  (let ((type-name (*defstruct-slot-accessor-p cm-slot-name)))
    (when (null type-name)
      (error "Internal error:  No *DEFSTRUCT type known for supposed accessor function ~S" cm-slot-name)
      )
    (let* ((cm-slots-list (structure-pvar-type-slot-accessors type-name))
	   (front-end-slots-list (structure-pvar-type-front-end-slot-accessors type-name))
	   (slot-position (position cm-slot-name cm-slots-list))
	   )
      (when (null slot-position)
	(error "Internal error:  No DEFSTRUCT accessor found for supposed *DEFSTRUCT accessor ~S" cm-slot-name)
	)
      (nth slot-position front-end-slots-list)
      )))

