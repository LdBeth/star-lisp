;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: (*SIM-I COMMON-LISP-GLOBAL); Base: 10; Muser: yes -*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


;;; (deftype pvar (&optional (element-type '*))
;;;   ;; I have to return a satisfies type with a closure so that typep can work.
;;;   ;; But, returning a closure will blow up both subtypep and the compiler on lucid.
;;;   (let ((closure (pvar-type-predicate (cadr (canonical-pvar-type `(pvar ,element-type))))))
;;;     `(satisfies ,closure)))
;;; 


(defun array-pvar-type-canonical-element-pvar-type (canonical-pvar-type)
  `(pvar ,(second (second canonical-pvar-type)))
  )


(defun structure-pvar-name (pvar) (pvar-structure-name pvar))


(defun *lisp-external-symbols-check ()

  (do-external-symbols (symbol (find-package '*sim))
    (when (not (member symbol *all-external-symbols* :test #'eq))
      (warn "The symbol ~S is an exported symbol of *Lisp but is not defined by def-starlisp" symbol)
      ))

  (dolist (symbol *all-external-symbols*)
    
    (let ((descriptor (read-from-string (get symbol '*lisp-descriptor))))

      (if (null descriptor)

	  (warn "Huh?  Symbol ~S on *all-external-symbols* does not have a *lisp descriptor" symbol)

	  (let ((type (find-value-for-keyword :type descriptor)))

	    (ecase type
	      (:function
		(cond
		  ((not (fboundp symbol))
		   (warn "The symbol ~S is supposed to be a function but has no function definition" symbol)
		   )
		  ((macro-function symbol)
		   (warn "The symbol ~S is supposed to be a function but is instead a macro" symbol)
		   )))
	      (:macro
		(cond
		  ((not (fboundp symbol))
		   (warn "The symbol ~S is supposed to be a macro but has no macro definition" symbol)
		   )
		  ((not (macro-function symbol))
		   (warn "The symbol ~S is supposed to be a macro but is instead a function" symbol)
		   )
		  ((fboundp (*SIM-i::make-*defun-function symbol))
		   (warn "The symbol ~S is supposed to be a macro but appears to be a *defun instead" symbol)
		   )))
	      (:*defun
		(cond
		  ((not (fboundp symbol))
		   (warn "The symbol ~S is supposed to be a *defun but has no macro definition" symbol)
		   )
		  ((not (macro-function symbol))
		   (warn "The symbol ~S is supposed to be a *defun but is instead a function" symbol)
		   )
		  ((not (fboundp (*SIM-i::make-*defun-function symbol)))
		   (warn "The symbol ~S is supposed to be a *defun but ~S has no function definition"
			 symbol (*SIM-i::make-*defun-function symbol)
			 ))))
	      ((:variable :compiler-variable)
	       (when (not (boundp symbol))
		 (warn "The symbol ~S is supposed to be a variable but it has no global binding" symbol)
		 ))
	      ((:deftype :declaration :other))
	      )

	    )))))
