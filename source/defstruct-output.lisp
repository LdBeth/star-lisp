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


;;;; Functions that generate code for *DEFSTRUCT macroexpansion.


(defun generate-front-end-defstruct-code (ds)
  `(defstruct
     (,(*defstruct-name ds)
      (:conc-name ,(*defstruct-slot-accessor-prefix-name ds))
      (:constructor ,(*defstruct-constructor-name ds))
      (:copier ,(*defstruct-copier-name ds))
      (:predicate ,(*defstruct-predicate-name ds))
      ,@(when (*defstruct-included-*defstruct-name ds)
	  `((:include ,(*defstruct-included-*defstruct-name ds)))
	  )
      ,@(when (*defstruct-print-function-name ds)
	  `((:print-function ,(*defstruct-print-function-name ds)))
	  )
      ,@(when (*defstruct-type ds)
	  `((:type ,(*defstruct-type ds)))
	  )
      )
     ,@(when (*defstruct-documentation ds)
	 (list (*defstruct-documentation ds))
	 )
     ,@(mapcar
	 #'(lambda (slot)
	     `(,(*defstruct-slot-name slot)
	       ,(*defstruct-slot-initial-value-form slot)
	       ,@(when (*defstruct-slot-type slot) `(:type #+SYMBOLICS ,(*defstruct-slot-type slot) #-SYMBOLICS t))
	       ))
	 (*defstruct-defined-slots-list ds)
	 )))

(defun make-canonical-structure-type (name)
  (make-canonical-pvar-type 'structure :name name)
  )


(defun check-accessor-argument-type (function-name argument structure-name)
  (safety-check
    (assert (and (structure-pvar-p argument) (included-*defstruct-p (pvar-structure-name argument) structure-name)) ()
	    "The argument to ~S, ~S, is not a ~S pvar"
	    function-name argument structure-name
	    )))


(defun generate-*defstruct-code (ds)

  `(
    ,(define-*defstruct-properties ds)
    ,(define-predicates-for-*defstruct ds)
    ,@(define-pvar-accessor-functions-for-slots ds)
    ,(define-make-function-for-*defstruct ds)
    ,(define-copier-function-for-*defstruct ds)
    ,(define-pref-function-for-*defstruct ds)
    ,(define-setf-pref-function-for-*defstruct ds)
    ,(define-!!-function-for-*defstruct ds)
    ,(define-array-to-pvar-function-for-*defstruct ds)
    )
  )

(defun structure-pvar-argname (ds)
  (modify-name (*defstruct-name ds) "" "-PVAR")
  )

(defun slot-accessor-function-name (ds slot)
  (if (*defstruct-slot-accessor-prefix-name ds)
      (modify-name (*defstruct-slot-name slot) (*defstruct-slot-accessor-prefix-name ds) "!!")
      (modify-name (*defstruct-slot-name slot) "" "!!")
      ))

(defun front-end-accessor-name (ds slot)
  (if (*defstruct-slot-accessor-prefix-name ds)
      (modify-name (*defstruct-slot-name slot) (*defstruct-slot-accessor-prefix-name ds) "")
      (*defstruct-slot-name slot)
      ))


(defun define-*defstruct-properties (ds)
  (let ((name (*defstruct-name ds)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',name '*defstruct-defined) t)
       (setf (get ',name '*defstruct-slot-accessors)
	     ',(mapcar #'(lambda (slot) (slot-accessor-function-name ds slot)) (*defstruct-all-slots-list ds)))
       (setf (get ',name '*defstruct-front-end-slot-accessors)
	     ',(mapcar #'(lambda (slot) (front-end-accessor-name ds slot)) (*defstruct-all-slots-list ds)))
       ,@(mapcar #'(lambda (slot)
		     `(*proclaim '(ftype (function (,name) ,(*defstruct-slot-type slot))
					 ,(front-end-accessor-name ds slot))))
		 (*defstruct-all-slots-list ds))
       (*proclaim '(ftype (function (&rest t) ,name) ,(*defstruct-constructor-name ds)))
       (*proclaim '(ftype (function (&rest t) ,name) ,(*defstruct-copier-name ds)))
       (*proclaim '(ftype (function (t) boolean) ,(*defstruct-predicate-name ds)))
       (deftype ,(modify-name name "" "-PVAR") () `(pvar ,',name)))))



(defun define-pvar-accessor-functions-for-slots (ds)
  
  (if (not (*defstruct-constant-size? ds))
      
      (error "One or more of the slots of the *Defstruct ~S is defined as a pvar.~@
              of indeterminate length.  Starlisp does not currently support this capability."
	     (*defstruct-name ds)
	     )
      
      (let ((prefix (*defstruct-slot-accessor-prefix-name ds)))
	(mapcan
	  #'(lambda (slot)
	      (let* ((slot-name (*defstruct-slot-name slot))
		     (slot-size (*defstruct-slot-length-in-bits slot))
		     (slot-type (*defstruct-slot-canonical-pvar-type slot))
		     (slot-offset (*defstruct-slot-offset slot))
		     (function-name (slot-accessor-function-name ds slot))
		     (aliasing-function-name (modify-name function-name "ALIAS!!-FOR-" ""))
		     (argument-name (structure-pvar-argname ds))
		     )
		slot-size
		(prog1
		  `(
		    (eval-when (:compile-toplevel :load-toplevel :execute)
		      (setf (get ',function-name '*defstruct-accessor) ',(*defstruct-name ds))
		      (setf (get ',function-name '*defstruct-alias!!-function) ',aliasing-function-name)
		      (setf (get ',function-name '*defstruct-offset) ',slot-offset)
		      (setf (get ',function-name '*defstruct-slot-type) ',slot-type)
		      )
		    (progn
		      
		      (defun ,aliasing-function-name (,argument-name)
			(if (void-pvar-p ,argument-name)
			    (handle-void-alias ,argument-name)
			    (progn
			      (check-accessor-argument-type ',aliasing-function-name ,argument-name ',(*defstruct-name ds))
			      (,(front-end-accessor-name ds slot) (pvar-structure ,argument-name))
			      )))
		      
		      (defun ,function-name (,argument-name)
			(simple-pvar-argument!! ,argument-name)
			(*let (pvar)
			  (declare (type ,slot-type pvar))
			  (setf (pvar-name pvar) ',(modify-name slot-name prefix "!!-RESULT"))
			  (if (void-pvar-p ,argument-name)
			      (*copy-pvar pvar ,argument-name)
			      (*copy-pvar pvar (,(front-end-accessor-name ds slot) (pvar-structure ,argument-name)))
			      )
			  pvar
			  ))
		      
		      )))))

	  (*defstruct-all-slots-list ds)
	  
	  ))))


(defun define-make-function-for-*defstruct (ds)
  (flet
    ((provided-variable-name (argname) (modify-name argname "" "-PROVIDED?")))
    (let* ((function-name (*defstruct-cm-constructor-name ds))
	   (return-pvar-name (modify-name function-name "" "-RESULT"))
	   )
      (when function-name
	`(progn
	   (eval-when (:compile-toplevel :load-toplevel :execute)
	     (setf (get ',(*defstruct-name ds) '*defstruct-make-function) ',function-name)
	     )
	   (defun ,function-name
		  (&key
		   ,@(mapcar
		       #'(lambda (slot)
			   `(,(*defstruct-slot-name slot)
			     ,(if (or (*defstruct-slot-cm-uninitialized? slot)
				      (*defstruct-cm-uninitialized-p ds)
				      )
				  nil
				  (*defstruct-slot-cm-initial-value-form slot)
				  )
			     ,(provided-variable-name (*defstruct-slot-name slot))
			     ))
		       (*defstruct-all-slots-list ds)
		       ))
	     ,(let (front-end-slots other-slots)
		(dolist (slot (*defstruct-all-slots-list ds))
		  (if (equal (*defstruct-slot-canonical-pvar-type slot) '(pvar front-end))
		      (push (*defstruct-slot-name slot) front-end-slots)
		      (push (*defstruct-slot-name slot) other-slots)))
		`(progn
		   (pvar-argument!! &front-end ,front-end-slots legal-pvar-value)
		   (pvar-argument!! ,other-slots legal-pvar-value)))
	     (*nocompile
	       (let ((return-pvar
		       (make-structure-pvar :stack '(pvar (structure ,(*defstruct-name ds))))
		       ))
		 (setf (pvar-name return-pvar) ',return-pvar-name)
		 ,@(mapcar
		     #'(lambda (slot)
			 (let ((slot-function-name (slot-accessor-function-name ds slot)))
			   `(when (or ,(provided-variable-name (*defstruct-slot-name slot))
				      ,(*defstruct-slot-name slot)
				      )
			      (*setf (,slot-function-name return-pvar) ,(*defstruct-slot-name slot))
			      )))
		     (*defstruct-all-slots-list ds)
		     )
		 return-pvar
		 ))))))))


(defun define-copier-function-for-*defstruct (ds)
  (let* ((function-name (*defstruct-cm-copier-name ds))
	 (argument-name (structure-pvar-argname ds))
	 (return-pvar-name (modify-name function-name "" "-RESULT"))
	)
    (when function-name
      (let ((canonical-type (make-canonical-structure-type (*defstruct-name ds))))
      `(defun ,function-name (,argument-name)
	 ()
	 (simple-pvar-argument!! ,argument-name)
	 (*let (,return-pvar-name)
	   (declare (type ,canonical-type ,return-pvar-name))
	   (declare (return-pvar-p t))
	   (*compile-blindly
	     (*set ,return-pvar-name (the ,canonical-type ,argument-name))
	     )
	   ,return-pvar-name
	   ))))))
      

;; does type1 structures include type2 structures?

(defun included-*defstruct-p (type1 type2)
  (or (eq type1 type2)
      (included-*defstruct-p
	(or (structure-pvar-type-included-*defstruct-name type1)
	    (return-from included-*defstruct-p nil))
	type2)))


(defun define-predicates-for-*defstruct (ds)

  (let ((global-function-name (*defstruct-global-cm-predicate-name ds))
	(parallel-function-name (*defstruct-parallel-cm-predicate-name ds))
	)

    `(progn

       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (get ',(*defstruct-name ds) '*defstruct-global-predicate-function) ',global-function-name)
	 (setf (get ',(*defstruct-name ds) '*defstruct-parallel-predicate-function) ',parallel-function-name)
	 )

       ,@(when global-function-name
	   `(
	     (defun ,global-function-name (pvar)
	       (simple-pvar-argument!! pvar)
	       (and (eq :structure (pvar-type pvar)) (included-*defstruct-p (pvar-structure-name pvar) ',(*defstruct-name ds)))
	       )
	     ))

       ,@(when parallel-function-name
	   `(
	     (defun ,parallel-function-name (pvar)
	       (!! (,global-function-name pvar))
	       )
	     )))))

(defun define-pref-function-for-*defstruct (ds)
  (if (null (*defstruct-constructor-name ds))
      (warn "Can't make a PREF function for *DEFSTRUCT ~S because you told me that no constructor function was to be created"
	    (*defstruct-name ds))
      (let ((function-name (modify-name (*defstruct-name ds) "PREF-" "")))
	`(progn
	   (eval-when (:compile-toplevel :load-toplevel :execute)
	     (setf (get ',(*defstruct-name ds) '*defstruct-pref-function) ',function-name)
	     )
	   (defun ,function-name (,(structure-pvar-argname ds) processor)
	     (*locally
	       (declare (type (pvar ,(*defstruct-name ds)) ,(structure-pvar-argname ds)))
	       (let ((result (,(*defstruct-constructor-name ds))))
		 (*compile (:safety 0 :warning-level :normal)
		   ,@(mapcar #'(lambda (slot)
				 `(setf (,(front-end-accessor-name ds slot) result)
					(pref (alias!! (,(slot-accessor-function-name ds slot) ,(structure-pvar-argname ds)))
					      processor)))
			     (*defstruct-all-slots-list ds)))
		 result)))))))

	  
(defun define-setf-pref-function-for-*defstruct (ds)
  (let ((function-name (modify-name (*defstruct-name ds) "SETF-PREF-" ""))
	(canonical-type (make-canonical-structure-type (*defstruct-name ds)))
	)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (get ',(*defstruct-name ds) '*defstruct-setf-pref-function) ',function-name)
	 )
       (defun ,function-name (,(structure-pvar-argname ds) processor value)
	 (*locally
	   (declare (type ,canonical-type ,(structure-pvar-argname ds)))
	   (assert (typep value ',(*defstruct-name ds)))
	   (*compile (:safety 0 :warning-level :normal)
	     (*all
	       (declare (return-pvar-p nil))
	       (*when (=!! (self-address!!) (!! (the fixnum processor)))
		 (*set ,(structure-pvar-argname ds) (the (pvar ,(*defstruct-name ds)) (!! value))))))
	   value)))))


(defun define-!!-function-for-*defstruct (ds)
  (let* ((function-name (modify-name (*defstruct-name ds) "!!-" ""))
	 (return-pvar-name (modify-name function-name "" "-RESULT"))
	 (canonical-type (make-canonical-structure-type (*defstruct-name ds)))
	 )
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (get ',(*defstruct-name ds) '*defstruct-!!-function) ',function-name)
	 )
       (defun ,function-name (value)
	 (*let (return-pvar)
	   (declare (type ,canonical-type return-pvar))
	   (declare (return-pvar-p t))
	   (setf (pvar-name return-pvar) ',return-pvar-name)
	   (*compile (:safety 0 :warning-level :normal)
		     ,@(mapcar
			 #'(lambda (slot)
			     (if (*defstruct-slot-type slot)
				 `(*setf (,(slot-accessor-function-name ds slot) return-pvar)
					 ,(if (equal (*defstruct-slot-canonical-pvar-type slot) '(pvar front-end))
					      `(front-end!! (,(front-end-accessor-name ds slot) value))
					      `(!! (,(front-end-accessor-name ds slot) value))))
				 `(*nocompile
				    (*setf (,(slot-accessor-function-name ds slot) return-pvar)
					 ,(if (equal (*defstruct-slot-canonical-pvar-type slot) '(pvar front-end))
					      `(front-end!! (,(front-end-accessor-name ds slot) value))
					      `(!! (,(front-end-accessor-name ds slot) value)))))))
			 (*defstruct-all-slots-list ds)))
	   return-pvar)))))



(defun define-array-to-pvar-function-for-*defstruct (ds)
  (let* ((function-name (modify-name (string (*defstruct-name ds)) "ARRAY-TO-" "-PVAR")))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (get ',(*defstruct-name ds) '*defstruct-array-to-pvar-function) ',function-name))
       (defun ,function-name (source-array dest-pvar array-offset start end 1-d-source-array 1-d-slot-array n-d-slot-array)
	 ,@(mapcar
	     #'(lambda (slot)
		 ;; We don't want to bother figuring out exactly which elements of the array
		 ;; are going to be moved into the CM.
		 `(progn
		    (dotimes (array-index (length 1-d-slot-array))
		      (setf (aref 1-d-slot-array array-index)
			    (if (,(*defstruct-predicate-name ds) (aref 1-d-source-array array-index))
				(,(front-end-accessor-name ds slot) (aref source-array array-index))
				'you-are-trying-to-write-something-not-of-the-structure-type)))
		    ;; If array-offset, start and end are lists then
		    ;; we must have called array-to-pvar-grid to begin with,
		    ;; otherwise array-to-pvar.
		    (if (listp array-offset)
			(array-to-pvar-grid
			  n-d-slot-array
			  (alias!! (,(slot-accessor-function-name ds slot) dest-pvar))
			  :array-offset array-offset
			  :grid-start start
			  :grid-end end)
			(array-to-pvar
			  1-d-slot-array
			  (alias!! (,(slot-accessor-function-name ds slot) dest-pvar))
			  :array-offset array-offset
			  :cube-address-start start
			  :cube-address-end  end))))
	     (*defstruct-all-slots-list ds))
	 dest-pvar))))


;;; (defun copy-structure!! (structure-pvar)
;;;   (if (general-pvarp structure-pvar)
;;;       (*let ((result structure-pvar)) result)
;;;       ;; What is pvar-canonical-pvar-type supposed to do? jwm
;;;       (funcall (structure-pvar-type-copier-name (funcall 'pvar-canonical-pvar-type structure-pvar)) structure-pvar)
;;;       ))
