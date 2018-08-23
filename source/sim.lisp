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


;;;
;;; This will copy the contents of a pvar into another pvar conditionally
;;;

(defun *copy-pvar (dest-pvar source-pvar)
  
  (simple-pvar-argument!! dest-pvar source-pvar)
  
  (safety-check (new-pvar-check source-pvar '*set))
  (safety-check (new-pvar-check-lvalue dest-pvar '*set))
  
  (let ((dest-is-general? (general-pvar-p dest-pvar))
        (source-is-general? (general-pvar-p source-pvar))
        )
    
    ;; The simple case.  Both pvars are general pvars
    ;; and neither contain any arrays or structures.
    
    (when (and dest-is-general? source-is-general?)
      ;      (when (and (null (general-pvar-array-list dest-pvar))
      ;		 (null (general-pvar-structure-list dest-pvar))
      ;		 (null (general-pvar-array-list source-pvar))
      ;		 (null (general-pvar-structure-list source-pvar))
      ;		 )
      (*copy-simple-general-pvar dest-pvar source-pvar)
      (return-from *copy-pvar nil)
      )
    
    (let ((dest-is-array? (array-pvar-p dest-pvar))
          (dest-is-structure? (structure-pvar-p dest-pvar))
          (source-is-array? (array-pvar-p source-pvar))
          (source-is-structure? (structure-pvar-p source-pvar))
          )
      
      (cond
       
       ((and dest-is-array? source-is-array?) (*copy-array-pvar dest-pvar source-pvar))
       
       ((and dest-is-structure? source-is-structure?) (*copy-structure-pvar dest-pvar source-pvar))
       
       ((and dest-is-general? source-is-array?) (*copy-array-pvar-into-general-pvar dest-pvar source-pvar))
       
       ((and dest-is-general? source-is-structure?) (*copy-structure-pvar-into-general-pvar dest-pvar source-pvar))
       
       ((and dest-is-array? source-is-general?) (*copy-general-pvar-into-array-pvar dest-pvar source-pvar))
       
       ((and dest-is-structure? source-is-general?) (*copy-general-pvar-into-structure-pvar dest-pvar source-pvar))
       
       ((and dest-is-general? source-is-general?) (*copy-complex-general-pvar dest-pvar source-pvar))
       
       ((or (and dest-is-array? source-is-structure?)
            (and dest-is-structure? source-is-array?)
            )
        (error "You cannot copy an ARRAY into a STRUCTURE, or vice versa!")
        )
       
       (t (error "Internal error in *copy-pvar.  This condition cannot happen!"))
       
       ))))




(defun *copy-simple-general-pvar (dest-pvar source-pvar)
  
  ;; Copy the contents of the processors one by one
  ;; for every selected processor.
  
  (let ((dest-pvar-array (pvar-array dest-pvar))
        (source-pvar-array (pvar-array source-pvar))
        (any-active nil)
        )
    (with-simple-vectors (dest-pvar-array source-pvar-array)
      (do-for-selected-processors-internal (processor)
        (setq any-active t)
        (setf (aref dest-pvar-array processor) (aref source-pvar-array processor))
        )
      (when any-active (make-non-void dest-pvar))
      )))


(defun arrays-the-same-shape? (array1 array2)
  (equal (array-dimensions array1) (array-dimensions array2))
  )


(defun *copy-array-pvar (dest-pvar source-pvar)

  ;; If a pvar is an array pvar, its pvar-data slot
  ;; (which aliases to be the pvar-array slot)
  ;; is a lisp array of the declared shape
  ;; of the *Lisp array.

  ;; Make sure the two arrays agree in shape.  Then
  ;; recursively copy each element of the array.

  (let ((dest-array (pvar-array dest-pvar))
	(source-array (pvar-array source-pvar))
	)

    (when (not (arrays-the-same-shape? dest-array source-array))
      (error
	"The destination pvar ~S is an array of dimensions ~S.~
       The source pvar ~S is an array of dimensions ~S.~
       Since these two shapes are not the same I cannot copy the source to the destination.~
      "
	dest-pvar
	(array-dimensions dest-array)
	source-pvar
	(array-dimensions source-array)
	))
	 
    (let ((number-of-elements (array-total-size dest-array)))

      (if (eql 1 (array-rank dest-array))

	  (dotimes (j number-of-elements)
	    (*copy-pvar (aref dest-array j) (aref source-array j))
	    )
      
	  (let ((displaced-dest-array (make-array number-of-elements :displaced-to dest-array))
		(displaced-source-array (make-array number-of-elements :displaced-to source-array))
		)
	    (dotimes (j number-of-elements)
	      (*copy-pvar (aref displaced-dest-array j) (aref displaced-source-array j))
	      ))

	  ))))


(defun *copy-structure-pvar (dest-pvar source-pvar)

  ;; If a pvar is a structure pvar its pvar-data slot
  ;; (aliased to the pvar-structure slot)
  ;; is a lisp structure corresponding slot by slot
  ;; to the *Lisp structure.

  ;; Make sure the two structures are the same type,
  ;; then recursively copy each slot.

  (let ((dest-structure (pvar-structure dest-pvar))
	(source-structure (pvar-structure source-pvar))
	)

    (when (not (eq (type-of dest-structure) (type-of source-structure)))
      (error
	"The destination pvar ~S is a structure pvar of type ~S.~
         The source pvar ~S is a structure pvar of type ~S.~
         Since these two types are not identical I cannot copy the source to the destination..~
        "
	dest-pvar
	(type-of dest-structure)
	source-pvar
	(type-of source-structure)
	))

    (let ((canonical-pvar-type (pvar-canonical-pvar-type dest-pvar)))
      (let ((slot-accessor-function-names
	      (structure-pvar-type-front-end-slot-accessors
		(structure-pvar-type-name canonical-pvar-type)
		)))
	(dolist (slot-name slot-accessor-function-names)
	  (*copy-pvar (funcall slot-name dest-structure) (funcall slot-name source-structure))
	  )))

    (when (eq 'address-object (pvar-structure-name dest-pvar))
      (if (all-processors-active-function) ;; (eql (*sum (!! 1)) *number-of-processors-limit*)
	  (set-address-object-cached-geometry-id dest-pvar (address-object-cached-geometry-id source-pvar))
	  (progn
	    (decache-address-object-pvar dest-pvar)
	    (cache-address-object-pvar-if-possible dest-pvar '*set :check-legality nil)
	    )))

    ))


(defun *copy-array-pvar-into-general-pvar (dest-pvar source-pvar)
  (cond
    ((void-pvar-p dest-pvar)
     (if (not (eq :stack (allocated-pvar-p dest-pvar)))
	 (setf (vp-set-heap-pvar-arrays (pvar-vp-set dest-pvar))
	       (cons (pvar-location dest-pvar) (vp-set-heap-pvar-arrays (pvar-vp-set dest-pvar)))
	       )
	 (return-pvar-array-to-pool (pvar-array dest-pvar))
	 )
     (make-pvar-into-array-pvar
       dest-pvar
       (pvar-array-dimensions source-pvar)
       (pvar-array-canonical-element-type source-pvar)
       (if (member dest-pvar *temp-pvar-original-list* :test #'eq) :stack :heap)
       )
     (setf (pvar-lvalue? dest-pvar) t)
     (*copy-array-pvar dest-pvar source-pvar)
     )
    ((null (any-processors-active-function))) ;; (*or t!!)))
    (t (error "You cannot copy an array pvar into any non-array pvar other than one which is uninitialized"))
    ))


(defun *copy-structure-pvar-into-general-pvar (dest-pvar source-pvar)
  ;; (describe-pvar dest-pvar)
  (cond
    ((void-pvar-p dest-pvar)
     (if (not (eq :stack (allocated-pvar-p dest-pvar)))
	 (setf (vp-set-heap-pvar-arrays (pvar-vp-set dest-pvar))
	       (cons (pvar-location dest-pvar) (vp-set-heap-pvar-arrays (pvar-vp-set dest-pvar)))
	       )
	 (return-pvar-array-to-pool (pvar-array dest-pvar))
	 )
     (make-pvar-into-structure-pvar
       dest-pvar
       (pvar-structure-name source-pvar)
       (if (member dest-pvar *temp-pvar-original-list* :test #'eq) :stack :heap)
       )
     ;; (describe-pvar dest-pvar)
     (setf (pvar-lvalue? dest-pvar) t)
     (*copy-structure-pvar dest-pvar source-pvar)
     )
    ((null (any-processors-active-function))) ;; (*or t!!)))
    (t (error "You cannot copy a structure pvar into any non-array pvar other than one which is uninitialized"))
    ))



(defun *copy-general-pvar-into-array-pvar (dest-pvar source-pvar)
  
  dest-pvar source-pvar

  (if (not (any-processors-active-function)) ;; (*or t!!))
      (return-from *copy-general-pvar-into-array-pvar nil)
      (error "You cannot copy an array pvar into a general pvar")
      ))



(defun *copy-general-pvar-into-structure-pvar (dest-pvar source-pvar)

  dest-pvar source-pvar

  (if (not (any-processors-active-function)) ;; (*or t!!))
      (return-from *copy-general-pvar-into-structure-pvar nil)
      (error "You cannot copy a structure pvar into a general pvar")
      ))


(defun *copy-complex-general-pvar (dest-pvar source-pvar)
  (declare (ignore dest-pvar source-pvar))
  ;; Crude, but effective.
  (error "Internal error.  This should not be able to happen anymore")
  )
;;;   (do-for-selected-processors-internal (j)
;;;     (*setf (pref dest-pvar j) (pref source-pvar j))
;;;     ))
;;; 
