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



(DEFUN !! (VALUE)

  "Returns a pvar that contains VALUE in all processors."

  (cond
    ((eq value t) t!!)
    ((eq value nil) nil!!)
    (t (!!-with-hash value))
    ))


;; Find constants in the hash table and return their
;; corresponding pvar.  If the constant isn't in the
;; hash table yet and the hash table isn't full, put it in.

(defun !!-with-hash (value)

  ;; Get the hash table for the current vp set.

  (let* ((current-vp-set *current-vp-set*)
	 (current-hash-table (vp-set-constants-hash-table current-vp-set))
	 )

    ;; If the hash table hasn't been created yet create it.

    (when (not current-hash-table)
      (setf (vp-set-constants-hash-table current-vp-set)
	    (make-hash-table :test #'eql :size *maximum-number-of-entries-in-constant-pvar-hash-table*)
	    )
      (setq current-hash-table (vp-set-constants-hash-table current-vp-set))
      )

    ;; If the value is a key in the hash table, use the
    ;; pvar value of that key.

    (let ((pvar (gethash value current-hash-table)))
      (if pvar (return-from !!-with-hash pvar))
      )

    (let ((constant-heap? nil))

      (let ((result

	      (cond
	      
		;; We only hash scalars.
	      
		((or (numberp value) (characterp value))
	       
		 ;; should we insert this constant into the hash table
		 ;; for future reference? 
	       
		 (let ((insert-into-table
			 (and (not (floatp value)) (not (complexp value))
			      (< (hash-table-count current-hash-table)
				 *maximum-number-of-entries-in-constant-pvar-hash-table*
				 )))
		       (new-pvar nil)
		       )
		 
		   ;; create the new pvar.  If it is to go into the hash table
		   ;; make it permanent, otherwise make it temporary.
		   ;; fill all its fields with 'value'.
		 
		   (setq new-pvar
			 (if insert-into-table
			     (prog1
			       (make-general-pvar :heap)
			       (setq constant-heap? t)
			       )
			     (make-general-pvar :stack)
			     ))

		   (setf (pvar-name new-pvar) (intern (format nil "CONSTANT-~S" value)))
		   (fill-array (pvar-array new-pvar) value)
		   (make-non-void new-pvar)
		 
		   ;; put the pvar into the hash table if we want to.
		 
		   (if insert-into-table (setf (gethash value current-hash-table) new-pvar))
		 
		   new-pvar
		 
		   ))

		;; handle arrays and structures separately.
	      
		((and (symbolp (type-of value))
		      (structure-pvar-type-known-*defstruct-type (type-of value))
		      )
		 (!!-structure (type-of value) value)
		 )
	      
		((arrayp value) (!!-array value))
	      
		(t (error "Cannot put values of type ~S into a pvar" (type-of value)))
	      
		)))

	(setf (pvar-constant? result) (if constant-heap? :heap-constant t))
	(setf (pvar-lvalue? result) nil)
	(setf (pvar-constant-value result) value)

	result

	))))


(defun !!-structure (type value)
  (let ((!!-function (get type '*defstruct-!!-function)))
    (when (null !!-function)
      (error "Internal error.  The type ~S is a known *DEFSTRUCT type but has no !! function" type)
      )
    (funcall !!-function value)
    ))


(defun front-end!! (value)
  (*let (result)
        (let ((result-array (pvar-array result)))
          (with-simple-vectors (result-array)
            (do-for-selected-processors-internal (j) (setf (aref result-array j) value))
            )
          result
          )))

(defun front-end-p!! (pvar)
  (simple-pvar-argument!! pvar)
  (safety-check (new-pvar-check pvar 'front-end-p!!))
  (cond
   ((array-pvar-p pvar) nil!!)
   ((structure-pvar-p pvar) nil!!)
   ((general-pvar-p pvar)
    (*let (result)
          (let ((result-array (pvar-array result))
                (pvar-array (pvar-array pvar))
                )
            (with-simple-vectors (result-array pvar-array)
              (do-for-selected-processors-internal (j)
                (let ((value (aref pvar-array j)))
                  (setf (aref result-array j) 
                    (and value (not (eq t value)) (not (numberp value)) (not (characterp value))))
                  )))
            result
            )))))

(defun print-hash ()
  (maphash #'(lambda (key val) (format t "~%~S: ~S" key val))
	   *constant-pvar-hash-table*
   ))



(defun self-address!! ()
  "Returns the self address of each processor."
  (incf-use-count 'self-address!!)
  (vp-set-self-address!! *current-vp-set*)
  )


(defun !!-array (value &optional (element-type t))
  (assert (arrayp value) () "Internal error.  !!-ARRAY called with non-array value")
  (let ((result (make-array-pvar :stack `(pvar (array ,element-type ,(array-dimensions value))))))
    (setf (pvar-constant? result) nil)
    (setf (pvar-lvalue? result) t)
    (setf (pvar-name result) '!!-RETURN)
    (let ((displaced-value-array
           (if (vectorp value)
               value
             (make-array (array-total-size value) :displaced-to value :element-type (array-element-type value))
             ))
          (displaced-pvar-array (pvar-array-displaced-array result))
          )
      (dotimes (j (length displaced-value-array))
        (*set (aref displaced-pvar-array j) (!! (aref displaced-value-array j)))
        ))
    result
    ))
