;; -*- Mode: Lisp; Syntax:Common-Lisp; Package: (*SIM-I COMMON-LISP-GLOBAL); Muser: yes -*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.

;;;> Bugs, comments and revisions due to porting can be sent to:
;;;> bug-starlisp@think.com.  Other than to Thinking Machines'
;;;> customers, no promise of support is intended or implied.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


(defun internal-pref (pvar processor)
  (ecase (pvar-class pvar)
    (:general (aref (pvar-array pvar) processor))
    (:array (pref-array-pvar pvar processor))
    (:structure
      (when (eq 'address-object (pvar-structure-name pvar))
	(decache-address-object-pvar pvar)
	)
      (funcall (structure-pvar-pref-function pvar) pvar processor))
    ))


(defun new-pref-function (pvar processor vp-set)

  (simple-pvar-argument!! pvar)

  ;; Make sure the vp set and the pvar agree.

  (if (not (functionp pvar))
      (assert (eq (pvar-vp-set pvar) vp-set) ()
	      "The pvar ~S is in vp set ~S, but you specified vp set ~S in your call to PREF"
	      pvar (pvar-vp-set pvar) vp-set
	      ))

    ;; Figure out which processor to read from.

  (let ((processor (if (address-object-p processor)
		       (address-object-cube-address (smash-address-object-to-new-vp-set processor vp-set))
		       processor
		       )))

    ;; Verify that that processor actually exists.

    (when (not (and (integerp processor) (< -1 processor (vp-set-size vp-set))))
	(error "Processor must be an integer between 0 and ~D (the size of vp set ~S)"
	       (vp-set-size vp-set) vp-set
	       ))

    ;; Evaluate the pvar closure (if it is a closure) in the
    ;; context of the the proper vp set and in the active set
    ;; of the processor being read from.

    (if (functionp pvar)
	(*with-vp-set vp-set
          (internal-pref (funcall pvar) processor)
	  )
	(internal-pref pvar processor)
	)
		
      ))



(defun pref-array-pvar (pvar processor)
  (let* ((dimensions (pvar-array-dimensions pvar))
	 (result-array (make-array dimensions :element-type t))
	 (displaced-result-array
	   (if (vectorp result-array)
	       result-array
	       (make-array (array-total-size result-array)
			   :displaced-to result-array
			   :element-type (array-element-type result-array)
			   )))
	 (displaced-pvar-array (pvar-array-displaced-array pvar))
	 )
    (dotimes (j (length displaced-result-array))
      (setf (aref displaced-result-array j) (internal-pref (aref displaced-pvar-array j) processor))
      )
    result-array
    ))


(*defun pset (pvar index value)

  (safety-check (new-pvar-check-lvalue-no-vp-check pvar 'pset))

  (let ((index (if (address-object-p index)
		   (address-object-cube-address (smash-address-object-to-new-vp-set index (pvar-vp-set pvar)))
		   index
		   )))

    (safety-check
      (check-cube-address index (pvar-vp-set pvar) 'pset)
      )

    (cond

      ((general-pvar-p pvar)
       (cond
	 ((or (numberp value) (characterp value))
	  (setf (svref (pvar-array pvar) index) value)
	  )
	 (t
	  (let ((value-type (type-of value)))
	    (cond
	      ((or (structure-pvar-type-known-*defstruct-type value-type) (typep value 'array))
	       (if (void-pvar-p pvar)
		   (*when (=!! (self-address!!) (!! index)) (*set pvar (!! value)))
		   (error "You cannot put an array or structure value into a processor of an initialized general pvar")
		   ))
	      (t (setf (svref (pvar-array pvar) index) value))
	      ))))
       (make-non-void pvar)
       )

      ((array-pvar-p pvar)
       (let* ((lisp-array-holding-pvars (pvar-array pvar)))
	 (assert (and (arrayp value) (equal (array-dimensions value) (array-dimensions lisp-array-holding-pvars))) ()
		 "You are trying to put an array of shape ~S into an array pvar of shape ~S"
		 (array-dimensions value)
		 (array-dimensions lisp-array-holding-pvars)
		 )
	 (with-displaced-arrays (lisp-array-holding-pvars value) (displaced-lisp-array-holding-pvars displaced-value)
	   (dotimes (j (length displaced-value))
	     (*setf (pref (aref displaced-lisp-array-holding-pvars j) index) (aref displaced-value j))
	     ))))

      ((structure-pvar-p pvar)
       (let ((type-name (pvar-structure-name pvar)))
	 (let ((setf-pref-function (get type-name '*defstruct-setf-pref-function)))
	   (when (null setf-pref-function)
	     (error "Internal error: The *DEFSTRUCT ~S exists but has no PREF function" type-name)
	     )
	   (assert (typep value type-name) ()
		   "You are trying to change the contents of a pvar of structure type ~S, but the value ~S is not of that type"
		   type-name value
		   )
	   (funcall setf-pref-function pvar index value)
	   (when (eq 'address-object type-name) (decache-address-object-pvar pvar))
	   )))

      (t (error "PREF: Unknown pvar type.  Pvar is ~S" pvar))

      )

    value

    ))


;(defun pset-array-into-general-pvar (pvar index array)
;
;  (let* ((array-list (general-pvar-array-list pvar))
;	 (array-pvar-with-same-shape
;	   (find (array-dimensions array)
;		 array-list
;		 :test #'equal
;		 :key #'(lambda (pvar) (array-dimensions (pvar-array pvar)))
;		 ))
;	 )
;
;    ;; If one isn't found create a pvar of that shape.
;
;    (when (null array-pvar-with-same-shape)
;      (let ((new-array-pvar (make-array-pvar :heap `(pvar (array t ,(array-dimensions array))))))
;	(setf (pvar-lvalue? new-array-pvar) t)
;	(setf (pvar-constant? new-array-pvar) nil)
;	(setf (pvar-name new-array-pvar) 'ARRAY-PVAR-CONTAINED-BY-GENERAL-PVAR)
;	(push new-array-pvar (general-pvar-array-list pvar))
;	(setq array-pvar-with-same-shape new-array-pvar)
;	))
;
;    ;; Copy the data into this array pvar, newly created or not.
;
;    (pset array-pvar-with-same-shape index array)
;    
;    ;; Make the processor point to this new array.
;
;    (setf (aref (pvar-array pvar) index) array-pvar-with-same-shape)
;
;    ))
;
;
;(defun pset-structure-into-general-pvar (pvar index structure) 
;
;  (let* ((structure-list (general-pvar-structure-list pvar))
;	 (structure-pvar-of-same-type
;	   (find (type-of structure)
;		 structure-list
;		 :test #'eq
;		 :key #'(lambda (pvar) (type-of (pvar-structure pvar)))
;		 ))
;	 )
;
;    (when (null structure-pvar-of-same-type)
;      (let ((new-structure-pvar (make-structure-pvar :heap `(pvar (structure ,(type-of structure))))))
;	(setf (pvar-lvalue? new-structure-pvar) t)
;	(setf (pvar-constant? new-structure-pvar) nil)
;	(setf (pvar-name new-structure-pvar) 'STRUCTURE-PVAR-CONTAINED-BY-GENERAL-PVAR)
;	(push new-structure-pvar (general-pvar-structure-list pvar))
;	(setq structure-pvar-of-same-type new-structure-pvar)
;	))
;
;    (pset structure-pvar-of-same-type index structure)
;
;    (setf (aref (pvar-array pvar) index) structure-pvar-of-same-type)
;
;    ))


;(*defun slot-setf-pref (cm-slot-name pvar processor-id value)
;
;  (pvar-check-lvalue pvar '*SETF)
;
;  (cond
;    ((structure-pvar-p pvar)
;     (let ((front-end-slot-name (front-end-slot-name-from-cm-slot-name cm-slot-name)))
;       (let ((slot-pvar (funcall front-end-slot-name (pvar-structure pvar))))
;	 (pset slot-pvar processor-id value)
;	 )))
;    ((general-pvar-p pvar)
;     (let ((structure-pvar (aref (pvar-array pvar) processor-id)))
;       (assert (and (pvar-p structure-pvar) (structure-pvar-p structure-pvar)) ()
;	       "The value of general pvar ~S in processor ~D is not a structure"
;	       pvar processor-id
;	       )
;       (slot-setf-pref cm-slot-name structure-pvar processor-id value)
;       ))
;    (t (error "Attempt to call ~S on a pvar which is not a structure"))
;    ))
;
;
;(*defun element-setf-pref (pvar list-of-pvar-indices processor-id value)
;
;  (pvar-check-lvalue pvar '*SETF)
;  (validate-all-pvars list-of-pvar-indices 'AREF!!)
;  (when (no-processors-active)
;    (error "Cannot select an element of array pvar using pvar indices when no processors are active")
;    )
;
;  (cond
;
;    ((array-pvar-p pvar)
;     ;; Make sure every index is a constant, so we know which element to PSET.
;     (let ((list-of-indices
;	     (mapcar
;	       #'(lambda (index-pvar)
;		   (let ((value (*min index-pvar)))
;		     (assert (eql value (*max index-pvar)) ()
;			     "The index pvar ~S provided to (*SETF (PREF (AREF!! ... is not all everywhere constant"
;			     index-pvar
;			     )
;		     value
;		     ))
;	       list-of-pvar-indices
;	       )))
;       ;; Make sure the indices are valid for the array pvar.
;       (let ((array-of-element-pvars (pvar-array pvar)))
;	 (assert (apply #'array-in-bounds-p array-of-element-pvars list-of-indices) ()
;		 "The indices ~S provided to (*SET (PREF (AREF!! ... are not all within the bounds of array pvar ~S"
;		 list-of-indices pvar
;		 )
;	 (let ((element-pvar (apply #'aref array-of-element-pvars list-of-indices)))
;	   (pset element-pvar processor-id value)
;	 ))))
;
;    ((general-pvar-p pvar)
;     (let ((array-pvar (pref pvar processor-id)))
;       (assert (array-pvar-p array-pvar) () "The value of general pvar ~S in processor ~D is not an array" pvar processor-id)
;       (element-setf-pref array-pvar list-of-pvar-indices processor-id value)
;       ))
;
;    (t (error "Attempt to INDEX into a pvar which is not an array"))
;
;    ))





