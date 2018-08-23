;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: (*SIM-I COMMON-LISP-GLOBAL); Muser: Yes -*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


;;;; ****************************************************************************
;;;;
;;;;                             SCAN FUNCTIONS
;;;;
;;;; ****************************************************************************


(defun scan!!
    
    ;; Back-compatibly, scan!! must do a cube scan.
    ;; If the additional keyword :dimension is provided and
    ;; is non-nil, then a grid scan along a particular dimension
    ;; is done. 
    
    (pvar
     scan-operator
     &key
     (dimension nil)
     (direction :forward)
     (segment-pvar nil)
     (include-self t)
     (identity nil)
     (segment-mode (if segment-pvar :start :none))
     )
  
  (simple-pvar-argument!! pvar &opt segment-pvar)
  
  ;; Handle scanning with SEGMENT SETS.
  
  (when (and segment-pvar (eq (pvar-type segment-pvar) :structure))
    (when (not (eq (pvar-structure-name pvar) 'segment-set))
      (error "Unrecognizable structure pvar of type ~S given to SCAN!!." (pvar-structure-name pvar))
      )
    (when dimension
      (error "Sorry.  Implementation limitation.  You can only use SEGMENT-SETS in CUBE order.")
      )
    (when (null include-self)
      (error "Sorry.  Implementation limitation.  You cannot use SEGMENT-SETS with an INCLUDE-SELF value of NIL.")
      )
    (unless (member segment-mode '(:start nil :none)) 
      (error "Sorry.  Implementation limitation.  You cannot use segment-sets with a SEGMENT-MODE."))
    (return-from scan!! (segment-set-scan!! pvar scan-operator segment-pvar :direction direction))
    )
  
  ;; Handle normal (old) SCANNING.  If a dimension was provided,
  ;; do a grid scan, otherwise do a cube scan.
  
  (if dimension
      (progn
        ;;(with-compile-time-local-property (compile-time-prop *compilep* nil)
        (scan-grid!! pvar scan-operator
                     :direction direction :segment-pvar segment-pvar
                     :include-self include-self :dimension dimension
                     :identity identity :segment-mode segment-mode))
    (old-scan!! pvar scan-operator
                :direction direction :segment-pvar segment-pvar
                :include-self include-self :identity identity :segment-mode segment-mode))
  
  )


(defun legal-backwards-keyword? (keyword)
  (or (eq keyword :backward) (eq keyword :backwards))
  )


(defun check-scan-args (pvar segment-pvar direction)
  (safety-check
    (new-pvar-check pvar 'scan!!)
    (when segment-pvar (new-pvar-check segment-pvar 'scan!!))
    (if (not (or (eq direction :forward) (legal-backwards-keyword? direction)))
	(error "~a is an invalid scan direction.  The only legitimate ones are :FORWARD and :BACKWARD."
	       direction
	       ))))


(defun scan-array-pvar-elementwise
       (scan-function pvar function-of-two-pvar-arguments-returning-pvar direction dimension segment-pvar include-self identity segment-mode)
  (assert (eq 'copy!! function-of-two-pvar-arguments-returning-pvar) ()
	  "The only scan operation currently allowed for array pvars is COPY!!.  Use *Map to scan elementwise"
	  )
  (*let ((result-pvar pvar))
    (declare (return-pvar-p t))
    (*map
      #'(lambda (result-array-element-pvar source-array-element-pvar)
	  (*set result-array-element-pvar
		(funcall scan-function
			 source-array-element-pvar
			 function-of-two-pvar-arguments-returning-pvar
			 :direction direction :dimension dimension :segment-pvar segment-pvar
			 :include-self include-self :identity identity
			 :segment-mode segment-mode
			 )))
      result-pvar
      pvar
      )
    result-pvar
    ))


(defun scan-structure-pvar-slotwise
       (scan-function pvar function-of-two-pvar-arguments-returning-pvar direction dimension segment-pvar include-self identity segment-mode)
  (assert (eq 'copy!! function-of-two-pvar-arguments-returning-pvar) ()
	  "The only scan operation currently allowed for structure pvars is COPY!!."
	  )
  (*let ((return-pvar pvar))
    (with-structure-elements-iterated
      ((slot-pvar) (return-pvar) (structure-pvar-type-slot-accessors (pvar-canonical-pvar-type pvar)) :aliased? t)
      (*set slot-pvar
	    (funcall scan-function
		     slot-pvar
		     function-of-two-pvar-arguments-returning-pvar
		     :direction direction :dimension dimension :segment-pvar segment-pvar
		     :include-self include-self :identity identity
		     :segment-mode segment-mode
		     )))
    return-pvar
    ))



(defun old-scan!! (pvar function-of-two-pvar-arguments-returning-pvar
		   &key
		   (direction :forward) (dimension nil) (segment-pvar nil)
		   (include-self t) (identity nil) (segment-mode nil)
		   )

  (check-scan-args pvar segment-pvar direction)
  dimension

  (cond

    ((array-pvar-p pvar)
     (scan-array-pvar-elementwise
       'old-scan!! pvar function-of-two-pvar-arguments-returning-pvar direction nil segment-pvar include-self identity segment-mode
       ))

    ((structure-pvar-p pvar)
     (scan-structure-pvar-slotwise
       'old-scan!! pvar function-of-two-pvar-arguments-returning-pvar direction nil segment-pvar include-self identity segment-mode
       ))

    (t

     (let ((serial-equivalent-of-scan-function
	     (cadr
	       (assoc function-of-two-pvar-arguments-returning-pvar *special-scan-functions*)))
	   )
       
       (*let ((css nil!!)			; to hold currently selected set
	      (pvar-to-scan pvar)		; copy of pvar to scan so we can modify it
	      (segment-pvar-to-use nil!!)	; possibly altered copy of segment pvar, if provided
	      (temp1 nil!!)			; some temporaries we will need.
	      (temp2 nil!!)
	      (temp3 nil!!)
	      )
	 
	 ;; capture the currently selected set
	 
	 (*all (*set css nil!!))
	 (*set css t!!)
	 
	 ;; if there are no active processors we have nothing more to do, so just
	 ;; return the pvar we created.
	 
	 (if (*and nil!!)
	     
	     pvar-to-scan
	     
	     ;; if provided, make a copy of the segment pvar.  Otherwise it is all nil
	     ;; as allocated above.
	     
	     (progn
	       
	       (if (not (eq segment-mode :segment))
		   (when segment-pvar (*set segment-pvar-to-use (not!! (null!! segment-pvar))))
		   (when segment-pvar (*all (*set segment-pvar-to-use (not!! (null!! segment-pvar)))))
		   )
	       
	       ;; force the segment-pvar's first/last active processor to contain T
	       
	       (*setf (pref segment-pvar-to-use
			    (if (eq segment-mode :segment)
				0
				(if (eq direction :forward)
				    (*min (self-address!!))
				    (*max (self-address!!))
				    )))
		      t
		      )
	       
	       ;; get the actual vectors we use to simulate the pvars
	       
	       (let ((values-vector (pvar-array pvar-to-scan))
		     (active-vector (pvar-array css))
		     (segment-vector (pvar-array segment-pvar-to-use))
		     )
		 
		 ;; and go for it!!!
		 
		 (inner-scan!! 
		   values-vector active-vector segment-vector
		   serial-equivalent-of-scan-function
		   function-of-two-pvar-arguments-returning-pvar
		   (legal-backwards-keyword? direction)
		   include-self
		   temp1 temp2 temp3
		   segment-mode
		   )
		 
		 )
	       
	       pvar-to-scan
	       
	       )))))))



(defun inner-scan!!

       (values-vector active-vector segment-vector
	serial-equivalent-of-scan-function function
	backwards-p include-self
	temp-pvar-1 temp-pvar-2 result-pvar
	segment-mode
	)

  (when backwards-p
    (setq values-vector (nreverse values-vector))
    (setq active-vector (nreverse active-vector))
    (if (not (eq segment-mode :segment))
	(setq segment-vector (nreverse segment-vector))
	(progn
	  (dotimes (j (1- (length segment-vector)))
	    (setf (svref segment-vector j) (svref segment-vector (1+ j)))
	    )
	  (setf (svref segment-vector (1- (length segment-vector))) t)
	  (setq segment-vector (nreverse segment-vector))
	  )))

  (let ((value-computed-so-far nil))
	      
    (*all
      
      (*when (=!! (self-address!!) (!! 0))
	
	(if (not (eq segment-mode :segment))

	    (dotimes (j (length values-vector))

	      (when (svref active-vector j)

		(if (svref segment-vector j)

		    ;; starting a new segment

		    (setq value-computed-so-far (svref values-vector j))

		    ;; continue a segment

		    (if serial-equivalent-of-scan-function

			(setf (svref values-vector j)
			      (setq value-computed-so-far
				    (funcall serial-equivalent-of-scan-function
					     value-computed-so-far
					     (svref values-vector j))))

			(progn
			  (*setf (pref temp-pvar-1 0) value-computed-so-far)
			  (*setf (pref temp-pvar-2 0) (svref values-vector j))
			  (*set result-pvar (*funcall function temp-pvar-1 temp-pvar-2))
			  (setf (svref values-vector j)
				(setq value-computed-so-far (pref result-pvar 0))))

			))))

	    (let ((started-new-segment nil))

	      (dotimes (j (length values-vector))

		(when (svref segment-vector j)
		  (setq started-new-segment t)
		  )

		(when (svref active-vector j)

		  (if started-new-segment

		      ;; starting a new segment

		      (progn
			(setq value-computed-so-far (svref values-vector j))
			(setq started-new-segment nil)
			)

		      ;; continue a segment

		      (if serial-equivalent-of-scan-function

			  (setf (svref values-vector j)
				(setq value-computed-so-far
				      (funcall serial-equivalent-of-scan-function
					       value-computed-so-far
					       (svref values-vector j))))

			  (progn
			    (*setf (pref temp-pvar-1 0) value-computed-so-far)
			    (*setf (pref temp-pvar-2 0) (svref values-vector j))
			    (*set result-pvar (*funcall function temp-pvar-1 temp-pvar-2))
			    (setf (svref values-vector j)
				  (setq value-computed-so-far (pref result-pvar 0))))

			  )))))

	    )

	;; we are done with the actual scan.  If we are not supposed
	;; to include self, then shift over all the results by one.

	(if (not (eq segment-mode :segment))

	    (when (null include-self)
	      (let ((previous-value nil))
		(dotimes (j (length values-vector))
		  (when (svref active-vector j)
		    (let ((temp (svref values-vector j)))
		      (setf (svref values-vector j) previous-value)
		      (setq previous-value temp)
		      )))))

	    (when (null include-self)
	      (let ((previous-value nil))
		(dotimes (j (length values-vector))
		  (when (svref segment-vector j)
		    (setq previous-value nil)
		    )
		  (when (svref active-vector j)
		    (let ((temp (svref values-vector j)))
		      (setf (svref values-vector j) previous-value)
		      (setq previous-value temp)
		      )))))

	    )

	)))

  (when backwards-p (setq values-vector (nreverse values-vector)))

 )


(defun scan-grid!! (pvar function-of-two-pvar-arguments-returning-pvar
			  &key
			  (dimension 0)
			  (direction :forward)
			  (segment-pvar nil)
			  (include-self t)
			  (identity nil)
			  (segment-mode nil)
			  )
  
  ;; check all the arguments
  
  (check-scan-args pvar segment-pvar direction)

  (cond

    ((array-pvar-p pvar)
     (scan-array-pvar-elementwise
       'scan-grid!! pvar function-of-two-pvar-arguments-returning-pvar direction dimension segment-pvar include-self identity segment-mode
       ))

    ((structure-pvar-p pvar)
     (scan-structure-pvar-slotwise
       'scan-grid!! pvar function-of-two-pvar-arguments-returning-pvar direction dimension segment-pvar include-self identity segment-mode
       ))

    (t

     (let ((dimension-number (assocv-cadr dimension *dimension-keyword-mappings*))
	   (serial-equivalent-of-scan-function
	     (assocv-cadr function-of-two-pvar-arguments-returning-pvar *special-scan-functions*))
	   (indices nil)
	   (backwards nil)
	   (segment-pvar-supplied-p (not (null segment-pvar)))
	   )
       
       (when (null dimension-number)
	 (assert (valid-integer-range-exclusive dimension 0 *number-of-dimensions*)
		 (dimension)
		 "Dimension argument to SCAN-GRID!! is not a valid dimension: ~S"
		 dimension)
	 (setq dimension-number dimension)
	 )
       
       (setq backwards (legal-backwards-keyword? direction))
       
       ;; copy the pvar we are to scan so the scan can be done in-place.  This is
       ;; the pvar we will return.  Also allocate a few temporaries we will need.
       
       (*let ((pvar-to-scan pvar)
	      (real-segment-pvar (if segment-pvar (not!! (null!! segment-pvar)) nil!!))
	      (temp1 nil!!)
	      (temp2 nil!!)
	      (temp3 nil!!)
	      )
	 
	 ;; if no processors are active there is nothing to do.
	 ;; Just return the copy we made.
	 
	 (if (no-processors-active)
	     
	     pvar-to-scan
	     
	     ;; Otherwise the fun begins!
	     
	     (*let ((css nil!!))
	       
	       ;; capture the currently selected set.
	       
	       (*all (*set css nil!!))
	       (*set css t!!)
	       
	       ;; Allocate arrays as long as the dimension we are scanning over.
	       ;; The mask is set up to cause the iteration macro to avoid
	       ;; iterating over the dimension we wish to scan.
	       
	       (let* ((scan-length (elt *current-cm-configuration* dimension-number))
		      (values-array (make-sequence 'vector scan-length :initial-element nil))
		      (active-array (make-sequence 'vector scan-length :initial-element nil))
		      (segment-array (make-sequence 'vector scan-length :initial-element nil))
		      (mask (make-list *number-of-dimensions* :initial-element t))
		      (cube-address-array (vp-set-array-of-cube-addresses *current-vp-set*))
		      )
		 
		 (setf (elt mask dimension-number) nil)
		 
		 ;; This macro iterates over every row we want to scan over in our
		 ;; hypergrid.  On each iteration indices is bound as a list of
		 ;; indices with values in all dimensions except the dimension we
		 ;; are to scan over.  Since all but one dimension is specified
		 ;; the values dictate a particular row of the hypergrid.
		 
		 (with-grid-indices-iterated
		   
		   (indices *number-of-dimensions* :mask mask :check-arguments nil)
		   (setf (elt indices dimension-number) nil)
		   
		   ;; for each row of the hypergrid, we copy the values to be scanned,
		   ;; the segment pvar values and which processors are active into the
		   ;; arrays we allocated above.  We also determine the first/last active
		   ;; processor.
		   
		   (let ((first-active nil))
		     
		     (dotimes (i scan-length)
		       (let ((j (if backwards (- (1- scan-length) i) i)))
			 (setf (elt indices dimension-number) j)
			 (let ((send-address (internal-cube-address-from-grid-address-list cube-address-array indices)))
			   (setf (svref values-array j) (pref pvar-to-scan send-address))
			   (setf (svref active-array j) (pref css send-address))
			   (when (and (not first-active) (svref active-array j))
			     (setq first-active j))
			   (setf (svref segment-array j)
				 (if segment-pvar-supplied-p
				     (not (null (pref real-segment-pvar send-address)))
				     nil
				     )))))
		     
		     ;; if there are no active processors in this row there is
		     ;; nothing to do.  Otherwise we do the scan along this row
		     ;; using our inner-scan with our arrays, and then copy the
		     ;; result out of the values-array back into the pvar.
		     
		     (when first-active
		       (setf (svref segment-array first-active) t)
		       (inner-scan!!
			 values-array active-array segment-array
			 serial-equivalent-of-scan-function
			 function-of-two-pvar-arguments-returning-pvar
			 backwards include-self
			 temp1 temp2 temp3
			 segment-mode
			 )
		       (dotimes (j scan-length)
			 (setf (elt indices dimension-number) j)
			 (let ((send-address (internal-cube-address-from-grid-address-list cube-address-array indices)))
			   (*setf (pref pvar-to-scan send-address) (svref values-array j))
			   ))))
		   
		   (setf (elt indices dimension-number) nil)
		   
		   ))
	       
	       pvar-to-scan
	       
	       )))))))


(defun spread!! (pvar dimension-constant coordinate-constant)

  (simple-pvar-argument!! pvar)

  (safety-check (new-pvar-check pvar 'spread!!))

  (if (null dimension-constant)

      ;; A cube address spread.  Kind of silly, but we allow it.

      (!! (pref pvar coordinate-constant))

      (progn

	(assert (and (integerp dimension-constant) (< -1 dimension-constant *number-of-dimensions*)) ()
		"The dimension-constant argument to spread!!, having value ~S, is not an integer between 0 and ~D,~@
                 the number of dimensions in the current vp set"
		dimension-constant *number-of-dimensions*
		)
	(assert (and (integerp coordinate-constant) (< -1 coordinate-constant (dimension-size dimension-constant))) ()
		"The coordinate-constant argument to spread!!, having value ~S, is not an integer between 0 and ~D,~@
                 the extent of dimension ~D in the current vp set"
		coordinate-constant (dimension-size dimension-constant) dimension-constant
		)

	(*all
	  (declare (return-pvar-p t))

	  (*let (result-pvar self-address-grid coordinate-constant-pvar segment-pvar)
	    (declare (return-pvar-p t))
	    (*set self-address-grid (self-address-grid!! (!! dimension-constant)))
	    (*set coordinate-constant-pvar (!! coordinate-constant))
	    (*set segment-pvar nil!!)
	    
	    ;; Select just the row/column which contains the data
	    ;; to be spread, and set up a segment pvar which is T
	    ;; for this row/column.

	    (*when (=!! self-address-grid coordinate-constant-pvar)
	      (declare (return-pvar-p nil))
	      (*set result-pvar pvar)
	      (*set segment-pvar t!!)
	      )

	    ;; Copy scan the data forwards from the row/column

	    (*when (>=!! self-address-grid coordinate-constant-pvar)
	      (declare (return-pvar-p nil))
	      (*set result-pvar
		    (scan!! result-pvar 'copy!! :dimension dimension-constant :direction :forward :segment-pvar segment-pvar)
		    ))

	    ;; Copy scan the data backwards from the row/column

	    (*when (<=!! self-address-grid coordinate-constant-pvar)
	      (declare (return-pvar-p nil))
	      (*set result-pvar
		    (scan!! result-pvar 'copy!! :dimension dimension-constant :direction :backward :segment-pvar segment-pvar)
		    ))

	    result-pvar

	    )))))



(defun reduce-and-spread!! (pvar operator dimension-constant)

  (simple-pvar-argument!! pvar)

  (safety-check (new-pvar-check pvar 'reduce-and-spread!!))

  (if (null dimension-constant)

      (*let (return-pvar)
	(*set return-pvar (scan!! pvar operator :direction :forward))
	(*set return-pvar (scan!! return-pvar 'copy!! :direction :backward))
	return-pvar
	)

      (progn
	
	(assert (and (integerp dimension-constant) (< -1 dimension-constant *number-of-dimensions*)) ()
		"The dimension-constant argument to reduce-and-spread!!, having value ~S, is not an integer between 0 and ~D,~@
                 the number of dimensions in the current vp set"
		dimension-constant *number-of-dimensions*
		)

	(*let (return-pvar)
	  (*set return-pvar (scan!! pvar operator :dimension dimension-constant :direction :forward))
	  (*set return-pvar (scan!! return-pvar 'copy!! :dimension dimension-constant :direction :backward))
	  return-pvar
	  )

	)))


(defun coerce!! (pvar type &aux (ctype (canonical-pvar-type type)))
  (simple-pvar-argument!! pvar)
  (if (or (null ctype) (atom ctype) (not (eq (car ctype) 'pvar)))
      (error "Invalid type ~S to coerce!!." type))
  (case (canonical-pvar-element-type ctype)
    (boolean
      (assert (*and (booleanp!! pvar)) () "Cannot coerce pvar with non-boolean values into boolean pvar")
      pvar
      )
    (front-end pvar)
    (unsigned-byte
      (assert (*and (and!! (integerp!! pvar) (not!! (minusp!! pvar)))) ()
	      "Cannot coerce pvar with negative or non-integer values to unsigned-byte pvar"
	      )
      pvar
      )
    (signed-byte
      (assert (*and (integerp!! pvar)) () "Cannot coerce pvar with non-integer values to signed-byte pvar")
      pvar
      )
    (defined-float
      (assert (*and (and!! (numberp!! pvar) (not!! (complexp!! pvar)))) ()
	      "Cannot coerce pvar with non-integer/non-float values to float pvar"
	      )
      (let ((mantissa (float-pvar-type-mantissa ctype)) (exponent (float-pvar-type-exponent ctype)))
	(if (or (eq mantissa '*) (eq exponent '*))
	    (float!! pvar)
	    (if (and (<= (float-pvar-type-mantissa ctype) 23) (<= (float-pvar-type-exponent ctype) 8))
		(float!! pvar (!! 0.0))
		(float!! pvar (!! 0.0d0))
		))))
    (complex
      (assert (*and (numberp!! pvar)) () "Cannot coerce pvar with non-numeric valuue to complex pvar")
      (let* ((mantissa (complex-pvar-type-mantissa ctype))
	     (exponent (complex-pvar-type-exponent ctype))
	     (float-pvar-type `(float-pvar ,mantissa ,exponent))
	     )
	(if (or (eq mantissa '*) (eq exponent '*))
	    (complex!! (float!! (realpart!! pvar)) (float!! (imagpart!! pvar)))
	    (if!! (complexp!! pvar)
		  (complex!! (coerce!! (realpart!! pvar) float-pvar-type) (coerce!! (imagpart!! pvar) float-pvar-type))
		  (complex!! (coerce!! pvar float-pvar-type) (coerce!! (!! 0) float-pvar-type))
		  ))))
    (string-char
      (if!! (and!! (characterp!! pvar) (string-char-p!! pvar)) pvar (code-char!! pvar))
      )
    (character
      (if (eql (pvar-type pvar) :array)
	  (let ((dimensions (array-pvar-dimensions pvar))
		(rank (array-pvar-rank pvar)))
	    (assert (and (= rank 1)
			 (= (car dimensions) 1))
		    (pvar)
		    "Can only coerce character vector pvars of length 1 to character pvars.")
	    (setq pvar (aref!! pvar 0))))
      (if!! (characterp!! pvar) pvar (code-char!! pvar))
      )
    (array
      (coerce-to-array-pvar pvar ctype)
      )
    (structure
      (error "The *Lisp Simulator does not handle coercions to structure pvars.")
      )
    ((t)
     (cond
       ((array-pvar-p pvar) (error "Array pvars cannot be coerced to general pvars"))
       ((structure-pvar-p pvar) (error "Structure pvars cannot be coerced to general pvars"))
       (t pvar)
       ))
    ((* pvar) (error "Invalid type ~S to coerce!!." ctype))
    ))


(defun coerce-to-array-pvar (pvar ctype)
  (when (not (array-pvar-p pvar))
    (error "You cannot coerce a non-array pvar to an array pvar.")
    )
  (let ((array-pvar (make-pvar-based-on-canonical-pvar-type :stack ctype)))
    (setf (pvar-constant? array-pvar) nil)
    (setf (pvar-lvalue? array-pvar) t)
    (setf (pvar-name array-pvar) 'COERCE!!-RETURN)
    (when (not (eql (array-pvar-rank pvar) (array-pvar-rank array-pvar)))
      (error "You cannot coerce an array pvar of rank ~D into an array pvar of rank ~D"
	     (array-pvar-rank pvar) (array-pvar-rank array-pvar)
	     ))
    (*set array-pvar pvar)
    (setf (pvar-lvalue? array-pvar) nil)
    array-pvar
    ))

