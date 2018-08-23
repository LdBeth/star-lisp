;;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *SIM-I -*-

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


;;;; Description:

;;;; RANK!!

;;;; This function is just like old RANK!! except that the user may specify
;;;; either a segment-pvar or an axis (dimension) or both.  If a segment-pvar alone
;;;; is specified then the ranking is done independently within each
;;;; segment.  If an axis alone is specified then the ranking is done
;;;; independently along each row of that dimension.  If both are 
;;;; specified then the ranking is done independently within each
;;;; segment, the segments all being defined within a single row.

;;;; Example:  (rank!! (random!! (!! 10)) '<=!! :segment-pvar (evenp!! (self-address!!)))
;;;; If the first 12 random elements were
;;;;
;;;; 0 2 4 2 1 7 5 3 4 7 8 2
;;;;
;;;; then the result would be
;;;;
;;;; 0 1 1 0 0 1 1 0 0 1 1 0


;;;; NEW-SORT!!

;;;; This function is just like old SORT!! except that the user may specify
;;;; either a segment-pvar or an axis (dimension) or both.  See NEW-RANK!!
;;;; above for a discussion of what each specification does.  NEW-SORT!!
;;;; also provides the user the ability to sort a pvar given a key
;;;; function.  This key function might be, for example, a *DEFSTRUCT
;;;; structure accessor, and the pvar to be sorted would be an instance
;;;; of the *DEFSTRUCT.

;;;; Example:  Using the above example except that sorting is being
;;;; done instead of ranking, the result would be:

;;;; 0 2 2 4 1 7 3 5 4 7 2 8

;;;; Example:  If AFOO were an instance of a *DEFSTRUCT with slot FOO-A!!,
;;;; then

;;;; (SORT!! AFOO '<=!! :DIMENSION 0 :KEY 'FOO-A!!)

;;;; Would sort AFOO using its A slot as the key, independently along each
;;;; row of dimension 0 (the X dimension).


;;;; (SEGMENTED-SPREAD!! PVAR DIMENSION-CONSTANT SEGMENT-PVAR ELEMENT-PVAR)

;;;; This is similar to SPREAD!!.  However, spreading can be done from
;;;; every defined segment, and the value to be spread is designated
;;;; within each segment using the boolean pvar element-pvar.

;;;; Example:

;;;; (segmented-spread!! pvar nil (zerop!! (mod!! (self-address!!) (!! 10))) (random!! (!! 5)))

;;;; This returns a pvar which has either the 0th, 1st, 2nd, 3rd or 4th
;;;; element of each segment defined by (zerop!! (mod!! (self-address!!) (!! 10)))
;;;; (i.e., every 10 processors constitute a segment), as the value
;;;; for the entire segment.


(*proclaim '(ftype (function (t) boolean-pvar) t!!-in-first-active-processor-along-row))

(defun t!!-in-first-active-processor-along-row (dimension-constant)

  ;; Return a pvar which is 1 in the first active processor
  ;; of every row along a particular dimension.

  ;; The trick is to plus scan across the dimension using 1 as
  ;; the source, and noting which processors still have a 1
  ;; after the scan.

  (*compile-blindly
    (*let (result temp)
      (declare (type boolean-pvar result))
      (declare (type (field-pvar *current-send-address-length*) temp))
      (declare (return-pvar-p t))
      nil
      (*all (*set result nil!!))
      (*set temp (scan!! (!! 1) '+!! :dimension dimension-constant))
      (*set result (=!! (!! 1) temp))
      result
      )))


(defvar *stable-rank* nil)


(defun rank!! (pvar predicate &key dimension segment-pvar)
  (simple-pvar-argument!! pvar &opt segment-pvar)
  (safety-check
    (new-vp-pvar-check pvar 'rank!!)
    (when segment-pvar (new-vp-pvar-check segment-pvar 'rank!!))
    (when (eq predicate (symbol-function '<=!!)) (setq predicate '<=!!))
    (when (not (eq predicate '<=!!))
      (error "Implementation limitation.  Predicate ~S is invalid. Predicate must be <=!!" predicate))
    (when dimension
      (when (or (not (integerp dimension)) (not (< -1 dimension *number-of-dimensions*)))
	(error "You specified dimension ~S, but there are ~D dimensions in the current vp set"
	       dimension *number-of-dimensions*
	       ))))
  (without-void-pvars (pvar)
    (internal-rank!! pvar predicate dimension segment-pvar)
    ))

(defun stable-rank!! (pvar predicate &key dimension segment-pvar)
  (let ((*stable-rank* t))
    (declare (special *stable-rank*))
    (rank!! pvar predicate :dimension dimension :segment-pvar segment-pvar)
    ))



#+*LISP-HARDWARE
(defun internal-rank!! (pvar predicate dimension segment-pvar)

  (case (pvar-type pvar)

    (:general (general-rank!! pvar predicate dimension segment-pvar))

    ;; Turn whatever pvar it is that is to be ranked into an
    ;; equivalent (from the point of view of ranking) unsigned
    ;; pvar.  Turn the segment pvar into a boolean pvar if it
    ;; is not a boolean pvar already.

    ;; finally, dispatch to the proper routine depending on the
    ;; kind of scan we are doing (segmented vs unsegmented, grid vs cube)

    (t 

      (let ((canonical-value (canonical-value-for-rank pvar))
	    (canonical-segment (if segment-pvar (canonical-segment segment-pvar) nil))
	    )

	(internal-unsigned-rank!! canonical-value dimension segment-pvar canonical-segment)

	))))


#+*LISP-SIMULATOR
(defun internal-rank!! (pvar predicate dimension segment-pvar)
  (cond
    ((*and (and!! (integerp!! pvar) (not!! (minusp!! pvar))))
     (let ((canonical-segment (if segment-pvar (canonical-segment segment-pvar) nil))
	   (canonical-value pvar)
	   )
       (internal-unsigned-rank!! canonical-value dimension segment-pvar canonical-segment)
       ))
    ((*and (integerp!! pvar))
     (let ((canonical-segment (if segment-pvar (canonical-segment segment-pvar) nil))
	   (canonical-value (signed-to-ordered-bits!! pvar))
	   )
       (internal-unsigned-rank!! canonical-value dimension segment-pvar canonical-segment)
       ))
    ((*and (floatp!! pvar))
     (let ((canonical-segment (if segment-pvar (canonical-segment segment-pvar) nil))
	   (canonical-value (float-to-ordered-bits!! pvar))
	   )
       (internal-unsigned-rank!! canonical-value dimension segment-pvar canonical-segment)
       ))
    (t (general-rank!! pvar predicate dimension segment-pvar))
    ))


(defun internal-unsigned-rank!! (canonical-value dimension segment-pvar canonical-segment)

  (cond

    ((and (null dimension) (null segment-pvar)) (unsegmented-cube-rank!!-internal canonical-value))

    ((and (null dimension) segment-pvar) (segmented-cube-rank!!-internal canonical-value canonical-segment))

    ((and dimension (null segment-pvar)) (grid-rank!!-internal canonical-value dimension))

    ((and dimension segment-pvar) (segmented-grid-rank!!-internal canonical-value dimension canonical-segment))

    ))


#+*LISP-HARDWARE
(defun unsegmented-cube-rank!!-internal (field-pvar)
  (*compile-blindly
    (let ((result (allocate-field-pvar *current-send-address-length*)))
      (*set (the (field-pvar *current-send-address-length*) result)
	    (rank!! (the (field-pvar *) field-pvar) '<=!!))
      result
      )))

#+*LISP-SIMULATOR
(defun unsegmented-cube-rank!!-internal (pvar)
  (let* ((value-and-rank-list nil)
         (count 0)
         (pvar-array (pvar-array pvar))
         (return-pvar (allocate-temp-general-pvar))
         (return-array (pvar-array return-pvar))
         )
    (with-simple-vectors (return-array)
      (do-for-selected-processors-internal (j)
        (push (cons (aref pvar-array j) j) value-and-rank-list)
        )
      (setq value-and-rank-list (nreverse value-and-rank-list))
      (setq value-and-rank-list
            (if *stable-rank*
                (stable-sort value-and-rank-list #'< :key #'car)
              (sort value-and-rank-list #'<= :key #'car)
              ))
      (do-for-selected-processors-internal (j)
        (setf (aref return-array (cdr (car value-and-rank-list)))
          count)
        (incf count)
        (pop value-and-rank-list)
        )
      return-pvar
      )))
  

(defun segmented-cube-rank!!-internal (source segment)
  (segmented-grid-rank!!-internal source nil segment)
  )


(defun grid-rank!!-internal (pvar dimension)
  ;; Make segments out of the rows and then do a segmented grid rank.
  (segmented-grid-rank!!-internal pvar dimension (t!!-in-first-active-processor-along-row dimension))
  )


(defun segmented-grid-rank!!-internal (pvar dimension segment-pvar)

  ;; Enumerate the start bits of the segment pvar.
  ;; This uniquely identifies each segment.
  ;; Scan out this tag to every processor in the segment.

  ;; Select every processor, and identify those
  ;; that were previously selected.

  ;; Append the enumeration tag and a bit which indicates
  ;; whether the processor was originally active or not
  ;; as the high order bits of the source
  ;; so that source values in the same segment will get
  ;; ranked next to each other, and so that inactive
  ;; processors will all have higher rankings per
  ;; segment than inactive processors.

  ;; Rank this concatenation across the whole machine.

  ;; Figure out where the end of each segment is.

  ;; Finally, scan across the segments forward and
  ;; backwards to get the minimum rank per segment
  ;; in every processor of the segment.  Then
  ;; subtract this minimum ranking from the
  ;; processor's rank value, to produce the ranking
  ;; relative to the segment.

  (*locally
    (declare (type (field-pvar *) pvar))
    (declare (type boolean-pvar segment-pvar))
    (*compile-blindly
      (*let (enumeration result min-result css segment-start segment-end pvar-copy)
	(declare (type (field-pvar *current-send-address-length*) enumeration result min-result))
	(declare (type boolean-pvar css segment-start segment-end))
	(declare (type (field-pvar *) pvar-copy))
	(declare (return-pvar-p t))
	nil
	(*all
	  (*set css nil!!)
	  (*set segment-start nil!!)
	  (if dimension
	      (*set segment-start (zerop!! (self-address-grid!! (!! (the fixnum dimension)))))
	      (*setf (pref segment-start 0) t)
	      )
	  (*set segment-end nil!!)
	  )
	(*set css t!!)
	(if (*and (null!! segment-pvar))      ;; if there are no segments selected
	    (*setf (pref segment-start 0) t)  ;; fake one
	    (*set segment-start segment-pvar));; else use selected segments - WRS 9/18/90
	;; make all the integers non-negative
	(*set pvar-copy (-!! pvar (!! (*min pvar))))
	(*all
	  (declare (return-pvar-p nil))
	  (*when (not!! css) (*set pvar-copy (!! 0)))
	  (*when segment-start (*set enumeration (enumerate!!)))
	  (*set enumeration
		(scan!! enumeration 'copy!! :segment-pvar segment-start :dimension dimension :direction :forward))
	  (let ((enumerate-size (*integer-length enumeration))
		(source-size (*integer-length pvar-copy))
		(active-set-tag-size 1)
		)
	    #+*LISP-SIMULATOR
	    (declare (ignore active-set-tag-size))
	    (*locally
	      (declare (type fixnum enumerate-size source-size active-set-tag-size))
	      (*let (concatenation)
		(declare (type (field-pvar (+ enumerate-size active-set-tag-size source-size)) concatenation))
		(declare (return-pvar-p nil))
		nil
		(*set concatenation pvar-copy)
		(*set concatenation (deposit-byte!! concatenation (!! source-size) (!! 1) (if!! css (!! 0) (!! 1))))
		(let ((temp (1+ source-size)))
		  (*locally
		    (declare (type fixnum temp))
		    (*set concatenation (deposit-byte!! concatenation (!! temp) (!! enumerate-size) enumeration))
		    ))
		(*set result (rank!! concatenation '<=!!))
		(*set segment-end
		      (scan!! segment-start 'copy!!
			      :dimension dimension :direction :backward :segment-pvar t!! :include-self nil))
		(if dimension
		    (*set segment-end
			  (or!! segment-end (=!! (self-address-grid!! (!! (the fixnum dimension)))
						 (!! (the fixnum (1- (dimension-size dimension)))))))
		    (*setf (pref segment-end (1- *number-of-processors-limit*)) t)
		    )
		(*set min-result (scan!! result 'min!! :segment-pvar segment-start :dimension dimension))
		(*set min-result (scan!! min-result 'min!! :direction :backward :segment-pvar segment-end :dimension dimension))
		(*set result (-!! result min-result))
		))))
	result
	))))


#+*LISP-HARDWARE
(defun general-rank!! (pvar predicate dimension segment-pvar)

  ;; Make sure the pvar contains only floats and ints.

  (*let ((temp-bit (not!! (or!! (integerp!! (the (pvar t) pvar)) (floatp!! (the (pvar t) pvar))))))
    (declare (type boolean-pvar temp-bit))
    (pvar-error-if
      temp-bit
      #.(slc::encode-pvar-overflow-tag 'rank!! :other :invalid-type nil)
      pvar
      ))

  ;; If the pvar contains only floats or ints but not both, then
  ;; smallest-possible-pvar will make a non-general pvar
  ;; of the contained type.

  (setq pvar (smallest-possible-pvar pvar))
  (cond ((not (eq (pvar-type pvar) :general))
	 (internal-rank!! pvar predicate dimension segment-pvar)
	 )
	(t		    
	 ;; at this point, I can guarantee that there are both integers and floats in PVAR.  There might
	 ;; be precision problems with converting the integers to floats.
	 (*nocompile
	   (*let ((temp1 pvar))
	     (declare (type (float-pvar (pvar-general-mantissa-length pvar)
					(pvar-general-exponent-length pvar)) temp1))
	     (internal-rank!! temp1 predicate dimension segment-pvar)
	     )))))


#+*LISP-SIMULATOR
(defun general-rank!! (pvar predicate dimension segment-pvar)

  ;; there are floats and integers.
  ;; coerce everything to double precision floats and then do the rank.

  (safety-check
    (assert (*and (and!! (numberp!! pvar) (not!! (complexp!! pvar)))) ()
	    "You cannot rank a pvar which contains a non-number or a complex number."
	    ))
  (rank!! (float!! pvar (!! pi)) predicate :dimension dimension :segment-pvar segment-pvar)
  )


(defun canonical-segment (pvar)
  #+*LISP-HARDWARE
  (case (pvar-type pvar)
    (:boolean pvar)
    (not!! (null!! pvar))
    )
  #+*LISP-SIMULATOR
  (if (*and (booleanp!! pvar)) pvar (not!! (null!! pvar)))
  )

#+*LISP-HARDWARE
(defun canonical-value-for-rank (pvar)

  ;; We want to return an unsigned pvar which preserves
  ;; the order of the original pvar.  Thus the unsigned pvar
  ;; can be used for ranking instead of the original one.

  (case (pvar-type pvar)
    (:field (smallest-possible-field-pvar pvar))
    (:signed (signed-to-ordered-bits!! pvar))
    (:float (float-to-ordered-bits!! pvar))
    (otherwise
      (progn
	(pvar-error-if t!! #.(slc::encode-pvar-overflow-tag 'rank!! :other :invalid-type nil) pvar)
	(!! 0)
	))))


(defun smallest-possible-field-pvar (field-pvar)

  ;; Reduce the pvar to the smallest possible number
  ;; of bits.  Since rank time is proportional to
  ;; the number of bits this is important.

  #+*LISP-HARDWARE
  (*compile-blindly
    (*locally
      (declare (type (field-pvar *) field-pvar))
      (let ((size (*integer-length field-pvar)))
	(if (eql size (pvar-length field-pvar))
	    field-pvar
	    (*let ((result field-pvar))
	      (declare (type (field-pvar size) result))
	      (declare (return-pvar-p t))
	      result
	      )))))

  #+*LISP-SIMULATOR
  field-pvar

  )

(defun signed-to-ordered-bits!! (signed-pvar)

  ;; If there are no negative values just convert
  ;; the pvar to be unsigned.  If there are negative
  ;; values add the negation of the most negative
  ;; value to all values to produce an unsigned
  ;; result.  Once an unsigned number has been
  ;; obtained, reduce it to the smallest number of
  ;; required bits.

  (*compile-blindly
    (*locally
      (declare (type (signed-pvar *) signed-pvar))
      (let ((min (*min signed-pvar)))
	(if (or (null min) (not (minusp min)))
	    (*let ((result signed-pvar))
	      (declare (type (unsigned-pvar (pvar-length signed-pvar)) result))
	      (declare (return-pvar-p t))
	      (smallest-possible-field-pvar result)
	      )
	    (*let ((result (+!! (!! (the fixnum (- min))) signed-pvar)))
	      (declare (type (unsigned-pvar (1+ (pvar-length signed-pvar))) result))
	      (declare (return-pvar-p t))
	      (smallest-possible-field-pvar result)
	      ))))))


#+*LISP-HARDWARE
(defun float-to-ordered-bits!! (float-pvar)

  "Rearrange the bits of a float so that when they
are considered as an unsigned integer, they are ordered
in the same sense that the original floating point numbers
were ordered.  This obviously depends on the internal
representation of floats, which for floats is IEEE.
It is an error if the argument float-pvar is not a float pvar."

  (let* ((length (pvar-length float-pvar))
	(sign-bit-pos (1- length))
	)
    (*locally
      (declare (type (integer 0 256) length sign-bit-pos))

      (*let ((bits (taken-as!! (the (float-pvar * *) float-pvar) '(field-pvar length))))
	(declare (type (field-pvar length) bits))
	nil
    
	;; first, we want to switch the sign bit,
	;; since negative is 1 and positive is 0
	;; and we want positive numbers to order
	;; bigger than negative ones.

	(*let ((sign (load-byte!! bits (!! sign-bit-pos)  (!! 1)))
	       (rest (load-byte!! bits (!! 0) (!! sign-bit-pos)))
	       )
	  (declare (type (pvar (unsigned-byte 1)) sign))
	  (declare (type (pvar (unsigned-byte sign-bit-pos)) rest))
	  (declare (return-pvar-p nil))
	  nil

	  (*set sign (if!! (zerop!! sign) (!! 1) (!! 0)))

	  ;; if the float was negative, we want to
	  ;; negate the rest of the bits because the
	  ;; exponent and mantissa are in twos
	  ;; complement.

	  (*when (zerop!! sign)
	    (*set rest (load-byte!! (lognot!! rest) (!! 0) (!! sign-bit-pos)))
	    )

	  ;; Now put the revised sign and other bits back together
	  ;; as a single unsigned integer.

	  (*set bits sign)
	  (*set bits (ash!! bits (!! sign-bit-pos)))
	  (*set bits (+!! bits rest))

	  )

	;; finally, make sure the unsigned integer
	;; has the fewest number of bits it needs.

	(smallest-possible-field-pvar bits)

	))))


#+*LISP-SIMULATOR
(defun float-to-ordered-bits!! (float-pvar)
  (*let ((double-float (float!! float-pvar (!! pi)))
	 result
	 )
    nil
    (multiple-value-bind (m e s)
    #-KCL
	(integer-decode-float most-positive-double-float)
    #+KCL
        (integer-decode-float (symbol-value 'most-positive-double-float))
      (declare (ignore s))
      (let ((integer-mantissa-size (+ 5 (integer-length m)))
	    (integer-exponent-size (+ 5 (integer-length e)))
	    )
	(do-for-selected-processors-internal (j)
	  nil
	  (multiple-value-bind (mantissa exponent sign)
	      (integer-decode-float (pref double-float j))
	    (*setf (pref result j)
		   (* sign (dpb exponent (byte integer-exponent-size integer-mantissa-size) mantissa))
		   ))))
      result
      )))

;;;; ***** END RANK!! ****



(defun test-segmented-cube-rank!! ()
  (macrolet
    ((typed-segmented-cube-rank-test (type initialization-expression)
       `(*let ((pvar ,initialization-expression)
	       (segment-pvar (evenp!! (self-address!!)))
	       result
	       supposed-result
	       )
	  (declare (type ,type pvar))
	  (declare (type (field-pvar 8) result supposed-result))
	  (declare (type boolean-pvar segment-pvar))
	  (format t "~%Testing segmented cube rank for type ~S" ',type)
	  (*nocompile (*set result (rank!! pvar '<=!! :segment-pvar segment-pvar)))
	  (*set supposed-result
		(if!! (evenp!! (self-address!!))
		      (if!! (=!! pvar (pref!! pvar (1+!! (self-address!!))))
			    (!! 2)
			    (if!! (<=!! pvar (pref!! pvar (1+!! (self-address!!))))
				  (!! 0)
				  (!! 1)
				  ))
		      (if!! (=!! pvar (pref!! pvar (1-!! (self-address!!))))
			    (!! 2)
			    (if!! (>=!! pvar (pref!! pvar (1-!! (self-address!!))))
				  (!! 1)
				  (!! 0)
				  ))))
	  (*when (/=!! result supposed-result)
	    (*when (/=!! supposed-result (!! 2))
	      (when (*or t!!)
		(print (*min (self-address!!)))
		))))))
    (typed-segmented-cube-rank-test (field-pvar 8) (random!! (!! 10)))
    (typed-segmented-cube-rank-test (signed-pvar 8) (-!! (random!! (!! 10)) (!! 5)))
    (typed-segmented-cube-rank-test (float-pvar 23 8) (random!! (!! 100.0)))
    ))


(defun test-grid-rank!! ()
  (assert (*nocompile (*and (=!! (self-address-grid!! (!! 0)) (rank!! (self-address-grid!! (!! 0)) '<=!! :dimension 0)))))
  )

(defun test-grid-segmented-cube-rank!! ()
  (*let ((pvar (random!! (!! 10)))
	 (segment-pvar (evenp!! (self-address-grid!! (!! 0))))
	 result
	 )
    (declare (type (field-pvar 8) pvar result))
    (declare (type boolean-pvar segment-pvar))
    (*nocompile (*set result (rank!! pvar '<=!! :segment-pvar segment-pvar :dimension 0)))
    (*let (supposed-result)
      (*set supposed-result
	    (if!! (evenp!! (self-address-grid!! (!! 0)))
		  (if!! (=!! pvar (news!! pvar 1 0))
			(!! 2)
			(if!! (<=!! pvar (news!! pvar 1 0))
			      (!! 0)
			      (!! 1)
			      ))
		  (if!! (=!! pvar (news!! pvar -1 0))
			(!! 2)
			(if!! (>=!! pvar (news!! pvar -1 0))
			      (!! 1)
			      (!! 0)
			      ))))
      (assert (*and (or!! (=!! supposed-result (!! 2))
			  (=!! supposed-result result)
			  )))
      )))


#+*LISP-HARDWARE
(defmacro with-copied-pvar ((old-pvar new-pvar &optional new-pvar-name) &body body)

  ;; Create a new pvar with the same properties as an old pvar
  ;; except that it has a different location, and so is a copy.

  (assert (and (symbolp old-pvar) (symbolp new-pvar)))
  (let ((location-symbol (gensym "PVAR-LOCATION-")))
    `(let ((,new-pvar (allocate-field-pvar (pvar-length ,old-pvar))))
       (let ((,location-symbol (pvar-location ,new-pvar)))
	 (copy-pvar-contents ,new-pvar ,old-pvar)
	 (setf (pvar-location ,new-pvar) ,location-symbol)
	 ,@(if new-pvar-name `((setf (pvar-name ,new-pvar) ',new-pvar-name)))
	 ,@body
	 ))))

#+*LISP-SIMULATOR
(defmacro with-copied-pvar ((old-pvar new-pvar &optional new-pvar-name) &body body)
  (assert (and (symbolp old-pvar) (symbolp new-pvar)))
  `(*let ((,new-pvar ,old-pvar))
     ,@(if new-pvar-name `((setf (pvar-name ,new-pvar) ',new-pvar-name)))
     ,@body
     ))

;;;; **** START SORT ****

;;;; If key is specified, then the key must be a function of a
;;;; single argument.  The KEY is applied to PVAR to obtain
;;;; the object to be used for comparisions.  The entire
;;;; PVAR is sorted.  The dimension and segment-pvar arguments
;;;; have the obvious semantics.


(defun sort!! (pvar predicate &key dimension segment-pvar key)

  (simple-pvar-argument!! pvar &opt segment-pvar)

  (safety-check
    (new-vp-pvar-check pvar 'sort!!)
    (when segment-pvar (new-vp-pvar-check segment-pvar 'sort!!))
    (when (eq predicate (symbol-function '<=!!)) (setq predicate '<=!!))
    (when (not (eq predicate '<=!!)) (error "Implementation limitation.  Predicate ~S must be <=!!" predicate))
    (when dimension
      (when (or (not (integerp dimension)) (not (< -1 dimension *number-of-dimensions*)))
	(error "You specified dimension ~S, but there are ~D dimensions in the current vp set"
	       dimension *number-of-dimensions*
	       )))
    )

  (when (or (eq key 'identity) (eq key #'identity)) (setq key nil))

  (without-void-pvars (pvar)
    (if (null segment-pvar)
	(internal-sort!! pvar predicate dimension segment-pvar key)
	(without-void-pvars (segment-pvar)
	  (let ((canonical-segment (canonical-segment segment-pvar)))
	    (internal-sort!! pvar predicate dimension canonical-segment key)
	    ))))

  )


(defun stable-sort!! (pvar predicate &key dimension segment-pvar key)
  (let ((*stable-rank* t))
    (sort!! pvar predicate :dimension dimension :segment-pvar segment-pvar :key key)
    ))



(defun internal-sort!! (pvar predicate dimension segment-pvar key)

  (declare (ignore predicate))

  (let ((key-pvar (if (null key) pvar (funcall key pvar))))

    (cond

      ((and (null dimension) (null segment-pvar)) (unsegmented-cube-sort!!-internal pvar key-pvar))

      ((and (null dimension) segment-pvar) (segmented-cube-sort!!-internal pvar segment-pvar key-pvar))

      ((and dimension (null segment-pvar)) (unsegmented-grid-sort!!-internal pvar dimension key-pvar))

      ((and dimension segment-pvar) (segmented-grid-sort!!-internal pvar dimension segment-pvar key-pvar))

      )))


#+*LISP-SIMULATOR
(eval-when (:compile-toplevel :load-toplevel :execute) 
  (defmacro the-defined-field (x) x)
  )


(defun-wcefi unsegmented-cube-sort!!-internal (pvar key-pvar)

  ;; If all the processors involved in the sort are contiguous then we can avoid having to do a pack and unpack

  (let ((min-self-address (*min (self-address!!)))
	(max-self-address (*max (self-address!!))))
	
    (cond

      ;; if no processors are selected, then just return

      ((null min-self-address) pvar)
	      
      ((all-active-processors-contiguous-p min-self-address max-self-address)
       (rank-and-send-for-sort!! pvar key-pvar min-self-address)
       )

      (t

       ;; The selected processors are not continuous.  

       (with-copied-pvar (pvar result sort!!-return)

	 (*let (rank ranked-data-p rendevous)
	   (declare (type (field-pvar *current-send-address-length*) rank rendevous))
	   (declare (type boolean-pvar ranked-data-p))
	   (declare (return-pvar-p nil))
	   nil

	   ;; First rank the data.

	   (*nocompile (*set rank (rank!! key-pvar '<=!!)))

	   (*all (*set ranked-data-p nil!!))	;clear the bit in all processors
		 
	   ;; Send the data to be sorted to the processor identified
	   ;; by the rank.  These processors are the first N processors,
	   ;; where N is the number of active processors.

	   (*pset :no-collisions
		  (the-defined-field pvar)
		  (the-defined-field result)
		  rank
		  :notify ranked-data-p
		  )
		 
	   ;; now have all the selected processors pack their
	   ;; self-addresses into the lower processors in the machine

	   (*pset :no-collisions (self-address!!) rendevous (enumerate!!))
		 
	   ;; Now each processor numbered 0 through N-1 has an address to
	   ;; send back to (in rendevous) and the data associated with
	   ;; that rank (in result).  We now send the reordered data
	   ;; back to the active processors.

	   (*all
	     (declare (return-pvar-p nil))
	     (*when ranked-data-p
	       (*pset :no-collisions (the-defined-field result) (the-defined-field result) rendevous)
	       ))

	   )

	 result

	 ))

      )))


(defun all-active-processors-contiguous-p (min-self-address max-self-address)
  (*compile-blindly
    (not (*let (temp-bit)
	   (declare (type boolean-pvar temp-bit))
	   (declare (return-pvar-p nil))
	   nil
	   (*all (*set temp-bit nil!!))
	   (*set temp-bit t!!)
	   (*all
	     (declare (return-pvar-p nil))
	     nil
	     (*or (and!! (not!! temp-bit)
			 (<=!! (!! (the fixnum min-self-address))
			       (self-address!!)
			       (!! (the fixnum max-self-address))
			       ))))))))




(defun rank-and-send-for-sort!! (data-pvar key-pvar min-self-address)

  ;; Create a destination pvar of the same type as the data pvar,
  ;; then rank the key pvar and send the data-pvar via the
  ;; rank ordering to the destination offset by the min-self-address.
  ;; By definition all the active data-pvar values are contiguous
  ;; in cube address space.

  (*compile-blindly
    (with-copied-pvar (data-pvar result sort!!-return)
      (*let (rank)
	(declare (type (pvar (unsigned-byte *current-send-address-length*)) rank))
	(declare (return-pvar-p nil))
	(*nocompile (*set rank (internal-rank!! key-pvar '<=!! nil nil)))
	(*pset :no-collisions
	       (the-defined-field data-pvar)
	       (the-defined-field result) 
	       (+!! (!! (the fixnum min-self-address)) rank)
	       ))
      result
      )))


;;;; To do a segmented-cube-sort!!

;;;; 1.  Do a segmented cube rank using the key.
;;;; 2.  Copy scan the self address of the beginning of
;;;;     the segment out to all the segment processors.
;;;; 3.  Enumerate the processors in each segment using copy-scan of (!! 1)
;;;; 4.  Each processor sends its data to the processor identified
;;;;     by the scanned out self address plus the rank value.
;;;; 5.  Each processor gets data from the processor identified
;;;;     by the scanned out self address plus the enumeration.


(defun segmented-cube-sort!!-internal (pvar segment-pvar key-pvar)
  (*compile-blindly
    (*locally
      (declare (type (field-pvar *) pvar key-pvar))
      (declare (type boolean-pvar segment-pvar))
      (let ((length #+*LISP-HARDWARE (pvar-length pvar) #+*LISP-SIMULATOR 32))
	#+*LISP-SIMULATOR
	(setq length length)  ;; hack to make compiler not issue warning
	(with-copied-pvar (pvar dest sort!!-return)
	  (*let (rank start-self-address enumeration)
	    (declare (type (field-pvar *current-send-address-length*) rank start-self-address enumeration))
	    (*nocompile (*set rank (rank!! key-pvar '<=!! :segment-pvar segment-pvar)))
	    (*set start-self-address (scan!! (self-address!!) 'copy!! :segment-pvar segment-pvar))
	    (*set enumeration (1-!! (scan!! (!! 1) '+!! :segment-pvar segment-pvar)))
	    (*pset :no-collisions 
		   ;;(the (field-pvar length) pvar) 
		   pvar
		   ;;(the (field-pvar length) dest)
		   dest
		   (+!! start-self-address rank)
		   )
	    (*set ;;(the (field-pvar length) dest)
		  dest
		  (pref!! ;;(the (field-pvar length) dest)
		          dest
			  (+!! start-self-address enumeration) 
			  :collision-mode :no-collisions))
	    )
	  dest
	  )))))


(defun unsegmented-grid-sort!!-internal (pvar dimension-constant key)
  (segmented-grid-sort!!-internal
    pvar dimension-constant (t!!-in-first-active-processor-along-row dimension-constant) key
    ))


(defun segmented-grid-sort!!-internal (pvar dimension-constant segment-pvar key-pvar)

  ;; This is the same algorithm as segmented-cube-sort!!-internal.
  ;; In place of a cube address, we use an address object which
  ;; has the property that it is independent of the number of
  ;; dimensions in the current Vp Set and a component along
  ;; a specified dimension can be incremented to obtain a
  ;; new address using the function address-plus-nth!!, just
  ;; as a send address can be incremented simply by using addition.


  (*compile-blindly

    (*locally
      (declare (type (field-pvar *) pvar key-pvar))
      (declare (type boolean-pvar segment-pvar))

      (let ((length #+*LISP-HARDWARE (pvar-length pvar) #+*LISP-SIMULATOR 32))
	#+*LISP-SIMULATOR
	(setq length length) ; hack to make compiler not issue a warning.
	(with-copied-pvar (pvar dest sort!!-return)
	  (*let (rank enumeration start-address-object)
	    (declare (type (field-pvar *current-send-address-length*) rank enumeration))
	    (declare (type (pvar address-object) start-address-object))
	    nil
	    (*nocompile
	      (*set rank
		    (rank!! key-pvar '<=!! :segment-pvar segment-pvar :dimension dimension-constant))
	      (*set start-address-object
		    (scan!! (self!!) 'copy!! :segment-pvar segment-pvar :dimension dimension-constant))
	      )

	    (*set enumeration
		  (1-!! (scan!! (!! 1) '+!! :segment-pvar segment-pvar :dimension dimension-constant)))
	    (progn
	      (*nocompile
		(*pset :no-collisions ;; (the (field-pvar length) pvar)
                        pvar ;; (the (field-pvar length) dest)
                        dest
		       (address-plus-nth!! start-address-object rank (!! (the fixnum dimension-constant)))
		       ))
	      (*set ;; (the (field-pvar length) dest)
                    dest
		    (pref!! ;;(the (field-pvar length) dest)
                            dest
			    (address-plus-nth!! start-address-object enumeration (!! (the fixnum dimension-constant)))
			    :collision-mode :no-collisions))
	      ))
	  dest
	  )))))



;;;; **** END NEW SORT ****




(*proclaim '(ftype (function (boolean-pvar) boolean-pvar) last-active-processor-in-segment!!))


(defun last-active-processor-in-segment!! (segment-pvar)
  (*locally
    (declare (type boolean-pvar segment-pvar))
    (*let (last-active-processors)
      (declare (type boolean-pvar last-active-processors))
      (declare (return-pvar-p t))
      (let ((last-active-processor (*max (self-address!!))))
	(*let (css count last-processor-in-segment)
	  (declare (type boolean-pvar css last-processor-in-segment))
	  (declare (type (field-pvar *current-send-address-length*) count))
	  (declare (return-pvar-p nil))
	  (*all (*set css nil!!) (*set last-processor-in-segment nil!!))
	  (*set css t!!)
	  (*when (and!! segment-pvar (plusp!! (self-address!!)))
	    (*pset :no-collisions segment-pvar last-processor-in-segment (1-!! (self-address!!)))
	    )
	  (*setf (pref last-processor-in-segment last-active-processor) t)
	  (*all
	    (*set count (if!! css (!! 1) (!! 0)))
	    (*set count (scan!! count '+!! :direction :backward :segment-pvar last-processor-in-segment))
	    (*set last-active-processors (=!! (!! 1) count))
	    )))
      last-active-processors
      )))


;(defun-wcefi segmented-reduce-and-spread!! (pvar operator dimension-constant segment-pvar)
;
;  (safety-check
;    (new-vp-pvar-check pvar 'segmented-reduce-and-spread!!)
;    (new-vp-pvar-check segment-pvar 'segmented-reduce-and-spread!!)
;    (assert (or (null dimension-constant) (< -1 dimension-constant *number-of-dimensions*)) ()
;	    "You asked to spread across dimension ~D, but only ~D dimensions exist"
;	    dimension-constant *number-of-dimensions*
;	    ))
;
;    (without-void-pvars (pvar segment-pvar)
;
;      (*let (boolean-segment-pvar last-processor-in-segment-pvar)
;	(declare (type boolean-pvar boolean-segment-pvar last-processor-in-segment-pvar))
;	(*nocompile (*set boolean-segment-pvar segment-pvar))
;	(*set last-processor-in-segment-pvar (last-active-processor-in-segment!! boolean-segment-pvar))
;	(scan!!
;	  (scan!! pvar operator :dimension dimension-constant :
;
;


;;;; Element pvar is a pvar which has a single T value in any
;;;; given segment.  It is an error is this is not true.

;;;; The value of pvar singled out by element pvar is spread to
;;;; all the active processors in the segment.


(defun-wcefi segmented-spread!! (pvar dimension-constant segment-pvar element-pvar)

  (safety-check
    (new-vp-pvar-check pvar 'segmented-spread!!)
    (new-vp-pvar-check segment-pvar 'segmented-spread!!)
    (new-vp-pvar-check element-pvar 'segmented-spread!!)
    )
    
  (without-void-pvars (pvar segment-pvar element-pvar)

    (*locally
      (declare (type (field-pvar (pvar-length pvar)) pvar))
      (*let (boolean-segment-pvar boolean-element-pvar start-of-segment-address)
	(declare (type boolean-pvar boolean-segment-pvar boolean-element-pvar))
	(declare (type (field-pvar *current-send-address-length*) start-of-segment-address))
	(*nocompile
	  (*set boolean-segment-pvar segment-pvar)
	  (*set boolean-element-pvar element-pvar)
	  )
      
	(with-copied-pvar (pvar result-pvar segmented-spread!!-return)

	  (*locally
	    (declare (type (field-pvar (pvar-length pvar)) result-pvar))

	    ;; Simply figure out the address of the first processor
	    ;; of each segment, and send the data from the element
	    ;; identified by element-pvar to the beginning of the
	    ;; segment.  Then copy scan the data to all the processors
	    ;; in the segment.

	    (cond
	
	      ((null dimension-constant)
	       (*set start-of-segment-address (scan!! (self-address!!) 'copy!! :segment-pvar boolean-segment-pvar))
	       (*when boolean-element-pvar
		 (*pset :no-collisions pvar result-pvar start-of-segment-address)
		 )
	       (*set result-pvar (scan!! result-pvar 'copy!! :segment-pvar boolean-segment-pvar))
	       result-pvar
	       )

	      ((and (integerp dimension-constant) (< -1 dimension-constant *number-of-dimensions*))
	       (*set start-of-segment-address
		     (scan!! (self-address!!) 'copy!! :dimension dimension-constant :segment-pvar boolean-segment-pvar))
	       (*when boolean-element-pvar
		 (*pset :no-collisions pvar result-pvar start-of-segment-address)
		 )
	       (*set result-pvar
		     (scan!! result-pvar 'copy!! :dimension dimension-constant :segment-pvar boolean-segment-pvar))
	       result-pvar
	       )

	      )))))))


