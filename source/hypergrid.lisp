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


(defun valid-coordinates-p
       (coordinates &optional (limit-coordinates *current-cm-configuration*))
  (and (or (listp coordinates) (vectorp coordinates))
       (eql (length coordinates) *number-of-dimensions*)
       (every #'(lambda (x) (and (integerp x) (>= x 0))) coordinates)
       (every #'< coordinates limit-coordinates)))

(defun valid-limit-coordinates-p
       (coordinates &optional (limit-coordinates *current-cm-configuration*))
  (and (or (listp coordinates) (vectorp coordinates))
       (eql (length coordinates) *number-of-dimensions*)
       (every #'(lambda (x) (and (integerp x) (> x 0))) coordinates)
       (every #'<= coordinates limit-coordinates)))


(defun next-grid-coordinates

       (start-coordinates end-coordinates current-coordinates mask n-dimensions direction)

  (labels

    ;; This function increments the current-coordinates in place.  It returns
    ;; current-coordinates if it has successfully incremented, or it returns NIL
    ;; if it cannot increment further.  The value of current-coordinates when
    ;; the function returns NIL is undefined.

    ((increment-component (component-index)

       ;; if we haven't exhausted this dimensions possible indices, just increment the
       ;; index and return.

       (if (< (elt current-coordinates component-index)
	      (1- (elt end-coordinates component-index)))

	   (incf (elt current-coordinates component-index))

	   ;; Otherwise, if this is the last index, return nil.

	   (if (last-dimension-p component-index)

	       nil

	       ;; Otherwise, find the next dimension to be interated over.
	       ;; A mask of NIL indicates that the dimension is not to be
	       ;; iterated over.  If there are no more dimensions to be
	       ;; iterated over next-unmasked-component returns nil and
	       ;; we return nil.

		 (let ((next (next-unmasked-component component-index mask)))

		   (if (null next)

		       nil

		       ;; Otherwise set the previous dimensions index back to
		       ;; its original value (usually 0), and recurse using the
		       ;; next dimension to be iterated over.

		       (progn
			 (setf (elt current-coordinates component-index)
			       (elt start-coordinates component-index))
			 (increment-component next)
			 )

		       )))))

     ;; This function finds the next dimension that should be iterated over,
     ;; given the current dimension and the mask of which dimensions to
     ;; iterate over.  If no such dimension exists it returns nil.

     (next-unmasked-component (current-component-index mask)
       (if (last-dimension-p current-component-index)
	   nil
	   (let ((next (next-component current-component-index)))
	     (if (null (elt mask next))
	       (next-unmasked-component next mask)
	       next
	       ))))

     (first-unmasked-component (first-component mask)
       (if (elt mask first-component)
	   first-component
	   (if (last-dimension-p first-component)
	       nil
	       (first-unmasked-component (next-component first-component) mask))))

     (last-dimension-p (dimension-index)
       (or (and (eq direction :forward) (eql dimension-index (1- n-dimensions)))
	   (and (eq direction :backward) (zerop dimension-index))))

     (next-component (current)
       (if (eq direction :forward) (1+ current) (1- current)))
	       
     )

    ;; if we can successfully increment, return the incremented coordinates,
    ;; otherwise return nil.  Incrementing is always done on the last dimension
    ;; first.

    (let* ((first-component (if (eq direction :forward) 0 (1- n-dimensions)))
	   (first-unmasked-component (first-unmasked-component first-component mask))
	   )
      (if (null first-unmasked-component)
	  nil
	  (if (increment-component first-unmasked-component)
	      current-coordinates
	      nil
	      )))

    ))


(defun check-args-for-with-grid-indices-iterated

       (direction bind-as mask start end n-dimensions)

  (assert (member direction '(:backward :forward)))
  (assert (member bind-as '(:list :vector)))
  (assert (or (null mask)
	      (and (listp mask)
		   (eql (length mask) n-dimensions)
		   (every #'(lambda (x) (or (eq x t) (eq x nil))) mask)
		   )))
  (when start (assert (valid-coordinates-p start)))
  (when end (assert (valid-limit-coordinates-p end)))
  (when (and start end ) (assert (every #'< start end)))
 )




