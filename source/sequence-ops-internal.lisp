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


(defun check-zero-length-sequence-start-and-end (start end)
  (*nocompile
    (if (not (*and (and!! (zerop!! start) (zerop!! end))))
	(error "The start and end indices for a zero length sequence must be zero everywhere")
	)))


(defun proper-start-and-end-check (start end sequence-length function)
  
  (*let (pvar-sequence-length)

    (if (numberp sequence-length)
	(*set pvar-sequence-length (!! (the fixnum sequence-length)))
	(*set pvar-sequence-length sequence-length)
	)

    (flet
      ((check-range (pvar parameter-name)
	 (assert (*and (and!! (integerp!! pvar) (>=!! pvar (!! 0)))) ()
		 "The ~S parameter to function ~S is somewhere negative or contains a non-integer value"
		 parameter-name function
		 )
	 (assert (*and (<=!! pvar pvar-sequence-length)) ()
		 "The ~S parameter to function ~S somewhere exceeds the length of the sequence (~D)"
		 parameter-name function (*max pvar-sequence-length)
		 )))

      (check-range start 'start)
      (check-range end 'end)

      (assert (*and (<=!! start end)) ()
	      "The start parameter to function ~S is somewhere greater than the end parameter"
	      function
	      )

      )))


(defun with-start-and-end-checked-and-coerced (start end length function-name function-of-start-and-end)
  (safety-check
    (proper-start-and-end-check start end length function-name)
    )
  (funcall function-of-start-and-end start end)
  )


(defmacro without-void-pvar-list (pvar-list-symbol &body body)
  (assert (symbolp pvar-list-symbol))
  (let ((void-pvar-symbol (gensym "VOID-PVAR-")))
    `(let ((,void-pvar-symbol (find-if 'void-pvar-p ,pvar-list-symbol)))
       (if (and ,void-pvar-symbol (*or t!!))
	   (sequence-pvar-check ,void-pvar-symbol nil)
	   (progn ,@body)
	   ))))



(*defun *map-vectors-nocheck (function start end backwards? function-expects-index-as-first-argument? &rest vector-pvars)
  
  ;; Apply function, which should be a function taking (length array-pvars)
  ;; pvar arguments and not returning any value of significance, to
  ;; each array element of each array pvar element of array-pvars.
  
  ;; If function-expects-index-as-first-argument? is true, then the
  ;; function will get as its first argument the index of the elements
  ;; it is being called with, and then the elements.

  ;; If there are no arrays there is nothing to do.
  
  (when (null vector-pvars) (return-from *map-vectors-nocheck nil))
  
  ;; If there are no processors active there is nothing to do.

  (when (not (*or t!!)) (return-from *map-vectors-nocheck nil))

  ;; This is a hack to allow us to either pass in a precreated list of vectors
  ;; or many different vectors not in a list.

  (let ((is-really-&rest (listp (car vector-pvars))))
    
    (when is-really-&rest (setq vector-pvars (car vector-pvars)))
    
    (let ((start-and-end-constant (and (pvar-constant-value start) (pvar-constant-value end)))
	  (min-index (if (pvar-constant-value start) (pvar-constant-value start) (*min start)))
	  (max-index (if (pvar-constant-value end) (pvar-constant-value end) (*max end)))
	  element-pvar-list
	  current-index
	  )
	    
      (flet
	((call-function (index) 
	   (if function-expects-index-as-first-argument?
	       (apply function index element-pvar-list)
	       (apply function element-pvar-list)
	       ))
	 (decf-pvars ()
	   (decf current-index)
	   (setq element-pvar-list
		 (mapcar
		   #'(lambda (vector-pvar) (alias!! (aref!! vector-pvar (!! current-index))))
		   vector-pvars
		   )))
	 (incf-pvars ()
	   (incf current-index)
	   (setq element-pvar-list
		 (mapcar
		   #'(lambda (vector-pvar) (alias!! (aref!! vector-pvar (!! current-index))))
		   vector-pvars
		   ))))

	;; Set up the current-index initially.

	(if backwards?
	    (setq current-index max-index)
	    (setq current-index (1- min-index))
	    )

	;; Call the function on the vector elements,
	;; and proceed on to the next/previous set of elements.
	    
	(let ((*interpreter-safety* 0))

	  (if backwards?

	      (if start-and-end-constant
		  (do ((index (1- max-index) (decf index)))
		      ((< index min-index))
		    (decf-pvars)
		    (call-function index)
		    )
		  (do ((index (1- max-index) (decf index)))
		      ((< index max-index))
		    (decf-pvars)
		    (*when (and!! (>=!! (!! (the fixnum index)) start) (<!! (!! (the fixnum index)) end))
		      (call-function index)
		      )
		    ))

	      (if start-and-end-constant
		  (do ((index min-index (incf index)))
		      ((>= index max-index))
		    (incf-pvars)
		    (call-function index)
		    )
		  (do ((index min-index (incf index)))
		      ((>= index max-index))
		    (incf-pvars)
		    (*when (and!! (>=!! (!! (the fixnum index)) start) (<!! (!! (the fixnum index)) end))
		      (call-function index)
		      )
		    ))))
	    
	))))


(defun copy-seq!!-internal (sequence)
  (*let (result)
    (*set result sequence)
    result
    ))


(defun *nreverse-internal (sequence)
  (let ((vector-length (array-pvar-total-size sequence)))
    (*let (temp)
      (dotimes (j (floor (/ vector-length 2)))
	(let ((other-index (- vector-length (1+ j))))
	  (*set temp (aref!! sequence (!! j)))
	  (*setf (aref!! sequence (!! j)) (aref!! sequence (!! other-index)))
	  (*setf (aref!! sequence (!! other-index)) temp)
	  ))))
  sequence
  )


(defun reverse!!-internal (sequence)
  (*let ((result sequence))
    (*nreverse-internal result)
    result
    ))


(defun subseq!!-internal (sequence start end)

  (let* ((vlength (array-pvar-total-size sequence))
	 (element-type (pvar-array-element-type sequence))
	 result-size
	 min-index
	 (start-constant (pvar-constant-value start))
	 (end-constant (pvar-constant-value end))
	 (start-and-end-constant (and start-constant end-constant))
	 )

    ;; Check the start and end parameters against
    ;; the length of the sequence and each other.

    (with-start-and-end-checked-and-coerced

      start end vlength 'subseq!!

      #'(lambda (start end)

	  ;; Subseq!! insists that all the subsequence lengths specified
	  ;; must be the same.  Check this and set result-size to
	  ;; how long a vector we must allocate for the result.

	  (if start-and-end-constant
	      (setq result-size (- (pvar-constant-value end) (pvar-constant-value start)))
	      (progn
		(setq result-size (*min (-!! end start)))
		(if (null result-size) (setq result-size 0))
		(assert (*and (=!! (!! (the fixnum result-size)) (-!! end start))) ()
			"The subsequences must all be the same length."
			)))
	
	  ;; Allocate an array pvar as our result.

	  (let ((dest (make-array-pvar
			:stack
			`(pvar (array ,(cadr element-type) (,result-size)))
			)))
	    (setf (pvar-lvalue? dest) t)
	  
	    ;; Now copy the data from the original sequence pvar
	    ;; to our result pvar.  Do this by selecting all
	    ;; the processors with a given start index, and copying
	    ;; the proper number of elements into the result
	    ;; pvar starting at the start index.  Then turn
	    ;; off all the processors with that index and do
	    ;; the same for the next index.  Exit when we have
	    ;; exhausted the possible start indices.

	    (when (plusp result-size)
	      (*let ((selected-set t!!))
		(with-css-saved
		  (block while-more-indices
		    (loop
		      (*when selected-set
			(unless (*or t!!) (return-from while-more-indices nil))
			(setq min-index (*min start))
			(*when (=!! (!! (the fixnum min-index)) start)
			  (*set selected-set nil!!)
			  (dotimes (j result-size)
			    (*setf (aref!! dest (!! j)) (aref!! sequence (+!! start (!! j))))
			    ))))))))

	    (setf (pvar-lvalue? dest) nil)

	    dest
	  
	    )))))


(defun *fill-internal (sequence item start end)

  (let ((vlength (array-pvar-total-size sequence)))
    
    (when (zerop vlength)
      (check-zero-length-sequence-start-and-end start end)
      (return-from *fill-internal sequence)
      )

    (*when (not!! (zerop!! (-!! end start)))
      (*map-vectors-nocheck
	#'(lambda (dest) (*compile () (*set dest item)))
	start end nil nil sequence
	))

    sequence

    ))
