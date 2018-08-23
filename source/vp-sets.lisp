;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *SIM-i; MUSER: YES-*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


(defstruct (vp-set (:print-function print-vp-set))
  name 
  geometry-allocation-form
  geometry
  internal-id 
  (nesting-level 0)
  t!!
  nil!!
  border-bits
  allocated
  instantiated
  voidable
  deallocated
  #+*LISP-SIMULATOR
  self-address!!
  #+*LISP-SIMULATOR
  constants-hash-table
  #+*LISP-SIMULATOR
  array-of-grid-addresses
  #+*LISP-SIMULATOR
  context-stack
  #+*LISP-SIMULATOR
  pvar-array-pool
  #+*LISP-SIMULATOR
  (context-level 0 :type fixnum)
  #+*LISP-SIMULATOR
  array-of-cube-addresses
  #+*LISP-SIMULATOR
  (heap-pvar-arrays nil)
  spare2
  )


(defun print-vp-set (vp-set stream depth)
  (declare (ignore depth))
  (format stream "#<VP-SET Name: ~A~@[, Dimensions ~A~]~@[, Geometry-id: ~A~]>"
	  (vp-set-name vp-set)
	  (if (vp-set-geometry vp-set) (geometry-dimensions (vp-set-geometry vp-set)) nil)
	  (if (vp-set-geometry vp-set) (geometry-id (vp-set-geometry vp-set)) nil)
	  ))


;;; We store *DEFVAR definitions in a *DEFVAR-SPECIFICATION
;;; structure.


(defstruct *defvar-specification
  name
  (initial-value-form nil)
  (vp-set-name nil)
  (in-vp-set-definition-p nil)
  (initial-value-function nil)
  (proclaimed-type nil))

;;;; Create the default VP-SET at Starlisp load time so it will always exist.

(eval-when (:load-toplevel :execute)
  (when (null *default-vp-set*)
    (setq *default-vp-set*
	  (make-vp-set
	    :name '*default-vp-set*
	    ))))


(defmacro vp-set-geometry-dimensions (vp-set) `(geometry-dimensions (vp-set-geometry ,vp-set)))
(defmacro vp-set-geometry-rank (vp-set) `(geometry-rank (vp-set-geometry ,vp-set)))
(defmacro vp-set-geometry-id (vp-set) `(geometry-id (vp-set-geometry ,vp-set)))

(defun vp-set-address-length (vp-set) (geometry-address-length (vp-set-geometry vp-set)))
(defun vp-set-grid-address-lengths (vp-set) (geometry-grid-address-lengths (vp-set-geometry vp-set)))
(defun vp-set-grid-address-length (vp-set n) (nth n (vp-set-grid-address-lengths vp-set)))

(defun find-geometry-that-fits-coordinates (coordinates)
  
  ;; if the current vp set's geometry works, use that,
  ;; otherwise search for a geometry that will fit,
  ;; otherwise if none will fit create one that will.
  
  (if (and *current-vp-set* 
           (coordinates-fit-into-geometry-p 
            (funcall 'vp-set-geometry *current-vp-set*) coordinates))
      (vp-set-geometry *current-vp-set*)
      (progn
	(do-for-active-geometries (geometry)
	  (when (coordinates-fit-into-geometry-p geometry coordinates)
	    (return-from find-geometry-that-fits-coordinates geometry)
	    ))
	(make-geometry-that-can-hold-coordinates coordinates)
	)))

(defun vp-set-instantiated-p (vp-set) (vp-set-instantiated vp-set))

(defun vp-set-dimensions (vp-set) (vp-set-geometry-dimensions vp-set))

(defun vp-set-total-size (vp-set) (apply '* (vp-set-dimensions vp-set)))

(defun vp-set-size (vp-set) (vp-set-total-size vp-set))

(defun vp-set-vp-ratio (vp-set) (/ (vp-set-total-size vp-set) *minimum-size-for-vp-set*))

(defun vp-set-rank (vp-set) (length (vp-set-geometry-dimensions vp-set)))


(defun check-cube-address (index vp-set function-name)
  (assert (valid-integer-range-exclusive index 0 (vp-set-size vp-set)) ()
	  "In ~S, value for cube address, ~S, is not between 0 and ~D, the size of vp set ~S"
	  function-name index (vp-set-size vp-set) vp-set
	  ))

(defun check-dimension (dimension vp-set function-name)
  (assert (valid-integer-range-exclusive dimension 0 (length (vp-set-dimensions vp-set))) ()
	  "In ~S, value for dimension argument, ~S, is not between 0 and ~D, the number of dimensions in vp set ~S"
	  function-name dimension (length (vp-set-dimensions vp-set)) vp-set
	  ))

(defun verify-consistent-mapping-for-vp-set (vp-set)
  (let* ((cube-address-array (vp-set-array-of-cube-addresses vp-set))
	 (grid-address-array (vp-set-array-of-grid-addresses vp-set))
	 (size (vp-set-size vp-set))
	 (number-of-dimensions (length (vp-set-dimensions vp-set)))
	 (dimension-index-list (iota number-of-dimensions))
	 )
    (dotimes (cube-index size)
      (let ((grid-indices
	      (mapcar
		#'(lambda (grid-index) (aref grid-address-array cube-index grid-index))
		dimension-index-list
		)))
	(let ((cube-from-grid-index (apply #'aref cube-address-array grid-indices)))
	  (assert (= cube-from-grid-index cube-index) () "Not working!")
	  )))))

(defmacro *with-vp-set (vp-set &body body)

  "Enable a VP SET."

  (let ((vp-set-symbol (gensym "VP-SET-"))
	(body-function-symbol (gensym "*WITH-VP-SET-BODY-FUNCTION-"))
	)

    `(flet ((,body-function-symbol () ,@body))
       (let* ((,vp-set-symbol ,vp-set))
	 (if (eq *current-vp-set* ,vp-set-symbol)
	     (progn
	       (incf (vp-set-nesting-level ,vp-set-symbol))
	       (unwind-protect
		   (,body-function-symbol)
		 (decf (vp-set-nesting-level ,vp-set-symbol))
		 ))
	     (progn
	       (setf (vp-set-context-level *current-vp-set*) *css-current-level*)
	       (unwind-protect
		   (multiple-value-bind
		     (*current-vp-set*
		      *number-of-dimensions*
		      *number-of-processors-limit*
		      *log-number-of-processors-limit*
		      *current-send-address-length*
		      *current-cm-configuration*
		      *current-grid-address-lengths*
		      *current-context-stack*
		      *css-current-level*
		      *css*
		      *ppp-default-start*
		      *ppp-default-end*
		      t!!
		      nil!!
		      )
		       (enter-vp-set-context ,vp-set-symbol)
		     (prog1
		       (,body-function-symbol)
		       (setf (vp-set-context-level *current-vp-set*) *css-current-level*)		     
		       ))
		 (decf (vp-set-nesting-level ,vp-set-symbol))
		 )))))))

(defun enter-vp-set-context (vp-set)

  (when (null (vp-set-instantiated-p vp-set))
    (error "You are trying to use a VP SET which has never been instantiated (i.e., its dimensions have not been defined)")
    )

  (incf (vp-set-nesting-level vp-set))

  ;; Return the parameters of this VP SET which will be dynamically bound

  (let* ((dimensions (vp-set-dimensions vp-set))
	 (address-lengths (vp-set-grid-address-lengths vp-set))
	 (number-of-processors (vp-set-total-size vp-set))
	 (log-number-of-processors (vp-set-address-length vp-set))
	 (context-stack (vp-set-context-stack vp-set))
	 (context-level (vp-set-context-level vp-set))
	 (css (get-context-at-level context-stack context-level))
    
	 )
    
    (values
      vp-set
      (length dimensions)
      number-of-processors
      log-number-of-processors
      log-number-of-processors 
      dimensions
      address-lengths
      context-stack
      context-level
      css
      0
      number-of-processors
      (vp-set-t!! vp-set)
      (vp-set-nil!! vp-set) 
      )

    ))

