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
;;;;                           BITBLT FUNCTIONS
;;;;
;;;; ****************************************************************************



(defun parse-cube-arguments (source-pvar dest-array array-offset cube-address-start cube-address-end)
  
  (assert (or (null source-pvar) (pvarp source-pvar)) () "The pvar argument is neither unspecified nor a pvar.")
  (assert (or (null dest-array) (vectorp dest-array)) () "The array argument is neither unspecified nor a 1-d array.")
  (assert (and (integerp array-offset) (not (minusp array-offset))) () "The array-offset is not a non-negative integer.")
  (when dest-array
    (assert (or (< array-offset (array-total-size dest-array))
		(zerop (array-total-size dest-array)))
	    ()
	    "The dest-array is ~D elements long, but you provided an array-offset of ~D."
	    (array-total-size dest-array) array-offset
	    ))
  (assert (and (integerp cube-address-start) (<= 0 cube-address-start (1- *number-of-processors-limit*))) ()
	  "The cube-address-start, ~S, is not an integer between 0 and ~D, which is the range of the active vp set."
	  cube-address-start *number-of-processors-limit*
	  )
  (assert (and (integerp cube-address-end)
	       (>= cube-address-end cube-address-start)
	       (<= cube-address-end *number-of-processors-limit*)
	       )
	  ()
	  "The cube-address-end, ~S, is not an integer between the value of cube-address-start, ~D, and ~D, the ~@
             upper limit of cube addresses for the active vp set."
	  cube-address-end cube-address-start *number-of-processors-limit*
	  )

  )


(defun parse-pvar-to-array-arguments (source-pvar dest-array array-offset cube-address-start cube-address-end)
  (parse-cube-arguments source-pvar dest-array array-offset cube-address-start cube-address-end)
  (when dest-array
    (let ((number-of-usable-array-elements (- (array-total-size dest-array) array-offset))
	  (number-of-processors-to-read (- cube-address-end cube-address-start))
	  )
      (assert (>= number-of-usable-array-elements number-of-processors-to-read) ()
	      "Starting at array-offset ~D, the array provided has ~D usable elements.  But you are attempting to~@
               read ~D elements out of the CM"
	      array-offset number-of-usable-array-elements number-of-processors-to-read
	      ))))


(defun parse-array-to-pvar-arguments (source-pvar dest-array array-offset cube-address-start cube-address-end)
  (parse-cube-arguments source-pvar dest-array array-offset cube-address-start cube-address-end)
  (when dest-array
    (let ((number-of-usable-array-elements (- (array-total-size dest-array) array-offset))
	  (number-of-processors-to-write (- cube-address-end cube-address-start))
	  )
      (assert (>= number-of-usable-array-elements number-of-processors-to-write) ()
	      "Starting at array-offset ~D, the array provided has ~D elements.  But you are attempting to~@
               write ~D elements into the CM"
	      array-offset number-of-usable-array-elements number-of-processors-to-write
	      ))))


(*defun pvar-to-array

	(source-pvar &optional dest-array
		     &key
		     (array-offset 0)
		     (cube-address-start 0)
		     (cube-address-end nil cube-address-end-provided)
		     (start 0 start-provided)
		     (end nil end-provided)
		     )

  (when start-provided (setq cube-address-start start))
  (when end-provided (setq cube-address-end end))

  (simple-pvar-argument!! source-pvar)

  (assert (pvarp source-pvar) () "The source-pvar argument to pvar-to-array must be a pvar")

  (*with-vp-set (pvar-vp-set source-pvar)

    (when (and (not cube-address-end-provided)
	       (not end-provided)
	       )
      (setq cube-address-end *number-of-processors-limit*))

    (parse-pvar-to-array-arguments source-pvar dest-array array-offset cube-address-start cube-address-end)

    (cond

      ;; If we request no processors to read data from, then there
      ;; is nothing to do.

      ((= cube-address-start cube-address-end) dest-array)

      ;; It is impossible to read data out of a void pvar.

      ((void-pvar-p source-pvar)
       (error "The pvar ~S is uninitialized.  You cannot read data out of it!" source-pvar)
       )

      (t

       (when (null dest-array)
	 (let ((number-of-processors-to-read-from (- cube-address-end cube-address-start)))
	   (setq dest-array (make-array (+ number-of-processors-to-read-from array-offset)))
	   ))

       (do ((j cube-address-start (1+ j)))
	   ((= j cube-address-end))
	 (setf (aref dest-array (+ array-offset (- j cube-address-start)))
	       (pref source-pvar j)))

       dest-array

       ))))


(*defun array-to-pvar

	(source-array &optional (dest-pvar nil)
		      &key
		      (array-offset 0)
		      (cube-address-start 0)
		      (cube-address-end nil cube-address-end-provided)
		      (start 0 start-provided)
		      (end nil end-provided)
		      )
  
  (when start-provided (setq cube-address-start start))
  (when end-provided (setq cube-address-end end))

  (flet

    ((internal-array-to-pvar ()

       ;; check the arguments.

       (when (and (not cube-address-end-provided)
		  (not end-provided)
		  )
	 (setq cube-address-end *number-of-processors-limit*))

       (parse-array-to-pvar-arguments dest-pvar source-array array-offset cube-address-start cube-address-end)

       (*let (return-pvar received-value?)
         (*all (*set received-value? nil!!))
	 (do ((j cube-address-start (1+ j)))
	     ((= j cube-address-end))
	   (*setf (pref return-pvar j) (aref source-array (+ array-offset (- j cube-address-start))))
	   (*setf (pref received-value? j) t)
	   )
	 (when dest-pvar
	   (*when received-value?
	     (*set dest-pvar return-pvar)
	     ))
	 return-pvar
	 )

       ))

    ;; Select the vp set of the destination pvar.
    ;; If it's not provided use the currently active vp set.

    (if dest-pvar
	(*with-vp-set (pvar-vp-set dest-pvar) (internal-array-to-pvar) dest-pvar)
	(internal-array-to-pvar)
	)

    ))



(defun next-subhypergrid-coordinates
       (start-coordinates end-coordinates current-coordinates n-dimensions)
  (labels
    ((increment-component
	(start-coordinates end-coordinates current-coordinates n-dimensions component-index)
       (if (< (svref current-coordinates component-index)
	      (svref end-coordinates component-index))
	   (incf (svref current-coordinates component-index))
	   (if (zerop component-index)
	       nil
	       (progn
		 (setf (svref current-coordinates component-index)
		       (svref start-coordinates component-index))
		 (increment-component
		   start-coordinates end-coordinates current-coordinates
		   n-dimensions (1- component-index)
		  ))))))

    (if
      (increment-component
	start-coordinates end-coordinates current-coordinates n-dimensions (1- n-dimensions))
      current-coordinates
      nil
     )))


(defmacro with-hypergrid-coordinates-iterated

	  ;; Iterate over a sub-hypergrid defined by start-grid-address and end-grid-address
	  ;; in row-major order.  A cube address value and a grid address value as a vector
	  ;; of the current coordinate in hyperspace are bound on each iteration.

	  ((start-grid-address end-grid-address)
	   (grid-vector-address-symbol cube-address-symbol)
	   &rest body)
  (let ((start-vector-symbol (gensym))
	(end-vector-symbol (gensym))
	(last-vector-symbol (gensym))
	(n-dimensions-symbol (gensym))
	(cube-address-array-symbol (gensym))
       )
    `(let* ((,start-vector-symbol (concatenate 'vector ,start-grid-address))
	    (,end-vector-symbol (concatenate 'vector ,end-grid-address))
	    (,last-vector-symbol (map 'vector #'1- ,end-vector-symbol))
	    (,grid-vector-address-symbol (copy-seq ,start-vector-symbol))
	    (,n-dimensions-symbol (length ,start-vector-symbol))
	    (,cube-address-symbol nil)
	    (,cube-address-array-symbol (vp-set-array-of-cube-addresses *current-vp-set*))
	   )
       (loop
	   (progn
	     (setq ,cube-address-symbol
		   (internal-cube-address-from-grid-address-vector
		     ,cube-address-array-symbol
		     ,grid-vector-address-symbol
		    ))
	     ,@body
	     (when (null (next-subhypergrid-coordinates
			   ,start-vector-symbol ,last-vector-symbol
			   ,grid-vector-address-symbol ,n-dimensions-symbol
			  ))
	       (return)
	      )))
      )))




(defun check-grid-function-arguments (dest-array array-offset grid-start grid-end)

  (assert (listp array-offset) () "The array-offset argument is neither unspecified nor a list")
  (assert (listp grid-start) () "The grid-start argument is neither unspecified nor a list")
  (assert (listp grid-end) () "The grid-end argument is neither unspecified nor a list")

  (when dest-array
    (assert (and (not (pvarp dest-array)) (arrayp dest-array)) ()
	    "The array argument (~A) is neither unspecified nor an array" dest-array
	    )
    (assert (eql *number-of-dimensions* (array-rank dest-array)) ()
	    "There are ~D dimensions in the active vp set, but ~D dimensions in the array provided (~A)"
	    *number-of-dimensions* (array-rank dest-array) dest-array
	    ))

  (assert (eql (length array-offset) *number-of-dimensions*) ()
	  "There are ~D dimensions in the active vp set, ~
           but you provided an array-offset, (~S), with a different number of dimensions"
	  (array-rank dest-array) array-offset
	  )
  (assert (eql (length grid-start) *number-of-dimensions*) ()
	  "There are ~D dimensions in the active vp set, but you provided a grid-start argument ~S of different rank"
	  *number-of-dimensions* grid-start
	  )
  (assert (eql (length grid-end) *number-of-dimensions*) ()
	  "There are ~D dimensions in the active vp set, but you provided a grid-end argument ~S of different rank"
	  *number-of-dimensions* grid-end
	  )

  (dotimes (j *number-of-dimensions*)
    (let ((start-coordinate (nth j grid-start)))
      (assert (and (integerp start-coordinate) (< -1 start-coordinate (nth j *current-cm-configuration*))) ()
	      "Coordinate ~D of grid-start (~S) is not an integer in the range (0,~D)"
	      j start-coordinate (nth j *current-cm-configuration*)
	      )))
  (dotimes (j *number-of-dimensions*)
    (let ((end-coordinate (nth j grid-end)))
      (assert (and (integerp end-coordinate) (<= (nth j grid-start) end-coordinate (nth j *current-cm-configuration*))) ()
	      "Coordinate ~D of grid-end (~S) is not an integer >= the corresponding grid-start coordinate (~D), ~
                 and <= ~D, the size of dimension ~D in the active vp set"
	      j end-coordinate (nth j grid-start) (nth j *current-cm-configuration*) j
	      )))

  )


(defun parse-pvar-to-array-grid-arguments (source-pvar dest-array array-offset grid-start grid-end)

  (declare (ignore source-pvar))

  (check-grid-function-arguments dest-array array-offset grid-start grid-end)

  (let ((pvar-subgrid-extent (map 'list #'- grid-end grid-start)))
    (when dest-array
      (let* ((array-dimensions (array-dimensions dest-array))
	     (array-subgrid-extent (map 'list #'- array-dimensions array-offset))
	     )
	(dotimes (j *number-of-dimensions*)
	  (assert (<= (nth j pvar-subgrid-extent) (nth j array-subgrid-extent)) ()
		  "Along dimension ~D, you are trying to extract ~D rows from the CM, but the array you provided, (~A), ~
                   given an array-offset of ~S, only has room for ~D rows."
		  j (nth j pvar-subgrid-extent) dest-array array-offset (nth j array-subgrid-extent)
		  ))))

    (values pvar-subgrid-extent
	    (concatenate 'vector array-offset)
	    (concatenate 'vector grid-start)
	    (concatenate 'vector grid-end)
	    )))



(defun parse-array-to-pvar-grid-arguments (source-pvar dest-array array-offset grid-start grid-end)

  (assert (or (null source-pvar) (pvarp source-pvar)) () "The destination pvar argument is neither unspecified nor a pvar")

  (check-grid-function-arguments dest-array array-offset grid-start grid-end)

  (let* ((array-dimensions (array-dimensions dest-array))
	 (pvar-subgrid-extent (map 'list #'- grid-end grid-start))
	 (array-subgrid-extent (map 'list #'- array-dimensions array-offset))
	 )
    (dotimes (j *number-of-dimensions*)
      (assert (>= (nth j array-subgrid-extent) (nth j pvar-subgrid-extent)) ()
	      "Along dimension ~D, you are trying to store into ~D rows (given a grid start of ~S and a grid end of ~S),
               but an array offset of ~S only provides for ~D rows of data in the array along that dimension."
	      j (nth j pvar-subgrid-extent) grid-start grid-end array-offset (nth j array-subgrid-extent)
	      ))
    (values pvar-subgrid-extent
	    (concatenate 'vector array-offset)
	    (concatenate 'vector grid-start)
	    (concatenate 'vector grid-end)
	    )
    ))


(*defun pvar-to-array-grid

	(source-pvar &optional dest-array
		     &key
		     (array-offset nil)
		     (grid-start nil)
		     (grid-end nil)
		     #+symbolics rasterp
		     &aux array-dimensions
		     )

  (simple-pvar-argument!! source-pvar)

  #+symbolics
  (when rasterp
    (return-from pvar-to-array-grid
      (pvar-to-raster-grid source-pvar dest-array :raster-offset array-offset :grid-start grid-start :grid-end grid-end)
      ))

  (new-pvar-check-no-vp-check source-pvar 'pvar-to-array-grid)

  (assert (not (void-pvar-p source-pvar)) ()
	  "The pvar ~S has never been initialized.  You cannot extract data out of such a pvar!" source-pvar
	  )

  (*with-vp-set (pvar-vp-set source-pvar)

    (when (null array-offset) (setq array-offset (make-list *number-of-dimensions* :initial-element 0)))
    (when (null grid-start) (setq grid-start (make-list *number-of-dimensions* :initial-element 0)))
    (when (null grid-end) (setq grid-end *current-cm-configuration*))

    (multiple-value-bind (pvar-subgrid-extent)

	(parse-pvar-to-array-grid-arguments source-pvar dest-array array-offset grid-start grid-end)
	
      ;; If the extent in any dimension is 0, then 0 elements
      ;; are in reality to be read, so there is nothing to do!

      (if (some #'zerop pvar-subgrid-extent)

	  dest-array

	  (progn
	      
	    (when (null dest-array)
	      (setq array-dimensions nil)
	      (dotimes (j *number-of-dimensions*)
		(push (+ (nth j pvar-subgrid-extent) (nth j array-offset)) array-dimensions)
		)
	      (setq array-dimensions (nreverse array-dimensions))
	      (setq dest-array (make-array array-dimensions))
	      )

	    ;; We will iterate over grid addresses.  We need to be able
	    ;; to translate from a grid address into an array offset.
	    ;; Figure out what we need to subtract from a grid address
	    ;; to get the right array offset.

	    (let ((address-translation-from-grid-to-array
		    (map 'vector #'- grid-start array-offset))
		  (array-address (make-sequence 'list *number-of-dimensions*))
		  )
	    
	      ;; for each grid address within the region defined,
	      ;; convert that grid address into the appropriate
	      ;; array offset.  Get the value stored in the array
	      ;; at that offset and stuff it into the CM.
	    
	      (with-hypergrid-coordinates-iterated
		(grid-start grid-end)
		(grid-address-as-vector cube-address)
		(dotimes (j *number-of-dimensions*)
		  (setf (nth j array-address)
			(- (svref grid-address-as-vector j)
			   (svref address-translation-from-grid-to-array j))))
		(let ((value (pref source-pvar cube-address)))
		  (setf (apply #'aref dest-array array-address) value)
		  ))

	      dest-array
	      
	      ))))))


(*defun array-to-pvar-grid

	(source-array &optional (dest-pvar nil)
		      &key
		      (array-offset nil)
		      (grid-start nil)
		      (grid-end nil)
		      #+symbolics rasterp
		      )

  #+symbolics
  (when rasterp
    (return-from array-to-pvar-grid
      (raster-to-pvar-grid source-array dest-pvar :raster-offset array-offset :grid-start grid-start :grid-end grid-end)
      ))

  (flet

    ((array-to-pvar-grid-internal ()

       (when (null array-offset) (setq array-offset (make-list *number-of-dimensions* :initial-element 0)))
       (when (null grid-start) (setq grid-start (make-list *number-of-dimensions* :initial-element 0)))
       (when (null grid-end) (setq grid-end *current-cm-configuration*))

       (multiple-value-bind (pvar-subgrid-vector)

	   (parse-array-to-pvar-grid-arguments dest-pvar source-array array-offset grid-start grid-end)

	 (declare (ignore pvar-subgrid-vector))

	 ;; Select exactly those processors which are being written to.

	 (*let (return-pvar received-value?)

	   (*all (*set received-value? nil!!))

	   ;; We will iterate over grid addresses.  We need to be able
	   ;; to translate from a grid address into an array offset.
	   ;; Figure out what we need to subtract from a grid address
	   ;; to get the right array offset.

	   (let ((address-translation-from-grid-to-array
		   (map 'vector #'- grid-start array-offset))
		 (array-address (make-sequence 'list *number-of-dimensions*))
		 )

	     ;; for each grid address within the region defined,
	     ;; convert that grid address into the appropriate
	     ;; array offset.  Get the value stored in the array
	     ;; at that offset and stuff it into the CM.

	     (with-hypergrid-coordinates-iterated
	       (grid-start grid-end)
	       (grid-address-as-vector cube-address)
	       (dotimes (j *number-of-dimensions*)
		 (setf (nth j array-address)
		       (- (svref grid-address-as-vector j)
			  (svref address-translation-from-grid-to-array j))))
	       (let ((value (apply #'aref source-array array-address)))
		 (*setf (pref return-pvar cube-address) value)
		 (*setf (pref received-value? cube-address) t)
		 ))

	     (when dest-pvar
	       (*when received-value?
		 (*set dest-pvar return-pvar)
		 ))

	     return-pvar

	     )))))

    (if dest-pvar
	(progn
	  (new-pvar-check-no-vp-check dest-pvar 'array-to-pvar-grid)
	  (*with-vp-set (pvar-vp-set dest-pvar) (array-to-pvar-grid-internal) dest-pvar)
	  )
	(array-to-pvar-grid-internal)
	)

    ))



;;; Pvar <-> Raster Transfers (added by HLV)
;;; ARRAY-TO-PVAR-GRID and PVAR-TO-ARRAY-GRID with :RASTERP T should dispatch to these functions, 
;;; without reversing any arguments.

(defun-wco RASTER-TO-PVAR-GRID (raster &optional dest-pvar &key raster-offset grid-start grid-end)
  (let ((result-vp-set *current-vp-set*))
    (*let (result!!)
      (let-vp-set (transposed-vp-set (create-vp-set (reverse *current-cm-configuration*)))
	(*with-vp-set transposed-vp-set
	  (*let (sideways-result!!)
	    (array-to-pvar-grid raster sideways-result!!
				:array-offset (reverse raster-offset)
				:grid-start (reverse grid-start)
				:grid-end (reverse grid-end))
	    (*pset :default sideways-result!! result!!
		   (grid!! (self-address-grid!! (!! 1)) (self-address-grid!! (!! 0)))
		   :vp-set result-vp-set))))
      (if dest-pvar
	  (progn (*set dest-pvar result!!)
		 dest-pvar)
	  result!!))))

(defun-wco PVAR-TO-RASTER-GRID (pvar!! &optional dest-raster &key raster-offset grid-start grid-end)
  (let ((pvar-vp-set *current-vp-set*))
    (let-vp-set (transposed-vp-set (create-vp-set (reverse *current-cm-configuration*)))
	(*with-vp-set transposed-vp-set
	  (*let (sideways-pvar!!)
	    (*with-vp-set pvar-vp-set
	      (*pset :default pvar!! sideways-pvar!!
		     (grid!! (self-address-grid!! (!! 1)) (self-address-grid!! (!! 0)))
		     :vp-set transposed-vp-set)
	      (pvar-to-array-grid sideways-pvar!! dest-raster
				  :array-offset (reverse raster-offset)
				  :grid-start (reverse grid-start)
				  :grid-end (reverse grid-end))))))))

