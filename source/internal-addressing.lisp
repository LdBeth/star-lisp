;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: (*SIM-I COMMON-LISP-GLOBAL); Muser: yes -*-

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


;;; Auxiliary functions for argument checking for the addressing functions


(defun vp-set-check (vp-set function-name)
  (assert (vp-set-p vp-set) ()
	  "The vp set argument to the function ~S was not a vp set, but has the value ~A"
	  function-name vp-set
	  ))


(defun validate-all-pvars (pvar-index-list function-name)
  (new-multiple-pvar-check pvar-index-list function-name)
  )


(defun check-grid-indices-values (index-list vp-set function-name)

  ;; make sure each element of index-list is within the
  ;; dimension boundaries of its dimension.

  (let ((j 0))
    (mapc
      #'(lambda (index upper-exclusive-bound)
	  (assert (valid-integer-range-exclusive index 0 upper-exclusive-bound) ()
		  "In ~S, the value for grid dimension ~D, ~S, is out of range with respect to vp set ~S"
		  function-name j index vp-set
		  )
	  (incf j)
	  )
      index-list
      (vp-set-dimensions vp-set)
     ))
  t)


(defun grid-indices-values-valid-p (index-list vp-set)

  ;; make sure each element of index-list is within the
  ;; dimension boundaries of its dimension.

  (let ((j 0))
    (mapc
      #'(lambda (index upper-exclusive-bound)
	  (when (null (valid-integer-range-exclusive index 0 upper-exclusive-bound))
	    (return-from grid-indices-values-valid-p nil)
	    )
	  (incf j)
	  )
      index-list
      (vp-set-dimensions vp-set)
     ))
  t)


(defun check-grid-indices (index-list vp-set function-name)

  ;; make sure index-list is the same length as the
  ;; number of grid dimensions and that each element
  ;; of the list is within the dimension boundaries
  ;; of its dimension.

  (assert (eql (length index-list) (length (vp-set-dimensions vp-set))) ()
	  "In ~S, wrong number of grid addresses provided (should be ~D)"
	  function-name (length (vp-set-dimensions vp-set)))
  (check-grid-indices-values index-list vp-set function-name)
  t
 )


(defun check-cube-address-pvar (pvar vp-set function-name)
  (let ((pvar-array (pvar-array pvar))
        (nprocessors (vp-set-size vp-set))
        )
    (with-simple-vectors (pvar-array)
      (do-for-selected-processors (j)
        (when (not (valid-integer-range-exclusive (aref pvar-array j) 0 nprocessors))
          (error "~S: Cube address value ~S,~% at processor address ~D,~% is not valid with respect to vp set ~S."
            function-name (aref pvar-array j) j vp-set
            ))))))

(defun check-dimension-pvar (pvar vp-set function-name)
  (let ((pvar-array (pvar-array pvar))
        (number-of-dimensions (length (vp-set-dimensions vp-set)))
        )
    (with-simple-vectors (pvar-array)
      (do-for-selected-processors-internal (j)
        (when (not (valid-integer-range-exclusive (aref pvar-array j) 0 number-of-dimensions))
          (error
              "~S: Dimension value ~S,~% at processor address ~D,~%~
               is not valid with respect to vp set ~S, which has ~D dimensions"
            function-name (aref pvar-array j) j vp-set (length (vp-set-dimensions vp-set))
            ))))))


(defun internal-cube-from-vp-grid-address!!
    (function-name error-if-bad-address relativep vp-set index-pvars)
  
  "Translates a grid address into a cube address in each selected processor.
   If error-if-bad-address is nil, NIL is returned in the returned pvar
   for the processors which have an invalid grid address specified,
   otherwise an error is generated.  The grid addresses may be relative,
   in which case they are converted to absolute.
  "
  (simple-pvar-argument!! &rest index-pvars)
  
  (safety-check
   (progn
     (vp-set-check vp-set function-name)
     (new-multiple-pvar-check index-pvars function-name)
     ))
  
  (let* ((return-pvar (allocate-temp-general-pvar))
         (return-array (pvar-array return-pvar))
         (number-of-dimensions (length index-pvars))
         (temp-array-of-indices (make-sequence 'simple-vector number-of-dimensions))
         (temp-list-of-indices-for-processor (make-list number-of-dimensions))
         )
    
    (with-simple-vectors (return-array)
      
      ;; put the pvar arrays into a vector for fast access
      
      (dotimes (j number-of-dimensions)
        (setf (aref temp-array-of-indices j) (pvar-array (nth j index-pvars)))
        )
      
      (let ((any-set nil))
        
        (do-for-selected-processors-internal (processor)
          
          (setq any-set t)
          
          ;; for each active processor, put its grid coordinate pvar
          ;; values into a vector.  If the grid coordinates are relative
          ;; add in the absolute grid coordinates for this processor.
          
          (let ((*interpreter-safety* 0))
            (dotimes (dimension number-of-dimensions)
              (setf (nth dimension temp-list-of-indices-for-processor)
                (if relativep
                    (+ (aref (aref temp-array-of-indices dimension) processor)
                       (internal-grid-from-vp-cube-address vp-set processor dimension function-name)
                       )
                  (aref (aref temp-array-of-indices dimension) processor)
                  ))))
          
          (if error-if-bad-address
              
              ;; check that the grid coordinates are valid, then convert them
              ;; to a cube address.
              
              (setf (aref return-array processor)
                (internal-cube-from-vp-grid-address vp-set temp-list-of-indices-for-processor function-name)
                )
            
            ;; if it's a valid address convert the address and store it,
            ;; otherwise store NIL.
            
            (setf (aref return-array processor)
              (if (not (grid-indices-values-valid-p temp-list-of-indices-for-processor vp-set))
                  nil
                (internal-cube-address-from-grid-address-list
                 (vp-set-array-of-cube-addresses vp-set)
                 temp-list-of-indices-for-processor
                 )))
            
            ))
        
        (when any-set (make-non-void return-pvar))
        
        ))
    
    return-pvar
    
    ))


(defun internal-grid-from-vp-cube-address (vp-set cube-address dimension function-name)
  (safety-check
    (progn
      (vp-set-check vp-set function-name)
      (check-cube-address cube-address vp-set function-name)
      (check-dimension dimension vp-set function-name)
      ))
  (aref (vp-set-array-of-grid-addresses vp-set) cube-address dimension)
  )


(defun internal-cube-from-vp-grid-address (vp-set indices function-name)
  (safety-check
    (progn
      (vp-set-check vp-set function-name)
      (check-grid-indices indices vp-set function-name)
      ))
  (internal-cube-address-from-grid-address-list (vp-set-array-of-cube-addresses vp-set) indices)
  )


