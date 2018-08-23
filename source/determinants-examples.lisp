;;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *LISP; -*-

(in-package '*lisp)
						

;;;; The goal of this exercise is to have a three dimensional
;;;; cubic vp set of arbitrary size, in which, in each processor
;;;; we place a square complex matrix of arbitrary size.
;;;; We then calculate the determinant of each matrix
;;;; in those processors which lie along the main diagonal
;;;; of the cube, and copy this result to all the other
;;;; processors in the X-Y plane determined by the Z
;;;; coordinate of the processor which has computed
;;;; the determinate.


;;;; Define a vp set of unknown size which has a pvar
;;;; which will eventually contain a complex matrix
;;;; of unknown size in each virtual processor.

(def-vp-set complex-cube-vp-set nil
  :*defvars
  ((matrix-pvar))
  )

;;;; Here we obtain the matrix we wish to deal with,
;;;; select its main diagonal, calculate the determinate
;;;; in each of the selected processors, and spread the
;;;; result out to every processor in the same XY plane.


(*proclaim '(*defun determinant-of-complex-matrix!!))

(defun main (side-of-cube side-of-matrix)
  (*cold-boot)
  (get-complex-matrix side-of-cube side-of-matrix)
  (*with-vp-set complex-cube-vp-set
    (*let ((determinant (!! 0.0)))
      (declare (type single-complex-pvar determinant))
      (*when (=!! (self-address-grid!! (!! 0))
		  (self-address-grid!! (!! 1))
		  (self-address-grid!! (!! 2))
		  )
	(*set determinant (determinant-of-complex-matrix!! matrix-pvar))
	(*pset :no-collisions determinant determinant
	       (cube-from-grid-address!!
		 (!! 0) (!! 0) (self-address-grid!! (!! 2))
		 )))
      (*set determinant (spread!! determinant 0 0))
      (*set determinant (spread!! determinant 1 0))
      (ppp determinant :mode :grid :end '(4 4 2) :ordering '(2 0 1))
      ))
  (deallocate-vp-set-processors complex-cube-vp-set)
  )

;;;; We first instantiate the complex-cube-vp-set to have
;;;; some defined size.  Then we create a complex matrix
;;;; of some defined square size
;;;; and make matrix-pvar contain that array.
;;;; Finally, we initialize the elements of the matrix
;;;; with random values.


(defun get-complex-matrix (side-of-cube side-of-matrix)
  (allocate-vp-set-processors
   complex-cube-vp-set
   (list side-of-cube side-of-cube side-of-cube)
   )
  (*with-vp-set 
   complex-cube-vp-set
   (progn
     ;;(with-compile-time-local-property (compile-time-prop *compilep* nil)
     (*set matrix-pvar
           (make-array!!
            (list side-of-matrix side-of-matrix)
            :element-type 'single-complex-pvar
            )))
   (*map
    #'(lambda (x)
        (*set (the single-complex-pvar x)
              (complex!! (!! (the fixnum (random 5))))
              ))
    matrix-pvar
    )))
  
  
  ;;;; This proclamation tells the *Lisp compiler what
  ;;;; type of pvar is returned from the named function.
  
  (*proclaim '(ftype (function (t) single-complex-pvar)
                     determinant-of-complex-matrix!!
                     recursive-determinant
                     ))


(*defun determinant-of-complex-matrix!! (complex-square-matrix)
  (declare (type (array-pvar (complex single-float) 2) complex-square-matrix))

  ;; Figure out how large on a side our matrix is.

  (let ((matrix-row-size (*array-dimension complex-square-matrix 0)))

    (*let ((determinant (!! 0.0)))
      (declare (type single-complex-pvar determinant))

      (cond

	;; A 1-element matrix is its own determinant.

	((eql 1 matrix-row-size)
	 (*set determinant (aref!! complex-square-matrix (!! 0) (!! 0)))


	 )

	;; A two-by-two matrix

	;; A B
	;; C D

	;; has determinant AD - BC

	((eql 2 matrix-row-size)
	 (*set determinant
	       (-!!
		 (*!! (aref!! complex-square-matrix (!! 0) (!! 0))
		      (aref!! complex-square-matrix (!! 1) (!! 1)))
		 (*!! (aref!! complex-square-matrix (!! 1) (!! 0))
		      (aref!! complex-square-matrix (!! 0) (!! 1)))
		 )))

	(t
	 (*set determinant
	       (recursive-determinant complex-square-matrix matrix-row-size)
	       ))

	)

      determinant

      )))



(defun recursive-determinant (complex-square-matrix matrix-row-size)

  ;; For more than two dimensions, we use a recursion
  ;; algorithm.  We allocate a complex matrix less by
  ;; 1 on a side than the original matrix.  Then for
  ;; each element of the top row of the original
  ;; matrix we copy the appropriate elements into
  ;; the reduced matrix and recurse.

  (*locally
    (declare (type (array-pvar (complex single-float) 2) complex-square-matrix))

    (let ((reduced-size (1- matrix-row-size))
	  (sign 1)
	  )

      (*let (reduced-array determinant)
	(declare
	  (type (array-pvar (complex single-float) (reduced-size reduced-size))
		reduced-array
		))
	(declare (type single-complex-pvar determinant))
	(*set determinant (!! 0.0))

	(dotimes (j matrix-row-size)
	  nil
	  (let ((reduced-column-index 0))
	    nil
	    (do ((column-index 1 (1+ column-index)))
		((= column-index matrix-row-size))
	      nil
	      (let ((reduced-row-index 0))
		nil
		(dotimes (row-index matrix-row-size)
		  nil
		  (when (not (eql row-index j))
		    (*locally
		      (declare (type fixnum reduced-row-index row-index))
		      (declare (type fixnum reduced-column-index column-index))
		      (*setf (aref!! reduced-array
				     (!! reduced-row-index)
				     (!! reduced-column-index)
				     )
			     (aref!! complex-square-matrix
				     (!! row-index)
				     (!! column-index)
				     )))
		    (incf reduced-row-index)
		    )))
	      (incf reduced-column-index)
	      ))

	  (*let ((sub-determinate (determinant-of-complex-matrix!! reduced-array))
		 (multiplicand
		   (aref!! complex-square-matrix (!! (the fixnum j)) (!! 0))))
	    (declare (type single-complex-pvar sub-determinate multiplicand))
	    (print (list 'DETERMINANT-OF-SUBMATRIX (pref sub-determinate 0)))
	    (print (list 'MUTLIPLICAND (pref multiplicand 0)))
	    (print (list 'SIGN sign))
	    (*set determinant 
		  (+!! determinant
		       (*!! sub-determinate multiplicand (!! (the fixnum sign)))
		       )))

	  (print (list 'SUBTOTAL (pref determinant 0)))
	  (setq sign (* sign -1))

	  )

	determinant

	))))


#|

  (main 8 3)
(DETERMINANT-OF-SUBMATRIX #C(8.0 0.0)) 
(MUTLIPLICAND #C(1.0 0.0)) 
(SIGN 1) 
(SUBTOTAL #C(8.0 0.0)) 
(DETERMINANT-OF-SUBMATRIX #C(4.0 0.0)) 
(MUTLIPLICAND #C(4.0 0.0)) 
(SIGN -1) 
(SUBTOTAL #C(-8.0 0.0)) 
(DETERMINANT-OF-SUBMATRIX #C(-2.0 0.0)) 
(MUTLIPLICAND #C(3.0 0.0)) 
(SIGN 1) 
(SUBTOTAL #C(-14.0 0.0)) 

(2 0 1) 

DIMENSION 2, COORDINATE 0

     DIMENSION 0    ----->

#C(-14.0 0.0) #C(-14.0 0.0) #C(-14.0 0.0) #C(-14.0 0.0) 
#C(-14.0 0.0) #C(-14.0 0.0) #C(-14.0 0.0) #C(-14.0 0.0) 
#C(-14.0 0.0) #C(-14.0 0.0) #C(-14.0 0.0) #C(-14.0 0.0) 
#C(-14.0 0.0) #C(-14.0 0.0) #C(-14.0 0.0) #C(-14.0 0.0) 


DIMENSION 2, COORDINATE 1

     DIMENSION 0    ----->

#C(-14.0 0.0) #C(-14.0 0.0) #C(-14.0 0.0) #C(-14.0 0.0) 
#C(-14.0 0.0) #C(-14.0 0.0) #C(-14.0 0.0) #C(-14.0 0.0) 
#C(-14.0 0.0) #C(-14.0 0.0) #C(-14.0 0.0) #C(-14.0 0.0) 
#C(-14.0 0.0) #C(-14.0 0.0) #C(-14.0 0.0) #C(-14.0 0.0) 
NIL


|#
