;;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *LISP; -*-


(in-package '*lisp)
						

;;; In this exercise we have an array of points on the front end.
;;; We wish to assign some fixed number of these points at random to
;;; each processor in the CM.

;;; We do this by first using the *Lisp array transfer functions to
;;; move the array of points into the CM in an efficient manner.
;;; The N points are stored in the first N CM processors.
;;; We then use PREF!! to fetch points from random processors,
;;; each CM processor calculating a random number between 0 and N
;;; to determine which point it is to retrieve.  This random fetch
;;; is repeated *number-of-points-per-processor* times, so that
;;; each processor ends up with *number-of-points-per-processor*
;;; points (some which may be duplicates).


(*defstruct point
  (x nil :type t :cm-type single-float-pvar)
  (y nil :type t :cm-type single-float-pvar)
  (z nil :type t :cm-type single-float-pvar)
  )

(*proclaim '(type fixnum *number-of-points* *number-of-point-per-processor*))
(defparameter *number-of-points* 500)
(defparameter *number-of-points-per-processor* 25)

(defvar points)

(def-vp-set bitblt-vp-set (list (max 512 *minimum-size-for-vp-set*)))

(*proclaim '(type (pvar (array point (*number-of-points-per-processor*))) *array-of-points-pvar*))
(*defvar *array-of-points-pvar* nil nil bitblt-vp-set )

(defun initialize-points ()
  (setq points
	(make-point
	  :x (make-array *number-of-points* :element-type 'single-float)
	  :y (make-array *number-of-points* :element-type 'single-float)
	  :z (make-array *number-of-points* :element-type 'single-float)
	  ))
  (dotimes (j *number-of-points*)
    (setf (aref (point-x points) j) (random 1.0))
    (setf (aref (point-y points) j) (random 1.0))
    (setf (aref (point-z points) j) (random 1.0))
    ))


(defun assign-points-to-processors-randomly (&key pretty-print)

  ;; Move the front end data into the CM into the first *number-of-points* processors.

  ;; This is the most efficient way of doing it.  It might take orders of
  ;; magnitude more time if the front end data were stored as an array of
  ;; defstructs, instead of as here being stored in a defstruct of 3 arrays.

  (*with-vp-set bitblt-vp-set

    (*let (temp-point)
      (declare (type (pvar point) temp-point))
      (array-to-pvar (point-x points) (alias!! (point-x!! temp-point)) :cube-address-end *number-of-points*)
      (array-to-pvar (point-y points) (alias!! (point-y!! temp-point)) :cube-address-end *number-of-points*)
      (array-to-pvar (point-z points) (alias!! (point-z!! temp-point)) :cube-address-end *number-of-points*)
    
      ;; Each processor selects *number-of-points-per-processor*
      ;; points at random and stores them into its local array of points.

      (*let (random-point)
	(declare (type (pvar fixnum) random-point))
	(dotimes (j *number-of-points-per-processor*)
	  (*set random-point (random!! (!! *number-of-points*)))
	  (*setf (aref!! *array-of-points-pvar* (!! j))
		 (pref!! temp-point random-point)
		 )))

      (when pretty-print
	(let ((*print-pretty* t))
	  (dotimes (j 3)
	    (let ((point (pref (aref!! *array-of-points-pvar* (!! j)) 0)))
	      (format t "~%Random point ~D in processor 0: (~5F ~5F ~5F)" 
		      j (point-x point) (point-y point) (point-z point)
		      )))))

      nil

      )))

