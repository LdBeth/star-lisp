;;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *SIM-i; MUSER: YES-*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


;;; Useful utilities.


(declaim (special *first-legal-geometry-id* *next-geometry-id*
                  *maximum-geometries-allowed* *all-geometries-array*
                  *illegal-geometry-id*
                  ))


(defmacro in-range!! (scalar-dimension pvar-component)
  `(and!! (not!! (minusp!! ,pvar-component))
	  (<!! ,pvar-component (!! (the fixnum ,scalar-dimension)))
	  ))

(defmacro in-range (scalar-dimension scalar-component)
  (assert (symbolp scalar-component))
  `(and (not (minusp ,scalar-component)) (< ,scalar-component ,scalar-dimension))
  )

;;; This macro provides an iteration form over all active geometries.

(defmacro do-for-active-geometries ((geometry) &body body)
  (assert (symbolp geometry))
  (let ((geometry-id-symbol (gensym "GEOMETRY-ID-")))
    `(do ((,geometry-id-symbol *first-legal-geometry-id* (1+ ,geometry-id-symbol)))
	 ((= ,geometry-id-symbol *maximum-geometries-allowed*))
       (let ((,geometry (aref *all-geometries-array* ,geometry-id-symbol)))
	 (when ,geometry
	   ,@body
	   )))))


;;; This reinitializes the geometry array and resets *next-geometry-id*.
;;; It must be called at *cold-boot time.  All previously created geometries
;;; are flushed.

(defun reinitialize-*lisp-geometries ()
  (dotimes (j *maximum-geometries-allowed*) (setf (aref *all-geometries-array* j) nil))
  (setq *next-geometry-id* *first-legal-geometry-id*)
  (setf (aref *all-geometries-array* *illegal-geometry-id*) nil)
  )


;;; Allocation and deallocation of geometries.


(defun make-geometry-at-geometry-array-slot (slot)
  (let ((new-geometry (make-geometry :id slot)))
    (setf (aref *all-geometries-array* slot) new-geometry)
    new-geometry
    ))

(defun allocate-next-geometry ()
  (when (eql (1+ *next-geometry-id*) *maximum-geometries-allowed*)
    (do ((j *first-legal-geometry-id* (1+ j)))
	((eql j *maximum-geometries-allowed*))
      (when (null (aref *all-geometries-array* j))
	(return-from allocate-next-geometry (make-geometry-at-geometry-array-slot j))
	))
    (error "Error.  You have run out of geometries.  You are probably allocating vp sets ~
            without deallocating them.  Each vp set you allocate uses up a geometry ~
            until that vp set is deallocated."
	   ))
  (incf *next-geometry-id*)
  (make-geometry-at-geometry-array-slot *next-geometry-id*)
  )

(defun deallocate-geometry (geometry)
  (assert (geometry-p geometry) ()
	  "The argument to deallocate-geometry, ~S, must be a geometry" geometry)
  (let ((id (geometry-id geometry)))
    (setf (aref *all-geometries-array* id) nil)
    (internal-deallocate-geometry geometry)
    (deallocate-paris-geometry geometry)
    nil
    ))


(defun geometry-from-id (id)
  (let ((geometry (aref *all-geometries-array* id)))
    (when (not (geometry-p geometry))
      (error "Possible internal error.  No geometry defined for geometry id ~D.~%~@
              This could mean that a processor in which the address object is being referenced now~%~@
              was not active when the address object was created.  I.e., you might be trying to~%~@
              reference unintialized data."
	     id))
    geometry
    ))


(defun *cold-boot-for-geometries () (reinitialize-*lisp-geometries))
  


(defun coordinates-fit-into-geometry-p (geometry coordinates)

  ;; Can a given set of coordinates index successfully
  ;; into a given goemetry?

  ;; If the geometry rank is not the same as the coordinates
  ;; rank then clearly it is impossible.

  ;; If any of the coordinates are negative, and the geometry
  ;; is not offset, then the coordinates cannot fit.

  ;; If the coordinates are non-negative and the geometry has
  ;; no offsets, then if the coordinates are smaller than
  ;; the geometry dimensions the coordinates will fit, otherwise
  ;; they won't.

  ;; If there are offsets, if any given offset is not zero,
  ;; it means that the indexing, instead of spanning the
  ;; range (0 - (1- dimension)) spans the range
  ;; ((- (/ dimension 2)) - (1- (/ dimension 2)))
  ;; assuming dimension is an even number for now.

  ;; Therefore, if there is an offset in a particular dimension
  ;; the coordinate must be incremented by the offset and then compared
  ;; against the dimension.

  ;; Here we go.

  ;; first make sure the ranks are the same.

  (and (eql (length coordinates) (geometry-rank geometry))

       ;; Now, is the geometry offset?

       (if (geometry-dimension-offsets geometry)

	   ;; Yes.  If the offset for a particular dimension
	   ;; is zero, we do a standard comparision (see case
	   ;; of no offsets below).  Otherwise, translate the
	   ;; legal indexing range back to its negative-positive
	   ;; span and do the comparision.  Seems to me we
	   ;; could just increment to coordinate by the right
	   ;; amount (the offset?).  Why did I do it this way?
	   
	   (every
	     #'(lambda (coordinate dimension offset)
		 (let ((real-coordinate (+ coordinate offset)))
		   (in-range dimension real-coordinate)
		   ))
	     coordinates
	     (geometry-dimensions geometry)
	     (geometry-dimension-offsets geometry)
	     )

	   ;; No, no offsets.  Every coordinate must lie
	   ;; in the range 0 inclusive to 'dimension' exclusive
	   ;; for every dimension.

	   (every
	     #'(lambda (coordinate dimension) (in-range dimension coordinate))
	     coordinates
	     (geometry-dimensions geometry)
	     )

	   )))


(defun find-geometry-that-fits-coordinates (coordinates)
  
  ;; if the current vp set's geometry works, use that,
  ;; otherwise search for a geometry that will fit,
  ;; otherwise if none will fit create one that will.
  
  (if (and *current-vp-set* (coordinates-fit-into-geometry-p (funcall 'vp-set-geometry *current-vp-set*) coordinates))
      (vp-set-geometry *current-vp-set*)
      (progn
	(do-for-active-geometries (geometry)
	  (when (coordinates-fit-into-geometry-p geometry coordinates)
	    (return-from find-geometry-that-fits-coordinates geometry)
	    ))
	(make-geometry-that-can-hold-coordinates coordinates)
	)))


