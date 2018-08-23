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


;;; Geometries are stored in an array of geometries, *maximum-geometries-allowed* long.
;;; At *cold-boot, all geometries are flushed and the array is initialized to NIL everywhere.
;;; Geometries can be allocated and deallocated.  Geometries are allocated sequentially
;;; until *maximum-geometries-allowed* geometries are allocated.  If none have been
;;; deallocated and *maximum-geometries-allowed* geometries have been allocated, then
;;; attempting to allocate another one will result in an error.  Otherwise the array
;;; is searched for a deallocated geometry slot.

;;; The index into the array where a particular geometry is stored in known as
;;; a geometry-id.  The geometry-id is used within *Lisp to uniquely identify
;;; geometries and to refer to them.


(defstruct geometry
  id
  rank
  dimensions
  dimension-offsets
  dimension-subsizes
  address-length
  grid-address-lengths
  spare1
  )


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant *illegal-geometry-id* 0)
  (defconstant *first-legal-geometry-id* 1)
  (defvar *maximum-geometries-allowed* 64)
  (defvar *next-geometry-id* *first-legal-geometry-id*)
  (defvar *all-geometries-array* nil
    "An array which, at index j, contains the geometry defined
     by geometry-id j.
    "
    )
  (setq *all-geometries-array* (make-array *maximum-geometries-allowed* :initial-element nil))
  )


;;; This reinitializes the geometry array and resets *next-geometry-id*.
;;; It must be called at *cold-boot time.  All previously created geometries
;;; are flushed.



;;; Allocation and deallocation of geometries.



(defun geometry-dimension (geometry n) (elt (geometry-dimensions geometry) n))
(defun geometry-dimension-offset (geometry n) (elt (geometry-dimension-offsets geometry) n))



(defun indices-from-row-major-index (row-major-index geometry)
  (let ((dimension-subsizes (geometry-dimension-subsizes geometry)))
    (let ((indices nil))
      (dolist (subsize dimension-subsizes)
	(multiple-value-bind (dividend remainder)
	    (truncate row-major-index subsize)
	  (push dividend indices)
	  (setq row-major-index remainder)
	  ))
      (setq indices (nreverse indices))
      indices
      )))

(defun grid-index-from-row-major-index (row-major-index geometry dimension)
  (let ((dimension-subsizes (geometry-dimension-subsizes geometry)))
    (do ((dimension-index 0 (1+ dimension-index))
	 (subsizes dimension-subsizes (cdr subsizes))
	 )
	((null subsizes))
      (multiple-value-bind (dividend remainder)
	  (truncate row-major-index (car subsizes))
	(when (= dimension dimension-index)
	  (return-from grid-index-from-row-major-index dividend)
	  )
	(setq row-major-index remainder)
	))
    (error "You can't get here")
    ))


(defun dimension-subsizes-from-dimensions (dimensions)
  (let ((dimension-subsizes nil))
    (dotimes (j (length dimensions))
      (push (reduce #'* (nthcdr (1+ j) dimensions)) dimension-subsizes)
      )
    (setq dimension-subsizes (nreverse dimension-subsizes))
    dimension-subsizes
    ))


(defun define-dimensions-and-offsets-from-coordinates (coordinates)
  (if (every #'non-negative-integer-p coordinates)
      (values (mapcar '1+ coordinates) nil)
      (let ((dimensions nil) (offsets nil))
	(dolist (coordinate coordinates)
	  (if (non-negative-integer-p coordinate)
	      (progn
		(push (1+ coordinate) dimensions)
		(push 0 offsets)
		)
	      (progn
		(push (1+ (abs coordinate)) dimensions)
		(push (abs coordinate) offsets)
		)))
	(values (nreverse dimensions) (nreverse offsets))
	)))


(defun create-geometry

       (&key dimensions weights on-chip-bits on-chip-pos off-chip-bits off-chip-pos ordering)

  (declare (ignore weights on-chip-bits on-chip-pos off-chip-bits off-chip-pos ordering))

  (assert (every 'non-negative-integer-p dimensions) () "The dimensions are not all non-negative integers")

  (let ((new-geometry (allocate-next-geometry)))
    (setf (geometry-rank new-geometry) (length dimensions))
    (setf (geometry-dimensions new-geometry) dimensions)
    (setf (geometry-dimension-subsizes new-geometry) (dimension-subsizes-from-dimensions dimensions))
    (setf (geometry-address-length new-geometry) (n-bits-for-address (apply '* dimensions)))
    (setf (geometry-grid-address-lengths new-geometry) (mapcar 'n-bits-for-address dimensions))
    new-geometry
    ))


(defun make-geometry-that-can-hold-coordinates (coordinates)

  ;; Creates a new geometry that can be indexed into
  ;; successfully by the coordinates.

  (multiple-value-bind (dimensions offsets)
      (define-dimensions-and-offsets-from-coordinates coordinates)
    (let ((new-geometry (create-geometry :dimensions dimensions)))
      (setf (geometry-dimension-offsets new-geometry) offsets)
      new-geometry
      )))

(defun internal-deallocate-geometry (geometry)
  (setf (geometry-id geometry) *illegal-geometry-id*)
  (setf (geometry-dimensions geometry) "This geometry has been deallocated.  You should not be accessing it.")
  )

(defun deallocate-paris-geometry (geometry)
  (declare (ignore geometry))
  nil
  )

;;; Useful utilities.


#+NIL ;; Why on earth is this here?
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




