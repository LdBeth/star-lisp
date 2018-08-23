;;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: (*sim-i :use common-lisp-global); MUSER: YES -*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


#|

  External interface to the pvar array pool mechanism:

    Initialize-table-of-pvar-array-pools

    must be called at *cold-boot time.

    Find-pool-of-size-or-make-one

    is used to create a new pool of a new size.

    Find-pool-of-size 

    can be used to determine if a pool of a certain size exists.
    If nil is returned, then a pool of that size does not exist.

    Get-pvar-array-of-size 

    is used to allocate a pvar array from a pool for use inside
    a pvar structure.

    Reclaim-pvar-array-of-size 

    is used to return the latest pvar array of size size
    to the pool.  Stack discipline is assumed.

    Return-pvar-array-to-pool 
 
    is used to return a specific pvar array to the pool.
    The array must be the most recently allocated array.  Thus this is
    equivalent to reclaim-pvar-array-of-size, but it checks that the array
    being returned is at the top of the stack.

    Test-pools

    is a simple test routine for the basic allocation and reclamation
    mechanisms.

|#

(defstruct array-pool
  (next 0 :type fixnum)
  (arrays nil :type (or null simple-vector))
  (total-length 0 :type fixnum)
  (array-size 0 :type fixnum)
  )

(declaim (type fixnum *initial-pool-size* *pool-size-increment*))
(defvar *initial-pool-size* 32 "Number of arrays to initially give to a pool")
(defvar *pool-size-increment* 32 "Number of arrays to add when a pool runs out of arrays")


(defvar *table-of-pvar-array-pools* nil
  "A data structure (now a hash table) containing all existing pools of pvar arrays"
  )

(defun initialize-table-of-pvar-array-pools ()

  ;; If the hash table doesn't exist, create it.
  ;; In any case clear it all out.  This is called
  ;; at *cold-boot time.

  (when (not (hash-table-p *table-of-pvar-array-pools*))
    (setq *table-of-pvar-array-pools* (make-hash-table :size 64))
    )
  (clrhash *table-of-pvar-array-pools*)
  )


(defun find-pool-of-size (size)

  ;; Returns the array pool of the designated size if
  ;; it exists in the hash table, or NIL.

  (assert (and (integerp size) (plusp size)) () "Pool sizes are supposed to be positive integers")
  (gethash size *table-of-pvar-array-pools*)
  )


(defun add-pool-to-table (pool)

  ;; Add an array pool to the hash table.
  ;; Make sure one of the same size doesn't exist already.

  (assert (array-pool-p pool) () "Internal error.  Attempt to add non array-pool object to array pool hash table")
  (let ((size (array-pool-array-size pool)))
    (let ((already-existing-pool (find-pool-of-size size)))
      (when (and already-existing-pool (not (eq pool already-existing-pool)))
	(error "Internal error.  A different pool of size ~D already exists." size)
	)
      (when already-existing-pool
	(warn "Internal question.  Why are you trying to add a pool that already exists?")
	)
    (setf (gethash size *table-of-pvar-array-pools*) pool)
    )))


(defun make-pvar-array-of-size (size)
  (make-sequence 'simple-vector size)
  )

(defun initialize-array-pool (size)

  ;; Create an array pool of the designated size
  ;; and add it to the hash table.

  (let ((pool (make-array-pool
		:next 0
		:arrays (make-sequence 'simple-vector *initial-pool-size* :initial-element nil)
		:total-length *initial-pool-size*
		:array-size size
		)))
    (declare (type array-pool pool))
    (let ((pool-of-arrays (array-pool-arrays pool)))
      (declare (type simple-vector pool-of-arrays))
      (dotimes (j (length pool-of-arrays))
	(declare (type fixnum j))
	(setf (aref pool-of-arrays j) (make-pvar-array-of-size size))
	))
    (add-pool-to-table pool)
    pool
    ))


;;; This function is the one that should be called
;;; to find and/or create a pool of pvar arrays
;;; of a certain size.


(defun find-pool-of-size-or-make-one (size)

  ;; If a pool of the designated size already exists
  ;; return the pool, otherwise create an array pool
  ;; and include it in our table of array pools.

  (let ((pool (find-pool-of-size size)))
    (assert (or (null pool) (array-pool-p pool)) () "Internal error.  find-pool-of-size returns bad result")
    (if pool pool (initialize-array-pool size))
    ))


(defun get-pvar-array-from-pool (pool)

  (declare (type array-pool pool))

  (let* ((current-length (array-pool-total-length pool))
	 (next (array-pool-next pool))
	 )

    (declare (type fixnum current-length next))

    ;; If we've exhausted our supply of arrays, we must
    ;; make more.  Replace our pool of arrays with a
    ;; larger pool and allocate new arrays for the
    ;; new slots of the larger pool.  

    (cond

      ((< next current-length) nil)

      ((= next current-length)
       (let* ((old-array (array-pool-arrays pool))
	      (new-length (+ current-length *pool-size-increment*))
	      (new-array (make-sequence 'simple-vector new-length :initial-element nil))
	      (size (array-pool-array-size pool))
	      )
	 (declare (type fixnum new-length size))
	 (declare (type simple-vector new-array old-array))
	 (dotimes (j (length new-array))
	   (declare (type fixnum j))
	   (if (< j current-length)
	       (assert (null (aref old-array j)) ()
		       "Internal error.  All the old arrays are in use but some are still present in pool"
		       )
	       (setf (aref new-array j) (make-pvar-array-of-size size))
	       ))
	 (setf (array-pool-arrays pool) new-array)
	 (setf (array-pool-total-length pool) new-length)
	 ))

      ((> next current-length) (error "Internal error.  Next array index exceeds size of array pool."))

      )

    ;; get the next free array and increment the free pointer.
    ;; Return the free array and set the slot previously
    ;; containing the array to NIL.

    (prog1
      (aref (array-pool-arrays pool) next)
      (setf (aref (array-pool-arrays pool) next) nil)
      (incf (array-pool-next pool))
      )

    ))


;;; This is the routine called by the pvar allocation routines.

(defun get-pvar-array-of-size (size)
  (let ((pool (find-pool-of-size size)))
    (assert (array-pool-p pool) ()
	    "Internal error.  There is no array pool of size ~D, but a pvar array of that size was requested" size
	    )
    (get-pvar-array-from-pool pool)
    ))


;;; This routine is called inside handle-returning-pvar and
;;; any other routine which pops the stack to reclaim
;;; pvar arrays.

(defun return-pvar-array-to-pool (array)
  (let* ((size (length array))
	 (pool (find-pool-of-size size))
	 )
    (assert (array-pool-p pool) ()
	    "Internal error.  No array pool of size ~D exists, but an array of that size is being reclaimed" size
	    )
    (let ((next (array-pool-next pool)))
      (declare (type fixnum next))
      (assert (plusp next) ()
	      "Internal error.  There are no allocated arrays of size ~D but one is being reclaimed" size
	      )
      (decf (array-pool-next pool))
      (setf (aref (array-pool-arrays pool) (1- next)) array)
      )))


;(defun test-pool (size)
;  (let ((pool (find-pool-of-size-or-make-one size)))
;    (dotimes (j (1+ *initial-pool-size*))
;      (let ((array (get-pvar-array-from-pool pool)))
;      (assert (arrayp array) () "Not returning an array")
;      (assert (= size (length array)) () "Array returns is not the right size")
;      (assert (= j (1- (array-pool-next pool))) () "Next not right")
;      (if (< j *initial-pool-size*)
;	  (assert (= (length (array-pool-arrays pool)) *initial-pool-size*) () "Length not right")
;	  (assert (= (length (array-pool-arrays pool)) (+ *initial-pool-size* *pool-size-increment*))
;		  ()
;		  "Length not right after increment"
;		  ))))
;    (dotimes (j 5)
;      (reclaim-pvar-array-of-size size)
;      )
;    (assert (= (- (1+ *initial-pool-size*) 5) (array-pool-next pool)) () "reclaiming pvar arrays not working")
;    pool
;    ))


(defun describe-array-pool (pool)
  (format t "~%Number of arrays in pool: ~D, Size of each array: ~D, Next available array: ~D~%"
	  (array-pool-total-length pool)
	  (array-pool-array-size pool)
	  (array-pool-next pool)
	  )
  (format t "Array status: ")
  (dotimes (j (array-pool-total-length pool))
    (let ((entry (aref (array-pool-arrays pool) j)))
      (cond
	((null entry) (format t "NIL "))
	((arrayp entry) (format t "A~D " (length entry)))
	(t (format t "??? ~A " entry))
	)))
  (terpri)
  )

(defun describe-all-array-pools ()
  (maphash
    #'(lambda (key value)
	(declare (ignore key))
	(terpri)
	(describe-array-pool value)
	)
    *table-of-pvar-array-pools*
    ))

;(defun test-pools ()
;  (initialize-table-of-pvar-array-pools)
;  (test-pool 32)
;  (test-pool 64)
;  (test-pool 10)
;  (maphash
;    #'(lambda (key pool)
;	(assert (member key '(32 64 10)))
;	(format t "~%Key size: ~D, Actual size: ~D, Pool next: ~D, Pool total size: ~D, Actual length: ~D"
;		key
;		(array-pool-array-size pool)
;		(array-pool-next pool)
;		(array-pool-total-length pool)
;		(length (array-pool-arrays pool))
;		))
;    *table-of-pvar-array-pools*
;    ))

