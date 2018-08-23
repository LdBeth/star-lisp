;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *sim-i; MUSER: YES -*-

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


(declaim (type fixnum *number-of-temporary-pvars-to-add*))
(defvar *number-of-temporary-pvars-to-add* 32)

(declaim (special *front-end-structure*))

(defun create-some-more-temporary-pvars ()

  "Create some uninitialized pvars and put them
   on the pvar stack for future use.
  "

  (flet ((new-temp-pvars ()
	   (let ((pvar-list nil))
	     (dotimes (j *number-of-temporary-pvars-to-add*) (push (make-pvar :lvalue? nil) pvar-list))
	     pvar-list
	    )))

    (if (null *temp-pvar-list*)
	(setq *temp-pvar-original-list* (setq *temp-pvar-list* (new-temp-pvars)))
	(nconc *temp-pvar-list* (new-temp-pvars))
      )))

;;;
;;;  This will allocate and return a temporary pvar.
;;;  This pvar will be reclaimed by things like *LET and *SET ...
;;;


(defun allocate-temp-pvar ()
  "Return an uninitialized pvar on the *Lisp stack"
  (let ((pvar (pop *temp-pvar-list*)))
    (when (null (cdr *temp-pvar-list*))
      ;; we are about to run out of pvars.
      ;; Add a few more to the end of the list
      (create-some-more-temporary-pvars)
      ;; in case we didn't have any to begin with.
      (if (null pvar) (setq pvar (pop *temp-pvar-list*)))
      )
    (clear-non-essential-pvar-slots pvar)
    pvar
    ))


(defun get-heap-pvar-of-size (size)
  (if (null (vp-set-heap-pvar-arrays *current-vp-set*))
      (make-pvar-array-of-size size)
      (let ((return-array (pop (vp-set-heap-pvar-arrays *current-vp-set*))))
	(assert (eql (length return-array) size) () "Internal error.  pvar arrays stored in vp set not right length")
	return-array
	)))


(defun get-pvar (where)
  (let ((pvar (ecase where ((:stack) (allocate-temp-pvar)) ((:heap) (make-pvar)))))
    (assert (null (pvar-array pvar)) () "Internal error.  The pvar array slot was not reclaimed or initialized to NIL")
    (setf (pvar-array pvar)
	  (case where
	    (:stack (get-pvar-array-of-size *number-of-processors-limit*))
	    (:heap (get-heap-pvar-of-size *number-of-processors-limit*))
	    ))
    (setf (pvar-vp-set pvar) *current-vp-set*)
    pvar
    ))


(defun make-general-pvar (where)
  "Creates a general pvar and fills it with garbage"
  (let* ((pvar (get-pvar where))
	 (data (pvar-data pvar))
	 )
    (fill-array data *junk-to-initialize-with*)
    (setf (pvar-class pvar) (general-pvar-class-label))
    (setf (pvar-lvalue? pvar) nil)
    (setf (pvar-constant? pvar) nil)
    (setf (pvar-canonical-pvar-type pvar) '(pvar t))
    (make-void pvar)
    pvar
    ))


(defun make-pvar-into-array-pvar (pvar dimensions element-type where)
  where
  (let* ((front-end-array-for-pvars (make-array dimensions))
	 (displaced-array
	   (if (vectorp front-end-array-for-pvars)
	       front-end-array-for-pvars
	       (make-array (array-total-size front-end-array-for-pvars)
			   :displaced-to front-end-array-for-pvars
			   )))
	 )
    (dotimes (j (length displaced-array))
      (setf (aref displaced-array j) (make-pvar-based-on-canonical-pvar-type :heap element-type))
      (setf (pvar-constant? (aref displaced-array j)) nil)
      (setf (pvar-lvalue? (aref displaced-array j)) t)
      )

    ;; Note that the slot for void-pvar-p is the same
    ;; as the  slot for array-dimensions.  This is ok,
    ;; since an array cannot ever be a void pvar.

    (setf (pvar-array pvar) front-end-array-for-pvars)
    (setf (pvar-class pvar) (array-pvar-class-label))
    (setf (pvar-lvalue? pvar) nil)
    (setf (pvar-constant? pvar) nil)
    (setf (pvar-canonical-pvar-type pvar)
	  (make-canonical-pvar-type 'array :dimensions dimensions :element-type (cadr element-type))
	  )
    (setf (pvar-array-dimensions pvar) dimensions)
    (setf (pvar-array-canonical-element-type pvar) element-type)
    (setf (pvar-array-displaced-array pvar) displaced-array)
    pvar
    ))


(defun make-array-pvar (where canonical-pvar-type)

  "Creates an array pvar and recursively all the pvars constituting
   the elements of the array pvar.
  "

  (let ((dimensions (array-pvar-type-dimensions canonical-pvar-type))
	(element-type (array-pvar-type-canonical-element-pvar-type canonical-pvar-type))
	)

    (when (not (every #'(lambda (x) (and (integerp x) (not (minusp x)))) dimensions))
      (setq dimensions (mapcar #'eval dimensions))
      (when (not (every #'(lambda (x) (and (integerp x) (not (minusp x)))) dimensions))
	(error "You want to have an array allocated of dimensions ~S, but the dimensions are not all integers" dimensions)
	))

    (let ((array-pvar (get-pvar where)))
      (make-pvar-into-array-pvar array-pvar dimensions element-type where)
      array-pvar
      )

    ))


(defun make-pvar-into-structure-pvar (pvar type-name where)
  where
  (let* ((front-end-make-function (structure-pvar-type-front-end-make-function type-name))
	 (cm-slot-accessor-names (structure-pvar-type-slot-accessors type-name))
	 (front-end-slot-accessor-names (structure-pvar-type-front-end-slot-accessors type-name))
	 (slot-types (mapcar #'(lambda (accessor-name) (*defstruct-slot-pvar-type accessor-name)) cm-slot-accessor-names))
	 (*front-end-structure* (funcall front-end-make-function))
	 )
    (declare (special *front-end-structure* *where*))
    (mapc
      #'(lambda (accessor type)
	  (eval `(setf (,accessor *front-end-structure*) (make-pvar-based-on-canonical-pvar-type :heap ',type)))
	  (eval `(setf (pvar-constant? (,accessor *front-end-structure*)) nil))
	  (eval `(setf (pvar-lvalue? (,accessor *front-end-structure*)) t))
	  )
      front-end-slot-accessor-names
      slot-types
      )

    ;; Note that the slot for void-pvar-p is the
    ;; same as the slot for structure-name, but this
    ;; is ok since structure pvars can never be void.

    (setf (pvar-structure pvar) *front-end-structure*)
    (setf (pvar-class pvar) (structure-pvar-class-label))
    (setf (pvar-lvalue? pvar) nil)
    (setf (pvar-constant? pvar) nil)
    (setf (pvar-canonical-pvar-type pvar) (make-canonical-pvar-type 'structure :name type-name))
    (setf (pvar-structure-name pvar) type-name)
    pvar
    ))


(defun make-structure-pvar (where canonical-pvar-type)
  "Creates a structure pvar and recursively all the pvars constituting
   the slots of the structure pvar.  The pvar does not have
   its initial values at this point.
  "

  (let* ((type-name (structure-pvar-type-name canonical-pvar-type))
	 (structure-pvar (get-pvar where))
	 )
    (make-pvar-into-structure-pvar structure-pvar type-name where)
    structure-pvar
    ))


(defun make-pvar-based-on-canonical-pvar-type (where canonical-pvar-type)
  ;; Make sure that structure pvars are given their
  ;; initial values as dictated by the creation function.
  (cond
    ((array-pvar-type-p canonical-pvar-type)
     (make-array-pvar where canonical-pvar-type)
     )
    ((structure-pvar-type-p canonical-pvar-type)
     (let ((function-name (get (structure-pvar-type-name canonical-pvar-type) '*defstruct-make-function)))
       (when (null function-name)
	 (error "Internal error: No *DEFSTRUCT make function for type ~S" (structure-pvar-type-name canonical-pvar-type))
	 )
       (ecase where
	 ((:stack) (funcall function-name))
	 ((:heap)
	  (let ((result (make-structure-pvar :heap canonical-pvar-type)))
	    (setf (pvar-lvalue? result) t)
	    (*set result (funcall function-name))
	    (setf (pvar-lvalue? result) nil)
	    result
	    )))
       ))
    (t
     (let ((pvar (make-general-pvar where)))
       (setf (pvar-canonical-pvar-type pvar) canonical-pvar-type)
       pvar
       ))
    ))


(defun allocate-temp-general-pvar () (make-general-pvar :stack))

(defun allocate-temp-array-pvar (canonical-pvar-type) 
  (make-array-pvar :stack canonical-pvar-type))

(defun allocate-temp-structure-pvar (canonical-pvar-type)
  (make-structure-pvar :stack canonical-pvar-type))

(defun allocate-temp-pvar-given-canonical-pvar-type (canonical-pvar-type)
  (make-pvar-based-on-canonical-pvar-type :stack canonical-pvar-type)
  )


(defun allocate-similar-temp-pvar (pvar)
  (case (pvar-type pvar)
    (:array
      (let ((array-pvar (get-pvar :stack)))
	(make-pvar-into-array-pvar array-pvar (pvar-array-dimensions pvar) (pvar-array-element-type pvar) :stack)
	array-pvar
	))
    (otherwise
      (allocate-temp-pvar-given-canonical-pvar-type (pvar-canonical-pvar-type pvar))
      )))


(*defun allocate!! (&optional initial-value (name 'allocate!!-return) (type '(pvar *)))
  "Allocate a permanent pvar"
  (let ((canonical-pvar-type (valid-pvar-type-p type)))
    (let ((canonical-pvar-type-with-integer-sizes
	    (non-lexical-canonical-pvar-type-with-numeric-lengths-from-canonical-pvar-type canonical-pvar-type)))
      (let ((pvar (make-pvar-based-on-canonical-pvar-type :heap canonical-pvar-type)))
	(setf (pvar-name pvar) name)
	(setf (pvar-constant? pvar) nil)
	(setf (pvar-lvalue? pvar) t)
	(setf (pvar-canonical-pvar-type pvar) canonical-pvar-type-with-integer-sizes)
	(when initial-value (*copy-pvar pvar initial-value))
	(push pvar *all-allocate!!-pvars*)
	pvar
	))))

(defun handle-void-alias (void-argument)
  (when (any-processors-active-function)
    (error "ALIAS!! was called with an uninitialized non-structure pvar.")
    )
  (let ((result (allocate-temp-general-pvar)))
    (setf (pvar-vp-set result) (pvar-vp-set void-argument))
    result
    ))




(defun reset-everything ()
  (setq *temp-pvar-list* nil)
  (setq *temp-pvar-original-list* nil)
  (setq *all-*defvar-specifications* nil)
  (setq *all-def-vp-sets* nil)
  (setq *all-allocate!!-pvars* nil)
  (setf (vp-set-constants-hash-table *default-vp-set*) nil)
  (reinitialize-*lisp-geometries)
  )
