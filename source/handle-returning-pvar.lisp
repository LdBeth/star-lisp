;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: (*SIM-I COMMON-LISP-GLOBAL); Base: 10; Muser: yes -*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


(defun *map-array (function-of-one-pvar-argument pvar)
  (assert (array-pvar-p pvar) () "*map-structure was called, but pvar argument was not an array pvar")
  (let ((lisp-array-holding-pvars (pvar-array pvar)))
    (with-array-elements-iterated
        lisp-array-holding-pvars
      element-pvar
      (funcall function-of-one-pvar-argument element-pvar)
      )))

(defun *map-structure (function-of-one-pvar-argument pvar)
  (assert (structure-pvar-p pvar) () "*map-structure was called, but pvar argument was not a structure pvar")
  (let* ((front-end-structure (pvar-structure pvar))
	 (type-name (pvar-structure-name pvar))
	 (front-end-slot-accessor-names (structure-pvar-type-front-end-slot-accessors type-name))
	 )
    (mapc
      #'(lambda (accessor) (funcall function-of-one-pvar-argument (funcall accessor front-end-structure)))
      front-end-slot-accessor-names
      )
    ))

(defun clean-up-stack (start end)
  (do ((pvar-list start (cdr pvar-list)))
      ((eq pvar-list end))
    (clean-up-stack-pvar (car pvar-list))
    ))


(defun clean-up-stack-pvar (pvar)
  (declare (type pvar pvar))
  (let ((class (pvar-class pvar)))
    (ecase class
      (:general
	(let ((array (pvar-array pvar)))
	  (if array
	      (progn
		(return-pvar-array-to-pool array)
		(setf (pvar-array pvar) nil)
		)
	      (error "Internal error.  Allocated general pvar on temporary list does not have pvar array")
	      )))
      (:array
	(*map-array #'(lambda (element-pvar) (*deallocate-internal element-pvar :allocate!!)) pvar)
	(setf (pvar-array pvar) nil)
	)
      (:structure
	(*map-structure #'(lambda (slot-pvar) (*deallocate-internal slot-pvar :allocate!!)) pvar)
	(setf (pvar-structure pvar) nil)
	))))

;;;
;;; This function determines whether the user was returning
;;; a temporary pvar.  If the return-value is not a pvar, then
;;; we set the *TEMP-PVAR-LIST* back to its previous value and
;;; return the return-value.  If the return-value is in fact
;;; a temporary pvar, then we must arrange to return it.
;;;
;;; This is used by *DEFUN and *LET.
;;;
;;; This function is used EVERYWHERE because it controls the
;;; temporary pvar stack so it should be optimized as hell!
;;;



(defun handle-returning-pvar

       (return-value old-*temp-pvar-list* definitely-return-pvar)

  (cond

    ;; definitely-return-pvar is set iff the value being
    ;; returned is a pvar bound by the *LET or *LET* that
    ;; is doing this call to handle-returning-pvar.

    ;; If the pvar is an lvalue, then it was either allocated
    ;; on the heap or it was allocated by a *LET or *LET*
    ;; which has not yet returned.  Hence the pvar is not
    ;; to be regarded as temporary.

    ;; If the pvar has the property :heap-constant, then it
    ;; was created by !! as a temporary but it was put into
    ;; one of the constant hash tables and the actual pvar
    ;; and its array were allocated on the heap.  Hence it
    ;; is not to be regarded as temporary.

    ((or definitely-return-pvar
	 (and return-value
	      (pvar-p return-value)
	      (not (pvar-lvalue? (the pvar return-value)))
	      (not (eq :heap-constant (pvar-constant? (the pvar return-value))))
	      ))

;     (print 'in-here-0)

     ;; the user is returning a temporary pvar.
     ;; Lets trade it with the first thing in the OLD-*TEMP-PVAR-LIST*

     (cond

       ;; in the special case where the user happens
       ;;to be returning the first pvar in OLD-*TEMP-PVAR-LIST*,
       ;; we don't have to do anything at all except make
       ;; sure what we return has been converted into
       ;; a temporary pvar (since *LET variables are
       ;; allocated off the *TEMP-PVAR-LIST* but are not
       ;; marked as temporary) and then clean up what
       ;; is above us on the stack.

       ((eq return-value (first old-*temp-pvar-list*))
;	(print 'in-here-1)
	(setf (pvar-lvalue? (the pvar return-value)) nil)
	(clean-up-stack (cdr old-*temp-pvar-list*) *temp-pvar-list*)
	)

       (t

	;; Some random temporary pvar somewhere above where
	;; we are returning to on the stack is being returned.
	;; We are simply going to swap the contents of the
	;; pvar being returned with the pvar at the place on
	;; the stack we want to return to, then clean up the
	;; stack.

;	(print 'in-here-2)

	(let* ((return-pvar (first old-*temp-pvar-list*))
	       (old-return-pvar-array (pvar-array return-pvar))
	       (old-return-pvar-type (pvar-type return-pvar))
	       (old-return-pvar-structure-name (pvar-structure-name return-pvar))
	       )

;	  (print (list 'return-pvar return-pvar))
;	  (print (list 'old-return-pvar-array old-return-pvar-array))

	  ;; move pvar data to the one we are going
	  ;; to return from the one the user passed back
	  ;; Swap the data arrays.

	  (copy-pvar-slots return-pvar return-value)
	  (setf (pvar-array return-value) old-return-pvar-array)
	  (setf (pvar-type return-value) old-return-pvar-type)
	  (if (eq :structure old-return-pvar-type)
	      (setf (pvar-structure-name return-value) old-return-pvar-structure-name)
	      )
	  (setf (pvar-lvalue? return-pvar) nil)
	  (setq return-value return-pvar)

	  (clean-up-stack (cdr old-*temp-pvar-list*) *temp-pvar-list*)

	  ))

       )
	 
     ;; set the *TEMP-PVAR-LIST* to the element after the one we are returning

     (setq *temp-pvar-list* (cdr old-*temp-pvar-list*))

     )

    ;; If we weren't returning a PVAR, then just 'pop' the stack
    ;; and 'free up' any temporary pvars that were allocated
    ;; while the form which this function wraps around was evaluated.

    (T
     (clean-up-stack old-*temp-pvar-list* *temp-pvar-list*)
     (setq *temp-pvar-list* old-*temp-pvar-list*)
     )

    )

;  (when *fuck*
;    (print 'leaving)
;    (dotimes (j 5) (print (nth j *temp-pvar-original-list*)))
;    )

  return-value

  )

(defun *deallocate-internal (pvar type)
  (ecase (pvar-class pvar)
    (:general
      (setf (vp-set-heap-pvar-arrays (pvar-vp-set pvar))
	    (cons (pvar-location pvar) (vp-set-heap-pvar-arrays (pvar-vp-set pvar)))
	    ))
    (:array (*map-array #'(lambda (element-pvar) (*deallocate-internal element-pvar :allocate!!)) pvar))
    (:structure (*map-structure #'(lambda (slot-pvar) (*deallocate-internal slot-pvar :allocate!!)) pvar))
    )
  (smash-deallocated-pvar pvar)
  (ecase type
    (:allocate!! (setq *all-allocate!!-pvars* (delete pvar *all-allocate!!-pvars* :test #'eq)))
    (:*defvar nil)
    )
  )

(defvar smashed-location "THIS PVAR HAS BEEN DEALLOCATED.  YOU SHOULD NOT BE REFERENCING IT")

(defun set-pvar-length (pvar value)
  #+*LISP-HARDWARE
  (setf (pvar-length pvar) value)
  #+*LISP-SIMULATOR
  (declare (ignore pvar))
  value
  )



(defun smash-deallocated-pvar (pvar)
  ;; smash the pvar so that it doesn't get reused.
  (set-pvar-length pvar 0)
  (setf (pvar-type pvar) :general)
  (setf (pvar-location pvar) smashed-location))
