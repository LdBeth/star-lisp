;;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *LISP; -*-

(in-package '*lisp)
						
;;;; Author:  JP Massar.


;;;; The object of this excercise is to add two binary numbers,
;;;; represented 1 bit per processor in cube address order, with
;;;; the least signficant bit of each number being in processor 0.

;;;; The algorithm used is to figure out which processors will
;;;; receive a carry bit, using an exclusive scan!!, and then
;;;; figure out the result in that processor as a function of
;;;; the values in that processor of the two binary numbers
;;;; and the carry bit.

;;;; This is a simple example program which does not use much in
;;;; the way of new 5.0 *Lisp features, except for *locally,
;;;; pppdbg and return-pvar-p.

;;;; This text resides in
;;;; /cm/starlisp/interpreter/f5301/very-long-addition-example.lisp
;;;; Ask your systems manager or applications engineer for its
;;;; exact location at your installation.


(defmacro *nocompile (&body body)
  `(progn ,@body)
  ;;`(with-compile-time-local-property (compile-time-prop *compilep* nil)
  ;;,@body)
  )


(defun very-long-add!!

       (bit-pvar1 bit-pvar2 length1 length2 &optional (check-overflow t))

  (declare (type fixnum length1 length2))

  nil

  (let ((last-processor (1- *number-of-processors-limit*)))
    (declare (type fixnum last-processor))

    (when check-overflow
      (assert
	(or (and (< length1 *number-of-processors-limit*)
		 (< length2 *number-of-processors-limit*)
		 )
	    (not (and
		   (plusp (*nocompile (pref bit-pvar1 last-processor)))
		   (plusp (*nocompile (pref bit-pvar2 last-processor)))
		   )))
	()
	"You are trying to add two numbers who result would be ~@
       a number longer than ~D binary digits, the total size of the ~@
       current vp set."
	*number-of-processors-limit*
	))


    (*all
      (declare (return-pvar-p t))

      ;; The input arguments, bit-pvar1 and bit-pvar2
      ;; may not be pvars of type (unsigned-byte 1)
      ;; (they may be general pvars, for instance).
      ;; Allocate two temporarys of the proper type
      ;; and coerce the input arguments into them.
      ;; The *SET's will error out if the input arguments
      ;; do not everywhere contain 1 or 0.

      (*let ((source1 0)
	     (source2 0)
	     dest
	     )
	(declare (type (pvar (unsigned-byte 1)) source1 source2 dest))
	(declare (return-pvar-p t))

	(*when (<!! (self-address!!) length1)
	  (*nocompile (*set source1 bit-pvar1))
	  )

	(*when (<!! (self-address!!) length2)
	  (*nocompile (*set source2 bit-pvar2))
	  )

	;; Any processor which contains a 1 in both input arguments
	;; is going to propagate a carry in all subsequent processors
	;; until a processor which contains a 0 in both arguments
	;; is encountered.

	;; Any processor which contains a 0 in both input arguments
	;; is going to cause a carry to not be propagated beyond it,
	;; and will prevent any carry from being generated until
	;; a subsequent processor containing 1 in both arguments
	;; is encountered.

	;; Thus processors with either two 1's or two 0's define
	;; 'segments' of carry propagation or non-propagation.

	(*let* ((zero-zero (and!! (zerop!! source1) (zerop!! source2)))
		(one-one (and!! (not!! (zerop!! source1))
				(not!! (zerop!! source2))
				))
		(zero-one (not!! (or!! zero-zero one-one)))
		(carry-segment
		  (or!! (zerop!! (self-address!!)) zero-zero one-one))
		will-receive-carry
		)
	  (declare (type boolean-pvar zero-zero one-one zero-one)
		   (type boolean-pvar carry-segment will-receive-carry)
		   )
	  (declare (return-pvar-p nil))

	  (*set will-receive-carry
		(scan!! one-one 'copy!!
			:segment-pvar carry-segment :include-self nil
			))
	  (*setf (pref will-receive-carry 0) nil)

	  (when check-overflow
	    (if (and (pref will-receive-carry last-processor)
		     (not (pref zero-zero last-processor))
		     )
		(error
		  "You are trying to add two numbers who result ~@
                     would be a number longer than ~D binary digits, ~@
                     the total size of the current vp set."
		  *number-of-processors-limit*
		  )))

	  ;; This implements the three-input binary addition
	  ;; algorithm.

	  (*set dest
		(cond!!
		  ((and!! (or!! one-one zero-zero) will-receive-carry) 1)
		  ((or!! one-one zero-zero) 0)
		  ((and!! zero-one will-receive-carry) 0)
		  (zero-one 1)
		  (t!! (when (*or t!!) (error "This can never happen!")) 0)
		  )))

	dest

	))))


(defun test-very-long-add ()
  (*warm-boot)
  (let ((length1 (1+ (random 20)))
	(length2 (1+ (random 20)))
	)
    (*let (source1 source2)
      (declare (type (field-pvar 1) source1 source2))
      (dotimes (j length1)
	(*setf (pref source1 j) (random 2))
	)
      (dotimes (j length2)
	(*setf (pref source2 j) (random 2))
	)
      (pppdbg source1 :end length1 :format "~1D ")
      (pppdbg source2 :end length2 :format "~1D ")
      (ppp (very-long-add!! source1 source2 length1 length2 t) :end 22 :format "~1D " :title "RESULT ")
      )))


#|

;; SAMPLE OUTPUT

  (test-very-long-add)
SOURCE1: 0 0 0 
SOURCE2: 1 0 1 0 0 0 0 0 0 1 0 1 0 1 0 0 0 0 
RESULT : 1 0 1 0 0 0 0 0 0 1 0 1 0 1 0 0 0 0 0 0 0 0 
NIL
  (test-very-long-add)
SOURCE1: 0 1 1 1 0 0 1 1 1 0 
SOURCE2: 1 0 1 1 1 0 0 1 1 0 0 0 0 1 0 0 0 
RESULT : 1 1 0 1 0 1 1 0 1 1 0 0 0 1 0 0 0 0 0 0 0 0 
NIL
  (test-very-long-add)
SOURCE1: 1 0 1 0 1 0 0 0 1 0 1 1 1 1 1 1 
SOURCE2: 0 1 1 1 1 0 1 1 0 0 0 0 0 1 0 1 0 1 
RESULT : 1 1 0 0 1 1 1 1 1 0 1 1 1 0 0 1 1 1 0 0 0 0 
NIL
  (very-long-add!! (!! 1) (!! 1) *number-of-processors-limit* *number-of-processors-limit* t)
Error: You are trying to add two numbers who result would be 
a number longer than 512 binary digits, the total size of the 
current vp set.

VERY-LONG-ADD!!
   Arg 0 (BIT-PVAR1): #<FIELD-Pvar 4-1 *DEFAULT-VP-SET* (32 16)>
   Arg 1 (BIT-PVAR2): #<FIELD-Pvar 12-1 *DEFAULT-VP-SET* (32 16)>
   Arg 2 (LENGTH1): 512
   Arg 3 (LENGTH2): 512
   Arg 4 (CHECK-OVERFLOW): T

|#
