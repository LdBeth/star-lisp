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


(defun /!! (pvar1 &rest pvars)
  (*let ((result (apply #'internal-/!! pvar1 pvars)))
    (if!! (complexp!! result)
	  result
	  (float!! result)
	  )))


(*defun 
 *or (PVAR)
 "Returns the OR of PVAR in all selected processors"
 (simple-pvar-argument!! pvar)
 (safety-check (new-pvar-check pvar '*or))
 (incf-use-count '*or)
 (let ((pvar-array (pvar-array pvar)))
   (with-simple-vectors (pvar-array)
     (block xyzzy
            (do-for-selected-processors-internal (processor)
              (when (aref pvar-array processor)
                (return-from xyzzy t)
                ))
            nil
            ))))


(*defun 
 *and (pvar)
 "returns the AND of PVAR in all selected processors"
 (simple-pvar-argument!! pvar)
 (safety-check (new-pvar-check pvar '*and))
 (incf-use-count '*and)
 (let ((pvar-array (pvar-array pvar)))
   (with-simple-vectors (pvar-array)
     (block xyzzy
            (do-for-selected-processors-internal (processor)
              (when (not (aref pvar-array processor))
                (return-from xyzzy nil)
                ))
            t
            ))))


(defun any-active-processors? () (*or t!!))



(defun general-/=!! (function-name pvar-=-function scalar-/=-function pvar1 &rest pvars)

  (simple-pvar-argument!! pvar1 &rest pvars)

  (safety-check
    (new-pvar-check pvar1 function-name)
    (new-multiple-pvar-check pvars function-name)
    )
  (incf-use-count '/=!!)

  (cond ((null pvars) t!!)

	;; only two pvars, do it fast

	((= 1 (length pvars))
	 (let ((pvar2 (first pvars))
	       (return-pvar (allocate-temp-general-pvar))
	       )
	   (when
	     (with-selected-general-pvar-arrays (processor) (pvar1-array pvar2-array return-array) (pvar1 pvar2 return-pvar)
	       (setf (aref return-array processor)
		     (funcall scalar-/=-function (aref pvar1-array processor) (aref pvar2-array processor))
		     ))
	     (make-non-void return-pvar)
	     )
	   return-pvar
	   ))
	
	 ;; the general case.  we must test pvar1 for =!!
	 ;; to all the PVARS.  If any return T!!, then we
	 ;; must return NIL!!.  We must also recursively call /=!! on the pvars

	(t
	 (*let (return-pvar)
	   (*set return-pvar t!!)
	   (dolist (pvar pvars)
	     (*set return-pvar (and!! return-pvar (not!! (funcall pvar-=-function pvar1 pvar)))))
	   (and!! return-pvar (apply 'general-/=!! pvar-=-function scalar-/=-function pvars))
	   ))

	))

(defun /=!! (pvar &rest pvars)
  (apply 'general-/=!! '/=!! #'=!! #'/= pvar pvars)
  )

(defun char/=!! (pvar &rest pvars)
  (apply 'general-/=!! 'char/=!! #'char=!! #'char/= pvar pvars)
  )

(defun char-not-equal!! (pvar &rest pvars)
  (apply 'general-/=!! 'char-not-equal!! #'char-equal!! #'char-not-equal pvar pvars)
  )

(defun enumerate!! ()
  "returns a pvar that contains 0 through n-1
   where n is the number of selected processors"
  ;; the simulator always returns the enumeration in order.
  (incf-use-count 'enumerate!!)
  (let* ((return-pvar (allocate-temp-general-pvar))
         (return-array (pvar-array return-pvar))
         (counter 0)
         )
    (with-simple-vectors (return-array)
      (let ((any-set nil))
        (do-for-selected-processors-internal (processor)
          (setq any-set t)
          (setf (aref return-array processor) counter)
          (incf counter)
          )
        (when any-set (make-non-void return-pvar))
        )
      return-pvar
      )))



(defun deposit-byte!! (into-pvar position-pvar size-pvar source-pvar)
  "Returns the deposit-byte of the specified pvar arguments."
  (simple-pvar-argument!! (into-pvar position-pvar size-pvar source-pvar))
  (safety-check
    (new-pvar-check into-pvar 'load-byte!!)
    (new-pvar-check source-pvar 'load-byte!!)
    (new-pvar-check position-pvar 'load-byte!!)
    (new-pvar-check size-pvar 'load-byte!!)
    )
  (incf-use-count 'deposit-byte!!)
  (let* ((return-pvar (allocate-temp-general-pvar)))
    (when
      (with-selected-general-pvar-arrays
	(processor)
	(return-array into-array source-array position-array size-array)
	(return-pvar into-pvar source-pvar position-pvar size-pvar)
	(let ((position (aref position-array processor))
	      (size (aref size-array processor))
	      )
	  (setf (aref return-array processor)
		(dpb (aref source-array processor)
		     (byte size position)
		     (aref into-array processor)
		     ))))
      (make-non-void return-pvar)
      )
    return-pvar
    ))


(defun xor!! (&rest pvars)
  (simple-pvar-argument!! &rest pvars)
  (let ((len (length pvars)))
    (if (eql 0 len)
	nil!!
	(*let ((count (!! 0)))
	  (dolist (pvar pvars)
	    (*when pvar (*incf count))
	   )
	  (oddp!! count)
	 ))))


(*defun *xor (pvar)
  (simple-pvar-argument!! pvar)
  (safety-check (new-pvar-check pvar '*xor))
  (*when pvar (oddp (*sum (!! 1))))
  )


(defun rotate-field (integer n word-size direction)

  "Internal function used by rot!!"
  (let* ((bytespec (byte word-size 0))
	 (field (ldb bytespec integer))		       ; bits to rotate
	 (index (1- word-size))
	 (high-order-bit (expt 2 index))      
	 )
    (setf (ldb bytespec integer) 0)
    (dotimes (j n)
      (if (eq direction 'right)
	  (if (oddp field)
	      (setq field (+ high-order-bit (ash (1- field) -1)))
	      (setq field (ash field -1)))
	  (if (logbitp index field)
	      (setq field (1+ (ash (- field high-order-bit) 1)))
	      (setq field (ash field 1))
	      )))
    (+ integer field)
    ))


(defun rot!! (integer-pvar n-pvar word-size-pvar)
  (simple-pvar-argument!! (integer-pvar n-pvar word-size-pvar))
  (safety-check
   (new-pvar-check integer-pvar 'rot!!)
   (new-pvar-check n-pvar 'rot!!)
   (new-pvar-check word-size-pvar 'rot!!)
   )
  (let* ((integer-pvar-array (pvar-array integer-pvar))
         (n-pvar-array (pvar-array n-pvar))
         (word-size-pvar-array (pvar-array word-size-pvar))
         (return-pvar (allocate-temp-general-pvar))
         (return-pvar-array (pvar-array return-pvar))
         )
    (with-simple-vectors (integer-pvar-array n-pvar-array word-size-pvar-array)
      (let ((any-set nil))
        (do-for-selected-processors-internal (j)
          (setq any-set t)
          (let ((integer (aref integer-pvar-array j))
                (n (aref n-pvar-array j))
                (word-size (aref word-size-pvar-array j))
                )
            (setf (aref return-pvar-array j)
              (rotate-field integer (abs n) word-size (if (> n 0) 'left 'right)))
            ))
        (when any-set (make-non-void return-pvar))
        )
      return-pvar
      )))

(defun equal!! (pvar1 pvar2)
  (equalp!! pvar1 pvar2))

(defun equalp!! (pvar1 pvar2)

  (simple-pvar-argument!! (pvar1 pvar2))
  
  (cond

    ((and (simple-general-pvar-p pvar1) (simple-general-pvar-p pvar2))

     (cond

       ((and (*and (numberp!! pvar1))
	     (*and (numberp!! pvar2)))
	(=!! pvar1 pvar2))
       
       (t (eql!! pvar1 pvar2))

       ))

    ((and (array-pvar-p pvar1) (array-pvar-p pvar2))
     (if (not (equal (array-pvar-dimensions pvar1) (array-pvar-dimensions pvar2)))
	 nil!!
	 (*let ((result t!!))
	   (with-many-array-elements-iterated (element1 element2) ((pvar-array pvar1) (pvar-array pvar2))
	     (*set result (and!! result (equalp!! element1 element2)))
	     )
	   result
	   )))

    ((and (structure-pvar-p pvar1) (structure-pvar-p pvar2))
     (if (not (eq (type-of (pvar-structure pvar1)) (type-of (pvar-structure pvar2))))
	 nil!!
	 (*let ((result t!!))
	   (with-structure-elements-iterated
	     ((slot1 slot2)
	      ((pvar-structure pvar1) (pvar-structure pvar2))
	      (structure-pvar-type-front-end-slot-accessors (pvar-canonical-pvar-type pvar1))
	      )
	     (*set result (and!! result (equalp!! slot1 slot2)))
	     )
	   result
	   )))

    ((or (and (simple-general-pvar-p pvar1) (or (array-pvar-p pvar2) (structure-pvar-p pvar2)))
	 (and (simple-general-pvar-p pvar2) (or (array-pvar-p pvar1) (structure-pvar-p pvar1)))
	 (and (array-pvar-p pvar1) (structure-pvar-p pvar2))
	 (and (structure-pvar-p pvar1) (array-pvar-p pvar2))
	 )
     nil!!
     )

    (t
     (*let (result)
       (do-for-selected-processors-internal (j)
	 (*setf (pref result j) (equalp (pref pvar1 j) (pref pvar2 j)))
	 )
       result
       ))

     ))


(eval-when (load)
  (setq *char-code-limit char-code-limit)
  (setq *char-code-length (integer-length (1- *char-code-limit)))
  (setq *character-length 16)
  )

(defun initialize-character (&key code bits font front-end-p constantp)
  code bits font front-end-p constantp
  (warn "The *Lisp simulator does simulate characters accurately.~@
         It simply uses the values of char-code-limit, char-bits-limit~@
         and char-font-limit from the implementation of Common Lisp~@
         that it is running under.  Executing initialize-character~@
         thus has no effect.  Sorry."
	))


(defun code-char!! (code-pvar)
  (simple-pvar-argument!! code-pvar)
  (safety-check
    (new-pvar-check code-pvar 'code-char!!))
  (incf-use-count 'code-char!!)
  (let ((return-pvar (allocate-temp-general-pvar)))
    (when
      (with-selected-general-pvar-arrays
	(processor)
	(return-array code-array)
	(return-pvar code-pvar)
	(let ((code (aref code-array processor)))
	  (setf (aref return-array processor) (code-char code))
	  ))
      (make-non-void return-pvar)
      )
    return-pvar
    ))


(defun digit-char!! (weight-pvar &optional (radix-pvar (!! 10)) (font-pvar (!! 0)))
  (simple-pvar-argument!! weight-pvar &opt (radix-pvar font-pvar))
  (safety-check
    (new-pvar-check weight-pvar 'make-weight!!)
    (new-pvar-check radix-pvar 'make-weight!!)
    (new-pvar-check font-pvar 'make-weight!!)
    )
  (incf-use-count 'make-weight!!)
  (let ((return-pvar (allocate-temp-general-pvar)))
    (when
      (with-selected-general-pvar-arrays
	(processor)
	(return-array weight-array radix-array font-array)
	(return-pvar weight-pvar radix-pvar font-pvar)
	(let ((radix (aref radix-array processor))
	      (font (aref font-array processor))
	      (weight (aref weight-array processor))
	      )
	  (setf (aref return-array processor) (digit-char weight radix font))
	  ))
      (make-non-void return-pvar)
      )
    return-pvar
    ))


(*defun *integer-length (pvar)
  (simple-pvar-argument!! pvar)
  (safety-check (new-pvar-check pvar '*integer-length))
  (let ((return-length 0))
    (let ((pvar-array (pvar-array pvar)))
      (do-for-selected-processors-internal (processor)
	(setq return-length (max return-length (integer-length (aref pvar-array processor))))
	))
    return-length
    ))

(defun structurep!! (pvar)
  (simple-pvar-argument!! pvar)
  (!! (structure-pvar-p pvar)))

(defun twinkle-twinkle-little* (pvar)
  (simple-pvar-argument!! pvar)
  nil)

(defun v+-constant (v x) (map 'vector #'(lambda (y) (+ y x)) v))
(defun v--constant (v x) (map 'vector #'(lambda (y) (- y x)) v))
(defun v*-constant (v x) (map 'vector #'(lambda (y) (* y x)) v))
(defun v/-constant (v x) (map 'vector #'(lambda (y) (/ y x)) v))
(defun v+ (&rest v) (apply #'map 'vector #'+ v))
(defun v- (&rest v) (apply #'map 'vector #'- v))
(defun v* (&rest v) (apply #'map 'vector #'* v))
(defun v/ (&rest v) (apply #'map 'vector #'/ v))
(defun dot-product (v1 v2)
  (reduce #'+ (concatenate 'vector (apply #'v* (list v1 v2)))))
(defun vabs-squared (v) (funcall #'dot-product v v))
(defun vabs (v) (sqrt (vabs-squared v)))
(defun cross-product (x y)
  (vector
    (- (* (aref x 1) (aref y 2)) (* (aref x 2) (aref y 1)))
    (- (* (aref x 2) (aref y 0)) (* (aref x 0) (aref y 2)))
    (- (* (aref x 0) (aref y 1)) (* (aref x 1) (aref y 0)))
    ))
(defun vscale (v x) (map 'vector #'(lambda (y) (* y x)) v))
(defun vscale-to-unit-vector (v) (vscale v (/ (vabs v))))
(defun vector-normal (v1 v2)
  (vscale-to-unit-vector (cross-product  v1 v2)))
(defun vfloor (x) (map 'vector #'floor x))
(defun vceiling (x) (map 'vector #'ceiling x))
(defun vround (x) (map 'vector #'round x))
(defun vtruncate (x) (map 'vector #'truncate x))
(defun vlist (x) (concatenate 'list x))



(defun check-all-same-float-type (float-pvar function-name)
  (let ((first-type nil) (first-value nil))
    (let ((float-pvar-array (pvar-array float-pvar)))
      (do-for-selected-processors-internal (j)
	(if (null first-type)
	    (progn
	      (setq first-value (svref float-pvar-array j))
	      (setq first-type (type-of first-value))
	      )
	    (if (not (eq first-type (type-of (svref float-pvar-array j))))
		(error "The argument given to ~S does not contain floats of the same exact type everywhere" function-name)
		))))
    first-value
    ))




(defvar *not-all-floats-error-message* "The argument given to ~S does not contain floats everywhere")

(defmacro make-float-limit-function
	  (function-name short-float-value single-float-value double-float-value long-float-value)
  `(defun ,function-name (float-pvar)
     (simple-pvar-argument!! float-pvar)
     #+SYMBOLICS
     (progn ,single-float-value ,long-float-value) ; ignore these two on Symbolics
     (when (not (*and (floatp!! float-pvar))) (error *not-all-floats-error-message* ',function-name))
     (etypecase (check-all-same-float-type float-pvar ',function-name)
       (short-float (!! ,short-float-value))
       #-SYMBOLICS
       (single-float (!! ,single-float-value))
       (double-float (!! ,double-float-value))
       #-SYMBOLICS
       (long-float (!! ,long-float-value))
       )))


(make-float-limit-function
  least-positive-float!!
  least-positive-short-float least-positive-single-float least-positive-double-float least-positive-long-float
  )

#-KCL
(make-float-limit-function
  most-positive-float!!
  most-positive-short-float most-positive-single-float most-positive-double-float most-positive-long-float
  )

#+KCL
(eval
  '(make-float-limit-function
     most-positive-float!!
     most-positive-short-float most-positive-single-float most-positive-double-float most-positive-long-float
     )
  )

(make-float-limit-function
  float-epsilon!!
  short-float-epsilon single-float-epsilon double-float-epsilon long-float-epsilon
  )

(make-float-limit-function
  negative-float-epsilon!!
  short-float-negative-epsilon single-float-negative-epsilon double-float-negative-epsilon long-float-negative-epsilon
  )

(make-float-limit-function
  least-negative-float!!
  least-negative-short-float least-negative-single-float least-negative-double-float least-negative-long-float
  )

#-KCL
(make-float-limit-function
  most-negative-float!!
  most-negative-short-float most-negative-single-float most-negative-double-float most-negative-long-float
  )

#+KCL
(eval
  '(make-float-limit-function
     most-negative-float!!
     most-negative-short-float most-negative-single-float most-negative-double-float most-negative-long-float
     ))

  
(defun set-vp-set-geometry (vp-set geometry)
  (declare (ignore vp-set geometry))
  (error "The *Lisp Simulator does not currently allow changing the geometry of a vp set")
  )




(defun integer-reverse!! (integer-pvar)
  (declare (ignore integer-pvar))
  (error "This function is unimplementable on the *Lisp Simulator.~@
          It depends on internal representations and field lengths,~@
          which the Simulator knows nothing about."
	 ))


(defun taken-as!! (pvar type)
  (declare (ignore type))
  (new-pvar-check pvar 'taken-as!!)
  (error "This function is unimplementable on the *Lisp Simulator.~@
          It depends on internal representations of data inside the ~@
          Connection Machine, which the simulator knows nothing about."
	 ))


(defun sideways-array-p (array-pvar)
  (safety-check
    (new-pvar-check array-pvar 'sideways-array-p)
    (assert (array-pvar-p array-pvar))
    )
  (pvar-sideways-p array-pvar)
  )


(defun typep!! (pvar scalar-type)
  (safety-check
    (new-pvar-check pvar 'typep!!)
    )
  (incf-use-count 'typep!!)
  (ecase (pvar-type pvar)
    (:general
      (let ((return-pvar (allocate-temp-general-pvar)))
	(when
	  (with-selected-general-pvar-arrays
	    (processor)
	    (return-array pvar-array)
	    (return-pvar pvar)
	    (setf (aref return-array processor) (typep (aref pvar-array processor) scalar-type))
	    )
	  (make-non-void return-pvar)
	  )
	return-pvar
	))
    (:structure
      (if (typep (pvar-location pvar) scalar-type) t!! nil!!)
      )
    (:array
      (error "Implementation deficiency.  Cannot do type!! correctly on array pvars.  Sorry.")
      )))


(defun *setf-realpart!! (pvar value)
  (*set pvar (complex!! value (imagpart!! pvar)))
  )

(defun *setf-imagpart!! (pvar value)
  (*set pvar (complex!! (realpart!! pvar) value))
  )


(defun *room (&key (how :by-vp-set) (print-statistics t) (stream t))
  (declare (ignore how print-statistics))
  (format stream "~%The *Lisp Simulator does not keep memory usage information.  Sorry.")
  )


(defun track-stack (&optional trace-type (trace-action :trace) (verbose t))
  (declare (ignore trace-type trace-action verbose))
  (format t "~%The *Lisp Simulator does not keep stack memory information.  Sorry.")
  (values)
  )


(*defun *array-rank (pvar-array)
  "Returns to the front end the number of dimensions of pvar-array.
   It is an error if pvar-array were to be a general-pvar array
   containing different size arrays.
  "
  (simple-pvar-argument!! pvar-array)
  (safety-check (new-pvar-check pvar-array '*array-rank))
  (case-pvar-array
    (pvar-array)
    nil
    (array-pvar-rank pvar-array)
    ))
