;;; -*- Syntax: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *sim-i; MUSER: YES -*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


#|  External interface for context stacks.



  Each Vp Set has its own context stack.

  *current-context-stack*

  The current VP Set's stack is bound to *current-context-stack*;
  this is done by *WITH-VP-SET and SET-VP-SET.

  *css*

  The actual bit-vector which is the css is bound to *CSS*.
  The variable is bound by things like *WHEN and *ALL,
  macros which affect context.  It of course also changes
  when the current vp set is switched.

  *current-css-level*

  This indicates how many nested levels of context exist per
  vp set.  It is bound in the same places *css* is bound.


  Create-context-stack 

  is used to create a context stack with
  a given number of processors.  It is used by the DEF-VP-SET
  and CREATE-VP-SET mechanisms.  The context stack becomes
  one of the slots of the Vp Set defstruct.

  initialize-context-stack

  is used to set up the context stack to a state where
  every processor is turned on.  If *css-current-level*
  is not zero this routine will error out as a safety
  check.

  push-css

  Pushes a new context, which is a function of the current
  context and its argument, a pvar.

  pop-css

  Pops a context and restores context to the
  previous context.  If the context stack underflows
  an internal error is signalled.

  pop-css-to-level

  Restores context to a particular context, indicated
  by an index.  This allows stack discipline to be
  violated within a *ALL or *WHEN and still have it
  restored properly when properly exiting the *ALL or *WHEN.

  get-context-at-level

  Returns the bit-vector at a given level in the stack.

|#


(defvar *initial-context-stack-length* 32)
(defvar *context-stack-length-increment* 32)

(defstruct context-stack
  (length 0 :type fixnum)
  arrays 
  )

(defun create-context-stack (number-of-processors)
  (declare (type fixnum number-of-processors))
  (let ((context-stack (make-context-stack)))
    (declare (type context-stack context-stack))
    (setf (context-stack-length context-stack) *initial-context-stack-length*)
    (setf (context-stack-arrays context-stack) (make-array *initial-context-stack-length*))
    (let ((arrays (context-stack-arrays context-stack)))
      (dotimes (j *initial-context-stack-length*)
	(setf (aref arrays j) (make-array number-of-processors :element-type 'bit))
	))
    context-stack
    ))

(defun initialize-context-stack (context-stack)
;;  (assert (zerop *css-current-level*) () "You are initializing the context stack but its level is not 0")
  (let* ((arrays (context-stack-arrays context-stack))
	 (first-array (aref arrays 0))
	 )
    (declare (type bit-vector first-array))
    (dotimes (j (length first-array))
      (setf (sbit first-array j) 1)
      )
    first-array
    ))

(defun get-context-at-level (context-stack level)
  (assert (< level (context-stack-length context-stack)) () "Internal error.  Attempt to get context beyond stack")
  (aref (context-stack-arrays context-stack) level)
  )

(defun incf-css-level ()
  (let ((context-stack *current-context-stack*)
	(current-level (incf *css-current-level*))
	)
    (declare (type context-stack context-stack))
    (declare (type fixnum current-level))
    (let ((length (context-stack-length context-stack)))
      (declare (type fixnum length))
      (cond
	((< current-level length) nil)
	((= current-level length)
	 (let* ((new-length (+ length *context-stack-length-increment*))
		(new-array (make-array new-length))
		(old-array (context-stack-arrays context-stack))
		)
	   (dotimes (j new-length)
	     (declare (fixnum j))
	     (if (< j length)
		 (setf (aref new-array j) (aref old-array j))
		 (setf (aref new-array j) (make-array *number-of-processors-limit* :element-type 'bit))
		 ))))
	((> current-level length) (error "Internal error.  Current context stack level exceeds length of stack"))
	))))




(defun push-css (pvar &optional (always nil))
  (safety-check
    (when (not (internal-pvarp pvar))
      (error "The argument to PUSH-CSS, ~S, is not a PVAR.  You may be ~@
              using a *WHEN or a *IF with a condition that does not really ~@
              evaluate to a PVAR."
	     pvar
	     )))
  (ecase (pvar-type pvar)
    (:general
      (new-pvar-check pvar 'internal-*Lisp)
      (incf-css-level)
      (let* ((pvar-data (pvar-data pvar))
	     (old-css *css*)
	     (new-css (get-context-at-level *current-context-stack* *css-current-level*))
	     )
	(declare (type bit-vector old-css new-css))
	(declare (type vector pvar-data))
	(if always
	    (dotimes (p *number-of-processors-limit*)
	      (setf (sbit new-css p) (the bit (if (aref pvar-data p) 1 0)))
	      )
	    (dotimes (p *number-of-processors-limit*)
	      (setf (sbit new-css p) (logand (sbit old-css p) (the bit (if (aref pvar-data p) 1 0))))
	      ))
	(setq *css* new-css)			;make the new CSS the current CSS
	))
    ((:array :structure) (push-css t!!))
    ))


;;; This will just simply pop out of the current context into the previous context

(defun pop-css ()
  (decf *css-current-level*)
  (if (<= *css-current-level* 0)
      (error "Internal error.  Context stack underflow.  This isn't supposed to happen!!")
   )
  (setq *css* (get-context-at-level *current-context-stack* *css-current-level*))
  )

(defun pop-css-to-level (level)
  (assert (>= level 0) () "*LISP internal error.  Context stack underflow.")
  (assert (<= level *css-current-level*) () "*Lisp internal error.  Attempt to position context at or above current context")
  (setq *css-current-level* level)
  (setq *css* (get-context-at-level *current-context-stack* *css-current-level*))
  )

(defun push-css-select-all () (push-css t!! t))

(defun push-css-select-none () (push-css nil!! t))


