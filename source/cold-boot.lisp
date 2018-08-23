;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *sim-i; MUSER: YES-*-

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


(defmacro *cold-boot (&rest args)
  (declare-arglist (&key initial-dimensions initial-geometry-definition safety undefine-all))
  (if *inside-*defun-p*
      (error "*COLD-BOOT is being called from inside a *DEFUN.  This will not work because *COLD-BOOT resets some ~@
              state used by the *DEFUN mechanism.  You should only run *COLD-BOOT from your top level function ~@
              and that function should be defined with DEFUN, not *DEFUN."
	     ))
  `(*cold-boot-1 ., args))


(defmacro *warm-boot ()
  (if *inside-*defun-p*
      (error "*WARM-BOOT is being called from inside a *DEFUN.  This will not work because *WARM-BOOT resets some ~@
              state used by the *DEFUN mechanism.  You should only run *WARM-BOOT from your top level function ~@
              and that function should be defined with DEFUN, not *DEFUN."
	     ))
  `(*warm-boot-1)
  )


(defvar *previous-default-geometry-definition* nil)
(defvar *previous-physical-number-of-processors-limit* nil)


(defun *cold-boot-1 (&key initial-dimensions initial-geometry-definition (safety 3) (undefine-all nil))


  (declare (special *has-a-*cold-boot-been-done-p*))

  (when (integerp initial-dimensions)
    (setq initial-dimensions (list initial-dimensions)))

  (when undefine-all
    (undefine-all-vp-sets-*defvars-and-allocate!!-pvars)
    )

  (starlisp-run-initializations *before-*cold-boot-initializations*)

  (let ((inside-*cold-boot-p t))
    (declare (special inside-*cold-boot-p))
    (paris-attach)

    (reinitialize-*lisp-geometries)

    (let ((geometry-definition-for-default-vp-set
	    (create-default-geometry-definition initial-dimensions initial-geometry-definition)
	    ))

      (setq *previous-default-geometry-definition* geometry-definition-for-default-vp-set)
		
      (paris-cold-boot)

      (setq *first-*cold-boot* nil)

      ;; Pop the *Lisp stack back to the beginning.

      (pop-starlisp-stack)

      ;; Sets up all the VP SETS.  Needs to know default-geometry-definition
      ;; so it can set up *default-vp-set*.

      (vp-set-*cold-boot-initialization geometry-definition-for-default-vp-set)

      )

    (set-hardware-context-and-test-and-simulator-initial-pvar-value)

    (setq *lisp-runnable-p t)

    ;; (Re)Initialize all *defvars, save those
    ;; defined by Vp Sets which have not been
    ;; allocated processors yet.

    (*defvar-*cold-boot-initialization)

    (setq *has-a-*cold-boot-been-done-p* t)

    (set-safety safety)

    (print-starlisp-herald)

    (starlisp-run-initializations *after-*cold-boot-initializations*)

    (setup-self-news)

    (setq *previous-physical-number-of-processors-limit* (vp-set-total-size *default-vp-set*))
    (values *minimum-size-for-vp-set* *current-cm-configuration*)

    ))


(defun *warm-boot-1 ()
  (starlisp-run-initializations *before-*warm-boot-initializations*)
  ;; What does this do, if anything?
  (paris-warm-boot)
  (pop-starlisp-stack)
  (*warm-boot-vp-sets)
  (starlisp-run-initializations *after-*warm-boot-initializations*)
  )


(defun set-safety (safety)
  (ecase safety
    ((3 2 1 0) (setq *interpreter-safety* safety))
    )
  (set-paris-safety safety)
  )


(defun undefine-all-vp-sets-*defvars-and-allocate!!-pvars ()
  (mapc
    #'(lambda (vp-set)
	(setf (vp-set-internal-id vp-set) nil)
	(if (boundp (vp-set-name vp-set)) (makunbound (vp-set-name vp-set)))
	)
    *all-def-vp-sets*
    )
  (setq *all-def-vp-sets* nil)
  (dolist (*defvar-specification *all-*defvar-specifications*)
    (let ((name (*defvar-specification-name *defvar-specification)))
      (when name (makunbound name))
      ))
  (setq *all-*defvar-specifications* nil)
  (setq *all-allocate!!-pvars* nil)
  (setq *all-dynamic-vp-sets* nil)
  )


(defun create-default-geometry-definition (initial-dimensions initial-geometry-definition)

  (cond

    ;; If the user specified initial dimensions, use those.

    (initial-dimensions
     (if initial-geometry-definition
	 (error "*COLD-BOOT: Do not specify both INITIAL-DIMENSIONS and INITIAL-GEOMETRY-DEFINITION.~@
                     An INITIAL-GEOMETRY-DEFINITION contains INITIAL-DIMENSIONS as part of it.
                    "
		)
	 `(create-geometry :dimensions ',initial-dimensions)
	 ))

    ;; If the user specified an initial-geometry-definition use that.

    (initial-geometry-definition)

    ;; If neither were provided, if there is a previous geometry definition
    ;; created for the default vp set during the last *cold-boot, use that.

    (*previous-default-geometry-definition*)

    ;; Otherwise, make a default geometry, 2 dimensional, such that
    ;; X and Y are as nearly equal as possible, X >= Y.

    (t (default-geometry-definition-from-machine-size))

    ))



;;; NON-PORTABLE, SIMULATOR SPECIFIC, *COLD-BOOT RELATED STUFF.



(defun vp-set-*cold-boot-initialization (default-geometry-parameters-form)

  "Performs all actions necessary at *COLD-BOOT time
   to make VP SETS work.
  "

  ;; Set up the *default-vp-set*.  This is where all *DEFVARS
  ;; will go which don't have a VP SET specified in their definition.

  (let ((default-vp-set-geometry (eval default-geometry-parameters-form)))
    (setf (vp-set-geometry *default-vp-set*) default-vp-set-geometry)
    (setf (vp-set-constants-hash-table *default-vp-set*) nil)
    )

  ;; set up a pool of pvar arrays for the default vp set.

  (initialize-table-of-pvar-array-pools)
  (setf (vp-set-pvar-array-pool *default-vp-set*) (find-pool-of-size-or-make-one (vp-set-total-size *default-vp-set*)))

  ;; Set up a context stack for the default vp set.
  
  (setf (vp-set-context-stack *default-vp-set*) (create-context-stack (vp-set-total-size *default-vp-set*)))
  (setf (vp-set-context-level *default-vp-set*) 0)
  
  (setq *css-current-level* (vp-set-context-level *default-vp-set*))
  (setq *css* (initialize-context-stack (vp-set-context-stack *default-vp-set*)))
  (setq *current-context-stack* (vp-set-context-stack *default-vp-set*))
  
  (setup-vp-set-heap-pvar-list *default-vp-set*)
  
  ;; Allocate T!! and NIL!!
  
  (define-vp-set-basic-pvars *default-vp-set*)
  
  (setf (vp-set-instantiated *default-vp-set*) t)
  
  ;; Define the grid to cube and vice versa mapping
  
  (create-cube-address-array-for-vp-set *default-vp-set*)
  (create-grid-address-array-for-vp-set *default-vp-set*)
  (verify-consistent-mapping-for-vp-set *default-vp-set*)
  
  ;; Make the default vp set be the current vp set.
  
  (set-vp-set *default-vp-set*)
  
  ;; smash all VP SETS previously created using CREATE-VP-SET.
  
  (dolist (vp-set *all-dynamic-vp-sets*)
    (let ((message "This VP SET has been deallocated by *COLD-BOOT.  You should not be accessing it"))
      (smash-vp-set-with-message vp-set message)
      ))
  
  (setq *all-dynamic-vp-sets* nil)
  
  ;; Instantiate all defined VP SETS except those specifically
  ;; designated as to be given sizes and enabled some time
  ;; during program execution.
  
  (dolist (vp-set *all-def-vp-sets*)
    (setf (vp-set-geometry vp-set) nil)
    (setf (vp-set-nesting-level vp-set) 0)
    (setf (vp-set-instantiated vp-set) nil)
    (setf (vp-set-internal-id vp-set) nil)
    (when (not (vp-set-voidable vp-set))
      (instantiate-vp-set vp-set)
      ))

    )




(defun set-hardware-context-and-test-and-simulator-initial-pvar-value ()
  (setq *junk-to-initialize-with* (gensym "ILLEGAL-VALUE-"))
  )

(defun pop-starlisp-stack ()
  (*warm-boot-temporary-pvars)
  (setq *temp-pvar-list* *temp-pvar-original-list*)
  (when (null *temp-pvar-list*) (create-some-more-temporary-pvars))
  )


(defun default-geometry-definition-from-machine-size () `(create-geometry :dimensions '(8 4)))
(defun paris-attach () nil)
(defun paris-cold-boot () nil)
(defun set-paris-safety (value) (declare (ignore value)) nil)
(defun setup-self-news () nil)
(defun paris-warm-boot () nil)





(defun *warm-boot-vp-sets ()

  ;; Reset context in all the defined VP SETS.

  (dolist (vp-set-list (list *all-def-vp-sets* *all-dynamic-vp-sets*))
    (dolist (vp-set vp-set-list)
      (when (vp-set-instantiated-p vp-set)
	(setf (vp-set-context-level vp-set) 0)
	(setf (vp-set-nesting-level vp-set) 0)
	)))

  (setf (vp-set-nesting-level *default-vp-set*) 0)
  (setf (vp-set-context-level *default-vp-set*) 0)

  ;; Restore all parameters of the default vp set

  (set-vp-set *default-vp-set*)

  )

(defun *warm-boot-temporary-pvars ()
  (do ((pvar-list *temp-pvar-original-list* (cdr pvar-list)))
      ((eq pvar-list *temp-pvar-list*))
    (clean-up-stack-pvar (car pvar-list))
    )
  (do ((pvar-list *temp-pvar-list* (cdr pvar-list)))
      ((null pvar-list))
    (when (not (null (pvar-array (car pvar-list))))
      (warn "Some temporary pvar beyond those currently allocated contains data.  Yikes!")
      (clean-up-stack-pvar (car pvar-list))
      (format t "Pvar is: ~S" (car pvar-list))
      )))






;;;; Initialization forms.


(defun starlisp-run-initializations (forms)
  (evaluate-initialization-forms forms)
  )


(defun evaluate-initialization-forms (forms)
  (mapc #'(lambda (form) (eval (cdr form))) forms)
  nil
 )


(defun check-initialization-arguments (name-of-form form variable fname)
  (assert (and (or (symbolp name-of-form) (stringp name-of-form)) name-of-form) (name-of-form)
	  "~S: The name-of-form argument (~S) is not a string" fname name-of-form)
  (when (symbolp name-of-form) (setq name-of-form (symbol-name name-of-form)))
  (assert (listp form) (form) "~S: The form argument (~S) is not a list" fname form)
  (assert (symbolp variable) (variable) "~S: The variable argument (~S) is not a symbol" fname variable)
  (setq variable (list variable))
  (mapc
    #'(lambda (v)
	(assert (member v *legal-initialization-symbols*) ()
	  "~S: ~S is not a valid name of an initialization list" fname v))
    variable
   )
  (values name-of-form form variable)
 )

(defun add-initialization (name-of-form form variable)
  (multiple-value-bind (name-of-form form variable)
      (check-initialization-arguments name-of-form form variable 'add-initialization)
    (mapc
      #'(lambda (v)
	  (let* ((forms-list (eval v))
		 (existing-form (assoc name-of-form forms-list :test #'equal))
		)
	    (when existing-form
	      (if (not (equal (cdr existing-form) form))
		  (error "~S:  A form by the name ~S, (~S) already exists for list ~S"
			 'add-initialization name-of-form form v)
		  (return-from add-initialization nil)
	       ))
	    (set v (nconc forms-list (list (cons name-of-form form))))
	   ))
      variable
     )
    name-of-form
   ))

(defun delete-initialization (name-of-form variable)
  (multiple-value-bind (name-of-form form variable)
      (check-initialization-arguments name-of-form 0 variable 'delete-initialization)
    #-SYMBOLICS
    (declare (ignore form))
    #+SYMBOLICS
    form  ;; ignored argument
    (mapc
      #'(lambda (v)
	  (let* ((forms-list (eval v))
		 (existing-form (assoc name-of-form forms-list :test #'equal))
		)
	    (if existing-form
		(set variable (delete existing-form forms-list :test #'equal))
		(format t "An initialization named ~S is not present for list ~S"
			  name-of-form v)
	     )))
      variable
     ))
  nil
 )

;;;****************************************************************


;;; Auxiliary functions for *COLD-BOOT


(defun check-cold-boot-dimensions (initial-dimensions)

  ;; check that initial dimensions are positive integers and satisfy constraints

  (when (numberp initial-dimensions)
    (setq initial-dimensions (list initial-dimensions 1)))

  (when (null (listp initial-dimensions))
    (error "Bad argument to *cold-boot: ~S" initial-dimensions))

  (mapc
    #'(lambda (x)
        (when (not (valid-integer-range x 1 most-positive-fixnum))
          (error "Illegal initial dimension specified: ~S" x))
        (when (and (not *do-not-print-anything-while-*cold-booting*)
		   (not (power-of-two-p x))
		   )
          (format *error-output* "~%Warning:  Hardware does not currently support non-power-of-two dimensions~%")
	  ))
    initial-dimensions
   )

  (when (and (not *do-not-print-anything-while-*cold-booting*)
	     (> (reduce #'* initial-dimensions) 256)
	     )
    (warn "This is a simulator!  Asking it to simulate many processors will ~@
cause it to run very slowly, generally in proportion to the number of processors. ~@
Also, it will consume increased amounts of space, causing you to GC and page ~@
a lot.  The speed of the simulator in no way reflects the speed of *Lisp ~@
running on a Connection Machine (which will in general run just as fast ~@
no many how many physical processors are present), but rather reflects the ~@
speed of a) the machine you are running on, b) the speed of the Lisp you ~@
are running on, and c) the skill (or lack thereof) with which this simulator ~@
was written.  The *Lisp simulator's goal is to help you write and debug ~@
*Lisp code, not to provide a miniature Connection Machine on which to run ~@
large (or small) computationally intensive *Lisp applications.  You can turn ~@
this warning off by doing (setq *do-not-print-anything-while-*cold-booting* t)"
	  ))

  )


(defun compare-ordered-numeric-lists (list1 list2)
  "Returns 'equal, 'less-than or 'greater-than"
  (if (not (eql (length list1) (length list2)))
      nil
      (if (null list1)
	  'equal
	  (if (eql (car list1) (car list2))
	      (compare-ordered-numeric-lists (cdr list1) (cdr list2))
	      (if (< (car list1) (car list2))
		  'less-than
		  'greater-than
	       )))))


(defun print-starlisp-herald ()
  (when (not *do-not-print-anything-while-*cold-booting*)
    (format t "~%~%~A.  Version ~S~%~%"
	    *starlisp-simulator-header* *starlisp-simulator-version*)
      (multiple-value-bind
	(second minute hour date month year day-of-week dst time-zone)
	  (get-decoded-time)
	second minute hour day-of-week dst time-zone   ;; ignored variables
	(when (eq 'greater-than (compare-ordered-numeric-lists
				  (list year month date)
				  *expiration-date*
				 ))
	  (format t "~%Warning:  This version is now out of date.~%")
	  (format t "You should get a newer version from TMC.~%~%")
	 ))))



(defun reset-*lisp-function-use-statistics ()
  (do-external-symbols (function-symbol (find-package '*sim))
    (setf (get function-symbol 'use-count) 0)
    )
  (dolist (function-symbol '(internal-/!! internal-sqrt!! internal-isqrt!!))
    (setf (get function-symbol 'use-count) 0)
    )
  t)


(defun display-*lisp-function-use-statistics ()
  (terpri)
  (do-external-symbols (function-symbol (find-package '*sim))
    (let ((count (get function-symbol 'use-count)))
      (if (not (zerop count))
	  (format t "~A: ~D~%" (symbol-name function-symbol) count)
	  )))
  (values)
  )

;;; functions to kill off pvars 


(defun kill-all-permanent-pvars ()  (setq *all-*defvar-specifications* nil))

