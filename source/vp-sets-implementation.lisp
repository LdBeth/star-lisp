;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *sim-i; MUSER: YES-*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


;;;; DEF-VP-SET and CREATE-VP-SET


(defmacro def-vp-set (name dimensions &key (geometry-definition-form nil) (*defvars nil))

  "Define a top level VP SET.  If dimensions is NIL and geometry-definition-form
   is not provided, the vp set is defined but not instantiated.  Otherwise
   the vp set is both defined and instantiated, unless a *COLD-BOOT
   has not yet been done, in which case the instantiation will take
   place at *COLD-BOOT time.

   If dimensions is NON-NIL it is illegal to specify the geometry-definition-form
   as well, as geometry-definition-form includes the dimensions.

   The *DEFVARS form, if provided, is a list of lists, each sublist
   consisting of a symbol, an optional initial value form, and an
   optional documentation string and an optional type declaration.
   If the type declaration is given a *PROCLAIM will be cons up
   using that type information.
  "

  (assert (symbolp name))

  (when dimensions
    (when geometry-definition-form
      (error "Do not specify both dimensions and a geometry-definition-form.  The geometry-definition-form should ~
      incorporate the dimensions parameter."
	     )))

  `(progn
     (eval-when (:load-toplevel :execute)
       (save-non-textual-*defvars-for-re-evaluation ',name)
       (deallocate-vp-set-by-name ',name)
       (defparameter ,name nil)
       (define-vp-set ',name ',dimensions ',geometry-definition-form)
       (process-textual-*defvars ',*defvars ',name)
       (process-non-textual-*defvars ',name)
       ',name
       )
     (eval-when (:compile-toplevel) (defvar-and-proclaim-textual-*defvars ',*defvars))
     )

  )


(defun defvar-and-proclaim-textual-*defvars (*defvar-forms)
  (mapc
    #'(lambda (*defvar-parameters)
	(let ((pvar-name (first *defvar-parameters))
	      (type (fourth *defvar-parameters))
	      )
	  (when type (starlisp-proclaim `(type ,type ,pvar-name)))
	  (eval `(defvar ,pvar-name))
	  ))
    *defvar-forms
    ))
	

(defun create-vp-set (dimensions &key (geometry nil))

  "Creates an unnamed Vp Set.  Can only be used if *Lisp is runnable.
   If neither the dimensions nor the geometry is specified then
   the Vp Set can be further specified later using
   ALLOCATE-PROCESSORS-FOR-VP-SET.  All Vp Sets created
   using CREATE-VP-SET are deallocated at *COLD-BOOT time.
  "

  (when (and dimensions geometry)
    (error "Do not specify both dimensions and a geometry.  A geometry already includes dimension information")
    )

  (when (not (*lisp-runnable-p))
    (error "You cannot use CREATE-VP-SET because you are not currently~@
            attached to a Connection Machine or you have not executed *COLD-BOOT.~@
            Possibly someone may have forcibly detached you or you may have just run diagnostics.
           "
	   ))

  (setq geometry (if geometry geometry (if dimensions (create-geometry :dimensions dimensions) nil)))

  (let ((new-vp-set
	  (make-vp-set
	    :name nil
	    :geometry-allocation-form nil
	    :geometry geometry
	    :allocated t
	    :instantiated nil
	    :voidable (null geometry)
	    )))

    (when geometry (instantiate-vp-set new-vp-set))
    (push new-vp-set *all-dynamic-vp-sets*)

    new-vp-set

    ))


(defvar *non-textual-*defvars* nil)



(defun save-non-textual-*defvars-for-re-evaluation (vp-set-name)

  ;; Find all *defvars which belong to a Vp Set but which
  ;; were not defined inside the Vp Sets definition textually.

  (setq *non-textual-*defvars*
	(remove-if-not
	  #'(lambda (*defvar-specification)
	      (and (eq vp-set-name (*defvar-specification-vp-set-name *defvar-specification))
		   (not (*defvar-specification-in-vp-set-definition-p *defvar-specification))
		   ))
	  *all-*defvar-specifications*
	  )))


(defun deallocate-vp-set-by-name (vp-set-name)
  (cond
    ((and (boundp vp-set-name)
	  (vp-set-p (eval vp-set-name))
	  (not (eq (vp-set-name (eval vp-set-name)) vp-set-name))
	  )
     (cerror
       "Destroy the Vp Set bound to the symbol, even though the names are not the same"
       "There is a Vp Set named ~S, which is bound to the symbol ~S !!"
       (vp-set-name (eval vp-set-name)) vp-set-name
       )
     (report-and-remove-all-traces-of-vp-set-name vp-set-name)
     (return-from deallocate-vp-set-by-name vp-set-name)
     )
    ((and (boundp vp-set-name)
	  (find vp-set-name *all-def-vp-sets* :test #'eq :key 'vp-set-name)
	  (not (eq (symbol-value vp-set-name) (find vp-set-name *all-def-vp-sets* :test #'eq :key 'vp-set-name)))
	  )
     (cerror
       "Destroy the Vp Set found by that name anyway"
       "There is a Vp Set named ~S, but the symbol ~S is not bound to it !!"
       (report-and-remove-all-traces-of-vp-set-name vp-set-name)
       (return-from deallocate-vp-set-by-name vp-set-name)
       ))
    ((and (boundp vp-set-name)
	  (vp-set-p (eval vp-set-name))
	  )
     (deallocate-vp-set (eval vp-set-name))
     )
    (t (makunbound vp-set-name))
    ))


(defun report-and-remove-all-traces-of-vp-set-name (vp-set-name)

  (mapc
    #'(lambda (allocate!!-pvar)
	(when (eq (vp-set-name (pvar-vp-set allocate!!-pvar)) vp-set-name)
	  (warn "Inconsistency.  The Vp Set named ~S still exists, being pointed at~@
                 by the pvar ~S, but the symbol ~S is not bound to the Vp Set by that name"
		vp-set-name allocate!!-pvar vp-set-name
		)
	  (*deallocate allocate!!-pvar)
	  ))
    *all-allocate!!-pvars*
    )

  (mapc
    #'(lambda (*defvar-specification)
	(when (eq vp-set-name (*defvar-specification-vp-set-name *defvar-specification))
	  (warn "Inconsistency.  The Vp Set named ~S still exists, being point at~@
                 by the *defvar ~S, but the symbol ~S is not bound to the Vp Set by that name"
		vp-set-name (*defvar-specification-name *defvar-specification) vp-set-name
		)
	  (*deallocate-*defvars (*defvar-specification-name *defvar-specification))
	  ))
    *all-*defvar-specifications*
    )

  (mapc
    #'(lambda (vp-set)
	(when (eq (vp-set-name vp-set) vp-set-name)
	  (warn "Inconsistency. The Vp Set named ~S still exists, but the symbol ~S is not bound to the Vp Set by that name"
		vp-set-name vp-set-name
		)))
    *all-def-vp-sets*
    )
  (setq *all-def-vp-sets*
	(remove-if
	  #'(lambda (vp-set) (eq (vp-set-name vp-set) vp-set-name))
	  *all-def-vp-sets*
	  ))

  )


(defun define-vp-set (name dimensions-form geometry-definition-form)
  (set name (internal-define-vp-set name dimensions-form geometry-definition-form))
  (setq *all-def-vp-sets* (append *all-def-vp-sets* (list (symbol-value name))))
  )


(defun process-textual-*defvars (*defvars vp-set-name)
  (cond
    ((null *defvars) nil)
    ((not (listp *defvars)) (error "The :*defvars keyword value to DEF-VP-SET, ~S, is not a list" *defvars))
    (t
     (when (eq 'quote (car *defvars)) (setq *defvars (cadr *defvars)))
     (assert (every #'listp *defvars) ()
	     "The :*defvars argument, ~S, must be a list of lists, each list~@
              being of the form (pvar-name &optional initial-value documentation type).~@
              You probably only had one pvar declaration and forgot the enclosing parens.~@
              Correct example: (def-vp-set fred '(8192) :*defvars ((foo (!! 2))).
             "
	     *defvars
	     )
     (mapc
       #'(lambda (*defvar-parameters)
	   (let ((pvar-name (first *defvar-parameters))
		 (initial-value-form (second *defvar-parameters))
		 (documentation (third *defvar-parameters))
		 (type (fourth *defvar-parameters))
		 )
	     (when type (starlisp-proclaim `(type ,type ,pvar-name)))
	     (eval `(*defvar ,pvar-name ,initial-value-form ,documentation ,vp-set-name))
	     (assert-*defvar-textually-with-vp-set-definition pvar-name)
	     ))
       *defvars
       ))))


(defun assert-*defvar-textually-with-vp-set-definition (*defvar-name)
  (let ((*defvar-specification
	  (find *defvar-name *all-*defvar-specifications* :test #'eq :key #'*defvar-specification-name)
	  ))
    (when (null *defvar-specification)
      (error "Internal error.  Just defined the *defvar ~S, but it is not in the *all-*defvar-specifications* list"
	     *defvar-name
	     ))
    (setf (*defvar-specification-in-vp-set-definition-p *defvar-specification) t)
    ))
    

(defun process-non-textual-*defvars (vp-set-name)
  (mapc
    #'(lambda (*defvar-specification)
	(*defvar-1
	  (*defvar-specification-name *defvar-specification)
	  (*defvar-specification-initial-value-form *defvar-specification)
	  vp-set-name
	  ))
    *non-textual-*defvars*
    )
  (setq *non-textual-*defvars* nil)
  )


(defun internal-define-vp-set (name dimensions-form geometry-definition-form)

  (let ((geometry-allocation-form
	  (if geometry-definition-form
	      geometry-definition-form
	      (if dimensions-form
		  `(create-geometry :dimensions ,dimensions-form)
		  nil
		  ))))

    (let ((new-vp-set
	    (make-vp-set
	      :name name
	      :geometry-allocation-form geometry-allocation-form
	      :geometry nil
	      :allocated t
	      :instantiated nil
	      :voidable (null geometry-allocation-form)
	      )))

      (if (and (*lisp-runnable-p) geometry-allocation-form)
	  (instantiate-vp-set new-vp-set)
	  (when (null geometry-allocation-form) (setf (vp-set-voidable new-vp-set) t))
	  )

      new-vp-set

      )))



;;;; ALLOCATE-VP-SET-PROCESSORS
;;;; ALLOCATE-PROCESSORS-FOR-VP-SET
;;;; DEALLOCATE-VP-SET-PROCESSORS
;;;; DEALLOCATE-PROCESSORS-FOR-VP-SET
;;;; WITH-VP-SET-PROCESSORS-ALLOCATED


(defvar *allow-vp-sets-to-grow-dynamically* t)


(defun allocate-vp-set-processors 
    (vp-set dimensions &key geometry-definition send-pattern)
  (allocate-processors-for-vp-set 
   vp-set dimensions 
   :geometry geometry-definition 
   :send-pattern send-pattern)
  )


(defun allocate-processors-for-vp-set (vp-set dimensions &key geometry send-pattern)

  "Given a VP SET which was previously defined, and which
   was specified to have its dimensions and bit mask defined
   at run time (using the :run option), this function gives
   the VP SET a geometry and bitmask and initializes any
   *DEFVARS which have been defined for this VP SET.
  "

  (when dimensions
    (when geometry
      (error "Do not specify both dimensions and a geometry.  The geometry should~@
              incorporate the dimensions parameter."
	     )))

  (when (not (*lisp-runnable-p))
    (error "You cannot call allocate-processors-for-vp-set before you attach and *COLD-BOOT")
    )

  (when (not *allow-vp-sets-to-grow-dynamically*)
    (assert (null (vp-set-instantiated vp-set)) ()
	    "You are trying to allocate vp set processors for a VP SET which already has processors allocated"
	    ))

  (let ((geometry
	  (if geometry
	      geometry
	      (create-geometry :dimensions dimensions)
	      )))

    (if (null (vp-set-instantiated vp-set))

	(progn

	  (assert (vp-set-voidable vp-set) () "Internal error.  Not instantiated but not voidable")

	  ;; If the vp set has never been instantiated, then instantiate it.

	  (progn
	    (setf (vp-set-geometry vp-set) geometry)
	    (setf (vp-set-geometry-allocation-form vp-set) nil)
	    (instantiate-vp-set vp-set)
	    (allocate-*defvars-for-vp-set vp-set)
	    nil
	    )

	  )

	;; Otherwise, grow it

	(progn
	  (when send-pattern
	    (assert (pvarp send-pattern) () "The send-pattern argument, if provided, must be a pvar")
	    )
	  (allocate-more-processors-for-vp-set vp-set geometry send-pattern)
	  )

	)))


(defun deallocate-vp-set-processors (vp-set &key (ok-if-not-instantiated nil))
  (deallocate-processors-for-vp-set vp-set :ok-if-not-instantiated ok-if-not-instantiated)
  )


(defun deallocate-processors-for-vp-set (vp-set &key (ok-if-not-instantiated nil))
  (assert (vp-set-p vp-set) () "The Vp Set argument to deallocate-allocate!!-pvars-for-vp-set is not a Vp Set")
  (when (vp-set-deallocated vp-set)
    (cerror "Continue anyway" "The Vp Set ~S has already been deallocated")
    )
  (when (not ok-if-not-instantiated)
    (when (not (vp-set-instantiated vp-set))
      (cerror
	"Continue anyway"
	"The Vp Set ~S has never been instantiated.  You should not be trying to deallocate its processors."
	)))
  (when (not (vp-set-voidable vp-set))
    (error "You are trying to deallocate processors from a Vp Set which was defined~@
            to have a fixed size.  You can only deallocate processors from a Vp Set~@
            which was defined originally without specifying its size.
           "
	   ))
  (deallocate-pvars-for-vp-set vp-set :delete-*defvars nil :deallocate-*defvar-memory t :delete-allocate!!-pvars t)
  (setf (vp-set-instantiated vp-set) nil)
  (internal-deallocate-vp-set vp-set t)
  (setf (vp-set-geometry vp-set) nil)
  )


(defmacro with-processors-allocated-for-vp-set ((vp-set &key dimensions geometry send-pattern) &body body)
  (let ((vp-set-symbol (gensym "VP-SET-")))
    `(let ((,vp-set-symbol ,vp-set))
       (progn
	 (allocate-processors-for-vp-set ,vp-set-symbol ,dimensions :geometry ,geometry :send-pattern ,send-pattern)
	 (unwind-protect
	     (progn ,@body)
	   (deallocate-processors-for-vp-set ,vp-set-symbol)
	   )))))



;;;; DEALLOCATE-VP-SET
;;;; DEALLOCATE-VP-SETS


(defun deallocate-def-vp-sets (&rest vp-sets)
  (if (and (eql (length vp-sets) 1) (eq (car vp-sets) :all))
      (mapcar 'deallocate-vp-set *all-def-vp-sets*)
      (mapcar 'deallocate-vp-set vp-sets)
      )
  nil
  )


(defun deallocate-vp-set (vp-set &optional (deallocate-geometry-p t))

  "Removes all traces of a VP SET from *Lisp's knowledge.
   All *DEFVAR and ALLOCATE!! pvars belonging to the
   VP SET are deallocated and destroyed.
  "

  (assert (vp-set-p vp-set) () "The argument to deallocate-vp-set, ~S, must be a vp set, but it is not" vp-set)

  (when (eq *current-vp-set* vp-set)
    (error "You are trying to destroy the current vp set!!")
    )

  (when (eq vp-set *default-vp-set*)
    (error "You cannot destroy the default vp set!!")
    )

  (when (not (zerop (vp-set-nesting-level vp-set)))
    (error "You are trying to destroy a VP SET which is currently active.  This is not allowed.")
    )

  (deallocate-pvars-for-vp-set vp-set :delete-*defvars t :deallocate-*defvar-memory t :delete-allocate!!-pvars t)

  ;; flush T!!, NIL!! and the flags.

  (internal-deallocate-vp-set vp-set deallocate-geometry-p)

  ;; eradicate all trace of the VP SET structure.

  (setq *all-def-vp-sets* (delete vp-set *all-def-vp-sets* :test #'eq))
  (when (vp-set-name vp-set) (makunbound (vp-set-name vp-set)))
  (setq *all-dynamic-vp-sets* (delete vp-set *all-dynamic-vp-sets* :test #'eq))

  )

(defmacro let-vp-set ((vp-set-name vp-set-creation-form) &body body)
  `(let ((,vp-set-name ,vp-set-creation-form))
     (unwind-protect
	 (progn ,@body)
       (deallocate-vp-set ,vp-set-name)
       )))


(defun vp-set-deallocated-p (vp-set)
  (and (vp-set-p vp-set)
       (not (member vp-set *all-def-vp-sets* :test #'eq))
       (not (member vp-set *all-dynamic-vp-sets* :test #'eq))
       (not (eq vp-set *default-vp-set*))
       ))


;;;; NON PORTABLE *LISP SIMULATOR SPECIFIC STUFF.



(defun instantiate-vp-set (vp-set)

  ;; Sets up a VP SET with everything it
  ;; needs, including a geometry, flags,
  ;; and T!! and NIL!! pvars.

  (assert (not (vp-set-instantiated vp-set)) ()
	  "You are trying to instantiate a VP SET which is already instantiated!"
	  )

  (assert (zerop (vp-set-nesting-level vp-set)) ()
	  "Nesting level not 0: You are apparently trying to *COLD-BOOT or define a VP SET not from top level."
	  )

  ;; Give a VP SET its geometry.

  (when (null (vp-set-geometry vp-set))
    (setf (vp-set-geometry vp-set) (eval (vp-set-geometry-allocation-form vp-set)))
    )

  (let ((vp-set-size (vp-set-total-size vp-set)))

    ;; Set up a pool of arrays for a vp set of this size
    ;; if one doesn't already exist.

    (setf (vp-set-pvar-array-pool vp-set) (find-pool-of-size-or-make-one vp-set-size))

    ;; Set up a context stack for this vp set.

    (setf (vp-set-context-stack vp-set) (create-context-stack vp-set-size))
    (initialize-context-stack (vp-set-context-stack vp-set))
    (setf (vp-set-context-level vp-set) 0)

    (setup-vp-set-heap-pvar-list vp-set)

    ;; Allocate T!! and NIL!! for it.

    (define-vp-set-basic-pvars vp-set)

    ;; Create cube address and grid address arrays

    (create-cube-address-array-for-vp-set vp-set)
    (create-grid-address-array-for-vp-set vp-set)
    (verify-consistent-mapping-for-vp-set vp-set)

    (setf (vp-set-constants-hash-table vp-set) nil)

    (setf (vp-set-instantiated vp-set) t)

    ))



(defun set-vp-set (vp-set)
  (multiple-value-setq
	     (*current-vp-set*
	      *number-of-dimensions*
	      *number-of-processors-limit*
	      *log-number-of-processors-limit*
	      *current-send-address-length*
	      *current-cm-configuration*
	      *current-grid-address-lengths*
	      *current-context-stack*
	      *css-current-level*
	      *css*
	      *ppp-default-start*
	      *ppp-default-end*
	      t!!
	      nil!!
	      )
    (enter-vp-set-context vp-set)
    ))



(defun allocate-more-processors-for-vp-set (vp-set geometry send-pattern)
  (declare (ignore vp-set geometry send-pattern))
  (error "The *Lisp Simulator does not currently allow Vp Sets to be grown.  Sorry.")
  )


(defun describe-vp-set (vp-set &key (verbose nil) (*defvars t) (stream t))

  (declare (ignore verbose))

  (format stream
	  "~%VP SET NAME: ~A~%   DIMENSIONS: ~A~%  NESTING-LEVEL: ~A~%"
	  (vp-set-name vp-set)
	  (vp-set-dimensions vp-set)
	  (vp-set-nesting-level vp-set)
	  )
  (if *defvars
      (if (some
	    #'(lambda (*defvar-spec) (eq (*defvar-specification-vp-set-name *defvar-spec) (vp-set-name vp-set)))
	    *all-*defvar-specifications*
	    )
	  (progn
	    (format stream "  *DEFVARS BELONGING TO ~A~%" (vp-set-name vp-set))
	    (dolist (*defvar-spec *all-*defvar-specifications*)
	      (if (eq (*defvar-specification-vp-set-name *defvar-spec) (vp-set-name vp-set))
		  (format stream "    NAME: ~A, INITIAL-VALUE-FORM: ~S, TYPE: ~A~%"
			  (*defvar-specification-name *defvar-spec)
			  (*defvar-specification-initial-value-form *defvar-spec)
			  (if (*defvar-specification-proclaimed-type *defvar-spec)
			      (*defvar-specification-proclaimed-type *defvar-spec)
			      'general-pvar
			      )))))
	  (format stream "  NO *DEFVARS BELONG TO THIS VP SET~%")
	  ))

  )
  

(defun define-vp-set-basic-pvars (vp-set)

  ;; Defines T!! and NIL!! for a VP SET.
  ;; these pvars are dynamically bound to
  ;; the T!! and NIL!! symbols when the VP
  ;; set is selected.  Also set up a SELF-ADDRESS
  ;; pvar.

  (let ((size (vp-set-total-size vp-set)))
    (let ((t!!-pvar
	    (make-pvar
	      :name 't!!
	      :location (make-pvar-array-of-size size)
	      :class (general-pvar-class-label)
	      :canonical-pvar-type '(pvar t)
	      :lvalue? nil
	      :constant? :heap-constant
	      :vp-set vp-set
	      ))
	  (nil!!-pvar
	    (make-pvar
	      :name 'nil!!
	      :location (make-pvar-array-of-size size)
	      :class (general-pvar-class-label)
	      :canonical-pvar-type '(pvar t)
	      :lvalue? nil
	      :constant? :heap-constant
	      :vp-set vp-set
	      ))
	  (self-address!!-pvar
	    (make-pvar
	      :name 'self-address!!
	      :location (make-pvar-array-of-size size)
	      :class (general-pvar-class-label)
	      :canonical-pvar-type '(pvar t)
	      :lvalue? nil
	      :constant? :heap-constant
	      :vp-set vp-set
	      ))
	  )
      (let ((t!!-array (pvar-array t!!-pvar))
	    (nil!!-array (pvar-array nil!!-pvar))
	    (self-address!!-array (pvar-array self-address!!-pvar))
	    )
	(dotimes (j size)
	  (setf (aref t!!-array j) t)
	  (setf (aref nil!!-array j) nil)
	  (setf (aref self-address!!-array j) j)
	  ))
      (setf (vp-set-t!! vp-set) t!!-pvar)
      (setf (vp-set-nil!! vp-set) nil!!-pvar)
      (setf (vp-set-self-address!! vp-set) self-address!!-pvar)
      )))




(defun smash-vp-set-with-message (vp-set message)
  (setf (vp-set-context-stack vp-set) message)
  (setf (vp-set-context-level vp-set) message)
  (setf (vp-set-pvar-array-pool vp-set) message)
  (setf (vp-set-t!! vp-set) message)
  (setf (vp-set-nil!! vp-set) message)
  )



(defun setup-vp-set-heap-pvar-list (vp-set)
  (let ((arrays-list (vp-set-heap-pvar-arrays vp-set)))
    (if (not (every #'vectorp arrays-list))
	(error "internal error.  The vp-set-heap-pvar-arrays list contains non pvar arrays")
	(if (not (null arrays-list))
	    (if (not (eql (vp-set-size vp-set) (length (car arrays-list))))
		(setf (vp-set-heap-pvar-arrays vp-set) nil)
		(assert (every #'(lambda (x) (eql (length x) (vp-set-size vp-set))) arrays-list) ()
			"Internal error: all the arrays on the heap-pvar-arrays list are not the same length!"
			))))))


(defun internal-deallocate-vp-set (vp-set deallocate-geometry-p)
  (when (and deallocate-geometry-p (vp-set-geometry vp-set))
    (deallocate-geometry (vp-set-geometry vp-set))
    )
  (setf (vp-set-internal-id vp-set) nil)
  (setf (vp-set-instantiated vp-set) nil)
  )


(defun create-cube-address-array-for-vp-set (vp-set)
  (let* ((dimensions (vp-set-dimensions vp-set))
	 (number-of-dimensions (length dimensions))
	 (total-size (vp-set-size vp-set))
	 )
    (if (< number-of-dimensions array-rank-limit)
	(let* ((cube-address-array (make-array dimensions :element-type 'fixnum))
	       (displaced-array
		 (if (= 1 number-of-dimensions)
		     cube-address-array
		     (make-array total-size :displaced-to cube-address-array :element-type 'fixnum)
		     ))
	       )
	  (dotimes (j total-size)
	    (setf (aref displaced-array j) j)
	    )
	  (setf (vp-set-array-of-cube-addresses vp-set) cube-address-array)
	  )
	(error "The Simulator cannot yet deal with vp sets of more than ~D dimensions" array-rank-limit)
	)))

(defun create-grid-address-array-for-vp-set (vp-set)
  (let* ((dimensions (vp-set-dimensions vp-set))
	 (number-of-dimensions (length dimensions))
	 (total-size (vp-set-size vp-set))
	 (cube-address-array (vp-set-array-of-cube-addresses vp-set))
	 )
    (let* ((grid-address-array
	     (make-array (list total-size number-of-dimensions) :element-type 'fixnum :initial-element -1)
	     )
	   (grid-coordinate-list nil)
	   )
      (with-grid-indices-iterated
	(
	 grid-coordinate-list
	 number-of-dimensions
	 :start-index-list (make-list number-of-dimensions :initial-element 0)
	 :end-index-list dimensions
	 :check-arguments nil
	 :bind-as :list
	 )
	(let ((cube-address (internal-cube-address-from-grid-address-list cube-address-array grid-coordinate-list)))
	  (dotimes (dimension-index number-of-dimensions)
	    (assert (= -1 (aref grid-address-array cube-address dimension-index)) () "Not working right!")
	    (setf (aref grid-address-array cube-address dimension-index) (nth dimension-index grid-coordinate-list))
	    )))
      (setf (vp-set-array-of-grid-addresses vp-set) grid-address-array)
      )))



(defun internal-cube-address-from-grid-addresses (cube-address-array &rest grid-addresses)
  (internal-cube-address-from-grid-address-list cube-address-array grid-addresses)
  )


(defun internal-cube-address-from-grid-address-list (cube-address-array grid-address-list)
  (let ((length (length grid-address-list)))
    (cond
      ((eql 1 length) (car grid-address-list))
      ((eql 2 length)
       (aref #-KCL (the (array fixnum 2) cube-address-array) #+KCL cube-address-array
	     (first grid-address-list) (second grid-address-list)
	     ))
      ((eql 3 length)
       (aref #-KCL (the (array fixnum 3) cube-address-array) #+KCL cube-address-array
	     (first grid-address-list) (second grid-address-list) (third grid-address-list)
	     ))
      (t (apply #'aref cube-address-array grid-address-list))
      )))

(defun internal-cube-address-from-grid-address-vector (cube-address-array grid-address-vector)
  (let ((length (length grid-address-vector)))
    (cond
      ((eql 1 length) (svref grid-address-vector 0))
      ((eql 2 length)
       (aref #-KCL (the (array fixnum 2) cube-address-array) #+KCL cube-address-array
	     (svref grid-address-vector 0) (svref grid-address-vector 1)
	     ))
      ((eql 3 length)
       (aref #-KCL (the (array fixnum 3) cube-address-array) #+KCL cube-address-array
	     (svref grid-address-vector 0) (svref grid-address-vector 1) (svref grid-address-vector 2)
	     ))
      ((eql 4 length)
       (aref #-KCL (the (array fixnum 4) cube-address-array) #+KCL cube-address-array
	     (svref grid-address-vector 0) (svref grid-address-vector 1)
	     (svref grid-address-vector 2) (svref grid-address-vector 3)
	     ))
      (t (apply #'aref cube-address-array (concatenate 'list grid-address-vector)))
      )))
