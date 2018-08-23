;;; -*- Mode: LISP; Syntax: Common-lisp; Package: (*SIM-I COMMON-LISP-GLOBAL); Base: 10 -*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


;;;; *****     WARNING WARNING WARNING WARNING WARNING WARNING      *****
;;;;
;;;; This code is shared between the Starlisp Interpreter and the
;;;; Starlisp Simulator.  DO NOT MAKE CHANGES IN THIS CODE UNLESS
;;;; YOU ARE ABSOLUTELY SURE THE CHANGES APPLY EQUALLY TO BOTH
;;;; SYSTEMS OR YOU ARE VERY CAREFUL TO CONDITIONALLY COMPILE!
;;;; VIOLATE THIS WARNING AT YOUR OWN RISK!
;;;;
;;;; *****     WARNING WARNING WARNING WARNING WARNING WARNING      *****


;;;;
;;;;          *****  DATA STRUCTURES FOR *DEFSTRUCT  *****
;;;;

;;;;  See defstruct-interface.lisp as well.


;;; Stores information about each slot in a *DEFRECORD.
;;; The included structure structure-slot-with-accessors
;;; can be found in structure.lisp.

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defstruct (*defstruct-slot (:print-function *defstruct-slot-printer))
    (name nil)
    (constant-size? nil)
    (length-in-bits nil)
    (offset nil)
    (canonical-pvar-type nil)
    (type nil)
    (cm-initial-value-form nil)
    (initial-value-form nil)
    (cm-uninitialized? nil)
    (read-only? nil)
    (keyword nil)
    ))


(defun *defstruct-slot-printer (dss stream prindepth)
  (declare (ignore prindepth))
  (if *print-verbose*
      (format stream
	      "~%#<*DEFSTRUCT-SLOT ~S,~%  Constant-size?: ~S, Length-in-bits: ~S, Canonical-pvar-type: ~S~%~
             type: ~S, CM-initial-value-form: ~S, initial-value-form: ~S~%~
             CM-uninitialized?: ~S, Read-only?: ~S, Offset: ~S~% >~%"
	      (*defstruct-slot-name dss)
	      (*defstruct-slot-constant-size? dss)
	      (*defstruct-slot-length-in-bits dss)
	      (*defstruct-slot-canonical-pvar-type dss)
	      (*defstruct-slot-type dss)
	      (*defstruct-slot-cm-initial-value-form dss)
	      (*defstruct-slot-initial-value-form dss)
	      (*defstruct-slot-cm-uninitialized? dss)
	      (*defstruct-slot-read-only? dss)
	      (*defstruct-slot-offset dss))
      (format stream "#<*defstruct-slot ~S>" (*defstruct-slot-name dss))))


;;; Given a name of a *DEFRECORD, returns the structure that
;;; holds the information about that *DEFRECORD.

(defun get-*defstruct-struct (name &key (if-not-found nil))
  (check-type if-not-found (member :error nil))
  (let ((*defstruct (get-*defstruct name)))
    (when (and (not *defstruct) (eq :error if-not-found))
      (error "The *DEFSTRUCT named ~S was not found." name)
      )
    *defstruct
    ))

(defun initialization-form-function-name (name) (modify-name name "INITIALIZATION-FORM-FUNCTION-FOR-*DEFSTRUCT-" ""))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun default-option-value-checker (ds value)
  "Most *DEFSTRUCT options must be symbols.  This implements that default"
  ds
  (symbolp value)
  )

(defun included-*defstruct-name-checker (ds name)

  ;; The :INCLUDE option checker is slightly more complicated,
  ;; since the included *DEFSTRUCT must exist.

  (let ((included-ds (get-*defstruct name)))

    (when (null included-ds)
      (error "The *DEFSTRUCT ~S, which you want to be included into *DEFSTRUCT ~S, does not exist." name (*defstruct-name ds))
      )

    (setf (*defstruct-included-slots-list ds) (mapcar #'copy-*defstruct-slot (*defstruct-all-slots-list included-ds)))
    (setf (*defstruct-all-slots-list ds) (*defstruct-included-slots-list ds))

    t

    ))

(defun slot-pvar-type-checker (dss type)

  ;; If the type provided is not a valid Starlisp pvar type,
  ;; and if prepending (pvar ... to the type provided
  ;; does not result in a valid Starlisp pvar type, then
  ;; the type provided is erroneous.

  (let* ((canonical-type
	   (if (canonical-pvar-type type)
	       (canonical-pvar-type type)
	       (error "For slot ~S, the type ~S is not recognizable as a valid pvar type."
		      (*defstruct-slot-name dss) type
		      )))
	 (slot-length (length-pvar-type canonical-type))
	 )
    (setf (*defstruct-slot-length-in-bits dss) slot-length)
    (setf (*defstruct-slot-constant-size? dss) (if slot-length t nil))
    canonical-type
    ))

)


(eval-when (:compile-toplevel :load-toplevel :execute) 

  (defvar **defstruct-option-info* nil)

  (defvar **defstruct-slot-option-info* nil)

  )


(eval-when (:compile-toplevel :load-toplevel :execute)

  (setq **defstruct-option-info*

	(list

	  (list
	    :conc-name
	    #'default-option-value-checker
	    #'(lambda (ds) (modify-name (*defstruct-name ds) "" "-"))
	    #'*defstruct-slot-accessor-prefix-name
	    #'(lambda (ds value) (setf (*defstruct-slot-accessor-prefix-name ds) value))
	    t
	    )
	  (list
	    :cm-constructor
	    #'default-option-value-checker
	    #'(lambda (ds) (modify-name (*defstruct-name ds) "MAKE-" "!!"))
	    #'*defstruct-cm-constructor-name
	    #'(lambda (ds value) (setf (*defstruct-cm-constructor-name ds) value))
	    t
	    )
	  (list
	    :constructor
	    #'default-option-value-checker
	    #'(lambda (ds) (modify-name (*defstruct-name ds) "MAKE-" ""))
	    #'*defstruct-constructor-name
	    #'(lambda (ds value) (setf (*defstruct-constructor-name ds) value))
	    t
	    )
	  (list
	    :cm-copier
	    #'default-option-value-checker
	    #'(lambda (ds) (modify-name (*defstruct-name ds) "COPY-" "!!"))
	    #'*defstruct-cm-copier-name
	    #'(lambda (ds value) (setf (*defstruct-cm-copier-name ds) value))
	    t
	    )
	  (list
	    :copier
	    #'default-option-value-checker
	    #'(lambda (ds) (modify-name (*defstruct-name ds) "COPY-" ""))
	    #'*defstruct-copier-name
	    #'(lambda (ds value) (setf (*defstruct-copier-name ds) value))
	    t
	    )
 	  (list
	    :global-cm-predicate
	    #'default-option-value-checker
	    #'(lambda (ds) (modify-name (*defstruct-name ds) "*" "-P"))
	    #'*defstruct-global-cm-predicate-name
	    #'(lambda (ds value) (setf (*defstruct-global-cm-predicate-name ds) value))
	    t
	    )
	  (list
	    :parallel-cm-predicate
	    #'default-option-value-checker
	    #'(lambda (ds) (modify-name (*defstruct-name ds) "" "-P!!"))
	    #'*defstruct-parallel-cm-predicate-name
	    #'(lambda (ds value) (setf (*defstruct-parallel-cm-predicate-name ds) value))
	    t
	    )
	  (list
	    :predicate
	    #'default-option-value-checker
	    #'(lambda (ds) (modify-name (*defstruct-name ds) "" "-P"))
	    #'*defstruct-predicate-name
	    #'(lambda (ds value) (setf (*defstruct-predicate-name ds) value))
	    t
	    )
	  (list
	    :include
	    #'included-*defstruct-name-checker
	    #'(lambda (ds) (declare (ignore ds)) nil)
	    #'*defstruct-included-*defstruct-name
	    #'(lambda (ds value) (setf (*defstruct-included-*defstruct-name ds) value))
	    t
	    )
	  (list
	    :print-function
	    #'default-option-value-checker
	    #'(lambda (ds) (declare (ignore ds)) nil)
	    #'*defstruct-print-function-name
	    #'(lambda (ds value) (setf (*defstruct-print-function-name ds) value))
	    t
	    )
	  (list
	    :type
	    #'(lambda (ds value) (declare (ignore ds value)) t)
	    #'(lambda (ds) (declare (ignore ds)) nil)
	    #'*defstruct-type
	    #'(lambda (ds value) (setf (*defstruct-type ds) value))
	    t
	    )
	  (list
	    :cm-uninitialized-p
	    #'(lambda (ds value) (declare (ignore ds value)) t)
	    #'(lambda (ds) (declare (ignore ds)) nil)
	    #'*defstruct-cm-uninitialized-p
	    #'(lambda (ds value) (setf (*defstruct-cm-uninitialized-p ds) value))
	    t
	    )
	  ))

  (defmacro *defstruct-option-name (x) `(first ,x))
  (defmacro *defstruct-option-valid-test-function (x) `(second ,x))
  (defmacro *defstruct-option-default-values-function (x) `(third ,x))
  (defmacro *defstruct-option-accessor-function (x) `(fourth ,x))
  (defmacro *defstruct-option-setf-function (x) `(fifth ,x))
  (defmacro *defstruct-option-single-value? (x) `(sixth ,x))

  (setq **defstruct-slot-option-info*

	(list

	  (list
	    :type
	    #'(lambda (dss x) (declare (ignore dss)) x)
	    #'(lambda (x) (declare (ignore x)) t)
	    #'*defstruct-slot-type
	    #'(lambda (dss value) (setf (*defstruct-slot-type dss) value))
	    )
	  (list
	    :cm-type
	    #'slot-pvar-type-checker
	    #'(lambda (dss) (canonical-pvar-type (list 'pvar (*defstruct-slot-type dss))))
	    #'*defstruct-slot-canonical-pvar-type
	    #'(lambda (dss value) (setf (*defstruct-slot-canonical-pvar-type dss) value))
	    )
	  (list
	    :cm-uninitialized-p
	    #'(lambda (dss x) (declare (ignore dss)) x)
	    #'(lambda (dss) (declare (ignore dss)) nil)
	    #'*defstruct-slot-cm-uninitialized?
	    #'(lambda (dss value) (setf (*defstruct-slot-cm-uninitialized? dss) value))
	    )
	  (list
	    :cm-initial-value
	    #'(lambda (dss value) (declare (ignore dss)) value)
	    #'(lambda (dss) (list (if (equal (*defstruct-slot-canonical-pvar-type dss)
					     '(pvar front-end))
				      'front-end!! '!!)
				  (*defstruct-slot-initial-value-form dss)))
	    #'*defstruct-slot-cm-initial-value-form
	    #'(lambda (dss value) (setf (*defstruct-slot-cm-initial-value-form dss) value))
	    )
	  (list
	    :read-only
	    #'(lambda (dss value) (declare (ignore dss)) value)
	    #'(lambda (dss) (declare (ignore dss)) nil)
	    #'*defstruct-slot-read-only?
	    #'(lambda (dss value) (setf (*defstruct-slot-read-only? dss) value))
	    )
	  ))

  (defmacro *defstruct-slot-option-name (x) `(first ,x))
  (defmacro *defstruct-slot-option-valid-test-function (x) `(second ,x))
  (defmacro *defstruct-slot-option-default-values-function (x) `(third ,x))
  (defmacro *defstruct-slot-option-accessor-function (x) `(fourth ,x))
  (defmacro *defstruct-slot-option-setf-function (x) `(fifth ,x))

  )


(defvar *all-*defstruct-properties*
	'(
	  *defstruct-structure *defstruct-pref-function *defstruct-!!-function
	  *defstruct-setf-pref-function *defstruct-make-function
	  )
  "All the property names that are given values on the property list of the
   symbol which is the name of a *DEFSTRUCT
  "
  )

(defvar *all-*defstruct-slot-properties*
	'(*defstruct-accessor *defstruct-offset)
  "All the property names that are given values on the property list of
   each symbol which is the name of an accessor function for a *DEFSTRUCT
  "
  )


;;;;
;;;;      *****     TOP LEVEL CODE FOR *DEFSTRUCT     *****
;;;;


(defmacro *defstruct (name-and-options &body slot-list)
  
  (let ((*defstruct-structure (make-*defstruct)))
    ;; Parse the *defstruct.
    (do-load-time-*defstruct-processing *defstruct-structure name-and-options slot-list)
    ;; Make sure old properties from a previous definition go away.
    ;; Store away the new *defstruct structure where we can get at it later.
    (set-*defstruct (*defstruct-name *defstruct-structure) *defstruct-structure)
    ;; Generate code for the *defstruct definition.
    `(progn
       (eval-when (:load-toplevel :execute) (delete-*defstruct-properties ',(*defstruct-name *defstruct-structure)))
       ,(funcall 'generate-front-end-defstruct-code *defstruct-structure)
       (eval-when (:load-toplevel :execute)
         (let ((*defstruct-structure (make-*defstruct)))
           (do-load-time-*defstruct-processing *defstruct-structure ',name-and-options ',slot-list)
           (set-*defstruct (*defstruct-name *defstruct-structure) *defstruct-structure)
           ))
       ,@(generate-*defstruct-code *defstruct-structure)
       ',(*defstruct-name *defstruct-structure)
       )))

(defun do-load-time-*defstruct-processing (ds name-and-options slot-list)
  (setf (*defstruct-original-form ds) `(*defstruct ,name-and-options ,@slot-list))
  (parse-name-and-options ds name-and-options)
  (when (stringp (car slot-list))
    ;; Documentation
    (setf (*defstruct-documentation ds) (car slot-list))
    (setq slot-list (cdr slot-list))
    )
  (parse-slot-list ds slot-list)
  (compute-size-of-*defstruct ds)
  )


(defun un-*defstruct (name)

  "Destroys the information associated with a *DEFSTRUCT
   and makes most of the functions created by the *DEFSTRUCT
   (but not those functions created by the corresponding
   DEFSTRUCT!) go away.
  "

  (let ((ds (get-*defstruct name)))
    (if ds
	(progn
	  (dolist (slot (*defstruct-all-slots-list ds))
	    (delete-*defstruct-slot-properties slot)
	    (fmakunbound (*defstruct-slot-name slot))
	    )
	  (fmakunbound (structure-pvar-type-!!-function name))
	  (fmakunbound (structure-pvar-type-pref-function name))
	  (fmakunbound (structure-pvar-type-setf-pref-function name))
	  (fmakunbound (structure-pvar-type-make-function name))
	  (fmakunbound (structure-pvar-type-parallel-predicate-function name))
	  (fmakunbound (structure-pvar-type-copier-name name))
	  (delete-*defstruct-properties name)
	  t
	  )
	nil
	)))


;;;;
;;;;           *****  PARSING CODE FOR *DEFSTRUCT OPTIONS  *****
;;;;



(defun parse-name-and-options (ds name-and-options)

  (when (not (listp name-and-options)) (setq name-and-options (list name-and-options)))

  (let ((name (car name-and-options))
	(options-list (cdr name-and-options))
	)

    (assert (symbolp name) (name) "The name of a *DEFSTRUCT must be a symbol")
    (setf (*defstruct-name ds) name)

    (assert (listp options-list) () "The name and options for the *DEFSTRUCT ~S form a dotted pair" (*defstruct-name ds))

    ;; Set up a structure to note whether each possible option
    ;; has actually been specified.  Initially every option
    ;; is tagged with NIL (not specified).

    (let ((options-present-vector (apply #'vector (mapcar #'(lambda (x) (list (car x) nil)) **defstruct-option-info*))))

      ;; parse each provided option, setting the tag
      ;; for the option to T after parsing.

      (mapc #'(lambda (x) (parse-*defstruct-option ds options-present-vector x)) options-list)

      ;; for each option that was not provided
      ;; put the default value in the appropriate *defstruct structure slot.

      (map
	nil
	#'(lambda (option-present option-info)
	    (when (not (second option-present)) (set-*defstruct-slot-using-option-info ds option-info))
	    )
	options-present-vector
	**defstruct-option-info*
	)

      ;; Here we should check for incompatible options, if there are any.

      )))



(defun set-*defstruct-slot-using-option-info (ds option-info &optional (value nil value-present?))

  ;; If a value was provided, use it, otherwise
  ;; get the default value and use it.

  (if value-present?
      (funcall (*defstruct-option-setf-function option-info) ds value)
      (funcall (*defstruct-option-setf-function option-info)
	       ds
	       (funcall (*defstruct-option-default-values-function option-info) ds)
	       )))


(defun parse-*defstruct-option (ds options-present-vector option)
  ;; Break the option-list into component parts,
  ;; and then process those parts.
  (multiple-value-bind (name values values-present? option-info)
      (parse-*defstruct-option-form ds option)
    (process-*defstruct-option name values values-present? option-info ds options-present-vector)
    ))


(defun parse-*defstruct-option-form (ds option)
  
  (assert (or (keywordp option) (and option (listp option) (keywordp (car option)) (listp (cdr option))))
	  ()
	  "~S, in the options list of the *DEFSTRUCT ~S, is not a valid option"
	  option (*defstruct-name ds)
	  )
  
  (when (keywordp option) (setq option (list option)))
    
  ;; Did the user actually provide an option value?

  (let* ((option-values-present? (and (listp option) (cdr option)))
	 (option-name (car option))
	 (option-values (cdr option))
	 (option-info (assoc option-name **defstruct-option-info*))
	 )
      
      (when (null option-info)
	(error "The option name ~S, in the options list of the *DEFSTRUCT ~S, is not a known option."
	       option-name (*defstruct-name ds)
	       ))
      
      (values option-name option-values option-values-present? option-info)

      ))



(defun process-*defstruct-option (option-name option-values values-present? option-info ds options-present-vector)
  
  ;; If this option has already been provided once, issue a warning.

  (if (second (find option-name options-present-vector :key #'car))
      (warn "More than one option ~S, in the options list of the *DEFSTRUCT ~S, is present.  The last value will be used"
	    option-name (*defstruct-name ds)
	    )
      (setf (second (find option-name options-present-vector :key #'car)) t)
      )
  
  ;; If the option only accepts a single argument
  ;; and more than one was provided, issue a warning.

  (when (*defstruct-option-single-value? option-info)
    (when (> (length option-values) 1)
      (warn "More than one value for option ~S in the options list of the *DEFSTRUCT ~S was provided.  The first will be used."
	    option-name (*defstruct-name ds)
	    ))
    (setq option-values (car option-values))
    )

  (cond
    
    ;; Don't do anything if the user specifically gave this
    ;; option a NIL argument.
    
    ((and (null option-values) values-present?) nil)
    
    ;; if no value was specified use the default.
    
    ((null option-values)
     (set-*defstruct-slot-using-option-info ds option-info)
     )
    
    ;; otherwise values were specified.  Make sure they are valid.
    
    (t
     (when (not (funcall (*defstruct-option-valid-test-function option-info) ds option-values))
       (error "The argument ~S to *DEFSTRUCT option ~S for *DEFSTRUCT ~S is not a valid argument."
	      option-values option-name (*defstruct-name ds)
	      ))
     (set-*defstruct-slot-using-option-info ds option-info option-values)
     )
    
    ))
  

;;;;
;;;;       *****  PARSING CODE FOR *DEFSTRUCT SLOTS AND THEIR OPTIONS  *****
;;;;


(defun parse-slot-list (ds slot-list)
  
  ;; Set up a list of used slot names to insure
  ;; no duplicates.

  (when (null slot-list)
    (error "There are no slots for *DEFSTRUCT ~S." (*defstruct-name ds))
    )

  (let ((slot-names nil) (slot-structures nil))
    
    (mapc
      
      #'(lambda (slot)
	  
	  (let ((dss (make-*defstruct-slot)))
	    
	    (assert (listp slot) ()
		    "The slot form ~S for *DEFSTRUCT ~S is not a list" slot (*defstruct-name ds)
		    )
	    (assert (> (length slot) 3) ()
		    "The slot form ~S for *DEFSTRUCT ~S does not have enough elements (at least 4) (did you forget :type ?)"
		    slot (*defstruct-name ds)
		    )
	    
	    (let ((name (car slot))
		  (initial-value-form (cadr slot))
		  (options (cddr slot))
		  )
	      
	      (when (and (listp initial-value-form) (eq '!! (car initial-value-form)))
		(error "The slot ~S for *DEFSTRUCT ~S has an initial value form ~S. ~@
                        The initial value form should return a front end value, but this form ~@
                        returns a pvar.  You probably just want to remove the '!!'."
		       name (*defstruct-name ds) initial-value-form
		       ))

	      (assert (symbolp name) ()
		      "The slot name ~S for *DEFSTRUCT ~S is not a symbol" name (*defstruct-name ds)
		      )
	      (assert (not (member name slot-names)) ()
		      "The slot name ~S is duplicated for *DEFSTRUCT ~S" name (*defstruct-name ds)
		      )

	      (when (eql (search "!!" (symbol-name name)) (- (length (symbol-name name)) 2))
		(warn "The slot name ~S for *DEFSTRUCT ~S ends in !!.  Since the CM accessor ~@
                       for this slot is generally made by appending !! to the slot name, are you sure ~@
                       you want a CM accessor ending in '!!!!' ?"
		      name (*defstruct-name ds)
		      ))

	      (pushnew name slot-names)
	      
	      (setf (*defstruct-slot-name dss) name)
	      (setf (*defstruct-slot-keyword dss) (intern (symbol-name name) 'keyword))
	      (setf (*defstruct-slot-initial-value-form dss) initial-value-form)

	      (parse-*defstruct-slot-options ds dss options name)

	      (push dss slot-structures)
	      
	      )))
      
      slot-list
      
      )
    
;;; Make sure user hasn't chosen slot names that conflict with boolean test name    
    (let* ((predicate-name (modify-name (*defstruct-name ds) "" "-P"))
	   (prefix-name (*defstruct-slot-accessor-prefix-name ds))
	   (real-slot-names (if prefix-name
				(map 'list #'(lambda (slot-name)
					       (modify-name slot-name prefix-name ""))
				     slot-names))))
      (if (and (eq predicate-name (*defstruct-predicate-name ds))
	       (member predicate-name real-slot-names))
	  (warn "I'll bet you really don't want a slot named 'P',~@
                 because this will conflict with the name of the predicate function, ~S"
		(*defstruct-predicate-name ds)
		))
      )

;;; This is the check that was here before... WRS 10/18/90
;    (let ((predicate-name (modify-name (*defstruct-name ds) "" "-P")))
;      (if (and (eq predicate-name (*defstruct-predicate-name ds)) (member (intern "P") slot-names))
;	  (warn "I'll bet you really don't want a slot named 'P',~@
;                 because this will conflict with the name of the predicate function, ~S"
;		(*defstruct-predicate-name ds)
;		)))

    (setf (*defstruct-defined-slots-list ds) (nreverse slot-structures))
    (setf (*defstruct-all-slots-list ds) (append (*defstruct-all-slots-list ds) (*defstruct-defined-slots-list ds)))
    
    ))


(defun parse-*defstruct-slot-options (ds dss options slot-name)
  
  (assert (evenp (length options)) ()
	  "For slot ~S in *DEFSTRUCT ~S, the options list ~D does not have an even number of entries"
	  slot-name (*defstruct-name ds) options
	  )
  
  ;; Set up a structure to note whether each possible option
  ;; has actually been specified.  Initially every option
  ;; is tagged with NIL (not specified).

  (let ((options-present-vector (apply #'vector (mapcar #'(lambda (x) (list (car x) nil)) **defstruct-slot-option-info*))))

    ;; Make a list of all the option-name option-value pairs and then parse each.

    (let ((option-value-pairs nil))
      (dotimes (j (/ (length options) 2))
	(push (list (first options) (second options)) option-value-pairs)
	(pop options)
	(pop options)
	)
      (mapc
	#'(lambda (pair)
	    (parse-*defstruct-slot-option-pair ds dss options-present-vector slot-name pair)
	    )
	option-value-pairs
	))

    ;; Make sure the :type option was provided.

    (when (not (cadr (find :type options-present-vector :key #'car)))
      (error "The :type option for the slot ~S in the *DEFSTRUCT ~S was not provided, but it is currently required."
	     (*defstruct-slot-name dss) (*defstruct-name ds)
	     ))

    ;; Make sure user knows what he is doing!

    (when (and (*defstruct-slot-cm-uninitialized? dss) (*defstruct-slot-cm-initial-value-form dss))
      (error "The slot named ~S for *DEFSTRUCT ~S has a CM-INITIAL-VALUE option and a CM-UNINITIALIZED-P option!"
	     (*defstruct-slot-name dss) (*defstruct-name ds)
	     ))

    (when (null (*defstruct-slot-canonical-pvar-type dss))
      (when (null (valid-pvar-type-p (list 'pvar (*defstruct-slot-type dss))))
	(error "The slot option value ~S for option :TYPE for slot ~S of *DEFSTRUCT ~S~@
                is not a Common Lisp type translatable to a PVAR type.~@
                (The :type option must  be a Common Lisp type, not a PVAR type.)"
	       (*defstruct-slot-type dss) (*defstruct-slot-name dss) (*defstruct-name ds)
	       )))

    ;; Provide defaults for other options.

    (map
      nil
      #'(lambda (slot-option-present slot-option-info)
	  (when (not (second slot-option-present)) (set-*defstruct-slot-slot-using-option-info dss slot-option-info))
	  )
      options-present-vector
      **defstruct-slot-option-info*
      )

    (let ((length (length-pvar-type (*defstruct-slot-canonical-pvar-type dss))))
      (setf (*defstruct-slot-length-in-bits dss) length)
      (setf (*defstruct-slot-constant-size? dss) (not (eq '* length)))
      )

;    (let ((length  (length-pvar-type (*defstruct-slot-canonical-pvar-type dss))))
;      (setf (*defstruct-slot-length-in-bits dss) (if (integerp length) length '*))
;      (setf (*defstruct-slot-constant-size? dss) (integerp length))
;      )

    ))


(defun set-*defstruct-slot-slot-using-option-info (dss option-info &optional (value nil value-present?))

  ;; If a value was provided, use it, otherwise
  ;; get the default value and use it.

  (if value-present?
      (funcall (*defstruct-slot-option-setf-function option-info) dss value)
      (funcall (*defstruct-slot-option-setf-function option-info)
	       dss
	       (funcall (*defstruct-slot-option-default-values-function option-info) dss)
	       )))


(defun parse-*defstruct-slot-option-pair (ds dss options-present-vector slot-name pair)

  (let* ((name (first pair))
	 (value (second pair))
	 (slot-info (assoc name **defstruct-slot-option-info*))
	 )

    ;; Make sure the it's a valid option.

    (when (null slot-info)
      (error "The slot option ~S for slot ~S in the *DEFSTRUCT ~S is not a known option."
	     name slot-name (*defstruct-name ds)
	     ))

    ;; Check for duplicate options.

    (if (cadr (find name options-present-vector :key #'car))
	(warn "More than one option ~S, in the options list for slot ~S of the *DEFSTRUCT ~S, is present.  The last will be used"
	      name  (*defstruct-slot-name dss) (*defstruct-name ds)
	      )
	(setf (cadr (find name options-present-vector :key #'car)) t)
	)

    ;; Check that the option value is legal.

    (setq value (funcall (*defstruct-slot-option-valid-test-function slot-info) dss value))

    ;; put the option value into the *defstruct-slot structure

    (set-*defstruct-slot-slot-using-option-info dss slot-info value)

    ))



(defun compute-size-of-*defstruct (ds)

  (setf (*defstruct-constant-size? ds)
	(reduce #'(lambda (x y) (and x y))
		(mapcar #'*defstruct-slot-constant-size? (*defstruct-all-slots-list ds))
		))
    
  (setf (*defstruct-total-length-in-bits ds)
	(if (not (*defstruct-constant-size? ds))
	    '*
	    (compute-slot-offsets-and-return-expression-for-total-length ds)
	    ))

  )

(defun compute-slot-offsets-and-return-expression-for-total-length (ds)
  (let ((expression 0))
    (mapcar
      #'(lambda (slot)
	  (let ((slot-length (*defstruct-slot-length-in-bits slot)))
	    (setf (*defstruct-slot-offset slot) (if (integerp expression) expression (copy-list expression)))
	    (cond
	      ((and (integerp expression) (integerp slot-length)) (incf expression slot-length))
	      ((integerp expression) (setq expression `(+ ,expression ,slot-length)))
	      ((integerp slot-length) (incf (second expression) slot-length))
	      (t (setq expression (nconc expression (list slot-length))))
	      )))
      (*defstruct-all-slots-list ds)
      )
    expression
    ))

