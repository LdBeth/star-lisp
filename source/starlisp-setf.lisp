;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: (*SIM-I COMMON-LISP-GLOBAL); Muser: yes -*-

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


(declaim (special *compiling* *compilep*))

(defparameter *array-pvar-accessor-functions* '(#+*LISP-HARDWARE row-major-aref!! aref!! bit!! sbit!!))

(defun array-accessor-function-p (symbol) (member symbol *array-pvar-accessor-functions*))
(defun structure-accessor-function-p (symbol) (*defstruct-slot-accessor-p symbol))


(defun really-is-a-special-form (symbol)
  (member symbol '(
                   block catch declare eval-when flet function go
                         if labels let let* macrolet multiple-value-call multiple-value-prog1 progn
                         progv quote return-from setq tagbody the throw unwind-protect
                         )
          :test #'eq
          ))


(defun really-macroexpandable-p (form &optional env)
  (and (listp form)
       (symbolp (car form))
       (not (really-is-a-special-form (car form)))
       (not (starlisp-form-not-to-macroexpand form))
       (or (macro-function (car form))
	   (multiple-value-bind (macroexpansion expanded?)
	       (macroexpand-1 form env)
	     (declare (ignore macroexpansion))
	     expanded?
	     ))))


(defun macroexpand-properly (form env)
  (if (really-macroexpandable-p form env)
      (macroexpand-properly (macroexpand-1 form env) env)
      form
      ))
      

(defun is-place (form &optional env)
  (cond
    ((symbolp form) t)
    ((and (listp form) (eq 'the (first form)))
     (if (eql 3 (length form))
	 (is-place (third form) env)
	 (error "Illegal THE form encountered: ~S." form)))
    ((and (listp form) (symbolp (first form)))
     (let ((symbol (first form)))
       (cond ((eq symbol 'taken-as!!)
	      (is-place (second form) env))
	     ((eq symbol 'alias!!)
	      (is-place (second form) env))
	     ((array-accessor-function-p symbol)
	      (and (is-place (second form) env) (compile-time-constant-pvars? (cddr form) env)))
	     ((structure-accessor-function-p symbol)
	      (is-place (second form) env))
	     ((really-macroexpandable-p form env)
	      (is-place (macroexpand-1 form env) env))
	     (t nil))))
    ((listp form) nil)
    (t nil)))



(defun symbolp-or-the (form) (or (symbolp form) (and (listp form) (eq 'the (car form)))))


(defun is-!!-form (form env)
  (and (listp form)
       (or (and (eq 'the (first form)) (eql 3 (length form)) (is-!!-form (third form) env))
	   (and (eq '!! (first form)) (>= 2 (length form)))
	   (and (really-macroexpandable-p form env) (is-!!-form (macroexpand-1 form env) env))
	   )))


(defun compile-time-constant-pvars? (pvar-form-list &optional env)

  ;; Returns T if every index form is a constant index
  ;; (i.e., is a list which begins with !!)

  (every #'(lambda (x) (is-!!-form x env)) pvar-form-list)

  )



(defun indirect-array-accessor-form-p (form env)

  ;; Returns T if an array accessor form
  ;; (like (aref!! x y) must be thought of
  ;; as being possibly indirect (which is
  ;; true unless the indices are all !!'s).

  (let ((indices (cddr form)))
    (not (compile-time-constant-pvars? indices env))
    ))


(defmacro *defsetf (accessor-function update-function)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (get ',accessor-function '*setf-function) ',update-function)
     ))

(defmacro *undefsetf (accessor-function)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (get ',accessor-function '*setf-function) nil)
     ))

(defun *defsetf-function (f) (get f '*setf-function))


(defmacro *setf
	  #-KCL
	  (&rest form-values-pairs &environment env)
	  #+KCL
	  (&environment env &rest form-values-pairs)


  ;; *SETF should always return no values.

  (when (not (evenp (length form-values-pairs)))
    (error "Odd number of arguments to *SETF.")
    )

  (cond

    ((zerop (length form-values-pairs)) '(values))

    ((> (length form-values-pairs) 2)
     `(progn
	(*setf ,(first form-values-pairs) ,(second form-values-pairs))
	(*setf ,@(cddr form-values-pairs))
	))

    (t

     (let ((form (first form-values-pairs))
	   (value (second form-values-pairs))
	   )

       (cond

	 ;; if the destination is a symbol use *SET

	 ((symbolp form)
	  (if (or (constantp form) (member form '(t!! nil!!)))
	      (error "You cannot perform a *SETF on the form ~S." form)
	      `(*set ,form ,value)
	      ))

	 ((listp form)

	  (cond

	    ;; putting things into the CM with PREF
	    ;; handle very specially (*SETF (PREF (AREF!! ...
	    ;; and (*SETF (PREF (FOO-A!! ...
	    ;; (structure accessors)

	    ((eq (first form) 'pref) (*setf-pref form value env))

	    ;; moving things around the CM with PREF!!
	    ;; This may eventually get much more complicated.

	    ((eq (first form) 'pref!!)
	     (let ((destination-pvar-form (second form)) (address-pvar-form (third form)))
	       `(*pset :no-collisions ,value ,destination-pvar-form ,address-pvar-form)
	       ))

	    ;; handle *SETF into arrays and structures.

	    ((or (array-accessor-function-p (car form)) (structure-accessor-function-p (car form)))

	     (multiple-value-bind (accessor-list is-aref-list is-indirect-list indices-list object-form)

		 (parse-accessor-form form env)

	       (if (notany #'identity is-indirect-list)

		   (*setf-subpart form value env)

		   (*setf-indirect accessor-list is-aref-list is-indirect-list indices-list object-form value)

		   )))

	    ((eq 'realpart!! (car form)) `(*setf-realpart!! ,(cadr form) ,value))
	    ((eq 'imagpart!! (car form)) `(*setf-imagpart!! ,(cadr form) ,value))

	    ((eq 'sideways-aref!! (car form))
	     `(setf-sideways-aref!! ,(cadr form) ,value ,@(cddr form)))
	    ((eq 'row-major-sideways-aref!! (car form))
	     `(setf-row-major-sideways-aref!! ,(cadr form) ,value ,@(cddr form)))
	    ((eq 'row-major-aref!! (car form))
	     `(setf-row-major-aref!! ,(cadr form) ,value ,@(cddr form)))

	    ((eq 'load-byte!! (car form))
	     (if (is-place (second form) env)
		 `(*setf ,(second form) (deposit-byte!! ,(second form) ,@(cddr form) ,value))
		 (let ((temp-symbol (gensym "*SETF-LOAD-BYTE-TEMP-")))
		   `(*let ()
		      (declare (return-pvar-p nil))
		      (let ((,temp-symbol (alias!! ,(second form))))
			(*set ,temp-symbol (deposit-byte!! ,temp-symbol ,@(cddr form) ,value))
			)))))

	    ((eq 'ldb!! (car form))
	     ;; form is (ldb!! byte place), value is new-byte
	     ;;(*setf (ldb!! byte place) new-byte)
	     ;;(*setf place (dpb!! new-byte byte place))
	     (if (symbolp (third form))
		 `(*set ,(third form) (dpb!! ,value ,(second form) ,(third form)))
		 (let ((temp-symbol (gensym "*SETF-LOAD-BYTE-TEMP-")))
		   `(*let ()
		      (declare (return-pvar-p nil))
		      (let ((,temp-symbol (alias!! ,(third form))))
			(*set ,temp-symbol (dpb!! ,value ,(cadr form) ,temp-symbol)))))))

	    ;; If there is a *DEFSETF for this form, invoke it.

	    ((*defsetf-function (car form))
	     `(let ((%TEMP-SETF% ,value))
		(pvar-argument!! %TEMP-SETF% legal-pvar-value)
		(,(*defsetf-function (car form)) ,@(cdr form) ,value))
	     )

	    ;; If the form is a macro, macroexpand it and try again.

	    ((really-macroexpandable-p form env)
	     `(*setf ,(macroexpand-1 form env) ,value)
	     )

	    ;; If we have a THE form, get inside it

;	    ((eq 'the (car form))
;	     (assert (eql 3 (length form)) () "Bad THE expression, ~S, given to *SETF" form)
;	     `(*setf (the ,(second form) ,(recursive-alias!! (third form) env)) ,value)
;	     )

	    ;; otherwise we assume what's going to come back
	    ;; is an lvalue, so use *SET and alias!!
	    ;; (alias!! macroexpands the form)

	    (t `(*set (alias!! ,form) ,value))

	    ))

	 (t (error "*SETF doesn't know how to store a value into ~S" form))

	 )))

    ))


(defmacro setf-pref
	  #-KCL
	  (&whole form pvar index value &environment environment)
	  #+KCL
	  (&whole form &environment environment pvar index value)
  (let ((new-form
	  (if (and *compiling* *compilep*)
	      (call-starlisp-compiler form environment)
	      form)))
    (if (eq new-form form)
	(if (or (not (symbolp (if (and (consp pvar) (eq (car pvar) 'the)) (caddr pvar) pvar)))
		(not (symbolp (if (and (consp value) (eq (car value) 'the)) (caddr value) value))))
	    `(*let () (declare (return-pvar-p nil)) (pset ,pvar ,index ,value))
	    `(pset ,pvar ,index ,value))
	new-form)))


(defun *setf-pref (form value env)

  ;; putting things into the CM with PREF
  ;; handle very specially (*SETF (PREF (AREF!! ...
  ;; and (*SETF (PREF (FOO-A!! ...
  ;; (structure accessors)

  (when (not (eql 3 (length form)))
    (when (and (not (eql 5 (length form)))
	       (not (eq :vp-set (fourth form)))
	       )
      (error "In *SETF, the form ~S is not a legal PREF form." form)
      ))

  (let* ((destination-pvar-form (second form))
	 (processor-index-form (third form))
	 (dest-is-place-p (is-place destination-pvar-form env))
	 body-form
	 )
	
    (if (symbolp destination-pvar-form)

	(setq body-form `(setf-pref ,destination-pvar-form ,processor-index-form ,value))

	(if dest-is-place-p

	    (setq body-form `(setf-pref ,(recursive-alias!! destination-pvar-form env) ,processor-index-form ,value))

	    (let ((processor-id-symbol (gensym "PROCESSOR-ID-")))
	  
	      (setq body-form
		    `(let ((,processor-id-symbol ,processor-index-form))
		       (*all
			 (declare (return-pvar-p nil))
			 (*when (=!! (self-address!!) (!! (the fixnum ,processor-id-symbol)))
			   (declare (return-pvar-p nil))
			   (setf-pref ,(recursive-alias!! destination-pvar-form env) ,processor-id-symbol ,value)
			   ))))
	  
	      )))

    (if (and (eql 5 (length form))
	     (not (eq '*current-vp-set* (fifth form)))
	     (not dest-is-place-p)
	     )
	`(*with-vp-set ,(fifth form) ,body-form)
	body-form
	)

    ))


(defmacro *incf
	  #-KCL
	  (pvar &optional (value '(!! 1)) &environment env)
	  #+KCL
	  (&environment env pvar &optional (value '(!! 1)))
  (if (is-place pvar env)
    `(*setf ,pvar (+!! ,pvar ,value))
    (error "The form ~S is not a recognizable place that *INCF knows how to increment." pvar)
    ))

(defmacro *decf 
	  #-KCL
	  (pvar &optional (value '(!! 1)) &environment env)
	  #+KCL
	  (&environment env pvar &optional (value '(!! 1)))
  (if (is-place pvar env)
    `(*setf ,pvar (-!! ,pvar ,value))
    (error "The form ~S is not a recognizable place that *DECF knows how to decrement." pvar)
    ))


(defun parse-accessor-form (form env)

  ;; Parse a nested set of array and structure
  ;; accessor forms.

  ;; Returns five values:
  ;;
  ;; The list of accessor functions used, outward first, innermost last
  ;; A list of T's and NIL's indicating whether the accessor is an array accessor
  ;; A list of T's and NIL's indicating whether the accessor,
  ;;   if it is an array, is possibly using indirect addressing.
  ;; A list of either indices lists (for array accessors) or
  ;;   NIL, for structure accessors.
  ;; The form which is being accessed by the innermost
  ;;   array or structure accessor.

  ;; For example, (aref!! (foo-a!! (aref!! (f x) y)) (!! 2) (!! 3))
  ;; would return

  ;; (values
  ;;   '(aref!! foo-a!! aref!!)
  ;;   '(t nil t)
  ;;   '(nil nil t)
  ;;   '(((!! 2) (!! 3)) nil (y))
  ;;   '(f x)
  ;;   )

  (let ((accessor-list nil)
	(is-aref-list nil)
	(is-indirect-list nil)
	(indices-list nil)
	object-form
	)

    (loop

;;      (format t "~% form=~a" form)
      (cond
	((array-accessor-function-p (car form))
	 (assert (> (length form) 1) () "The form ~A is not a valid array reference" form)
	 (push (car form) accessor-list)
	 (cond ((and (listp (cadr form))
		     (eq (car (cadr form)) 'the))
		(setq object-form (macroexpand (third (cadr form)) env)))
	       (t
		(setq object-form (macroexpand (cadr form) env))))
	 (push (cddr form) indices-list)
	 (push (indirect-array-accessor-form-p form env) is-indirect-list)
	 (push t is-aref-list)
	 )
	((structure-accessor-function-p (car form))
	 (assert (eql 2 (length form)) () "The form ~A is not a valid structure slot reference" form)
	 (push (car form) accessor-list)
	 (cond ((and (listp (cadr form))
		     (eq (car (cadr form)) 'the))
		(setq object-form (macroexpand (third (cadr form)) env)))
	       (t
		(setq object-form (macroexpand (cadr form) env))))
	 (push nil indices-list)
	 (push nil is-indirect-list)
	 (push nil is-aref-list)
	 ))

      (if (or (not (listp object-form))
	      (and (not (array-accessor-function-p (car object-form)))
		   (not (and (structure-accessor-function-p (car object-form))
			     ))))
	  
	  (return-from parse-accessor-form
	    (values (nreverse accessor-list)
		    (nreverse is-aref-list)
		    (nreverse is-indirect-list)
		    (nreverse indices-list)
		    object-form
		    ))
	  
	  (setq form object-form)	  
	  ))))


(defun recursive-alias!! (form env)
  (cond
    ((symbolp form) form)
    ((listp form)
     (let ((function (first form)))
       (cond
	 ((not (symbolp function)) form)
	 ((eq 'the function) `(the ,(second form) ,(recursive-alias!! (third form) env)))
	 ((really-is-a-special-form function) form)
	 ((or (structure-accessor-function-p function) (array-accessor-function-p function))
	  `(alias!! (,function ,(recursive-alias!! (second form) env) ,@(cddr form))))
	 ((really-macroexpandable-p form env) (recursive-alias!! (macroexpand-1 form env) env))
	 (t form)
	 )))
    (t form)
    ))


(defun *setf-subpart (place-form value-form env)
  
  ;; We know the form begins with AREF!!, BIT!!, or SBIT!!
  ;; or is a structure accessor function symbol.

  `(*set (alias!! (,(car place-form) ,(recursive-alias!! (cadr place-form) env) ,@(cddr place-form))) ,value-form)

  )


(defun *setf-indirect (accessor-list is-aref-list is-indirect-list indices-list object-form value)

  ;; if the innermost accessor reference is not
  ;; indirect, put back together that part of this
  ;; mess which is not indirect and try again on
  ;; just the indirect part.

  (if (not (eq t (car (last is-indirect-list))))

      (apply '*setf-indirect
	     (multiple-value-list
	       (reconstruct-non-indirect-*setf-parse
		 accessor-list is-aref-list is-indirect-list indices-list object-form value
		 )))

      (cond

	;; If this is just a simple indirection form
	;; (i.e., one indirect aset and nothing else)
	;; call the basic indirection function directly.

	((and (eql 1 (length accessor-list)) (member (car accessor-list) '(aref!! sbit!! bit!!)))
	 `(setf-aref!! (alias!! ,object-form) ,value ,@(car indices-list)))

	((eql 1 (length accessor-list))
	 (error "You should never get here")
	 )

	;; if this is just a long string of array references
	;; with no structure accessors call the function
	;; which deals with just aref!!'s.

	((every #'identity is-aref-list)
	 `(*let ()
	    (general-all-aref-indirect-aset!!
	      (alias!! ,object-form)
	      '(,@accessor-list)
	      (list ,@(mapcar #'(lambda (list) `(list ,@list)) indices-list))
	      ,value
	      )))

	;; if there is a single structure access preceding
	;; all the array accessors, call the function which
	;; deals with that.

	;; general-structure-first-indirect-aset!! doesn't seem to be in the simulator anywhere,
	;; but it's available in the interpreter directory in the file aref.lisp -- WRS 8/31/90

	((every #'identity (cdr is-aref-list))
	 `(*let ()
	    (general-structure-first-indirect-aset!!
	      (alias!! ,object-form)
	      '(,@accessor-list)
	      (list nil ,@(mapcar #'(lambda (list) `(list ,@list)) (cdr indices-list)))
	      ,value
	      )))

	 ;; Otherwise there must be an indirect reference inside
	 ;; a structure accessor inside something else.

	 (t
	  (let ((object-form-symbol (gensym "OBJECT-FORM-")))
	    (multiple-value-bind (inner-form inner-type)
		(reconstruct-form-inside-and-including-innermost-structure-access	
		  accessor-list is-aref-list is-indirect-list indices-list object-form-symbol value)
	      (let ((temp-symbol (gensym "ACCESSOR-TEMP-")))
		`(*let ()
		   (let ((,object-form-symbol ,object-form))
		     (*let ((,temp-symbol ,inner-form))
		       (declare (type ,inner-type ,temp-symbol))
		       nil
		       (*setf ,(reconstruct-form-up-to-innermost-structure-access
				 accessor-list is-aref-list is-indirect-list indices-list temp-symbol)
			      ,value
			      )
		       (*setf ,inner-form ,temp-symbol)
		       )))))))

	 )))


(defun reconstruct-form-inside-and-including-innermost-structure-access
       (accessor-list is-aref-list is-indirect-list indices-list object-form value)
  is-indirect-list
  value
  (let ((last-structure-access-position (position 'nil is-aref-list :test #'eq :from-end t)))
    (let ((accessor-list (nthcdr last-structure-access-position accessor-list))
	  (indices-list (nthcdr last-structure-access-position indices-list))
	  )
      (mapc
	#'(lambda (accessor indices)
	    (cond
	      ((array-accessor-function-p accessor)
	       (setq object-form `(,accessor ,object-form ,@indices))
	       )
	      ((structure-accessor-function-p accessor)
	       (setq object-form `(,accessor ,object-form))
	       )
	      (t (error "Internal error."))
	      ))
	(reverse accessor-list)
	(reverse indices-list)
	)
      (let ((outermost-structure-accessor (car object-form)))
	(values object-form (*defstruct-slot-pvar-type outermost-structure-accessor))
	))))


(defun reconstruct-form-up-to-innermost-structure-access
       (accessor-list is-aref-list is-indirect-list indices-list inner-object-form-symbol)
  is-indirect-list
  (assert (symbolp inner-object-form-symbol) () "internal error")
  (let* ((last-structure-access-position (position 'nil is-aref-list :test #'eq :from-end t))
	 (number-to-remove (- (length is-aref-list) last-structure-access-position))
	 (accessor-list (nbutlast accessor-list number-to-remove))
	 (indices-list (nbutlast indices-list number-to-remove))
	 (object-form inner-object-form-symbol)
	 )
    (mapc
	#'(lambda (accessor indices)
	    (cond
	      ((array-accessor-function-p accessor)
	       (setq object-form `(,accessor ,object-form ,@indices))
	       )
	      ((structure-accessor-function-p accessor)
	       (setq object-form `(,accessor ,object-form))
	       )
	      (t (error "Internal error."))
	      ))
	(reverse accessor-list)
	(reverse indices-list)
	)
    object-form
    ))


(defun reconstruct-non-indirect-*setf-parse (accessor-list is-aref-list is-indirect-list indices-list object-form value)

  (assert (some #'identity is-indirect-list) () "internal-error")

  (let ((accessor-list (reverse accessor-list))
	(is-aref-list (reverse is-aref-list))
	(is-indirect-list (reverse is-indirect-list))
	(indices-list (reverse indices-list))
	)

    (mapc
      #'(lambda (accessor is-aref is-indirect indices)
	  (if is-indirect
	      (return-from reconstruct-non-indirect-*setf-parse
		(values
		  (reverse accessor-list) (reverse is-aref-list)
		  (reverse is-indirect-list) (reverse indices-list)
		  object-form value
		  ))
	      (progn
		(cond
		  (is-aref (setq object-form `(alias!! (,accessor ,object-form ,@indices))))
		  (t (setq object-form `(alias!! (,accessor ,object-form))))
		  )
		(pop accessor-list)
		(pop is-aref-list)
		(pop is-indirect-list)
		(pop indices-list)
		)))
      accessor-list is-aref-list is-indirect-list indices-list
      )

    ))


(setf (get 'load-byte!! 'alias!!-function) 'aliased-load-byte!!)
(setf (get 'realpart!! 'alias!!-function) 'aliased-realpart!!)
(setf (get 'imagpart!! 'alias!!-function) 'aliased-imagpart!!)
(setf (get 'row-major-aref!! 'alias!!-function) 'aliased-row-major-aref!!)

(defparameter *simulator-cannot-handle-alias!!-accessors*
	      '(load-byte!! ldb!! realpart!! imagpart!!)
  "Accessors that the Starlisp Simulator cannot deal with"
  )

(defun simulator-cannot-handle (accessor)
  (error "Because of the nature of its implementation, the Starlisp Simulator~@
          cannot perform the alias!! operation on the ~S operator.  Sorry."
	 accessor
	 ))

(defmacro alias!! #-KCL (form &environment env) #+KCL (&environment env form)
  (cond ((symbolp form) form)
	((listp form)
	 (let ((accessor-function (car form)))
	   (when (simulator-loaded-p)
	     (when (member accessor-function *simulator-cannot-handle-alias!!-accessors* :test #'eq)
	       (simulator-cannot-handle accessor-function)))
	   (cond ((eq accessor-function 'alias!!) `(alias!! ,(cadr form)))
		 ((member accessor-function '(structure-alias!! aliased-aref!! aliased-row-major-aref!!
					      aliased-load-byte!! aliased-imagpart!! aliased-realpart!!))
		  form)
		 ((and (symbolp accessor-function) (*defstruct-slot-accessor-p accessor-function))
		  (if (simulator-loaded-p)
		      `(,(funcall '*defstruct-slot-alias!!-function accessor-function) ,@(cdr form))
		      `(structure-alias!! ',accessor-function ,@(cdr form))))
		 ((member accessor-function '(AREF!! BIT!! SBIT!! load-byte!!))
		  (when (not (>= (length form) 2))
		    (error "Incorrect number of arguments to ~S in ~S." accessor-function form))
		  (let ((indices (mapcar #'(lambda (form) (macroexpand-properly form env)) (cddr form))))
		    (when (not (every #'(lambda (x) (is-!!-form x env)) indices))
		      (error "The form to be ALIASED, ~S, must have constant pvars (i.e, !!'s) for its index arguments." form))
		    `(,(get accessor-function 'alias!!-function) ,@(cdr form))))
		 ((eq accessor-function 'ldb!!)
		  (let ((byte (cadr form)) (place (caddr form)))
		    (unless (and (consp byte) (eq (car byte) 'byte!!) (is-!!-form (cadr byte) env) (is-!!-form (caddr byte) env))
		      (error "The ldb!! form ~S to alias!! must have constant size and position arguments." form))
		    `(aliased-load-byte!! ,place ,(caddr byte) ,(cadr byte))))
		 ((get accessor-function 'alias!!-function)
		  `(,(get accessor-function 'alias!!-function) ,@(cdr form)))
		 ((and (symbolp accessor-function) (eq 'the accessor-function) (eql 3 (length form)))
		  `(the ,(second form) (alias!! ,(third form))))
		 ((really-macroexpandable-p form env) `(alias!! ,(macroexpand-1 form env)))
		 (t
		  (warn "Do not know what it means to alias the form ~S.  The form itself is being returned." form)
		  form))))
	(t (error "Unknown form ~S to ALIAS!!." form))))

(defun simulator-loaded-p ()
  (member (proper-symbol-for-*features* *starlisp-simulator-features-symbol*) *features*))

(defun hardware-loaded-p ()
  (member (proper-symbol-for-*features* *starlisp-hardware-features-symbol*) *features*))


