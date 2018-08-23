;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: *SIM-I -*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


;;;
;;; This file contains most of the macros necessary for the *Lisp simulator
;;; Other macros which are implementation dependent can be found in the
;;; file port.lisp

;;;; ****************************************************************************
;;;;
;;;;                           CODE FOR *DEFUN
;;;;
;;;; ****************************************************************************


(defun split-declarations-and-body (forms env)

  "Returns two values, a list of declarations and a list of forms"

  (cond

    ;; If there aren't any more forms, we are done.

    ((null forms) (values nil nil))

    ;; If the form is not a non-nil list, then there
    ;; are no more declarations.

    ((null (consp (car forms))) (values nil forms))

    (t
     (let ((form (car forms)))

       (cond

	 ;; If the form is not a DECLARE, and it cannot possibly
	 ;; expand into a DECLARE, then there are no more declarations.

	 ;; Be careful about special forms because special forms
	 ;; can have macro expansions, but they cannot expand
	 ;; into declares.  

	 ((and (not (eq (car form) 'declare))
	       (or (listp (car form))
		   (really-is-a-special-form (car form))
		   (null (macro-function (car form)))
		   ))
	  (values nil forms)
	  )

	 ;; If the form is a declare, grab it and recurse on
	 ;; the rest of the forms.

	 ((eq (car form) 'declare)
	  (multiple-value-bind (other-declares other-forms)
	      (split-declarations-and-body (cdr forms) env)
	    (values (cons form other-declares) other-forms)
	    ))

	 ;; If the form is a macro, macroexpand it once and recurse.
	 ;; We macroexpand only once because things might macroexpand
	 ;; into special forms or certain disgusting Common Lisps
	 ;; may macroexpand DECLARE statements into oblivion!

	 ((macro-function (car form))
	  (let ((macroexpanded-form (macroexpand-1 form env)))
	    (multiple-value-bind (declares other-forms)
		(split-declarations-and-body (cons macroexpanded-form (cdr forms)) env)
	      (values declares other-forms)
	      )))

	 (t (error "Internal error.  You can't get here from Common Lisp"))

	 )))))



(defun split-documentation-and-body (body)
  (if (eql 1 (length body))
      (values nil body)
      (if (stringp (car body))
	  (multiple-value-bind (doc-string-list real-body)
	      (split-documentation-and-body (cdr body))
	    (values (cons (car body) doc-string-list) real-body)
	    )
	  (values nil body)
       )))


(defun parse-body-form-documentation-and-declarations (body env)
  (multiple-value-bind (documentation body)
      (split-documentation-and-body body)
    (multiple-value-bind (declarations body)
	(split-declarations-and-body body env)
      (values documentation declarations body)
      )))


(defun declaration-into-list-of-single-declarations (declaration)
  (mapcar
    #'(lambda (decl-spec) `(declare ,decl-spec))
    (cdr declaration)
    ))


(defun declarations-into-list-of-single-declarations (declarations)
  (mapcan 'declaration-into-list-of-single-declarations declarations)
  )


(defun *lisp-declaration-p (single-declaration)
  (if (return-pvar-p-declaration-p single-declaration)
      t
      (let ((decl-spec (cadr single-declaration)))
	(if (eq 'type (first decl-spec))
	    (if (canonical-pvar-type (second decl-spec)) t nil)
	    nil
	    ))))


(defun return-pvar-p-value-from-list-of-*lisp-declarations (list-of-single-declarations)
  (let ((number-of-return-pvar-p-declarations
	  (count-if
	    'return-pvar-p-declaration-p
	    list-of-single-declarations
	    )))
    (cond
      ;; Allegro 1.3.2 count-if on a nil list returns nil, rather 0, as CLtL states
      #+:ccl-1.3 ((null number-of-return-pvar-p-declarations) :maybe)
      ((eql 0 number-of-return-pvar-p-declarations) :maybe)
      ((eql 1 number-of-return-pvar-p-declarations)
       (let ((return-pvar-p-declaration
	       (find-if
		 'return-pvar-p-declaration-p
		 list-of-single-declarations
		 )))
	 (if (return-pvar-p-declaration-value return-pvar-p-declaration) :yes :no)
	 ))
      (t (error "More than 1 return-pvar-p declaration found"))
      )))

(defun separate-*lisp-declarations-from-other-declarations (list-of-single-declarations)
  (values
    (remove-if-not '*lisp-declaration-p list-of-single-declarations)
    (remove-if '*lisp-declaration-p list-of-single-declarations)
    ))


(defun create-*defun-wrapping-form (function-name arglist)
  (let ((old-temp-pvar-list-symbol (gensym "*DEFUN-TEMP-")))
    `(let ((,old-temp-pvar-list-symbol *temp-pvar-list*))
       (handle-returning-pvar (,function-name ,@arglist) ,old-temp-pvar-list-symbol nil)
       )))


(defmacro *defun
    #-KCL (fname arglist &rest body &environment env)
  #+KCL (&environment env fname arglist &rest body)
  #+symbolics (declare (zwei:indentation 2 1))
  (let ((internal-symbol (make-*defun-function fname)))
    (multiple-value-bind (documentation declarations body)
        (parse-body-form-documentation-and-declarations body env)
      (setq declarations (declarations-into-list-of-single-declarations declarations))
      (multiple-value-bind (*lisp-declarations other-declarations)
          (separate-*lisp-declarations-from-other-declarations declarations)
        (let ((return-pvar-p-value (return-pvar-p-value-from-list-of-*lisp-declarations *lisp-declarations))
              (return-value-symbol (gensym "RETURN-VALUE-"))
              )
          `(progn
             (defmacro ,fname (&rest args)		
               ,@documentation
               ,@(hack-declarations-for-symbolics other-declarations arglist)
               (create-*defun-wrapping-form ',internal-symbol args)
               )
             (defun ,internal-symbol ,arglist
               ,@other-declarations
               #+symbolics
               (declare (system:function-parent ,fname defun))
               (let ((,return-value-symbol (block ,fname ,@body)))
                 (check-return-pvar-p ,return-value-symbol ,return-pvar-p-value)
                 ,return-value-symbol
                 ))
             ',fname 
             ))))))


(defmacro *locally
    #-KCL
    (&body body &environment env)
    #+KCL
    (&environment env &body body)
  (multiple-value-bind (declarations real-body)
      (split-declarations-and-body body env)
    declarations
    `(progn ,@real-body)
    ))


;;;; ****************************************************************************
;;;;
;;;;                              CODE FOR *SET
;;;;
;;;; ****************************************************************************


(defun even-and-odd-elements (sequence)
  (assert (evenp (length sequence)))
  (let ((even-elements nil) (odd-elements nil) (count 0))
    (map nil
	 #'(lambda (element)
	     (if (evenp count) (push element even-elements) (push element odd-elements))
	     (incf count)
	     )
	 sequence
	 )
    (values (nreverse even-elements) (nreverse odd-elements))
    ))


(defvar *allow-compiled-*set nil)


(defmacro *set
    #-KCL
    (&rest dest-source-pairs &environment env)
    #+KCL
    (&environment env &rest dest-source-pairs)
  (argument-list-declaration
    dest-pvar source-pvar-expression &rest more-dest-source-pvar-pairs
    )

  (when *allow-compiled-*set
    (return-from *set
      (let ((length (length dest-source-pairs)))
	(cond
	  ((zerop length) nil)
	  ((oddp length) (error "Odd number of arguments to *SET"))
	  (t
	   (multiple-value-bind (destinations sources)
	       (even-and-odd-elements dest-source-pairs)
	     `(progn
		,@(mapcar
		    #'(lambda (dest source)
			(let ((code #+*lisp-simulator nil
				    #-*lisp-simulator
				    (*set-can-be-compiled-p `(,dest ,source) env)))
			  (if code
			      code
			      (let ((*allow-compiled-*set nil))
				(macroexpand-1 `(*set ,dest ,source) env)
				))))
		    destinations
		    sources
		    ))))))))

  (let ((length (length dest-source-pairs)))
    (cond
      ((zerop length) nil)
      ((oddp length) (error "Odd number of arguments to *SET"))
      ((and (= 2 length) (symbolp (second dest-source-pairs)) (symbolp (first dest-source-pairs)))
       `(*copy-pvar ,(first dest-source-pairs) ,(second dest-source-pairs))
       )

      (t
       (multiple-value-bind (destinations sources)
	   (even-and-odd-elements dest-source-pairs)
	 (let ((old-temp-pvar-list-symbol (gensym "OLD-TEMP-PVAR-LIST-")))
	   `(let ((,old-temp-pvar-list-symbol *temp-pvar-list*))
	      ,@(mapcar #'(lambda (dest source) `(*copy-pvar ,dest ,source)) destinations sources)
	      (handle-returning-pvar nil ,old-temp-pvar-list-symbol nil)
	      ))))

      )))
		

;;;; ****************************************************************************
;;;;
;;;;                         CODE FOR *LET and *LET*
;;;;
;;;; ****************************************************************************


(defmacro *let
    #-KCL
    (bindings &body body &environment env)
    #+KCL
    (&environment env bindings &body body)
    (*let-1 t bindings body env)
    )

(defmacro *let*
    #-KCL
  (bindings &body body &environment env)
  #+KCL
  (&environment env bindings &body body)
  (*let-1 nil bindings body env)
  )


;;; We want a function which returns 8 values:
;;; 1) A list of variables being bound
;;; 2) A list of initialization forms for those variables
;;; 3) A list of types for each bound variable, obtained from the declarations
;;; 4) A list of any variables declared that are not bound
;;; 5) A list of the types declared for variables that are declared but not bound
;;; 6) A list of any other random decl-specs.
;;; 7) One of :YES :NO or :MAYBE as to (declare (return-pvar-p ...))
;;; 7) The body of the *LET or *LET*

;;; The input is two forms, the list of bindings and the list of forms
;;; that constitute the body.  


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *no-binding-for-*let-value*)
  (setq *no-binding-for-*let-value* (gensym "NO-BINDING-"))
  )


(defun parse-*let-bindings-declarations-and-body (bindings body-forms env)
  
  ;; Each body form must be macroexpanded until one of them
  ;; produces a non DECLARE statement.  Further forms in the
  ;; body need not be macroexpanded.
  
  (multiple-value-bind (bound-variables-list initialization-forms-list)
      
      (parse-*let-bindings bindings)
    
    (let ((declare-list nil) (real-body-forms body-forms))
      
      ;; Each time we find a declare we put it on the declare list
      ;; and remove it from the real-body-forms list.
      
      (block 
       macroexpander
       (dolist (form body-forms)
         (do ((macroexpanded-form form (macroexpand-1 macroexpanded-form env)))
             ((or (symbolp macroexpanded-form) (not (listp macroexpanded-form))) (return-from macroexpander nil))
           (cond
            ((eq 'declare (car macroexpanded-form))
             (push macroexpanded-form declare-list)
             (pop real-body-forms)
             (return)
             )
            ((or (listp (car macroexpanded-form))
                 (really-is-a-special-form (car macroexpanded-form))
                 (not (macro-function (car macroexpanded-form)))
                 )
             (return-from macroexpander nil))
            ))))
      
      (multiple-value-bind (variable-types random-variables random-variable-types return-pvar-p other-declarations)
          
          (parse-*let-declarations bound-variables-list declare-list)
        
        (values
         bound-variables-list
         initialization-forms-list
         variable-types
         random-variables
         random-variable-types
         other-declarations
         return-pvar-p
         real-body-forms
         )))))


(defun parse-*let-bindings (bindings)

  (let ((variables-list nil) (initialization-forms-list nil))
    
    (mapc
      #'(lambda (binding)
	  (cond
	    ((symbolp binding)
	     (push binding variables-list)
	     (push *no-binding-for-*let-value* initialization-forms-list)
	     )
	    ((listp binding)
	     (assert (< (length binding) 3) ()
		     "The binding form ~S for a *LET or *LET* has more than two elements" binding)
	     (assert (eql 2 (length binding)) ()
		     "The binding form ~S for a *LET or *LET* has no initial value specified" binding)
	     (assert (symbolp (car binding)) ()
		     "The object ~S is not a symbol and hence is not legal as the car of the binding form ~S for *LET or *LET*"
		     (car binding) binding)
	     (assert (not (member (car binding) '(t!! nil!! t nil))) ()
		     "You cannot bind the symbol ~S !!" (car binding))
	     (push (first binding) variables-list)
	     (push (second binding) initialization-forms-list)
	     )
	    (t
	     (error "The object ~S is not a legal binding form for *LET or *LET*" binding)
	     )))
      bindings
      )

    (values (nreverse variables-list) (nreverse initialization-forms-list))

    ))


(defun parse-*let-declarations (bound-variables-list declare-list)

  (let ((all-decl-specs (apply #'append (mapcar #'cdr declare-list))))
    
    ;; Separate out the type declarations from other declarations.

    (let ((all-type-decl-specs
	    (remove-if
	      #'(lambda (decl-spec) (or (not (listp decl-spec)) (not (eq 'type (car decl-spec)))))
	      all-decl-specs
	      ))
	  (other-decl-specs
	    (remove-if-not
	      #'(lambda (decl-spec) (or (not (listp decl-spec)) (not (eq 'type (car decl-spec)))))
	      all-decl-specs
	      ))
	  )

      (let ((variable-type-pairs-list (parse-type-decl-specs all-type-decl-specs)))

	;; Check for duplicate variable declarations.

	(dolist (pair variable-type-pairs-list)
	  (let ((variable (first pair)))
	    (when (not (eql 1 (count variable variable-type-pairs-list :key #'car)))
	      (error "The variable ~S was declared twice in the same set of *LET or *LET* declaration forms" variable)
	      )))

	(let ((unbound-variables-list nil)
	      (unbound-variables-type-list nil)
	      (bound-variables-type-list nil)
	      )

	  ;; For each bound variable, if it has a type declaration,
	  ;; associate that type with the bound variable.

	  (setq bound-variables-type-list
		(mapcar
		  #'(lambda (bv)
		      (let ((variable-type-pair (assoc bv variable-type-pairs-list)))
			(if variable-type-pair (second variable-type-pair) '(pvar *))
			))
		  bound-variables-list
		  ))

	  ;; For each type / variable pair that was declared,
	  ;; if the variable is not one of the bound *LET or *LET*
	  ;; variables, then push the variable and its associated
	  ;; type onto their own lists.

	  (mapc
	    #'(lambda (variable-type-pair)
		(let ((variable (first variable-type-pair))
		      (type (second variable-type-pair))
		      )
		  (when (not (member variable bound-variables-list))
		    (push variable unbound-variables-list)
		    (push type unbound-variables-type-list)
		    )))
	    variable-type-pairs-list
	    )

	  ;; Look for return-pvar-p declarations.

	  (let ((return-pvar-p :maybe) (found-return-pvar-p nil))

	    (dolist (other-decl other-decl-specs)
	      (when (and (listp other-decl) (eq 'return-pvar-p (car other-decl)))
		(cond
		  ((not (eql 2 (length other-decl)))
		   (error "The return-pvar-p declaration ~S is not syntactically correct" other-decl)
		   )
		  ((not (member (second other-decl) '(t nil)))
		   (error "The return-pvar-p declaration ~S is not syntactically correct.  You must specify T or NIL" other-decl)
		   )
		  (found-return-pvar-p
		   (error "More than one return-pvar-p declaration exists.")
		   )
		  (t
		   (setq found-return-pvar-p t)
		   (if (eq t (second other-decl))
		       (setq return-pvar-p :yes)
		       (setq return-pvar-p :no)
		       )))))

	    (values bound-variables-type-list unbound-variables-list unbound-variables-type-list return-pvar-p other-decl-specs)

	  ))))))


(defun parse-type-decl-specs (type-decl-specs) 

  ;; A type-decl-spec has the form (type <type> &rest symbols)
  ;; This checks that syntax, then returns a list, each of
  ;; whose elements is a 2 element list, the first element being
  ;; a symbol, and the second element being the canonical pvar
  ;; type that that symbol is being declared as.

 (mapcan
    #'(lambda (decl-spec)
	(assert (not (eql 1 (length decl-spec))) () "The type declaration ~S has no type or variables!" decl-spec)
	(assert (not (eql 2 (length decl-spec))) ()
		"The type declaration ~S is lacking any variables.  You're parentheses may be in the wrong place." decl-spec)
	(let ((type (second decl-spec))
	      (symbols (cddr decl-spec))
	      )
	  (assert (every #'symbolp symbols) ()
		  "The type declaration ~S contains non-symbols as objects being declared" decl-spec)
	  (let ((canonical-type (valid-pvar-type-p type)))
	    (mapcar #'(lambda (symbol) (list symbol canonical-type)) symbols)
	    )))
    type-decl-specs
    ))


(defun allocate-array-pvar-form-given-canonical-type (canonical-pvar-type)
  (let ((dimensions (array-pvar-type-dimensions canonical-pvar-type))
	(element-type (array-pvar-type-element-type canonical-pvar-type))
	)
    (cond
      ((eq element-type t)
       (error "*Lisp does not currently allow arrays with element type T (i.e., arrays with general pvars as elements"))
      ((eq element-type '*)
       (error "*Lisp does not currently allow arrays with element type * (i.e., arrays with unspecified pvars as elements)"))
      ((eq '* (length-pvar-type `(pvar ,element-type)))
       (error "*Lisp does not currently allow arrays to be allocated whose element length is not completely specified"))
      )
    (let ((dimensions-form
	    (cond
	      ((eq '* dimensions)
	       (error "*Lisp does not allow arrays to be allocated with unspecified dimensions")
	       )
	      ((symbolp dimensions) dimensions)
	      ((every #'integerp dimensions) `',dimensions)
	      ((some #'(lambda (x) (eq x '*)) dimensions)
	       (error "*Lisp does not allow arrays to be allocated with any dimension size left unspecified")
	       )
	      (t `(list ,@dimensions))
	      ))
	  (dimensions-symbol (gensym "DIMENSIONS-TEMP-"))
	  )
      `(let ((,dimensions-symbol ,dimensions-form))
	 (allocate-temp-array-pvar
	   (make-canonical-pvar-type 'array :dimensions ,dimensions-symbol :element-type ',element-type)
	   )))))

(defun allocate-temp-pvar-form-given-canonical-pvar-type (canonical-pvar-type)
  (cond
    ((null canonical-pvar-type) `(allocate-temp-general-pvar))
    ((array-pvar-type-p canonical-pvar-type)
     (allocate-array-pvar-form-given-canonical-type canonical-pvar-type))
    ((structure-pvar-type-p canonical-pvar-type)
     `(allocate-temp-structure-pvar ',canonical-pvar-type)
     )
    (t `(allocate-temp-general-pvar))
    ))

(defun set-*let-pvar-fields (pvar name canonical-pvar-type)
  (setf (pvar-name pvar) name)
  (setf (pvar-canonical-pvar-type pvar) canonical-pvar-type)
  (setf (pvar-lvalue? pvar) t)
  (setf (pvar-constant? pvar) nil)
  )
  

(defun check-return-pvar-p (value return-pvar-p)
  (ecase return-pvar-p
    (:yes (assert (pvar-p value) ()
		  "You promised that the enclosing *defun, *let or *let* was returning a pvar, but it isn't, it's returning ~S"
		  value
		  ))
    (:no (assert (not (pvar-p value)) ()
		 "You promised that the enclosing *defun, *let or *let* was not returning a pvar, but it is, it's returning ~S"
		 value
		 ))
    (:maybe t)
    ))

(defun *let-1 (*let? bindings body env)
  
  (multiple-value-bind
    (
     bound-variables initialization-forms bound-variable-types unbound-variables
     unbound-variable-types other-decl-specs return-pvar-p body
     )
      (parse-*let-bindings-declarations-and-body bindings body env)
    
    other-decl-specs unbound-variable-types

    (dolist (v unbound-variables)
      (warn "The symbol ~S was declared but not bound in a *LET or *LET*" v)
      )
    
    (let ((bound-variable-types (mapcar #'canonical-pvar-type bound-variable-types)))

      (let* ((return-value-symbol (gensym "*LET-RETURN-VALUE-"))
	     (old-temp-pvar-list-symbol (gensym "OLD-TEMP-PVAR-LIST-"))
	     (is-definitely-pvar-code
	       `(or ,@(mapcar
			#'(lambda (v) `(eq ,v ,return-value-symbol))
			bound-variables
			)))
	     )
	     
	`(let ((,old-temp-pvar-list-symbol *temp-pvar-list*))

	   (,(if *let? 'let 'let*)
	    (,@(mapcar
		 #'(lambda (v i type)
		     (let ((temp (gensym (concatenate 'string (symbol-name v) "-"))))
		       `(,v (let ((,temp ,(allocate-temp-pvar-form-given-canonical-pvar-type type)))
			      (set-*let-pvar-fields ,temp ',v ',type)
			      ,@(when (not (eq i *no-binding-for-*let-value*))
				  `((*set ,temp ,i))
				  )
			      ,temp
			      ))))
		 bound-variables
		 initialization-forms
		 bound-variable-types
		 ))
	    nil

	    (let ((,return-value-symbol (progn ,@body)))
	      ,@(when (not (eq :maybe return-pvar-p))
		  `((check-return-pvar-p ,return-value-symbol ,return-pvar-p))
		  )
	      (handle-returning-pvar
		,return-value-symbol
		,old-temp-pvar-list-symbol
		,is-definitely-pvar-code
		))

	    ))))))

