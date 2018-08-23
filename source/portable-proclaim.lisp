;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *SIM-I; MUSER: YES -*-

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


#|

This defines the Starlisp type system.

*PROCLAIM.  

This is a user level macro.  It is used to
tell Starlisp (mostly the Starlisp compiler) various things,
like type information.

Evaluating a *PROCLAIM will have the side effect of
changing various property lists of symbols and internal
lists that Starlisp keeps around.

*DECLAIM 

Like *PROCLAIM, but takes a set of proclamations which
do not have to be quoted.

PROCLAIM, DECLAIM

These shadow their Common Lisp counterparts, enabling *Lisp
to record information (mostly type information) provided by
these constructs.  Each ends up calling its Lisp counterpart
eventually.

UNPROCLAIM

This is a user level function.  it is used to undo the
effects of a *PROCLAIM, *DECLAIM, DECLAIM or a PROCLAIM.

STANDARD-TYPE-P

For Starlisp's internal use.  Whether a type name is a
standard Common Lisp type or not.

CANONICAL-TYPE

For Starlisp's internal use.  Returns a symbol or form
which is the canonical representation for the type
specified by the input.

PROCLAIMED-TYPE

Returns the type a symbol has been proclaimed to have.

PROCLAIMED-FTYPE

Returns the function type a symbol has been proclaimed to have.

PROCLAIMED-SPECIAL-P

Returns whether a symbol have been proclaimed special.

PVAR

This is redefined as a Common Lisp deftype.  
The Common Lisp function typep then works
using this deftype definition.

 A canonical type is one of the following:
 (pvar boolean)
 (pvar (unsigned-byte <length>)), where length is *, or an expression.
 (pvar (signed-byte <length>)), where length is *, or an expression.
 (pvar (defined-float <mantissa> <exponent>)), where mantissa and exponent are *, or an expression.
 (pvar (complex (defined-float <mantissa> <exponent>))), where mantissa and exponent are *, or an expression.
 (pvar (array <element-type> <dimensions>)), where element-type is * or a canonical pvar element-type,
      and where dimensions are *, or a list of *, or expressions.
 (pvar (structure <name>)), where name is *, or a symbol that has been defined by *defstruct.
 (pvar string-char)
 (pvar character)
 (pvar front-end)
 (pvar t)
 (pvar *)

CANONICAL-PVAR-TYPE

For Starlisp's internal use.  Returns a symbol or form
which is the canonical representation of the pvar type
specified by the input, or NIL if the input is not
a legal canonical pvar type.

VALID-PVAR-TYPE-P

For Starlisp's internal use.  Same as CANONICAL-PVAR-TYPE
except that it takes an extra optional argument, error,
which defaults to T.  If error is T, then if the type
passed in is not a legal pvar type, the routine will
error out and print out a message as to why the type
was not legal.

LENGTH-PVAR-TYPE

For Starlisp's internal use.  Returns the length in bits
of a type or * if the type specifies an indeterminate
length.

CANONICAL-PVAR-ELEMENT-TYPE

This will return one of boolean, unsigned-byte, signed-byte, defined-float,
complex, string-char, character, array, structure, t, front-end, or *.

MAKE-CANONICAL-PVAR-TYPE

This takes a pvar element type as defined in CANONICAL-TYPE-ELEMENT-TYPE
and other parameters and returns a canonical pvar type.

MAKE-*DEFUN-FUNCTION

Given a symbol foo, this returns another symbol which is the
name of an internal function which would be defined if
*DEFUN were used to define foo.

ARRAY-PVAR-TYPE-P
STRUCTURE-PVAR-TYPE-P
FLOAT-PVAR-TYPE-P
COMPLEX-PVAR-TYPE-P
BOOLEAN-PVAR-TYPE-P
FRONT-END-PVAR-TYPE-P
GENERAL-PVAR-TYPE-P
STRING-CHAR-PVAR-TYPE-P
CHARACTER-PVAR-TYPE-P
SIGNED-PVAR-TYPE-P
UNSIGNED-PVAR-TYPE-P

These all decide if a pvar type is of the
appropriate class.  They only work on
canonical pvar types.

FLOAT-PVAR-TYPE-MANTISSA
FLOAT-PVAR-TYPE-EXPONENT
COMPLEX-PVAR-TYPE-MANTISSA
COMPLEX-PVAR-TYPE-EXPONENT 
ARRAY-PVAR-TYPE-DIMENSIONS
ARRAY-PVAR-TYPE-ELEMENT-TYPE
STRUCTURE-PVAR-TYPE-NAME

These extract parameters from a canonical
pvar type of the proper type.

|#



(declaim (special *speed* *safety* *space* *compilation-speed* *verbose*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant short-float-mantissa 15)
  (defconstant short-float-exponent 8)
  (defconstant single-float-mantissa 23)
  (defconstant single-float-exponent 8)
  (defconstant double-float-mantissa 52)
  (defconstant double-float-exponent 11)
  (defconstant long-float-mantissa 74)
  (defconstant long-float-exponent 21)
  (defconstant extended-float-mantissa 96)
  (defconstant extended-float-exponent 31)
  )



(defmacro *proclaim (decl-spec)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (starlisp-proclaim ,decl-spec)))

(defun proclaim (proclamation)
  (starlisp-proclaim proclamation)
  )

(defmacro *declaim (&rest decl-specs)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(mapcar #'(lambda (ds) `(starlisp-proclaim ',ds)) decl-specs)
     ))

(defmacro declaim (&rest decl-specs) `(*declaim ,@decl-specs))

(defun proclaimed-type (symbol &optional (default-type 't))
  (get symbol :*lisp-type default-type))

(defun proclaimed-ftype (symbol &optional lexical-only-p)
  (or (cdr (assoc symbol (list-of-slc-function-types)))
      (and (not lexical-only-p) (get-function-type symbol))))

(defun proclaimed-declaration-p (symbol)
  (get symbol :*lisp-declaration))

(defun proclaimed-special-p (symbol)
  (get symbol :*lisp-special))


;;;
;;; This is so that we can capture deftype information for starlisp-proclaim to do type expansion.
;;;

(defmacro deftype (name lambda-list &rest body)
  `(progn
     (setf (get ',name :*lisp-deftype)
       #'(lambda (&rest form)
           (destructuring-bind ,lambda-list form ,@body)))
       (cl:deftype ,name ,lambda-list ,@body)))

(defun expand-one-deftype (type)
  (if (atom type) (setq type (list type)))
  (let ((expander (get (first type) :*lisp-deftype)))
    (if (null expander)
	nil
      (apply expander (rest type))
      )))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-body-macroexpand-1 (form env)
    (let ((*compiling* nil) (*compilep* nil))
      (declare (special *compiling* *compilep*))
      (macroexpand-1 form env)
      )))


(defun check-ftype-proclamation (ftype-proclamation)
  (let ((*print-level* 10))
    (flet
      ((ftype-error (string)
	 (format
	   t
	   "~%~%The FTYPE proclamation, ~S, is not of the form ~@
            (ftype (function (&rest <argument types>) <result type>) &rest function-names).~@
            You might have your parentheses misplaced."
	   ftype-proclamation
	   )
	 (error "~A" string)
	 ))
      (cond
	((= (length ftype-proclamation) 1) (ftype-error "There is nothing after the FTYPE."))
	((= (length ftype-proclamation) 2) (ftype-error "No function-names were provided."))
	((not (listp (second ftype-proclamation))) (ftype-error "The FUNCTION specification is not a list"))
	((not (every #'symbolp (cddr ftype-proclamation))) (ftype-error "The FUNCTION-NAMES are not all symbols"))
	)
      (let ((function-spec (second ftype-proclamation)))
	(cond
	  ((not (eq 'function (first function-spec))) (ftype-error "The FUNCTION specification does not begin with FUNCTION"))
	  ((< (length function-spec) 3) (ftype-error "The FUNCTION specification does not specify a result type"))
	  ((> (length function-spec) 3) (ftype-error "The FUNCTION specification specifies more than one result type"))
	  ((not (listp (second function-spec))) (ftype-error "The FUNCTION specification argument list is not a list"))
	  )))
    nil
    ))

(defun starlisp-proclaim (decl-spec)
  
  (case (car decl-spec)
    
    (special
     (dolist (variable (cdr decl-spec))
       (setf (get variable :*lisp-special) t)))
    
    (ftype
     ;; Syntax like, (ftype (function (...) type) &rest foos)
     (let ((ftype (cadr decl-spec)))
       (check-ftype-proclamation decl-spec)
       (let* ((argument-types 
               (mapcar #'(lambda (type)
                           (if (member type lambda-list-keywords) type (canonical-type type nil t)))
		       (cadr ftype)))
              (rtype (caddr ftype))
              (return-type (if (and (consp rtype) (member (car rtype) '(satisfies and or not member values)))
                               rtype
                             (canonical-type rtype nil t))))
         (dolist (function (cddr decl-spec))
           (set-function-type 
	    function 
	    `(function ,argument-types ,return-type)))
         (setq decl-spec
	   `(ftype 
	     (function 
	      ,(mapcar 
		#'(lambda (type) 
		    (if (and (consp type) (eq (car type) 'pvar)) 'pvar type))
		argument-types)
;; 04/09/03 changed return-type to (or return-type t)
	      ,(if (and (consp return-type) (eq (car return-type) 'pvar)) 
		   'pvar
		 (or return-type t)))
	     ,@(cddr decl-spec)))))
     (print (list 'decl-spec decl-spec)))

    (function
     (if (and (cddr decl-spec) (listp (caddr decl-spec)))
         (let ((ftype `(function ,(caddr decl-spec) ,@(cdddr decl-spec))))
           (set-function-type (cadr decl-spec) ftype))
       (dolist (variable (cdr decl-spec))
         (setf (get variable :*lisp-type) 'function))))

    (inline)
    (notinline)

    ((optimize *optimize)
     (dolist (quality-value (cdr decl-spec))
       (let ((quality (if (consp quality-value) (car quality-value) quality-value))
             (value (if (consp quality-value) (cadr quality-value) 3)))
         (case quality
           (speed (setq *speed* value))
           (safety (setq *safety* value))
           (space (setq *space* value))
           (compilation-speed (setq *compilation-speed* value))
           (otherwise
            (cerror "Ignore declaration." "Invalid quality ~S for optimize proclamation." quality))))))

    (declaration
     (dolist (declaration (cdr decl-spec))
       (setf (get declaration :*lisp-declaration) t)))

    (ignore
     (dolist (variable (cdr decl-spec))
       (setf (get variable ':*lisp-ignore) t)))
    (ignorable
     (dolist (variable (cdr decl-spec))
       (setf (get variable ':*lisp-ignorable) t)))

    (type
     (let ((type (canonical-type (cadr decl-spec) nil t)))
       (if (and (null type) (cadr decl-spec))
           (cerror "Ignore declaration." "~S is not a valid type in proclaim decl-spec ~S." 
                   (cadr decl-spec) decl-spec)
         (dolist (variable (cddr decl-spec))
	   ;; Put the type in an easy to find place.
           (setf (get variable :*lisp-type) type)
	   ;; If type is a pvar type, then put a slc-pvar descriptor on the descriptor property.  
	   ;; If the type is not a pvar type, then add a vanilla descriptor to the descriptor property.
           (if (and (consp type) (eq (car type) 'pvar))
	       ;; Type is a pvar type.
               (progn
                 (set-variable-descriptor 
                  variable (make-pvar-for-compiler :type type :reference variable :read-only t))
                 (funcall 'cl:proclaim `(type pvar ,variable)))
	     ;; Type is not a pvar, add vanilla descriptor.
             (progn
               (set-variable-descriptor 
                variable (make-descriptor-for-compiler :type type :reference variable))
               (funcall 'cl:proclaim `(type ,type ,variable))))))))

    (*defun
     (dolist (function (cdr decl-spec))
       (funcall 'proclaim-*defun-1 function)))

    (*option
     (let* ((keyword (cadr decl-spec)) (value (caddr decl-spec))
            (option (find-compiler-keyword keyword)))
       (if option
           (deal-with-compiler-option-value option value keyword)
         (cerror "Ignore declaration." "Unknown starlisp compiler option ~S." keyword))))

    (otherwise
     (cond ((not (symbolp (car decl-spec)))
            (warn "Invalid decl-spec ~S to proclaim, retrying with (TYPE ~{~S~^ ~})." decl-spec decl-spec)
            (return-from starlisp-proclaim (proclaim (cons 'type decl-spec))))
           ((standard-type-p (car decl-spec))
            (let ((type (car (canonical-type decl-spec))))
              (dolist (variable (cdr decl-spec))
		;; If variable was previously proclaimed to be a pvar, then get rid of pvar declaration.
                (setf (get variable :*lisp-type) type)
                (set-variable-descriptor variable (make-descriptor-for-compiler :type type :reference variable)))))
           ((proclaimed-declaration-p (car decl-spec))
            nil
            )
           (t 
            (warn "Invalid decl-spec ~S to proclaim, retrying with (TYPE ~{~S~^ ~})." decl-spec decl-spec)
            (return-from starlisp-proclaim (proclaim (cons 'type decl-spec)))
            ))))

  (unless (or (eq (car decl-spec) 'type) (eq (car decl-spec) '*defun))
    (funcall 'cl:proclaim decl-spec))

  nil

  )

(defun unproclaim (decl-spec)
  (starlisp-unproclaim decl-spec))

;; this should call lucid unproclaim afterwards,  same as *proclaim.
(defun starlisp-unproclaim (decl-spec)
  (case (car decl-spec)
    (special
      (dolist (variable (cdr decl-spec))
	(remprop variable 'special)))
    (ftype
      (dolist (function (cddr decl-spec))
	(remove-function-type function)))
    (function
      (if (and (cddr decl-spec) (listp (caddr decl-spec)))
	  (remprop (cadr decl-spec) 'function-type)
	  (dolist (variable (cdr decl-spec))
	    (remprop variable 'type))))
    ((inline notinline))
    ((optimize *optimize)
      (dolist (quality-value (cdr decl-spec))
	(let ((quality (if (consp quality-value) (car quality-value) quality-value)))
	  (case quality
	    (speed (setq *speed* 1))
	    (safety (setq *safety* 1))
	    (space (setq *space* 1))
	    (compilation-speed (setq *compilation-speed* 1))
	    (otherwise
	      (error "Invalid quality ~S for optimize proclamation.~
                    Only SPEED, SAFETY, COMPILATION-SPACE, and SPACE are allowed." quality))))))
    (declaration
      (dolist (declaration (cdr decl-spec))
	(remprop declaration 'declaration)))
    (ignore
      (dolist (variable (cdr decl-spec))
	(remprop variable 'ignore)))
    (type
      (dolist (variable (cddr decl-spec))
	(remprop variable 'type)
	(remove-variable-descriptor variable)))
    (*defun
      (dolist (function (cdr decl-spec)) nil)) ;(unproclaim-*defun-1 function)

    (otherwise
     (cond ((not (symbolp (car decl-spec)))
            (warn "Invalid decl-spec ~S to unproclaim, retrying with (TYPE ~{~S~^ ~})." decl-spec decl-spec)
            (return-from starlisp-unproclaim (unproclaim (cons 'type decl-spec))))
           ((standard-type-p (car decl-spec))
            (dolist (variable (cdr decl-spec))
              (remprop variable 'type)
              (remove-variable-descriptor variable)))
           ((proclaimed-declaration-p (car decl-spec))
            nil
            )
	    (t 
      (warn "Invalid decl-spec ~S to unproclaim, retrying with (TYPE ~{~S~^ ~})." decl-spec decl-spec)
      (return-from starlisp-unproclaim (unproclaim (cons 'type decl-spec)))))))
  nil)

(defun standard-type-p (type)
  (and (member type 
               '(array atom bignum bit bit-vector character common compiled-function 
                       complex cons double-float fixnum float
                       function hash-table integer keyword list 
                       long-float nil null number package pathname random-state ratio
                       rational readtable sequence short-float simple-array 
                       simple-bit-vector simple-string simple-vector
                       single-float standard-char stream string string-char 
                       symbol t vector mod signed-byte unsigned-byte))
       t))

 
 
(defun canonical-front-end-type (type)
  (case (if (consp type) (car type) type)
    ((boolean standard-char string-char character t null number nil)
     type)
    (integer
      (if (consp type)
	  `(integer ,(or (cadr type) '*) ,(or (caddr type) '*))
	  '(integer * *)))
    (mod
      (let ((limit (if (consp type) (or (cadr type) '*) '*)))
	`(integer 0 ,(if (eq limit '*) '* (list limit)))))
    (unsigned-byte
      (let ((limit (if (consp type) (or (cadr type) '*) '*)))
	`(integer 0 ,(if (eq limit '*) '* (list (simplify-expression (expt2-symbol) limit))))))
    (signed-byte
      (let* ((limit (if (consp type) (or (cadr type) '*) '*))
	     (real-limit (if (eq limit '*) '* (simplify-expression (expt2-1-symbol) limit))))
	`(integer ,(if (integerp real-limit)
		       (- real-limit)
		       (list (simplify-expression '- -1 real-limit)))
		  (,real-limit))))
    (bit '(integer 0 1))
    (fixnum '(integer #.most-negative-fixnum #.most-positive-fixnum))
    (bignum '(or (integer * (#.most-negative-fixnum)) (integer (#.most-positive-fixnum) *)))
    ((float short-float single-float double-float long-float)
     (if (consp type)
	 `(,(car type) ,(or (cadr type) '*) ,(or (caddr type) '*))
	 `(,type * *)))
    (list
      `(or cons null))
    (rational
      `(or integer ratio))
    (sequence
      `(or (array * (*)) cons null))
    ;; string, simple-string, bit-vector, simple-bit-vector
    (string
      `(string ,@(if (and (consp type) (cdr type)) (cdr type) '(*))))
    (simple-string
      `(simple-string ,@(if (and (consp type) (cdr type)) (cdr type) '(*))))
    (bit-vector
      `(bit-vector ,@(if (and (consp type) (cdr type)) (cdr type) '(*))))
    (simple-bit-vector
      `(simple-bit-vector ,@(if (and (consp type) (cdr type)) (cdr type) '(*))))
    (array
      `(array
	 ,(if (and (consp type) (cadr type)) (or (cadr (canonical-pvar-type `(pvar ,(cadr type)))) (cadr type) '*) '*)
	 ,@(if (and (consp type) (cddr type)) (cddr type) '(*))))
    (simple-array
      `(simple-array
	 ,(if (and (consp type) (cadr type)) (or (cadr (canonical-pvar-type `(pvar ,(cadr type)))) (cadr type) '*) '*)
	 ,@(if (and (consp type) (cddr type)) (cddr type) '(*))))
    (vector
      `(array
	 ,(if (and (consp type) (cadr type)) (or (cadr (canonical-pvar-type `(pvar ,(cadr type)))) (cadr type) '*) '*)
	 (,@(if (and (consp type) (cddr type)) (cddr type) '(*)))))
    (simple-vector
      `(array t (,@(if (and (consp type) (cdr type)) (cdr type) '(*)))))
    ;; Complex.?
    (otherwise type)))

;
; A canonical type is one of the following:
; (pvar boolean)
; (pvar (unsigned-byte <length>)), where length is *, or an expression.
; (pvar (signed-byte <length>)), where length is *, or an expression.
; (pvar (defined-float <mantissa> <exponent>)), where mantissa and exponent are *, or an expression.
; (pvar (complex (defined-float <mantissa> <exponent>))), where mantissa and exponent are *, or an expression.
; (pvar (array <element-type> <dimensions>)), where element-type is * or a canonical pvar element-type,
;      and where dimensions are *, or a list of *, or expressions.
; (pvar (structure <name>)), where name is *, or a symbol that has been defined by *defstruct.
; (pvar string-char)
; (pvar character)
; (pvar front-end)
; (pvar t)
; (pvar *)
;

(defun canonical-type (type &optional canonical-front-end-type (syntax-check nil))
  (cond ((standard-type-p type)
	 (if canonical-front-end-type (canonical-front-end-type type) type))
	((eq type 'pvar)
	 '(pvar *))
	((eq type 'fixnum)
	 `(integer #.most-negative-fixnum #.most-positive-fixnum))
	((or (equal type '(member nil t)) (equal type '(member t nil)) (eq type 'boolean))
	 'boolean)
	((eq type 'front-end)
	 'front-end)
	((eq type 'structure)
	 (if canonical-front-end-type '(structure *) 'structure))
	((eq type 'defined-float) (if canonical-front-end-type '(float * *) 'float))
	#+symbolics
	((and (symbolp type) (get type 'si:defstruct-description))
	 (if canonical-front-end-type `(structure ,type) type))
	#+lucid
	((and (symbolp type) (lucid::structure-type-p type))
	 (if canonical-front-end-type `(structure ,type) type))
  	#+cmu
  	((and (symbolp type) (get type 'lisp::%structure-definition))
  	 (if canonical-front-end-type `(structure ,type) type))
	((atom type)
	 (canonical-type (or (expand-one-deftype type) (return-from canonical-type nil))))
	((eq (car type) 'member)
	 (let ((members (cdr type)))
	   (cond ((every #'integerp members)
		  (canonical-type `(integer ,(apply #'min members) ,(apply #'max members)) canonical-front-end-type syntax-check))
		 ((every #'characterp members)
		  (if (every #'string-char-p members)
		      'string-char
		      'character))
		 (t type))))
	((eq (car type) 'defined-float)
	 `(defined-float ,(or (cadr type) '*) ,(or (caddr type) '*)))
	((standard-type-p (car type))
	 (if canonical-front-end-type (canonical-front-end-type type) type))
	((member (car type) '(and or not member satisfies))
	 type)
	((eq (car type) 'structure) ; wonder if I should bother with this?
	 (if (or (null (cadr type)) (eq (cadr type) '*))
	     'structure
	     (if canonical-front-end-type type (cadr type))))
	((eq (car type) 'pvar)
	 (let* ((pvar-type (if syntax-check (canonical-pvar-type (valid-pvar-type-p type t)) (canonical-pvar-type type)))
		(atomic-element-type nil)
		(element-type (cadr pvar-type)))
	   (case (setq atomic-element-type (if (consp element-type) (car element-type) element-type))
	     ((unsigned-byte signed-byte)
	      (let ((length (cadr element-type)))
		(if (numberp length)
		    (if (integerp length)
			(if (<= 1 length)
			    (check-paris-restriction nil length '*maximum-integer-length*
						     "Certain operations can not handle integer pvars with length")
			    (error "Integer pvars must have a positive length instead of ~S." length))
			(error "Integer pvars must have an integer length instead of ~S." length)))))
	     ((defined-float complex)
	       (let* ((complexp (eq atomic-element-type 'complex))
		      (element-type (if complexp (cadr element-type) element-type))
		      (mantissa (cadr element-type)) (exponent (caddr element-type)))
		 (if (numberp mantissa)
		     (if (integerp mantissa)
			 (if (<= 1 mantissa)
			     (check-paris-restriction nil mantissa '*maximum-significand-length*
						      (if complexp
							  "Certain operations can not handle complex pvars with mantissa"
							  "Certain operations can not handle float pvars with mantissa"))
			     (error "~:[Float~;Complex~] pvars must have a positive mantissa length instead of ~S."
				    complexp mantissa))
			 (error "~:[Float~;Complex~] pvars must have an integer mantissa length instead of ~S."
				complexp mantissa)))
		 (if (numberp exponent)
		     (if (integerp exponent)
			 (if (<= 1 exponent)
			     (check-paris-restriction nil exponent '*maximum-exponent-length*
						      (if complexp
							  "Certain operations can not handle complex pvars with exponent"
							  "Certain operations can not handle float pvars with exponent"))
			     (error "~:[Float~;Complex~] pvars must have a positive exponent length instead of ~S."
				    complexp exponent))
			 (error "~:[Float~;Complex~] pvars must have an integer exponent length instead of ~S."
				complexp exponent)))
		 (if (and (integerp exponent) (integerp mantissa))
		     (if (not (and (>= exponent 2) (>= mantissa 1) (>= (expt 2 (1- exponent)) (1+ mantissa))))
			 (warn "Certain operations can not handle ~:[float~;complex~] pvars with format {~D,~D}."
			       complexp mantissa exponent))))))
	   pvar-type))
	((member (car type)
		 `(#+symbolics si:xr-bq-list #+symbolics si:xr-bq-append #+symbolics si:xr-bq-nconc
		   #+symbolics si:xr-bq-cons #+symbolics si:xr-bq-list*
		   #+lcl3.0 lrs:bq-list #+lcl3.0 lrs:bq-append #+lcl3.0 lrs:bq-nconc
		   #+lcl3.0 lrs:bq-cons #+lcl3.0 lrs:bq-list*
		   cons nth nthcdr first second third fourth fifth sixth seventh eighth ninth tenth
		   car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr 
		   caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr 
		   cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr 
		   last list list* append copy-list copy-alist copy-tree revappend nconc nreconc butlast nbutlast
		   ldiff rplaca rplacd subst subst-if subst-if-not nsubst nsubst-if nsubst-if-not sublis
		   nsublis assoc assoc-if assoc-if-not rassoc rassoc-if rassoc-if-not 
		   ))
	 t)
	(t (canonical-type (or (expand-one-deftype type)
			       (return-from canonical-type nil))))))


(defun check-pvar-element-type (object element-type)
  (let ((type (if (consp element-type) (car element-type) element-type))
	(rest (if (consp element-type) (cdr element-type) ()))
	(pvar-type (pvar-type object)))
    (flet ((float-pvar-type (pvar rest mantissa exponent &optional (pvar-types '(:float :number-float)))
	     (and (member (pvar-type pvar) pvar-types)
		  (or (eql mantissa '*)
		      (eql (pvar-mantissa-length pvar) mantissa))
		  (or (eql exponent '*)
		      (eql (pvar-exponent-length pvar) exponent))
		  (or (null rest)
		      (eql (car rest) '*))
		  (or (null (cdr rest))
		      (eql (cadr rest) '*))))
	   (check-integer-range (rest low high)
	     (and (or (null rest)
		      (eql (car rest) '*)
		      (if (consp (car rest))
			  (eql (1+ (caar rest)) low)
			  (eql (car rest) low)))
		  (or (null (cdr rest))
		      (eql (cadr rest) '*)
		      (if (consp (cadr rest))
			  (eql (1- (caadr rest)) high)
			  (eql (cadr rest) high))))))
      (case type
	((t)
	 (eql pvar-type :general))
	(boolean
	  (eql pvar-type :boolean))
	(front-end
	  (eql pvar-type :front-end))
	(number
	  ;; (pvar number),  number type does not specialize.
	  (member pvar-type '(:field :signed :number-float :number-signed :float)))
	(integer
	 (case pvar-type
	   (:field
	     (check-integer-range rest 0 (1- (expt 2 (pvar-length object)))))
	   (:signed
	     (let ((expt (expt 2 (1- (pvar-length object)))))
	       (check-integer-range rest (- expt) (1- expt))))
	   ;; obsolete.
	   (:number-signed
	     t)
	   (otherwise nil)))
	(signed-byte
	  (and (member pvar-type '(:signed :number-signed))
	       (or (null rest)
		   (eql (car rest) '*)
		   (eql (pvar-length object) (car rest)))))
	(unsigned-byte
	  (and (eql pvar-type :field)
	       (or (null rest)
		   (eql (car rest) '*)
		   (eql (pvar-length object) (car rest)))))
	(mod
	  (and (eql pvar-type :field)
	       (or (null rest)
		   (eql (car rest) '*)
		   (eql (1- (expt 2 (pvar-length object))) (car rest)))))
	(float
	  (float-pvar-type object rest '* '*))
	(short-float
	  (float-pvar-type object rest short-float-mantissa short-float-exponent))
	(single-float
	  (float-pvar-type object rest single-float-mantissa single-float-exponent))
	(double-float
	  (float-pvar-type object rest double-float-mantissa double-float-exponent))
	(long-float
	  (float-pvar-type object rest long-float-mantissa long-float-exponent))
	(defined-float
	  (float-pvar-type object () (if (null rest) '* (car rest)) (if (null (cdr rest)) '* (cadr rest))))
	((nil)
	 nil)
	;; Otherwise, I didn't understand the element-type, try again to see if it
	;; is a user defined deftype.
	(otherwise (check-pvar-element-type object (or (expand-pvar-element-type element-type)
					       (return-from check-pvar-element-type nil))))))))

(defun pvarp (object &optional (element-type '*))
  (if (internal-pvarp object)
      (if (eql element-type '*)
	  t
	  (check-pvar-element-type object element-type))))

(defun boolean-pvarp (arg)
  (and (internal-pvarp arg)
       (eql (pvar-type arg) :boolean)))
(defun front-end-pvarp (arg)
  (and (internal-pvarp arg)
       (eql (pvar-type arg) :front-end)))
(defun signed-pvarp (arg &optional (length '*))
  (and (internal-pvarp arg)
       (eql (pvar-type arg) :signed)
       (or (eql length '*)
	   (eql (pvar-length arg) length))))
(defun unsigned-pvarp (arg &optional (length '*))
  (and (internal-pvarp arg)
       (eql (pvar-type arg) :field)
       (or (eql length '*)
	   (eql (pvar-length arg) length))))
(defun float-pvarp (arg &optional (mantissa '*) (exponent '*))
  (and (internal-pvarp arg)
       (eql (pvar-type arg) :float)
       (or (eql mantissa '*)
	   (eql (pvar-mantissa-length arg) mantissa))
       (or (eql exponent '*)
	   (eql (pvar-exponent-length arg) exponent))))
(defun single-float-pvarp (arg)
  (and (internal-pvarp arg)
       (eql (pvar-type arg) :float)
       (eql (pvar-mantissa-length arg) single-float-mantissa)
       (eql (pvar-exponent-length arg) single-float-exponent)))
(defun short-float-pvarp (arg)
  (and (internal-pvarp arg)
       (eql (pvar-type arg) :float)
       (eql (pvar-mantissa-length arg) short-float-mantissa)
       (eql (pvar-exponent-length arg) short-float-exponent)))
(defun double-float-pvarp (arg)
  (and (internal-pvarp arg)
       (eql (pvar-type arg) :float)
       (eql (pvar-mantissa-length arg) double-float-mantissa)
       (eql (pvar-exponent-length arg) double-float-exponent)))
(defun long-float-pvarp (arg)
  (and (internal-pvarp arg)
       (eql (pvar-type arg) :float)
       (eql (pvar-mantissa-length arg) long-float-mantissa)
       (eql (pvar-exponent-length arg) long-float-exponent)))
(defun extended-float-pvarp (arg)
  (and (internal-pvarp arg)
       (eql (pvar-type arg) :float)
       (eql (pvar-mantissa-length arg) extended-float-mantissa)
       (eql (pvar-exponent-length arg) extended-float-exponent)))
(defun character-pvarp (arg)
  (and (internal-pvarp arg)
       (eql (pvar-type arg) :character)))
(defun string-char-pvarp (arg)
  (and (internal-pvarp arg)
       (eql (pvar-type arg) :string-char)))
(defun general-pvarp (arg)
  (and (internal-pvarp arg)
       (eql (pvar-type arg) :general)
       (not (void-pvar-p arg))))

(defun complex-pvarp (arg &optional (mantissa '*) (exponent '*))
  (and (internal-pvarp arg)
       (eql (pvar-type arg) :complex)
       (or (eql mantissa '*)
	   (eql (pvar-mantissa-length arg) mantissa))
       (or (eql exponent '*)
	   (eql (pvar-exponent-length arg) exponent))))
(defun single-complex-pvarp (arg)
  (and (internal-pvarp arg)
       (eql (pvar-type arg) :complex)
       (eql (pvar-mantissa-length arg) single-float-mantissa)
       (eql (pvar-exponent-length arg) single-float-exponent)))
(defun short-complex-pvarp (arg)
  (and (internal-pvarp arg)
       (eql (pvar-type arg) :complex)
       (eql (pvar-mantissa-length arg) short-float-mantissa)
       (eql (pvar-exponent-length arg) short-float-exponent)))
(defun double-complex-pvarp (arg)
  (and (internal-pvarp arg)
       (eql (pvar-type arg) :complex)
       (eql (pvar-mantissa-length arg) double-float-mantissa)
       (eql (pvar-exponent-length arg) double-float-exponent)))
(defun long-complex-pvarp (arg)
  (and (internal-pvarp arg)
       (eql (pvar-type arg) :complex)
       (eql (pvar-mantissa-length arg) long-float-mantissa)
       (eql (pvar-exponent-length arg) long-float-exponent)))
(defun extended-complex-pvarp (arg)
  (and (internal-pvarp arg)
       (eql (pvar-type arg) :complex)
       (eql (pvar-mantissa-length arg) extended-float-mantissa)
       (eql (pvar-exponent-length arg) extended-float-exponent)))

(defun array-pvarp (arg &optional (element-type '*) (dimensions '*))
  (and (internal-pvarp arg)
       (eql (pvar-type arg) :array)
       (or (eql element-type '*)
	   (equal (cadr (portable-pvar-array-element-type arg)) element-type) )
       (or (eql dimensions '*)
	   (array-pvarp-check-dimensions (portable-pvar-array-dimensions arg) dimensions))))

(defun array-pvarp-check-dimensions (arg-array-dimensions array-dimensions)
  (do ((arg-array-dimensions arg-array-dimensions (cdr arg-array-dimensions))
       (array-dimensions array-dimensions (cdr array-dimensions)))
      ((or (null arg-array-dimensions) (null array-dimensions))
       (and (null arg-array-dimensions) (null array-dimensions)))
    (unless (or (eq (car array-dimensions) '*) (equal (eval (car array-dimensions)) (car arg-array-dimensions)))
      (return nil))))

(defun structure-pvarp (arg &optional (name '*))
  (and (internal-pvarp arg)
       (eql (pvar-type arg) :structure)
       (or (eq name '*) (funcall 'included-*defstruct-p (portable-pvar-structure-name arg) name))))


(defparameter unsigned-pvarp-limit 0)
(defparameter unsigned-pvarps nil)
(defparameter signed-pvarp-limit 0)
(defparameter signed-pvarps nil)
 
(defun unsigned-pvarp-closure (length)
  #'(lambda (arg)
      (and (internal-pvarp arg)
	   (eql (pvar-type arg) :field)
	   (eql (pvar-length arg) length))))
(defun signed-pvarp-closure (length)
  #'(lambda (arg)
      (and (internal-pvarp arg)
	   (eql (pvar-type arg) :signed)
	   (eql (pvar-length arg) length))))


(defun initialize-integer-pvarps (limit)
  (setq unsigned-pvarp-limit limit)
  (setq signed-pvarp-limit limit)
  (setq unsigned-pvarps (make-array limit))
  (setq signed-pvarps (make-array limit))
  (let ((*print-case* :upcase))
    (dotimes (index limit)
      (setf (symbol-function
	      (setf (aref unsigned-pvarps index)
		    (intern (format nil "UNSIGNED~D-PVARP" index) *starlisp-internal-package-name*)))
	    (unsigned-pvarp-closure index))
      (setf (symbol-function
	      (setf (aref signed-pvarps index)
		    (intern (format nil "SIGNED~D-PVARP" index) *starlisp-internal-package-name*)))
	    (signed-pvarp-closure index)))))

(initialize-integer-pvarps 129)


#+lucid
(defvar save-normalize-type #'lucid::normalize-type)

#+symbolics
(proclaim '(special *dont-use-closure-for-pvar-types*))
#+lucid
(defvar *dont-use-closure-for-pvar-types* nil)

#+lucid
(defun lucid::normalize-type (type)
  (let ((*dont-use-closure-for-pvar-types* t))
    (funcall save-normalize-type type)))

(defparameter *pvar-type-predicates* ())

(defun add-pvar-type-predicate (pvar-type predicate)
  (unless (symbolp predicate)
    (error "predicate ~S must be a symbol." predicate))
  (setq pvar-type (canonical-pvar-type pvar-type))
  (unless (and (consp pvar-type) (eq (car pvar-type) 'pvar))
    (error "~S must be a pvar type." pvar-type))
  (pushnew (cons (cadr pvar-type) predicate) *pvar-type-predicates* :test #'equal)
  (values))

(defun pvar-type-predicate (element-type)
  (or (cdr (assoc element-type *pvar-type-predicates* :test #'equal))
      (case (if (consp element-type) (car element-type) element-type)
	(* 'internal-pvarp)
	(signed-byte
	  (cond ((and (integerp (cadr element-type)) (>= (cadr element-type) 0) (< (cadr element-type) signed-pvarp-limit))
		 (aref signed-pvarps (cadr element-type)))
		((or #+lucid *dont-use-closure-for-pvar-types*
		     (atom element-type) (eql (cadr element-type) '*))
		 'signed-pvarp)
		(t `(lambda (object) (signed-pvarp object ,(cadr element-type))))))
	(unsigned-byte
	  (cond ((and (integerp (cadr element-type)) (>= (cadr element-type) 0) (< (cadr element-type) unsigned-pvarp-limit))
		 (aref unsigned-pvarps (cadr element-type)))
		((or #+lucid *dont-use-closure-for-pvar-types*
		     (atom element-type) (null (cdr element-type)) (eql (cadr element-type) '*))
		 'unsigned-pvarp)
		(t `(lambda (object) (unsigned-pvarp object ,(cadr element-type))))))
	(boolean
	  'boolean-pvarp)
	(front-end
	  'front-end-pvarp)
	(character
	  'character-pvarp)
	(string-char
	  'string-char-pvarp)
	((t)
	 'general-pvarp)
	(single-float
	  'single-float-pvarp)
	(short-float
	  'short-float-pvarp)
	(double-float
	  'double-float-pvarp)
	(long-float
	  'long-float-pvarp)
	(extended-float
	  'extended-float-pvarp)
	(structure
	  (cond ((or (null (cadr element-type)) (eq (cadr element-type) '*))
		 'structure-pvarp)
		((get (cadr element-type) '*defstruct-global-predicate-function))
		#+lucid
		(*dont-use-closure-for-pvar-types*
		 'structure-pvarp)
		(t `(lambda (object) (structure-pvarp object ',(cadr element-type))))))
	(array
	  (if (or #+lucid *dont-use-closure-for-pvar-types*
		  (and (eq (cadr element-type) '*) (eq (caddr element-type) '*)))
	      'array-pvarp
	      `(lambda (object) (array-pvarp object ',(cadr element-type) ',(caddr element-type)))))
	(defined-float
	  (cond ((or (atom element-type)
		     (and (or (null (cdr element-type)) (eql (cadr element-type) '*))
			  (or (null (cddr element-type)) (eql (caddr element-type) '*))))
		 'float-pvarp)
		((and (eql (cadr element-type) short-float-mantissa) (eql (caddr element-type) short-float-exponent))
		 'short-float-pvarp)
		((and (eql (cadr element-type) single-float-mantissa) (eql (caddr element-type) single-float-exponent))
		 'single-float-pvarp)
		((and (eql (cadr element-type) double-float-mantissa) (eql (caddr element-type) double-float-exponent))
		 'double-float-pvarp)
		((and (eql (cadr element-type) long-float-mantissa) (eql (caddr element-type) long-float-exponent))
		 'long-float-pvarp)
		((and (eql (cadr element-type) extended-float-mantissa) (eql (caddr element-type) extended-float-exponent))
		 'extended-float-pvarp)
		#+lucid (*dont-use-closure-for-pvar-types* 'float-pvarp)
		(t `(lambda (object)
		      (float-pvarp object
				   ,(if (eq (cadr element-type) '*) ''* (cadr element-type))
				   ,(if (eq (caddr element-type) '*) ''* (caddr element-type)))))))
	(complex
	  (let ((element-type (cadr element-type)))
	    (case (if (consp element-type) (car element-type) element-type)
	      (single-float
		'single-complex-pvarp)
	      (short-float
		'short-complex-pvarp)
	      (double-float
		'double-complex-pvarp)
	      (long-float
		'long-complex-pvarp)
	      (extended-float
		'extended-complex-pvarp)
	      (defined-float
		(cond ((or (atom element-type)
			   (and (or (null (cdr element-type)) (eql (cadr element-type) '*))
				(or (null (cddr element-type)) (eql (caddr element-type) '*))))
		       'complex-pvarp)
		      ((and (eql (cadr element-type) short-float-mantissa) (eql (caddr element-type) short-float-exponent))
		       'short-complex-pvarp)
		      ((and (eql (cadr element-type) single-float-mantissa) (eql (caddr element-type) single-float-exponent))
		       'single-complex-pvarp)
		      ((and (eql (cadr element-type) double-float-mantissa) (eql (caddr element-type) double-float-exponent))
		       'double-complex-pvarp)
		      ((and (eql (cadr element-type) long-float-mantissa) (eql (caddr element-type) long-float-exponent))
		       'long-complex-pvarp)
		      ((and (eql (cadr element-type) extended-float-mantissa) (eql (caddr element-type) extended-float-exponent))
		       'extended-complex-pvarp)
		      #+lucid (*dont-use-closure-for-pvar-types* 'complex-pvarp)
		      (t `(lambda (object)
			    (complex-pvarp object
					   ,(if (eq (cadr element-type) '*) ''* (cadr element-type))
					   ,(if (eq (caddr element-type) '*) ''* (caddr element-type)))))))
	      (t nil))))
	(otherwise
	  #+lucid (if *dont-use-closure-for-pvar-types* 'pvarp `(lambda (object) (pvarp object ',element-type)))
	  #-lucid `(lambda (object) (pvarp object ',element-type))))))



(defun expand-pvar-element-type (element-type)
  #+lucid (declare (special lucid::*deftype-hashtable*))
  (let* ((type (if (consp element-type) (car element-type) element-type))
	 (expander #+lucid (gethash type lucid::*deftype-hashtable*) #-lucid (get type 'deftype)))
    (if expander
	#+lucid (funcall expander (if (consp element-type) element-type (list element-type)) nil) 
	#-lucid (apply expander (if (consp element-type) (cdr element-type) ()))
	nil)))
  

;;;; **** WARNING.  DANGER, DANGER WILL ROBINSON.  WARNING ****
;;;;
;;;; The logic of this code is duplicated in valid-pvar-type-p.
;;;; If you change something here change it there too!

(defun canonical-pvar-type (pvar-type)
  (if (or (eql pvar-type 'pvar)
	  (equal pvar-type '(pvar))
	  (equal pvar-type '(pvar *)))
      '(pvar *)
      (if (not (and (consp pvar-type) (eql (car pvar-type) 'pvar)))
	  (if pvar-type
	      (canonical-pvar-type (expand-deftype pvar-type)))
	  (let* ((element-type (cadr pvar-type))
		 (first (if (consp element-type) (car element-type) element-type))
		 (rest  (if (consp element-type) (cdr element-type) ())))
	    (case first
	      ((t)
	       '(pvar t))
	      (boolean
		'(pvar boolean))
	      (null
		'(pvar boolean))
	      (front-end
		'(pvar front-end))
	      (member
		(if (or (equal element-type '(member nil t))
			(equal element-type '(member t nil)))
		    `(pvar boolean)
		    nil))
	      (number
		'(pvar number))
	      (integer
		(let* ((low (cond ((null rest) '*)
				  ((consp (car rest)) (1+ (caar rest)))
				  (t (car rest))))
		       (high (cond ((null (cdr rest)) '*)
				   ((consp (cadr rest)) (1- (caadr rest)))
				   (t (cadr rest)))))
		  (cond ((eql low '*)
			 '(pvar (signed-byte *)))
			((>= low 0)
			 (if (eql high '*)
			     '(pvar (unsigned-byte *))
			     `(pvar (unsigned-byte ,(max 1 (integer-length high))))))
			(t (if (eql high '*)
			       '(pvar (signed-byte *))
			       `(pvar (signed-byte ,(max 2 (1+ (max (integer-length low) (integer-length high)))))))))))
	      (unsigned-byte
		(let ((length (if rest (macroexpand (car rest)) '*)))
		  (if (and (not (integerp length)) (constantp length)) (setq length (eval length)))
		  `(pvar (unsigned-byte ,length))))
	      (signed-byte
		(let ((length (if rest (macroexpand (car rest)) '*)))
		  (if (and (not (integerp length)) (constantp length)) (setq length (eval length)))
		  `(pvar (signed-byte ,length))))
	      (fixnum
		`(pvar (signed-byte ,(1+ (max (integer-length most-negative-fixnum) (integer-length most-positive-fixnum))))))
	      (bit
		`(pvar (unsigned-byte 1)))
	      (mod
		(if (or (null rest) (eql (car rest) '*))
		    '(pvar (unsigned-byte *))
		    `(pvar (unsigned-byte ,(max 1 (integer-length (1- (car rest))))))))
	      (float
		`(pvar (defined-float * *)))
	      (short-float
		`(pvar (defined-float #.short-float-mantissa #.short-float-exponent)))
	      (single-float
		`(pvar (defined-float #.single-float-mantissa #.single-float-exponent)))
	      (double-float
		`(pvar (defined-float #.double-float-mantissa #.double-float-exponent)))
	      (long-float
		`(pvar (defined-float #.long-float-mantissa #.long-float-exponent)))
	      (defined-float
		(let ((mantissa (if rest (car rest) '*)) (exponent (if (cdr rest) (cadr rest) '*)))
		  (if (and (not (integerp mantissa)) (constantp mantissa)) (setq mantissa (eval mantissa)))
		  (if (and (not (integerp exponent)) (constantp exponent)) (setq exponent (eval exponent)))
		  `(pvar (defined-float ,mantissa ,exponent))))
	      (complex
		(let ((canonical-element-type (or (cadr (canonical-pvar-type `(pvar ,@rest))) '*)))
		  (if (and (consp canonical-element-type)
			   (member (car canonical-element-type) '(unsigned-byte signed-byte)))
		      (setq canonical-element-type `(defined-float #.single-float-mantissa #.single-float-exponent)))
		  `(pvar (complex ,(if (or (eq canonical-element-type 't) (eq canonical-element-type '*))
				       '(defined-float * *)
				       canonical-element-type)))))
	      (string-char
		`(pvar string-char))
	      (standard-char
		'(pvar string-char))
	      (character
		`(pvar character))
	      ((array simple-array)
		(let ((canonical-element-type (or (cadr (canonical-pvar-type `(pvar ,(car rest)))) '*)))
		  `(pvar (array ,canonical-element-type
				,(cond ((equal (cdr rest) '(())) '())
				       ((integerp (cadr rest)) (make-list (cadr rest) :initial-element '*))
				       ((or (null (cadr rest)) (eq (cadr rest) '*)) '*)
				       ((constantp (cadr rest)) 
					(let ((dimensions (eval (cadr rest))))
					  (if (integerp dimensions)
					      (make-list dimensions :initial-element '*)
					      dimensions)))
				       ((atom (cadr rest)) (cadr rest))
				       (t (mapcar #'(lambda (dim) (if (constantp dim) (eval dim) dim)) (cadr rest))))))))
	      (vector
		(let ((canonical-element-type (or (cadr (canonical-pvar-type `(pvar ,(car rest)))) '*)))
		  `(pvar (array ,canonical-element-type
				(,(if (or (null (cadr rest)) (eq (cadr rest) '*))
				      '* 
				      (if (constantp (cadr rest)) (eval (cadr rest)) (cadr rest))))))))
	      (simple-vector
		(canonical-pvar-type `(pvar (vector t ,(or (cadr rest) '*)))))
	      ((bit-vector simple-bit-vector)
	       `(pvar (array (unsigned-byte 1) (,(or (if (constantp (car rest)) (eval (car rest)) (car rest)) '*)))))
	      ((string simple-string)
	       `(pvar (array string-char (,(or (if (constantp (car rest)) (eval (car rest)) (car rest)) '*)))))
	      (structure
		(let ((name (or (car rest) '*)))
		  (unless (or (eq name '*) (and (symbolp name) (get name '*defstruct-structure)))
		    (warn "~S is not a known structure type." name))
		  `(pvar (structure ,name))))
	      (ratio `(pvar (defined-float #.single-float-mantissa #.single-float-exponent)))
	      (otherwise
		(if (and (symbolp element-type) (get element-type '*defstruct-structure))
		    `(pvar (structure ,element-type))
		    (canonical-pvar-type `(pvar ,(or (expand-pvar-element-type element-type)
						     (return-from canonical-pvar-type nil)))))))))))

(proclaim '(special nbits-per-lisp))

(defun length-pvar-type (type &optional (eval t))
  (declare (special *SIM::*char-code-length *SIM::*character-length))
  (if (and (consp type) (eql (car type) 'pvar))
      (let ((element (cadr type)))
	(case (if (consp element) (car element) element)
	  ((t)
	   `*)
	  (boolean
	    1)
	  (front-end
	    (if eval nbits-per-lisp 'nbits-per-lisp))
	  (number
	    '*)
	  ((unsigned-byte signed-byte)
	   (or (cadr element) '*))
	  (short-float
	    '#.(+ 1 short-float-mantissa short-float-exponent))
	  (single-float
	    '#.(+ 1 single-float-mantissa single-float-exponent))
	  (double-float
	    '#.(+ 1 double-float-mantissa double-float-exponent))
	  (long-float
	    '#.(+ 1 long-float-mantissa long-float-exponent))
	  (defined-float
	    (cond ((and (consp element)
			(integerp (cadr element))
			(integerp (caddr element)))
		   (+ 1 (cadr element) (caddr element)))
		  ((and (consp element)
			(cadr element) (caddr element)
			(not (eq (cadr element) '*))
			(not (eq (caddr element) '*)))
		   `(+ 1 ,(cadr element) ,(caddr element)))
		  (t '*)))
	  (complex
	   (let* ((element (cadr element))
		  (length (cond ((and (consp element)
				      (integerp (cadr element))
				      (integerp (caddr element)))
				 (+ 1 (cadr element) (caddr element)))
				((and (consp element)
				      (cadr element) (caddr element)
				      (not (eq (cadr element) '*))
				      (not (eq (caddr element) '*)))
				 `(+ 1 ,(cadr element) ,(caddr element)))
				(t '*))))
	     (if (eq length '*)
		 '*
		 (if (integerp length)
		     (* length 2)
		     `(* ,length 2)))))
	  (structure
	    (if (equal type '(pvar (structure *)))
		'*
		(funcall 'structure-pvar-type-total-length-in-bits type)))
	  (array (length-pvar-type-for-array type eval))
	  (string-char (if eval *SIM::*char-code-length '*SIM::*char-code-length))
	  (character (if eval *SIM::*character-length '*SIM::*character-length))
	  (otherwise (length-pvar-type `(pvar ,(or (expand-pvar-element-type element)
					   (return-from length-pvar-type '*)))))))
      (if type
	  (length-pvar-type (canonical-pvar-type type)))))

(defun length-pvar-type-for-array (type &optional (eval t))
  (let ((array-element-type-length (length-pvar-type `(pvar ,(array-pvar-type-element-type type)) eval))
	(array-dimensions (array-pvar-type-dimensions type)))
    (cond ((eq '* array-element-type-length) '*)
	  ((numberp array-dimensions) '*)
	  ((eq array-dimensions '*) '*)
	  ((atom array-dimensions)
	   (if (and (null array-dimensions) (cddadr type))
	       array-element-type-length
	       `(* ,array-element-type-length (apply '* ,array-dimensions))))
	  ((find '* array-dimensions) '*)
	  (t
	   (values (apply 'simplify-expression '* array-element-type-length array-dimensions))))))

;;This will return one of boolean, unsigned-byte, signed-byte, defined-float,
;; complex, string-char, character, array, structure, t, front-end, or *.
(defun canonical-pvar-element-type (type)
  (unless (and (consp type) (eq (car type) 'pvar))
    (error "~S is not a canonical pvar type." type))
  (if (consp (cadr type))
      (caadr type)
      (cadr type)))

(defun array-pvar-type-p (type)
  (unless (and (consp type) (eq (car type) 'pvar))
    (error "~S is not a canonical pvar type." type))
  (and (consp (cadr type)) (eq (caadr type) 'array)))

(defun structure-pvar-type-p (type)
  (unless (and (consp type) (eq (car type) 'pvar))
    (error "~S is not a canonical pvar type." type))
  (and (consp (cadr type)) (eq (caadr type) 'structure)))

(defun array-pvar-type-dimensions (type)
  (unless (array-pvar-type-p type)
    (error "~S is not a canonical pvar array type." type))
  (caddr (cadr type)))

(defun array-pvar-type-element-type (type)
  (unless (array-pvar-type-p type)
    (error "~S is not a canonical pvar array type." type))
  (cadr (cadr type)))

(defun structure-pvar-type-name (type)
  (unless (structure-pvar-type-p type)
    (error "~S is not a canonical pvar array type." type))
  (cadr (cadr type)))

(defun float-pvar-type-p (type)
  (unless (and (consp type) (eq (car type) 'pvar))
    (error "~S is not a canonical pvar type." type))
  (and (consp (cadr type)) (eq (caadr type) 'defined-float)))

(defun complex-pvar-type-p (type)
  (unless (and (consp type) (eq (car type) 'pvar))
    (error "~S is not a canonical pvar type." type))
  (and (consp (cadr type)) (eq (caadr type) 'complex)))

(defun boolean-pvar-type-p (type)
  (unless (and (consp type) (eq (car type) 'pvar))
    (error "~S is not a canonical pvar type." type))
  (eq (cadr type) 'boolean))

(defun front-end-pvar-type-p (type)
  (unless (and (consp type) (eq (car type) 'pvar))
    (error "~S is not a canonical pvar type." type))
  (eq (cadr type) 'front-end))

(defun general-pvar-type-p (type)
  (unless (and (consp type) (eq (car type) 'pvar))
    (error "~S is not a canonical pvar type." type))
  (eq (cadr type) 't))

(defun string-char-pvar-type-p (type)
  (unless (and (consp type) (eq (car type) 'pvar))
    (error "~S is not a canonical pvar type." type))
  (eq (cadr type) 'string-char))

(defun character-pvar-type-p (type)
  (unless (and (consp type) (eq (car type) 'pvar))
    (error "~S is not a canonical pvar type." type))
  (eq (cadr type) 'character))

(defun signed-pvar-type-p (type)
  (unless (and (consp type) (eq (car type) 'pvar))
    (error "~S is not a canonical pvar type." type))
  (and (consp (cadr type)) (eq (caadr type) 'signed-byte)))

(defun unsigned-pvar-type-p (type)
  (unless (and (consp type) (eq (car type) 'pvar))
    (error "~S is not a canonical pvar type." type))
  (and (consp (cadr type)) (eq (caadr type) 'unsigned-byte)))

(defun float-pvar-type-mantissa (type)
  (unless (float-pvar-type-p type)
    (error "~S is not a canonical pvar float type." type))
  (cadr (cadr type)))

(defun float-pvar-type-exponent (type)
  (unless (float-pvar-type-p type)
    (error "~S is not a canonical pvar float type." type))
  (caddr (cadr type)))

(defun complex-pvar-type-mantissa (type)
  (unless (complex-pvar-type-p type)
    (error "~S is not a canonical pvar complex type." type))
  (cadr (cadadr type)))

(defun complex-pvar-type-exponent (type)
  (unless (complex-pvar-type-p type)
    (error "~S is not a canonical pvar complex type." type))
  (caddr (cadadr type)))

;; Returns a type given all that stuff.
(defun make-canonical-pvar-type
    (pvar-element-type &key length mantissa exponent (dimensions nil dimp) element-type name)
  (case pvar-element-type
    ((boolean string-char character * t)
     (unless (and (null length) (null mantissa) (null exponent) (null dimensions) (null element-type) (null name))
       (error "Invalid keyword argument." ))
     `(pvar ,pvar-element-type))
    ((unsigned-byte signed-byte)
     (unless (and (null mantissa) (null exponent) (null dimensions) (null element-type) (null name))
       (error "Invalid keyword argument." ))
     `(pvar (,pvar-element-type ,(or length '*))))
    (defined-float
      (unless (and (null length) (null dimensions) (null element-type) (null name))
	(error "Invalid keyword argument." ))
      `(pvar (defined-float ,(or mantissa '*) ,(or exponent '*))))
    (complex
      (unless (and (null length) (null dimensions) (null element-type) (null name))
	(error "Invalid keyword argument." ))
      `(pvar (complex (defined-float ,(or mantissa '*) ,(or exponent '*)))))
    ((array simple-array)
     (unless (and (null length) (null mantissa) (null exponent) (null name))
       (error "Invalid keyword argument." ))
     `(pvar (array ,(or element-type '*) ,(if dimp dimensions '*))))
    (structure
      (unless (and (null length) (null mantissa) (null exponent) (null dimensions) (null element-type))
	(error "Invalid keyword argument." ))
      `(pvar (structure ,(or name '*))))
    (otherwise (error "Invalid pvar element-type ~S." element-type))))


(defun non-lexical-canonical-pvar-type-with-numeric-lengths-from-canonical-pvar-type (cpt)
  (flet
    ((eval-not* (x) (if (eq x '*) '* (eval x))))
    (let ((cpet (canonical-pvar-element-type cpt)))
      (case cpet
	(boolean cpt)
	(front-end cpt)
	((unsigned-byte signed-byte)
	  (let ((length (cadr (cadr cpt))))
	    (make-canonical-pvar-type cpet :length (eval-not* length))
	    ))
	(defined-float
	  (let ((mantissa (cadr (cadr cpt)))
		(exponent (caddr (cadr cpt))))
	    (if (and (or (integerp mantissa) (eq mantissa '*))
		     (or (integerp exponent) (eq exponent '*)))
		cpt
		(make-canonical-pvar-type cpet :mantissa (eval-not* mantissa) :exponent (eval-not* exponent))
		)))
	(complex
	  (let ((mantissa (cadr (cadadr cpt)))
		(exponent (caddr (cadadr cpt))))
	    (if (and (or (integerp mantissa) (eq mantissa '*))
		     (or (integerp exponent) (eq exponent '*)))
		cpt
		(make-canonical-pvar-type cpet :mantissa (eval-not* mantissa) :exponent (eval-not* exponent))
		)))
	(structure cpt)
	(string-char cpt)
	(character cpt)
	(array
	  (let ((dimensions (array-pvar-type-dimensions cpt))
		(element-type (array-pvar-type-element-type cpt)))
	    (setq dimensions
		  (cond
		    ((symbolp dimensions) (eval dimensions))
		    ((listp dimensions) (mapcar #'eval-not* dimensions))
		    ))
	    (make-canonical-pvar-type
	      'array
	      :dimensions dimensions
	      :element-type
	      (cadr (non-lexical-canonical-pvar-type-with-numeric-lengths-from-canonical-pvar-type `(pvar ,element-type)))
	      )))
	((t) cpt)
	(* cpt)
	(otherwise (error "Unknown pvar type ~S, probably internal error." cpt))
	))))


(DEFMACRO PROCLAIM-*DEFUN (FUNCTION-NAME)
  `(EVAL-WHEN (LOAD EVAL COMPILE)
     (funcall 'PROCLAIM-*DEFUN-1 ',FUNCTION-NAME)))

(defun make-*defun-function (function-name)
  (let ((new-function-name (let ((*print-case* :upcase)) (format nil "*DEFUN-~A" function-name))))
    (intern new-function-name (symbol-package function-name))))

(defmacro get-*defun-function (function-name)
  `(get ,function-name '*lisp-defun))

(defmacro *trace (&rest *defun-function-names)
  `(trace ,@(mapcar 'make-*defun-function *defun-function-names))
  )

(defmacro *untrace (&rest *defun-function-names)
  `(untrace ,@(mapcar 'make-*defun-function *defun-function-names))
  )

(defvar *warn-about-integer-dimension* t)
(defvar *warn-about-array-rank* t)


(defun pvar-type-syntax-error (form reason error)
  (if error
      (error "Invalid pvar type ~S.  Reason: ~A." form reason)
      (throw 'invalid-pvar-type nil)
      ))


(defun vicious-dotted-pair-catcher-for-jeff (list)
  (cond
   ((null list) nil)
   ((not (listp (cdr list))) t)
   (t (or (and (listp (car list)) (vicious-dotted-pair-catcher-for-jeff (car list)))
          (vicious-dotted-pair-catcher-for-jeff (cdr list))
          ))))

;;;; **** WARNING.  DANGER, DANGER WILL ROBINSON.  WARNING ****
;;;;
;;;; The logic of this code is duplicated in canonical-pvar-type.
;;;; If you change something here change it there too!


(defun valid-pvar-type-p (pvar-type &optional (error t))
  
  (catch 
   'invalid-pvar-type
   
   (macrolet
       ((syntax-error (reason) `(pvar-type-syntax-error pvar-type ,reason error)))
     
     (labels
         ((evaluatable-expression-p (expression)
                                    (and expression (or (symbolp expression) (listp expression)))
                                    )
          (valid-form-p (expression predicate)
                        (or (funcall predicate expression) (evaluatable-expression-p expression))
                        )
          (check-rest (pvar-type rest min max &optional (reason nil reason-provided))
                      (when (> (length rest) max)
                        (syntax-error (if reason-provided reason "Too many arguments")))
                      (when (< (length rest) min)
                        (syntax-error (if reason-provided reason "Too few arguments")))
                      ))
       
       
       (when (or (eql pvar-type 'pvar) (equal pvar-type '(pvar)) (equal pvar-type '(pvar *)))
         (return-from valid-pvar-type-p '(pvar *))
         )
       
       ;; If it is something other than (pvar ...)
       ;; check whether it can be expanded via deftype.  If so,
       ;; recurse, otherwise it isn't a valid pvar type.
       
       (when (not (and (consp pvar-type) (eql (car pvar-type) 'pvar)))
         (let ((deftype-expansion (expand-deftype pvar-type)))
           (return-from valid-pvar-type-p
             (if (null deftype-expansion)
                 (syntax-error "The form is not expandable via DEFTYPE and is not a known pvar type")
               (valid-pvar-type-p (expand-deftype pvar-type) error)
               ))))
       
       (when (vicious-dotted-pair-catcher-for-jeff pvar-type)
         (syntax-error "The form has a dotted pair in it somewhere.  Types cannot have dotted pairs.")
         )
       
       (let* ((element-type (cadr pvar-type))
              (data-type (if (consp element-type) (car element-type) element-type))
              (data-type-arguments (if (consp element-type) (cdr element-type) ())))
         
         (when (> (length pvar-type) 2)
           (syntax-error
            "Illegal syntax.  You probably misplaced a parenthesis.  Use (pvar <type>), not (pvar <type> <something-else>)"
            ))
         
         (case data-type
           
           ;; (pvar t), (pvar boolean) (pvar front-end)
           
           ((t) (check-rest pvar-type data-type-arguments 0 0) '(pvar t))
           (boolean (check-rest pvar-type data-type-arguments 0 0) '(pvar boolean))
           (front-end (check-rest pvar-type data-type-arguments 0 0) '(pvar front-end))
           
           ;; (pvar (member t nil))
           
           (member
            (if (or (equal element-type '(member nil t))
                    (equal element-type '(member t nil)))
                `(pvar boolean)
              (syntax-error "Starlisp cannot handle MEMBER type specifiers except for (MEMBER T NIL).  Sorry")
              ))
           
           ;; (pvar (integer <low> <high>))
           
           (integer
            (check-rest pvar-type data-type-arguments 0 2)
            (integer-valid-pvar-type-p pvar-type data-type-arguments error)
            )
           
           ;; (pvar (unsigned-byte <length>))
           
           (unsigned-byte
            (check-rest pvar-type data-type-arguments 0 1 "Only 1 argument (a length) can be provided for unsigned pvar types")
            (let ((data-type-arguments (if data-type-arguments (macroexpand (car data-type-arguments)) '*)))
              (if (valid-form-p data-type-arguments #'(lambda (x) (and (integerp x) (plusp x))))
                  `(pvar (unsigned-byte ,data-type-arguments))
                (syntax-error "The length argument cannot possibly evaluate to a positive integer")
                )))
           
           ;; (pvar (signed-byte <length>))
           
           (signed-byte
            (check-rest pvar-type data-type-arguments 0 1 "Only 1 argument (a length) can be provided for signed pvar types")
            (let ((data-type-arguments (if data-type-arguments (macroexpand (car data-type-arguments)) '*)))
              (if (valid-form-p data-type-arguments #'(lambda (x) (and (integerp x) (> x 1))))
                  `(pvar (signed-byte ,data-type-arguments))
                (syntax-error "The length argument cannot possibly evaluate to a positive integer >= 2")
                )))
           
           (fixnum
            (check-rest pvar-type data-type-arguments 0 0 "The FIXNUM specification does not allow any arguments")
            `(pvar (signed-byte ,(1+ (max (integer-length most-negative-fixnum) (integer-length most-positive-fixnum))))))
           
           (bit
            (check-rest pvar-type data-type-arguments 0 0 "The BIT specification does not allow any arguments")
            `(pvar (unsigned-byte 1)))
           
           ;; (pvar (mod <modulus>))
           
           (mod
            (check-rest pvar-type data-type-arguments 1 1)
            (let ((data-type-arguments (macroexpand (car data-type-arguments))))
              (if (valid-form-p data-type-arguments #'plusp)
                  (if (eq data-type-arguments '*)
                      '(pvar (unsigned-byte *))
                    (if (integerp data-type-arguments)
                        `(pvar (unsigned-byte ,(max 1 (integer-length (1- data-type-arguments)))))
                      `(pvar (unsigned-byte (max 1 (integer-length (1- ,data-type-arguments)))))
                      ))
                (syntax-error "Mod's argument cannot possibly evaluate to a positive integer")
                )))
           
           ;; (pvar (float <mantissa> <exponent>))
           
           (float
            (check-rest pvar-type data-type-arguments 0 2)
            (if (null data-type-arguments)
                `(pvar (defined-float * *))
              (valid-pvar-type-p `(pvar (defined-float ,@data-type-arguments)) error)
              ))
           
           (short-float
            (check-rest pvar-type data-type-arguments 0 0 "The SHORT-FLOAT specification does not allow any arguments")
            `(pvar (defined-float ,short-float-mantissa ,short-float-exponent)))
           (single-float
            (check-rest pvar-type data-type-arguments 0 0 "The SINGLE-FLOAT specification does not allow any arguments")
            `(pvar (defined-float ,single-float-mantissa ,single-float-exponent)))
           (double-float
            (check-rest pvar-type data-type-arguments 0 0 "The DOUBLE-FLOAT specification does not allow any arguments")
            `(pvar (defined-float ,double-float-mantissa ,double-float-exponent)))
           (long-float
            (check-rest pvar-type data-type-arguments 0 0 "The LONG-FLOAT specification does not allow any arguments")
            `(pvar (defined-float ,long-float-mantissa ,long-float-exponent)))
           
           ;; (pvar (defined-float <mantissa> <exponent>))
           
           (defined-float
               (check-rest pvar-type data-type-arguments 0 2
                           "Only a mantissa and exponent may be provided for 'defined-float'"
                           )
               (let ((mantissa (macroexpand (if data-type-arguments (first data-type-arguments) '*)))
                     (exponent (macroexpand (if (cdr data-type-arguments) (second data-type-arguments) '*))))
                 (if (not (valid-form-p mantissa #'(lambda (x) (and (integerp x) (plusp x)))))
                     (syntax-error "The mantissa expression cannot possibly evaluate to a positive integer"))
                 (if (not (valid-form-p exponent #'(lambda (x) (and (integerp x) (plusp x)))))
                     (syntax-error "The exponent expression cannot possibly evaluate to a positive integer"))
                 `(pvar (defined-float ,mantissa ,exponent))
                 ))
           
           ;; (pvar (complex (defined-float <mantissa> <exponent>)))
           
           (complex
            (check-rest pvar-type data-type-arguments 0 1
                        "Either no argument or one argument (a floating point type specifier) is required after 'complex'"
                        )
            (if (null data-type-arguments)
                '(pvar (complex (defined-float * *)))
              (let ((canonical-element-type (cadr (valid-pvar-type-p `(pvar ,@data-type-arguments)))))
                (if (and (not (eq '* canonical-element-type))
                         (or (symbolp canonical-element-type) (not (eq 'defined-float (car canonical-element-type))))
                         )
                    (syntax-error "The complex pvar type currently requires a floating point component type")
                  (if (eq '* canonical-element-type)
                      `(pvar (complex (defined-float * *)))
                    `(pvar (complex ,canonical-element-type))
                    )
                  ))))
           
           (string-char
            (check-rest pvar-type data-type-arguments 0 0)
            `(pvar string-char)
            )
           (standard-char
            (check-rest pvar-type data-type-arguments 0 0)
            '(pvar string-char)
            )
           (character
            (check-rest pvar-type data-type-arguments 0 0)
            `(pvar character)
            )
           
           ;; (pvar (string <length>))
           
           ((string simple-string)
            (check-rest pvar-type data-type-arguments 0 1)
            (if (null data-type-arguments)
                `(pvar (array string-char (*)))
              (progn
                (if (valid-form-p (car data-type-arguments) #'(lambda (x) (and (integerp x) (not (minusp x)))))
                    (if (and (listp (car data-type-arguments)) (integerp (caar data-type-arguments)))
                        (syntax-error "The length argument must be a single integer, not a list of integers")
                      `(pvar (array string-char ,data-type-arguments))
                      )
                  (syntax-error "The length argument cannot possibly evaluate to a non-negative integer")
                  ))))
           
           ;; (pvar (bit-vector <length>))
           
           ((bit-vector simple-bit-vector)
            (check-rest pvar-type data-type-arguments 0 1)
            (if (null data-type-arguments)
                `(pvar (array (unsigned-byte 1) (*)))
              (progn
                (if (valid-form-p (car data-type-arguments) #'(lambda (x) (and (integerp x) (not (minusp x)))))
                    (if (and (listp (car data-type-arguments)) (integerp (caar data-type-arguments)))
                        (syntax-error "The length argument must be a single integer, not a list of integers")
                      `(pvar (array (unsigned-byte 1) ,data-type-arguments))
                      )
                  (syntax-error "The length argument cannot possibly evaluate to a non-negative integer")
                  ))))
           
           ;; (pvar (array <element-type> <dimensions>))
           
           ((array simple-array)
            (check-rest pvar-type data-type-arguments 0 2)
            (array-valid-pvar-type-p pvar-type data-type-arguments error)
            )
           
           ;; (pvar (vector <element-type> <length>))
           
           ((vector simple-vector)
            (check-rest pvar-type data-type-arguments 0 2)
            (vector-valid-pvar-type-p pvar-type data-type-arguments error)
            )
           
           ;; (pvar (structure foo))
           
           (structure
            (check-rest pvar-type data-type-arguments 1 1)
            (let ((name (car data-type-arguments)))
              (unless (symbolp name)
                (syntax-error "The structure type designator must be a symbol"))
              (unless (get name '*defstruct-structure)
                (warn "~S is not a known structure type." name))
              `(pvar (structure ,name))
              ))
           
           ;; (pvar foo)
           
           (otherwise
            (if (and (symbolp element-type) (get element-type '*defstruct-structure))
                `(pvar (structure ,element-type))
              (let ((expansion (expand-pvar-element-type element-type)))
                (when (null expansion)
                  (return-from valid-pvar-type-p
                    (syntax-error "The type expression does not correspond to nor is expandable to any known pvar type")
                    ))
                (valid-pvar-type-p `(pvar ,expansion) error)
                )))
           
           ))))))



(defun integer-valid-pvar-type-p (pvar-type data-type-arguments error)
  (macrolet
    ((syntax-error (reason) `(pvar-type-syntax-error pvar-type ,reason error)))
    (cond
      ((null data-type-arguments) '(pvar (signed-byte *)))
      (t
       (let ((low (first data-type-arguments)) (high (second data-type-arguments)))
	 (setq low
	       (cond
		 ((or (integerp low) (eq low '*)) low)
		 ((listp low)
		  (if (or (not (eql 1 (length low))) (not (integerp (car low))))
		      (syntax-error "The first argument must be a one element list containing an integer")
		      (1+ (car low))
		      ))
		 (t (syntax-error "Starlisp cannot handle arguments to the INTEGER type which are not integers"))
		 ))
	 (setq high
	       (cond
		 ((and (null high) (eql 1 (length data-type-arguments))) '*)
		 ((or (integerp high) (eq high '*)) high)
		 ((listp high)
		  (if (or (not (eql 1 (length high))) (not (integerp (car high))))
		      (syntax-error "The second argument must be a one element list containing an integer")
		      (1- (car high))
		      ))
		 (t (syntax-error "Starlisp cannot handle arguments to the INTEGER type which are not integers"))
		 ))
	 (when (and (integerp low) (integerp high) (< high low))
	   (syntax-error "The minimum of the range specified is greater than the maximum!"))
	 (cond ((eql low '*)
		'(pvar (signed-byte *)))
	       ((>= low 0)
		(if (eql high '*)
		    '(pvar (unsigned-byte *))
		    `(pvar (unsigned-byte ,(max 1 (integer-length high))))))
	       (t
		(if (eql high '*)
		    '(pvar (signed-byte *))
		    `(pvar (signed-byte ,(max 2 (1+ (max (integer-length low) (integer-length high))))))
		    ))))))))



(defun array-valid-pvar-type-p (pvar-type data-type-arguments error)
  (macrolet
    ((syntax-error (reason) `(pvar-type-syntax-error pvar-type ,reason error)))
    (labels
      ((evaluatable-expression-p (expression)
	 (and expression (or (symbolp expression) (listp expression)))
	 )
       (valid-form-p (expression predicate)
	 (or (funcall predicate expression) (evaluatable-expression-p expression))
	 ))
      (when (and (listp (car data-type-arguments))
		 (integerp (car (car data-type-arguments)))
		 )
	(syntax-error
	  "The array element type is a list beginning with a number.  Perhaps you reversed the element type and dimensions"
	  ))
      (if (null data-type-arguments)
	  `(pvar (array * *))
	  (let ((canonical-element-type (cadr (valid-pvar-type-p `(pvar ,(car data-type-arguments)) error))))
	    (cond
;	      ((and (eq '* (length-pvar-type `(pvar ,canonical-element-type))) (not (eq '* canonical-element-type)))
;	       (syntax-error "Starlisp does not currently allow arrays of varying length pvars.  Sorry"))
	      ((null canonical-element-type)
	       (syntax-error "The element type is not a recognized legal element pvar type"))
	      (t
	       (if (eql 1 (length data-type-arguments))
		   `(pvar (array ,canonical-element-type *))
		   (let ((dimensions (second data-type-arguments)))
		     (cond
		       ((and (integerp dimensions) (not (minusp dimensions)))
			(if *warn-about-integer-dimension*
			    (warn "I'll bet you meant ~S, a 1-dimensional array ~D long,~%~@
                                   instead of ~S, a ~D dimensional array of indeterminate size.~%~@
                                   To turn off this warning do ~%~@
                                   (setq ~S::*warn-about-integer-dimension* nil)."
				  `(pvar (array ,canonical-element-type (,dimensions)))
				  dimensions
				  `(pvar (array ,canonical-element-type ,dimensions))
				  dimensions
				  *starlisp-internal-package-name*
				  ))
			(when (minusp dimensions)
			  (syntax-error "A negative number of dimensions makes no sense."))
			`(pvar (array ,canonical-element-type ,(make-list dimensions :initial-element '*)))
			)
		       ((symbolp dimensions)
			`(pvar (array ,canonical-element-type ,dimensions)))
		       ((listp dimensions)
			(when (eq (car dimensions) 'quote)
			  (syntax-error "The dimensions list should not be quoted.")
			  )
			(if (every #'(lambda (x) (valid-form-p x #'(lambda (y) (and (integerp y) (not (minusp y))))))
				   dimensions
				   )
			    (progn
			      (when (every #'integerp dimensions)
				(when (>= (apply #'* dimensions) *array-total-size-limit)
				  (warn "You are declaring an array of size ~D elements or greater.  This is non-portable."
					*array-total-size-limit
					))
				(when (>= (length dimensions) *array-rank-limit)
				  (when *warn-about-array-rank*
				    (warn "You are declaring an array of rank ~D or greater.~%~@
                                           Common Lisp only portably allows arrays of rank 7 or less.~%~@
                                           You will not be able to read your arrays out of the CM in a ~%~@
                                           straightforward manner.  To turn off this warning do ~%~@
                                           (setq ~S::*warn-about-array-rank* nil)"
					  *array-rank-limit *starlisp-internal-package-name*
					  ))))
			      (when (some #'(lambda (x) (and (integerp x) (>= x *array-dimension-limit))) dimensions)
				(warn "You are declaring an array which has a dimension of size ~D elements or greater.  This
is non-portable."
				      *array-dimension-limit
				      ))
			      `(pvar (array ,canonical-element-type ,dimensions))
			      )
			      (syntax-error
				"Every component of the dimensions list will not evaluate to a non-negative integer")))
			(t (syntax-error
			     "The dimensions argument is unrecognizable as a potential list of non-negative integers"))
			))))
	       ))))))


(defun vector-valid-pvar-type-p (pvar-type data-type-arguments error)
  (macrolet
    ((syntax-error (reason) `(pvar-type-syntax-error pvar-type ,reason error)))
    (if (null data-type-arguments)
	'(pvar (array * (*)))
	(let ((canonical-element-type (cadr (canonical-pvar-type `(pvar ,(car data-type-arguments))))))
	  (cond
	    ((and (eq '* (length-pvar-type `(pvar ,canonical-element-type))) (not (eq '* canonical-element-type)))
	     (syntax-error "Starlisp does not currently allow arrays of varying length pvars.  Sorry"))
	    ((null canonical-element-type)
	     (syntax-error "The element type is not a recognized legal element pvar type"))
	    (t
	     (if (eql 1 (length data-type-arguments))
		 `(pvar (array ,canonical-element-type (*)))
		 (let ((length (second data-type-arguments)))
		   (cond
		     ((integerp length) 
		      (when (minusp length)
			(syntax-error "A negative length makes no sense."))
		      (when (>= length *array-dimension-limit)
			(warn "You are declaring a vector which has a length of ~D elements or greater.  This is non-portable"
			      *array-dimension-limit
			      ))
		      `(pvar (array ,canonical-element-type (,length)))
		      )
		     ((and length (symbolp length))
		      `(pvar (array ,canonical-element-type (,length))))
		     ((and length (listp length))
		      (if (integerp (car length))
			  (syntax-error "The length argument must be a single integer, not a list of integers")
			  `(pvar (array ,canonical-element-type (,length)))
			  ))
		     (t (syntax-error "The length argument is unrecognizable as a possible non-negative integer"))
		     )))))))))


(deftype defined-float (&optional mantissa exponent)
  (declare (ignore mantissa exponent))
  'float)

#+symbolics
(si:allow-redefinition 'pvar 'deftype)
(deftype pvar (&optional (element-type '*))
  ;; I have to return a satisfies type with a closure so that typep can work.
  ;; But, returning a closure will blow up both subtypep and the compiler on lucid.
  (let ((closure (pvar-type-predicate (cadr (canonical-pvar-type `(pvar ,element-type))))))
    `(satisfies ,closure)))



(deftype boolean-pvar ()
  `(pvar boolean))

(deftype signed-pvar (&optional width)
  `(pvar (signed-byte ,width)))

(deftype signed-byte-pvar (&optional width)
  `(pvar (signed-byte ,width)))

(deftype field-pvar (&optional width)
  `(pvar (unsigned-byte ,width)))

(deftype unsigned-pvar (&optional width)
  `(pvar (unsigned-byte ,width)))

(deftype unsigned-byte-pvar (&optional width)
  `(pvar (unsigned-byte ,width)))

(deftype float-pvar (&optional (mantissa '*) (exponent '*))
  `(pvar (defined-float ,mantissa ,exponent)))

(deftype short-float-pvar ()
  `(pvar short-float))

(deftype single-float-pvar ()
  `(pvar single-float))

(deftype double-float-pvar ()
  `(pvar double-float))

(deftype long-float-pvar ()
  `(pvar long-float))

(deftype extended-float ()
  `(defined-float ,extended-float-mantissa ,extended-float-exponent))

(deftype character-pvar ()
  `(pvar character))

(deftype string-char-pvar ()
  `(pvar string-char))

(deftype complex-pvar (&optional (mantissa '*) (exponent '*))
  `(pvar (complex (defined-float ,mantissa ,exponent))))

(deftype short-complex-pvar ()
  `(pvar (complex short-float)))

(deftype single-complex-pvar ()
  `(pvar (complex single-float)))

(deftype double-complex-pvar ()
  `(pvar (complex double-float)))

(deftype long-complex-pvar ()
  `(pvar (complex long-float)))

(deftype array-pvar (&optional (element-type '*) (dimensions '*))
  `(pvar (array ,element-type ,dimensions)))

(deftype vector-pvar (&optional (element-type '*) (length '*))
  `(pvar (array ,element-type (,length))))

(deftype string-pvar (&optional (length '*))
  `(pvar (array string-char (,length)))
  )

(deftype bit-vector-pvar (&optional (length '*))
  `(pvar (array (unsigned-byte 1) (,length)))
  )

(deftype general-pvar ()
  `(pvar t))

(deftype front-end ()
  't)

(deftype front-end-pvar ()
  `(pvar front-end)
  )
