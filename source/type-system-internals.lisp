;;; -*- SYNTAX: COMMON-LISP; MODE: LISP; BASE: 10; PACKAGE: *SIM-I; MUSER: YES -*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


(defun expand-one-deftype (type)
  (if (atom type) (setq type (list type)))
  (let ((expander (get (first type) :*lisp-deftype)))
    (if (null expander)
	nil
      (apply expander (rest type))
      )))

(defun parse-body-macroexpand-1 (form env)
  (let ((*compiling* nil) (*compilep* nil))
    (macroexpand-1 form env)
    ))

(defun proclaimed-type (symbol &optional (default-type 't))
  (get symbol :*lisp-type default-type))

(defun proclaimed-ftype (symbol &optional lexical-only-p)
  (or (cdr (assoc symbol (list-of-slc-function-types)))
      (and (not lexical-only-p) (get-function-type symbol))))

(defun proclaimed-declaration-p (symbol)
  (get symbol :*lisp-declaration))

(defun proclaimed-special-p (symbol)
  (get symbol :*lisp-special))


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
           (set-function-type function `(function ,argument-types ,return-type)))
         (setq decl-spec
               `(ftype (function ,(mapcar #'(lambda (type) (if (and (consp type) (eq (car type) 'pvar)) 'pvar type))
                                    argument-types)
;; 04/09/03 changed return-type to (or return-type t)
                                 ,(if (and (consp return-type) (eq (car return-type) 'pvar)) 'pvar (or return-type t)))
                       ,@(cddr decl-spec))))))

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

(defun starlisp-unproclaim (decl-spec)
  
  (case (car decl-spec)
    
    (special
     (dolist (variable (cdr decl-spec))
       (remprop variable ':*lisp-special)))
    
    (ftype
     (dolist (function (cddr decl-spec))
       (remove-function-type function)))
    
    (function
     (if (and (cddr decl-spec) (listp (caddr decl-spec)))
         (remprop (cadr decl-spec) 'function-type)
       (dolist (variable (cdr decl-spec))
         (remprop variable :*lisp-type))))
    
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
       (remprop declaration :*lisp-declaration)))
    
    (ignore
     (dolist (variable (cdr decl-spec))
       (remprop variable :*lisp-ignore)))
    (ignorable
     (dolist (variable (cdr decl-spec))
       (remprop variable :*lisp-ignorable)))
    
    (type
     (dolist (variable (cddr decl-spec))
       (remprop variable :*lisp-type)
       (remove-variable-descriptor variable)))
    
    (*defun
     (dolist (function (cdr decl-spec)) (declare (ignore function)))) ;(unproclaim-*defun-1 function)
    
    (otherwise
     (cond 
      ((not (symbolp (car decl-spec)))
       (warn "Invalid decl-spec ~S to unproclaim, retrying with (TYPE ~{~S~^ ~})." decl-spec decl-spec)
       (return-from starlisp-unproclaim (starlisp-unproclaim (cons 'type decl-spec))))
      ((standard-type-p (car decl-spec))
       (dolist (variable (cdr decl-spec))
         (remprop variable 'type)
         (remove-variable-descriptor variable)))
      ((proclaimed-declaration-p (car decl-spec))
       nil
       )
      (t 
       (warn "Invalid decl-spec ~S to unproclaim, retrying with (TYPE ~{~S~^ ~})." decl-spec decl-spec)
       (return-from starlisp-unproclaim (starlisp-unproclaim (cons 'type decl-spec)))
       )))
    
    )
  
  nil
  
  )

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
        ((and (symbolp type) (kernel:structure-class-p (find-class type nil)))
         (if canonical-front-end-type `(structure ,type) type))
  	#+old-cmu
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

(defun check-paris-restriction (node length constant what &optional arguments more-string more-arguments)
  (if (and *verbose*
	   (boundp constant)
	   (symbol-value constant)
	   (integerp length)
	   (> length (symbol-value constant)))
      (if node
	  (compiler-warn
	    node "~? ~D, which is larger than ~S.~@[~%~?~]" what arguments length constant more-string more-arguments)
	  (warn "~? ~D, which is larger than ~S.~@[~%~?~]" what arguments length constant more-string more-arguments))))




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


(defparameter unsigned-pvarp-limit 0)
(defparameter unsigned-pvarps nil)
(defparameter signed-pvarp-limit 0)
(defparameter signed-pvarps nil)

#|

;;; Stuff for *Lisp Interpreter/Compiler

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

|#

 
(defun unsigned-pvarp-closure (length)
  (declare (ignore length))
  #'(lambda (arg) (internal-pvarp arg))
  )

(defun signed-pvarp-closure (length)
  (declare (ignore length))
  #'(lambda (arg) (internal-pvarp arg))
  )


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

  
