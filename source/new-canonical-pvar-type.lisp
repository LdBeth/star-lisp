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


(defun expand-deftype (type)
  (let (first expander (first-time t))
    (loop (setq first (if (consp type) (car type) type))
          (if (eql first 'pvar) (return type))
          (setq expander (get first :*lisp-deftype))
          (if (null expander) (return (if first-time nil type)))
          (setq type (apply expander (if (consp type) (cdr type) ())))
          (setq first-time nil)
          )))

(defun expand-pvar-element-type (element-type)
  (let* ((type (if (consp element-type) (car element-type) element-type))
	 (expander (get type :*lisp-deftype)))
    (if expander
        (apply expander (if (consp element-type) (cdr element-type) ()))
	nil)))


(defmacro create-specific-canonical-functions (specific-types)
  `(progn
     ,@(mapcar 
           #'(lambda (s) 
               `(setf (get ',s :*lisp-canonical-function)
                  ',(intern (concatenate 'string
                              "CANONICAL-" (symbol-name s) "-PVAR-TYPE")
                            (find-package :*lisp-i)
                            )))
         specific-types
         )))

(create-specific-canonical-functions
 (integer unsigned-byte signed-byte fixnum mod 
          defined-float complex array simple-array
          vector simple-vector bit-vector simple-bit-vector
          string simple-string structure
          ))

(defun value-if-constant (x)
  (if (constantp x) 
      (if (symbolp x) (symbol-value x) (eval x))
    x
    ))

;;;; **** WARNING.  DANGER, DANGER WILL ROBINSON.  WARNING ****
;;;;
;;;; The logic of this code is duplicated in valid-pvar-type-p.
;;;; If you change something here change it there too!

(defun canonical-pvar-type (pvar-type)
  (cond
   ((or (eql pvar-type 'pvar) (equal pvar-type '(pvar)) (equal pvar-type '(pvar *)))
    '(pvar *)
    )
   ((not (and (consp pvar-type) (eql (car pvar-type) 'pvar)))
    (and pvar-type (canonical-pvar-type (expand-deftype pvar-type)))
    )
   (t (canonical-specific-pvar-type pvar-type))
   ))



(defun canonical-specific-pvar-type (pvar-type)
  (let* ((element-type (cadr pvar-type))
         (first (if (consp element-type) (car element-type) element-type))
         (rest  (if (consp element-type) (cdr element-type) ()))
         (dispatch-function (get first :*lisp-canonical-function))
         )
    (case first
      ((t) '(pvar t))
      ((boolean null) '(pvar boolean))
      (front-end '(pvar front-end))
      (string-char '(pvar string-char))
      (standard-char '(pvar string-char))
      (character '(pvar character))
      (bit (canonical-pvar-type '(pvar (unsigned-byte 1))))
      (number '(pvar number))
      (float `(pvar (defined-float * *)))
      (short-float `(pvar (defined-float #.short-float-mantissa #.short-float-exponent)))
      (single-float `(pvar (defined-float #.single-float-mantissa #.single-float-exponent)))
      (double-float `(pvar (defined-float #.double-float-mantissa #.double-float-exponent)))
      (long-float `(pvar (defined-float #.long-float-mantissa #.long-float-exponent)))
      (otherwise
       (cond
        (dispatch-function
         (funcall dispatch-function rest))
        ((and (symbolp element-type) (get element-type '*defstruct-structure))
         `(pvar (structure ,element-type)))
        (t 
         (let ((expanded-element-type (expand-pvar-element-type element-type)))
           (if expanded-element-type 
               (canonical-pvar-type `(pvar ,expanded-element-type))
             nil
             )))
        )))))
        

(defun canonical-integer-pvar-type (rest)
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
               `(pvar (signed-byte ,(max 2 (1+ (max (integer-length low) (integer-length high))))))
               )))))

(defun canonical-unsigned-byte-pvar-type (rest)
  (let ((length (if rest (macroexpand (car rest)) '*)))
    `(pvar (unsigned-byte ,(value-if-constant length)))
    ))

(defun canonical-signed-byte-pvar-type (rest)
  (let ((length (if rest (macroexpand (car rest)) '*)))
    `(pvar (signed-byte ,(value-if-constant length)))
    ))

(defun canonical-fixnum-pvar-type (rest)
  (declare (ignore rest))
  (let ((size #.(1+ (max (integer-length most-negative-fixnum)
                         (integer-length most-positive-fixnum)
                         ))))
    `(pvar (signed-byte ,size))
    ))


(defun canonical-mod-pvar-type (rest)  
  (if (or (null rest) (eql (car rest) '*))
      '(pvar (unsigned-byte *))
    `(pvar (unsigned-byte ,(max 1 (integer-length (1- (car rest))))))
    ))

(defun canonical-defined-float-pvar-type (rest)
  (let ((mantissa (if rest (car rest) '*)) (exponent (if (cdr rest) (cadr rest) '*)))
    (if (and (not (integerp mantissa)) (constantp mantissa)) (setq mantissa (eval mantissa)))
    (if (and (not (integerp exponent)) (constantp exponent)) (setq exponent (eval exponent)))
    `(pvar (defined-float ,mantissa ,exponent))
    ))

(defun canonical-complex-pvar-type (rest)
  (let ((canonical-element-type (or (cadr (canonical-pvar-type `(pvar ,@rest))) '*)))
    (if (and (consp canonical-element-type)
             (member (car canonical-element-type) '(unsigned-byte signed-byte)))
        (setq canonical-element-type `(defined-float #.single-float-mantissa #.single-float-exponent)))
    `(pvar (complex ,(if (or (eq canonical-element-type 't) (eq canonical-element-type '*))
                         '(defined-float * *)
                       canonical-element-type
                       )))))

(defun canonical-array-pvar-type (rest)
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
                         (t (mapcar #'(lambda (dim) (value-if-constant dim)) (cadr rest)))
                         )))))

(defun canonical-simple-array-pvar-type (rest)
  (canonical-array-pvar-type rest)
  )

(defun canonical-vector-pvar-type (rest)
  (let ((canonical-element-type (or (cadr (canonical-pvar-type `(pvar ,(car rest)))) '*)))
    `(pvar (array ,canonical-element-type
                  (,(if (or (null (cadr rest)) (eq (cadr rest) '*))
                        '* 
                      (value-if-constant (cadr rest))
                      ))))))

(defun canonical-simple-vector-pvar-type (rest)
  (canonical-pvar-type `(pvar (vector t ,(or (cadr rest) '*))))
  )

(defun canonical-bit-vector-pvar-type (rest)
  (canonical-pvar-type
   `(pvar (array (unsigned-byte 1) 
                 (,(or (value-if-constant (car rest)) '*))
                 ))))

(defun canonical-simple-bit-vector-pvar-type (rest)
  (canonical-bit-vector-pvar-type rest)
  )

(defun canonical-string-pvar-type (rest)
  (canonical-pvar-type
   `(pvar (array string-char (,(or (value-if-constant (car rest)) '*))))
   ))

(defun canonical-simple-string-pvar-type (rest)
  (canonical-string-pvar-type rest)
  )

(defun canonical-structure-pvar-type (rest)
  (let ((name (or (car rest) '*)))
    (unless (or (eq name '*) (and (symbolp name) (get name '*defstruct-structure)))
      (warn "~S is not a known structure type." name))
    `(pvar (structure ,name))
    ))


(defun length-pvar-type (type &optional (eval t))
  (if (and (consp type) (eql (car type) 'pvar))
      (let* ((element (cadr type))
             (specifier (if (consp element) (car element) element)))
        (case specifier
          ((t) '*)
          (boolean 1)
          (front-end (if eval nbits-per-lisp 'nbits-per-lisp))
          (string-char (if eval *SIM::*char-code-length '*SIM::*char-code-length))
          (character (if eval *SIM::*character-length '*SIM::*character-length))
          (number '*)
          ((unsigned-byte signed-byte) (or (cadr element) '*))
          (short-float #.(+ 1 short-float-mantissa short-float-exponent))
          (single-float #.(+ 1 single-float-mantissa single-float-exponent))
          (double-float #.(+ 1 double-float-mantissa double-float-exponent))
          (long-float #.(+ 1 long-float-mantissa long-float-exponent))
          (defined-float (length-defined-float-pvar-type type element))
          (complex (length-complex-pvar-type type element))
          (structure (length-structure-pvar-type type element))
          (array (length-array-pvar-type type element eval))
          (otherwise
           (let ((expansion (expand-pvar-element-type element)))
             (if expansion
                 (length-pvar-type `(pvar ,expansion))
               '*
               )))
          ))
    (and type (length-pvar-type (canonical-pvar-type type)))
    ))

(defun length-defined-float-pvar-type (type element)
  (declare (ignore type))
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

(defun length-complex-pvar-type (type element)
  (let ((length (length-defined-float-pvar-type type (cadr element))))
    (if (eq length '*)
        '*
      (if (integerp length)
          (* length 2)
        `(* ,length 2)
        ))))

(defun length-structure-pvar-type (type element)
  (declare (ignore element))
  (if (equal type '(pvar (structure *)))
      '*
    (structure-pvar-type-total-length-in-bits type)
    ))

(defun length-array-pvar-type (type element eval)
  (declare (ignore element))
  (length-pvar-type-for-array type eval)
  )

 
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


;; Returns a type given all that stuff.

(defun make-canonical-pvar-type
    (pvar-element-type &key length mantissa exponent (dimensions nil dimp) element-type name)
  (flet ((error-not-all-null 
          (&rest args) 
          (unless (every #'null args) 
            (error "Invalid keyword argument to MAKE-CANONICAL-PVAR-TYPE.")
            )))
    (case pvar-element-type
      ((boolean string-char character * t)
       (error-not-all-null length mantissa exponent dimensions element-type name)
       `(pvar ,pvar-element-type))
      ((unsigned-byte signed-byte)
       (error-not-all-null mantissa exponent dimensions element-type name)
       `(pvar (,pvar-element-type ,(or length '*))))
      (defined-float
          (error-not-all-null length dimensions element-type name)
          `(pvar (defined-float ,(or mantissa '*) ,(or exponent '*))))
      (complex
       (error-not-all-null length dimensions element-type name)
       `(pvar (complex (defined-float ,(or mantissa '*) ,(or exponent '*)))))
      ((array simple-array)
       (error-not-all-null length mantissa exponent name)
       `(pvar (array ,(or element-type '*) ,(if dimp dimensions '*))))
      (structure
       (error-not-all-null length mantissa exponent dimensions element-type)
       `(pvar (structure ,(or name '*))))
      (otherwise 
       (error "Invalid pvar-element-type ~S in MAKE-CANONICAL-PVAR-TYPE." element-type))
      )))
  

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
