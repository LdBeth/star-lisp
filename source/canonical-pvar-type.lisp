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

;;; (defun canonical-pvar-type (pvar-type)
;;;   (if (or (eql pvar-type 'pvar)
;;;           (equal pvar-type '(pvar))
;;;           (equal pvar-type '(pvar *)))
;;;       '(pvar *)
;;;     (if (not (and (consp pvar-type) (eql (car pvar-type) 'pvar)))
;;;         (if pvar-type
;;;             (canonical-pvar-type (expand-deftype pvar-type)))
;;;       (let* ((element-type (cadr pvar-type))
;;;              (first (if (consp element-type) (car element-type) element-type))
;;;              (rest  (if (consp element-type) (cdr element-type) ())))
;;;         (case first
;;;           ((t)
;;;            '(pvar t))
;;;           (boolean
;;;            '(pvar boolean))
;;;           (null
;;;            '(pvar boolean))
;;;           (front-end
;;;            '(pvar front-end))
;;;           (member
;;;            (if (or (equal element-type '(member nil t))
;;;                    (equal element-type '(member t nil)))
;;;                `(pvar boolean)
;;;              nil))
;;;           (number
;;;            '(pvar number))
;;;           (integer
;;;            (let* ((low (cond ((null rest) '*)
;;;                              ((consp (car rest)) (1+ (caar rest)))
;;;                              (t (car rest))))
;;;                   (high (cond ((null (cdr rest)) '*)
;;;                               ((consp (cadr rest)) (1- (caadr rest)))
;;;                               (t (cadr rest)))))
;;;              (cond ((eql low '*)
;;;                     '(pvar (signed-byte *)))
;;;                    ((>= low 0)
;;;                     (if (eql high '*)
;;;                         '(pvar (unsigned-byte *))
;;;                       `(pvar (unsigned-byte ,(max 1 (integer-length high))))))
;;;                    (t (if (eql high '*)
;;;                           '(pvar (signed-byte *))
;;;                         `(pvar (signed-byte ,(max 2 (1+ (max (integer-length low) (integer-length high)))))))))))
;;;           (unsigned-byte
;;;            (let ((length (if rest (macroexpand (car rest)) '*)))
;;;              (if (and (not (integerp length)) (constantp length)) (setq length (eval length)))
;;;              `(pvar (unsigned-byte ,length))))
;;;           (signed-byte
;;;            (let ((length (if rest (macroexpand (car rest)) '*)))
;;;              (if (and (not (integerp length)) (constantp length)) (setq length (eval length)))
;;;              `(pvar (signed-byte ,length))))
;;;           (fixnum
;;;            `(pvar (signed-byte ,(1+ (max (integer-length most-negative-fixnum) (integer-length most-positive-fixnum))))))
;;;           (bit
;;;            `(pvar (unsigned-byte 1)))
;;;           (mod
;;;            (if (or (null rest) (eql (car rest) '*))
;;;                '(pvar (unsigned-byte *))
;;;              `(pvar (unsigned-byte ,(max 1 (integer-length (1- (car rest))))))))
;;;           (float
;;;            `(pvar (defined-float * *)))
;;;           (short-float
;;;            `(pvar (defined-float #.short-float-mantissa #.short-float-exponent)))
;;;           (single-float
;;;            `(pvar (defined-float #.single-float-mantissa #.single-float-exponent)))
;;;           (double-float
;;;            `(pvar (defined-float #.double-float-mantissa #.double-float-exponent)))
;;;           (long-float
;;;            `(pvar (defined-float #.long-float-mantissa #.long-float-exponent)))
;;;           (defined-float
;;;               (let ((mantissa (if rest (car rest) '*)) (exponent (if (cdr rest) (cadr rest) '*)))
;;;                 (if (and (not (integerp mantissa)) (constantp mantissa)) (setq mantissa (eval mantissa)))
;;;                 (if (and (not (integerp exponent)) (constantp exponent)) (setq exponent (eval exponent)))
;;;                 `(pvar (defined-float ,mantissa ,exponent))))
;;;           (complex
;;;            (let ((canonical-element-type (or (cadr (canonical-pvar-type `(pvar ,@rest))) '*)))
;;;              (if (and (consp canonical-element-type)
;;;                       (member (car canonical-element-type) '(unsigned-byte signed-byte)))
;;;                  (setq canonical-element-type `(defined-float #.single-float-mantissa #.single-float-exponent)))
;;;              `(pvar (complex ,(if (or (eq canonical-element-type 't) (eq canonical-element-type '*))
;;;                                   '(defined-float * *)
;;;                                 canonical-element-type)))))
;;;           (string-char
;;;            `(pvar string-char))
;;;           (standard-char
;;;            '(pvar string-char))
;;;           (character
;;;            `(pvar character))
;;;           ((array simple-array)
;;;            (let ((canonical-element-type (or (cadr (canonical-pvar-type `(pvar ,(car rest)))) '*)))
;;;              `(pvar (array ,canonical-element-type
;;;                            ,(cond ((equal (cdr rest) '(())) '())
;;;                                   ((integerp (cadr rest)) (make-list (cadr rest) :initial-element '*))
;;;                                   ((or (null (cadr rest)) (eq (cadr rest) '*)) '*)
;;;                                   ((constantp (cadr rest)) 
;;;                                    (let ((dimensions (eval (cadr rest))))
;;;                                      (if (integerp dimensions)
;;;                                          (make-list dimensions :initial-element '*)
;;;                                        dimensions)))
;;;                                   ((atom (cadr rest)) (cadr rest))
;;;                                   (t (mapcar #'(lambda (dim) (if (constantp dim) (eval dim) dim)) (cadr rest))))))))
;;;           (vector
;;;            (let ((canonical-element-type (or (cadr (canonical-pvar-type `(pvar ,(car rest)))) '*)))
;;;              `(pvar (array ,canonical-element-type
;;;                            (,(if (or (null (cadr rest)) (eq (cadr rest) '*))
;;;                                  '* 
;;;                                (if (constantp (cadr rest)) (eval (cadr rest)) (cadr rest))))))))
;;;           (simple-vector
;;;            (canonical-pvar-type `(pvar (vector t ,(or (cadr rest) '*)))))
;;;           ((bit-vector simple-bit-vector)
;;;            `(pvar (array (unsigned-byte 1) (,(or (if (constantp (car rest)) (eval (car rest)) (car rest)) '*)))))
;;;           ((string simple-string)
;;;            `(pvar (array string-char (,(or (if (constantp (car rest)) (eval (car rest)) (car rest)) '*)))))
;;;           (structure
;;;            (let ((name (or (car rest) '*)))
;;;              (unless (or (eq name '*) (and (symbolp name) (get name '*defstruct-structure)))
;;;                (warn "~S is not a known structure type." name))
;;;              `(pvar (structure ,name))))
;;;           (ratio `(pvar (defined-float #.single-float-mantissa #.single-float-exponent)))
;;;           (otherwise
;;;            (if (and (symbolp element-type) (get element-type '*defstruct-structure))
;;;                `(pvar (structure ,element-type))
;;;              (canonical-pvar-type `(pvar ,(or (expand-pvar-element-type element-type)
;;;                                               (return-from canonical-pvar-type nil)))))))))))
;;; 
;;; (declaim (special nbits-per-lisp))
;;; 
;;; (defun length-pvar-type (type &optional (eval t))
;;;   (declare (special *SIM::*char-code-length *SIM::*character-length))
;;;   (if (and (consp type) (eql (car type) 'pvar))
;;;       (let ((element (cadr type)))
;;;         (case (if (consp element) (car element) element)
;;;           ((t)
;;;            `*)
;;;           (boolean
;;;            1)
;;;           (front-end
;;;            (if eval nbits-per-lisp 'nbits-per-lisp))
;;;           (number
;;;            '*)
;;;           ((unsigned-byte signed-byte)
;;;            (or (cadr element) '*))
;;;           (short-float
;;;            '#.(+ 1 short-float-mantissa short-float-exponent))
;;;           (single-float
;;;            '#.(+ 1 single-float-mantissa single-float-exponent))
;;;           (double-float
;;;            '#.(+ 1 double-float-mantissa double-float-exponent))
;;;           (long-float
;;;            '#.(+ 1 long-float-mantissa long-float-exponent))
;;;           (defined-float
;;;               (cond ((and (consp element)
;;;                           (integerp (cadr element))
;;;                           (integerp (caddr element)))
;;;                      (+ 1 (cadr element) (caddr element)))
;;;                     ((and (consp element)
;;;                           (cadr element) (caddr element)
;;;                           (not (eq (cadr element) '*))
;;;                           (not (eq (caddr element) '*)))
;;;                      `(+ 1 ,(cadr element) ,(caddr element)))
;;;                     (t '*)))
;;;           (complex
;;;            (let* ((element (cadr element))
;;;                   (length (cond ((and (consp element)
;;;                                       (integerp (cadr element))
;;;                                       (integerp (caddr element)))
;;;                                  (+ 1 (cadr element) (caddr element)))
;;;                                 ((and (consp element)
;;;                                       (cadr element) (caddr element)
;;;                                       (not (eq (cadr element) '*))
;;;                                       (not (eq (caddr element) '*)))
;;;                                  `(+ 1 ,(cadr element) ,(caddr element)))
;;;                                 (t '*))))
;;;              (if (eq length '*)
;;;                  '*
;;;                (if (integerp length)
;;;                    (* length 2)
;;;                  `(* ,length 2)))))
;;;           (structure
;;;            (if (equal type '(pvar (structure *)))
;;;                '*
;;;              (funcall 'structure-pvar-type-total-length-in-bits type)))
;;;           (array (length-pvar-type-for-array type eval))
;;;           (string-char (if eval *SIM::*char-code-length '*SIM::*char-code-length))
;;;           (character (if eval *SIM::*character-length '*SIM::*character-length))
;;;           (otherwise (length-pvar-type `(pvar ,(or (expand-pvar-element-type element)
;;;                                                    (return-from length-pvar-type '*)))))))
;;;     (if type
;;;         (length-pvar-type (canonical-pvar-type type)))))
;;; 
;;; (defun length-pvar-type-for-array (type &optional (eval t))
;;;   (let ((array-element-type-length (length-pvar-type `(pvar ,(array-pvar-type-element-type type)) eval))
;;; 	(array-dimensions (array-pvar-type-dimensions type)))
;;;     (cond ((eq '* array-element-type-length) '*)
;;; 	  ((numberp array-dimensions) '*)
;;; 	  ((eq array-dimensions '*) '*)
;;; 	  ((atom array-dimensions)
;;; 	   (if (and (null array-dimensions) (cddadr type))
;;; 	       array-element-type-length
;;; 	       `(* ,array-element-type-length (apply '* ,array-dimensions))))
;;; 	  ((find '* array-dimensions) '*)
;;; 	  (t
;;; 	   (values (apply 'simplify-expression '* array-element-type-length array-dimensions))))))
;;; 
;;; ;; Returns a type given all that stuff.
;;; (defun make-canonical-pvar-type
;;;     (pvar-element-type &key length mantissa exponent (dimensions nil dimp) element-type name)
;;;   (case pvar-element-type
;;;     ((boolean string-char character * t)
;;;      (unless (and (null length) (null mantissa) (null exponent) (null dimensions) (null element-type) (null name))
;;;        (error "Invalid keyword argument." ))
;;;      `(pvar ,pvar-element-type))
;;;     ((unsigned-byte signed-byte)
;;;      (unless (and (null mantissa) (null exponent) (null dimensions) (null element-type) (null name))
;;;        (error "Invalid keyword argument." ))
;;;      `(pvar (,pvar-element-type ,(or length '*))))
;;;     (defined-float
;;;       (unless (and (null length) (null dimensions) (null element-type) (null name))
;;; 	(error "Invalid keyword argument." ))
;;;       `(pvar (defined-float ,(or mantissa '*) ,(or exponent '*))))
;;;     (complex
;;;       (unless (and (null length) (null dimensions) (null element-type) (null name))
;;; 	(error "Invalid keyword argument." ))
;;;       `(pvar (complex (defined-float ,(or mantissa '*) ,(or exponent '*)))))
;;;     ((array simple-array)
;;;      (unless (and (null length) (null mantissa) (null exponent) (null name))
;;;        (error "Invalid keyword argument." ))
;;;      `(pvar (array ,(or element-type '*) ,(if dimp dimensions '*))))
;;;     (structure
;;;       (unless (and (null length) (null mantissa) (null exponent) (null dimensions) (null element-type))
;;; 	(error "Invalid keyword argument." ))
;;;       `(pvar (structure ,(or name '*))))
;;;     (otherwise (error "Invalid pvar element-type ~S." element-type))))
;;; 
;;; 
;;; (defun non-lexical-canonical-pvar-type-with-numeric-lengths-from-canonical-pvar-type (cpt)
;;;   (flet
;;;     ((eval-not* (x) (if (eq x '*) '* (eval x))))
;;;     (let ((cpet (canonical-pvar-element-type cpt)))
;;;       (case cpet
;;; 	(boolean cpt)
;;; 	(front-end cpt)
;;; 	((unsigned-byte signed-byte)
;;; 	  (let ((length (cadr (cadr cpt))))
;;; 	    (make-canonical-pvar-type cpet :length (eval-not* length))
;;; 	    ))
;;; 	(defined-float
;;; 	  (let ((mantissa (cadr (cadr cpt)))
;;; 		(exponent (caddr (cadr cpt))))
;;; 	    (if (and (or (integerp mantissa) (eq mantissa '*))
;;; 		     (or (integerp exponent) (eq exponent '*)))
;;; 		cpt
;;; 		(make-canonical-pvar-type cpet :mantissa (eval-not* mantissa) :exponent (eval-not* exponent))
;;; 		)))
;;; 	(complex
;;; 	  (let ((mantissa (cadr (cadadr cpt)))
;;; 		(exponent (caddr (cadadr cpt))))
;;; 	    (if (and (or (integerp mantissa) (eq mantissa '*))
;;; 		     (or (integerp exponent) (eq exponent '*)))
;;; 		cpt
;;; 		(make-canonical-pvar-type cpet :mantissa (eval-not* mantissa) :exponent (eval-not* exponent))
;;; 		)))
;;; 	(structure cpt)
;;; 	(string-char cpt)
;;; 	(character cpt)
;;; 	(array
;;; 	  (let ((dimensions (array-pvar-type-dimensions cpt))
;;; 		(element-type (array-pvar-type-element-type cpt)))
;;; 	    (setq dimensions
;;; 		  (cond
;;; 		    ((symbolp dimensions) (eval dimensions))
;;; 		    ((listp dimensions) (mapcar #'eval-not* dimensions))
;;; 		    ))
;;; 	    (make-canonical-pvar-type
;;; 	      'array
;;; 	      :dimensions dimensions
;;; 	      :element-type
;;; 	      (cadr (non-lexical-canonical-pvar-type-with-numeric-lengths-from-canonical-pvar-type `(pvar ,element-type)))
;;; 	      )))
;;; 	((t) cpt)
;;; 	(* cpt)
;;; 	(otherwise (error "Unknown pvar type ~S, probably internal error." cpt))
;;; 	))))
;;; 
;;; 
;;; 
;;; (defvar *warn-about-integer-dimension* t)
;;; (defvar *warn-about-array-rank* t)
;;; 
;;; 
;;; (defun pvar-type-syntax-error (form reason error)
;;;   (if error
;;;       (error "Invalid pvar type ~S.  Reason: ~A." form reason)
;;;       (throw 'invalid-pvar-type nil)
;;;       ))
;;; 
;;; 
;;; (defun vicious-dotted-pair-catcher-for-jeff (list)
;;;   (cond
;;;    ((null list) nil)
;;;    ((not (listp (cdr list))) t)
;;;    (t (or (and (listp (car list)) (vicious-dotted-pair-catcher-for-jeff (car list)))
;;;           (vicious-dotted-pair-catcher-for-jeff (cdr list))
;;;           ))))
;;; 
;;; ;;;; **** WARNING.  DANGER, DANGER WILL ROBINSON.  WARNING ****
;;; ;;;;
;;; ;;;; The logic of this code is duplicated in canonical-pvar-type.
;;; ;;;; If you change something here change it there too!
;;; 
;;; 
;;; (defun valid-pvar-type-p (pvar-type &optional (error t))
;;;   
;;;   (catch 
;;;    'invalid-pvar-type
;;;    
;;;    (macrolet
;;;        ((syntax-error (reason) `(pvar-type-syntax-error pvar-type ,reason error)))
;;;      
;;;      (labels
;;;          ((evaluatable-expression-p (expression)
;;;                                     (and expression (or (symbolp expression) (listp expression)))
;;;                                     )
;;;           (valid-form-p (expression predicate)
;;;                         (or (funcall predicate expression) (evaluatable-expression-p expression))
;;;                         )
;;;           (check-rest (pvar-type rest min max &optional (reason nil reason-provided))
;;;                       (when (> (length rest) max)
;;;                         (syntax-error (if reason-provided reason "Too many arguments")))
;;;                       (when (< (length rest) min)
;;;                         (syntax-error (if reason-provided reason "Too few arguments")))
;;;                       ))
;;;        
;;;        
;;;        (when (or (eql pvar-type 'pvar) (equal pvar-type '(pvar)) (equal pvar-type '(pvar *)))
;;;          (return-from valid-pvar-type-p '(pvar *))
;;;          )
;;;        
;;;        ;; If it is something other than (pvar ...)
;;;        ;; check whether it can be expanded via deftype.  If so,
;;;        ;; recurse, otherwise it isn't a valid pvar type.
;;;        
;;;        (when (not (and (consp pvar-type) (eql (car pvar-type) 'pvar)))
;;;          (let ((deftype-expansion (expand-deftype pvar-type)))
;;;            (return-from valid-pvar-type-p
;;;              (if (null deftype-expansion)
;;;                  (syntax-error "The form is not expandable via DEFTYPE and is not a known pvar type")
;;;                (valid-pvar-type-p (expand-deftype pvar-type) error)
;;;                ))))
;;;        
;;;        (when (vicious-dotted-pair-catcher-for-jeff pvar-type)
;;;          (syntax-error "The form has a dotted pair in it somewhere.  Types cannot have dotted pairs.")
;;;          )
;;;        
;;;        (let* ((element-type (cadr pvar-type))
;;;               (data-type (if (consp element-type) (car element-type) element-type))
;;;               (data-type-arguments (if (consp element-type) (cdr element-type) ())))
;;;          
;;;          (when (> (length pvar-type) 2)
;;;            (syntax-error
;;;             "Illegal syntax.  You probably misplaced a parenthesis.  Use (pvar <type>), not (pvar <type> <something-else>)"
;;;             ))
;;;          
;;;          (case data-type
;;;            
;;;            ;; (pvar t), (pvar boolean) (pvar front-end)
;;;            
;;;            ((t) (check-rest pvar-type data-type-arguments 0 0) '(pvar t))
;;;            (boolean (check-rest pvar-type data-type-arguments 0 0) '(pvar boolean))
;;;            (front-end (check-rest pvar-type data-type-arguments 0 0) '(pvar front-end))
;;;            
;;;            ;; (pvar (member t nil))
;;;            
;;;            (member
;;;             (if (or (equal element-type '(member nil t))
;;;                     (equal element-type '(member t nil)))
;;;                 `(pvar boolean)
;;;               (syntax-error "Starlisp cannot handle MEMBER type specifiers except for (MEMBER T NIL).  Sorry")
;;;               ))
;;;            
;;;            ;; (pvar (integer <low> <high>))
;;;            
;;;            (integer
;;;             (check-rest pvar-type data-type-arguments 0 2)
;;;             (integer-valid-pvar-type-p pvar-type data-type-arguments error)
;;;             )
;;;            
;;;            ;; (pvar (unsigned-byte <length>))
;;;            
;;;            (unsigned-byte
;;;             (check-rest pvar-type data-type-arguments 0 1 "Only 1 argument (a length) can be provided for unsigned pvar types")
;;;             (let ((data-type-arguments (if data-type-arguments (macroexpand (car data-type-arguments)) '*)))
;;;               (if (valid-form-p data-type-arguments #'(lambda (x) (and (integerp x) (plusp x))))
;;;                   `(pvar (unsigned-byte ,data-type-arguments))
;;;                 (syntax-error "The length argument cannot possibly evaluate to a positive integer")
;;;                 )))
;;;            
;;;            ;; (pvar (signed-byte <length>))
;;;            
;;;            (signed-byte
;;;             (check-rest pvar-type data-type-arguments 0 1 "Only 1 argument (a length) can be provided for signed pvar types")
;;;             (let ((data-type-arguments (if data-type-arguments (macroexpand (car data-type-arguments)) '*)))
;;;               (if (valid-form-p data-type-arguments #'(lambda (x) (and (integerp x) (> x 1))))
;;;                   `(pvar (signed-byte ,data-type-arguments))
;;;                 (syntax-error "The length argument cannot possibly evaluate to a positive integer >= 2")
;;;                 )))
;;;            
;;;            (fixnum
;;;             (check-rest pvar-type data-type-arguments 0 0 "The FIXNUM specification does not allow any arguments")
;;;             `(pvar (signed-byte ,(1+ (max (integer-length most-negative-fixnum) (integer-length most-positive-fixnum))))))
;;;            
;;;            (bit
;;;             (check-rest pvar-type data-type-arguments 0 0 "The BIT specification does not allow any arguments")
;;;             `(pvar (unsigned-byte 1)))
;;;            
;;;            ;; (pvar (mod <modulus>))
;;;            
;;;            (mod
;;;             (check-rest pvar-type data-type-arguments 1 1)
;;;             (let ((data-type-arguments (macroexpand (car data-type-arguments))))
;;;               (if (valid-form-p data-type-arguments #'plusp)
;;;                   (if (eq data-type-arguments '*)
;;;                       '(pvar (unsigned-byte *))
;;;                     (if (integerp data-type-arguments)
;;;                         `(pvar (unsigned-byte ,(max 1 (integer-length (1- data-type-arguments)))))
;;;                       `(pvar (unsigned-byte (max 1 (integer-length (1- ,data-type-arguments)))))
;;;                       ))
;;;                 (syntax-error "Mod's argument cannot possibly evaluate to a positive integer")
;;;                 )))
;;;            
;;;            ;; (pvar (float <mantissa> <exponent>))
;;;            
;;;            (float
;;;             (check-rest pvar-type data-type-arguments 0 2)
;;;             (if (null data-type-arguments)
;;;                 `(pvar (defined-float * *))
;;;               (valid-pvar-type-p `(pvar (defined-float ,@data-type-arguments)) error)
;;;               ))
;;;            
;;;            (short-float
;;;             (check-rest pvar-type data-type-arguments 0 0 "The SHORT-FLOAT specification does not allow any arguments")
;;;             `(pvar (defined-float ,short-float-mantissa ,short-float-exponent)))
;;;            (single-float
;;;             (check-rest pvar-type data-type-arguments 0 0 "The SINGLE-FLOAT specification does not allow any arguments")
;;;             `(pvar (defined-float ,single-float-mantissa ,single-float-exponent)))
;;;            (double-float
;;;             (check-rest pvar-type data-type-arguments 0 0 "The DOUBLE-FLOAT specification does not allow any arguments")
;;;             `(pvar (defined-float ,double-float-mantissa ,double-float-exponent)))
;;;            (long-float
;;;             (check-rest pvar-type data-type-arguments 0 0 "The LONG-FLOAT specification does not allow any arguments")
;;;             `(pvar (defined-float ,long-float-mantissa ,long-float-exponent)))
;;;            
;;;            ;; (pvar (defined-float <mantissa> <exponent>))
;;;            
;;;            (defined-float
;;;                (check-rest pvar-type data-type-arguments 0 2
;;;                            "Only a mantissa and exponent may be provided for 'defined-float'"
;;;                            )
;;;                (let ((mantissa (macroexpand (if data-type-arguments (first data-type-arguments) '*)))
;;;                      (exponent (macroexpand (if (cdr data-type-arguments) (second data-type-arguments) '*))))
;;;                  (if (not (valid-form-p mantissa #'(lambda (x) (and (integerp x) (plusp x)))))
;;;                      (syntax-error "The mantissa expression cannot possibly evaluate to a positive integer"))
;;;                  (if (not (valid-form-p exponent #'(lambda (x) (and (integerp x) (plusp x)))))
;;;                      (syntax-error "The exponent expression cannot possibly evaluate to a positive integer"))
;;;                  `(pvar (defined-float ,mantissa ,exponent))
;;;                  ))
;;;            
;;;            ;; (pvar (complex (defined-float <mantissa> <exponent>)))
;;;            
;;;            (complex
;;;             (check-rest pvar-type data-type-arguments 0 1
;;;                         "Either no argument or one argument (a floating point type specifier) is required after 'complex'"
;;;                         )
;;;             (if (null data-type-arguments)
;;;                 '(pvar (complex (defined-float * *)))
;;;               (let ((canonical-element-type (cadr (valid-pvar-type-p `(pvar ,@data-type-arguments)))))
;;;                 (if (and (not (eq '* canonical-element-type))
;;;                          (or (symbolp canonical-element-type) (not (eq 'defined-float (car canonical-element-type))))
;;;                          )
;;;                     (syntax-error "The complex pvar type currently requires a floating point component type")
;;;                   (if (eq '* canonical-element-type)
;;;                       `(pvar (complex (defined-float * *)))
;;;                     `(pvar (complex ,canonical-element-type))
;;;                     )
;;;                   ))))
;;;            
;;;            (string-char
;;;             (check-rest pvar-type data-type-arguments 0 0)
;;;             `(pvar string-char)
;;;             )
;;;            (standard-char
;;;             (check-rest pvar-type data-type-arguments 0 0)
;;;             '(pvar string-char)
;;;             )
;;;            (character
;;;             (check-rest pvar-type data-type-arguments 0 0)
;;;             `(pvar character)
;;;             )
;;;            
;;;            ;; (pvar (string <length>))
;;;            
;;;            ((string simple-string)
;;;             (check-rest pvar-type data-type-arguments 0 1)
;;;             (if (null data-type-arguments)
;;;                 `(pvar (array string-char (*)))
;;;               (progn
;;;                 (if (valid-form-p (car data-type-arguments) #'(lambda (x) (and (integerp x) (not (minusp x)))))
;;;                     (if (and (listp (car data-type-arguments)) (integerp (caar data-type-arguments)))
;;;                         (syntax-error "The length argument must be a single integer, not a list of integers")
;;;                       `(pvar (array string-char ,data-type-arguments))
;;;                       )
;;;                   (syntax-error "The length argument cannot possibly evaluate to a non-negative integer")
;;;                   ))))
;;;            
;;;            ;; (pvar (bit-vector <length>))
;;;            
;;;            ((bit-vector simple-bit-vector)
;;;             (check-rest pvar-type data-type-arguments 0 1)
;;;             (if (null data-type-arguments)
;;;                 `(pvar (array (unsigned-byte 1) (*)))
;;;               (progn
;;;                 (if (valid-form-p (car data-type-arguments) #'(lambda (x) (and (integerp x) (not (minusp x)))))
;;;                     (if (and (listp (car data-type-arguments)) (integerp (caar data-type-arguments)))
;;;                         (syntax-error "The length argument must be a single integer, not a list of integers")
;;;                       `(pvar (array (unsigned-byte 1) ,data-type-arguments))
;;;                       )
;;;                   (syntax-error "The length argument cannot possibly evaluate to a non-negative integer")
;;;                   ))))
;;;            
;;;            ;; (pvar (array <element-type> <dimensions>))
;;;            
;;;            ((array simple-array)
;;;             (check-rest pvar-type data-type-arguments 0 2)
;;;             (array-valid-pvar-type-p pvar-type data-type-arguments error)
;;;             )
;;;            
;;;            ;; (pvar (vector <element-type> <length>))
;;;            
;;;            ((vector simple-vector)
;;;             (check-rest pvar-type data-type-arguments 0 2)
;;;             (vector-valid-pvar-type-p pvar-type data-type-arguments error)
;;;             )
;;;            
;;;            ;; (pvar (structure foo))
;;;            
;;;            (structure
;;;             (check-rest pvar-type data-type-arguments 1 1)
;;;             (let ((name (car data-type-arguments)))
;;;               (unless (symbolp name)
;;;                 (syntax-error "The structure type designator must be a symbol"))
;;;               (unless (get name '*defstruct-structure)
;;;                 (warn "~S is not a known structure type." name))
;;;               `(pvar (structure ,name))
;;;               ))
;;;            
;;;            ;; (pvar foo)
;;;            
;;;            (otherwise
;;;             (if (and (symbolp element-type) (get element-type '*defstruct-structure))
;;;                 `(pvar (structure ,element-type))
;;;               (let ((expansion (expand-pvar-element-type element-type)))
;;;                 (when (null expansion)
;;;                   (return-from valid-pvar-type-p
;;;                     (syntax-error "The type expression does not correspond to nor is expandable to any known pvar type")
;;;                     ))
;;;                 (valid-pvar-type-p `(pvar ,expansion) error)
;;;                 )))
;;;            
;;;            ))))))
;;; 
;;; 
;;; 
;;; (defun integer-valid-pvar-type-p (pvar-type data-type-arguments error)
;;;   (macrolet
;;;     ((syntax-error (reason) `(pvar-type-syntax-error pvar-type ,reason error)))
;;;     (cond
;;;       ((null data-type-arguments) '(pvar (signed-byte *)))
;;;       (t
;;;        (let ((low (first data-type-arguments)) (high (second data-type-arguments)))
;;; 	 (setq low
;;; 	       (cond
;;; 		 ((or (integerp low) (eq low '*)) low)
;;; 		 ((listp low)
;;; 		  (if (or (not (eql 1 (length low))) (not (integerp (car low))))
;;; 		      (syntax-error "The first argument must be a one element list containing an integer")
;;; 		      (1+ (car low))
;;; 		      ))
;;; 		 (t (syntax-error "Starlisp cannot handle arguments to the INTEGER type which are not integers"))
;;; 		 ))
;;; 	 (setq high
;;; 	       (cond
;;; 		 ((and (null high) (eql 1 (length data-type-arguments))) '*)
;;; 		 ((or (integerp high) (eq high '*)) high)
;;; 		 ((listp high)
;;; 		  (if (or (not (eql 1 (length high))) (not (integerp (car high))))
;;; 		      (syntax-error "The second argument must be a one element list containing an integer")
;;; 		      (1- (car high))
;;; 		      ))
;;; 		 (t (syntax-error "Starlisp cannot handle arguments to the INTEGER type which are not integers"))
;;; 		 ))
;;; 	 (when (and (integerp low) (integerp high) (< high low))
;;; 	   (syntax-error "The minimum of the range specified is greater than the maximum!"))
;;; 	 (cond ((eql low '*)
;;; 		'(pvar (signed-byte *)))
;;; 	       ((>= low 0)
;;; 		(if (eql high '*)
;;; 		    '(pvar (unsigned-byte *))
;;; 		    `(pvar (unsigned-byte ,(max 1 (integer-length high))))))
;;; 	       (t
;;; 		(if (eql high '*)
;;; 		    '(pvar (signed-byte *))
;;; 		    `(pvar (signed-byte ,(max 2 (1+ (max (integer-length low) (integer-length high))))))
;;; 		    ))))))))
;;; 
;;; 
;;; 
;;; (defun array-valid-pvar-type-p (pvar-type data-type-arguments error)
;;;   (macrolet
;;;     ((syntax-error (reason) `(pvar-type-syntax-error pvar-type ,reason error)))
;;;     (labels
;;;       ((evaluatable-expression-p (expression)
;;; 	 (and expression (or (symbolp expression) (listp expression)))
;;; 	 )
;;;        (valid-form-p (expression predicate)
;;; 	 (or (funcall predicate expression) (evaluatable-expression-p expression))
;;; 	 ))
;;;       (when (and (listp (car data-type-arguments))
;;; 		 (integerp (car (car data-type-arguments)))
;;; 		 )
;;; 	(syntax-error
;;; 	  "The array element type is a list beginning with a number.  Perhaps you reversed the element type and dimensions"
;;; 	  ))
;;;       (if (null data-type-arguments)
;;; 	  `(pvar (array * *))
;;; 	  (let ((canonical-element-type (cadr (valid-pvar-type-p `(pvar ,(car data-type-arguments)) error))))
;;; 	    (cond
;;; ;	      ((and (eq '* (length-pvar-type `(pvar ,canonical-element-type))) (not (eq '* canonical-element-type)))
;;; ;	       (syntax-error "Starlisp does not currently allow arrays of varying length pvars.  Sorry"))
;;; 	      ((null canonical-element-type)
;;; 	       (syntax-error "The element type is not a recognized legal element pvar type"))
;;; 	      (t
;;; 	       (if (eql 1 (length data-type-arguments))
;;; 		   `(pvar (array ,canonical-element-type *))
;;; 		   (let ((dimensions (second data-type-arguments)))
;;; 		     (cond
;;; 		       ((and (integerp dimensions) (not (minusp dimensions)))
;;; 			(if *warn-about-integer-dimension*
;;; 			    (warn "I'll bet you meant ~S, a 1-dimensional array ~D long,~%~@
;;;                                    instead of ~S, a ~D dimensional array of indeterminate size.~%~@
;;;                                    To turn off this warning do ~%~@
;;;                                    (setq ~S::*warn-about-integer-dimension* nil)."
;;; 				  `(pvar (array ,canonical-element-type (,dimensions)))
;;; 				  dimensions
;;; 				  `(pvar (array ,canonical-element-type ,dimensions))
;;; 				  dimensions
;;; 				  *starlisp-internal-package-name*
;;; 				  ))
;;; 			(when (minusp dimensions)
;;; 			  (syntax-error "A negative number of dimensions makes no sense."))
;;; 			`(pvar (array ,canonical-element-type ,(make-list dimensions :initial-element '*)))
;;; 			)
;;; 		       ((symbolp dimensions)
;;; 			`(pvar (array ,canonical-element-type ,dimensions)))
;;; 		       ((listp dimensions)
;;; 			(when (eq (car dimensions) 'quote)
;;; 			  (syntax-error "The dimensions list should not be quoted.")
;;; 			  )
;;; 			(if (every #'(lambda (x) (valid-form-p x #'(lambda (y) (and (integerp y) (not (minusp y))))))
;;; 				   dimensions
;;; 				   )
;;; 			    (progn
;;; 			      (when (every #'integerp dimensions)
;;; 				(when (>= (apply #'* dimensions) *array-total-size-limit)
;;; 				  (warn "You are declaring an array of size ~D elements or greater.  This is non-portable."
;;; 					*array-total-size-limit
;;; 					))
;;; 				(when (>= (length dimensions) *array-rank-limit)
;;; 				  (when *warn-about-array-rank*
;;; 				    (warn "You are declaring an array of rank ~D or greater.~%~@
;;;                                            Common Lisp only portably allows arrays of rank 7 or less.~%~@
;;;                                            You will not be able to read your arrays out of the CM in a ~%~@
;;;                                            straightforward manner.  To turn off this warning do ~%~@
;;;                                            (setq ~S::*warn-about-array-rank* nil)"
;;; 					  *array-rank-limit *starlisp-internal-package-name*
;;; 					  ))))
;;; 			      (when (some #'(lambda (x) (and (integerp x) (>= x *array-dimension-limit))) dimensions)
;;; 				(warn "You are declaring an array which has a dimension of size ~D elements or greater.  This
;;; is non-portable."
;;; 				      *array-dimension-limit
;;; 				      ))
;;; 			      `(pvar (array ,canonical-element-type ,dimensions))
;;; 			      )
;;; 			      (syntax-error
;;; 				"Every component of the dimensions list will not evaluate to a non-negative integer")))
;;; 			(t (syntax-error
;;; 			     "The dimensions argument is unrecognizable as a potential list of non-negative integers"))
;;; 			))))
;;; 	       ))))))
;;; 
;;; 
;;; (defun vector-valid-pvar-type-p (pvar-type data-type-arguments error)
;;;   (macrolet
;;;     ((syntax-error (reason) `(pvar-type-syntax-error pvar-type ,reason error)))
;;;     (if (null data-type-arguments)
;;; 	'(pvar (array * (*)))
;;; 	(let ((canonical-element-type (cadr (canonical-pvar-type `(pvar ,(car data-type-arguments))))))
;;; 	  (cond
;;; 	    ((and (eq '* (length-pvar-type `(pvar ,canonical-element-type))) (not (eq '* canonical-element-type)))
;;; 	     (syntax-error "Starlisp does not currently allow arrays of varying length pvars.  Sorry"))
;;; 	    ((null canonical-element-type)
;;; 	     (syntax-error "The element type is not a recognized legal element pvar type"))
;;; 	    (t
;;; 	     (if (eql 1 (length data-type-arguments))
;;; 		 `(pvar (array ,canonical-element-type (*)))
;;; 		 (let ((length (second data-type-arguments)))
;;; 		   (cond
;;; 		     ((integerp length) 
;;; 		      (when (minusp length)
;;; 			(syntax-error "A negative length makes no sense."))
;;; 		      (when (>= length *array-dimension-limit)
;;; 			(warn "You are declaring a vector which has a length of ~D elements or greater.  This is non-portable"
;;; 			      *array-dimension-limit
;;; 			      ))
;;; 		      `(pvar (array ,canonical-element-type (,length)))
;;; 		      )
;;; 		     ((and length (symbolp length))
;;; 		      `(pvar (array ,canonical-element-type (,length))))
;;; 		     ((and length (listp length))
;;; 		      (if (integerp (car length))
;;; 			  (syntax-error "The length argument must be a single integer, not a list of integers")
;;; 			  `(pvar (array ,canonical-element-type (,length)))
;;; 			  ))
;;; 		     (t (syntax-error "The length argument is unrecognizable as a possible non-negative integer"))
;;; 		     )))))))))
;;; 
;;; 
;;; 