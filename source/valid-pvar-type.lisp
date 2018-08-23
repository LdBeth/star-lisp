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

(defvar *warn-about-integer-dimension* t)
(defvar *warn-about-array-rank* t)


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


(defun evaluatable-expression-p (expression)
  (and expression (or (symbolp expression) (listp expression))))

(defun valid-form-p (expression predicate)
  (or (funcall predicate expression) (evaluatable-expression-p expression)))

(defparameter *vptp-pvar-type* nil)
(defparameter *vptp-error* nil)

(defun pvar-type-syntax-error (form reason error)
  (if error
      (error "Invalid pvar type ~S.  Reason: ~A." form reason)
      (throw 'invalid-pvar-type nil)
      ))

(defun vptp-syntax-error (reason)
  (pvar-type-syntax-error *vptp-pvar-type* reason *vptp-error*))

(defun vptp-check-rest (rest min max &optional (reason nil reason-provided))
  (when (> (length rest) max)
    (vptp-syntax-error (if reason-provided reason "Too many arguments")))
  (when (< (length rest) min)
    (vptp-syntax-error (if reason-provided reason "Too few arguments")))
  )
  
 
(defun valid-pvar-type-p (pvar-type &optional (error t))
  
  (catch 
   'invalid-pvar-type

   (let ((*vptp-pvar-type* pvar-type) (*vptp-error* error))
   
     (canonical-pvar-type

      (cond
     
       ((or (eql pvar-type 'pvar) (equal pvar-type '(pvar)) (equal pvar-type '(pvar *)))
        '(pvar *)
        )
       
       ;; If it is something other than (pvar ...)
       ;; check whether it can be expanded via deftype.  If so,
       ;; recurse, otherwise it isn't a valid pvar type.
       
       ((not (and (consp pvar-type) (eql (car pvar-type) 'pvar)))
         (let ((deftype-expansion (expand-deftype pvar-type)))
           (if deftype-expansion 
               (valid-pvar-type-p (expand-deftype pvar-type) error)
             (vptp-syntax-error "The form is not expandable via DEFTYPE and is not a known pvar type")
             )))
       
       ((vicious-dotted-pair-catcher-for-jeff pvar-type)
         (vptp-syntax-error "The form has a dotted pair in it somewhere.  Types cannot have dotted pairs.")
         )

       ((> (length pvar-type) 2)
        (vptp-syntax-error
         "Illegal syntax.  You probably misplaced a parenthesis.~
          Use (pvar <type>), not (pvar <type> <something-else>)"
         ))
       
       (t
        (valid-pvar-type-p-internal pvar-type error)
        )

       )))))


(defun valid-pvar-type-p-internal (pvar-type error)
  
  (let* ((element-type (cadr pvar-type))
         (data-type (if (consp element-type) (car element-type) element-type))
         (data-type-arguments (if (consp element-type) (cdr element-type) ())))
    
    (case data-type
      
      ;; Stuff with no arguments
      
      ((t boolean front-end fixnum bit 
          short-float single-float double-float long-float
          string-char character
          )
       (vptp-check-rest 
        data-type-arguments 0 0
        (format nil "The ~A specification does not allow any arguments"
          (symbol-name data-type)
          ))
       `(pvar ,data-type)
       )
      
      ;; (pvar (integer <low> <high>))
      (integer (integer-valid-pvar-type-p data-type-arguments))
      
      ;; (pvar (unsigned-byte <length>))
      (unsigned-byte (unsigned-byte-valid-pvar-type-p data-type-arguments))

      ;; (pvar (signed-byte <length>))
      (signed-byte (signed-byte-valid-pvar-type-p data-type-arguments))

      ;; (pvar (float <mantissa> <exponent>))
      (float
       (vptp-check-rest data-type-arguments 0 2)
       (if (null data-type-arguments)
           `(pvar (defined-float * *))
         (valid-pvar-type-p `(pvar (defined-float ,@data-type-arguments)) error)
         ))
      
      ;; (pvar (defined-float <mantissa> <exponent>))
      (defined-float (defined-float-valid-pvar-type-p data-type-arguments))
                
      ;; (pvar (complex (defined-float <mantissa> <exponent>)))
      (complex (complex-valid-pvar-type-p data-type-arguments))

      (standard-char
       (vptp-check-rest data-type-arguments 0 0)
       '(pvar string-char)
       )
      
      ;; (pvar (string <length>))
      ((string simple-string) (string-valid-pvar-type-p data-type-arguments))
             
      ;; (pvar (bit-vector <length>))
      ((bit-vector simple-bit-vector) (bit-vector-valid-pvar-type-p data-type-arguments))
             
      ;; (pvar (array <element-type> <dimensions>))
      ((array simple-array) (array-valid-pvar-type-p data-type-arguments))
      
      ;; (pvar (vector <element-type> <length>))
      ((vector simple-vector)
       (vptp-check-rest data-type-arguments 0 2)
       (vector-valid-pvar-type-p data-type-arguments)
       )
      
      ;; (pvar (structure foo))
      (structure
       (vptp-check-rest data-type-arguments 1 1)
       (let ((name (car data-type-arguments)))
         (unless (symbolp name)
           (vptp-syntax-error "The structure type designator must be a symbol"))
         (unless (get name '*defstruct-structure)
           (warn "~S is not a known structure type." name))
         `(pvar (structure ,name))
         ))
      
      ;; (pvar foo)
      
      (otherwise
       (if (and (symbolp element-type) (get element-type '*defstruct-structure))
           `(pvar (structure ,element-type))
         (let ((expansion (expand-pvar-element-type element-type)))
           (if expansion
               (valid-pvar-type-p `(pvar ,expansion) error)
             (vptp-syntax-error 
              "The type expression does not correspond to nor is expandable to any known pvar type")
             ))))
      
      )))


(defun unsigned-byte-valid-pvar-type-p (data-type-arguments)
  (vptp-check-rest 
   data-type-arguments 0 1 
   "Only 1 argument (a length) can be provided for unsigned pvar types")
  (let ((data-type-arguments (if data-type-arguments (macroexpand (car data-type-arguments)) '*)))
    (if (valid-form-p data-type-arguments #'(lambda (x) (and (integerp x) (plusp x))))
        `(pvar (unsigned-byte ,data-type-arguments))
      (vptp-syntax-error "The length argument cannot possibly evaluate to a positive integer")
      )))

(defun signed-byte-valid-pvar-type-p (data-type-arguments)
  (vptp-check-rest 
   data-type-arguments 0 1 
   "Only 1 argument (a length) can be provided for signed pvar types")
  (let ((data-type-arguments (if data-type-arguments (macroexpand (car data-type-arguments)) '*)))
    (if (valid-form-p data-type-arguments #'(lambda (x) (and (integerp x) (> x 1))))
        `(pvar (signed-byte ,data-type-arguments))
      (vptp-syntax-error "The length argument cannot possibly evaluate to a positive integer >= 2")
      )))

(defun integer-valid-pvar-type-p (data-type-arguments)
  (vptp-check-rest data-type-arguments 0 2)
  (cond
   ((null data-type-arguments) '(pvar (signed-byte *)))
   (t
    (let ((low (first data-type-arguments)) (high (second data-type-arguments)))
      (setq low
            (cond
             ((or (integerp low) (eq low '*)) low)
             ((listp low)
              (if (or (not (eql 1 (length low))) (not (integerp (car low))))
                  (vptp-syntax-error "The first argument must be a one element list containing an integer")
                (1+ (car low))
                ))
             (t (vptp-syntax-error "Starlisp cannot handle arguments to the INTEGER type which are not integers"))
             ))
      (setq high
            (cond
             ((and (null high) (eql 1 (length data-type-arguments))) '*)
             ((or (integerp high) (eq high '*)) high)
             ((listp high)
              (if (or (not (eql 1 (length high))) (not (integerp (car high))))
                  (vptp-syntax-error "The second argument must be a one element list containing an integer")
                (1- (car high))
                ))
             (t (vptp-syntax-error "Starlisp cannot handle arguments to the INTEGER type which are not integers"))
             ))
      (when (and (integerp low) (integerp high) (< high low))
        (vptp-syntax-error "The minimum of the range specified is greater than the maximum!"))
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
               )))))))


(defun defined-float-valid-pvar-type-p (data-type-arguments)
  (vptp-check-rest data-type-arguments 0 2
                   "Only a mantissa and exponent may be provided for 'defined-float'"
                   )
  (let ((mantissa (macroexpand (if data-type-arguments (first data-type-arguments) '*)))
        (exponent (macroexpand (if (cdr data-type-arguments) (second data-type-arguments) '*))))
    (if (not (valid-form-p mantissa #'(lambda (x) (and (integerp x) (plusp x)))))
        (vptp-syntax-error "The mantissa expression cannot possibly evaluate to a positive integer"))
    (if (not (valid-form-p exponent #'(lambda (x) (and (integerp x) (plusp x)))))
        (vptp-syntax-error "The exponent expression cannot possibly evaluate to a positive integer"))
    `(pvar (defined-float ,mantissa ,exponent))
    ))

(defun complex-valid-pvar-type-p (data-type-arguments)
  (vptp-check-rest 
   data-type-arguments 0 1
   "Either no argument or one argument (a floating point type specifier) is required after 'complex'"
   )
  (if (null data-type-arguments)
      '(pvar (complex (defined-float * *)))
    (let ((canonical-element-type (cadr (valid-pvar-type-p `(pvar ,@data-type-arguments) *vptp-error*))))
      (if (and (not (eq '* canonical-element-type))
               (or (symbolp canonical-element-type) (not (eq 'defined-float (car canonical-element-type))))
               )
          (vptp-syntax-error "The complex pvar type requires a floating point component type")
        (if (eq '* canonical-element-type)
            `(pvar (complex (defined-float * *)))
          `(pvar (complex ,canonical-element-type))
          )
        ))))

(defun string-valid-pvar-type-p (data-type-arguments)
  (vptp-check-rest data-type-arguments 0 1)
  (if (null data-type-arguments)
      `(pvar (array string-char (*)))
    (progn
      (if (valid-form-p (car data-type-arguments) #'(lambda (x) (and (integerp x) (not (minusp x)))))
          (if (and (listp (car data-type-arguments)) (integerp (caar data-type-arguments)))
              (vptp-syntax-error "The length argument must be a single integer, not a list of integers")
            `(pvar (array string-char ,data-type-arguments))
            )
        (vptp-syntax-error "The length argument cannot possibly evaluate to a non-negative integer")
        ))))


(defun bit-vector-valid-pvar-type-p (data-type-arguments)
  (vptp-check-rest data-type-arguments 0 1)
  (if (null data-type-arguments)
      `(pvar (array (unsigned-byte 1) (*)))
    (progn
      (if (valid-form-p (car data-type-arguments) #'(lambda (x) (and (integerp x) (not (minusp x)))))
          (if (and (listp (car data-type-arguments)) (integerp (caar data-type-arguments)))
              (vptp-syntax-error "The length argument must be a single integer, not a list of integers")
            `(pvar (array (unsigned-byte 1) ,data-type-arguments))
            )
        (vptp-syntax-error "The length argument cannot possibly evaluate to a non-negative integer")
        ))))

(defun array-valid-pvar-type-p (data-type-arguments)
  (vptp-check-rest data-type-arguments 0 2)
  (when (and (listp (car data-type-arguments))
             (integerp (car (car data-type-arguments)))
             )
    (vptp-syntax-error 
     "The array element type is a list beginning with a number.~
      Perhaps you reversed the element type and dimensions"
     ))
  (if (null data-type-arguments)
      `(pvar (array * *))
    (let ((canonical-element-type 
           (cadr (valid-pvar-type-p `(pvar ,(car data-type-arguments)) *vptp-error*))))
      (cond
       ;;((and (eq '* (length-pvar-type `(pvar ,canonical-element-type))) (not (eq '* canonical-element-type)))
       ;;(syntax-error "Starlisp does not currently allow arrays of varying length pvars.  Sorry"))
       ((null canonical-element-type)
        (vptp-syntax-error "The element type is not a recognized legal element pvar type"))
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
                (vptp-syntax-error "A negative number of dimensions makes no sense."))
              `(pvar (array ,canonical-element-type ,(make-list dimensions :initial-element '*)))
              )
             ((symbolp dimensions)
              `(pvar (array ,canonical-element-type ,dimensions)))
             ((listp dimensions)
              (when (eq (car dimensions) 'quote)
                (vptp-syntax-error "The dimensions list should not be quoted.")
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
                (vptp-syntax-error
                 "Every component of the dimensions list will not evaluate to a non-negative integer")))
             (t (vptp-syntax-error
                 "The dimensions argument is unrecognizable as a potential list of non-negative integers"))
             ))))
       ))))


(defun vector-valid-pvar-type-p (data-type-arguments)
  (if (null data-type-arguments)
      '(pvar (array * (*)))
    (let ((canonical-element-type (cadr (canonical-pvar-type `(pvar ,(car data-type-arguments))))))
      (cond
       ((and (eq '* (length-pvar-type `(pvar ,canonical-element-type))) (not (eq '* canonical-element-type)))
        (vptp-syntax-error "Starlisp does not currently allow arrays of varying length pvars.  Sorry"))
       ((null canonical-element-type)
        (vptp-syntax-error "The element type is not a recognized legal element pvar type"))
       (t
        (if (eql 1 (length data-type-arguments))
            `(pvar (array ,canonical-element-type (*)))
          (let ((length (second data-type-arguments)))
            (cond
             ((integerp length) 
              (when (minusp length)
                (vptp-syntax-error "A negative length makes no sense."))
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
                  (vptp-syntax-error "The length argument must be a single integer, not a list of integers")
                `(pvar (array ,canonical-element-type (,length)))
                ))
             (t (vptp-syntax-error "The length argument is unrecognizable as a possible non-negative integer"))
             ))))))))


