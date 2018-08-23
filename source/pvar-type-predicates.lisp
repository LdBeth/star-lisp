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

(defun pvarp (object &optional (element-type '*))
  (if (internal-pvarp object)
      (if (eql element-type '*)
	  t
	  (check-pvar-element-type object element-type))))

(defun ptype? (pvar key) (eql (pvar-type pvar) key))

#|

;;; predicates used by *Lisp Interpreter/Compiler

(defun boolean-pvarp (arg)
  (and (internal-pvarp arg) (ptype? arg :boolean)))

(defun front-end-pvarp (arg)
  (and (internal-pvarp arg) (ptype? arg :front-end)))

(defun signed-pvarp (arg &optional (length '*))
  (and (internal-pvarp arg)
       (ptype? arg :signed)
       (or (eql length '*)
	   (eql (pvar-length arg) length))))

(defun unsigned-pvarp (arg &optional (length '*))
  (and (internal-pvarp arg)
       (ptype? arg :field)
       (or (eql length '*)
	   (eql (pvar-length arg) length))))

(defun float-pvarp (arg &optional (mantissa '*) (exponent '*))
  (and (internal-pvarp arg)
       (ptype? arg :float)
       (or (eql mantissa '*)
	   (eql (pvar-mantissa-length arg) mantissa))
       (or (eql exponent '*)
	   (eql (pvar-exponent-length arg) exponent))))

(defun specified-float-pvarp (arg mantissa exponent)
  (and (float-pvarp arg)
       (eql (pvar-mantissa-length arg) mantissa)
       (eql (pvar-exponent-length arg) exponent)
       ))

(defun single-float-pvarp (arg) 
  (specified-float-pvarp arg single-float-mantissa single-float-exponent))

(defun short-float-pvarp (arg)
  (specified-float-pvarp arg short-float-mantissa short-float-exponent))

(defun double-float-pvarp (arg)
  (specified-float-pvarp arg double-float-mantissa double-float-exponent))

(defun long-float-pvarp (arg)
  (specified-float-pvarp arg long-float-mantissa long-float-exponent))

(defun extended-float-pvarp (arg)
  (specified-float-pvarp arg extended-float-mantissa extended-float-exponent))

(defun character-pvarp (arg)
  (and (internal-pvarp arg) (ptype? arg :character)))

(defun string-char-pvarp (arg)
  (and (internal-pvarp arg) (ptype? arg :string-char)))

(defun general-pvarp (arg)
  (and (internal-pvarp arg) (ptype? arg :general) (not (void-pvar-p arg))))

(defun complex-pvarp (arg &optional (mantissa '*) (exponent '*))
  (and (internal-pvarp arg)
       (ptype? arg :complex)
       (or (eql mantissa '*)
	   (eql (pvar-mantissa-length arg) mantissa))
       (or (eql exponent '*)
	   (eql (pvar-exponent-length arg) exponent))))

(defun specified-complex-pvarp (arg mantissa exponent)
  (and (complex-pvarp arg)
       (eql (pvar-mantissa-length arg) mantissa)
       (eql (pvar-exponent-length arg) exponent)
       ))

(defun single-complex-pvarp (arg)
  (specified-complex-pvarp arg single-float-mantissa single-float-exponent))

(defun short-complex-pvarp (arg)
  (specified-complex-pvarp arg short-float-mantissa short-float-exponent))

(defun double-complex-pvarp (arg)
  (specified-complex-pvarp arg double-float-mantissa double-float-exponent))

(defun long-complex-pvarp (arg)
  (specified-complex-pvarp arg long-float-mantissa long-float-exponent))

(defun extended-complex-pvarp (arg)
  (specified-complex-pvarp arg extended-float-mantissa extended-float-exponent))

(defun array-pvarp (arg &optional (element-type '*) (dimensions '*))
  (and (internal-pvarp arg)
       (ptype? arg :array)
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
       (ptype? arg :structure)
       (or (eq name '*) (funcall 'included-*defstruct-p (portable-pvar-structure-name arg) name))))


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


;;; JP Massar.  7/13/89
;;; Made internal type predicates not error out.


(defun boolean-pvarp (arg) (internal-pvarp arg))
(defun front-end-pvarp (arg) (internal-pvarp arg))
(defun signed-pvarp (arg &optional (length '*)) (declare (ignore length)) (internal-pvarp arg))
(defun unsigned-pvarp (arg &optional (length '*)) (declare (ignore length)) (internal-pvarp arg))
(defun float-pvarp (arg &optional (mantissa '*) (exponent '*))
  (declare (ignore mantissa exponent))
  (internal-pvarp arg)
  )
(defun single-float-pvarp (arg) (internal-pvarp arg))
(defun short-float-pvarp (arg) (internal-pvarp arg))
(defun double-float-pvarp (arg) (internal-pvarp arg))
(defun long-float-pvarp (arg) (internal-pvarp arg))
(defun extended-float-pvarp (arg) (internal-pvarp arg))
(defun character-pvarp (arg) (internal-pvarp arg))
(defun string-char-pvarp (arg) (internal-pvarp arg))
(defun general-pvarp (arg) (internal-pvarp arg))
(defun complex-pvarp (arg &optional (mantissa '*) (exponent '*))
  (declare (ignore mantissa exponent))
  (internal-pvarp arg)
  )
(defun single-complex-pvarp (arg) (internal-pvarp arg))
(defun short-complex-pvarp (arg) (internal-pvarp arg))
(defun double-complex-pvarp (arg) (internal-pvarp arg))
(defun long-complex-pvarp (arg) (internal-pvarp arg))
(defun extended-complex-pvarp (arg) (internal-pvarp arg))


