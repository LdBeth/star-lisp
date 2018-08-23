;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: *SIM-I; Base: 10; Muser: yes -*-

(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


(defun iota (n)
  (let ((result nil))
    (dotimes (j n) (push j result))
    (nreverse result)
    ))


(defun power-of-two-p (x) (and (plusp x) (eql 1 (logcount x))))

(defun next-power-of-two->= (x)
  (assert (and (integerp x) (> x 0)) () "The domain of next-power-of-two->= is positive integers")
  (labels
    ((next (power-of-two) (if (<= x power-of-two) power-of-two (next (* power-of-two 2)))))
    (next 1)
    ))

(defun fill-array (array value)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  ;; Fill a vector with a value.
  ;; Usually this is a pvar array, which is a (vector t),
  ;; but it could be a context array, which is (vector bit).
  (let ((size (length array))
        (array array)
        )
    (declare (type simple-vector array))
    (declare (type fixnum size))
    (dotimes (j size)
      (declare (type fixnum j))
      (setf (aref array j) value)
      )))

(defun return-pvar-p-declaration-p (declare-form)
  (and
    (eql 2 (length declare-form))
    (listp (second declare-form))
    (eq 'return-pvar-p (first (second declare-form)))
    (eql 2 (length (second declare-form)))
    (member (second (second declare-form)) '(t nil))
    t
    ))

(defun return-pvar-p-declaration-value (return-pvar-p-declaration)
  (second (second return-pvar-p-declaration))
  )


(defun char-flipcase (char)
  (if (upper-case-p char)
      (char-downcase char)
    (if (lower-case-p char)
        (char-upcase char)
      char
      )))

(defun compare (x y)
  (safety-check
   (when (or (not (and (numberp x) (not (complexp x))))
             (not (and (numberp y) (not (complexp y))))
             )
     (error "Only non-complex numeric values may be used in COMPARE")
     ))
  (if (< x y) -1 (if (> x y) 1 0))
  )

(defun starlisp-sqrt (x)
  (safety-check
   (when (and (not (complexp x)) (minusp x))
     (error "Taking the square root of a negative non-complex number is illegal in *Lisp.~@
              Use complex!! to first coerce the pvar to have complex values."
       )))
  (sqrt x)
  )

(defun starlisp-expt (base power)
  (safety-check
   (when (or (not (complexp base)) (not (complexp power)))
     (when (and (floatp base) (minusp base) (floatp power))
       (error "Raising a negative floating point number to a floating point power is illegal in *Lisp.~@
                Use complex!! to first coerce the base to have complex values."
         ))
     (when (and (integerp base) (integerp power) (minusp power))
       (error "Raising an integer to a negative integer power is illegal in *Lisp.~@
                Use float!! to first coerce the base to have floating point values"
         ))))
  (expt base power)
  )

(defun starlisp-log (number &optional base)
  (safety-check
   (when (and (not (complexp number)) (minusp number))
     (error "Taking the log of a negative non-complex number is illegal in *Lisp.~@
              Use complex!! to first coerce the pvar to have complex values."
       ))
   (when (and base (not (complexp base)) (minusp base))
     (error "Taking the log of a number to a negative non-complex base is illegal in *Lisp.~@
              Use complex!! to first coerce the base pvar to have complex values."
       )))
  (if base
      (log number base)
    (log number)
    ))


(defun starlisp-asin (x)
  (safety-check
   (when (and (not (complexp x)) (> x 1.0))
     (error "Taking the arc-sine of a non-complex number which is greater than 1.0 is illegal in *Lisp.~@
              Use complex!! to first coerce the argument to have complex values."
       )))
  (asin x)
  )

(defun starlisp-acos (x)
  (safety-check
   (when (and (not (complexp x)) (> x 1.0))
     (error "Taking the arc-cosine of a non-complex number which is greater than 1.0 is illegal in *Lisp.~@
              Use complex!! to first coerce the argument to have complex values."
       )))
  (acos x)
  )

(defun starlisp-acosh (x)
  (safety-check
   (when (and (not (complexp x)) (< x 1.0))
     (error "Taking the acosh of a non-complex number which is less than 1.0 is illegal in *Lisp.~@
              Use complex!! to first coerce the argument to have complex values."
       ))
   (acosh x)
   ))

(defun starlisp-atanh (x)
  (safety-check
   (when (and (not (complexp x)) (> x 1.0))
     (error "Taking the atanh of a non-complex number which is less than 1.0 is illegal in *Lisp.~@
              Use complex!! to first coerce the argument to have complex values."
       ))
   (atanh x)
   ))

(defun front-end-gray-code-from-integer (integer)
    (logxor integer (ash integer -1))
    )

(defun front-end-integer-from-gray-code (gray-code)
  (let ((answer 0))
    (dotimes (bit (integer-length (abs gray-code)))
      (setq answer (logxor answer (ash gray-code (- bit))))
      )
    answer
    ))

(defun load-byte (source position size)
  (ldb (byte size position) source)
  )


(defun n-bits-for-address (number)
  (assert (plusp number))
  (max 1 (integer-length (1- number)))
  )
 

;;; A quicker, more mnemonic function for jumping immediately
;;; into or out of the *Lisp package:  (W.R.S. -- 8/11/89)

;;; A non-NIL argument puts you in the *Lisp package, a NIL argument puts you
;;; in the User package, and no argument toggles you between the two.


(defun *lisp (&optional (select-*lisp :toggle))
  (eval `(in-package
          ,(if (eq select-*lisp :toggle)
             (if (eq *package* (find-package :*lisp))
               :cl-user
               :*lisp)
             (if select-*lisp :*lisp :cl-user))))
  (format t "Default package is now ~A.~%" (package-name *package*))
  (values))

(defun cl-user::*lisp (&optional (select-*lisp :toggle))
  (*lisp select-*lisp))

(defun non-negative-integer-p (x) (and (integerp x) (> x -1)))

#+Allegro
(defun string-char-p (x) (excl::string-char-p x))
#-Allegro
(defun string-char-p (x) (characterp x))
