(in-package :*sim-i)

;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
;;;> 
;;;> The Thinking Machines *Lisp Simulator is in the public domain.
;;;> You are free to do whatever you like with it, including but
;;;> not limited to distributing, modifying, and copying.
;;;>
;;;> *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+

;;; Author:  JP Massar.


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
  (defconstant nbits-per-lisp 4)
)

(defun error-not-cpt (type)
  (error "~S is not a canonical pvar type." type))

(defun error-if-not-cpt (type)
  (unless (and (consp type) (eq (car type) 'pvar))
    (error-not-cpt type)
    ))


;;This will return one of boolean, unsigned-byte, signed-byte, defined-float,
;; complex, string-char, character, array, structure, t, front-end, or *.

(defun canonical-pvar-element-type (type)
  (error-if-not-cpt type)
  (if (consp (cadr type)) (caadr type) (cadr type))
  )

(defun array-pvar-type-p (type)
  (error-if-not-cpt type)
  (and (consp (cadr type)) (eq (caadr type) 'array)))

(defun structure-pvar-type-p (type)
  (error-if-not-cpt type)
  (and (consp (cadr type)) (eq (caadr type) 'structure)))

(defun array-pvar-type-dimensions (type)
  (unless (array-pvar-type-p type) (error-not-cpt type))
  (caddr (cadr type)))

(defun array-pvar-type-element-type (type)
  (unless (array-pvar-type-p type) (error-not-cpt type))
  (cadr (cadr type)))

(defun structure-pvar-type-name (type)
  (unless (structure-pvar-type-p type) (error-not-cpt type))
  (cadr (cadr type)))

(defun float-pvar-type-p (type)
  (error-if-not-cpt type)
  (and (consp (cadr type)) (eq (caadr type) 'defined-float)))

(defun complex-pvar-type-p (type)
  (error-if-not-cpt type)
  (and (consp (cadr type)) (eq (caadr type) 'complex)))

(defun boolean-pvar-type-p (type)
  (error-if-not-cpt type)
  (eq (cadr type) 'boolean))

(defun front-end-pvar-type-p (type)
  (error-if-not-cpt type)
  (eq (cadr type) 'front-end))

(defun general-pvar-type-p (type)
  (error-if-not-cpt type)
  (eq (cadr type) 't))

(defun string-char-pvar-type-p (type)
  (error-if-not-cpt type)
  (eq (cadr type) 'string-char))

(defun character-pvar-type-p (type)
  (error-if-not-cpt type)
  (eq (cadr type) 'character))

(defun signed-pvar-type-p (type)
  (error-if-not-cpt type)
  (and (consp (cadr type)) (eq (caadr type) 'signed-byte)))

(defun unsigned-pvar-type-p (type)
  (error-if-not-cpt type)
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
